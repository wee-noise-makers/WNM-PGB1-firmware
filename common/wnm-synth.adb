-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2017 Fabien Chouteau                  --
--                                                                           --
--    Wee Noise Maker is free software: you can redistribute it and/or       --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Maker is distributed in the hope that it will be useful,     --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with We Noise Maker. If not, see <http://www.gnu.org/licenses/>. --
--                                                                           --
-------------------------------------------------------------------------------

with HAL; use HAL;
with Interfaces; use Interfaces;

with WNM.Mixer;
with WNM.Voices.Snare_Voice;
with WNM.Voices.Sampler_Voice;
with WNM.Voices.Speech_Voice;
with WNM.Voices.Chord_Voice;

with Tresses; use Tresses;
with Tresses.Drums.Kick;
with Tresses.Drums.Cymbal;
with Tresses.Voices.Macro;
with Tresses.Macro;
with Tresses.Interfaces;

with WNM.Generic_Queue;

package body WNM.Synth is

   TK          : aliased Tresses.Drums.Kick.Instance;
   TS          : aliased WNM.Voices.Snare_Voice.Instance;
   TC          : aliased Tresses.Drums.Cymbal.Instance;
   Lead        : aliased Tresses.Voices.Macro.Instance;
   Bass        : aliased Tresses.Voices.Macro.Instance;
   Chord       : aliased WNM.Voices.Chord_Voice.Instance;
   Speech      : aliased WNM.Voices.Speech_Voice.Instance;
   Sampler1    : aliased WNM.Voices.Sampler_Voice.Instance;
   Sampler2    : aliased WNM.Voices.Sampler_Voice.Instance;

   subtype Voice_Class is Tresses.Interfaces.Four_Params_Voice'Class;
   type Voice_Access is access all Voice_Class;

   subtype Lead_Engine_Range is MIDI.MIDI_Data range 0 .. 11;
   function Lead_Engines (V : MIDI.MIDI_Data) return Tresses.Engines
   is (case V is
          when 0  => Tresses.Voice_Saw_Swarm,
          when 1  => Tresses.Voice_Acid,
          when 2  => Tresses.Voice_Analog_Buzz,
          when 3  => Tresses.Voice_Analog_Morph,
          when 4  => Tresses.Voice_FM2OP,
          when 5  => Tresses.Voice_Sand,
          when 6  => Tresses.Voice_Bass_808,
          when 7  => Tresses.Voice_House_Bass,
          when 8  => Tresses.Voice_Pluck_Bass,
          when 9  => Tresses.Voice_Reese,
          when 10 => Tresses.Voice_Screech,
          when Lead_Engine_Range'Last .. MIDI.MIDI_Data'Last
                  => Tresses.Voice_Plucked);

   subtype Snare_Engine_Range is MIDI.MIDI_Data range 0 .. 1;
   function Snare_Engines (V : MIDI.MIDI_Data)
                           return Voices.Snare_Voice.Snare_Engine
   is (case V is
          --  when 0      => Voices.Snare_Voice.Snare,
          when 0      => Voices.Snare_Voice.Analog_Snare,
          when Snare_Engine_Range'Last .. MIDI.MIDI_Data'Last
                      => Voices.Snare_Voice.Clap);

   subtype Tresses_Channels
     is MIDI.MIDI_Channel range Kick_Channel .. Speech_Channel;

   Synth_Oct_Offset : constant array (Tresses_Channels) of Integer :=
     (Speech_Channel     => 0,
      Sample1_Channel    => 0,
      Sample2_Channel    => 0,
      Kick_Channel       => -4,
      Snare_Channel      => -1,
      Cymbal_Channel     => 0,
      Lead_Channel       => 0,
      Bass_Channel       => -3,
      Chord_Channel      => 0,
      Reverb_Channel     => 0,
      Filter_Channel     => 0,
      Drive_Channel      => 0,
      Bitcrusher_Channel => 0);

   Synth_Voices : constant array (Tresses_Channels) of
     Voice_Access :=
       (Speech_Channel     => Speech'Access,
        Sample1_Channel    => Sampler1'Access,
        Sample2_Channel    => Sampler2'Access,
        Kick_Channel       => TK'Access,
        Snare_Channel      => TS'Access,
        Cymbal_Channel     => TC'Access,
        Lead_Channel       => Lead'Access,
        Bass_Channel       => Bass'Access,
        Chord_Channel      => Chord'Access,
        Reverb_Channel     => WNM.Mixer.FX_Reverb'Access,
        Filter_Channel     => WNM.Mixer.FX_Filter'Access,
        Drive_Channel      => WNM.Mixer.FX_Drive'Access,
        Bitcrusher_Channel => WNM.Mixer.FX_Bitcrush'Access);

   LFO_Targets : array (Tresses_Channels) of MIDI.MIDI_Data :=
     (others => Voice_Pan_CC);
     --  (others => MIDI.MIDI_Data'Last);

   LFOs : array (Tresses_Channels) of Tresses.LFO.Instance;
   LFO_Syncs : array (Tresses_Channels) of Boolean := (others => False);
   LFO_Values : array (Tresses_Channels) of Tresses.S16;

   type Voice_Parameters_Array is array (Tresses_Channels) of
     Voice_Parameters;

   In_Voice_Parameters : Voice_Parameters_Array :=
     (others => (Voice_Volume_CC => Param_Range'Last / 2,
                 Voice_Pan_CC    => Param_Range'Last / 2,
                 Voice_Param_4_CC => Param_Range'Last,
                 others          => 0));
   Out_Voice_Parameters : Voice_Parameters_Array := (others => (others => 0));

   Last_Key : array (MIDI.MIDI_Channel) of MIDI.MIDI_Key := (others => 0);
   FX_Send : array (MIDI.MIDI_Channel) of FX_Kind := (others => Bypass);

   Pan_For_Chan : array (MIDI.MIDI_Channel) of WNM_HAL.Audio_Pan :=
     (others => WNM_HAL.Init_Pan);

   Volume_For_Chan : array (MIDI.MIDI_Channel) of WNM_HAL.Audio_Volume :=
     (others => WNM_HAL.Init_Volume);

   Recording_Source : Rec_Source;
   Recording_Size   : Natural;

   Passthrough : Audio_Input_Kind := Line_In;

   G_CPU_Load : CPU_Load := 0.0 with Volatile, Atomic;
   G_Max_CPU_Load : CPU_Load := 0.0 with Volatile, Atomic;
   G_Count_Missed_Deadlines : HAL.UInt32 := 0 with Volatile, Atomic;

   -- Coproc Event Queue --
   package Coproc_Event_Queues
   is new WNM.Generic_Queue (Coproc.Message, "Synth coproc");
   Coproc_Event_Queue : Coproc_Event_Queues.Instance
     (WNM_Configuration.Coproc_Queue_Capacity);

   procedure Next_Points (Output : out WNM.Mixer.FX_Send_Buffers);

   function To_Param (V : MIDI.MIDI_Data)
                      return Tresses.Param_Range
     with Inline_Always;

   function To_Volume (V : Tresses.Param_Range)
                       return WNM_HAL.Audio_Volume
     with Inline_Always;

   function To_Pan (V : Tresses.Param_Range)
                    return WNM_HAL.Audio_Pan
     with Inline_Always;

   function Add_Sat (K : MIDI.MIDI_Key; O : Integer) return MIDI.MIDI_Key
     with Inline_Always;

   function To_Shape (V : MIDI.MIDI_Data)
                      return Tresses.LFO.Shape_Kind
     with Inline_Always;

   --------------
   -- To_Param --
   --------------

   function To_Param (V : MIDI.MIDI_Data)
                      return Tresses.Param_Range
   is
   begin
      return Param_Range (V) *
        (Param_Range'Last / Param_Range (MIDI.MIDI_Data'Last));
   end To_Param;

   ---------------
   -- To_Volume --
   ---------------

   function To_Volume (V : Tresses.Param_Range)
                       return WNM_HAL.Audio_Volume
   is (WNM_HAL.Audio_Volume (V / 328));

   ------------
   -- To_Pan --
   ------------

   function To_Pan (V : Tresses.Param_Range)
                    return WNM_HAL.Audio_Pan
   is (WNM_HAL.Audio_Pan (V / 328));

   -------------
   -- Add_Sat --
   -------------

   function Add_Sat (K : MIDI.MIDI_Key; O : Integer) return MIDI.MIDI_Key is
      Result : constant Integer := Integer (K) + O;
   begin
      if Result > Integer (MIDI.MIDI_Key'Last) then
         return MIDI.MIDI_Key'Last;
      elsif Result < Integer (MIDI.MIDI_Key'First) then
         return MIDI.MIDI_Key'First;
      else
         return MIDI.MIDI_Key (Result);
      end if;
   end Add_Sat;

   --------------
   -- To_Shape --
   --------------

   function To_Shape (V : MIDI.MIDI_Data)
                      return Tresses.LFO.Shape_Kind
   is (case V is
          when 0 => Tresses.LFO.Sine,
          when 1 => Tresses.LFO.Triangle,
          when 2 => Tresses.LFO.Ramp_Up,
          when 3 => Tresses.LFO.Ramp_Down,
          when 4 => Tresses.LFO.Exp_Up,
          when others => Tresses.LFO.Exp_Down
      );

   -------------------
   -- Last_CPU_Load --
   -------------------

   function Last_CPU_Load return CPU_Load
   is (G_CPU_Load);

   ------------------
   -- Max_CPU_Load --
   ------------------

   function Max_CPU_Load return CPU_Load
   is (G_Max_CPU_Load);

   ----------------------
   -- Missed_Deadlines --
   ----------------------

   function Missed_Deadlines return HAL.UInt32
   is (G_Count_Missed_Deadlines);

   ------------------------
   -- Clear_Max_CPU_Load --
   ------------------------

   procedure Clear_Max_CPU_Load is
   begin
      G_Max_CPU_Load := 0.0;
   end Clear_Max_CPU_Load;

   ----------------------------
   -- Clear_Missed_Deadlines --
   ----------------------------

   procedure Clear_Missed_Deadlines is
   begin
      G_Count_Missed_Deadlines := 0;
   end Clear_Missed_Deadlines;

   ----------------------
   -- Push_Copro_Event --
   ----------------------

   procedure Push_Copro_Event (Msg : WNM.Coproc.Message) is
   begin
      Coproc_Event_Queues.Push (Coproc_Event_Queue, Msg);
   end Push_Copro_Event;

   ---------------------------
   -- Process_Coproc_Events --
   ---------------------------

   procedure Process_Coproc_Events is
      use MIDI;
      use WNM.Coproc;

      Msg : WNM.Coproc.Message;
      Success : Boolean;
   begin
      loop
         Coproc_Event_Queues.Pop (Coproc_Event_Queue, Msg, Success);

         exit when not Success;

         case Msg.Kind is

            when WNM.Coproc.Buffer_Available =>

               Set_Indicator_IO (GP19);
               Next_Points (WNM.Mixer.Mixer_Buffers (Msg.Buffer_Id));
               Clear_Indicator_IO (GP19);

               --  Send the buffers back to main CPU
               WNM.Coproc.Push_To_Main (Msg);

            when WNM.Coproc.MIDI_Event =>

               --  Ada.Text_IO.Put_Line ("Synth message is MIDI");
               --  Ada.Text_IO.Put_Line
               --    (Clock'Img & " - " &
               --     MIDI.Img (Msg.MIDI_Evt));

               if Msg.MIDI_Evt.Chan in Synth_Voices'Range then
                  declare
                     Voice : Voice_Class renames
                       Synth_Voices (Msg.MIDI_Evt.Chan).all;
                  begin
                     case Msg.MIDI_Evt.Kind is
                     when MIDI.Note_On =>

                        Set_Indicator_IO (GP16);
                        declare
                           Offset : constant Integer :=
                             Synth_Oct_Offset (Msg.MIDI_Evt.Chan) * 12;

                           Key : constant MIDI_Key :=
                             Add_Sat (Msg.MIDI_Evt.Key, Offset);
                        begin
                           if Msg.MIDI_Evt.Chan = Sample1_Channel then
                              Sampler1.Set_MIDI_Pitch (Key);
                           elsif Msg.MIDI_Evt.Chan = Sample2_Channel then
                              Sampler2.Set_MIDI_Pitch (Key);
                           elsif Msg.MIDI_Evt.Chan = Speech_Channel then
                              Speech.Set_MIDI_Pitch (Key);
                           elsif Msg.MIDI_Evt.Chan = Chord_Channel then

                              Chord.Key_On (Msg.MIDI_Evt.Key,
                                            To_Param
                                              (Msg.MIDI_Evt.Velocity));

                           else
                              Voice.Set_Pitch (Tresses.MIDI_Pitch
                                               (Standard.MIDI.MIDI_UInt8
                                                  (Key)));
                           end if;

                           Voice.Note_On (To_Param
                                          (Msg.MIDI_Evt.Velocity));

                           Last_Key (Msg.MIDI_Evt.Chan) := Key;

                           if LFO_Syncs (Msg.MIDI_Evt.Chan) then
                              LFOs (Msg.MIDI_Evt.Chan).Sync;
                           end if;
                        end;

                        Clear_Indicator_IO (GP16);
                     when MIDI.Note_Off =>

                        declare
                           Offset : constant Integer :=
                             Synth_Oct_Offset (Msg.MIDI_Evt.Chan) * 12;

                           Key : constant MIDI_Key :=
                             Add_Sat (Msg.MIDI_Evt.Key, Offset);
                        begin
                           if Msg.MIDI_Evt.Chan = Chord_Channel then
                              Chord.Key_Off (Msg.MIDI_Evt.Key);

                           elsif Last_Key (Msg.MIDI_Evt.Chan) = Key then
                              --  Only apply note_off if it matches the last
                              --  played key.
                              Voice.Note_Off;
                              Last_Key (Msg.MIDI_Evt.Chan) := 0;
                           end if;
                        end;

                     when MIDI.Continous_Controller =>
                        case Msg.MIDI_Evt.Controller is
                           when LFO_Compatible_CC =>

                              if Msg.MIDI_Evt.Chan = Sample1_Channel
                                and then
                                  Msg.MIDI_Evt.Controller = Voice_Param_1_CC
                              then
                                 Sampler1.Set_Sample
                                   (Msg.MIDI_Evt.Controller_Value);

                              elsif Msg.MIDI_Evt.Chan = Sample2_Channel
                                and then
                                  Msg.MIDI_Evt.Controller = Voice_Param_1_CC
                              then
                                 Sampler2.Set_Sample
                                   (Msg.MIDI_Evt.Controller_Value);

                              elsif Msg.MIDI_Evt.Chan = Speech_Channel
                                and then
                                  Msg.MIDI_Evt.Controller = Voice_Param_1_CC
                              then
                                 Speech.Set_Word
                                   (Msg.MIDI_Evt.Controller_Value);

                              else
                                 In_Voice_Parameters
                                   (Msg.MIDI_Evt.Chan)
                                   (Msg.MIDI_Evt.Controller)
                                   := To_Param
                                     (Msg.MIDI_Evt.Controller_Value);
                              end if;

                           when Voice_Engine_CC =>

                              case Msg.MIDI_Evt.Chan is
                              when Lead_Channel =>
                                 Lead.Set_Engine
                                   (Lead_Engines
                                      (Msg.MIDI_Evt.Controller_Value));
                              when Bass_Channel =>
                                 Bass.Set_Engine
                                   (Lead_Engines
                                      (Msg.MIDI_Evt.Controller_Value));
                                 when Snare_Channel =>
                                    TS.Set_Engine
                                      (Snare_Engines
                                         (Msg.MIDI_Evt.Controller_Value));
                              when others =>
                                 null;
                              end case;

                           when Voice_FX_CC =>
                              FX_Send (Msg.MIDI_Evt.Chan) :=
                                (case Msg.MIDI_Evt.Controller_Value is
                                    when FX_Select_Bypass     => Bypass,
                                    when FX_Select_Overdrive  => Overdrive,
                                    when FX_Select_Reverb     => Reverb,
                                    when FX_Select_Filter     => Filter,
                                    when FX_Select_Bitcrusher => Bitcrusher,
                                    when others => Bypass);

                           when Voice_LFO_Rate_CC =>
                              LFOs (Msg.MIDI_Evt.Chan).Set_Rate
                                (To_Param (Msg.MIDI_Evt.Controller_Value),
                                 WNM_Configuration.Audio.Samples_Per_Buffer);

                           when Voice_LFO_Shape_CC =>
                              LFOs (Msg.MIDI_Evt.Chan).Set_Shape
                                (To_Shape (Msg.MIDI_Evt.Controller_Value));

                           when Voice_LFO_Amp_CC =>
                              LFOs (Msg.MIDI_Evt.Chan).Set_Amplitude
                                (To_Param (Msg.MIDI_Evt.Controller_Value));

                           when Voice_LFO_Amp_Mode_CC =>
                              LFOs (Msg.MIDI_Evt.Chan).Set_Amp_Mode
                                (Tresses.LFO.Amplitude_Kind'Enum_Val
                                   (Msg.MIDI_Evt.Controller_Value));

                           when Voice_LFO_Target_CC =>
                              LFO_Targets (Msg.MIDI_Evt.Chan) :=
                                Msg.MIDI_Evt.Controller_Value;

                           when Voice_LFO_Loop_CC =>
                              LFOs (Msg.MIDI_Evt.Chan).Set_Loop_Mode
                                (if Msg.MIDI_Evt.Controller_Value /= 0
                                 then Tresses.LFO.Repeat
                                 else Tresses.LFO.One_Shot);

                           when Voice_LFO_Sync_CC =>
                              LFO_Syncs (Msg.MIDI_Evt.Chan) :=
                                Msg.MIDI_Evt.Controller_Value /= 0;

                           when others =>
                              null;
                        end case;
                     when others =>
                        null;
                     end case;
                  end;
               end if;

            when Synth_CPU_Crash =>
               raise Program_Error with "Unexpected coproc message for synth";
         end case;
      end loop;
   end Process_Coproc_Events;

   -----------------
   -- Next_Points --
   -----------------

   procedure Next_Points (Output : out WNM.Mixer.FX_Send_Buffers)
   is
      Buffer, Aux_Buffer : WNM_HAL.Mono_Buffer;

      --------------
      -- Add_Clip --
      --------------

      function Add_Clip (A : Param_Range; B : S16) return Param_Range is
         Result : constant S32 := S32 (A) + S32 (B);
      begin
         if Result > S32 (Param_Range'Last) then
            return Param_Range'Last;
         elsif Result < S32 (Param_Range'First) then
            return Param_Range'First;
         else
            return Param_Range (Result);
         end if;
      end Add_Clip;

      Synthesis_Start : constant WNM_HAL.Time_Microseconds := WNM_HAL.Clock;
   begin

      --  Take input params
      Out_Voice_Parameters := In_Voice_Parameters;

      --  Compute LFOs
      for Chan in LFOs'Range loop
         LFO_Values (Chan) := LFOs (Chan).Render;
      end loop;

      --  Apply LFOs
      for Chan in Tresses_Channels loop
         if LFO_Targets (Chan) in LFO_Compatible_CC then
            Out_Voice_Parameters (Chan)(LFO_Targets (Chan)) :=
              Add_Clip (Out_Voice_Parameters (Chan)(LFO_Targets (Chan)),
                        LFO_Values (Chan));
         end if;
      end loop;

      --  Set params
      for Chan in Tresses_Channels loop
         Synth_Voices (Chan).Set_Param
           (1, Out_Voice_Parameters (Chan)(Voice_Param_1_CC));
         Synth_Voices (Chan).Set_Param
           (2, Out_Voice_Parameters (Chan)(Voice_Param_2_CC));
         Synth_Voices (Chan).Set_Param
           (3, Out_Voice_Parameters (Chan)(Voice_Param_3_CC));
         Synth_Voices (Chan).Set_Param
           (4, Out_Voice_Parameters (Chan)(Voice_Param_4_CC));

         Pan_For_Chan (Chan) := To_Pan
           (Out_Voice_Parameters (Chan)(Voice_Pan_CC));

         Volume_For_Chan (Chan) := To_Volume
           (Out_Voice_Parameters (Chan)(Voice_Volume_CC));

      end loop;

      --  Send the FX parameters in FX buffer
      Output.Parameters (Overdrive) := Out_Voice_Parameters (Drive_Channel);
      Output.Parameters (Bitcrusher) :=
        Out_Voice_Parameters (Bitcrusher_Channel);
      Output.Parameters (Filter) := Out_Voice_Parameters (Filter_Channel);
      Output.Parameters (Reverb) := Out_Voice_Parameters (Reverb_Channel);

      declare
      begin
         TK.Render (Buffer);
         WNM_HAL.Mix (Output.L (FX_Send (Kick_Channel)),
                      Output.R (FX_Send (Kick_Channel)),
                      Input => Buffer,
                      Volume => Volume_For_Chan (Kick_Channel),
                      Pan => Pan_For_Chan (Kick_Channel));

         TS.Render (Buffer);
         WNM_HAL.Mix (Output.L (FX_Send (Snare_Channel)),
                      Output.R (FX_Send (Snare_Channel)),
                      Input => Buffer,
                      Volume => Volume_For_Chan (Snare_Channel),
                      Pan => Pan_For_Chan (Snare_Channel));

         TC.Render (Buffer);
         WNM_HAL.Mix (Output.L (FX_Send (Cymbal_Channel)),
                      Output.R (FX_Send (Cymbal_Channel)),
                      Input => Buffer,
                      Volume => Volume_For_Chan (Cymbal_Channel),
                      Pan => Pan_For_Chan (Cymbal_Channel));

         Lead.Render (Buffer, Aux_Buffer);
         WNM_HAL.Mix (Output.L (FX_Send (Lead_Channel)),
                      Output.R (FX_Send (Lead_Channel)),
                      Input => Buffer,
                      Volume => Volume_For_Chan (Lead_Channel),
                      Pan => Pan_For_Chan (Lead_Channel));

         Bass.Render (Buffer, Aux_Buffer);
         WNM_HAL.Mix (Output.L (FX_Send (Bass_Channel)),
                      Output.R (FX_Send (Bass_Channel)),
                      Input => Buffer,
                      Volume => Volume_For_Chan (Bass_Channel),
                      Pan => Pan_For_Chan (Bass_Channel));

         Chord.Render (Buffer);
         WNM_HAL.Mix (Output.L (FX_Send (Chord_Channel)),
                      Output.R (FX_Send (Chord_Channel)),
                      Input => Buffer,
                      Volume => Volume_For_Chan (Chord_Channel),
                      Pan => Pan_For_Chan (Chord_Channel));

         Sampler1.Render (Buffer);
         WNM_HAL.Mix (Output.L (FX_Send (Sample1_Channel)),
                      Output.R (FX_Send (Sample1_Channel)),
                      Input => Buffer,
                      Volume => Volume_For_Chan (Sample1_Channel),
                      Pan => Pan_For_Chan (Sample1_Channel));

         Sampler2.Render (Buffer);
         WNM_HAL.Mix (Output.L (FX_Send (Sample2_Channel)),
                      Output.R (FX_Send (Sample2_Channel)),
                      Input => Buffer,
                      Volume => Volume_For_Chan (Sample2_Channel),
                      Pan => Pan_For_Chan (Sample2_Channel));

         --  Speech.Render (Buffer);
         --  WNM_HAL.Mix (Output.L (FX_Send (Speech_Channel)),
         --               Output.R (FX_Send (Speech_Channel)),
         --               Input => Buffer,
         --               Volume => Volume_For_Chan (Speech_Channel),
         --               Pan => Pan_For_Chan (Speech_Channel));
      end;

      declare
         Synthesis_End      : constant WNM_HAL.Time_Microseconds :=
           WNM_HAL.Clock;
         Synthesis_Duration : constant WNM_HAL.Time_Microseconds :=
           Synthesis_End - Synthesis_Start;

         Synthesis_Duration_Float : constant Float :=
           Float (Synthesis_Duration) / 1_000_000.0;

         Synthesized_Time : constant Float :=
           (1.0 / Float (WNM_Configuration.Audio.Sample_Frequency) *
                Float (WNM_Configuration.Audio.Samples_Per_Buffer));
      begin
         G_CPU_Load :=
           CPU_Load (Synthesis_Duration_Float / Synthesized_Time) * 100.0;

         if G_CPU_Load > 100.0 then
            G_Count_Missed_Deadlines := G_Count_Missed_Deadlines + 1;
         end if;

         if G_CPU_Load > G_Max_CPU_Load then
            G_Max_CPU_Load := G_CPU_Load;
         end if;
      end;

      WNM_HAL.Clear_Indicator_IO (WNM_HAL.GP19);

   end Next_Points;

   ---------------------
   -- Set_Passthrough --
   ---------------------

   procedure Set_Passthrough (Kind : Audio_Input_Kind) is
   begin
      Select_Audio_Input (Kind);
      Passthrough := Kind;
   end Set_Passthrough;

   ---------------------
   -- Get_Passthrough --
   ---------------------

   function Get_Passthrough return Audio_Input_Kind is
   begin
      return Passthrough;
   end Get_Passthrough;

   ----------------------
   -- Lead_Engine_Last --
   ----------------------

   function Lead_Engine_Last return MIDI.MIDI_Data
   is (Lead_Engine_Range'Last);

   ---------------------
   -- Lead_Engine_Img --
   ---------------------

   function Lead_Engine_Img (Engine : MIDI.MIDI_Data) return String
   is (Tresses.Img (Lead_Engines (Engine)));

   ----------------------
   -- Lead_Param_Label --
   ----------------------

   function Lead_Param_Label (Engine : MIDI.MIDI_Data;
                              Id : Tresses.Param_Id)
                              return String
   is
      E : constant Tresses.Engines := Lead_Engines (Engine);
   begin
      return Tresses.Macro.Param_Label (E, Id);
   end Lead_Param_Label;

   ----------------------------
   -- Lead_Param_Short_Label --
   ----------------------------

   function Lead_Param_Short_Label (Engine : MIDI.MIDI_Data;
                                    Id : Tresses.Param_Id)
                                    return Tresses.Short_Label
   is
      E : constant Tresses.Engines := Lead_Engines (Engine);
   begin
      return Tresses.Macro.Param_Short_Label (E, Id);
   end Lead_Param_Short_Label;

   ----------------------
   -- Kick_Param_Label --
   ----------------------

   function Kick_Param_Label (Id : Tresses.Param_Id) return String
   is (Tresses.Macro.Param_Label (Tresses.Drum_Kick, Id));

   ----------------------------
   -- Kick_Param_Short_Label --
   ----------------------------

   function Kick_Param_Short_Label (Id : Tresses.Param_Id)
                                    return Tresses.Short_Label
   is (Tresses.Macro.Param_Short_Label (Tresses.Drum_Kick, Id));

   -----------------------
   -- Snare_Engine_Last --
   -----------------------

   function Snare_Engine_Last return MIDI.MIDI_Data
   is (Snare_Engine_Range'Last);

   ----------------------
   -- Snare_Engine_Img --
   ----------------------

   function Snare_Engine_Img (Engine : MIDI.MIDI_Data) return String
   is (WNM.Voices.Snare_Voice.Img (Snare_Engines (Engine)));

   -----------------------
   -- Snare_Param_Label --
   -----------------------

   function Snare_Param_Label (Id : Tresses.Param_Id) return String
   is (TS.Param_Label (Id));

   -----------------------------
   -- Snare_Param_Short_Label --
   -----------------------------

   function Snare_Param_Short_Label (Id : Tresses.Param_Id)
                                    return Tresses.Short_Label
   is (TS.Param_Short_Label (Id));

   ------------------------
   -- Cymbal_Param_Label --
   ------------------------

   function Cymbal_Param_Label (Id : Tresses.Param_Id) return String
   is (Tresses.Macro.Param_Label (Tresses.Drum_Cymbal, Id));

   ------------------------------
   -- Cymbal_Param_Short_Label --
   ------------------------------

   function Cymbal_Param_Short_Label (Id : Tresses.Param_Id)
                                      return Tresses.Short_Label
   is (Tresses.Macro.Param_Short_Label (Tresses.Drum_Cymbal, Id));

   -------------------------
   -- Sampler_Param_Label --
   -------------------------

   function Sampler_Param_Label (Id : Tresses.Param_Id)
                                 return String
   is (Sampler1.Param_Label (Id));

   -------------------------------
   -- Sampler_Param_Short_Label --
   -------------------------------

   function Sampler_Param_Short_Label (Id : Tresses.Param_Id)
                                       return Tresses.Short_Label
   is (Sampler1.Param_Short_Label (Id));

   ------------------------
   -- Reverb_Param_Label --
   ------------------------

   function Reverb_Param_Label (Id : Tresses.Param_Id)
                                 return String
   is (Mixer.FX_Reverb.Param_Label (Id));

   ------------------------------
   -- Reverb_Param_Short_Label --
   ------------------------------

   function Reverb_Param_Short_Label (Id : Tresses.Param_Id)
                                       return Tresses.Short_Label
   is (Mixer.FX_Reverb.Param_Short_Label (Id));

   ------------------------
   -- Filter_Param_Label --
   ------------------------

   function Filter_Param_Label (Id : Tresses.Param_Id)
                                return String
   is (Mixer.FX_Filter.Param_Label (Id));

   ------------------------------
   -- Filter_Param_Short_Label --
   ------------------------------

   function Filter_Param_Short_Label (Id : Tresses.Param_Id)
                                      return Tresses.Short_Label
   is (Mixer.FX_Filter.Param_Short_Label (Id));

   -----------------------
   -- Drive_Param_Label --
   -----------------------

   function Drive_Param_Label (Id : Tresses.Param_Id)
                               return String
   is (Mixer.FX_Drive.Param_Label (Id));

   -----------------------------
   -- Drive_Param_Short_Label --
   -----------------------------

   function Drive_Param_Short_Label (Id : Tresses.Param_Id)
                                     return Tresses.Short_Label
   is (Mixer.FX_Drive.Param_Short_Label (Id));

   --------------------------
   -- Bitcrush_Param_Label --
   --------------------------

   function Bitcrush_Param_Label (Id : Tresses.Param_Id)
                               return String
   is (Mixer.FX_Bitcrush.Param_Label (Id));

   --------------------------------
   -- Bitcrush_Param_Short_Label --
   --------------------------------

   function Bitcrush_Param_Short_Label (Id : Tresses.Param_Id)
                                        return Tresses.Short_Label
   is (Mixer.FX_Bitcrush.Param_Short_Label (Id));

   ------------------------
   -- Speech_Param_Label --
   ------------------------

   function Speech_Param_Label (Id : Tresses.Param_Id)
                                return String
   is (Speech.Param_Label (Id));

   ------------------------------
   -- Speech_Param_Short_Label --
   ------------------------------

   function Speech_Param_Short_Label (Id : Tresses.Param_Id)
                                      return Tresses.Short_Label
   is (Speech.Param_Short_Label (Id));

   -----------------------
   -- Chord_Param_Label --
   -----------------------

   function Chord_Param_Label (Id : Tresses.Param_Id)
                               return String
   is (Chord.Param_Label (Id));

   -----------------------------
   -- Chord_Param_Short_Label --
   -----------------------------

   function Chord_Param_Short_Label (Id : Tresses.Param_Id)
                                     return Tresses.Short_Label
   is (Chord.Param_Short_Label (Id));

   -------------------
   -- Now_Recording --
   -------------------

   function Now_Recording return Rec_Source
   is (Recording_Source);

   ---------------------
   -- Start_Recording --
   ---------------------

   procedure Start_Recording (Filename : String;
                              Source   : Rec_Source;
                              Max_Size : Positive)
   is
      pragma Unreferenced (Max_Size, Filename);
   begin
      if Recording_Source /= None then
         return;
      end if;

      Recording_Source := Source;
      Recording_Size := 0;
   end Start_Recording;

   --------------------
   -- Stop_Recording --
   --------------------

   procedure Stop_Recording is
   begin
      Recording_Source := None;
   end Stop_Recording;

   -----------------
   -- Record_Size --
   -----------------

   function Record_Size return Natural
   is (Recording_Size);

begin
   Lead.Set_Engine (Voice_Saw_Swarm);
end WNM.Synth;
