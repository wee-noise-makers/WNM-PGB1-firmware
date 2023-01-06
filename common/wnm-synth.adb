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
with WNM.Sample_Stream;          use WNM.Sample_Stream;
with WNM.Sample_Library;         use WNM.Sample_Library;
with WNM.Coproc;
with MIDI;

with WNM.Speech;

with Tresses.Drums.Kick;
with Tresses.Drums.Clap;
with Tresses.Drums.Cymbal;
with Tresses.Voices.Macro;
with Tresses.Macro;
with Tresses.Interfaces;

package body WNM.Synth is

   TK   : aliased Tresses.Drums.Kick.Instance;
   TS   : aliased Tresses.Drums.Clap.Instance;
   TC   : aliased Tresses.Drums.Cymbal.Instance;
   Lead : aliased Tresses.Voices.Macro.Instance;

   subtype Voice_Class is Tresses.Interfaces.Four_Params_Voice'Class;
   type Voice_Access is access all Voice_Class;

   function Lead_Engines (V : MIDI.MIDI_Data) return Tresses.Synth_Engines
   is (case V is
          when 0 => Tresses.Voice_Saw_Swarm,
          when 1 => Tresses.Voice_Acid,
          when 2 => Tresses.Voice_Analog_Buzz,
          when 3 => Tresses.Voice_Analog_Morph,
          when 4 => Tresses.Voice_FM2OP,
          when others => Tresses.Voice_Plucked);

   Kick_Channel   : constant MIDI.MIDI_Channel := 1;
   Snare_Channel  : constant MIDI.MIDI_Channel := 2;
   Cymbal_Channel : constant MIDI.MIDI_Channel := 3;
   Lead_Channel   : constant MIDI.MIDI_Channel := 4;

   Synth_Voices : constant array (MIDI.MIDI_Channel range 1 .. 4) of
     Voice_Access :=
       (Kick_Channel   => TK'Access,
        Snare_Channel  => TS'Access,
        Cymbal_Channel => TC'Access,
        Lead_Channel   => Lead'Access);

   Recording_Source : Rec_Source;
   Recording_Size   : Natural;

   Pan_For_Track : array (WNM.Tracks) of WNM_HAL.Audio_Pan :=
     (others => WNM_HAL.Init_Pan);

   Volume_For_Track : array (WNM.Tracks) of WNM_HAL.Audio_Volume :=
     (others => WNM_HAL.Init_Volume);

   Passthrough : Audio_Input_Kind := Line_In;

   Next_Start : WNM.Time.Time_Microseconds := WNM.Time.Time_Microseconds'First;
   Glob_Sample_Clock : Sample_Time := 0 with Volatile;

   procedure Copy_Stereo_To_Mono (L, R : Mono_Buffer;
                                  Dst : out Mono_Buffer);
   pragma Unreferenced (Copy_Stereo_To_Mono);

   procedure Process_Coproc_Events;

   -------------------------------
   -- MIDI_Val_To_Tresses_Param --
   -------------------------------

   function MIDI_Val_To_Tresses_Param (V : MIDI.MIDI_Data)
                                       return Tresses.Param_Range
   is
      use Tresses;
   begin
      return Param_Range (V) *
        (Param_Range'Last / Param_Range (MIDI.MIDI_Data'Last));
   end MIDI_Val_To_Tresses_Param;

   ------------------
   -- Sample_Clock --
   ------------------

   function Sample_Clock return Sample_Time
   is (Glob_Sample_Clock);

   -------------------------
   -- Copy_Stereo_To_Mono --
   -------------------------

   procedure Copy_Stereo_To_Mono (L, R : Mono_Buffer;
                                  Dst : out Mono_Buffer)
   is
      Tmp  : Integer_32;
   begin

      for Index in Dst'Range loop
         Tmp := Integer_32 (L (Index)) + Integer_32 (R (Index));
         Tmp := Tmp / 2;

         if Tmp > Integer_32 (Mono_Point'Last) then
            Dst (Index) := Mono_Point'Last;
         elsif Tmp < Integer_32 (Integer_16'First) then
            Dst (Index) := Mono_Point'First;
         else
            Dst (Index) := Mono_Point (Tmp);
         end if;
      end loop;
   end Copy_Stereo_To_Mono;

   ----------
   -- Trig --
   ----------

   procedure Trig (Track  : Sample_Stream.Stream_Track;
                   Sample : Sample_Library.Valid_Sample_Index)
   is
   begin
      Start (Track       => Track,
             Sample      => Sample,
             Start_Point => Sample_Library.Sample_Point_Index'First,
             End_Point   => Sample_Library.Sample_Point_Index'Last,
             Looping     => False);
   end Trig;

   ---------------------------
   -- Process_Coproc_Events --
   ---------------------------

   procedure Process_Coproc_Events is
      Msg : WNM.Coproc.Message;
      Success : Boolean;
   begin
      loop
         WNM.Coproc.Pop (Msg, Success);

         exit when not Success;

         case Msg.Kind is

            when WNM.Coproc.Sampler_Event =>
               if Msg.Sampler_Evt.On then
                  Trig (Msg.Sampler_Evt.Track, Msg.Sampler_Evt.Sample);
               end if;

            when WNM.Coproc.Speech_Event =>
               if Msg.Speech_Evt.On then
                  WNM.Speech.Start (Msg.Speech_Evt.Track,
                                    Msg.Speech_Evt.W,
                                    Msg.Speech_Evt.Key);
               else
                  WNM.Speech.Stop (Msg.Speech_Evt.Track);
               end if;

            when WNM.Coproc.Speech_CC_Event =>

               WNM.Speech.Set_Stretch (Msg.Speech_CC_Evt.Track,
                                       Msg.Speech_CC_Evt.Stretch);

            when WNM.Coproc.Track_Vol_Pan =>
               Volume_For_Track (Msg.TVP_Track) := Msg.TVP_Vol;
               Pan_For_Track (Msg.TVP_Track) := Msg.TVP_Pan;

            when WNM.Coproc.MIDI_Event =>

               if Msg.MIDI_Evt.Chan in Synth_Voices'Range then
                  declare
                     use MIDI;

                     Voice : Voice_Class renames
                       Synth_Voices (Msg.MIDI_Evt.Chan).all;
                  begin
                     case Msg.MIDI_Evt.Kind is
                     when MIDI.Note_On =>
                        Voice.Set_Pitch (Tresses.MIDI_Pitch
                                         (Standard.MIDI.MIDI_UInt8
                                            (Msg.MIDI_Evt.Key)));
                        Voice.Strike;

                     when MIDI.Continous_Controller =>
                        if Msg.MIDI_Evt.Controller <= 3 then
                           Voice.Set_Param
                             (Tresses.Param_Id
                                (Msg.MIDI_Evt.Controller + 1),
                              MIDI_Val_To_Tresses_Param
                                (Msg.MIDI_Evt.Controller_Value));

                        elsif Msg.MIDI_Evt.Controller = 4
                          and then
                            Msg.MIDI_Evt.Chan = Lead_Channel
                        then
                           Lead.Set_Engine
                             (Lead_Engines (Msg.MIDI_Evt.Controller_Value));
                        end if;

                     when others =>
                        null;
                     end case;
                  end;
               end if;
         end case;
      end loop;
   end Process_Coproc_Events;

   ------------
   -- Update --
   ------------

   function Update return WNM.Time.Time_Microseconds is
      Now : constant WNM.Time.Time_Microseconds := WNM.Time.Clock;
   begin
      if Now >= Next_Start then
         Next_Start := Next_Start + 0;

         Process_Coproc_Events;

         --  Generate_Audio;
      end if;

      return Next_Start;
   end Update;

   -----------------
   -- Next_Points --
   -----------------

   procedure Next_Points (Output : out WNM_HAL.Stereo_Buffer;
                          Input  :     WNM_HAL.Stereo_Buffer)
   is
   begin
      if Passthrough /= None then
         Output := Input;
      else
         Output := (others => (0, 0));
      end if;

      declare
         Sample_Buf : Mono_Buffer;
         Success : Boolean;
      begin
         for Track in Stream_Track loop
            Next_Buffer (Track, Sample_Buf, Success);
            if Success then
               WNM_HAL.Mix (Output,
                            Sample_Buf,
                            Volume_For_Track (To_Track (Track)),
                            Pan_For_Track (To_Track (Track)));
            end if;
         end loop;
      end;

      -- Speech synth --
      Speech.Next_Points (Output);

      declare
         Buffer, Aux_Buffer : Mono_Buffer;

      begin
         TK.Render (Buffer);
         WNM_HAL.Mix (Output => Output,
                      Input => Buffer,
                      Volume => 50,
                      Pan => 50);

         TS.Render (Buffer);
         WNM_HAL.Mix (Output => Output,
                      Input => Buffer,
                      Volume => 50,
                      Pan => 50);

         TC.Render (Buffer);
         WNM_HAL.Mix (Output => Output,
                      Input => Buffer,
                      Volume => 50,
                      Pan => 50);

         Lead.Render (Buffer, Aux_Buffer);
         WNM_HAL.Mix (Output => Output,
                      Input => Buffer,
                      Volume => 50,
                      Pan => 50);
      end;

      -- Recording --
      --  if Recording_Source /= None then
      --     declare
      --        Sample_Buf : Mono_Buffer;
      --     begin
      --        case Recording_Source is
      --           when None =>
      --              null;
      --           when Line_In =>
      --              Copy_Stereo_To_Mono (In_L, In_R, Sample_Buf);
      --           when Master_Output =>
      --              Copy_Stereo_To_Mono (Out_L, Out_R, Sample_Buf);
      --        end case;
      --
      --        Len := Write (Recording_File, Sample_Buf'Address,
      --                      Sample_Buf'Length * 2);
      --        Recording_Size := Recording_Size + Len;
      --     end;
      --  end if;

      Glob_Sample_Clock :=
        Glob_Sample_Clock + WNM_Configuration.Audio.Samples_Per_Buffer;
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
      E : constant Tresses.Synth_Engines := Lead_Engines (Engine);
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
      E : constant Tresses.Synth_Engines := Lead_Engines (Engine);
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
   -- Snare_Param_Label --
   -----------------------

   function Snare_Param_Label (Id : Tresses.Param_Id) return String
   is (Tresses.Macro.Param_Label (Tresses.Drum_Snare, Id));

   -----------------------------
   -- Snare_Param_Short_Label --
   -----------------------------

   function Snare_Param_Short_Label (Id : Tresses.Param_Id)
                                    return Tresses.Short_Label
   is (Tresses.Macro.Param_Short_Label (Tresses.Drum_Snare, Id));

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
   Lead.Set_Engine (Tresses.Voice_Saw_Swarm);
   Lead.Set_Param (3, 0);
   Lead.Set_Param (4, Tresses.Param_Range'Last);
end WNM.Synth;
