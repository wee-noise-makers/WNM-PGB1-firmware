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

pragma Warnings (Off, "hides compilation unit");
with WNM.MIDI;
pragma Warnings (On, "hides compilation unit");

with WNM.Speech;

with Tresses.Drums.Kick;
with Tresses.Drums.Snare;
with Tresses.Drums.Cymbal;
with Tresses.Voices.Macro;

package body WNM.Synth is

   TK : Tresses.Drums.Kick.Instance;
   TS : Tresses.Drums.Snare.Instance;
   TC : Tresses.Drums.Cymbal.Instance;
   Lead : Tresses.Voices.Macro.Instance;

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

            when WNM.Coproc.Synth_Event =>
               declare
                  P1 : constant Tresses.Param_Range :=
                    MIDI_Val_To_Tresses_Param (Msg.Synth_Evt.P1);
                  P2 : constant Tresses.Param_Range :=
                    MIDI_Val_To_Tresses_Param (Msg.Synth_Evt.P2);
                  Pitch : constant Tresses.Pitch_Range :=
                    Tresses.MIDI_Pitch
                      (Standard.MIDI.MIDI_UInt8 (Msg.Synth_Evt.Key));
               begin
                  case Msg.Synth_Evt.Voice is
                  when WNM.Coproc.Kick =>
                     TK.Set_Param (1, P1);
                     TK.Set_Param (2, P2);
                     TK.Set_Pitch (Pitch);
                     if Msg.Synth_Evt.Trigger then
                        TK.Strike;
                     end if;
                  when WNM.Coproc.Snare =>
                     TS.Set_Param (1, P1);
                     TS.Set_Param (2, P2);
                     TS.Set_Pitch (Pitch);
                     if Msg.Synth_Evt.Trigger then
                        TS.Strike;
                     end if;
                  when WNM.Coproc.Cymbal =>
                     TC.Set_Param (1, P1);
                     TC.Set_Param (2, P2);
                     TC.Set_Pitch (Pitch);
                     if Msg.Synth_Evt.Trigger then
                        TC.Strike;
                     end if;
                  when WNM.Coproc.Lead =>
                     Lead.Set_Param (1, P1);
                     Lead.Set_Param (2, P2);
                     Lead.Set_Pitch (Pitch);
                     if Msg.Synth_Evt.Trigger then
                        Lead.Strike;
                     end if;
                  end case;
               end;
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
         Buffer : Tresses.Mono_Buffer (Mono_Buffer'Range);
         Buffer_B : Tresses.Mono_Buffer (Mono_Buffer'Range);

         procedure Mix is
            subtype S32 is Integer_32;
            L, R : S32;
         begin
            for Idx in Buffer'Range loop
               L := S32 (Output (Idx).L) + S32 (Buffer (Idx));

               L := S32'Min (L, S32 (Mono_Point'Last));
               L := S32'Max (L, S32 (Mono_Point'First));
               Output (Idx).L := Mono_Point (L);

               R := S32 (Output (Idx).R) + S32 (Buffer (Idx));
               R := S32'Min (R, S32 (Mono_Point'Last));
               R := S32'Max (R, S32 (Mono_Point'First));
               Output (Idx).R := Mono_Point (R);
            end loop;
         end Mix;
      begin
         TK.Render (Buffer);
         Mix;

         TS.Render (Buffer);
         Mix;

         TC.Render (Buffer);
         Mix;

         Lead.Render (Buffer, Buffer_B);
         Mix;
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
