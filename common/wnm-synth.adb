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
with WNM;                        use WNM;
with WNM.Sample_Library;         use WNM.Sample_Library;
with WNM.Audio;                  use WNM.Audio;
with WNM.Coproc;

with WNM.Speech;
with WNM.Project;

package body WNM.Synth is

   Recording_Source : Rec_Source;
   Recording_Size   : Natural;

   Pan_For_Track : array (WNM.Tracks) of Project.Audio_Pan :=
     (others => 0);

   Volume_For_Track : array (WNM.Tracks) of Project.Audio_Volume :=
     (others => 50);

   Passthrough : Audio_Input_Kind := Line_In;

   Next_Start : WNM.Time.Time_Microseconds := WNM.Time.Time_Microseconds'First;
   Glob_Sample_Clock : Sample_Time := 0 with Volatile;

   procedure Copy_Stereo_To_Mono (L, R : Mono_Buffer;
                                  Dst : out Mono_Buffer);
   pragma Unreferenced (Copy_Stereo_To_Mono);

   procedure Process_Coproc_Events;

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

            when WNM.Coproc.Track_Vol_Pan =>
               Volume_For_Track (Msg.TVP_Track) := Msg.TVP_Vol;
               Pan_For_Track (Msg.TVP_Track) := Msg.TVP_Pan;
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

   procedure Next_Points (Output : out Audio.Stereo_Buffer;
                          Input  :     Audio.Stereo_Buffer)
   is
      procedure Mix (Mono_Points : Mono_Buffer;
                     ST          : Stream_Track);

         ---------
         -- Mix --
         ---------

      procedure Mix (Mono_Points : Mono_Buffer;
                     ST          : Stream_Track)
      is
         Val         : Integer_32;

         Sample, Left, Right : Float;

         Volume       : Float;
         Pan          : Float;
      begin
         if ST = Always_On then
            Volume := 1.0;
            Pan    := 0.0;
         else
            Volume := Float (Volume_For_Track (To_Track (ST))) / 50.0;
            Pan    := Float (Pan_For_Track (To_Track (ST))) / 100.0;
         end if;

         for Index in Mono_Points'Range loop

            Sample := Float (Mono_Points (Index));
            Sample := Sample * Volume;

            Right := Sample * (1.0 - Pan);
            Left  := Sample * (1.0 + Pan);

            Val := Integer_32 (Output (Index).L) + Integer_32 (Left);
            if Val > Integer_32 (Mono_Point'Last) then
               Output (Index).L := Mono_Point'Last;
            elsif Val < Integer_32 (Mono_Point'First) then
               Output (Index).L := Mono_Point'First;
            else
               Output (Index).L := Mono_Point (Val);
            end if;

            Val := Integer_32 (Output (Index).R) + Integer_32 (Right);
            if Val > Integer_32 (Mono_Point'Last) then
               Output (Index).R := Mono_Point'Last;
            elsif Val < Integer_32 (Mono_Point'First) then
               Output (Index).R := Mono_Point'First;
            else
               Output (Index).R := Mono_Point (Val);
            end if;

         end loop;
      end Mix;

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
               Mix (Sample_Buf, Track);
            end if;
         end loop;
      end;

      -- Speech synth --
      Speech.Next_Points (Output);

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

      Glob_Sample_Clock := Glob_Sample_Clock + Samples_Per_Buffer;
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

end WNM.Synth;
