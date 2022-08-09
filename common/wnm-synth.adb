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
--  with WNM.Sample_Library;         use WNM.Sample_Library;
with WNM.Audio;                  use WNM.Audio;
with WNM.File_System;            use WNM.File_System;
with WNM.MIDI.Queues;
--  with Semihosting;

with WNM.Sequencer;

package body WNM.Synth is

   Recording_File   : aliased File_Descriptor;
   Recording_Source : Rec_Source;
   Recording_Size   : File_Signed_Size;

   Pan_For_Track : array (WNM.Tracks) of Integer := (others => 0)
     with Atomic_Components;
   Volume_For_Track : array (WNM.Tracks) of Integer := (others => 50)
     with Atomic_Components;

   Passthrough : Input_Kind := Line_In;

   Next_Start : WNM.Time.Time_Microseconds := WNM.Time.Time_Microseconds'First;
   Glob_Sample_Clock : Sample_Time := 0 with Volatile;

   procedure Copy_Stereo_To_Mono (L, R : Mono_Buffer;
                                  Dst : out Mono_Buffer);

   procedure Process_MIDI_Events;

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

   -----------
   -- Event --
   -----------

   procedure Event (Msg : MIDI.Message) is
      Track : constant WNM.Tracks := WNM.MIDI.To_Track (Msg.Chan);
   begin

      case Msg.Kind is
         when MIDI.Note_On =>
            Trig (Track);
         when  MIDI.Note_Off =>
            null;
         when others =>
            null;
      end case;
   end Event;

   ----------
   -- Trig --
   ----------

   procedure Trig (Track : WNM.Tracks) is
   begin
      Start (Track       => To_Stream_Track (Track),
             Sample      => Sequencer.Selected_Sample (Track),
             Start_Point => Sample_Library.Sample_Point_Index'First,
             End_Point   => Sample_Library.Sample_Point_Index'Last,
             Looping     => False);
   end Trig;

   ----------------
   -- Change_Pan --
   ----------------

   procedure Change_Pan (Track : WNM.Tracks;
                         Pan   : Integer)
   is
   begin
      Pan_For_Track (Track) := Pan_For_Track (Track) + Pan * 10;
      if Pan_For_Track (Track) > 100 then
         Pan_For_Track (Track) := 100;
      elsif Pan_For_Track (Track) < -100 then
         Pan_For_Track (Track) := -100;
      end if;
   end Change_Pan;

   ---------
   -- Pan --
   ---------

   function Pan (Track : WNM.Tracks) return Integer
   is (Pan_For_Track (Track));

   -------------------
   -- Change_Volume --
   -------------------

   procedure Change_Volume (Track  : WNM.Tracks;
                            Volume : Integer)
   is
   begin
      Volume_For_Track (Track) := Volume_For_Track (Track) + Volume;
      if Volume_For_Track (Track) > 100 then
         Volume_For_Track (Track) := 100;
      elsif Volume_For_Track (Track) < 0 then
         Volume_For_Track (Track) := 0;
      end if;
   end Change_Volume;

   ------------
   -- Volume --
   ------------

   function Volume (Track : WNM.Tracks) return Natural
   is (Natural (Volume_For_Track (Track)));

   -------------------------
   -- Process_MIDI_Events --
   -------------------------

   procedure Process_MIDI_Events is
      procedure Pop is new WNM.MIDI.Queues.Synth_Pop (Event);
   begin
      Pop;
   end Process_MIDI_Events;

   ------------
   -- Update --
   ------------

   function Update return WNM.Time.Time_Microseconds is
      Now : constant WNM.Time.Time_Microseconds := WNM.Time.Clock;
   begin
      if Now >= Next_Start then
         Next_Start := Next_Start + 0;

         Process_MIDI_Events;

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
      --  Len : File_System.File_Signed_Size;

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
            elsif Val < Integer_32 (Integer_16'First) then
               Output (Index).L := Mono_Point'First;
            else
               Output (Index).L := Mono_Point (Val);
            end if;

            Val := Integer_32 (Output (Index).R) + Integer_32 (Right);
            if Val > Integer_32 (Mono_Point'Last) then
               Output (Index).R := Mono_Point'Last;
            elsif Val < Integer_32 (Integer_16'First) then
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
      --        Len := Write (Recording_File, Sample_Buf'Address, Sample_Buf'Length * 2);
      --        Recording_Size := Recording_Size + Len;
      --     end;
      --  end if;

      Glob_Sample_Clock := Glob_Sample_Clock + Samples_Per_Buffer;
   end Next_Points;

   ---------------------
   -- Set_Passthrough --
   ---------------------

   procedure Set_Passthrough (Kind : Input_Kind) is
   begin
      WNM.Audio.Select_Input (Kind);
      Passthrough := Kind;
   end Set_Passthrough;

   ---------------------
   -- Get_Passthrough --
   ---------------------

   function Get_Passthrough return Input_Kind is
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
      pragma Unreferenced (Max_Size);
   begin
      if Recording_Source /= None then
         return;
      end if;

      Recording_Source := Source;
      Create_File (Recording_File, Filename);
      Recording_Size := 0;
   end Start_Recording;

   --------------------
   -- Stop_Recording --
   --------------------

   procedure Stop_Recording is
   begin
      Close (Recording_File);
      Recording_Source := None;
   end Stop_Recording;

   -----------------
   -- Record_Size --
   -----------------

   function Record_Size return Natural
   is (Natural (Recording_Size));

end WNM.Synth;
