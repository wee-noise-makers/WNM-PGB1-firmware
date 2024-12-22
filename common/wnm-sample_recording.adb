-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2024 Fabien Chouteau                    --
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

with Interfaces;
with System.Storage_Elements;
with WNM.Utils;

package body WNM.Sample_Recording is

   type Recording_State is record
      QOA_State   : QOA.Encoder_State;
      Frame_Audio : QOA.Frame_Audio_Data;
      Next_Point_In : QOA.QOA_Frame_Point_Index :=
        QOA.QOA_Frame_Point_Index'First;

      Rec_Frame_Count : Natural := 0;
   end record;

   Sample_Sector_Buffer  : WNM_HAL.Storage_Sector_Data
     with Alignment => 2;

   State : Recording_State;
   Frame_Head, Frame_Tail : QOA.QOA_Frame_Index := QOA.QOA_Frame_Index'First;

   ----------------
   -- Push_Point --
   ----------------

   procedure Push_Point (State : in out Recording_State; Point : Tresses.S16)
   is
      use WNM.QOA;
   begin
      State.Frame_Audio (State.Next_Point_In) := Point;

      if State.Next_Point_In = QOA.QOA_Frame_Point_Index'Last then
         State.Next_Point_In := QOA.QOA_Frame_Point_Index'First;

         QOA.Encode_Frame (State.QOA_State,
                           State.Frame_Audio,
                           Sample_Rec_QOA_Buffer (Frame_Tail));

         Frame_Tail := @ + 1;

         --  We reached back to the head which means the buffer is full and we
         --  will now erase the oldest frames with new ones.
         if Frame_Tail = Frame_Head then
            Frame_Head := @ + 1;
         else
            State.Rec_Frame_Count := @ + 1;
         end if;

      else
         State.Next_Point_In := @ + 1;
      end if;
   end Push_Point;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      QOA.Reset (State.QOA_State);
      State.Next_Point_In := QOA.QOA_Frame_Point_Index'First;
      State.Rec_Frame_Count := 0;

      Frame_Head := QOA.QOA_Frame_Index'First;
      Frame_Tail := QOA.QOA_Frame_Index'First;

      Play_Start_Offset := 0;
      Play_End_Offset := 0;

      Wave_Data := (others => 0);
      Next_Wave := Waveform_Index'First;
      Next_Wave_Cnt := 0;
      Wave_Acc := 0;

      --  Clear the shared buffer
      WNM.Shared_Buffers.Shared_Buffer := (others => 0);
   end Reset;

   ----------------------
   -- Init_From_Sample --
   ----------------------

   procedure Init_From_Sample (Id : Sample_Index) is
      Buffer : WNM_HAL.Mono_Buffer;

      First : constant Sample_Point_Index := Sample_Point_Index'First;
      Last  : constant Sample_Point_Index := First + Entry_Len (Id);
      --  wnm-project-step_sequencer.adb:724:60 range check failed
      --  value 0 not in 1..16
      --  EXCEPTION INFORMATION
      --  raised CONSTRAINT_ERROR : wnm-project-step_sequencer.adb:724:60 range check failed
      --  value 0 not in 1..16
      --  [/home/chouteau/src/github/WNM-PS1-firmware/simulator/bin/wnm-pgb1-simulator]
      --  0x48c250 Wnm.Project.Step_Sequencer.Execute_Step at wnm-project-step_sequencer.adb:724
      --  0x48c309 Wnm.Project.Step_Sequencer.Midi_Clock_Tick at wnm-project-step_sequencer.adb:768
      --  0x489585 Wnm.Midi_Clock.Update at midi-time-generic_clock.adb:61
      --  0x4c88c2 Wnm.Tasks.Sequencer_1khz_Tick at wnm-tasks.adb:103
      --  0x4d6d62 Asfml_Sim.Seq_1khz_Task at asfml_sim.adb:156
      --  0x59cc25 System.Tasking.Stages.Task_Wrapper at s-tassta.adb:1211

      Buff_Idx : Natural;
   begin
      Reset;

      Buff_Idx := Buffer'First;
      for Idx in First .. Last loop
         Buffer (Buff_Idx) := Sample_Library.Sample_Data (Id).Audio (Idx);

         if Buff_Idx = Buffer'Last then
            Buff_Idx := Buffer'First;
            Record_Buffer (Buffer);
         else
            Buff_Idx := @ + 1;
         end if;
      end loop;

      if Buff_Idx /= Buffer'First then
         Buffer (Buff_Idx .. Buffer'Last) := (others => 0);
         Record_Buffer (Buffer);
      end if;
   end Init_From_Sample;

   -------------------
   -- Record_Buffer --
   -------------------

   procedure Record_Buffer (Input : WNM_HAL.Mono_Buffer) is
      use Interfaces;
      use Tresses;
   begin
      for Elt of Input loop
         Wave_Acc := @ + (abs S32 (Elt));
         Push_Point (State, Elt);
      end loop;

      Play_End_Offset :=
        Sample_Point_Count (State.Rec_Frame_Count * QOA.Points_Per_Frame);

      Next_Wave_Cnt := @ + Input'Length;
      if Next_Wave_Cnt > Wave_Segment_Count then
         declare
            Mean : constant S32 := abs (Wave_Acc / S32 (Wave_Segment_Count));
            Ratio : constant Float := Float (Mean) / Float (S16'Last / 2);

            Max   : constant Float := Float (Waveform_Point'Last);
            Value : constant Waveform_Point :=
              Waveform_Point
                (Float'Min (Float (Waveform_Point'Last) * Ratio, Max));
         begin
            Next_Wave_Cnt := 0;
            Wave_Acc := 0;

            if Next_Wave = Waveform_Index'Last then
               Wave_Data (Wave_Data'First .. Wave_Data'Last - 1) :=
                 Wave_Data (Wave_Data'First + 1 .. Wave_Data'Last);
               Wave_Data (Next_Wave) := Value;
            else
               Wave_Data (Next_Wave) := Value;
               Next_Wave := Next_Wave + 1;
            end if;
         end;
      end if;
   end Record_Buffer;

   ---------------------
   -- Recorded_Length --
   ---------------------

   function Recorded_Length return Sample_Point_Count is
   begin
      return Sample_Point_Count (State.Rec_Frame_Count * QOA.Points_Per_Frame);
   end Recorded_Length;

   -----------------
   -- Save_Sample --
   -----------------

   procedure Save_Sample (Id : Sample_Index; Name : String)
   is
      use WNM.QOA;
      use WNM_Configuration.Storage;

      subtype Sector_Count
      is WNM_HAL.Sample_Sector_Id range 0 .. Sectors_Per_Sample - 1;

      Sample_Point_Per_Sector : constant := Sector_Byte_Size / 2;
      type Sector_Point_Index is range 1 .. Sample_Point_Per_Sector;
      type Sector_Audio_Data is array (Sector_Point_Index) of Mono_Point;

      Sector : Sector_Count := Sector_Count'First;

      Base_Sector : constant WNM_HAL.Sample_Sector_Id :=
        ((WNM_HAL.Sample_Sector_Id (Id) - 1) * Sectors_Per_Sample);

      Length : constant Sample_Point_Count :=
        Play_End_Offset - Play_Start_Offset;

      Frame_Id : QOA.QOA_Frame_Index;

      Sector_Audio : Sector_Audio_Data
        with Address => Sample_Sector_Buffer'Address;

      Dst_Index : Sector_Point_Index := Sector_Audio_Data'First;
   begin

      Sample_Library.Sample_Data (Id).Len := Sample_Storage_Len (Length);

      Utils.Copy_Str (Name, Sample_Library.Sample_Data (Id).Name);

      if State.Rec_Frame_Count > 0 then

         --  What follows is way too complicated to my taste. That's the price
         --  to pay for a ring buffer of QOA frames, but I think there a way
         --  to make this code a lot clearer.

         declare
            --  Find which frames holds the first sample point

            First_Frame_Offset : constant QOA_Frame_Index :=
              QOA_Frame_Index (Play_Start_Offset / Points_Per_Frame);

            First_Frame : constant QOA_Frame_Index :=
              Frame_Head + First_Frame_Offset;

            --  Find the index of the first sample in this frame

            First_Point_Offset : constant QOA_Frame_Point_Index :=
              QOA_Frame_Point_Index (Play_Start_Offset mod Points_Per_Frame);

            Point_In_Frame : QOA_Frame_Point_Index := First_Point_Offset;

            Remaining : Sample_Point_Count := Recorded_Length;
         begin

            --  We take recorded frames
            Frame_Id := First_Frame;
            loop
               exit when Remaining = 0;

               --  Decode it
               Decode_Frame (Sample_Rec_QOA_Buffer (Frame_Id),
                             State.Frame_Audio);

               --  Transfer audio points to the sector buffer
               loop
                  Sector_Audio (Dst_Index) :=
                    State.Frame_Audio (Point_In_Frame);

                  Remaining := @ - 1;

                  if Dst_Index = Sector_Point_Index'Last then
                     --  We have one complete sector, write it to storage
                     Write_To_Storage (Base_Sector + Sector,
                                       Sample_Sector_Buffer);

                     --  Now to the next sector
                     Sector := @ + 1;
                     Dst_Index := Sector_Point_Index'First;
                  else
                     Dst_Index := @ + 1;
                  end if;

                  exit when Remaining = 0 or else
                    Point_In_Frame = QOA_Frame_Point_Index'Last;

                  Point_In_Frame := Point_In_Frame + 1;
               end loop;

               Frame_Id := @ + 1;
               Point_In_Frame := QOA_Frame_Point_Index'First;
            end loop;
         end;
      end if;

      --  The current sector potentially contains some audio data. If it's
      --  not the last sector, we can just write it as is.
      if Sector /= Sector_Count'Last then
         Write_To_Storage (Base_Sector + Sector, Sample_Sector_Buffer);
      end if;

      --  Now we go to the last sector (potentially with some remaining audio
      --  data).
      Sector := Sector_Count'Last;

      --  Write the sample meta data
      declare
         use System.Storage_Elements;

         Sector_End_Addr : constant Integer_Address :=
           To_Integer (Sample_Sector_Buffer'Address) + Sector_Byte_Size;

         Dst_Len : Sample_Storage_Len
           with Address =>
             To_Address (Sector_End_Addr - 4);
         Dst_Name : Sample_Entry_Name
           with Address =>
             To_Address (Sector_End_Addr - 4 - Sample_Name_Length);
      begin
         Dst_Len := Sample_Storage_Len (Length);
         Utils.Copy_Str (Name, Dst_Name);
      end;

      --  Write the last sector with metadata
      Write_To_Storage (Base_Sector + Sector,
                        Sample_Sector_Buffer);

      --  Reload sample info from storage
      Sample_Library.Load (Id);
   end Save_Sample;

   -------------------
   -- Waveform_Data --
   -------------------

   function Waveform_Data return Waveform is
   begin
      return Wave_Data;
   end Waveform_Data;

   -----------------
   -- Start_Point --
   -----------------

   function Start_Point return Sample_Point_Count
   is (Play_Start_Offset);

   ---------------
   -- End_Point --
   ---------------

   function End_Point return Sample_Point_Count
   is (Play_End_Offset);

   --------------------
   -- Count_To_Index --
   --------------------

   function Count_To_Index (C : Sample_Point_Count) return Waveform_Index is
      Ratio : constant Float := Float (C) / Float (Sample_Point_Count'Last);
      Value : constant Integer :=
        Integer (Ratio * Float (Waveform_Index'Last));
   begin
      if Value < Integer (Waveform_Index'First) then
         return Waveform_Index'First;
      elsif Value > Integer (Waveform_Index'Last) then
         return Waveform_Index'Last;
      else
         return Waveform_Index (Value);
      end if;
   end Count_To_Index;

   -----------------------
   -- Start_Point_Index --
   -----------------------

   function Start_Point_Index return Waveform_Index
   is (Count_To_Index (Start_Point));

   ---------------------
   -- End_Point_Index --
   ---------------------

   function End_Point_Index return Waveform_Index
   is (Count_To_Index (End_Point));

   -----------------------------
   -- Last_Played_Point_Index --
   -----------------------------

   function Last_Played_Point_Index return Waveform_Index is
      Ret : constant Waveform_Index := Count_To_Index (Last_Played_Offset);
   begin
      Last_Played_Offset := 0;
      return Ret;
   end Last_Played_Point_Index;

   ----------------------
   -- Move_Start_Point --
   ----------------------

   procedure Move_Start_Point (Count : Integer) is
      New_Value : constant Integer := Integer (Play_Start_Offset) + Count;
   begin
      if New_Value in 0 .. Integer (Play_End_Offset) then
         Play_Start_Offset := Sample_Point_Count (New_Value);
      end if;
   end Move_Start_Point;

   --------------------
   -- Move_End_Point --
   --------------------

   procedure Move_End_Point (Count : Integer) is
      New_Value : constant Integer := Integer (Play_End_Offset) + Count;
   begin
      if New_Value in Integer (Play_Start_Offset) .. Integer (Recorded_Length)
      then
         Play_End_Offset := Sample_Point_Count (New_Value);
      end if;
   end Move_End_Point;

   ---------------
   -- Get_Point --
   ---------------

   function Get_Point (Cache : in out QOA.Decoder_Cache;
                       Index :        Sample_Point_Index)
                       return Tresses.S16
   is
      Length : constant Sample_Point_Count :=
        Play_End_Offset - Play_Start_Offset;
   begin
      if Index > Length then
         return 0;
      else
         declare
            Rec_Start_Point : constant Sample_Point_Index :=
              Sample_Point_Index (Frame_Head) * QOA.Points_Per_Frame;

            Play_Offset : constant Sample_Point_Count :=
              Play_Start_Offset + Index;

            Real_Index : constant Sample_Point_Index :=
              Sample_Point_Index
                ((Natural (Rec_Start_Point) + Natural (Play_Offset)) mod
                   Natural (Sample_Point_Count'Last));
         begin
            Last_Played_Offset := Play_Offset;

            return QOA.Decode_Single_Sample
              (Cache,
               Sample_Rec_QOA_Buffer,
               QOA.QOA_Sample_Point_Index (Real_Index));
         end;
      end if;
   end Get_Point;

end WNM.Sample_Recording;
