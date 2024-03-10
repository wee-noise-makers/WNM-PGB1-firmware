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

package body WNM.Shared_Buffers is

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Length := 0;

      Rec_Start_Point := Sample_Point_Index'First;
      Rec_End_Point := Sample_Point_Index'First;

      Play_Start_Offset := 0;
      Play_End_Offset := 0;

      Wave_Data := (others => 0);
      Next_Wave := Waveform_Index'First;
      Next_Wave_Cnt := 0;
      Wave_Acc := 0;
   end Init;

   ----------------------
   -- Init_From_Sample --
   ----------------------

   procedure Init_From_Sample (Id : Valid_Sample_Index) is
      Buffer : WNM_HAL.Mono_Buffer;

      First : constant Sample_Point_Index := Sample_Point_Index'First;
      Last  : constant Sample_Point_Index := First + Entry_Len (Id);

      Buff_Idx : Natural;
   begin
      Init;

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
         Sample_Rec_Buffer (Rec_End_Point) := Elt;

         if Rec_End_Point = Sample_Point_Index'Last then
            Rec_End_Point := Sample_Point_Index'First;
         else
            Rec_End_Point := @ + 1;
         end if;

         if Rec_Start_Point = End_Point then
            if Rec_Start_Point = Sample_Point_Index'Last then
               Rec_Start_Point := Sample_Point_Index'First;
            else
               Rec_Start_Point := @ + 1;
            end if;
         end if;

         if Length < Sample_Point_Index'Last then
            Length := @ + 1;
         end if;

      end loop;

      Play_End_Offset := Length;

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
      return Length;
   end Recorded_Length;

   -----------------
   -- Save_Sample --
   -----------------

   procedure Save_Sample (Id   : Valid_Sample_Index;
                          Name : String)
   is
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

      Src_Offset : Sample_Point_Index := Sample_Point_Index'First;
   begin

      Sample_Library.Sample_Data (Id).Len := Sample_Storage_Len (Length);

      Utils.Copy_Str (Name, Sample_Library.Sample_Data (Id).Name);

      --  We fill and write sectors to the flash one by one
      if Length > 0 then
         declare
            Data : Sector_Audio_Data
              with Address => Sample_Sector_Buffer'Address;
            Dst_Index : Sector_Point_Index := Sector_Audio_Data'First;

         begin
            loop
               declare
                  Src_Index : constant Sample_Point_Index :=
                    Sample_Point_Index
                      ((Natural (Rec_Start_Point) +
                         Natural (Play_Start_Offset) +
                         Natural (Src_Offset)) mod
                         Natural (Sample_Point_Count'Last));
               begin
                  Data (Dst_Index) := Sample_Rec_Buffer (Src_Index);
                  Dst_Index := @ + 1;
                  Src_Offset := @ + 1;

                  if Dst_Index = Sector_Point_Index'Last then
                     --  We have one complete sector
                     Dst_Index := Sector_Point_Index'First;
                     Write_To_Storage (Base_Sector + Sector,
                                       Sample_Sector_Buffer);

                     --  Now to the next sector
                     Sector := @ + 1;
                  end if;
               end;

               exit when Src_Offset >= Length - 1;
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

      --  if Length > 0 then
      --     for Offset in 0 .. Length - 1 loop
      --
      --
      --
      --        Write_To_Storage (Base_Sector + Sector, Sample_Sector_Buffer);
      --
      --        declare
      --           In_Index : constant Sample_Point_Index :=
      --             Sample_Point_Index
      --               ((Natural (Rec_Start_Point) +
      --                  Natural (Play_Start_Offset) +
      --                  Natural (Offset)) mod
      --                  Natural (Sample_Point_Count'Last));
      --
      --           Out_Index : constant Sample_Point_Index :=
      --             Sample_Point_Index'First + Sample_Point_Count (Offset);
      --        begin
      --           Sample_Library.Sample_Data (Id).Audio (Out_Index)
      --             := Sample_Rec_Buffer (In_Index);
      --        end;
      --     end loop;
      --  end if;

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
      if New_Value in Integer (Play_Start_Offset) .. Integer (Length) then
         Play_End_Offset := Sample_Point_Count (New_Value);
      end if;
   end Move_End_Point;

   ---------------
   -- Get_Point --
   ---------------

   function Get_Point (Index : Sample_Point_Index) return Tresses.S16 is
      Length : constant Sample_Point_Count :=
        Play_End_Offset - Play_Start_Offset;
   begin
      if Index > Length then
         return 0;
      else
         declare
            Real_Index : constant Sample_Point_Index :=
              Sample_Point_Index
                ((Natural (Rec_Start_Point) +
                   Natural (Play_Start_Offset) +
                   Natural (Index)) mod
                   Natural (Sample_Point_Count'Last));
         begin
            Last_Played_Offset := Real_Index;
            return Sample_Rec_Buffer (Real_Index);
         end;
      end if;
   end Get_Point;

end WNM.Shared_Buffers;
