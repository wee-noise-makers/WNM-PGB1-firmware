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

with System.Address_To_Access_Conversions;
with WNM.QOA; use WNM.QOA;

package body WNM.Sample_Library is

   package Global_Address_To_Access
   is new System.Address_To_Access_Conversions (Global_Sample_Array);

   -----------------
   -- Sample_Data --
   -----------------

   function Sample_Data return not null Global_Sample_Array_Access is
   begin
      return Global_Sample_Array_Access
        (Global_Address_To_Access.To_Pointer (Sample_Data_Base));
   end Sample_Data;

   -----------------
   -- Load_Points --
   -----------------

   type Cache_Frame_Id is record
      Sample_Id : Sample_Index := Invalid_Sample_Entry;
      Frame_Id  : Integer      := -1;
   end record;

   type Frame_Cache_Entry is record
      Id    : Cache_Frame_Id;
      Audio : QOA.Frame_Audio_Data;
   end record;

   type Cache_Entry_Index is mod 2;
   Frame_Cache : array (Cache_Entry_Index) of Frame_Cache_Entry;
   Last_In : Cache_Entry_Index := Cache_Entry_Index'Last;

   procedure Load_Points (Sample_Id   : Valid_Sample_Index;
                          Point_Index : QOA.Sample_Point_Index;
                          A, B        : out Mono_Point)
   is
      use HAL;

      ------------------
      -- Decode_Point --
      ------------------

      function Decode_Point (Idx      : QOA.Sample_Point_Index)
                             return Mono_Point
      is
         Frame_Id : constant Integer :=
           Integer (Idx) / QOA.Points_Per_Frame;
         Point_Index : constant QOA.Sample_Point_Index :=
           Idx mod QOA.Points_Per_Frame;
         Cache_Id : constant Cache_Frame_Id := (Sample_Id, Frame_Id);
      begin

         if Sample_Storage_Len (Idx) >= Sample_Data.all (Sample_Id).Len - 1
         then
            return 0;
         end if;

         --  First search for the frame in cache
         for X in Frame_Cache'Range loop
            if Frame_Cache (X).Id = Cache_Id then
               return Frame_Cache (X).Audio (Point_Index);
            end if;
         end loop;

         --  Frame not found, we have to decode it
         Last_In := Last_In + 1;
         Frame_Cache (Last_In).Id := Cache_Id;
         QOA.Decode_Frame
           (Sample_Data.all (Sample_Id).Audio (Cache_Id.Frame_Id),
            Frame_Cache (Last_In).Audio);

         return Frame_Cache (Last_In).Audio (Point_Index);
      end Decode_Point;

   begin
      A := Decode_Point (Point_Index);
      B := Decode_Point (Point_Index + 1);
   end Load_Points;

   ----------------
   -- Entry_Name --
   ----------------

   function Entry_Name (Index : Sample_Index) return Sample_Entry_Name is
   begin
      if Index = Invalid_Sample_Entry then
         return "-- Invalid -- ";
      elsif Entries (Index).Used then
         return Entries (Index).Name;
      else
         return "- No Sample - ";
      end if;
   end Entry_Name;

   ---------------
   -- Entry_Len --
   ---------------

   function Entry_Len (Index : Sample_Index) return QOA.Sample_Point_Count is
   begin
      if Index /= Invalid_Sample_Entry and then Entries (Index).Used then
         return Entries (Index).Length;
      else
         return 0;
      end if;
   end Entry_Len;

   --------------------------
   -- Entry_Device_Address --
   --------------------------

   function Entry_Device_Address (Index : Valid_Sample_Index)
                                  return HAL.UInt32
   is
      use WNM_Configuration.Storage;
      use HAL;
   begin
      return Sample_Library_Base_Addr +
        (UInt32 (Index) - 1) * Sectors_Per_Sample * Sector_Byte_Size;
   end Entry_Device_Address;

   ----------
   -- Load --
   ----------

   procedure Load is
      use HAL;
      Len : UInt32;
   begin

      for Sample_Id in Valid_Sample_Index loop

         Len := Sample_Data.all (Sample_Id).Len;

         if Len = 0 then
            Entries (Sample_Id) :=
              (Used => False,
               Name => (others => ' '),
               Length => 0);
         else
            Entries (Sample_Id).Name := Sample_Data.all (Sample_Id).Name;

            Len := UInt32'Min (Len, UInt32 (QOA.Sample_Point_Count'Last));

            Entries (Sample_Id).Length := QOA.Sample_Point_Count (Len);
            Entries (Sample_Id).Used := True;
         end if;
      end loop;
   end Load;

   ----------------------------
   -- Point_Index_To_Seconds --
   ----------------------------

   function Point_Index_To_Seconds
     (Index : QOA.Sample_Point_Index)
      return Sample_Time
   is
   begin
      return Sample_Time (Float (Index) /
                            Float (WNM_Configuration.Audio.Sample_Frequency));
   end Point_Index_To_Seconds;

end WNM.Sample_Library;
