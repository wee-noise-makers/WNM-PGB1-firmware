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

   function Entry_Len (Index : Sample_Index) return Sample_Point_Count is
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

   procedure Load (Id : Valid_Sample_Index) is
      use HAL;
      Len : UInt32;
   begin
      Len := Sample_Data.all (Id).Len;

      if Len = 0 then
         Entries (Id) :=
           (Used => False,
            Name => (others => ' '),
            Length => 0);
      else
         Entries (Id).Name := Sample_Data.all (Id).Name;

         Len := UInt32'Min (Len, UInt32 (QOA.Sample_Point_Count'Last));

         Entries (Id).Length := Sample_Point_Count (Len);
         Entries (Id).Used := True;
      end if;
   end Load;

   ----------
   -- Load --
   ----------

   procedure Load is
   begin
      for Sample_Id in Valid_Sample_Index loop
         Load (Sample_Id);
      end loop;
   end Load;

   ----------------
   -- To_Seconds --
   ----------------

   function To_Seconds (Index : Sample_Point_Count) return Sample_Time is
   begin
      return Sample_Time (Float (Index) /
                            Float (WNM_Configuration.Audio.Sample_Frequency));
   end To_Seconds;

end WNM.Sample_Library;
