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

with Ada.Text_IO;

with WNM.File_System;
with System.Address_To_Access_Conversions;

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
         return "--  Invalid  --";
      elsif Entries (Index).Used then
         return Entries (Index).Name;
      else
         return "-- No Sample --";
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

   ---------------------
   -- Parse_Info_Line --
   ---------------------

   procedure Parse_Info_Line (Line : String) is
      Delim_Char : constant Character := ':';

      Delim1, Delim2 : Natural := Line'Last;

      Sample_Id : Valid_Sample_Index;
   begin
      for Index in Line'First .. Line'Last loop
         if Line (Index) = Delim_Char then
            Delim1 := Index;
            exit;
         end if;
      end loop;

      for Index in Delim1 + 1 .. Line'Last loop
         if Line (Index) = Delim_Char then
            Delim2 := Index;
            exit;
         end if;
      end loop;

      declare
         ID : constant Natural :=
           Natural'Value (Line (Line'First .. Delim1 - 1));
      begin
         if ID in
           Natural (Valid_Sample_Index'First) ..
           Natural (Valid_Sample_Index'Last)
         then
            Sample_Id := Valid_Sample_Index (ID);
         else
            raise Program_Error;
         end if;
      end;

      declare
         Name : constant String := Line (Delim1 + 1 .. Delim2 - 1);
      begin
         if Name'Length = Sample_Entry_Name'Length then
            Entries (Sample_Id).Name := Name;
         end if;
      end;

      declare
         Len : constant Natural :=
           Natural'Value (Line (Delim2 + 1 .. Line'Last));
      begin
         if Len <= Natural (Sample_Point_Count'Last) then
            Entries (Sample_Id).Length := Sample_Point_Count (Len);
            Entries (Sample_Id).Used := Len > 0;
         else
            raise Program_Error;
         end if;
      end;

   end Parse_Info_Line;

   ----------
   -- Load --
   ----------

   procedure Load is
      use WNM.File_System;

      Line : String (1 .. 64);
      Last : Natural;
   begin
      if File_System.Open_Read (Sample_Entries_Filename) /= File_System.Ok
      then
         Ada.Text_IO.Put_Line ("Cannot open Sample Entries file...");
         return;
      end if;

      loop
         Read_Line (Line, Last);
         exit when Last < Line'First;

         if Line (Last) = ASCII.LF then
            Last := Last - 1;
         end if;

         if Last >= Line'First then
            Parse_Info_Line (Line (1 .. Last));
         end if;
      end loop;

      File_System.Close;
   end Load;

   ----------------------------
   -- Point_Index_To_Seconds --
   ----------------------------

   function Point_Index_To_Seconds (Index : Sample_Point_Index)
                                    return Sample_Time
   is
   begin
      return Sample_Time (Float (Index) /
                            Float (WNM_Configuration.Audio.Sample_Frequency));
   end Point_Index_To_Seconds;

end WNM.Sample_Library;
