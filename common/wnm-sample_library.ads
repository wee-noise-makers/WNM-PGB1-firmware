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

with Ada.Unchecked_Conversion;

with WNM_Configuration;

with HAL;

package WNM.Sample_Library
with Elaborate_Body
is

   --  Audio samples are located in a dedicated part of the Flash. All the
   --  sample have the same amout of memory available which means that getting
   --  the address of sample data is just an access to an array based on the
   --  sample index.
   --
   --  The meta-data associated with each sample (name and length) is located
   --  at the end of the sample memory.
   --
   --  Sample Edit Mode --
   --
   --  The edition of sample is done exclusively in RAM using a special mode.
   --  In sample edit mode, the sequencer/playback is stopped to provide the
   --  maximum CPU time and allocate RAM memory for editing.
   --
   --  The sample under edit is first loaded in RAM. User can then apply a
   --  number of actions/settins/effects to the sample:
   --    - Preview sample
   --    - Record from audio in
   --    - Import/Export from/to USB (via a website?)
   --    - Set sample lenght
   --    - Set start point, end point, repeat point, etc.
   --    - Apply effects: bitcrush, drive, echo, reverse, pitch, sample rate,
   --      etc.
   --
   --  The sample is then written back into flash.

   subtype Sample_Storage_Len is HAL.UInt32;

   Samples                 : constant :=
     WNM_Configuration.Storage.Nbr_Samples;

   Single_Sample_Data_Byte_Size : constant :=
     WNM_Configuration.Storage.Sample_Library_Byte_Size / Samples;

   Sample_Metadata_Byte_Size : constant :=
     WNM_Configuration.Storage.Sample_Name_Length
       + (Sample_Storage_Len'Size / 8) * 9;

   Sample_Audio_Byte_Size : constant :=
     Single_Sample_Data_Byte_Size - Sample_Metadata_Byte_Size;

   Points_Per_Sample : constant :=
     Sample_Audio_Byte_Size / 2;

   type Sample_Point_Count is range 0 .. Points_Per_Sample;
   subtype Sample_Point_Index
     is Sample_Point_Count range 0 .. Sample_Point_Count'Last - 1;

   type Sample_Audio_Data is array (Sample_Point_Index) of Mono_Point
     with Size => Points_Per_Sample * 16;

   Sample_Id_Last : constant := Samples - 1;
   type Sample_Index is new HAL.UInt5 range 0 .. Sample_Id_Last;

   subtype Sample_Entry_Name
     is String (1 .. WNM_Configuration.Storage.Sample_Name_Length);

   type Single_Sample_Data is record
      Audio    : Sample_Audio_Data;
      S1, E1, S2, E2, S3, E3, S4, E4 : Sample_Storage_Len := 0;
      Name     : Sample_Entry_Name;
      Len      : Sample_Storage_Len;
   end record
     with Pack, Size => Single_Sample_Data_Byte_Size * 8;

   type Single_Sample_Data_Access is access all Single_Sample_Data;

   type Global_Sample_Array
   is array (Sample_Index) of aliased Single_Sample_Data
     with Size => Storage.Sample_Library_Byte_Size * 8;

   type Global_Sample_Array_Access is access all Global_Sample_Array;

   function Sample_Data return not null Global_Sample_Array_Access;

   function Entry_Name (Index : Sample_Index) return Sample_Entry_Name;

   function Entry_Len (Index : Sample_Index) return Sample_Point_Count;

   function Entry_Device_Address (Index : Sample_Index)
                                  return HAL.UInt32;
   --  Return the base address of an sample entry in the device memory space

   procedure Load (Id : Sample_Index);
   procedure Load;

   type Slice_Index is new HAL.UInt2 range 0 .. 3;

   function Has_Slice (Id : Sample_Index; Slice : Slice_Index)
                       return Boolean
   is (Slice = Slice_Index'First);

   type Slice_Id is record
      Slice : Slice_Index;
      Sample : Sample_Index;
   end record
     with Pack, Size => 7;

   function To_CC
   is new Ada.Unchecked_Conversion (Slice_Id, MIDI.MIDI_Data);
   function From_CC
   is new Ada.Unchecked_Conversion (MIDI.MIDI_Data, Slice_Id);

   type Sample_Time is delta 0.01 range 0.0 .. 3.0;

   function To_Seconds (Index : Sample_Point_Count) return Sample_Time;

private

   type Sample_Entry is record
      Used   : Boolean := False;
      Name   : Sample_Entry_Name := (others => ASCII.NUL);
      Length : Sample_Point_Count := 0;
   end record with Pack;

   Entries : array (Sample_Index) of Sample_Entry;

end WNM.Sample_Library;
