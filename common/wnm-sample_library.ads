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

with WNM_HAL;

with WNM_Configuration;

with HAL;

package WNM.Sample_Library
with Elaborate_Body
is

   --  Audio samples are located in a dedicated part of the Flash. All the
   --  sample have the same amout of memory available which means that getting
   --  sample data is just an access to an array based on the sample index.
   --
   --  The minimum erasable flash size a 4096 byte sector, so this is the base
   --  unit.
   --
   --  We allocate 5632 sectors (22.0 MB) for all the sample data. At 44100Hz
   --  that's (5632 * 4096) / (44100 * 2) = 261.5, so a little over 261
   --  seconds of audio data. That means 128 samples of ~2 seconds.
   --
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

   Single_Sample_Byte_Area : constant :=
     WNM_Configuration.Storage.Sample_Library_Byte_Size / Samples;

   Sample_Metadata_Byte_Size : constant :=
     WNM_Configuration.Storage.Sample_Name_Lenght
       + Sample_Storage_Len'Size / 8;

   Single_Sample_Audio_Byte_Size : constant :=
     Single_Sample_Byte_Area - Sample_Metadata_Byte_Size;

   Single_Sample_Point_Cnt : constant := Single_Sample_Audio_Byte_Size / 2;

   subtype Sample_Index is Natural range 0 .. Samples;
   subtype Valid_Sample_Index is Sample_Index range 1 .. Sample_Index'Last;

   Invalid_Sample_Entry : constant Sample_Index := Sample_Index'First;

   type Sample_Point_Count is range 0 .. Single_Sample_Point_Cnt;
   subtype Sample_Point_Index
     is Sample_Point_Count range 0 .. Sample_Point_Count'Last - 1;

   type Single_Sample_Audio_Data
   is array (Sample_Point_Index) of WNM_HAL.Mono_Point
     with Size => Single_Sample_Audio_Byte_Size * 8;

   subtype Sample_Entry_Name
     is String (1 .. WNM_Configuration.Storage.Sample_Name_Lenght);

   type Single_Sample_Data is record
      Audio : Single_Sample_Audio_Data;
      Name  : Sample_Entry_Name;
      Len   : HAL.UInt32;
   end record
     with Pack, Size => Single_Sample_Byte_Area * 8;

   type Single_Sample_Data_Access is access all Single_Sample_Data;

   type Global_Sample_Array
   is array (Valid_Sample_Index) of aliased Single_Sample_Data
     with Size => Storage.Sample_Library_Byte_Size * 8;

   type Global_Sample_Array_Access is access all Global_Sample_Array;

   --  function Sample_Data return not null Global_Sample_Array_Access;

   procedure Load_Points (Sample_Id   : Valid_Sample_Index;
                          Point_Index : Sample_Point_Index;
                          A, B        : out Mono_Point);
   --  Load two consecutive sample points from the given sample

   function Entry_Name (Index : Sample_Index) return Sample_Entry_Name;

   function Entry_Len (Index : Sample_Index) return Sample_Point_Count;

   procedure Load;

   type Sample_Time is delta 0.001 range 0.0 .. 3.0;

   function Point_Index_To_Seconds (Index : Sample_Point_Index)
                                    return Sample_Time;

private

   type Sample_Entry is record
      Used   : Boolean := False;
      Name   : Sample_Entry_Name := (others => ASCII.NUL);
      Length : Sample_Point_Count := 0;
   end record with Pack;

   Entries : array (Valid_Sample_Index) of Sample_Entry;

end WNM.Sample_Library;
