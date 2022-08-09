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

with WNM.Audio;
with WNM.Storage;

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
   --  We allocate 2200 sectors (~8.6 MB) for all the sample data. At 44100Hz
   --  that's (2220 * 4096) / (44100 * 2) = 102.17, so a little over 102
   --  seconds of audio data. We can either use this space for 100 samples of
   --  ~1 seconds or 50 samples of ~2 seconds.
   --
   --  We decide to go for 50 samples of ~2 seconds.
   --
   --  The meta-data associated with each sample (length, name, entry point,
   --  exit point, repeat point, etc.) will be located in a file on the
   --  filesystem because it is edited more frequenly by users and having it
   --  in fixed flash sector(s) would result in bad wear level of the flash.
   --
   --  Sample Edit Mode --
   --
   --  The edition of sample is done exclusively in RAM using a special mode.
   --  In sample edit mode, the sequencer/playback is stopped to provide the
   --  maximum CPU time and allocate RAM memory for editing.
   --
   --  The sample uder edit is first loaded in RAM. User can then apply a number
   --  of actions/settins/effects to the sample:
   --    - Preview sample
   --    - Record from audio in
   --    - Import/Export from/to USB (via a website?)
   --    - Set sample lenght
   --    - Set start point, end point, repeat point, etc.
   --    - Apply effects: bitcrush, drive, echo, reverse, pitch, sample rate, etc.
   --
   --  The sample is then written back into flash.


   Samples             : constant := 50;

   Single_Sample_Byte_Size : constant := Storage.Sample_Library_Size / Samples;
   Single_Sample_Point_Cnt : constant := Single_Sample_Byte_Size / 2;

   subtype Sample_Index is Natural range 0 .. Samples;
   subtype Valid_Sample_Index is Sample_Index range 1 .. Sample_Index'Last;

   Invalid_Sample_Entry : constant Sample_Index := Sample_Index'First;

   type Sample_Point_Count is range 0 .. Single_Sample_Point_Cnt;
   subtype Sample_Point_Index is Sample_Point_Count range 1 .. Sample_Point_Count'Last;

   type Single_Sample_Data is array (Sample_Point_Index) of Audio.Mono_Point
     with Size => Single_Sample_Byte_Size * 8;

   type Single_Sample_Data_Access is access all Single_Sample_Data;

   type Global_Sample_Array is array (Valid_Sample_Index) of aliased Single_Sample_Data
     with Size => Storage.Sample_Library_Size * 8;

   type Global_Sample_Array_Access is access all Global_Sample_Array;

   function Sample_Data return not null Global_Sample_Array_Access;

   subtype Sample_Entry_Name is String (1 .. 15);

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
