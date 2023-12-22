-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2023 Fabien Chouteau                    --
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

package WNM.QOA is

   Code_Linker_Section : constant String := ".time_critical.qoa_code";
   Data_Linker_Section : constant String := ".time_critical.qoa_data";

   Points_Per_Slice : constant := 20; -- From QOA specs

   --  QOA parameters --

   --  We use a slightly modified version of QOA encoding. The number of
   --  slices per frame is reduced (from original 256) to decode smaller
   --  pieces of the sample. This is because using QOA an entire frame has
   --  to be decoded all at once.
   Slices_Per_Frame  : constant := 8; -- To get smaller frames to decode
   Frames_Per_Sample : constant := 562; -- To maximize the use of sample area

   Points_Per_Frame : constant := Slices_Per_Frame * Points_Per_Slice;
   Points_Per_Sample : constant := Frames_Per_Sample * Points_Per_Frame;

   Slice_Bit_Size : constant := Points_Per_Slice * 3 + 4;
   LMS_Data_Bit_Size : constant := 16 * 4 * 2;
   Frame_Bit_Size : constant :=
     LMS_Data_Bit_Size + Slice_Bit_Size * Slices_Per_Frame;
   Sample_Bit_Size : constant := Frame_Bit_Size * Frames_Per_Sample;

   type Sample_Point_Count is range 0 .. Points_Per_Sample;
   subtype Sample_Point_Index
     is Sample_Point_Count range 0 .. Sample_Point_Count'Last - 1;

   type Slice_Residuals
   is array (Sample_Point_Count range 0 .. Points_Per_Slice - 1) of UInt3
     with Pack, Size => Points_Per_Slice * 3;

   type Slice is record
      Quantize_Scalefactor : UInt4;
      Residuals : Slice_Residuals;
   end record
     with Pack, Size => Slice_Bit_Size;

   type Slice_Array is array (0 .. Slices_Per_Frame - 1) of Slice
     with Pack, Size => Slices_Per_Frame * Slice_Bit_Size;

   type LMS_Array is array (0 .. 3) of Mono_Point
     with Size => 16 * 4;

   type Frame is record
      History : LMS_Array;
      Weight  : LMS_Array;
      Slices  : Slice_Array;
   end record
     with Pack, Size => Frame_Bit_Size;

   type QOA_Sample is array (0 .. Frames_Per_Sample - 1) of Frame
     with Size => Sample_Bit_Size;

   type Sample_Audio_Data is array (Sample_Point_Index) of Mono_Point
     with Size => Points_Per_Sample * 16;

   procedure Encode (Audio : Sample_Audio_Data; QOA_Data : out QOA_Sample);

   type Frame_Audio_Data
   is array (Sample_Point_Index range 0 .. Points_Per_Frame - 1)
     of Mono_Point
     with Size => Points_Per_Frame * 16;

   procedure Decode_Frame (QOA_Frame : Frame;
                           Audio : out Frame_Audio_Data)
     with Linker_Section => Code_Linker_Section;

   function Decode_Single_Sample (QOA_Data : QOA_Sample;
                                  Idx      : Sample_Point_Index)
                                  return Mono_Point;
end WNM.QOA;
