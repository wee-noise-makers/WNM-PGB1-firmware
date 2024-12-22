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
with WNM.Sample_Library;
with Tresses;

package WNM.QOA is

   Code_Linker_Section : constant String := ".time_critical.qoa_code";
   Data_Linker_Section : constant String := ".time_critical.qoa_data";

   Points_Per_Slice : constant := 20; -- From QOA specs

   --  QOA parameters --

   --  We use a slightly modified version of QOA encoding. The number of
   --  slices per frame is reduced (from original 256) to decode smaller
   --  pieces of the sample. This is because using QOA an entire frame has
   --  to be decoded all at once.
   Slices_Per_Frame  : constant := 8;
   --  To get smaller frames to decode

   Frames_Per_Sample : constant := 564;
   --  To be able to encode a full sample of
   --  WNM.Sample_Library.Points_Per_Sample points in QOA.

   Points_Per_Frame : constant := Slices_Per_Frame * Points_Per_Slice;
   Points_Per_Sample : constant := Frames_Per_Sample * Points_Per_Frame;

   pragma Compile_Time_Error
     (Points_Per_Sample < WNM.Sample_Library.Points_Per_Sample,
      "QOA sample too small");

   Slice_Bit_Size : constant := Points_Per_Slice * 3 + 4;
   LMS_Data_Bit_Size : constant := 16 * 4 * 2;
   Frame_Bit_Size : constant :=
     LMS_Data_Bit_Size + Slice_Bit_Size * Slices_Per_Frame;
   Sample_Bit_Size : constant := Frame_Bit_Size * Frames_Per_Sample;

   type QOA_Sample_Point_Count is range 0 .. Points_Per_Sample;
   subtype QOA_Sample_Point_Index
     is QOA_Sample_Point_Count range 0 .. QOA_Sample_Point_Count'Last - 1;

   type Slice_Residuals
   is array (QOA_Sample_Point_Count range 0 .. Points_Per_Slice - 1) of UInt3
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

   type QOA_Frame is record
      History : LMS_Array;
      Weight  : LMS_Array;
      Slices  : Slice_Array;
   end record
     with Pack, Size => Frame_Bit_Size;

   type QOA_Frame_Index is mod Frames_Per_Sample;
   type QOA_Sample is array (QOA_Frame_Index) of QOA_Frame
     with Size => Sample_Bit_Size;

   type Sample_Audio_Data is array (QOA_Sample_Point_Index) of Mono_Point
     with Size => Points_Per_Sample * 16;

   procedure Encode (Audio : Sample_Audio_Data; QOA_Data : out QOA_Sample);

   subtype QOA_Frame_Point_Index
     is QOA_Sample_Point_Index range 0 .. Points_Per_Frame - 1;
   type Frame_Audio_Data is array (QOA_Frame_Point_Index) of Mono_Point
     with Size => Points_Per_Frame * 16;

   type Encoder_State is private;
   procedure Reset (State : out Encoder_State);

   procedure Encode_Frame (State : in out Encoder_State;
                           Audio :        Frame_Audio_Data;
                           Frame :    out QOA_Frame)
     with Linker_Section => Code_Linker_Section;

   procedure Decode_Frame (Frame : QOA_Frame;
                           Audio : out Frame_Audio_Data)
     with Linker_Section => Code_Linker_Section;

   type Decoder_Cache is private;

   procedure Invalidate (Cache : in out Decoder_Cache);

   function Decode_Single_Sample (Cache    : in out Decoder_Cache;
                                  QOA_Data :        QOA_Sample;
                                  Idx      :        QOA_Sample_Point_Index)
                                  return  Tresses.Mono_Point
     with Linker_Section => QOA.Code_Linker_Section;

private

   type Decoder_Cache is record
      Valid : Boolean := False;
      Frame_Idx : QOA_Sample_Point_Index := QOA_Sample_Point_Index'First;
      Frame_Points : Frame_Audio_Data;
   end record;

   use Tresses;

   subtype Scalefactor_Id is Tresses.S32 range 0 .. 15;

   --  The decoder LMS data is 32 bits while the stored data is 16 bits
   type Work_LMS_Array is array (LMS_Array'Range) of Tresses.S32;

   type Work_LMS is record
      Weight : Work_LMS_Array := (others => 0);
      History : Work_LMS_Array := (others => 0);
   end record;

   subtype Quant_Int is Tresses.S32 range 0 .. 7;
   QUANT_TAB : constant array (Tresses.S32 range -8 .. 8) of Quant_Int :=
     (7, 7, 7, 5, 5, 3, 3,
      1, 0, 0, 2, 2, 4, 4,
      6, 6, 6);

   --  subtype Scale_Int is Integer_32 range 1 .. 2048;
   --  SCALEFACTOR_TAB : constant array (Integer_32 range 0 .. 15)
   --    of Scale_Int :=
   --    (1, 7, 21, 45, 84, 138, 211, 304, 421,
   --     562, 731, 928, 1157, 1419, 1715, 2048);

   subtype Recip_Int is S32 range 32 .. 65536;
   RECIPROCAL_TAB : constant array (S32 range 0 .. 15) of Recip_Int :=
     (65536, 9363, 3121, 1457, 781, 475, 311,
      216, 156, 117, 90, 71, 57, 47, 39, 32);

   subtype Dequant_Int is S32 range -14336 .. 14336;
   DEQUANT_TAB : constant array (UInt4, UInt3) of Dequant_Int :=
     ((1,    -1,    3,    -3,    5,    -5,     7,     -7),
      (5,    -5,   18,   -18,   32,   -32,    49,    -49),
      (16,   -16,   53,   -53,   95,   -95,   147,   -147),
      (34,   -34,  113,  -113,  203,  -203,   315,   -315),
      (63,   -63,  210,  -210,  378,  -378,   588,   -588),
      (104,  -104,  345,  -345,  621,  -621,   966,   -966),
      (158,  -158,  528,  -528,  950,  -950,  1477,  -1477),
      (228,  -228,  760,  -760, 1368, -1368,  2128,  -2128),
      (316,  -316, 1053, -1053, 1895, -1895,  2947,  -2947),
      (422,  -422, 1405, -1405, 2529, -2529,  3934,  -3934),
      (548,  -548, 1828, -1828, 3290, -3290,  5117,  -5117),
      (696,  -696, 2320, -2320, 4176, -4176,  6496,  -6496),
      (868,  -868, 2893, -2893, 5207, -5207,  8099,  -8099),
      (1064, -1064, 3548, -3548, 6386, -6386,  9933,  -9933),
      (1286, -1286, 4288, -4288, 7718, -7718, 12005, -12005),
      (1536, -1536, 5120, -5120, 9216, -9216, 14336, -14336))
       with Linker_Section => Data_Linker_Section;

   type Encoder_State is record
      Base_LMS : Work_LMS := (Weight => (others => 0),
                              History => (others => 0));
      Prev_Scalefactor : S32 := 0;
   end record;

   procedure Clamp_Weights (State : in out Encoder_State);

end WNM.QOA;
