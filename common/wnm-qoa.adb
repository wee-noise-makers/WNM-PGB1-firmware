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

with Interfaces; use Interfaces;

with Tresses.DSP;

package body WNM.QOA is

   subtype Scalefactor_Id is Integer_32 range 0 .. 15;

   --  The decoder LMS data is 32 bits while the stored data is 16 bits
   type Work_LMS_Array is array (LMS_Array'Range) of Integer_32;

   type Work_LMS is record
      Weight : Work_LMS_Array := (others => 0);
      History : Work_LMS_Array := (others => 0);
   end record;

   subtype Quant_Int is Integer_32 range 0 .. 7;
   QUANT_TAB : constant array (Integer_32 range -8 .. 8) of Quant_Int :=
     (7, 7, 7, 5, 5, 3, 3,
      1, 0, 0, 2, 2, 4, 4,
      6, 6, 6);

   --  subtype Scale_Int is Integer_32 range 1 .. 2048;
   --  SCALEFACTOR_TAB : constant array (Integer_32 range 0 .. 15)
   --    of Scale_Int :=
   --    (1, 7, 21, 45, 84, 138, 211, 304, 421,
   --     562, 731, 928, 1157, 1419, 1715, 2048);

   subtype Recip_Int is Integer_32 range 32 .. 65536;
   RECIPROCAL_TAB : constant array (Integer_32 range 0 .. 15) of Recip_Int :=
     (65536, 9363, 3121, 1457, 781, 475, 311,
      216, 156, 117, 90, 71, 57, 47, 39, 32);

   subtype Dequant_Int is Integer_32 range -14336 .. 14336;
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

   function QOA_Predict (LMS : Work_LMS) return Integer_32
     with Inline_Always, Linker_Section => Code_Linker_Section;

   function QOA_Clamp (V, Min, Max : Integer_32) return Integer_32
     with Inline_Always, Linker_Section => Code_Linker_Section;

   procedure QOA_LMS_Update (LMS      : in out Work_LMS;
                             Sample   : Integer_32;
                             Residual : Integer_32)
     with Linker_Section => Code_Linker_Section;

   -----------------
   -- QOA_Predict --
   -----------------

   function QOA_Predict (LMS : Work_LMS) return Integer_32 is
      Prediction : Integer_64 := 0;
   begin
      for X in LMS_Array'Range loop
         Prediction := Prediction +
           Integer_64 (LMS.Weight (X)) * Integer_64 (LMS.History (X));
         null;
      end loop;
      return Integer_32
        ((Prediction - (Prediction mod 2**13)) / 2**13); -- Shift_Right
   end QOA_Predict;

   -------------
   -- QOA_Div --
   -------------

   function QOA_Div (V, Scalefactor : Integer_32) return Integer_32 is
      Reciprocal : constant Integer_32 := RECIPROCAL_TAB (Scalefactor);
      Tmp : constant Integer_64 := Integer_64 (V) * Integer_64 (Reciprocal)
        + (1 * (2 ** 15));
      N : constant Integer_32 := Integer_32 ((Tmp - (Tmp mod 2**16)) / 2**16);
   begin
      if V > 0 and then N < 0 then
         return N + 2;
      elsif V < 0 and then N > 0 then
         return N - 2;
      elsif V > 0 and then N = 0 then
         return N + 1;
      elsif V < 0 and then N = 0 then
         return N - 1;
      elsif V = 0 and then N > 0 then
         return N - 1;
      elsif V = 0 and then N < 0 then
         return N + 1;
      else
         return N;
      end if;
   end QOA_Div;

   ---------------
   -- QOA_Clamp --
   ---------------

   function QOA_Clamp (V, Min, Max : Integer_32) return Integer_32
   is
   begin
      if V < Min then
         return Min;
      else
         if V > Max then
            return Max;
         else
            return V;
         end if;
      end if;
   end QOA_Clamp;

   --------------------
   -- QOA_LMS_Update --
   --------------------

   procedure QOA_LMS_Update (LMS      : in out Work_LMS;
                             Sample   : Integer_32;
                             Residual : Integer_32)
   is
      D : constant Integer_32 := (Residual - (Residual mod 2**4)) / 2**4;
   begin
      for X in LMS_Array'Range loop
         if LMS.History (X) < 0 then
            LMS.Weight (X) := LMS.Weight (X) - D;
         else
            LMS.Weight (X) := LMS.Weight (X) +  D;
         end if;
      end loop;
      for X in LMS_Array'First .. LMS_Array'Last - 1 loop
         LMS.History (X) := LMS.History (X + 1);
      end loop;
      LMS.History (LMS.History'Last) := Sample;
   end QOA_LMS_Update;

   ------------
   -- Encode --
   ------------

   procedure Encode (Audio : Sample_Audio_Data; QOA_Data : out QOA_Sample) is
      In_Idx : Sample_Point_Count := Audio'First;

      Base_LMS : Work_LMS := (Weight => (others => 0),
                              History => (others => 0));

      Prev_Scalefactor : Integer_32 := 0;

      -------------------
      -- Clamp_Weights --
      -------------------

      procedure Clamp_Weights is
         A, B, C, D : Integer_64;
      begin
         A := Integer_64 (Base_LMS.Weight (0)) ** 2;
         B := Integer_64 (Base_LMS.Weight (1)) ** 2;
         C := Integer_64 (Base_LMS.Weight (2)) ** 2;
         D := Integer_64 (Base_LMS.Weight (3)) ** 2;

         if (A + B + C + D) > 16#2fffffff# then
            Base_LMS.Weight := (others => 0);
         end if;
      end Clamp_Weights;

      Best_Scalefactor : Scalefactor_Id;
      Best_LMS : Work_LMS;
      Best_Error : Unsigned_64;

      Frame_Cnt : Natural := 0;
      Slice_Cnt : Natural := 0;
   begin
      Base_LMS.Weight (2) := -2**13;
      Base_LMS.Weight (3) := 2**14;

      for Frame of QOA_Data loop
         Frame_Cnt := Frame_Cnt + 1;
         --  Put_Line ("##### Frame:" & Frame_Cnt'Img);
         Clamp_Weights;

         --  Write current LMS State
         for X in LMS_Array'Range loop
            Frame.History (X) :=
              Mono_Point (Tresses.DSP.Clip_S16 (Base_LMS.History (X)));
            Frame.Weight (X) :=
              Mono_Point (Tresses.DSP.Clip_S16 (Base_LMS.Weight (X)));
         end loop;

         Slice_Cnt := 0;
         for Best_Slice of Frame.Slices loop
            Slice_Cnt := Slice_Cnt + 1;

            --  Brute for search for the best scalefactor. Just go through all
            --  16 scalefactors, encode all samples for the current slice and
            --  meassure the total squared error.

            Best_Scalefactor := Scalefactor_Id'First;
            Best_Error := Unsigned_64'Last;

            for Sfi in Scalefactor_Id loop
               --  There is a strong correlation between the scalefactors of
               --  neighboring slices. As an optimization, start testing the
               --  best scalefactor of the previous slice first.

               declare
                  Scalefactor : constant Integer_32 :=
                    (Sfi + Prev_Scalefactor) mod 16;

                  --  We have to reset the LMS state to the last known good
                  --  one before trying each scalefactor, as each pass updates
                  --  the LMS state when encoding.
                  LMS : Work_LMS := Base_LMS;

                  Current_Error : Unsigned_64 := 0;
                  Current_Slice : Slice;
               begin

                  Current_Slice.Quantize_Scalefactor := UInt4 (Scalefactor);
                  for Res_Idx in Slice_Residuals'Range loop
                     declare
                        Input_Sample : constant Integer_32 :=
                          Integer_32 (Audio (In_Idx + Res_Idx));

                        Predicted : constant Integer_32 := QOA_Predict (LMS);

                        Residual : constant Integer_32 :=
                          Input_Sample - Predicted;

                        Scaled : constant Integer_32 :=
                          QOA_Div (Residual, Scalefactor);

                        Clamped : constant Integer_32 :=
                          QOA_Clamp (Scaled, -8, 8);

                        Quantized : constant Integer_32 :=
                          QUANT_TAB (Clamped);

                        Dequantized : constant Integer_32 :=
                          DEQUANT_TAB (UInt4 (Scalefactor),
                                       UInt3 (Quantized));

                        Reconstructed : constant Integer_32 :=
                          QOA_Clamp (Predicted + Dequantized,
                                     Integer_32 (Integer_16'First),
                                     Integer_32 (Integer_16'Last));

                        Error : constant Integer_64 :=
                          Integer_64 (Input_Sample) -
                          Integer_64 (Reconstructed);

                     begin
                        --  if Sfi = 0 then
                        --     Put_Line ("------------------");
                        --     Put_Line ("in:" & Input_Sample'Img);
                        --     Put_Line ("lms: H(" &
                        --                 LMS.History (0)'Img & ", " &
                        --                 LMS.History (1)'Img & ", " &
                        --                 LMS.History (2)'Img & ", " &
                        --                 LMS.History (3)'Img & ")" &
                        --                 " W(" &
                        --                 LMS.Weight (0)'Img & ", " &
                        --                 LMS.Weight (1)'Img & ", " &
                        --                 LMS.Weight (2)'Img & ", " &
                        --                 LMS.Weight (3)'Img & ")");
                        --
                        --     Put_Line ("predicted:" & Predicted'Img);
                        --     Put_Line ("residual:" & Residual'Img);
                        --     Put_Line ("scaled:" & Scaled'Img);
                           --  Put_Line ("quantized:" & Quantized'Img);
                           --  Put_Line ("reconstructed:" & Reconstructed'Img);
                           --  Put_Line ("error:" & Error'Img);
                        --  end if;

                        Current_Error :=
                          Current_Error + Unsigned_64 (Error * Error);

                        --  Put_Line ("current_error:" & Current_Error'Img);
                        if Current_Error > Best_Error then
                           --  Put_Line ("!!! Big error break" &
                           --              Current_Error'Img  &
                           --              " >" & Best_Error'Img);
                           exit;

                        end if;

                        QOA_LMS_Update (LMS, Reconstructed, Dequantized);

                        Current_Slice.Residuals (Res_Idx) := UInt3 (Quantized);
                     end;
                  end loop;

                  --  Put_Line ("sfi:" & Sfi'Img &
                  --   " error:" & Current_Error'Img);

                  if Current_Error < Best_Error then
                     Best_Error := Current_Error;
                     Best_Slice := Current_Slice;
                     Best_LMS := LMS;
                     Best_Scalefactor := Scalefactor;
                  end if;
               end;
            end loop;

            --  Put_Line ("---------- Frame:" & Frame_Cnt'Img
            --            & " Slice:" & Slice_Cnt'Img);
            --  Put_Line ("best error:" & Best_Error'Img);
            --  Put_Line ("best scalefactor:" & Best_Scalefactor'Img);
            --  Put_Line ("best_lms: H(" &
            --              T (Best_LMS.History (0)'Img) & ", " &
            --              T (Best_LMS.History (1)'Img) & ", " &
            --              T (Best_LMS.History (2)'Img) & ", " &
            --              T (Best_LMS.History (3)'Img) & ")" &
            --              " W(" &
            --              T (Best_LMS.Weight (0)'Img) & ", " &
            --              T (Best_LMS.Weight (1)'Img) & ", " &
            --              T (Best_LMS.Weight (2)'Img) & ", " &
            --              T (Best_LMS.Weight (3)'Img) & ")");

            Prev_Scalefactor := Best_Scalefactor;
            Base_LMS := Best_LMS;

            In_Idx := In_Idx + Points_Per_Slice;
         end loop;
         --  if Frame_Cnt = 5 then
         --     raise Program_Error;
         --  end if;
      end loop;
   end Encode;

   ------------------
   -- Decode_Frame --
   ------------------

   procedure Decode_Frame (QOA_Frame : Frame;
                           Audio : out Frame_Audio_Data)
   is
      LMS : Work_LMS;

      Out_Idx : Sample_Point_Index := Audio'First;
   begin
      for X in LMS_Array'Range loop
         LMS.History (X) := Integer_32 (QOA_Frame.History (X));
         LMS.Weight (X)  := Integer_32 (QOA_Frame.Weight (X));
      end loop;

      --  Put_Line ("lms: H(" &
      --              T (LMS.History (0)'Img) & ", " &
      --              T (LMS.History (1)'Img) & ", " &
      --              T (LMS.History (2)'Img) & ", " &
      --              T (LMS.History (3)'Img) & ")" &
      --              " W(" &
      --              T (LMS.Weight (0)'Img) & ", " &
      --              T (LMS.Weight (1)'Img) & ", " &
      --              T (LMS.Weight (2)'Img) & ", " &
      --              T (LMS.Weight (3)'Img) & ")");

      for Slice of QOA_Frame.Slices loop

         for Quantized of Slice.Residuals loop
            declare
               Predicted : constant Integer_32 := QOA_Predict (LMS);
               Dequantized : constant Integer_32 :=
                 DEQUANT_TAB (Slice.Quantize_Scalefactor, Quantized);
               Reconstructed : constant Integer_32 :=
                 QOA_Clamp (Predicted + Dequantized,
                            Integer_32 (Integer_16'First),
                            Integer_32 (Integer_16'Last));
            begin
               --  Put_Line ("-----");
               --  Put_Line ("predicted:" & Predicted'Img);
               --  Put_Line ("scalefactor:" & Slice.Quantize_Scalefactor'Img);
               --  Put_Line ("quantized:" & Quantized'Img);
               --  Put_Line ("dequantized:" & Dequantized'Img);
               --  Put_Line ("reconstructed:" & Reconstructed'Img);
               Audio (Out_Idx) := Mono_Point (Reconstructed);
               Out_Idx := Out_Idx + 1;
               QOA_LMS_Update (LMS, Reconstructed, Dequantized);
            end;
         end loop;
      end loop;
   end Decode_Frame;

   --------------------------
   -- Decode_Single_Sample --
   --------------------------

   function Decode_Single_Sample (QOA_Data : QOA_Sample;
                                  Idx      : Sample_Point_Index)
                                  return Mono_Point
   is
      --  First we have to find in which frame and which slice the point is
      Frame_Idx : constant Sample_Point_Index := Idx / Points_Per_Frame;
      Sample_Idx : constant Sample_Point_Index := Idx mod Points_Per_Frame;

      Frame_Points : Frame_Audio_Data;

   begin
      --  Put_Line ("Global Idx" & Idx'Img);
      --  Put_Line ("Frame Idx" & Frame_Idx'Img);
      --  Put_Line ("Sample Idx" & Sample_Idx'Img);
      Decode_Frame (QOA_Data (Integer (Frame_Idx)), Frame_Points);

      return Frame_Points (Sample_Idx);
   end Decode_Single_Sample;

end WNM.QOA;
