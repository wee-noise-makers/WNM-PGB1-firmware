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

   function QOA_Predict (LMS : Work_LMS) return S32
     with Inline_Always, Linker_Section => Code_Linker_Section;

   function QOA_Clamp (V, Min, Max : S32) return S32
     with Inline_Always, Linker_Section => Code_Linker_Section;

   procedure QOA_LMS_Update (LMS      : in out Work_LMS;
                             Sample   : S32;
                             Residual : S32)
     with Linker_Section => Code_Linker_Section;

   -----------------
   -- QOA_Predict --
   -----------------

   function QOA_Predict (LMS : Work_LMS) return S32 is
      Prediction : Integer_64 := 0;
   begin
      for X in LMS_Array'Range loop
         Prediction := Prediction +
           Integer_64 (LMS.Weight (X)) * Integer_64 (LMS.History (X));
         null;
      end loop;
      return S32
        ((Prediction - (Prediction mod 2**13)) / 2**13); -- Shift_Right
   end QOA_Predict;

   -------------
   -- QOA_Div --
   -------------

   function QOA_Div (V, Scalefactor : S32) return S32 is
      Reciprocal : constant S32 := RECIPROCAL_TAB (Scalefactor);
      Tmp : constant S64 := S64 (V) * Integer_64 (Reciprocal)
        + (1 * (2 ** 15));
      N : constant S32 := S32 ((Tmp - (Tmp mod 2**16)) / 2**16);
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

   function QOA_Clamp (V, Min, Max : S32) return S32
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
                             Sample   : S32;
                             Residual : S32)
   is
      D : constant S32 := (Residual - (Residual mod 2**4)) / 2**4;
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
      In_Idx : QOA_Sample_Point_Count := Audio'First;

      Best_Scalefactor : Scalefactor_Id;
      Best_LMS : Work_LMS;
      Best_Error : Unsigned_64;

      State : Encoder_State;

      Frame_Cnt : Natural := 0;
      Slice_Cnt : Natural := 0;
   begin
      Reset (State);

      for Frame of QOA_Data loop
         Frame_Cnt := Frame_Cnt + 1;
         --  Put_Line ("##### Frame:" & Frame_Cnt'Img);
         Clamp_Weights (State);

         --  Write current LMS State
         for X in LMS_Array'Range loop
            Frame.History (X) :=
              Tresses.Mono_Point
                (Tresses.DSP.Clip_S16 (State.Base_LMS.History (X)));
            Frame.Weight (X) :=
              Tresses.Mono_Point
                (Tresses.DSP.Clip_S16 (State.Base_LMS.Weight (X)));
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
                  Scalefactor : constant S32 :=
                    (Sfi + State.Prev_Scalefactor) mod 16;

                  --  We have to reset the LMS state to the last known good
                  --  one before trying each scalefactor, as each pass updates
                  --  the LMS state when encoding.
                  LMS : Work_LMS := State.Base_LMS;

                  Current_Error : Unsigned_64 := 0;
                  Current_Slice : Slice;
               begin

                  Current_Slice.Quantize_Scalefactor := UInt4 (Scalefactor);
                  for Res_Idx in Slice_Residuals'Range loop
                     declare
                        Input_Sample : constant S32 :=
                          S32 (Audio (In_Idx + Res_Idx));

                        Predicted : constant S32 := QOA_Predict (LMS);

                        Residual : constant S32 :=
                          Input_Sample - Predicted;

                        Scaled : constant S32 :=
                          QOA_Div (Residual, Scalefactor);

                        Clamped : constant S32 :=
                          QOA_Clamp (Scaled, -8, 8);

                        Quantized : constant S32 :=
                          QUANT_TAB (Clamped);

                        Dequantized : constant S32 :=
                          DEQUANT_TAB (UInt4 (Scalefactor),
                                       UInt3 (Quantized));

                        Reconstructed : constant S32 :=
                          QOA_Clamp (Predicted + Dequantized,
                                     S32 (Integer_16'First),
                                     S32 (Integer_16'Last));

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

            State.Prev_Scalefactor := Best_Scalefactor;
            State.Base_LMS := Best_LMS;

            In_Idx := In_Idx + Points_Per_Slice;
         end loop;
         --  if Frame_Cnt = 5 then
         --     raise Program_Error;
         --  end if;
      end loop;
   end Encode;

   ------------------
   -- Encode_Frame --
   ------------------

   procedure Encode_Frame (State : in out Encoder_State;
                           Audio :        Frame_Audio_Data;
                           Frame :    out QOA_Frame)
   is
      In_Idx : QOA_Sample_Point_Index := Audio'First;

      Best_Scalefactor : Scalefactor_Id;
      Best_LMS : Work_LMS;
      Best_Error : Unsigned_64;

      Slice_Cnt : Natural := 0;
   begin
      Clamp_Weights (State);

      --  Write current LMS State
      for X in LMS_Array'Range loop
         Frame.History (X) :=
           Tresses.Mono_Point
             (Tresses.DSP.Clip_S16 (State.Base_LMS.History (X)));
         Frame.Weight (X) :=
           Tresses.Mono_Point
             (Tresses.DSP.Clip_S16 (State.Base_LMS.Weight (X)));
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
               Scalefactor : constant S32 :=
                 (Sfi + State.Prev_Scalefactor) mod 16;

               --  We have to reset the LMS state to the last known good
               --  one before trying each scalefactor, as each pass updates
               --  the LMS state when encoding.
               LMS : Work_LMS := State.Base_LMS;

               Current_Error : Unsigned_64 := 0;
               Current_Slice : Slice;
            begin

               Current_Slice.Quantize_Scalefactor := UInt4 (Scalefactor);
               for Res_Idx in Slice_Residuals'Range loop
                  declare
                     Input_Sample : constant S32 :=
                       S32 (Audio (In_Idx + Res_Idx));

                     Predicted : constant S32 := QOA_Predict (LMS);

                     Residual : constant S32 :=
                       Input_Sample - Predicted;

                     Scaled : constant S32 :=
                       QOA_Div (Residual, Scalefactor);

                     Clamped : constant S32 :=
                       QOA_Clamp (Scaled, -8, 8);

                     Quantized : constant S32 :=
                       QUANT_TAB (Clamped);

                     Dequantized : constant S32 :=
                       DEQUANT_TAB (UInt4 (Scalefactor),
                                    UInt3 (Quantized));

                     Reconstructed : constant S32 :=
                       QOA_Clamp (Predicted + Dequantized,
                                  S32 (Integer_16'First),
                                  S32 (Integer_16'Last));

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

         State.Prev_Scalefactor := Best_Scalefactor;
         State.Base_LMS := Best_LMS;
         In_Idx := In_Idx + Points_Per_Slice;
      end loop;
   end Encode_Frame;

   ------------------
   -- Decode_Frame --
   ------------------

   procedure Decode_Frame (Frame :     QOA_Frame;
                           Audio : out Frame_Audio_Data)
   is

      LMS : Work_LMS;

      Out_Idx : QOA_Sample_Point_Index := Audio'First;
   begin
      for X in LMS_Array'Range loop
         LMS.History (X) := S32 (Frame.History (X));
         LMS.Weight (X)  := S32 (Frame.Weight (X));
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

      for Slice of Frame.Slices loop

         for Quantized of Slice.Residuals loop
            declare
               Predicted : constant S32 := QOA_Predict (LMS);
               Dequantized : constant S32 :=
                 DEQUANT_TAB (Slice.Quantize_Scalefactor, Quantized);
               Reconstructed : constant S32 :=
                 QOA_Clamp (Predicted + Dequantized,
                            S32 (Integer_16'First),
                            S32 (Integer_16'Last));
            begin
               --  Put_Line ("-----");
               --  Put_Line ("predicted:" & Predicted'Img);
               --  Put_Line ("scalefactor:" & Slice.Quantize_Scalefactor'Img);
               --  Put_Line ("quantized:" & Quantized'Img);
               --  Put_Line ("dequantized:" & Dequantized'Img);
               --  Put_Line ("reconstructed:" & Reconstructed'Img);
               Audio (Out_Idx) := Tresses.Mono_Point (Reconstructed);
               Out_Idx := Out_Idx + 1;
               QOA_LMS_Update (LMS, Reconstructed, Dequantized);
            end;
         end loop;
      end loop;
   end Decode_Frame;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate (Cache : in out Decoder_Cache) is
   begin
      Cache.Valid := False;
   end Invalidate;

   --------------------------
   -- Decode_Single_Sample --
   --------------------------

   function Decode_Single_Sample (Cache    : in out Decoder_Cache;
                                  QOA_Data :        QOA_Sample;
                                  Idx      :        QOA_Sample_Point_Index)
                                  return Tresses.Mono_Point
   is
      --  First we have to find in which frame and which slice the point is
      Frame_Idx : constant QOA_Sample_Point_Index := Idx / Points_Per_Frame;
      Sample_Idx : constant QOA_Sample_Point_Index := Idx mod Points_Per_Frame;

   begin
      --  Put_Line ("Global Idx" & Idx'Img);
      --  Put_Line ("Frame Idx" & Frame_Idx'Img);
      --  Put_Line ("Sample Idx" & Sample_Idx'Img);

      --  Check if not in cached frame
      if not Cache.Valid or else Cache.Frame_Idx /= Frame_Idx then

         --  Decode frame in cache
         Decode_Frame (QOA_Data (QOA_Frame_Index (Frame_Idx)),
                       Cache.Frame_Points);
         Cache.Frame_Idx := Frame_Idx;
         Cache.Valid := True;
      end if;

      return Cache.Frame_Points (Sample_Idx);
   end Decode_Single_Sample;

   -----------
   -- Reset --
   -----------

   procedure Reset (State : out Encoder_State) is
   begin
      State := (others => <>);
      State.Base_LMS.Weight (2) := -2**13;
      State.Base_LMS.Weight (3) := 2**14;
   end Reset;

   -------------------
   -- Clamp_Weights --
   -------------------

   procedure Clamp_Weights (State : in out Encoder_State) is
      A, B, C, D : Integer_64;
   begin
      A := Integer_64 (State.Base_LMS.Weight (0)) ** 2;
      B := Integer_64 (State.Base_LMS.Weight (1)) ** 2;
      C := Integer_64 (State.Base_LMS.Weight (2)) ** 2;
      D := Integer_64 (State.Base_LMS.Weight (3)) ** 2;

      if (A + B + C + D) > 16#2fffffff# then
         State.Base_LMS.Weight := (others => 0);
      end if;
   end Clamp_Weights;

end WNM.QOA;
