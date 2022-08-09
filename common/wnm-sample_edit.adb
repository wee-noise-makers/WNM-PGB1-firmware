-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2022 Fabien Chouteau                    --
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

package body WNM.Sample_Edit is

   Work_RAM : Sample_Library.Single_Sample_Data;
   Work_Start : Sample_Point_Index := Sample_Point_Index'First;
   Work_Stop : Sample_Point_Index := Sample_Point_Index'Last;

   ----------
   -- Load --
   ----------

   procedure Load (Index : Sample_Library.Valid_Sample_Index;
                   Start, Stop : Sample_Point_Index)
   is
   begin
      Work_Start := Start;
      Work_Stop := Stop;
      Work_RAM := Sample_Library.Sample_Data (Index);
   end Load;

   -----------
   -- Start --
   -----------

   function Start return Sample_Point_Index
   is (Work_Start);

   ----------
   -- Stop --
   ----------

   function Stop return Sample_Point_Index
   is (Work_Stop);

   ---------------
   -- Inc_Start --
   ---------------

   procedure Inc_Start is
      Point_Len : constant Sample_Point_Count := Stop - Start;
      Chunk_Len : constant Sample_Point_Count := Point_Len / Waveform'Length;
   begin
      if Work_Start < Work_Stop then
         Work_Start := Work_Start + Chunk_Len;
      end if;
   end Inc_Start;

   --------------
   -- Inc_Stop --
   --------------

   procedure Inc_Stop is
      Point_Len : constant Sample_Point_Count := Stop - Start;
      Chunk_Len : constant Sample_Point_Count := Point_Len / Waveform'Length;
   begin
      if Work_Stop /= Sample_Point_Index'Last then
         Work_Stop := Work_Stop + Chunk_Len;
      end if;
   end Inc_Stop;

   ---------------
   -- Dec_Start --
   ---------------

   procedure Dec_Start is
      Point_Len : constant Sample_Point_Count := Stop - Start;
      Chunk_Len : constant Sample_Point_Count := Point_Len / Waveform'Length;
   begin
      if Work_Start > Sample_Point_Index'First  then
         Work_Start := Work_Start - Chunk_Len;
      end if;
   end Dec_Start;

   --------------
   -- Dec_Stop --
   --------------

   procedure Dec_Stop is
      Point_Len : constant Sample_Point_Count := Stop - Start;
      Chunk_Len : constant Sample_Point_Count := Point_Len / Waveform'Length;
   begin
      if Work_Stop > Work_Start then
         Work_Stop := Work_Stop - Chunk_Len;
      end if;
   end Dec_Stop;

   ---------------------
   -- Update_Waveform --
   ---------------------

   procedure Update_Waveform is
      use type Audio.Mono_Point;

      Point_Len : constant Sample_Point_Count := Stop - Start;
      Chunk_Len : constant Sample_Point_Count := Point_Len / Waveform'Length;

      Point_Cursor : Sample_Point_Index := Work_Start;

      Acc : Float;
   begin
      for Elt of Waveform loop

         --  Calculate mean of audio points in a chunk
         Acc := 0.0;
         for Count in 1 .. Chunk_Len loop
            if Point_Cursor in Start .. Stop - 1 then
               Acc := Acc + Float (abs Work_RAM (Point_Cursor));
               Point_Cursor := Point_Cursor + 1;
            end if;

         end loop;

         Acc := (Acc * 2.0) / Float (Chunk_Len);

         Elt := Waveform_Point (Acc / Float (WNM.Audio.Mono_Point'Last));
      end loop;
   end Update_Waveform;

end WNM.Sample_Edit;
