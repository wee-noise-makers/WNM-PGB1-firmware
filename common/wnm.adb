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

with HAL; use HAL;

package body WNM is

   --------------
   -- To_Value --
   --------------

   function To_Value (B : Keyboard_Button) return Keyboard_Value
   is (case B is
          when C1 => 1,
          when C2 => 2,
          when C3 => 3,
          when C4 => 4,
          when L1 => 5,
          when L2 => 6,
          when L3 => 7,
          when L4 => 8,
          when C5 => 9,
          when C6 => 10,
          when C7 => 11,
          when C8 => 12,
          when L5 => 13,
          when L6 => 14,
          when L7 => 15,
          when L8 => 16);

   ---------------
   -- To_Button --
   ---------------

   function To_Button (V : Keyboard_Value) return Keyboard_Button
   is (case V is
          when 1  => C1,
          when 2  => C2,
          when 3  => C3,
          when 4  => C4,
          when 5  => L1,
          when 6  => L2,
          when 7  => L3,
          when 8  => L4,
          when 9  => C5,
          when 10 => C6,
          when 11 => C7,
          when 12 => C8,
          when 13 => L5,
          when 14 => L6,
          when 15 => L7,
          when 16 => L8);

   Rand_X : UInt32 := 123456789;
   Rand_Y : UInt32 := 362436069;
   Rand_Z : UInt32 := 521288629;

   ------------
   -- Random --
   ------------

   function Random return Rand_Percent is
      T : UInt32;
   begin
      Rand_X := Rand_X xor Shift_Left (Rand_X, 16);
      Rand_X := Rand_X xor Shift_Right (Rand_X, 5);
      Rand_X := Rand_X xor Shift_Left (Rand_X, 1);

      T := Rand_X;
      Rand_X := Rand_Y;
      Rand_Y := Rand_Z;
      Rand_Z := T xor Rand_X xor Rand_Y;

      return Rand_Percent (Rand_Z mod 100);
   end Random;

   ----------------
   -- Enum_Count --
   ----------------

   function Enum_Count return Natural
   is (T'Pos (T'Last) - T'Pos (T'First) + 1);

   ---------
   -- Img --
   ---------

   function Img (L : CPU_Load) return String is
      type Load_Img is delta 0.01 range 0.0 .. 10000.0;
      --  Use a fixed point type to get a 'Img without
      --  scientific notation...

      Clamp : Load_Img;

   begin
      if L > CPU_Load (Load_Img'Last) then
         Clamp := Load_Img'Last;
      else
         Clamp := Load_Img (L);
      end if;

      return Clamp'Img;
   end Img;

end WNM;
