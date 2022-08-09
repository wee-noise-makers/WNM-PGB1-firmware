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

   ---------
   -- Img --
   ---------

   function Img (T : Trigger) return String
   is (case T is
          when None        => "None",
          when Always      => "Always",
          when Fill        => "Fill",
          when Percent_25  => "25%",
          when Percent_50  => "50%",
          when Percent_75  => "75%",
          when One_Of_Two  => "1/2",
          when One_Of_Three => "1/3");

   ---------
   -- Img --
   ---------

   function Img (D : Note_Duration) return String
   is (case D is
          when Double  => "Double",
          when Whole   => "Whole",
          when Half    => "Half",
          when Quarter => "Quarter",
          when N_8th   => "8th",
          when N_16th  => "16th",
          when N_32nd  => "32nd");

   ---------
   -- Img --
   ---------

   function Img (R : Repeat_Rate) return String
   is (case R is
       when Rate_1_2  => "1/2",
       when Rate_1_3  => "1/3",
       when Rate_1_4  => "1/4",
       when Rate_1_5  => "1/5",
       when Rate_1_6  => "1/6",
       when Rate_1_8  => "1/8",
       when Rate_1_10 => "1/10",
       when Rate_1_12 => "1/12",
       when Rate_1_16 => "1/16",
       when Rate_1_20 => "1/20",
       when Rate_1_24 => "1/24",
       when Rate_1_32 => "1/32");

   --------------
   -- To_Value --
   --------------

   function To_Value (B : Keyboard_Button) return Keyboard_Value is
   begin
      return Keyboard_Value (B'Enum_Rep - B1'Enum_Rep + 1);
   end To_Value;

   ---------------
   -- To_Button --
   ---------------

   function To_Button (V : Keyboard_Value) return Keyboard_Button
   is (case V is
          when 1  => B1,
          when 2  => B2,
          when 3  => B3,
          when 4  => B4,
          when 5  => B5,
          when 6  => B6,
          when 7  => B7,
          when 8  => B8,
          when 9  => B9,
          when 10 => B10,
          when 11 => B11,
          when 12 => B12,
          when 13 => B13,
          when 14 => B14,
          when 15 => B15,
          when 16 => B16);

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

   --  --------------
   --  -- To_Track --
   --  --------------
   --
   --  function To_Track (Chan : MIDI.MIDI_Channel) return Tracks
   --  is (case Chan is
   --         when 0      => B1,
   --         when 1      => B2,
   --         when 2      => B3,
   --         when 3      => B4,
   --         when 4      => B5,
   --         when 5      => B6,
   --         when 6      => B7,
   --         when 7      => B8,
   --         when 8      => B9,
   --         when 9      => B10,
   --         when 10     => B11,
   --         when 11     => B12,
   --         when 12     => B13,
   --         when 13     => B14,
   --         when 14     => B15,
   --         when 15     => B16);
   --
   --  ---------------------
   --  -- To_MIDI_Channel --
   --  ---------------------
   --
   --  function To_MIDI_Channel (Chan : Tracks) return MIDI.MIDI_Channel
   --  is (case Chan is
   --         when B1  => 0,
   --         when B2  => 1,
   --         when B3  => 2,
   --         when B4  => 3,
   --         when B5  => 4,
   --         when B6  => 5,
   --         when B7  => 6,
   --         when B8  => 7,
   --         when B9  => 8,
   --         when B10 => 9,
   --         when B11 => 10,
   --         when B12 => 11,
   --         when B13 => 12,
   --         when B14 => 13,
   --         when B15 => 14,
   --         when B16 => 15);

end WNM;
