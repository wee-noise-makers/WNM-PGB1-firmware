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

pragma Warnings (Off);
with WNM_Configuration; use WNM_Configuration;
with WNM_HAL;           use WNM_HAL;
with Enum_Next;
pragma Warnings (On);

package WNM is

   subtype Keyboard_Button is Button range B1 .. B16;

   type Keyboard_Value is range 1 .. 16;

   function To_Value (B : Keyboard_Button) return Keyboard_Value;
   function To_Button (V : Keyboard_Value) return Keyboard_Button;

   subtype Tracks is Keyboard_Value;
   subtype Patterns is Keyboard_Value;
   subtype Chords is Keyboard_Value;

   subtype Beat_Per_Minute is Positive range 50 .. 200;
   subtype Sequencer_Steps is Keyboard_Value;

   Steps_Per_Beat      : constant := 4;
   Max_Events_Per_Step : constant := 6;

   UI_Task_Period_Microseconds  : constant := 50 * 1_000;
   GUI_Task_Period_Microseconds : constant := 50 * 1_000;
   LED_Task_Period_Microseconds : constant := 50 * 1_000;

   Long_Press_Time_Span_Microseconds : constant := 300 * 1_000;
   --  How much time (in miliseconds) users have to press a button to get the
   --  alternative function.

   Audio_Queue_Size : constant := 3;

   function Img (V : Keyboard_Value) return String
   is (case V is
          when 1 => "01",
          when 2 => "02",
          when 3 => "03",
          when 4 => "04",
          when 5 => "05",
          when 6 => "06",
          when 7 => "07",
          when 8 => "08",
          when 9 => "09",
          when 10 => "10",
          when 11 => "11",
          when 12 => "12",
          when 13 => "13",
          when 14 => "14",
          when 15 => "15",
          when 16 => "16")
   with Inline_Always;

   type Rand_Percent is range 0 .. 100;
   function Random return Rand_Percent;

   generic
      type T is (<>);
   function Enum_Count return Natural;

end WNM;
