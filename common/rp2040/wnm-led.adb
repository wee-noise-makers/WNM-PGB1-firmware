-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2021 Fabien Chouteau                  --
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
with WNM.Time;

with WNM.RP2040.WS2812;

package body WNM.LED is

   Next_Start : WNM.Time.Time_Microseconds :=
     WNM.Time.Clock + LED_Task_Period_Microseconds;

   Data : aliased RP2040.WS2812.LED_Data;

   LED_Red   : constant HAL.UInt32 := 16#00_FF_00#;
   LED_Green : constant HAL.UInt32 := 16#00_00_FF#;
   LED_Blue  : constant HAL.UInt32 := 16#FF_00_00#;

   function To_LED_Index (B : LEDs) return Natural
   is (case B is
          when B1   => Data'First + 0,
          when B2   => Data'First + 1,
          when B3   => Data'First + 2,
          when B4   => Data'First + 3,
          when B5   => Data'First + 4,
          when B6   => Data'First + 5,
          when B7   => Data'First + 6,
          when B8   => Data'First + 7,
          when B9   => Data'First + 9,
          when B10  => Data'First + 10,
          when B11  => Data'First + 11,
          when B12  => Data'First + 12,
          when B13  => Data'First + 13,
          when B14  => Data'First + 14,
          when B15  => Data'First + 15,
          when B16  => Data'First + 16,
          when Play => Data'First + 8,
          when Rec  => Data'First + 17);
   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (B : LEDs) is
   begin
      if B = Rec then
         Data (To_LED_Index (B)) := LED_Red;
      elsif B = Play then
         Data (To_LED_Index (B)) := LED_Green;
      else
         Data (To_LED_Index (B)) := LED_Blue;
      end if;
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (B : LEDs) is
   begin
      Data (To_LED_Index (B)) := 0;
   end Turn_Off;

   ------------------
   -- Turn_Off_All --
   ------------------

   procedure Turn_Off_All is
   begin
      Data := (others => 0);
   end Turn_Off_All;

   ----------------
   -- Brightness --
   ----------------

   function Brightness (B : LEDs) return UInt8
   is (0);

   ------------
   -- Update --
   ------------

   function Update return WNM.Time.Time_Microseconds is
   begin
      if WNM.Time.Clock >= Next_Start then

         RP2040.WS2812.Push_Data_DMA (Data'Access);

         Next_Start := Next_Start + LED_Task_Period_Microseconds;
      end if;

      return Next_Start;
   end Update;

end WNM.LED;
