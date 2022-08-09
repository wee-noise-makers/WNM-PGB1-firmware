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

with WNM_Sim;

with Sf.Graphics.Color;

package body WNM.LED is

   LED_Brightness : array (LEDs) of UInt8 := (others => 0);
   Next_Start : WNM.Time.Time_Microseconds :=
     WNM.Time.Clock + LED_Task_Period_Microseconds;

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (B : LEDs) is
   begin
      LED_Brightness (B) := LED_Brightness (B) + 1;
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (B : LEDs) is
   begin
      LED_Brightness (B) := 0;
   end Turn_Off;

   ------------------
   -- Turn_Off_All --
   ------------------

   procedure Turn_Off_All is
   begin
      LED_Brightness := (others => 0);
   end Turn_Off_All;

   ----------------
   -- Brightness --
   ----------------

   function Brightness (B : LEDs) return UInt8
   is (LED_Brightness (B));

   ------------
   -- Update --
   ------------

   function Update return WNM.Time.Time_Microseconds is
   begin
      if WNM.Time.Clock >= Next_Start then
         Next_Start := Next_Start + LED_Task_Period_Microseconds;
      end if;

      for L in LEDs loop
         if LED_Brightness (L) /= 0 then
            case L is
               when B1 .. B8 | B9 .. B16 =>
                  WNM_Sim.SFML_LEDs (L) := Sf.Graphics.Color.sfBlue;
               when Rec =>
                  WNM_Sim.SFML_LEDs (L) := Sf.Graphics.Color.sfRed;
               when Play =>
                  WNM_Sim.SFML_LEDs (L) := Sf.Graphics.Color.sfGreen;
            end case;
         else
            WNM_Sim.SFML_LEDs (L) := Sf.Graphics.Color.sfTransparent;
         end if;
      end loop;

      return Next_Start;
   end Update;

end WNM.LED;
