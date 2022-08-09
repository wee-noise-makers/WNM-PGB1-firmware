-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with WNM.Screen;
with WNM.GUI.Bitmap_Fonts;

with RP.GPIO;

with System.Machine_Code; use System.Machine_Code;

package body WNM.RP2040 is

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      X : Integer := 0;
   begin
      Screen.Clear;
      GUI.Bitmap_Fonts.Print (X, 0, "Fatal Error :(");
      Screen.Update;

      loop
         null;
      end loop;
   end Last_Chance_Handler;
begin
   RP.Clock.Initialize (XOSC_Frequency);
   RP.GPIO.Enable;
   RP.DMA.Enable;

   Asm ("cpsie i",
        Volatile => True);
end WNM.RP2040;
