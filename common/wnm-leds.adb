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

with WNM.Persistent;

package body WNM.LEDs is

   G_Color : RGB_Rec := (255, 255, 255);

   ------------
   -- Dimmed --
   ------------

   function Dimmed (H : Hue) return RGB_Rec is
      C : constant RGB_Rec := Hue_To_RGB (H);
   begin
      return
        (case WNM.Persistent.Data.LED_Brightness is
          when 5 => C,
          when 4 => (C.R / 2, C.G / 2, C.B / 2),
          when 3 => (C.R / 4, C.G / 4, C.B / 4),
          when 2 => (C.R / 8, C.G / 8, C.B / 8),
          when 1 => (C.R / 16, C.G / 16, C.B / 16));
   end Dimmed;

   -------------
   -- Set_Hue --
   -------------

   procedure Set_Hue (H : Hue) is
   begin
      G_Color := Dimmed (H);
   end Set_Hue;

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (B : LED) is
   begin
      Set (B, G_Color);
   end Turn_On;

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (B : LED; H : Hue) is
   begin
      Set (B, Dimmed (H));
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (B : LED) is
   begin
      Set (B, (0, 0, 0));
   end Turn_Off;

   ------------------
   -- Turn_Off_All --
   ------------------

   procedure Turn_Off_All is
   begin
      Clear_LEDs;
   end Turn_Off_All;

   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      Update_LEDs;
   end Update;

end WNM.LEDs;
