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

package body WNM.LEDs is

   Hue_To_RGB : constant array (Hue) of RGB_Rec :=
     (Red          => (255, 000, 000),
      Rose         => (255, 000, 128),
      Magenta      => (255, 000, 255),
      Violet       => (128, 000, 255),
      Blue         => (000, 000, 255),
      Azure        => (000, 128, 255),
      Cyan         => (000, 255, 255),
      Spring_Green => (000, 255, 128),
      Green        => (000, 255, 000),
      Chartreuse   => (128, 255, 000),
      Yellow       => (255, 255, 000));

   G_Color : RGB_Rec := (255, 255, 255);

   -------------
   -- Set_Hue --
   -------------

   procedure Set_Hue (H : Hue) is
   begin
      G_Color := Hue_To_RGB (H);
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
      Set (B, Hue_To_RGB (H));
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

   function Update return WNM.Time.Time_Microseconds is
   begin
      Update_LEDs;

      return 0;
   end Update;

end WNM.LEDs;
