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

package WNM.LEDs is

   type Hue is (Red, Rose, Magenta, Violet, Blue, Azure, Cyan, Spring_Green,
                Green, Chartreuse, Yellow, Orange);

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
      Yellow       => (255, 255, 000),
      Orange       => (255, 128, 000));

   Play        : constant Hue := Green;
   Recording   : constant Hue := Red;
   Step        : constant Hue := Magenta;
   Track       : constant Hue := Azure;
   Pattern     : constant Hue := Blue;
   Chord       : constant Hue := Orange;
   Part        : constant Hue := Chartreuse;
   FX          : constant Hue := Violet;

   procedure Set_Hue (H : Hue);

   procedure Turn_On (B : LED) with Inline_Always;
   procedure Turn_On (B : LED; H : Hue) with Inline_Always;
   procedure Turn_Off (B : LED) with Inline_Always;
   procedure Turn_Off_All with Inline_Always;

   procedure Update;

end WNM.LEDs;
