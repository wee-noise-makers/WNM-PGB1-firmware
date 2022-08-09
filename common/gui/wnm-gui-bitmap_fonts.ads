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

with HAL.Bitmap;

with WNM.Screen;

package WNM.GUI.Bitmap_Fonts is

   Width : constant := 6;
   Height : constant := 8;

   procedure Print (X_Offset    : in out Integer;
                    Y_Offset    : Integer;
                    C           : Character;
                    Invert_From : Integer := WNM.Screen.Width;
                    Invert_To   : Integer := WNM.Screen.Width)
   with Pre => Invert_From <= Invert_To;

   procedure Print (X_Offset    : in out Integer;
                    Y_Offset    : Integer;
                    Str         : String;
                    Invert_From : Integer := WNM.Screen.Width;
                    Invert_To   : Integer := WNM.Screen.Width)
   with Pre => Invert_From <= Invert_To;
end WNM.GUI.Bitmap_Fonts;
