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

package WNM.GUI.Parameters is

   type Parameter_Slot is (Up, Down);

   procedure Print_Int (Slot  : Parameter_Slot;
                        Name  : String;
                        Value : Integer;
                        Min   : Integer;
                        Max   : Integer);

   subtype Percentage is Natural range 0 .. 100;

   procedure Print_Percentage (Slot  : Parameter_Slot;
                               Name  : String;
                               Value : Natural);

   subtype Pan is Integer range -100 .. 100;

   procedure Print_Pan (Slot  : Parameter_Slot;
                        Name  : String;
                        Value : Pan);

end WNM.GUI.Parameters;
