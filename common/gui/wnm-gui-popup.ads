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

with WNM.Time;

package WNM.GUI.Popup is

   Text_Length : constant := 16;
   subtype Popup_Text is String (1 .. Text_Length);

   procedure Display (T : Popup_Text; Duration : Time.Time_Microseconds);

   procedure Update;

private

   type Popup_State is (Disabled, Text_Popup);

   State  : Popup_State := Disabled;
   Text   : Popup_Text := (others => ' ');
   Expire : Time.Time_Microseconds := 0;

end WNM.GUI.Popup;
