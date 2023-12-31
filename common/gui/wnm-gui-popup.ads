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

private with WNM.GUI.Bitmap_Fonts;

package WNM.GUI.Popup is

   Text_Length : constant := 20;
   subtype Popup_Text is String (1 .. Text_Length);

   procedure Display (T : String; Duration : Time.Time_Microseconds);

   procedure Display_2L (Top, Bottom : String;
                         Duration : Time.Time_Microseconds);

   procedure Update;

private

   type Popup_State is (Disabled, Text_Popup_1L, Text_Popup_2L);

   State  : Popup_State := Disabled;
   T_Text   : Popup_Text := (others => ' ');
   B_Text   : Popup_Text := (others => ' ');

   subtype Tick_Count is Integer range 0 .. Text_Length * Bitmap_Fonts.Width;
   Remaining_Ticks : Tick_Count := 0;
   Tick_Period : Time.Time_Microseconds := 0;
   Next_Tick : Time.Time_Microseconds := 0;

end WNM.GUI.Popup;
