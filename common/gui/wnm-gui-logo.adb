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

with WNM.Screen;      use WNM.Screen;
with wnm_logo_text;
with wnm_logo_wave_1;
with wnm_logo_wave_2;
with wnm_logo_wave_3;
with wnm_logo_wave_4;

package body WNM.GUI.Logo is

   Left : constant Positive := (Screen.Width - wnm_logo_text.Data.W) / 2;
   Top  : constant Positive := (Screen.Height
                                - wnm_logo_text.Data.H
                                - wnm_logo_wave_1.Data.H) / 2;

   --------------------
   -- Draw_On_Screen --
   --------------------

   procedure Draw_On_Screen (Anim_Step : HAL.UInt2) is
   begin
      WNM.Screen.Clear;
      Copy_Bitmap (wnm_logo_text.Data, Left, Top);

      WNM.Screen.Draw_Line (Start     => (Left, Top + 9),
                            Stop      => (Left + 28, Top + 9));
      WNM.Screen.Draw_Line (Start     => (Left + 57, Top + 9),
                            Stop      => (Left + 83, Top + 9));

      Copy_Bitmap ((case Anim_Step is
                      when 0 => wnm_logo_wave_1.Data,
                      when 1 => wnm_logo_wave_2.Data,
                      when 2 => wnm_logo_wave_3.Data,
                      when 3 => wnm_logo_wave_4.Data),
                   Left + 27, Top + 7);
   end Draw_On_Screen;

end WNM.GUI.Logo;
