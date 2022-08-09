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

with HAL; use HAL;

with WNM.GUI.Bitmap_Fonts; use WNM.GUI.Bitmap_Fonts;
with WNM.Screen;

package body WNM.GUI.Popup is

   Text_Left : constant := (Screen.Width - (Text_Length * Bitmap_Fonts.Width)) / 2;
   Text_Top  : constant := (Screen.Height - Bitmap_Fonts.Height) / 2;

   Rect_Left   : constant := Text_Left - 4;
   Rect_Right  : constant := Screen.Width - Rect_Left;
   Rect_Top    : constant := Text_Top - 4;
   Rect_Bottom : constant := Screen.Height - Rect_Top;

   BG_Rect : constant Screen.Rect := ((Rect_Left, Rect_Top),
                                      Screen.Width - 2 * Rect_Left,
                                      Screen.Height - 2 * Rect_Top);

   -------------
   -- Display --
   -------------

   procedure Display (T : Popup_Text; Duration : Time.Time_Microseconds) is
   begin
      Text := T;
      Expire := Time.Clock + Duration;
      State := Text_Popup;
   end Display;

   ------------
   -- Update --
   ------------

   procedure Update is

      -------------
      -- Draw_BG --
      -------------

      procedure Draw_BG is
      begin
         Screen.Fill_Rect (BG_Rect, False);

         --  Top
         Screen.Draw_Line ((Rect_Left + 1, Rect_Top + 1),
                           (Rect_Right - 1, Rect_Top + 1));

         --  Right
         Screen.Draw_Line ((Rect_Right - 1, Rect_Top + 1),
                           (Rect_Right - 1, Rect_Bottom - 1));

         --  Bottom
         Screen.Draw_Line ((Rect_Left + 1, Rect_Bottom - 1),
                           (Rect_Right - 1, Rect_Bottom - 1));

         --  Left
         Screen.Draw_Line ((Rect_Left + 1, Rect_Top + 1),
                           (Rect_Left + 1, Rect_Bottom - 1));
      end Draw_BG;
   begin
      case State is
         when Disabled =>
            --  Nothing to do
            return;
         when Text_Popup =>
            if Expire < Time.Clock then
               State := Disabled;
            else
               --  Draw the text popup
               declare
                  B : Integer := 1;
               begin
                  Draw_BG;
                  B := Text_Left;
                  Print (X_Offset    => B,
                         Y_Offset    => Text_Top,
                         Str         => Text);
               end;
            end if;
      end case;
   end Update;

end WNM.GUI.Popup;
