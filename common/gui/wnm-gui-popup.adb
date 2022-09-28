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
with WNM.Utils;

package body WNM.GUI.Popup is

   generic
      Lines : Positive;
   package Geometry is
      Text_Left : constant Natural :=
        (Screen.Width - (Text_Length * Bitmap_Fonts.Width)) / 2;

      Text_Spacing : constant Natural := Bitmap_Fonts.Height;
      Text_Top  : constant Natural :=
        (Screen.Height - (Text_Spacing * Lines)) / 2;

      Rect_Left   : constant Natural := Text_Left - 4;
      Rect_Right  : constant Natural := Screen.Width - Rect_Left;
      Rect_Top    : constant Natural := Text_Top - 4;
      Rect_Bottom : constant Natural := Screen.Height - Rect_Top;

      BG_Rect : constant Screen.Rect := ((Rect_Left, Rect_Top),
                                         Screen.Width - 2 * Rect_Left,
                                         Screen.Height - 2 * Rect_Top);

      procedure Draw_BG (Remaining : Tick_Count);

   end Geometry;

   package body Geometry is

      -------------
      -- Draw_BG --
      -------------

      procedure Draw_BG (Remaining : Tick_Count) is
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

         Screen.Draw_Line ((Rect_Left + 1, Rect_Bottom - 2),
                           (Rect_Left + Remaining, Rect_Bottom - 2));

      end Draw_BG;

   end Geometry;

   package Geom_1L is new Geometry (Lines => 1);
   package Geom_2L is new Geometry (Lines => 2);

   -------------
   -- Display --
   -------------

   procedure Display (T : String; Duration : Time.Time_Microseconds) is
   begin
      WNM.Utils.Copy_Str (T, T_Text);

      Remaining_Ticks := Tick_Count'Last;
      Tick_Period := Duration / Time.Time_Microseconds (Tick_Count'Last);
      Next_Tick := Time.Clock + Tick_Period;

      State := Text_Popup_1L;
   end Display;

   ----------------
   -- Display_2L --
   ----------------

   procedure Display_2L (Top, Bottom : String;
                         Duration    : Time.Time_Microseconds)
   is
   begin
      WNM.Utils.Copy_Str (Top, T_Text);
      WNM.Utils.Copy_Str (Bottom, B_Text);

      Remaining_Ticks := Tick_Count'Last;
      Tick_Period := Duration / Time.Time_Microseconds (Tick_Count'Last);
      Next_Tick := Time.Clock + Tick_Period;

      State := Text_Popup_2L;
   end Display_2L;

   ------------
   -- Update --
   ------------

   procedure Update is

      B : Integer := 1;
      Now : constant Time.Time_Microseconds := Time.Clock;
   begin
      if State = Disabled then
         return;
      end if;

      while Now > Next_Tick loop
         if Remaining_Ticks > 0 then
            Next_Tick := Next_Tick + Tick_Period;
            Remaining_Ticks := Remaining_Ticks - 1;
         else
            State := Disabled;
            exit;
         end if;
      end loop;

      case State is
         when Disabled =>
            return;

         when Text_Popup_1L =>
            Geom_1L.Draw_BG (Remaining_Ticks);

            B := Geom_1L.Text_Left;
            Print (X_Offset    => B,
                   Y_Offset    => Geom_1L.Text_Top,
                   Str         => T_Text);

         when Text_Popup_2L =>
            Geom_2L.Draw_BG (Remaining_Ticks);

            B := Geom_2L.Text_Left;
            Print (X_Offset    => B,
                   Y_Offset    => Geom_2L.Text_Top,
                   Str         => T_Text);

            B := Geom_1L.Text_Left;
            Print (X_Offset    => B,
                   Y_Offset    =>
                      Geom_2L.Text_Top + Geom_2L.Text_Spacing,
                   Str         => B_Text);
      end case;

   end Update;

end WNM.GUI.Popup;
