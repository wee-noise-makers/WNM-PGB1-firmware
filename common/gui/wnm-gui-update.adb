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

with HAL;                   use HAL;
with WNM.GUI.Bitmap_Fonts;  use WNM.GUI.Bitmap_Fonts;
with WNM.GUI.Parameters;
with WNM.Screen;
with WNM.UI;
with WNM.GUI.Menu;
with WNM.GUI.Menu.Drawing;  use WNM.GUI.Menu.Drawing;
with WNM.GUI.Logo;
with WNM.GUI.Popup;
with WNM.Project;
with WNM.Audio_Routing;
with WNM.Time;

package body WNM.GUI.Update is

   ------------
   -- Update --
   ------------

   procedure Update is
      B : Integer;
   begin

      WNM.Screen.Clear;

      --  Splash screen
      if WNM.Time.Clock < WNM.Time.Milliseconds (1_000) then
         WNM.GUI.Logo.Draw_On_Screen (UInt2 (Anim_Step mod 4));
         WNM.Screen.Update;
         Anim_Step := Anim_Step + 1;
         return;
      end if;

      -- Header --
      Menu.Drawing.Draw_Battery (Anim_Step);
      Screen.Draw_H_Line (8);

      case WNM.UI.Input_GUI_Mode is
         when WNM.UI.Volume_BPM_Mute =>

            WNM.GUI.Parameters.Print_Percentage
              (Slot  => WNM.GUI.Parameters.Up,
               Name  => "Volume",
               Value => Integer (WNM.Audio_Routing.Get_Main_Volume));

            WNM.GUI.Parameters.Print_Int
              (Slot  => WNM.GUI.Parameters.Down,
               Name  => "BPM",
               Value => Integer (WNM.Project.Get_BPM),
               Min   => Integer (Beat_Per_Minute'First),
               Max   => Integer (Beat_Per_Minute'Last));

            case WNM.UI.Input_GUI_Mode is
               when WNM.UI.Volume_BPM_Mute =>
                  WNM.GUI.Menu.Drawing.Draw_Value ("Mute");
               when others =>
                  null;
            end case;

         when WNM.UI.Main_Modes =>

            Menu.Draw;

         when WNM.UI.FX_Alt =>
            B :=
              Box_Left + (Box_Right - Box_Left - 7 * Bitmap_Fonts.Width) / 2;

            Print (X_Offset => B,
                   Y_Offset => Box_Top,
                   Str      => "FX/Copy");

            Draw_Alt_Slider;

         when WNM.UI.Track_Select =>
            Draw_Title ("Track Select", "");
            Draw_Value (Project.Img (Project.Mode (Project.Editing_Track)));

      end case;

      WNM.GUI.Popup.Update;

      WNM.Screen.Update;

      Anim_Step := Anim_Step + 1;
   end Update;

end WNM.GUI.Update;
