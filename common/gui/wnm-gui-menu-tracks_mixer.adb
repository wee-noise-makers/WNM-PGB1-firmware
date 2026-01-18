-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2026 Fabien Chouteau                  --
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

with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Screen; use WNM.Screen;
with WNM.Project; use WNM.Project;

package body WNM.GUI.Menu.Tracks_Mixer is

   Singleton : aliased Instance;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Singleton'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This   : in out Instance) is
      Top : constant Top_Settings := This.Current_Setting;

      Top_Text_Y : constant := Box_Top + 2;
      Bot_Text_Y : constant := Box_Bottom - 1 - Font_Height * 1;

      Col_Top : constant := Top_Text_Y + Font_Height + 2;
      Col_Bot : constant := Bot_Text_Y - 2;

      Col_Width : constant := Screen_Width / 8;
   begin
      for Id in Track_Id loop
         declare
            X_Col_Center : constant Natural :=
              (Natural (Id) - 1) * Col_Width + Col_Width / 2;
         begin

            Draw_Line ((X_Col_Center - 2, Col_Top),
                       (X_Col_Center - 2, Col_Bot));
            Draw_Line ((X_Col_Center + 2, Col_Top),
                       (X_Col_Center + 2, Col_Bot));

            Draw_Str (X_Col_Center - Font_Width,
                      Top_Text_Y,
                      (case Id is
                          when 1 => "KD",
                          when 2 => "SD",
                          when 3 => "HH",
                          when 4 => "BA",
                          when 5 => "LD",
                          when 6 => "CH",
                          when 7 => "S1",
                          when 8 => "S2"),
                     Underline => This.Selected_Track = Id);
            Draw_Str (X_Col_Center - Font_Width,
                      Bot_Text_Y,
                      (case Top is
                          when Volume    => Project.Track_Volume (Id)'Img,
                          when Pan       => Project.Track_Pan (Id)'Img,
                          when Master_FX => Project.Master_FX (Id)'Img),
                     Underline => This.Selected_Track = Id);
         end;
      end loop;

      Draw_Menu_Box (Img (Top),
                     Count => Top_Settings_Count,
                     Index => Top_Settings'Pos (Top) -
                       Top_Settings'Pos (Top_Settings'First));

   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Instance;
                       Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            if This.Selected_Track = Track_Id'First
              and then
                This.Current_Setting /= Top_Settings'First
            then
               This.Selected_Track := Track_Id'Last;
               Prev (This.Current_Setting);
            else
               Prev (This.Selected_Track);
            end if;

         when Right_Press =>
            if This.Selected_Track = Track_Id'Last
              and then
                This.Current_Setting /= Top_Settings'Last
            then
               This.Selected_Track := Track_Id'First;
               Next (This.Current_Setting);
            else
               Next (This.Selected_Track);
            end if;

         when Up_Press =>
            Project.Next_Value (This.Selected_Track, This.Current_Setting);

         when Down_Press =>
            Project.Prev_Value (This.Selected_Track, This.Current_Setting);

         when A_Press =>
            Next (This.Current_Setting);

         when B_Press =>
            Menu.Pop (Exit_Value => Success);

         when Slider_Touch =>
            Project.Set (This.Selected_Track,
                         This.Current_Setting,
                         Event.Slider_Value);

      end case;
   end On_Event;

end WNM.GUI.Menu.Tracks_Mixer;
