-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2023 Fabien Chouteau                  --
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
with WNM.Screen;

package body WNM.GUI.Menu.Track_Mixer is

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
   procedure Draw (This : in out Instance) is
   begin
      Draw_Menu_Box
        ((case This.Setting is
            when Project.Volume => "Volume",
            when Project.Pan    => "Pan"),

         Count => Mixer_Settings_Count,
         Index => Mixer_Settings'Pos (This.Setting) -
             Mixer_Settings'Pos (Mixer_Settings'First));

      declare
         Col_Spacing : constant := Box_Width / 9;
         Bar_Width : constant := Col_Spacing - 6;
         First_Line_X : constant := Box_Left + Col_Spacing;

         X : Natural := First_Line_X;
      begin
         for T in Mixer_Track loop
            Draw_Str (X - Font_Width, Value_Text_Y + 2,
                      (case T is
                          when 1 => "KD",
                          when 2 => "SD",
                          when 3 => "HH",
                          when 4 => "BS",
                          when 5 => "LD",
                          when 6 => "CH",
                          when 7 => "S1",
                          when 8 => "S2"));

            declare
               Val : constant Natural :=
                 (case This.Setting is
                     when Project.Volume =>
                       Natural (Project.Track_Volume (T)) / 4,

                     when Project.Pan =>
                       Natural (Project.Track_Pan (T)) / 4);

               Bar_Bot : constant := Value_Text_Y - 1;

               Bar_Top : constant Natural :=
                 Bar_Bot - Val;

               Bar_Top_Max : constant Natural :=
                 Bar_Bot - Natural (Audio_Volume'Last) / 4;

               Bar_Left : constant Natural := X - Bar_Width / 2;
               Bar_Right : constant Natural := Bar_Left + Bar_Width;
            begin
               Screen.Fill_Rect (((Bar_Left,
                                 Bar_Top),
                                 Bar_Width,
                                 Val));

               if T = This.Selected_T then
                  Screen.Draw_Line ((Bar_Left - 2, Bar_Bot),
                                    (Bar_Left - 2, Bar_Top_Max));
                  Screen.Draw_Line ((Bar_Right + 2, Bar_Bot),
                                    (Bar_Right + 2, Bar_Top_Max));
               end if;
            end;

            X := X + Col_Spacing;
         end loop;
      end;

      --  declare
      --     Line_Spacing : constant := 6;
      --     Select_Line_Spacing : constant := 3;
      --     First_Line_Y : constant := Box_Top - 2;
      --
      --     Y : Natural := First_Line_Y;
      --  begin
      --     for T in Mixer_Track loop
      --        if T = This.Selected_T then
      --
      --           Draw_Str (Box_Left, Y, "X");
      --           Screen.Draw_Line ((Box_Left + 10, Y),
      --                             (Box_Right, Y));
      --           Y := Y + Select_Line_Spacing;
      --        else
      --           Screen.Draw_Line ((Box_Left + 10, Y),
      --                             (Box_Right, Y));
      --           Y := Y + Line_Spacing;
      --        end if;
      --     end loop;
      --  end;
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
            if This.Selected_T = Mixer_Track'First then
               Prev (This.Setting);
               This.Selected_T := Mixer_Track'Last;
            else
               Prev (This.Selected_T);
            end if;
         when Right_Press =>
            if This.Selected_T = Mixer_Track'Last then
               Next (This.Setting);
               This.Selected_T := Mixer_Track'First;
            else
               Next (This.Selected_T);
            end if;
         when Up_Press =>
            Project.Next_Value_Fast (This.Setting, This.Selected_T);
         when Down_Press =>
            Project.Prev_Value_Fast (This.Setting, This.Selected_T);
         when Slider_Touch  =>
            Project.Set (This.Setting, Event.Slider_Value,
                         This.Selected_T);

         when B_Press =>
            Menu.Pop (Exit_Value => Failure);

         when others =>
            null;

      end case;
   end On_Event;

end WNM.GUI.Menu.Track_Mixer;
