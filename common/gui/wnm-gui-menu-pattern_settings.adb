-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2022 Fabien Chouteau                  --
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

with WNM.Project; use WNM.Project;
with WNM.Project.Step_Sequencer;
with WNM.Screen;

with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.GUI.Update;

with go_next_icon;
with go_back_icon;
with loop_icon;

package body WNM.GUI.Menu.Pattern_Settings is

   package Sub_Settings_Next is new Enum_Next (Sub_Settings,
                                               Wrap => False);
   use Sub_Settings_Next;

   Pattern_Menu_Singleton : aliased Pattern_Settings_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Pattern_Menu_Singleton'Access);
   end Push_Window;

   ------------
   -- To_Top --
   ------------

   function To_Top (S : Sub_Settings) return Top_Settings
   is (case S is
          when Length   => Only_This,
          when Has_Link => Only_This);

   ---------------
   -- Draw_Icon --
   ---------------

   procedure Draw_Icon (P        : Patterns;
                        Selected : Boolean;
                        Playing  : Boolean;
                        Link     : Boolean)
   is
      Rect_Size : constant := 6;
      Space : constant := 4;
      Top : constant := Drawing.Box_Top + Space;
      Left : constant := Drawing.Box_Right - 8 * (Rect_Size + Space);

      X : constant Integer := Left +
        (if P < 9
         then (Integer (P) - 1)
         else (Integer (P) - 9)) * (Rect_Size + Space);

      Y : constant Integer := Top +
        (if P < 9 then 0 else Rect_Size + Space);
   begin

      if Playing and then Update.Anim_Step mod 14 > 7 then
         Screen.Fill_Rect (((X, Y), Rect_Size, Rect_Size));
      else
         Screen.Draw_Rect (((X, Y), Rect_Size, Rect_Size));
      end if;

      if Selected then
         Screen.Draw_Rect (((X - 1, Y - 1), Rect_Size + 2, Rect_Size + 2));
      end if;

      if Link then
         Screen.Fill_Rect (((X + Rect_Size, Y + 1),
                           Space, Rect_Size - 2));
      end if;
   end Draw_Icon;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Pattern_Settings_Menu)
   is
      Sub : constant Sub_Settings := This.Current_Setting;
      Top : constant Top_Settings := To_Top (Sub);
      T  : constant Tracks := Project.Editing_Track;
      EP : constant Patterns := Project.Editing_Pattern;
      PP : constant Patterns := Project.Step_Sequencer.Playing_Step (T).P;
   begin
      Draw_Menu_Box ("Pattern settings",
                     Count => Top_Settings_Count,
                     Index => Top_Settings'Pos (Top));

      case Top is
         when Only_This =>
            case Sub is
               when Length =>
                  Draw_Title ("Length", "");
               when Has_Link =>
                  Draw_Title ("Link", "");
            end case;

            Draw_Pattern_Length (6,
                                 Project.Pattern_Length (T, EP),
                                 Selected => Sub = Length);

            Screen.Copy_Bitmap
              ((case Project.Link (T, EP) is
                  when True => go_next_icon.Data,
                  when False => (if EP = 1
                                  or else
                                    not Project.Link (T, EP - 1)
                                 then loop_icon.Data
                                 else go_back_icon.Data)),

               Box_Left + 70, Value_Text_Y - 2);

            if Sub = Has_Link then
               Screen.Draw_Line ((Box_Left + 70, Select_Line_Y),
                           (Box_Left + 81, Select_Line_Y));
            end if;
      end case;

      for P in Patterns loop
         Draw_Icon (P, P = EP, P = PP, Project.Link (T, P));
      end loop;

   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Pattern_Settings_Menu;
                       Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            Prev (This.Current_Setting);
         when Right_Press =>
            Next (This.Current_Setting);
         when Up_Press =>
            Project.Next_Value (This.Current_Setting);
         when Down_Press =>
            Project.Prev_Value (This.Current_Setting);
         when A_Press =>
            null;
         when B_Press =>
            null;
         when Slider_Touch =>
            Project.Set (This.Current_Setting, Event.Slider_Value);
      end case;

   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed (This  : in out Pattern_Settings_Menu)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus (This       : in out Pattern_Settings_Menu;
                       Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Pattern_Settings;
