-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2020 Fabien Chouteau                    --
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
with WNM.Project;          use WNM.Project;

package body WNM.GUI.Menu.Step_Settings is

   package Sub_Settings_Next is new Enum_Next (Sub_Settings,
                                               Wrap => False);
   use Sub_Settings_Next;

   Step_Settings_Singleton : aliased Step_Settings_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Step_Settings_Singleton'Access);
   end Push_Window;

   ------------
   -- To_Top --
   ------------

   function To_Top (S : Sub_Settings) return Top_Settings
   is (case S is
          when Project.Condition => Condition,
          when Project.Note => Note,
          when Project.Duration => Note,
          when Project.Velo => Note,
          when Project.Repeat => Repeat,
          when Project.Repeat_Rate => Repeat,
          when Project.CC_A => CC_Value,
          when Project.CC_B => CC_Value,
          when Project.CC_C => CC_Value,
          when Project.CC_D => CC_Value);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Step_Settings_Menu)
   is
      Track : constant Tracks := Editing_Track;
      Step : constant Sequencer_Steps := Editing_Step;
      Sub  : constant Sub_Settings := This.Current_Setting;
      Top_Setting : constant Top_Settings := To_Top (Sub);
   begin
      Draw_Menu_Box
        ("Step settings",
         Count => Top_Settings_Count,
         Index => Top_Settings'Pos (Top_Setting));

      case Top_Setting is
         when Condition =>
            Draw_Title ("Condition", "");
            Draw_Value (Project.Img (Project.Trigger));

         when Note =>
            case Sub is
               when Project.Note =>
                  Draw_Title (Project.Img (Project.Note_Mode), "(press A)");
               when Project.Duration =>
                  Draw_Title ("Duration", "");
               when Project.Velo =>
                  Draw_Title ("Velocity", "");
               when others =>
                  null;
                  --  raise Program_Error;
            end case;

            Draw_Value (Project.Note_Img (Step),
                        Selected => Sub = Project.Note);

            Draw_Duration (Project.Duration (Step),
                           Sub = Project.Duration);

            Draw_CC_Value (Id => D,
                           Value => Project.Velocity (Step),
                           Label =>  "VEL",
                           Selected => Sub = Velo);

         when Repeat =>
            case Sub is
               when Repeat =>
                  Draw_Title ("Repeat Count", "");
               when Project.Repeat_Rate =>
                  Draw_Title ("Repeat Rate", "");
               when others =>
                  null;
            end case;

            Draw_Value (Repeat (Step)'Img,
                        Selected => Sub = Repeat);

            Draw_Value_Left
              (Project.Img (Repeat_Rate (Step)),
               Selected => Sub = Project.Repeat_Rate);

         when CC_Value =>
            declare
               Selected : constant Project.CC_Id :=
                 (case Sub is
                     when CC_A => Project.A,
                     when CC_B => Project.B,
                     when CC_C => Project.C,
                     when others => Project.D);
            begin
               Draw_CC_Control_Page
                 (T => Track,
                  Selected => Selected,
                  Val_A => Project.CC_Value (Id => A),
                  Val_B => Project.CC_Value (Id => B),
                  Val_C => Project.CC_Value (Id => C),
                  Val_D => Project.CC_Value (Id => D),
                  Ena_A => CC_Enabled (Id => A),
                  Ena_B => CC_Enabled (Id => B),
                  Ena_C => CC_Enabled (Id => C),
                  Ena_D => CC_Enabled (Id => D));
            end;
      end case;
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Step_Settings_Menu;
      Event : Menu_Event)
   is
      Step : constant Sequencer_Steps := Editing_Step;
   begin
      case Event.Kind is
         when Left_Press =>
            Prev (This.Current_Setting);
         when Right_Press =>
            Next (This.Current_Setting);

         when Up_Press =>
            Next_Value (This.Current_Setting);
            --  Next_Value_Fast (This.Current_Setting);
         when Down_Press =>
            Prev_Value (This.Current_Setting);
            --  Prev_Value_Fast (This.Current_Setting);

         when A_Press =>
            case This.Current_Setting is
               when Note => Note_Mode_Next;

               when others => null;
            end case;

         when B_Press =>
            case This.Current_Setting is
               when CC_A => Project.CC_Toggle (Step, A);
               when CC_B => Project.CC_Toggle (Step, B);
               when CC_C => Project.CC_Toggle (Step, C);
               when CC_D => Project.CC_Toggle (Step, D);
               when others => null;
            end case;

         when Slider_Touch =>
            Project.Set (This.Current_Setting, Event.Slider_Value);
      end case;

   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding procedure On_Pushed
     (This  : in out Step_Settings_Menu)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding procedure On_Focus
     (This       : in out Step_Settings_Menu;
      Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Step_Settings;
