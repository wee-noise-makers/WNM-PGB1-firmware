-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2022 Fabien Chouteau                    --
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

with WNM.GUI.Menu.Sample_Select;
with WNM.GUI.Menu.Sample_Trim;
with WNM.GUI.Menu.Text_Dialog;
with WNM.GUI.Menu.Yes_No_Dialog;
with WNM.GUI.Menu.Audio_Input_Select;
with WNM.GUI.Menu.Recording;
with WNM.GUI.Menu.Drawing;
with WNM.GUI.Bitmap_Fonts;

with WNM.Mixer;
with WNM.Sample_Recording;
with WNM.Screen;

with new_sample_icon;
with edit_sample_icon;

package body WNM.GUI.Menu.Sample_Edit is

   Edit_Sample_Singleton : aliased Edit_Sample_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Edit_Sample_Singleton'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Edit_Sample_Menu) is

      Spacing : constant := 15;
      X_New : constant Natural :=
        (Screen.Width - (2 * new_sample_icon.Data.W + Spacing)) / 2;

      X_Edit : constant Natural :=
        X_New + new_sample_icon.Data.W + Spacing;

      Y_Icons : constant := 18;
   begin
      Screen.Copy_Bitmap (new_sample_icon.Data,
                          X_New, Y_Icons,
                          Invert_Color => This.Mode = New_Sample);

      Screen.Copy_Bitmap (edit_sample_icon.Data,
                          X_Edit, Y_Icons,
                          Invert_Color => This.Mode = Edit_Sample);

      case This.Mode is
         when New_Sample =>
            Screen.Draw_Rect (((X_New - 1, Y_Icons - 1),
                              new_sample_icon.Data.W + 2,
                              new_sample_icon.Data.H + 2));
         when Edit_Sample =>
            Screen.Draw_Rect (((X_Edit - 1, Y_Icons - 1),
                              edit_sample_icon.Data.W + 2,
                              edit_sample_icon.Data.H + 2));
      end case;

      Drawing.Draw_Str ((Screen.Width - 10 * Bitmap_Fonts.Width) / 2,
                        Drawing.Box_Bottom - 8,
                        (case This.Mode is
                            when Edit_Sample => "Edit Sample",
                            when New_Sample  => "New Sample"));
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Edit_Sample_Menu;
                       Event :        Menu_Event)
   is
   begin
      case Event.Kind is
         when A_Press =>
            case This.Mode is

            when New_Sample =>
               This.State := Select_Input;
               Mixer.Enter_Sample_Rec_Mode (Mixer.Preview);
               Audio_Input_Select.Push_Window;

            when Edit_Sample =>
               This.State := Select_Sample;
               This.Sample_Entry := Sample_Index'First;
               Sample_Select.Push_Window ("Sample to edit");
            end case;

         when Left_Press | Right_Press | Up_Press | Down_Press =>
            Next (This.Mode);

         when others =>
            null;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Edit_Sample_Menu)
   is
   begin
      null;
   end On_Pushed;

   ------------
   -- On_Pop --
   ------------

   overriding
   procedure On_Pop (This : in out Edit_Sample_Menu) is
      pragma Unreferenced (This);
   begin
      Mixer.Enter_Sample_Rec_Mode (Mixer.None);
   end On_Pop;

   ---------------
   -- Exit_Edit --
   ---------------

   procedure Exit_Edit (This       : in out Edit_Sample_Menu;
                        Exit_Value :        Window_Exit_Value)
   is
      pragma Unreferenced (This);
   begin
      Menu.Pop (Exit_Value);
   end Exit_Edit;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus
     (This       : in out Edit_Sample_Menu;
      Exit_Value : Window_Exit_Value)
   is
      New_State : Edit_Sample_State;
   begin

      --  Transition to the new state
      case This.State is
         when Select_Mode =>
            New_State := Select_Mode;

         when Select_Input =>

            if Exit_Value = Success then
               New_State := Record_Sample;
            else
               Mixer.Enter_Sample_Rec_Mode (Mixer.None);
               New_State := Select_Mode;
            end if;

         when Record_Sample =>

            if Exit_Value = Success then
               New_State := Trim;
            else
               New_State := Select_Input;
            end if;

         when Select_Sample =>

            if Exit_Value = Success then
               Sample_Recording.Init_From_Sample
                 (Menu.Sample_Select.Selected);
               New_State := Trim;

            else
               Mixer.Enter_Sample_Rec_Mode (Mixer.None);
               New_State := Select_Mode;
            end if;

         when Trim =>

            if Exit_Value = Success then
               New_State := Enter_Name;
            else
               case This.Mode is
                  when New_Sample =>
                     New_State := Record_Sample;
                  when Edit_Sample =>
                     New_State := Select_Sample;
               end case;
            end if;

         when Enter_Name =>

            if Exit_Value = Success then
               New_State := Select_Index;
            else
               New_State := Trim;
            end if;

         when Select_Index =>
            if Exit_Value = Success then
               New_State := Confirm;
            else
               New_State := Enter_Name;
            end if;

         when Confirm =>

            if Exit_Value = Success then
               New_State := Select_Mode;

               Mixer.Enter_Sample_Rec_Mode (Mixer.Saving);

               Sample_Recording.Save_Sample
                 (Sample_Select.Selected,
                  Text_Dialog.Value);

            else
               New_State := Select_Index;
            end if;

      end case;

      This.State := New_State;

      --  Push the next window
      case New_State is
         when Select_Mode =>
            null; -- Stay on the current window

         when Select_Input =>
            Mixer.Enter_Sample_Rec_Mode (Mixer.Preview);
            Audio_Input_Select.Push_Window;

         when Record_Sample =>
            Mixer.Enter_Sample_Rec_Mode (Mixer.Rec);
            Recording.Push_Window;

         when Select_Sample =>
            Mixer.Enter_Sample_Rec_Mode (Mixer.Preview);
            Sample_Select.Push_Window ("Sample to edit");

         when Trim =>
            Mixer.Enter_Sample_Rec_Mode (Mixer.Play);
            Sample_Trim.Push_Window;

         when Confirm =>
            Yes_No_Dialog.Set_Title ("Save Sample?");
            Yes_No_Dialog.Push_Window;

         when Enter_Name =>
            Text_Dialog.Set_Title ("Sample name?");
            case This.Mode is
               when New_Sample =>
                  Text_Dialog.Push_Window;
               when Edit_Sample =>
                  Text_Dialog.Push_Window
                    (Sample_Library.Entry_Name (Sample_Select.Selected));
            end case;

         when Select_Index =>
            Sample_Select.Push_Window ("Sample location");
      end case;

   end On_Focus;

end WNM.GUI.Menu.Sample_Edit;
