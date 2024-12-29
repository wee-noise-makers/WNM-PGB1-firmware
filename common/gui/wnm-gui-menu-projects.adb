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

with HAL; use HAL;

with WNM.GUI.Menu.Drawing;           use WNM.GUI.Menu.Drawing;
with WNM.GUI.Menu.Yes_No_Dialog;
with WNM.GUI.Menu.Text_Dialog;
with WNM.GUI.Popup;
with WNM.GUI.Menu.Project_Select;
with WNM.GUI.Bitmap_Fonts;

with WNM.Project.Library;
with WNM.File_System;
with WNM.Utils;
with WNM.Screen;

with edit_project_icon;
with edit_project_icon_2;
with edit_project_icon_3;
with load_project_icon;
with load_project_icon_2;
with load_project_icon_3;
with save_project_icon;
with save_project_icon_2;
with save_project_icon_3;
with delete_project_icon;
with delete_project_icon_2;
with clear_project_icon;
with clear_project_icon_2;
with clear_project_icon_3;

package body WNM.GUI.Menu.Projects is

   Singleton : aliased Instance;

   function Menu_Item_Text (Item : Menu_Items) return String
   is (case Item is
          when Save_Project   => "Save",
          when Load_Project   => "Load",
          when Rename_Project => "Rename",
          when Delete_Project => "Delete",
          when Clear_Project  => "Clear");

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

   Anim_Step : HAL.UInt32 := 0;

   overriding
   procedure Draw
     (This   : in out Instance)
   is
      Step : constant HAL.UInt32 := Anim_Step / 6;
   begin
      Draw_Menu_Box ("Menu",
                     Count => Menu_Items_Count,
                     Index => Menu_Items'Pos (This.Item));

      Screen.Copy_Bitmap
        ((case This.Item is
            when Load_Project =>
           (case Step  mod 3 is
               when 0      => load_project_icon.Data,
               when 1      => load_project_icon_2.Data,
               when others => load_project_icon_3.Data),

            when Save_Project =>
           (case Step mod 3 is
               when 0      => save_project_icon.Data,
               when 1      => save_project_icon_2.Data,
               when others => save_project_icon_3.Data),

            when Rename_Project =>
           (case Step mod 3 is
               when 0 => edit_project_icon.Data,
               when 1 => edit_project_icon_2.Data,
               when 2 => edit_project_icon_3.Data,
               when others => edit_project_icon_2.Data),

            when Delete_Project =>
           (case Step mod 8 is
               when 0 .. 3      => delete_project_icon.Data,
               when others => delete_project_icon_2.Data),

            when Clear_Project =>
           (case Step mod 4 is
               when 0      => clear_project_icon.Data,
               when 1      => clear_project_icon_2.Data,
               when 2      => clear_project_icon_3.Data,
               when others => clear_project_icon_2.Data)),
         Box_Center.X - (load_project_icon.Data.W / 2),
         Box_Top + 2);

      Anim_Step := Anim_Step + 1;

      Draw_Str_Center (Box_Bottom - Bitmap_Fonts.Height - 1,
                       Menu_Item_Text (This.Item));
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Instance;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when A_Press =>
            case This.Item is
               when Save_Project | Load_Project |
                    Rename_Project | Delete_Project =>

                  Menu.Project_Select.Push_Window;
                  This.State := Select_Project;

               when Clear_Project =>
                  Yes_No_Dialog.Set_Title ("Clear current?");
                  Yes_No_Dialog.Push_Window;
                  This.State := Confirm;
            end case;

         when B_Press =>
            Menu.Pop (Exit_Value => Success);

         when Up_Press =>
            null;
         when Down_Press =>
            null;

         when Right_Press =>
            if This.Item /= Menu_Items'Last then
               This.Item := Menu_Items'Succ (This.Item);
            else
               This.Item := Menu_Items'First;
            end if;

         when Left_Press =>
            if This.Item /= Menu_Items'First then
               This.Item := Menu_Items'Pred (This.Item);
            else
               This.Item := Menu_Items'Last;
            end if;
         when Slider_Touch =>
            null;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Instance)
   is
      --  pragma Unreferenced (This);
   begin
      This.Item := Menu_Items'First;
      --  Menu.System_Info.Push_Window;
   end On_Pushed;

   -------------
   -- Do_Load --
   -------------

   procedure Do_Load (P : Project.Library.Prj_Index) is
      use WNM.File_System;
      Err : File_System.Storage_Error;
   begin
      Err := Project.Library.Load_Project (P);

      if Err /= Ok then
         GUI.Popup.Display_2L ("Can't Load Project:",
                               File_System.Img (Err),
                               2_500_000);
      else
         GUI.Popup.Display ("Project Loaded",
                            500_000);
      end if;
   end Do_Load;

   -------------
   -- Do_Save --
   -------------

   procedure Do_Save (P : Project.Library.Prj_Index; Name : String) is
      use WNM.File_System;
      Err : File_System.Storage_Error;
   begin
      Err := Project.Library.Save_Project_With_Name (P, Name);

      if Err /= Ok then
         GUI.Popup.Display_2L ("Can't Save Project",
                               File_System.Img (Err),
                               1_500_000);
      else
         GUI.Popup.Display ("Project Saved",
                            500_000);
      end if;
   end Do_Save;

   ---------------
   -- Do_Rename --
   ---------------

   procedure Do_Rename (P : Project.Library.Prj_Index; Name : String) is
      use WNM.File_System;
      Err : File_System.Storage_Error;
   begin
      Err := Project.Library.Rename_Project (P, Name);

      if Err /= Ok then
         GUI.Popup.Display_2L ("Can't Rename Project",
                               File_System.Img (Err),
                               1_500_000);
      else
         GUI.Popup.Display ("Project Renamed",
                            500_000);
      end if;
   end Do_Rename;

   ---------------
   -- Do_Delete --
   ---------------

   procedure Do_Delete (P : Project.Library.Prj_Index) is
      use WNM.File_System;
      Err : File_System.Storage_Error;
   begin
      Err := Project.Library.Delete_Project (P);

      if Err /= Ok then
         GUI.Popup.Display_2L ("Can't Delete Project:",
                               File_System.Img (Err),
                               2_500_000);
      else
         GUI.Popup.Display ("Project Deleted",
                            500_000);
      end if;
   end Do_Delete;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus
     (This       : in out Instance;
      Exit_Value : Window_Exit_Value)
   is
      New_State : Window_State;

   begin

      --  Transition to the new state
      case This.State is
         when Idle =>
            New_State := Idle;

         when Select_Project =>

            if Exit_Value = Success then

               case This.Item is
                  when Save_Project =>

                     if Project.Library.Has_Project (Project_Select.Selected)
                     then
                        New_State := Confirm;
                     else
                        New_State := Enter_Name;
                     end if;

                  when Load_Project =>
                     --  Load only if project exists
                     if Project.Library.Has_Project (Project_Select.Selected)
                     then
                        Do_Load (Project_Select.Selected);
                     end if;

                     New_State := Idle;
                     Menu.Exit_Menu;

                  when Delete_Project =>

                     --  Delete only if project exists
                     if Project.Library.Has_Project (Project_Select.Selected)
                     then
                        New_State := Confirm;
                     else
                        --  Cannot delete a project that doesn't exist
                        New_State := Idle;
                     end if;

                  when Rename_Project =>

                     --  Rename only if project exists
                     if Project.Library.Has_Project (Project_Select.Selected)
                     then
                        New_State := Enter_Name;
                     else
                        --  Cannot delete a project that doesn't exist
                        New_State := Idle;
                     end if;

                  when Clear_Project =>
                     New_State := Idle;

               end case;
            else
               New_State := Idle;
            end if;

         when Enter_Name =>
            if Exit_Value = Success then
               case This.Item is
                  when Save_Project =>
                     Do_Save (Project_Select.Selected,
                              Text_Dialog.Value);
                     New_State := Idle;
                     Menu.Exit_Menu;

                  when Rename_Project =>
                     New_State := Confirm;
                  when others =>
                     raise Program_Error;
               end case;
            else
               New_State := Idle;
            end if;

         when Confirm =>
            if Exit_Value = Success then
               case This.Item is
                  when Save_Project =>
                     Do_Save (Project_Select.Selected,
                              Project.Library.Entry_Name
                                (Project_Select.Selected));
                     New_State := Idle;
                     Menu.Exit_Menu;

                  when Rename_Project =>
                     Do_Rename (Project_Select.Selected,
                                Text_Dialog.Value);
                     New_State := Idle;

                  when Delete_Project =>
                     Do_Delete (Project_Select.Selected);
                     New_State := Idle;

                  when Clear_Project =>
                     Project.Clear;
                     New_State := Idle;
                     Menu.Exit_Menu;

                  when others =>
                     raise Program_Error;
               end case;
            else
               New_State := Idle;
            end if;
      end case;

      This.State := New_State;

      --  Push the next window
      case New_State is
         when Idle =>
            null;
         when Select_Project =>
            Project_Select.Push_Window;
         when Enter_Name =>
            Text_Dialog.Set_Title ("Project name?");
            case This.Item is
               when Rename_Project =>
                  Text_Dialog.Push_Window
                    (Utils.Trim (Project.Library.Entry_Name
                     (Project_Select.Selected)));
               when others =>
                  Text_Dialog.Push_Window;
            end case;
         when Confirm =>
            case This.Item is
               when Save_Project =>
                  Yes_No_Dialog.Set_Title ("Replace project?");
               when Rename_Project =>
                  Yes_No_Dialog.Set_Title ("Rename project?");
               when Delete_Project =>
                  Yes_No_Dialog.Set_Title ("Delete project?");
               when others =>
                  raise Program_Error;
            end case;
            Yes_No_Dialog.Push_Window;
      end case;
   end On_Focus;

end WNM.GUI.Menu.Projects;
