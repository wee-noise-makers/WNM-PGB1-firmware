-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                    Copyright (C) 2022 Fabien Chouteau                     --
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

with WNM.GUI.Menu.Project_Select;
with WNM.GUI.Menu.Text_Dialog;
with WNM.GUI.Menu.Yes_No_Dialog;

with WNM.GUI.Popup;

with WNM.Project.Storage;

package body WNM.GUI.Menu.Save_Project is

   Singleton : aliased Instance;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Singleton'Access);
   end Push_Window;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Instance)
   is
   begin
      This.State := Select_Project;
      This.Prj_Entry := Invalid_Prj_Entry;
      Project_Select.Push_Window;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus
     (This       : in out Instance;
      Exit_Value : Window_Exit_Value)
   is
      New_State : Window_State;

      -----------------
      -- Save_Result --
      -----------------

      procedure Save_Result (R : Project.Storage.Storage_Error) is
         use Project.Storage;
      begin
         if R /= Ok then
            GUI.Popup.Display_2L ("Can't Save Project",
                                  Project.Storage.Img (R),
                                  1_500_000);
         else
            GUI.Popup.Display ("Project Saved",
                               500_000);
         end if;
      end Save_Result;
   begin

      --  Transition to the new state
      case This.State is
         when Select_Project =>

            if Exit_Value = Success then

               if Has_Project (Project_Select.Selected) then
                  New_State := Confirm_Overwrite;
               else
                  New_State := Enter_Name;
               end if;

            else
               Menu.Pop (Exit_Value);
               return;
            end if;

         when Enter_Name =>
            if Exit_Value = Success then
               Save_Result (Save_Project_With_Name
                            (Project_Select.Selected,
                               Text_Dialog.Value));

               Menu.Pop (Exit_Value);
               return;
            else
               New_State := Select_Project;
            end if;

         when Confirm_Overwrite =>
            if Exit_Value = Success then
               Save_Result
                 (Project.Library.Save_Project (Project_Select.Selected));
               Menu.Pop (Exit_Value);
               return;
            else
               New_State := Select_Project;
            end if;
      end case;

      This.State := New_State;

      --  Push the next window
      case New_State is
         when Select_Project =>
            Project_Select.Push_Window;
         when Enter_Name =>
            Text_Dialog.Set_Title ("Project name?");
            Text_Dialog.Push_Window;
         when Confirm_Overwrite =>
            Yes_No_Dialog.Set_Title ("Replace project?");
            Yes_No_Dialog.Push_Window;
      end case;

   end On_Focus;

end WNM.GUI.Menu.Save_Project;
