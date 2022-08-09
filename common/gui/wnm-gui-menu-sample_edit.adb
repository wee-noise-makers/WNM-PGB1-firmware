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
with WNM.GUI.Menu.Recording;
with WNM.GUI.Menu.Sample_Trim;
with WNM.GUI.Menu.Text_Dialog;
with WNM.GUI.Menu.Assign_To_Track;
with WNM.GUI.Menu.Yes_No_Dialog;
with WNM.Sample_Stream;
with WNM.Synth;

with WNM.Sample_Edit;

package body WNM.GUI.Menu.Sample_Edit is

   Edit_Sample_Singleton : aliased Edit_Sample_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Edit_Sample_Singleton'Access);
   end Push_Window;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Edit_Sample_Menu)
   is
   begin
      This.State := Select_Sample;
      This.Sample_Entry := Invalid_Sample_Entry;

      Sample_Select.Push_Window;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus
     (This       : in out Edit_Sample_Menu;
      Exit_Value : Window_Exit_Value)
   is
      New_State : Edit_Sample_State;
      use WNM.Audio;
   begin

      --  Transition to the new state
      case This.State is
         when Select_Sample =>

            if Exit_Value = Success then
               WNM.Sample_Edit.Load (Menu.Sample_Select.Selected,
                                     Sample_Point_Index'First,
                                     Sample_Point_Index'Last);
               New_State := Trim;

            else
               Menu.Pop (Exit_Value);
               return;
            end if;

         when Trim =>

            if Exit_Value = Success then
               New_State := Enter_Name;
            else
               New_State := Select_Sample;
            end if;

         when Enter_Name =>

            if Exit_Value = Success then
               New_State := Confirm;
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
               Menu.Exit_Menu;
               return;
            else
               New_State := Select_Index;
            end if;

      end case;

      This.State := New_State;

      --  Push the next window
      case New_State is
         when Select_Sample =>
            Sample_Select.Push_Window;
         when Trim =>
            Sample_Trim.Push_Window;
         when Confirm =>
            Yes_No_Dialog.Push_Window;
         when Enter_Name =>
            Text_Dialog.Set_Title ("Sample name?");
            Text_Dialog.Push_Window;
         when Select_Index =>
            Sample_Select.Push_Window;
      end case;

   end On_Focus;

end WNM.GUI.Menu.Sample_Edit;
