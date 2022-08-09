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

with WNM.Sequencer;

package WNM.GUI.Menu.Step_Settings is

   procedure Push_Window;

private


   type Top_Settings is (Condition, Note, Repeat, CC_A, CC_B, CC_C, CC_D);
   function Top_Settings_Count is new Enum_Count (Top_Settings);

   subtype Sub_Settings is WNM.Sequencer.User_Step_Settings;

   function Sub_Settings_Count is new Enum_Count (Sub_Settings);

   type Step_Settings_Menu is new Menu_Window with record
      Current_Setting : Sub_Settings := Sub_Settings'First;
   end record;

   overriding
   procedure Draw (This : in out Step_Settings_Menu);

   overriding
   procedure On_Event (This  : in out Step_Settings_Menu;
                       Event : Menu_Event);

   overriding
   procedure On_Pushed (This  : in out Step_Settings_Menu);

   overriding
   procedure On_Focus (This       : in out Step_Settings_Menu;
                       Exit_Value : Window_Exit_Value);

end WNM.GUI.Menu.Step_Settings;
