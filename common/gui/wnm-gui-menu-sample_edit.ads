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

with WNM.Sample_Library; use WNM.Sample_Library;

package WNM.GUI.Menu.Sample_Edit is

   procedure Push_Window;

private

   type Edit_Sample_State is (Select_Mode,
                              Select_Input,
                              Record_Sample,
                              Select_Sample,
                              Trim,
                              Enter_Name,
                              Select_Index,
                              Confirm);

   type Main_Sample_Edit_Mode is (New_Sample, Edit_Sample);
   package Main_Sample_Edit_Mode_Next
   is new Enum_Next (Main_Sample_Edit_Mode);
   use Main_Sample_Edit_Mode_Next;

   type Edit_Sample_Menu is new Menu_Window with record
      Mode         : Main_Sample_Edit_Mode := Edit_Sample;
      State        : Edit_Sample_State;
      Sample_Entry : Sample_Index := Invalid_Sample_Entry;
   end record;

   overriding
   procedure Draw (This : in out Edit_Sample_Menu);

   overriding
   procedure On_Event (This  : in out Edit_Sample_Menu;
                       Event :        Menu_Event);

   overriding
   procedure On_Pushed (This  : in out Edit_Sample_Menu);

   overriding
   procedure On_Focus (This       : in out Edit_Sample_Menu;
                       Exit_Value : Window_Exit_Value);

   procedure Exit_Edit (This       : in out Edit_Sample_Menu;
                        Exit_Value :        Window_Exit_Value);

end WNM.GUI.Menu.Sample_Edit;
