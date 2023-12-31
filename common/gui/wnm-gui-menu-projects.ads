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

package WNM.GUI.Menu.Projects is

   procedure Push_Window;

private

   type Menu_Items is (Save_Project,
                       Load_Project,
                       Rename_Project,
                       Delete_Project,
                       Clear_Project);

   function Menu_Items_Count is new Enum_Count (Menu_Items);

   type Window_State is (Idle,
                         Select_Project,
                         Enter_Name,
                         Confirm);

   type Instance is new Menu_Window with record
      Item : Menu_Items;
      State : Window_State := Select_Project;
   end record;

   overriding
   procedure Draw (This : in out Instance);

   overriding
   procedure On_Event (This  : in out Instance;
                       Event : Menu_Event);

   overriding
   procedure On_Pushed (This  : in out Instance);

   overriding
   procedure On_Focus (This       : in out Instance;
                       Exit_Value : Window_Exit_Value);

end WNM.GUI.Menu.Projects;
