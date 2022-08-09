-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2020 Fabien Chouteau                  --
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

package WNM.GUI.Menu.Yes_No_Dialog is

   procedure Push_Window;

   Title_Max_Len : constant := 15;

   procedure Set_Title (Title : String)
     with Pre => Title'Length <= Title_Max_Len;

private

   type Yes_No_Dialog_Window is new Menu_Window with record
      Yes : Boolean;
   end record;

   overriding
   procedure Draw (This : in out Yes_No_Dialog_Window);

   overriding
   procedure On_Event (This  : in out Yes_No_Dialog_Window;
                       Event : Menu_Event);

   overriding
   procedure On_Pushed (This : in out Yes_No_Dialog_Window);

   overriding
   procedure On_Focus (This       : in out Yes_No_Dialog_Window;
                       Exit_Value : Window_Exit_Value)
   is null;

end WNM.GUI.Menu.Yes_No_Dialog;
