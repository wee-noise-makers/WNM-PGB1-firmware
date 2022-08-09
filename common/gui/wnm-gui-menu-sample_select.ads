-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2017 Fabien Chouteau                  --
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

package WNM.GUI.Menu.Sample_Select is

   procedure Push_Window;

   function Selected return Sample_Index;

private

   type Sample_Select_Window is new Menu_Window with record
      Index  : WNM.Sample_Library.Sample_Index;
   end record;

   overriding
   procedure Draw (This : in out Sample_Select_Window);

   overriding
   procedure On_Event (This  : in out Sample_Select_Window;
                       Event : Menu_Event);

   overriding
   procedure On_Pushed (This  : in out Sample_Select_Window);

   overriding
   procedure On_Focus (This       : in out Sample_Select_Window;
                       Exit_Value : Window_Exit_Value);

end WNM.GUI.Menu.Sample_Select;
