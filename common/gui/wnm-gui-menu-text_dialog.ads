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

package WNM.GUI.Menu.Text_Dialog is

   procedure Push_Window;

   procedure Push_Window (Str : String);
   --  Push with a preset text

   Title_Max_Len : constant := 15;

   procedure Set_Title (Title : String)
     with Pre => Title'Length <= Title_Max_Len;

   function Value return String;

private

   subtype Text_Range is Natural range 1 .. 15;

   type Text_Dialog_Window is new Menu_Window with record
      Text          : String (Text_Range);
      Len           : Natural;
      Index         : Natural;
      Reset_On_Push : Boolean;
   end record;

   overriding
   procedure Draw (This   : in out Text_Dialog_Window);

   overriding
   procedure On_Event (This  : in out Text_Dialog_Window;
                       Event : Menu_Event);

   overriding
   procedure On_Pushed (This  : in out Text_Dialog_Window);

   overriding
   procedure On_Focus (This       : in out Text_Dialog_Window;
                       Exit_Value : Window_Exit_Value)
   is null;

end WNM.GUI.Menu.Text_Dialog;
