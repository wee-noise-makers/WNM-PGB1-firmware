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

with WNM.GUI.Menu.Root;
with WNM.GUI.Menu.Track_Settings;
with WNM.GUI.Menu.Pattern_Settings;
with WNM.GUI.Menu.Step_Settings;

package body WNM.GUI.Menu is

   subtype Stack_Index is Natural range 1 .. 10;

   Stack : array (Stack_Index) of Any_Menu_Window := (others => null);
   Stack_Cnt : Natural := 0;

   procedure Open (Kind : Base_Menu_Kind) is
   begin
      Exit_Menu;
      case Kind is
         when Main_Menu =>
            GUI.Menu.Root.Push_Root_Window;
         when Step_Menu =>
            GUI.Menu.Step_Settings.Push_Window;
         when Track_Menu =>
            GUI.Menu.Track_Settings.Push_Window;
         when Pattern_Menu =>
            GUI.Menu.Pattern_Settings.Push_Window;
      end case;
   end Open;

   -------------
   -- In_Menu --
   -------------

   function In_Menu return Boolean
   is (Stack_Cnt /= 0);

   ----------
   -- Draw --
   ----------

   procedure Draw is
   begin
      if Stack_Cnt /= 0 then
         Stack (Stack_Cnt).Draw;
      else
         raise Program_Error with "We are not in the menu...";
      end if;
   end Draw;

   --------------
   -- On_Event --
   --------------

   procedure On_Event (Event : Menu_Event) is
   begin
      if Stack_Cnt /= 0 then
         Stack (Stack_Cnt).On_Event (Event);
      end if;
   end On_Event;

   ----------
   -- Push --
   ----------

   procedure Push (Window : not null Any_Menu_Window) is
   begin
      if Stack_Cnt = Stack_Index'Last then
         raise Program_Error with "No more room in the windows stack";
      end if;

      Stack_Cnt := Stack_Cnt + 1;
      Stack (Stack_Cnt) := Window;
      Window.On_Pushed;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Exit_Value : Window_Exit_Value) is
   begin
      if Stack_Cnt = 0 then
         raise Program_Error with "No window in the stack";
      end if;

      Stack_Cnt := Stack_Cnt - 1;
      if Stack_Cnt /= 0 then
         Stack (Stack_Cnt).On_Focus (Exit_Value);
      end if;
   end Pop;

   ---------------
   -- Exit_Menu --
   ---------------

   procedure Exit_Menu is
   begin
      while Stack_Cnt /= 0 loop
         Pop (None);
      end loop;
   end Exit_Menu;

end WNM.GUI.Menu;
