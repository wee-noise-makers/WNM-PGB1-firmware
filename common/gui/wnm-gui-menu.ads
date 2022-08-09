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

package WNM.GUI.Menu is

   type Base_Menu_Kind is (Main_Menu, Step_Menu, Track_Menu, Pattern_Menu);

   procedure Open (Kind : Base_Menu_Kind);

   function In_Menu return Boolean;
   procedure Draw;

   type Menu_Event_Kind is (Left_Press,
                            Right_Press,
                            Encoder_Left,
                            Encoder_Right);

   type Menu_Event (Kind : Menu_Event_Kind := Left_Press) is record
      case Kind is
         when Encoder_Left | Encoder_Right =>
            Value : Integer;
         when others =>
            null;
      end case;
   end record;

   procedure On_Event (Event : Menu_Event);

   type Window_Exit_Value is (None, Success, Failure);

   type Menu_Window is interface;

   type Any_Menu_Window is access all Menu_Window'Class;

   procedure Draw (This : in out Menu_Window)
   is abstract;

   procedure On_Event (This  : in out Menu_Window;
                       Event : Menu_Event)
   is abstract;

   procedure On_Pushed (This  : in out Menu_Window)
   is abstract;
   procedure On_Focus (This       : in out Menu_Window;
                       Exit_Value : Window_Exit_Value)
   is abstract;

   procedure Push (Window : not null Any_Menu_Window);
   procedure Pop (Exit_Value : Window_Exit_Value);

   procedure Exit_Menu;
   --  Pop all the windows in stack with a None exit value

end WNM.GUI.Menu;
