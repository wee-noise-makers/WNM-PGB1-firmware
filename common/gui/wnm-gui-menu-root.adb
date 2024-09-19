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

with WNM.GUI.Menu.Drawing;           use WNM.GUI.Menu.Drawing;
--  with WNM.GUI.Menu.Sample_Select;     use WNM.GUI.Menu.Sample_Select;
with WNM.GUI.Menu.Text_Dialog;       use WNM.GUI.Menu.Text_Dialog;
with WNM.GUI.Menu.Yes_No_Dialog;
--  with WNM.GUI.Menu.Create_Sample;
--  with WNM.GUI.Menu.Passthrough;
with WNM.GUI.Menu.Projects;
with WNM.GUI.Menu.Inputs;
with WNM.GUI.Menu.System_Info;
with WNM.Power_Control;

package body WNM.GUI.Menu.Root is

   On_Stack : Boolean := False with Volatile;

   Root_Window_Singleton : aliased Root_Menu;

   function Menu_Item_Text (Item : Menu_Items) return String
   is (case Item is
          when Projects        => "Projects",
          when Inputs          => "Inputs",
          when Test_Text_Input => "Test text input",
          when DFU_Mode        => "Update Mode",
          when System_Info     => "System Info");

   ----------------------
   -- Push_Root_Window --
   ----------------------

   procedure Push_Root_Window is
   begin
      if not On_Stack then
         Push (Root_Window_Singleton'Access);
         On_Stack := True;
      end if;
   end Push_Root_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This   : in out Root_Menu)
   is
   begin
      Draw_Menu_Box ("Menu",
                     Count => Menu_Items_Count,
                     Index => Menu_Items'Pos (This.Item));
      Draw_Title (Menu_Item_Text (This.Item), "");
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Root_Menu;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when A_Press =>
            case This.Item is
               when Projects =>
                  Menu.Projects.Push_Window;

               when Inputs =>
                  Menu.Inputs.Push_Window;

               when Test_Text_Input =>
                  Text_Dialog.Set_Title ("Enter some text");
                  Text_Dialog.Push_Window;

               when DFU_Mode =>
                  Yes_No_Dialog.Set_Title ("Enter Update Mode?");
                  Yes_No_Dialog.Push_Window;

               when System_Info =>
                  Menu.System_Info.Push_Window;

            end case;

         when B_Press =>
            null;

         when Up_Press =>
            null;
         when Down_Press =>
            null;

         when Right_Press =>
            if This.Item /= Menu_Items'Last then
               This.Item := Menu_Items'Succ (This.Item);
            else
               This.Item := Menu_Items'First;
            end if;

         when Left_Press =>
            if This.Item /= Menu_Items'First then
               This.Item := Menu_Items'Pred (This.Item);
            else
               This.Item := Menu_Items'Last;
            end if;

         when Slider_Touch =>
            null;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Root_Menu)
   is
   begin
      This.Item := Menu_Items'First;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus
     (This       : in out Root_Menu;
      Exit_Value : Window_Exit_Value)
   is
   begin
      case This.Item is
         when DFU_Mode =>
            if Exit_Value = Success then
               WNM.Power_Control.Enter_DFU_Mode;
            end if;

         when others =>
            null;
      end case;
   end On_Focus;

   ------------
   -- On_Pop --
   ------------

   overriding
   procedure On_Pop (This : in out Root_Menu) is
      pragma Unreferenced (This);
   begin
      On_Stack := False;
   end On_Pop;

end WNM.GUI.Menu.Root;
