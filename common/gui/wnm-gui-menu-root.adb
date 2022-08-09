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

with WNM.Power_Control;
with WNM.GUI.Menu.Drawing;           use WNM.GUI.Menu.Drawing;
with WNM.GUI.Menu.Sample_Select;     use WNM.GUI.Menu.Sample_Select;
with WNM.GUI.Menu.Text_Dialog;       use WNM.GUI.Menu.Text_Dialog;
with WNM.GUI.Menu.Sample_Edit;
with WNM.GUI.Menu.FM_Settings;
with WNM.GUI.Menu.Create_Sample;
with WNM.GUI.Menu.Passthrough;

with WNM.Projects;

package body WNM.GUI.Menu.Root is

   Root_Window_Singleton : aliased Root_Menu;

   function Menu_Item_Text (Item : Menu_Items) return String
   is (case Item is
          when Save_Project    => "Save project",
          when Load_Project    => "Load project",
          when Edit_Sample     => "Edit sample",
          when Create_Sample   => "Create sample",
          when Change_Sample   => "Change sample",
          when FM_Tune         => "FM Tuner",
          when Set_Passthrough => "Passthrough",
          when Test_Text_Input => "Test text input",
          when Load            => "Load",
          when Save            => "Save",
          when Settings        => "Settings",
          when Shutdown        => "Shutdown");

   ----------------------
   -- Push_Root_Window --
   ----------------------

   procedure Push_Root_Window is
   begin
      Push (Root_Window_Singleton'Access);
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

   overriding procedure On_Event
     (This  : in out Root_Menu;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            case This.Item is
               when Save_Project =>
                  WNM.Projects.Save;
               when Load_Project =>
                  WNM.Projects.Load;
               when Edit_Sample =>
                  Menu.Sample_Edit.Push_Window;
               when Create_Sample =>
                  Menu.Create_Sample.Push_Window;
               when Change_Sample =>
                  Sample_Select.Push_Window;
               when FM_Tune =>
                  FM_Settings.Push_Window;
               when Set_Passthrough =>
                  Passthrough.Push_Window;
               when Test_Text_Input =>
                  Text_Dialog.Set_Title ("Enter some text");
                  Text_Dialog.Push_Window;
               when Shutdown =>
                  WNM.Power_Control.Power_Down;
               when others =>
                  null;
            end case;
         when Right_Press =>
            Menu.Pop (Exit_Value => None);
         when Encoder_Right =>
            null;
         when Encoder_Left =>
            if Event.Value > 0 then
               if This.Item /= Menu_Items'Last then
                  This.Item := Menu_Items'Succ (This.Item);
               else
                  This.Item := Menu_Items'First;
               end if;
            elsif Event.Value < 0 then
               if This.Item /= Menu_Items'First then
                  This.Item := Menu_Items'Pred (This.Item);
               else
                  This.Item := Menu_Items'Last;
               end if;
            end if;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding procedure On_Pushed
     (This  : in out Root_Menu)
   is
   begin
      This.Item := Menu_Items'First;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding procedure On_Focus
     (This       : in out Root_Menu;
      Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Root;
