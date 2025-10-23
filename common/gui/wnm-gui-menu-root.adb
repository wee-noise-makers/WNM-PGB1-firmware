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

with WNM.GUI.Menu.Drawing;       use WNM.GUI.Menu.Drawing;
with WNM.GUI.Menu.Yes_No_Dialog;
with WNM.GUI.Bitmap_Fonts;
with WNM.GUI.Menu.Projects;
with WNM.GUI.Menu.Inputs;
with WNM.GUI.Menu.System_Info;
with WNM.GUI.Menu.MIDI_Settings;
with WNM.GUI.Menu.User_Waveform;
with WNM.Power_Control;
with WNM.Screen;

with edit_wave_icon;
with project_icon;
with line_in_icon;
with midi_icon;
with system_info_icon;
with firmware_update_icon;

package body WNM.GUI.Menu.Root is

   On_Stack : Boolean := False with Volatile;

   Root_Window_Singleton : aliased Root_Menu;

   function Menu_Item_Text (Item : Menu_Items) return String
   is (case Item is
          when Projects        => "Projects",
          when Inputs          => "Inputs",
          when User_Waveform   => "Custom Waveform",
          when MIDI_Settings   => "MIDI Settings",
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

   ---------------
   -- Draw_Icon --
   ---------------

   procedure Draw_Icon (Bmp : Screen.Bitmap) is
      Icon_W : constant Natural := Bmp.W;
      Icon_H : constant Natural := Bmp.H;
      Icon_Left : constant Natural :=
        Box_Center.X - (Icon_W / 2);
      Icon_Top : constant Natural :=
        Box_Center.Y - 3 - (Icon_H / 2);
   begin
      Screen.Copy_Bitmap (Bmp, Icon_Left, Icon_Top);
   end Draw_Icon;

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

      case This.Item is
         when Projects =>
            Draw_Icon (project_icon.Data);
         when User_Waveform =>
            Draw_Icon (edit_wave_icon.Data);
         when Inputs =>
            Draw_Icon (line_in_icon.Data);
         when MIDI_Settings =>
            Draw_Icon (midi_icon.Data);
         when DFU_Mode =>
            Draw_Icon (firmware_update_icon.Data);
         when System_Info =>
            Draw_Icon (system_info_icon.Data);
      end case;

      Draw_Str_Center (Box_Bottom - Bitmap_Fonts.Height - 2,
                       Menu_Item_Text (This.Item));
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

               when User_Waveform =>
                  Menu.User_Waveform.Push_Window;

               when MIDI_Settings =>
                  Menu.MIDI_Settings.Push_Window;

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
