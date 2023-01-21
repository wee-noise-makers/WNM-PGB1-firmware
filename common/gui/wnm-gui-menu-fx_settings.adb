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

with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;

package body WNM.GUI.Menu.FX_Settings is

   package Sub_Settings_Next is new Enum_Next (Sub_Settings,
                                               Wrap => False);
   use Sub_Settings_Next;

   Pattern_Menu_Singleton : aliased Pattern_Settings_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Pattern_Menu_Singleton'Access);
   end Push_Window;

   ------------
   -- To_Top --
   ------------

   function To_Top (S : Sub_Settings) return Top_Settings
   is (case S is
          when WNM.Project.Drive_Amount   => Overdrive,
          when WNM.Project.Delay_Time     => Delayline,
          when WNM.Project.Delay_Feedback => Delayline,
          when WNM.Project.Filter_Mode    => Filter,
          when WNM.Project.Filter_Cutoff  => Filter,
          when WNM.Project.Filter_Reso    => Filter);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Pattern_Settings_Menu)
   is
      use WNM.Project;

      Top_Setting : constant Top_Settings := To_Top (This.Current_Setting);
      Sub_Setting : constant Sub_Settings := This.Current_Setting;
   begin
      Draw_Menu_Box ("FX settings",
                     Count => Top_Settings_Count,
                     Index => Top_Settings'Pos
                       (To_Top (This.Current_Setting)));

      case Top_Setting is
         when Overdrive =>
            Draw_Title ("Overdrive", "");

            Draw_CC_Value (Id => A,
                           Value => Project.Drive_Amount_Value,
                           Label => "DRV",
                           Selected => True);

         when Delayline =>
            Draw_Title ("Delay", "");

            Draw_CC_Value (Id => A,
                           Value => Project.Delay_Time_Value,
                           Label => "TIM",
                           Selected => Sub_Setting = Delay_Time);

            Draw_CC_Value (Id => B,
                           Value => Project.Delay_Feedback_Value,
                           Label => "FBK",
                           Selected => Sub_Setting = Delay_Feedback);

         when Filter =>
            Draw_Title ("Filter", "");

            Draw_CC_Value (Id => A,
                           Value => Project.Filter_Mode_Kind'Pos
                             (Project.Filter_Mode_Value),
                           Label => "MOD",
                           Selected => Sub_Setting = Filter_Mode);
            Draw_CC_Value (Id => B,
                           Value => Project.Filter_Cutoff_Value,
                           Label => "CTF",
                           Selected => Sub_Setting = Filter_Cutoff);
            Draw_CC_Value (Id => C,
                           Value => Project.Filter_Reso_Value,
                           Label => "RES",
                           Selected => Sub_Setting = Filter_Reso);
      end case;

   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Pattern_Settings_Menu;
                       Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            null;
         when Right_Press =>
            null;

         when Encoder_Right =>
            case Event.Value is
               when 0 =>
                  null;
               when 1 =>
                  Project.Next_Value (This.Current_Setting);
               when 2 .. Integer'Last =>
                  Project.Next_Value_Fast (This.Current_Setting);
               when -1 =>
                  Project.Prev_Value (This.Current_Setting);
               when Integer'First .. -2 =>
                  Project.Prev_Value_Fast (This.Current_Setting);
            end case;

         when Encoder_Left =>
            if Event.Value > 0 then
               Next (This.Current_Setting);
            elsif Event.Value < 0 then
               Prev (This.Current_Setting);
            end if;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed (This  : in out Pattern_Settings_Menu)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus (This       : in out Pattern_Settings_Menu;
                       Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.FX_Settings;
