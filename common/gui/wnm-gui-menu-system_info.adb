-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2023 Fabien Chouteau                  --
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

with HAL;

with WNM.GUI.Menu.Drawing;
with WNM.GUI.Menu.Yes_No_Dialog;
with WNM.Synth;
with WNM.Mixer;
with WNM.Project.Library;
with WNM.Persistent;
with WNM.Screen;
with WNM.LEDs;

package body WNM.GUI.Menu.System_Info is

   Singleton : aliased Instance;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Singleton'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Instance) is
   begin
      Drawing.Draw_Menu_Box ("System info",
                             Info_Kind_Count,
                             Info_Kind'Pos (This.K));

      case This.K is

         when Version =>
            Drawing.Draw_Title ("Firmware Version", "");
            Drawing.Draw_Lines_Center
              (Drawing.Box_Top + 13,
               WNM_HAL.Firmware_Version);

         when LED_Brightness =>
            Drawing.Draw_Title ("LED Brightness", "");
            Drawing.Draw_Value (WNM.Persistent.Data.LED_Brightness'Img);

         when Synth_CPU_Load =>
            Drawing.Draw_Title ("Synth CPU Load", "");
            Drawing.Draw_Value (Img (Synth.Last_CPU_Load));

         when Synth_Max_CPU_Load =>
            Drawing.Draw_Title ("Synth Max CPU Load", "");
            Drawing.Draw_Value (Img (Synth.Max_CPU_Load));

         when Synth_Missed_Deadlines =>
            Drawing.Draw_Title ("Synth Missed DL", "");
            Drawing.Draw_Value (Synth.Missed_Deadlines'Img);

         when DAC_Missed_Deadlines =>
            Drawing.Draw_Title ("DAC Missed DL", "");
            Drawing.Draw_Value (Mixer.Missed_DAC_Deadlines'Img);

         when Input_Missed_Deadlines =>
            Drawing.Draw_Title ("Input Missed DL", "");
            Drawing.Draw_Value (Mixer.Missed_Input_Deadlines'Img);

         when Prj_Last_Load_Size =>
            Drawing.Draw_Title ("Size of last loaded", "project");
            Drawing.Draw_Value (Project.Library.Last_Loaded_Size'Img);

         when Prj_Last_Save_Size =>
            Drawing.Draw_Title ("Size of last saved", "project");
            Drawing.Draw_Value (Project.Library.Last_Saved_Size'Img);

         when Raise_Exception =>
            Drawing.Draw_Title ("Press A to raise", "an exception");

         when Touch =>
            declare
               use WNM.GUI.Menu.Drawing;

               type TP_Img is delta 0.01 range 0.0 .. 10.0;
               --  Use a fixed point type to get a 'Img without
               --  scientific notation...

               State : constant Touch_Data := WNM_HAL.Touch_Strip_State;

               Touch_Str : constant String :=
                 (if State.Touch
                  then TP_Img'Image (TP_Img (State.Value))
                  else "_.__");

               Bar_Length : constant Natural :=
                 Natural (Float ((Box_Right - Box_Left)) * State.Value);

               Thresh_Y : constant := Box_Bottom - Font_Height - 2;
               Read_Y   : constant := Thresh_Y - Font_Height - 2;

               TP_Spacing : constant := (Box_Right - Box_Left) / 3;
               TP1_X    : constant := Box_Left + 3;
               TP2_X    : constant := TP1_X + TP_Spacing;
               TP3_X    : constant := TP2_X + TP_Spacing;
            begin

               Drawing.Draw_Title ("Touch sensor " & Touch_Str, "");

               Drawing.Draw_Str (TP1_X, Read_Y, WNM_HAL.TP1'Img);
               Drawing.Draw_Str (TP2_X, Read_Y, WNM_HAL.TP2'Img);
               Drawing.Draw_Str (TP3_X, Read_Y, WNM_HAL.TP3'Img);

               Drawing.Draw_Str (TP1_X, Thresh_Y,
                                 WNM.Persistent.Data.TP1_Threshold'Img);
               Drawing.Draw_Str (TP2_X, Thresh_Y,
                                 WNM.Persistent.Data.TP2_Threshold'Img);
               Drawing.Draw_Str (TP3_X, Thresh_Y,
                                 WNM.Persistent.Data.TP3_Threshold'Img);

               if State.Touch then
                  Screen.Draw_Line
                    ((Box_Left, Box_Top + Font_Height + 8),
                     (Box_Left + Bar_Length, Box_Top + Font_Height + 8));
               end if;
            end;

         when HP_Detect =>
            Drawing.Draw_Title ("Head Phone", "");
            Drawing.Draw_Value (if WNM_HAL.HP_Detect
                                then "Detected"
                                else "Not Detected");

         when Battery =>
            Drawing.Draw_Title ("Battery", "");
            Drawing.Draw_Value (WNM_HAL.Battery_Millivolts'Img & " mV");

      end case;
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Instance;
                       Event : Menu_Event)
   is
      package LED_Dim_Next is new Enum_Next (WNM.LEDs.Brightness);
      use LED_Dim_Next;

      use HAL;

   begin
      case Event.Kind is
         when A_Press =>
            case This.K is
               when Synth_Max_CPU_Load =>
                  Synth.Clear_Max_CPU_Load;
               when Synth_Missed_Deadlines =>
                  Synth.Clear_Missed_Deadlines;
               when DAC_Missed_Deadlines =>
                  Mixer.Clear_Missed_DAC_Deadlines;
               when Input_Missed_Deadlines =>
                  Mixer.Clear_Missed_Input_Deadlines;
               when Raise_Exception =>
                  Yes_No_Dialog.Set_Title ("Raise exception?");
                  Yes_No_Dialog.Push_Window;

               when Touch =>
                  --  Set new threshold values
                  WNM.Persistent.Data.TP1_Threshold := WNM_HAL.TP1 + 200;
                  WNM.Persistent.Data.TP2_Threshold := WNM_HAL.TP2 + 200;
                  WNM.Persistent.Data.TP3_Threshold := WNM_HAL.TP3 + 200;

                  WNM_HAL.Set_Thresholds (WNM.Persistent.Data.TP1_Threshold,
                                          WNM.Persistent.Data.TP2_Threshold,
                                          WNM.Persistent.Data.TP3_Threshold);
               when others =>
                  null;
            end case;

         when B_Press =>
            Menu.Pop (Exit_Value => Failure);

         when Right_Press =>
            Next (This.K);
         when Left_Press =>
            Prev (This.K);

         when Up_Press =>
            case This.K is
               when LED_Brightness =>
                  Next (WNM.Persistent.Data.LED_Brightness);
               when others =>
                  null;
            end case;

         when Down_Press =>
            case This.K is
               when LED_Brightness =>
                  Prev (WNM.Persistent.Data.LED_Brightness);
               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
   end On_Event;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus (This       : in out Instance;
                       Exit_Value : Window_Exit_Value)
   is
   begin
      case This.K is
         when Raise_Exception =>
            if Exit_Value = Success then
               raise Program_Error with "System info raise";
            end if;

         when others =>
            null;
      end case;
   end On_Focus;

end WNM.GUI.Menu.System_Info;
