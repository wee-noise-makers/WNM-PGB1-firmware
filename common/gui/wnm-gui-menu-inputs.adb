-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2023 Fabien Chouteau                  --
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

with WNM.Audio_Routing;
with WNM.Mixer;
with WNM.Project;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Screen;
with WNM.GUI.Bitmap_Fonts;

with line_in_icon;
with internal_mic_icon;
with headset_icon;

package body WNM.GUI.Menu.Inputs is

   Singleton : aliased Instance;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Singleton'Access);
   end Push_Window;

   ------------
   -- To_Top --
   ------------

   function To_Top (S : Sub_Settings) return Top_Settings
   is (case S is
          when Line_In_Mute .. Input_Volume  => Audio_In,
          when Input_FX                      => Audio_In_FX);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This   : in out Instance) is
      Sub : constant Sub_Settings := This.Current_Setting;
      Top : constant Top_Settings := To_Top (Sub);

      Icon_Size : constant := 16;

      Icons_Y : constant :=
        Drawing.Box_Bottom - Bitmap_Fonts.Height - Icon_Size - 3;

      Spacing : constant := 32;
      L_Icon_X : constant := Drawing.Box_Left + 6;
      M_Icon_X : constant := L_Icon_X + Spacing;
      R_Icon_X : constant := M_Icon_X + Spacing;

      procedure Cross (X, Y : Integer; On : Boolean) is
      begin
         Screen.Draw_Line ((X, Y),
                           (X + Icon_Size, Y + Icon_Size),
                           On);
         Screen.Draw_Line ((X + 1, Y),
                           (X + Icon_Size + 1, Y + Icon_Size),
                           On);

         Screen.Draw_Line ((X + Icon_Size, Y),
                           (X, Y + Icon_Size),
                           On);
         Screen.Draw_Line ((X + Icon_Size + 1, Y),
                           (X + 1, Y + Icon_Size),
                           On);
      end Cross;

   begin
      Draw_Menu_Box
        ("Inputs settings",
         Count => Top_Settings_Count,
         Index => Top_Settings'Pos (To_Top (This.Current_Setting)));

      case Top is
         when Audio_In =>

            case This.Current_Setting is
               when Line_In_Mute =>
                  Draw_Title ("Line Input", "");
               when Internal_Mic_Mute =>
                  Draw_Title ("Internal Microphone", "");
               when Headset_Mic_Mute =>
                  Draw_Title ("Headset Microphone", "");
               when Input_Volume =>
                  Draw_Title ("Input Volume", "");
               when others =>
                  null;
            end case;

            Draw_Volume (Id => WNM.Project.A,
                         Value => 0,
                         Label => (if Audio_Routing.Get_Line_In_Mute
                                   then "Off"
                                   else "On"),
                         Selected => False);

            Draw_Volume (Id => WNM.Project.B,
                         Value => 0,
                         Label => (if Audio_Routing.Get_Internal_Mic_Mute
                                   then "Off"
                                   else "On"),
                         Selected => False);

            Draw_Volume (Id => WNM.Project.C,
                         Value => 0,
                         Label => (if Audio_Routing.Get_Headset_Mic_Mute
                                   then "Off"
                                   else "On"),
                         Selected => False);

            Draw_Volume (Id => WNM.Project.D,
                         Value => Audio_Routing.Get_ADC_Volume,
                         Label => "VOL",
                         Selected => Sub = Input_Volume);

            Screen.Copy_Bitmap
              (line_in_icon.Data,
               L_Icon_X, Icons_Y,
               Invert_Color => Sub = Line_In_Mute);

            if Audio_Routing.Get_Line_In_Mute then
               Cross (L_Icon_X, Icons_Y, Sub /= Line_In_Mute);
            end if;

            Screen.Copy_Bitmap
              (internal_mic_icon.Data,
               M_Icon_X, Icons_Y,
               Invert_Color => Sub = Internal_Mic_Mute);

            if Audio_Routing.Get_Internal_Mic_Mute then
               Cross (M_Icon_X, Icons_Y, Sub /= Internal_Mic_Mute);
            end if;

            Screen.Copy_Bitmap
              (headset_icon.Data,
               R_Icon_X, Icons_Y,
               Invert_Color =>  Sub = Headset_Mic_Mute);

            if Audio_Routing.Get_Headset_Mic_Mute then
               Cross (R_Icon_X, Icons_Y, Sub /= Headset_Mic_Mute);
            end if;

         when Audio_In_FX =>

            Draw_Title ("Input FX: " & Img (Mixer.Input_FX), "");
            Draw_FX (Id => WNM.Project.A,
                     Value => Mixer.Input_FX,
                     Selected => Sub = Input_FX);

      end case;
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Instance;
                       Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            Prev (This.Current_Setting);
         when Right_Press =>
            Next (This.Current_Setting);

         when Up_Press =>
            case This.Current_Setting is
               when Line_In_Mute =>
                  Audio_Routing.Toggle_Line_In_Mute;
               when Internal_Mic_Mute =>
                  Audio_Routing.Toggle_Internal_Mic_Mute;
               when Headset_Mic_Mute =>
                  Audio_Routing.Toggle_Headset_Mic_Mute;
               when Input_Volume =>
                  Audio_Routing.Change_ADC_Volume (1);
               when Input_FX =>
                  Mixer.Input_FX_Next;
            end case;

         when Down_Press =>
            case This.Current_Setting is
               when Line_In_Mute =>
                  Audio_Routing.Toggle_Line_In_Mute;
               when Internal_Mic_Mute =>
                  Audio_Routing.Toggle_Internal_Mic_Mute;
               when Headset_Mic_Mute =>
                  Audio_Routing.Toggle_Headset_Mic_Mute;
               when Input_Volume =>
                  Audio_Routing.Change_ADC_Volume (-1);
               when Input_FX =>
                  Mixer.Input_FX_Prev;
            end case;

         when B_Press =>
            Menu.Pop (Exit_Value => Failure);

         when others =>
            null;
      end case;
   end On_Event;

end WNM.GUI.Menu.Inputs;
