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

with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Persistent;
with WNM.Screen;

with midi_clock_in_icon;
with midi_clock_out_icon;

package body WNM.GUI.Menu.MIDI_Settings is

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
          when others => MIDI_Misc);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This   : in out Instance) is
      Sub : constant Sub_Settings := This.Current_Setting;
      Top : constant Top_Settings := To_Top (Sub);

      Icon_W : constant := 19;
      Icon_H : constant := 24;

      procedure Cross (X, Y : Integer; On : Boolean) is
      begin
         Screen.Draw_Line ((X, Y),
                           (X + Icon_W, Y + Icon_H),
                           On);
         Screen.Draw_Line ((X + 1, Y),
                           (X + Icon_W + 1, Y + Icon_H),
                           On);

         Screen.Draw_Line ((X + Icon_W, Y),
                           (X, Y + Icon_H),
                           On);
         Screen.Draw_Line ((X + Icon_W + 1, Y),
                           (X + 1, Y + Icon_H),
                           On);
      end Cross;

      In_Icon : constant Screen.Point :=
        (Box_Left + Box_Width / 3 - Icon_W / 2, Title_Text_Y + 10);
      Out_Icon : constant Screen.Point :=
        (Box_Right - Box_Width / 3 - Icon_W / 2, In_Icon.Y);

   begin
      Draw_Menu_Box
        ("MIDI settings",
         Count => Top_Settings_Count,
         Index => Top_Settings'Pos (To_Top (This.Current_Setting)));

      case Top is
         when MIDI_Misc =>

            case This.Current_Setting is
               when Clock_In =>
                  Draw_Title ("MIDI Clock Input", "");
               when Clock_Out =>
                  Draw_Title ("MIDI Clock Output", "");
            end case;

            Screen.Copy_Bitmap (midi_clock_in_icon.Data,
                                In_Icon.X, In_Icon.Y,
                                Invert_Color => Sub = Clock_In);

            if not Persistent.Data.MIDI_Clock_Input then
               Cross (In_Icon.X, In_Icon.Y, Sub /= Clock_In);
            end if;

            Screen.Copy_Bitmap (midi_clock_out_icon.Data,
                                Out_Icon.X, Out_Icon.Y,
                                Invert_Color => Sub = Clock_Out);

            if not Persistent.Data.MIDI_Clock_Output then
               Cross (Out_Icon.X, Out_Icon.Y, Sub /= Clock_Out);
            end if;

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
               when Clock_In =>
                  Persistent.Data.MIDI_Clock_Input := not @;
               when Clock_Out =>
                  Persistent.Data.MIDI_Clock_Output := not @;
            end case;

         when Down_Press =>
            case This.Current_Setting is
               when Clock_In =>
                  Persistent.Data.MIDI_Clock_Input := not @;
               when Clock_Out =>
                  Persistent.Data.MIDI_Clock_Output := not @;
            end case;

         when B_Press =>
            Menu.Pop (Exit_Value => Failure);

         when others =>
            null;
      end case;
   end On_Event;

end WNM.GUI.Menu.MIDI_Settings;
