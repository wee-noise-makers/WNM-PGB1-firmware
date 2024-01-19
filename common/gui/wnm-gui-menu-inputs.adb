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

with WNM.Mixer;
with WNM.Project;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;

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
          when Line_In_Volume .. Input_FX    => Audio_In,
          when MIDI_In_Mode .. MIDI_In_Clock => MIDI_In);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This   : in out Instance) is
      Top_Setting : constant Top_Settings := To_Top (This.Current_Setting);
   begin
      Draw_Menu_Box
        ("Inputs settings",
         Count => Top_Settings_Count,
         Index => Top_Settings'Pos (To_Top (This.Current_Setting)));

      case Top_Setting is
         when Audio_In =>

            case This.Current_Setting is
               when Line_In_Volume =>
                  Draw_Title ("Line Input", "");
               when Internal_Mic_Volume =>
                  Draw_Title ("Internal Microphone", "");
               when Headset_Mic_Volume =>
                  Draw_Title ("Headset Microphone", "");
               when Input_FX =>
                  Draw_Title ("FX: " & Img (Mixer.Input_FX), "");
               when others =>
                  null;
            end case;

            Draw_Volume (Id => WNM.Project.A,
                         Value => Mixer.Get_Line_In_Volume,
                         Label => "LIN",
                         Selected =>
                            This.Current_Setting = Line_In_Volume);

            Draw_Volume (Id => WNM.Project.B,
                         Value => Mixer.Get_Internal_Mic_Volume,
                         Label => "MIC",
                         Selected =>
                            This.Current_Setting = Internal_Mic_Volume);

            Draw_Volume (Id => WNM.Project.C,
                         Value => Mixer.Get_Headset_Mic_Volume,
                         Label => "HSM",
                         Selected =>
                            This.Current_Setting = Headset_Mic_Volume);

            Draw_FX (Id => WNM.Project.D,
                     Value => Mixer.Input_FX,
                     Selected => This.Current_Setting = Input_FX);

         when MIDI_In =>
            Draw_Title ("MIDI In", "");
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
               when Line_In_Volume =>
                  Mixer.Change_Line_In_Volume (1);
               when Internal_Mic_Volume =>
                  Mixer.Change_Internal_Mic_Volume (1);
               when Headset_Mic_Volume =>
                  Mixer.Change_Headset_Mic_Volume (1);
               when Input_FX =>
                  Mixer.Input_FX_Next;
               when others =>
                  null;
            end case;

         when Down_Press =>
            case This.Current_Setting is
               when Line_In_Volume =>
                  Mixer.Change_Line_In_Volume (-1);
               when Internal_Mic_Volume =>
                  Mixer.Change_Internal_Mic_Volume (-1);
               when Headset_Mic_Volume =>
                  Mixer.Change_Headset_Mic_Volume (-1);
               when Input_FX =>
                  Mixer.Input_FX_Prev;
               when others =>
                  null;
            end case;

         when B_Press =>
            Menu.Pop (Exit_Value => Failure);

         when others =>
            null;
      end case;
   end On_Event;

end WNM.GUI.Menu.Inputs;
