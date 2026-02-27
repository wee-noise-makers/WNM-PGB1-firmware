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

with HAL;                   use HAL;
with WNM.GUI.Bitmap_Fonts;  use WNM.GUI.Bitmap_Fonts;
with WNM.GUI.Parameters;
with WNM.Screen;
with WNM.UI;
with WNM.Sequence_Copy;     use WNM.Sequence_Copy;
with WNM.GUI.Menu;
with WNM.GUI.Menu.Drawing;  use WNM.GUI.Menu.Drawing;
with WNM.GUI.Logo;
with WNM.GUI.Popup;
with WNM.Project;
with WNM.Audio_Routing;
with WNM.Time;
with WNM.Synth;

package body WNM.GUI.Update is

   ------------
   -- Update --
   ------------

   procedure Update is
      B : Integer;
      T : constant Tracks := Project.Editing_Track;
   begin

      WNM.Screen.Clear;

      --  Splash screen
      if WNM.Time.Clock < WNM.Time.Milliseconds (1_000) then
         WNM.GUI.Logo.Draw_On_Screen (UInt2 (Anim_Step mod 4));
         WNM.Screen.Update;
         Anim_Step := Anim_Step + 1;
         return;
      end if;

      -- Header --
      B := 1;
      Print (X_Offset    => B,
             Y_Offset    => 0,
             Str         => Project.Track_Name (T));
      if WNM_Configuration.Individual_Synth_Perf_Enabled
        and then
          Project.Mode (T) in Project.Synth_Track_Mode_Kind
      then
         Print (X_Offset    => B,
                Y_Offset    => 0,
                Str         =>
                  Img (Synth.Synth_CPU_Load
                    (Project.Voice_MIDI_Chan (Project.Mode (T)))));
      end if;

      B := 79;
      Print (X_Offset    => B,
             Y_Offset    => 0,
             Str         => 'P' & Img (Project.Editing_Pattern) &
                            "S" & Img (Project.Editing_Step));
      Menu.Drawing.Draw_Battery (Anim_Step);
      Screen.Draw_H_Line (0, Screen_Width - 1, 8);

      case WNM.UI.Input_GUI_Mode is
         when WNM.UI.Volume_BPM_Mute | WNM.UI.Volume_BPM_Solo =>

            WNM.GUI.Parameters.Print_Percentage
              (Slot  => WNM.GUI.Parameters.Up,
               Name  => "Volume",
               Value => Integer (WNM.Audio_Routing.Get_Main_Volume));

            WNM.GUI.Parameters.Print_Int
              (Slot  => WNM.GUI.Parameters.Down,
               Name  => "BPM",
               Value => Integer (WNM.Project.Get_BPM),
               Min   => Integer (Beat_Per_Minute'First),
               Max   => Integer (Beat_Per_Minute'Last));

            case WNM.UI.Input_GUI_Mode is
               when WNM.UI.Volume_BPM_Mute =>
                  WNM.GUI.Menu.Drawing.Draw_Value ("Mute");
               when WNM.UI.Volume_BPM_Solo =>
                  WNM.GUI.Menu.Drawing.Draw_Value ("Solo");
               when others =>
                  null;
            end case;

         when WNM.UI.Pattern_Mode |
              WNM.UI.Track_Mode |
              WNM.UI.Step_Mode |
              WNM.UI.Song_Mode |
              WNM.UI.FX_Mode |
              WNM.UI.Sample_Edit_Mode =>

            Menu.Draw;

         when WNM.UI.FX_Alt =>
            B :=
              Box_Left + (Box_Right - Box_Left - 7 * Bitmap_Fonts.Width) / 2;

            Print (X_Offset => B,
                   Y_Offset => Box_Top - 5,
                   Str      => "FX/Copy");

            Draw_Chords_Progress (Screen_Width / 2, Box_Top + 15);

            Draw_Alt_Slider;

         when WNM.UI.Copy =>
            declare
               Blink : constant Boolean := (Anim_Step mod 10) < 5;
               FB : constant String := (if Blink then "  " else "??");
               TB : constant String :=
                 (if Is_Complete (WNM.UI.Copy_T.From) and then Blink
                  then "  " else "??");
            begin
               B := 1;
               Print (X_Offset    => B,
                      Y_Offset    => Box_Top,
                      Str         => "Copy " & WNM.UI.Copy_T.From.Kind'Img);
               B := 1;
               Print (X_Offset    => B,
                      Y_Offset    => Box_Top + 9,
                      Str         => "From " & Image (WNM.UI.Copy_T.From, FB));
               B := 1;
               Print (X_Offset    => B,
                      Y_Offset    => Box_Top + 18,
                      Str         => "To   " & Image (WNM.UI.Copy_T.To, TB));
            end;

         when WNM.UI.Step_Select |
              WNM.UI.Track_Select |
              WNM.UI.Pattern_Select |
              WNM.UI.Song_Select
            =>
            raise Program_Error with
              "These mode are not expected for the GUI";
      end case;

      WNM.GUI.Popup.Update;

      WNM.Screen.Update;

      Anim_Step := Anim_Step + 1;
   end Update;

end WNM.GUI.Update;
