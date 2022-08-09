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
with WNM.Sequencer;         use WNM.Sequencer;
with WNM.Sequence_Copy;     use WNM.Sequence_Copy;
with WNM.Pattern_Sequencer; use WNM.Pattern_Sequencer;
with WNM.Master_Volume;
with WNM.GUI.Menu;
with WNM.GUI.Menu.Drawing;  use WNM.GUI.Menu.Drawing;
with WNM.GUI.Logo;
with WNM.GUI.Popup;
--  with WNM.Sample_Library;    use WNM.Sample_Library;


package body WNM.GUI.Update is

   Anim_Step : HAL.UInt32 := 0;

   Next_Start : Time.Time_Microseconds := Time.Time_Microseconds'First;

   function Header_Str return String is
      Result : String (1 .. 21) := "P00:T00:S00  P:00:S00";
   begin
      Result (2 .. 3) := Img (Sequencer.Editing_Pattern);
      Result (6 .. 7) := Img (Sequencer.Editing_Track);
      Result (10 .. 11) := Img (Sequencer.Editing_Step);

      Result (16 .. 17) := Img (Sequencer.Playing_Pattern);
      Result (20 .. 21) := Img (Sequencer.Playing_Step);
      return Result;
   end Header_Str;

   ------------
   -- Update --
   ------------

   function Update return Time.Time_Microseconds is
      B : Integer;

      BPM : Natural;
      Volume : Natural;
      Now : constant Time.Time_Microseconds := Time.Clock;
   begin
      if Now < Next_Start then
         return Next_Start;
      end if;

      Next_Start := Next_Start + GUI_Task_Period_Microseconds;

      WNM.Screen.Clear;

      --  Splash screen
      if WNM.Time.Clock < 1_000_000 then
         WNM.GUI.Logo.Draw_On_Screen (UInt2 (Anim_Step mod 4));
         WNM.Screen.Update;
         Anim_Step := Anim_Step + 1;
         return Next_Start;
      end if;

      -- Header --
      B := 1;
      Print (X_Offset    => B,
             Y_Offset    => 0,
             Str         => Header_Str);
      Screen.Draw_H_Line (8);

      case WNM.UI.Input_GUI_Mode is
         when WNM.UI.Volume_BPM_Mute | WNM.UI.Volume_BPM_Solo =>
            BPM := Integer (WNM.Sequencer.BPM);
            Volume := Integer (WNM.Master_Volume.Value);

            WNM.GUI.Parameters.Print_Percentage (Slot  => WNM.GUI.Parameters.Up,
                                                 Name  => "Volume",
                                                 Value => Volume);
            WNM.GUI.Parameters.Print_Int (Slot  => WNM.GUI.Parameters.Down,
                                          Name  => "BPM",
                                          Value => BPM,
                                          Min   => 50,
                                          Max   => 200);

            case WNM.UI.Input_GUI_Mode is
               when WNM.UI.Volume_BPM_Mute =>
                  WNM.GUI.Menu.Drawing.Draw_Value ("Mute");
               when WNM.UI.Volume_BPM_Solo =>
                  WNM.GUI.Menu.Drawing.Draw_Value ("Solo");
               when others =>
                  raise Program_Error;
            end case;

         --  when WNM.UI.Pattern_Mode =>
         --     B := 1;
         --     Print (X_Offset    => B,
         --            Y_Offset    => Box_Top,
         --            Str         => "Pattern Mode");
         --  when WNM.UI.Track_Mode =>
         --     B := 1;
         --     Print (X_Offset    => B,
         --            Y_Offset    => Box_Top,
         --            Str         => "Track Mode");
         --  when WNM.UI.Step_Mode =>
         --     B := 1;
         --     Print (X_Offset    => B,
         --            Y_Offset    => Box_Top,
         --            Str         => "Step Mode");

         when WNM.UI.Pattern_Chaining =>
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => Box_Top,
                   Str         => "Chain patterns");
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => Box_Top + 9,
                   Str         => "Current: " & Img (Sequencer.Playing_Pattern));
         when WNM.UI.Pattern_Mode | WNM.UI.Track_Mode | WNM.UI.Step_Mode =>
            Menu.Draw;
         when WNM.UI.FX_Alt =>
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => Box_Top,
                   Str         => "FX/Copy",
                   Invert_From => 0);
         when WNM.UI.Copy =>
            declare
               Blink : constant Boolean := (Anim_Step mod 10) < 5;
               FB : constant String := (if Blink then "  " else "??");
               TB : constant String :=
                 (if Is_Complete (WNM.UI.Copy_T.From) and then blink
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

         when WNM.UI.Step_Select | WNM.UI.Track_Select | WNM.UI.Pattern_Select  =>
            --  These mode are not expected for the GUI
            raise Program_Error;
      end case;

      WNM.GUI.Popup.Update;

      WNM.Screen.Update;

      Anim_Step := Anim_Step + 1;

      return Next_Start;
   end Update;

end WNM.GUI.Update;
