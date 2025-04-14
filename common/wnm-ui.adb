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

with WNM.Project;
with WNM.Project.Step_Sequencer;
with WNM.MIDI_Clock; use WNM.MIDI_Clock;
with WNM.GUI.Menu;
with WNM.GUI.Menu.Root;
with WNM.GUI.Menu.Drum_Settings;
with WNM.LEDs;
with WNM.Time;
with WNM.UI.Logs;
with WNM.Audio_Routing;
with WNM.Voices.Auto_Filter_FX;
with WNM.Voices.Stutter_FX;
with WNM.Mixer;

with HAL; use HAL;

package body WNM.UI is

   procedure Signal_Event (B : Button; Evt : Button_Event);

   procedure Toggle_FX (B : Keyboard_Button);
   procedure Toggle_Mute (Track : WNM.Tracks);

   Last_Main_Mode : Main_Modes := Chord_Mode;
   Current_Input_Mode : Input_Mode_Type := Last_Main_Mode;

   Track_Muted : array (WNM.Tracks) of Boolean := (others => False);

   Anim_Step : UInt32 := 0;

   ----------------
   -- Input_Mode --
   ----------------

   function Input_Mode return Input_Mode_Type is
   begin
      return Current_Input_Mode;
   end Input_Mode;

   --------------------
   -- Input_GUI_Mode --
   --------------------

   function Input_GUI_Mode return Input_Mode_Type is
   begin
      return Current_Input_Mode;
   end Input_GUI_Mode;

   ------------------
   -- Signal_Event --
   ------------------

   procedure Signal_Event (B : Button; Evt : Button_Event) is
   begin
      WNM.UI.Logs.Log_Button_Event (B, Evt);

      if B in PAD_Up | PAD_Down | PAD_Left | PAD_Right | PAD_A | PAD_B
        and then
          Evt = On_Press
      then
         case Current_Input_Mode is
            when Volume_BPM_Mute =>
               case B is
                  when PAD_Up =>
                     WNM.Audio_Routing.Change_Main_Volume (1);
                  when PAD_Down =>
                     WNM.Audio_Routing.Change_Main_Volume (-1);
                  when PAD_Left =>
                     WNM.Project.Change_BPM (-1);
                  when PAD_Right =>
                     WNM.Project.Change_BPM (1);
                  when others =>
                     null;
               end case;

            when FX_Alt =>
               case B is
                  when PAD_Up =>
                     Project.Alt_Slider_Target_Next;
                  when PAD_Down =>
                     Project.Alt_Slider_Target_Prev;
                  when PAD_Left =>
                     Project.Alt_Slider_Track_Prev;
                  when PAD_Right =>
                     Project.Alt_Slider_Track_Next;
                  when others =>
                     null;
               end case;

            when others =>
               declare
                  Kind : constant GUI.Menu.Menu_Event_Kind :=
                    (case B is
                        when PAD_Up => GUI.Menu.Up_Press,
                        when PAD_Down => GUI.Menu.Down_Press,
                        when PAD_Left => GUI.Menu.Left_Press,
                        when PAD_Right => GUI.Menu.Right_Press,
                        when PAD_A => GUI.Menu.A_Press,
                        when PAD_B => GUI.Menu.B_Press,
                        when others => raise Program_Error);
                  Menu_Evt : GUI.Menu.Menu_Event (Kind => Kind);
               begin
                  GUI.Menu.On_Event (Menu_Evt);
               end;
         end case;
         return;
      end if;

      case Current_Input_Mode is

         when Main_Modes =>
            case Evt is
               when On_Press =>
                  case B is
                  when Track_Button =>
                     Current_Input_Mode := Track_Select;

                  when Func =>
                     --  Switch to Func mode
                     Current_Input_Mode := FX_Alt;

                  when Menu =>
                     GUI.Menu.Root.Push_Root_Window;

                  when Drum_Edit =>
                     GUI.Menu.Drum_Settings.Push_Window;

                  when Drums_Play =>
                     Project.Step_Sequencer.Drums_Play_Stop;

                  when Play_Button =>
                     Project.Step_Sequencer.Play_Pause;

                  when Chord_Button =>
                     Project.Editing_Chord := B;
                     Project.Step_Sequencer.On_Press (B, Current_Input_Mode);

                  when Lead_Button =>
                     Project.Editing_Lead := B;
                     Project.Step_Sequencer.On_Press (B, Current_Input_Mode);

                  when Chord_Alt =>

                     Current_Input_Mode := Chord_Mode;
                     GUI.Menu.Open (GUI.Menu.Chord_Menu);
                     Last_Main_Mode := Current_Input_Mode;

                  when Lead_Alt =>
                     Current_Input_Mode := Lead_Mode;
                     GUI.Menu.Open (GUI.Menu.Lead_Menu);
                     Last_Main_Mode := Current_Input_Mode;

                  when others =>
                     null;
                  end case;
               when On_Long_Press =>
                  case B is
                  when Play_Button =>
                     --  Switch to volume/BPM config mode
                     Current_Input_Mode := Volume_BPM_Mute;

                     when others => null;
                  end case;
               when On_Release =>
                  case B is
                  when Chord_Button =>
                     Project.Step_Sequencer.On_Release (B,
                                                        Current_Input_Mode);
                  when Lead_Button =>

                     Project.Step_Sequencer.On_Release (B,
                                                        Current_Input_Mode);

                  when others => null;
                  end case;
               when others => null;
            end case;

         when Track_Select =>
            case B is
               when Keyboard_Button =>
                  if B in C1 .. C4 | C5 .. C7 | L1 .. L4 then
                     Project.Editing_Track := To_Value (B);
                  end if;

               when Track_Button =>
                  if Evt = On_Release then
                     Current_Input_Mode := Track_Settings_Mode;
                     GUI.Menu.Open (GUI.Menu.Track_Settings_Menu);
                  end if;
               when others => null;
            end case;
         when FX_Alt =>
            case Evt is
               when On_Press =>
                  case B is
                     when Keyboard_Button =>
                        Toggle_FX (B);
                        --  Select_Done := True;

                     when others =>
                        null;

                  end case;
               when On_Release =>
                  if B = Func then
                     --  Go back a main mode
                     Current_Input_Mode := Last_Main_Mode;
                  end if;
               when others =>
                  null;
            end case;

         when Volume_BPM_Mute =>

            if B = Play_Button and then Evt = On_Release then
               Current_Input_Mode := Last_Main_Mode;
            end if;

            if B in Keyboard_Button and then Evt = On_Press then
               Toggle_Mute (To_Value (B));
            end if;

      end case;

   end Signal_Event;

   ---------------
   -- Toggle_FX --
   ---------------

   procedure Toggle_FX (B : Keyboard_Button) is
   begin

      case B is
         when C1 .. C4 =>
            declare
               use Project;
               Kind : constant Roll_Kind :=
                 (case B is
                     when C1     => Eighth,
                     when C2     => Quarter,
                     when C3     => Half,
                     when C4     => Beat,
                     when others => Off);

            begin
               if Kind = Roll_State then
                  Roll (Project.Off);
               else
                  Roll (Kind);
               end if;
            end;

         when C5 =>
            Project.Step_Fill_Toogle;

         when C6 .. C8 =>
            declare
               use Project;
               Kind : constant Auto_Fill_Kind :=
                 (case B is
                     when C6    => Auto_Low,
                     when C7    => Auto_High,
                     when C8    => Auto_Buildup,
                     when others => Off);

            begin
               if Kind = Auto_Fill_State then
                  Auto_Fill (Project.Off);
               else
                  Auto_Fill (Kind);
               end if;
            end;

         when L1 .. L3 | L5 .. L7 =>
            declare
               use Voices.Auto_Filter_FX;
               Kind : constant Mode_Kind :=
                 (case B is
                     when L1 => Fix_Low_Pass,
                     when L2 => Fix_Band_Pass,
                     when L3 => Fix_High_Pass,
                     when L5 => Sweep_Low_Pass,
                     when L6 => Sweep_Band_Pass,
                     when L7 => Sweep_High_Pass,
                     when others => Off);

            begin
               if Kind = Mixer.Auto_Filter_Mode then
                  Mixer.Set_Auto_Filter (Off);
               else
                  Mixer.Set_Auto_Filter (Kind);
               end if;
            end;

         when L4 | L8 =>
            declare
               use Voices.Stutter_FX;
               Kind : constant Mode_Kind :=
                 (case B is
                     when L4  => On_Short,
                     when L8 => On_Trip,
                     when others => Off);

            begin
               if Kind = Mixer.Stutter_Mode then
                  Mixer.Set_Stutter (Off);
               else
                  Mixer.Set_Stutter (Kind);
               end if;
            end;
      end case;

   end Toggle_FX;

   --------------------
   -- Has_Long_Press --
   --------------------

   function Has_Long_Press (B : Button) return Boolean
   is (case B is
          when Play_Button     => True,
          when PAD_Up .. PAD_B => True,
          when others => False);

   function Has_Repeat_Press (B : Button) return Boolean
   is (case B is
          when PAD_Up .. PAD_Right => True,
          when others => False);

   Last_State    : WNM_HAL.Buttons_State := (others => Up);
   Long_Press_Deadline : array (Button) of WNM.Time.Time_Microseconds :=
     (others => WNM.Time.Time_Microseconds'Last);
   Last_Event    : array (Button) of Button_Event := (others => On_Release);

   ------------
   -- Update --
   ------------

   procedure Update is
      Now : constant Time.Time_Microseconds := Time.Clock;

      State : WNM_HAL.Buttons_State;
   begin
      State := WNM_HAL.State;

      --  Handle buttons
      for B in Button loop
         if Last_State (B) = State (B) then
            --  The button didn't change, let's check if we are waiting for
            --  a long press event.
            if Has_Long_Press (B)
              and then
                State (B) = Down
              and then
                Last_Event (B) = Waiting_For_Long_Press
              and then
                Long_Press_Deadline (B) < Now
            then
               if Has_Repeat_Press (B) then
                  Signal_Event (B, On_Press);
                  Long_Press_Deadline (B) :=
                    Now + Repeat_Press_Time_Span_Microseconds;
               else
                  Last_Event (B) := On_Long_Press;
                  Signal_Event (B, Last_Event (B));
               end if;
            end if;

         elsif State (B) = Down then
            --  Button was justed pressed

            if Has_Long_Press (B) then
               --  If this button has long press event we don't signal the
               --  On_Press right now, but we record the time at wich it was
               --  pressed.

               Last_Event (B) := Waiting_For_Long_Press;
               Long_Press_Deadline (B) :=
                 Now + Long_Press_Time_Span_Microseconds;
            else
               Last_Event (B) := On_Press;
               Signal_Event (B, Last_Event (B));
            end if;
         else
            --  Button was just released

            if Last_Event (B) = Waiting_For_Long_Press then
               --  The button was released before we reached the long press
               --  delay. It was not a long press after all so we first send
               --  The On_Press event and then the On_Realease.
               Signal_Event (B, On_Press);
            end if;

            Last_Event (B) := On_Release;
            Signal_Event (B, Last_Event (B));
         end if;

         Last_State (B) := State (B);
      end loop;

      declare
         TP : constant WNM_HAL.Touch_Data := WNM_HAL.Touch_Strip_State;
      begin
         if TP.Touch then
            case Current_Input_Mode is
               when Volume_BPM_Mute =>
                  WNM.Audio_Routing.Set_Main_Volume
                    (Audio_Volume (TP.Value * Float (Audio_Volume'Last)));
               when FX_Alt =>
                  Project.Alt_Slider_Set (TP.Value);
               when others =>
                  GUI.Menu.On_Event ((Kind => GUI.Menu.Slider_Touch,
                                      Slider_Value => TP.Value));
            end case;
         end if;
      end;
   end Update;

   -----------------
   -- Update_LEDs --
   -----------------

   procedure Update_LEDs is

      Beat_Step : constant Boolean :=
        WNM.MIDI_Clock.Step in 1 .. 12 | 24 .. 36;

      Select_Blink : constant Boolean := Anim_Step mod 6 < 2;
   begin
      Anim_Step := Anim_Step + 1;

      LEDs.Turn_Off_All;

      -- Play LED --
      if WNM.MIDI_Clock.Running and then Beat_Step then
         LEDs.Turn_On (Play_Button, LEDs.Play);
      end if;

      if WNM.Project.Step_Sequencer.Drums_On then
         LEDs.Turn_On (Drums_Play, LEDs.Dim_Green);
      end if;

      --  The FX LED is on if there's at least one FX enabled
      if Some_FX_On then
         LEDs.Turn_On (Func, LEDs.FX);
      end if;

      --  B1 .. B16 LEDs --
      case Current_Input_Mode is

         -- FX selection mode --
         when FX_Alt =>

            LEDs.Set_Hue (LEDs.FX);

            case Project.Roll_State is
               when Project.Off =>
                  null;
               when Project.Beat =>
                  LEDs.Turn_On (C4);
               when Project.Half =>
                  LEDs.Turn_On (C3);
               when Project.Quarter =>
                  LEDs.Turn_On (C2);
               when Project.Eighth =>
                  LEDs.Turn_On (C1);
            end case;

            if Project.Step_Fill then
               LEDs.Turn_On (C5);
            end if;

            case Project.Auto_Fill_State is
               when Project.Off =>
                  null;
               when Project.Auto_Low =>
                  LEDs.Turn_On (C6);
               when Project.Auto_High =>
                  LEDs.Turn_On (C7);
               when Project.Auto_Buildup =>
                  LEDs.Turn_On (C8);
            end case;

            case Mixer.Auto_Filter_Mode is
               when Voices.Auto_Filter_FX.Off =>
                  null;
               when Voices.Auto_Filter_FX.Fix_Low_Pass =>
                  LEDs.Turn_On (L1);
               when Voices.Auto_Filter_FX.Fix_Band_Pass =>
                  LEDs.Turn_On (L2);
               when Voices.Auto_Filter_FX.Fix_High_Pass =>
                  LEDs.Turn_On (L3);
               when Voices.Auto_Filter_FX.Sweep_Low_Pass =>
                  LEDs.Turn_On (L5);
               when Voices.Auto_Filter_FX.Sweep_Band_Pass =>
                  LEDs.Turn_On (L6);
               when Voices.Auto_Filter_FX.Sweep_High_Pass =>
                  LEDs.Turn_On (L7);
            end case;

            case Mixer.Stutter_Mode is
               when Voices.Stutter_FX.Off =>
                  null;
               when Voices.Stutter_FX.On_Short =>
                  LEDs.Turn_On (L4);
               when Voices.Stutter_FX.On_Trip =>
                  LEDs.Turn_On (L8);
            end case;

            --  Volume and BPM mode --
         when Volume_BPM_Mute =>

            LEDs.Set_Hue (LEDs.Track);

            for B in C1 .. C7 loop
               if not Muted (To_Value (B)) then
                  LEDs.Turn_On (B);
               end if;
            end loop;
            for B in L1 .. L4 loop
               if not Muted (To_Value (B)) then
                  LEDs.Turn_On (B);
               end if;
            end loop;

         when Track_Select =>
            LEDs.Set_Hue (LEDs.Track);
            LEDs.Turn_On (Track_Button);
            LEDs.Turn_On (To_Button (Keyboard_Value (Project.Editing_Track)));

         when others =>
            LEDs.Set_Hue (LEDs.Chord);
            for B in C1 .. C8 loop
               LEDs.Turn_On (B);
            end loop;

            LEDs.Set_Hue (LEDs.Step);
            for B in L1 .. L8 loop
               LEDs.Turn_On (B);
            end loop;

            case Last_Main_Mode is
               when Track_Settings_Mode =>
                  LEDs.Turn_On (Track_Button, LEDs.Chord);
               when Chord_Mode =>
                  LEDs.Turn_On (Chord_Alt, LEDs.Chord);
                  if Select_Blink then
                     LEDs.Turn_Off (Project.Editing_Chord);
                  end if;
               when Lead_Mode =>
                  LEDs.Turn_On (Lead_Alt, LEDs.Step);
                  if Select_Blink then
                     LEDs.Turn_Off (Project.Editing_Lead);
                  end if;
            end case;
      end case;

      WNM.LEDs.Update;
   end Update_LEDs;

   -----------------
   -- Toggle_Mute --
   -----------------

   procedure Toggle_Mute (Track : WNM.Tracks) is
   begin
      Track_Muted (Track) := not Track_Muted (Track);
   end Toggle_Mute;

   -----------
   -- Muted --
   -----------

   function Muted (Track : WNM.Tracks) return Boolean
   is (Track_Muted (Track));

   ----------------
   -- Some_FX_On --
   ----------------

   function Some_FX_On return Boolean is
      use type Project.Roll_Kind;
      use type Project.Auto_Fill_Kind;
      use type Voices.Auto_Filter_FX.Mode_Kind;
      use type Voices.Stutter_FX.Mode_Kind;
   begin
      return Project.Roll_State /= Project.Off
        or else
          Project.Auto_Fill_State /= Project.Off
          or else
            Project.Step_Fill
            or else
              Mixer.Auto_Filter_Mode /= Voices.Auto_Filter_FX.Off
              or else
                Mixer.Stutter_Mode /= Voices.Stutter_FX.Off;
   end Some_FX_On;

end WNM.UI;
