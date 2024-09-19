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
with WNM.Project.Song_Part_Sequencer;
with WNM.MIDI_Clock; use WNM.MIDI_Clock;
with WNM.GUI.Menu;
with WNM.GUI.Menu.Root;
with WNM.LEDs;
with WNM.Time;
with WNM.UI.Logs;
with WNM.Audio_Routing;

with HAL; use HAL;

with WNM.GUI.Popup;

package body WNM.UI is

   procedure Signal_Event (B : Button; Evt : Button_Event);

   procedure Toggle_FX (B : Keyboard_Button);
   procedure Toggle_Mute (Track : WNM.Tracks);
   procedure Toggle_Solo (Track : WNM.Tracks);
   function In_Solo return Boolean;
   function Solo return WNM.Tracks;

   FX_Is_On : array (Keyboard_Button) of Boolean := (others => False);
   Last_Main_Mode : Main_Modes := Track_Mode;
   Current_Input_Mode : Input_Mode_Type := Last_Main_Mode;

   Select_Done : Boolean := False;

   Recording_On : Boolean := False;

   Track_Muted : array (WNM.Tracks) of Boolean := (others => False);
   Solo_Mode_Enabled : Boolean := False;
   Solo_Track : WNM.Tracks := 1;

   Play_Released : Boolean := False;

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
      if Current_Input_Mode in Pattern_Select |
                               Track_Select |
                               Step_Select |
                               Song_Select
      then
         return Last_Main_Mode;
      else
         return Current_Input_Mode;
      end if;
   end Input_GUI_Mode;

   function Recording return Boolean
   is (Recording_On);

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
            when Volume_BPM_Mute | Volume_BPM_Solo =>
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
                  when Pattern_Button =>
                     Current_Input_Mode := Pattern_Select;
                     Select_Done := False;
                  when Track_Button =>
                     Current_Input_Mode := Track_Select;
                     Select_Done := False;
                  when Step_Button =>
                     Current_Input_Mode := Step_Select;
                     Select_Done := False;
                  when Song_Button =>
                     Current_Input_Mode := Song_Select;
                     Select_Done := False;

                  when Func =>
                     --  Switch to Func mode
                     Current_Input_Mode := FX_Alt;
                     Select_Done := False;

                  when Menu =>
                     GUI.Menu.Root.Push_Root_Window;

                  when Play =>
                     Project.Step_Sequencer.Play_Pause;

                  when Rec =>
                     case Current_Input_Mode is
                        when others =>
                           null;
                     end case;

                     Recording_On := not Recording_On;

                  when Keyboard_Button =>

                     Project.Step_Sequencer.On_Press (B, Current_Input_Mode);

                  when others =>
                     null;
                  end case;
               when On_Long_Press =>
                  case B is
                  when Play =>
                     --  Switch to volume/BPM config mode
                     if Solo_Mode_Enabled then
                        Current_Input_Mode := Volume_BPM_Solo;
                     else
                        Current_Input_Mode := Volume_BPM_Mute;
                     end if;

                     --  when Rec =>
                  --     --  Switch to squence edition mode
                  --     Sequencer.Rec_Long;
                  --  when B1 .. B16 =>
                  --
                  --     GUI.Menu.Open (GUI.Menu.Step_Menu);
                  --     Editing_Step := To_Value (B);
                  --
                  --  when Pattern_Button =>
                  --     GUI.Menu.Open (GUI.Menu.Pattern_Menu);
                  --     Current_Input_Mode := Pattern_Select;
                  when others => null;
                  end case;
               when On_Release =>
                  case B is
                  when Keyboard_Button =>
                     Project.Step_Sequencer.On_Release (B, Current_Input_Mode);
                  when others => null;
                  end case;
               when others => null;
            end case;

         when Pattern_Select =>
            case B is
               when Keyboard_Button =>
                  Project.Editing_Pattern := To_Value (B);
                  Select_Done := True;

               when Pattern_Button =>
                  if Evt = On_Release then
                     if Select_Done then
                        --  Go back a main mode
                        Current_Input_Mode := Last_Main_Mode;

                     else
                        --  Switch to pattern mode
                        Current_Input_Mode := Pattern_Mode;
                        GUI.Menu.Open (GUI.Menu.Pattern_Menu);
                        Last_Main_Mode := Current_Input_Mode;

                        --  Switching mode disables recording.
                        --  TODO: Is that a good thing?
                        Recording_On := False;
                     end if;
                  end if;
               when others => null;
            end case;

         when Track_Select =>
            case B is
               when Keyboard_Button =>
                  Project.Editing_Track := To_Value (B);
                  Select_Done := True;

               when Track_Button =>
                  if Evt = On_Release then
                     if Select_Done then
                        --  Go back a main mode
                        Current_Input_Mode := Last_Main_Mode;

                     else
                        --  Switch to track mode
                        Current_Input_Mode := Track_Mode;
                        GUI.Menu.Open (GUI.Menu.Track_Menu);
                        Last_Main_Mode := Current_Input_Mode;

                        --  Switching mode disables recording.
                        --  TODO: Is that a good thing?
                        Recording_On := False;
                     end if;
                  end if;
               when others => null;
            end case;

         when Step_Select =>
            case B is
               when Keyboard_Button =>
                  Project.Editing_Step := To_Value (B);
                  Select_Done := True;

               when Step_Button =>
                  if Evt = On_Release then
                     if Select_Done then
                        --  Go back a main mode
                        Current_Input_Mode := Last_Main_Mode;

                     else
                        --  Switch to step mode
                        Current_Input_Mode := Step_Mode;
                        GUI.Menu.Open (GUI.Menu.Step_Menu);
                        Last_Main_Mode := Current_Input_Mode;

                        --  Switching mode disables recording.
                        --  TODO: Is that a good thing?
                        Recording_On := False;
                     end if;
                  end if;
               when others => null;
            end case;

         when Song_Select =>
            case B is
               when Keyboard_Button =>
                  Project.Editing_Song_Elt := To_Value (B);
                  Select_Done := True;

               when Song_Button =>
                  if Evt = On_Release then
                     if Select_Done then
                        --  Go back a main mode
                        Current_Input_Mode := Last_Main_Mode;

                     else
                        --  Switch to song mode
                        Current_Input_Mode := Song_Mode;
                        GUI.Menu.Open (GUI.Menu.Chord_Menu);
                        Last_Main_Mode := Current_Input_Mode;

                        --  Switching mode disables recording.
                        --  TODO: Is that a good thing?
                        Recording_On := False;
                     end if;
                  end if;
               when others => null;
            end case;

         when FX_Alt =>
            case Evt is
               when On_Press =>
                  case B is
                     when Keyboard_Button =>
                        Toggle_FX (B);
                        Select_Done := True;

                     when Pattern_Button =>
                        Copy_T := WNM.Sequence_Copy.Start_Copy_Pattern
                          (Project.Editing_Track);
                        Current_Input_Mode := Copy;
                        Select_Done := True;

                     when Track_Button =>
                        Copy_T := WNM.Sequence_Copy.Start_Copy_Track;

                        Current_Input_Mode := Copy;
                        Select_Done := True;

                     when Step_Button =>
                        Copy_T := WNM.Sequence_Copy.Start_Copy_Step
                          (Project.Editing_Track,
                           Project.Editing_Pattern);

                        Current_Input_Mode := Copy;
                        Select_Done := True;

                     when Rec =>
                        --  Switch to sample edit mode
                        Current_Input_Mode := Sample_Edit_Mode;
                        GUI.Menu.Open (GUI.Menu.Sample_Edit_Menu);
                        Last_Main_Mode := Current_Input_Mode;

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

         when Copy =>
            if Evt = On_Release and then B = Func then
               Current_Input_Mode := Last_Main_Mode;
            elsif Evt = On_Press then
               WNM.Sequence_Copy.Apply (Copy_T, B);
               if WNM.Sequence_Copy.Is_Complete (Copy_T) then
                  WNM.Project.Do_Copy (Copy_T);
                  WNM.GUI.Popup.Display ("     copied     ", 500_000);
               end if;
            end if;

         when Volume_BPM_Mute | Volume_BPM_Solo =>

            if B = Play and then Evt = On_Release then
               Current_Input_Mode := Last_Main_Mode;
            end if;

            if Current_Input_Mode = Volume_BPM_Mute then

               if B in Keyboard_Button and then Evt = On_Press then
                  Toggle_Mute (To_Value (B));
               end if;

               if B = Track_Button and then Evt = On_Press then
                  Current_Input_Mode := Volume_BPM_Solo;
               end if;

            else

               if B = Track_Button and then Evt = On_Press then
                  Current_Input_Mode := Volume_BPM_Mute;
                  Solo_Mode_Enabled := False;
               end if;

               if B in Keyboard_Button and then Evt = On_Press then
                  Toggle_Solo (To_Value (B));

                  if not Solo_Mode_Enabled then
                     --  We disabled solo so go back to mute mode
                     Current_Input_Mode := Volume_BPM_Mute;
                  end if;
               end if;
            end if;
      end case;

   end Signal_Event;

   ---------------
   -- Toggle_FX --
   ---------------

   procedure Toggle_FX (B : Keyboard_Button) is
   begin

      if B in B2 .. B5 then
         declare
            use Project;
            Kind : constant Roll_Kind :=
              (case B is
                  when B2 => Eighth,
                  when B3 => Quarter,
                  when B4 => Half,
                  when B5 => Beat,
                  when others => Off);

         begin
            if Kind = Roll_State then
               Roll (Project.Off);
            else
               Roll (Kind);
            end if;

            for X in B2 .. B5 loop
               FX_Is_On (X) := False;
            end loop;
            case Roll_State is
               when Off => null;
               when Beat => FX_Is_On (B5) := True;
               when Half => FX_Is_On (B4) := True;
               when Quarter => FX_Is_On (B3) := True;
               when Eighth => FX_Is_On (B2) := True;
            end case;
         end;

      else
         FX_Is_On (B) := not FX_Is_On (B);
      end if;
   end Toggle_FX;

   -----------
   -- FX_On --
   -----------

   function FX_On (B : Keyboard_Button) return Boolean
   is (FX_Is_On (B));

   --------------------
   -- Has_Long_Press --
   --------------------

   function Has_Long_Press (B : Button) return Boolean
   is (case B is
          when B1             => False,
          when B2             => False,
          when B3             => False,
          when B4             => False,
          when B5             => False,
          when B6             => False,
          when B7             => False,
          when B8             => False,
          when B9             => False,
          when B10            => False,
          when B11            => False,
          when B12            => False,
          when B13            => False,
          when B14            => False,
          when B15            => False,
          when B16            => False,
          when Rec            => False,
          when Play           => True,
          when Func           => False,
          when Step_Button    => False,
          when Track_Button   => False,
          when Pattern_Button => False,
          when Song_Button   => False,
          when Menu           => False,
          when PAD_Up         => True,
          when PAD_Down       => True,
          when PAD_Left       => True,
          when PAD_Right      => True,
          when PAD_A          => False,
          when PAD_B          => False);

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

      if not Play_Released then
         --  The device starts when holding the play button down. We wait for
         --  the user to release this button before doing anything.
         if State (Play) = Up then
            Play_Released := True;
         else
            return;
         end if;
      end if;

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
               when Volume_BPM_Mute | Volume_BPM_Solo =>
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
        WNM.MIDI_Clock.Step in 1 .. 6 | 24 .. 30;
   begin
      LEDs.Turn_Off_All;

      -- Rec LED --
      if Recording then
         LEDs.Turn_On (Rec, LEDs.Recording);
      end if;

      -- Play LED --
      if WNM.MIDI_Clock.Running  then
         --  LEDs.Turn_On (Play);

         if Beat_Step then
            LEDs.Turn_On (Play, LEDs.Play);
         end if;
      end if;

      --  The FX LED is on if there's at least one FX enabled
      LEDs.Set_Hue (LEDs.FX);
      if (for some B in Keyboard_Button => FX_Is_On (B)) then
         LEDs.Turn_On (Func);
      end if;

      --  B1 .. B16 LEDs --
      case Current_Input_Mode is

         -- FX selection mode --
         when FX_Alt =>

            LEDs.Set_Hue (LEDs.FX);
            for B in Keyboard_Button loop
               if FX_Is_On (B) then
                  LEDs.Turn_On (B);
               end if;
            end loop;

            -- Step select mode --
         when Step_Select =>
            LEDs.Set_Hue (LEDs.Step);
            LEDs.Turn_On (To_Button (Project.Editing_Step));

            -- Track assign mode --
         when Track_Select =>
            LEDs.Set_Hue (LEDs.Track);
            LEDs.Turn_On (To_Button (Project.Editing_Track));

            --  Pattern select --
         when Pattern_Select =>
            LEDs.Set_Hue (LEDs.Pattern);
            LEDs.Turn_On (To_Button (Project.Editing_Pattern));

            --  Song Part select --
         when Song_Select =>
            case Project.Editing_Song_Elt is
               when Parts =>
                  LEDs.Set_Hue (LEDs.Part);
               when Chord_Progressions =>
                  LEDs.Set_Hue (LEDs.Chord);
            end case;
            LEDs.Turn_On (To_Button (Project.Editing_Song_Elt));

            --  Volume and BPM mode --
         when Volume_BPM_Mute | Volume_BPM_Solo =>

            LEDs.Set_Hue (LEDs.Track);

            if Solo_Mode_Enabled then
               LEDs.Turn_On (Track_Button);
               LEDs.Turn_On (To_Button (Solo));
            else
               for B in B1 .. B16 loop
                  if not Muted (To_Value (B)) then
                     LEDs.Turn_On (B);
                  end if;
               end loop;
            end if;

         when others =>
            case Last_Main_Mode is
            when Pattern_Mode =>
               LEDs.Set_Hue (LEDs.Pattern);
               LEDs.Turn_On (Pattern_Button);
               LEDs.Turn_On (To_Button (Project.Editing_Pattern));

            when Song_Mode =>
               case Project.Editing_Song_Elt is
               when Parts =>
                  LEDs.Set_Hue (LEDs.Part);
               when Chord_Progressions =>
                  LEDs.Set_Hue (LEDs.Chord);
               end case;

               LEDs.Turn_On (Song_Button);
               LEDs.Turn_On (To_Button (Project.Editing_Song_Elt));

               --  Blinking playing Chord
               if WNM.MIDI_Clock.Running then
                  if Beat_Step then
                     LEDs.Turn_On
                       (To_Button (Project.Song_Part_Sequencer.Playing),
                        LEDs.Violet);
                  end if;
               end if;

            when Track_Mode =>

               LEDs.Set_Hue (LEDs.Track);
               LEDs.Turn_On (Track_Button);

               --  Playing step
               --  if WNM.MIDI_Clock.Running then
               --     LEDs.Turn_On
               --       (To_Button (Project.Step_Sequencer.Playing_Step),
               --        LEDs.Play);
               --  end if;

               if Recording then

                  --  Active steps in edit mode
                  LEDs.Set_Hue (LEDs.Recording);
                  for B in Keyboard_Button loop
                     if Project.Set (Step => To_Value (B)) then
                        LEDs.Turn_On (B);
                     end if;
                  end loop;
               else

                  for B in Keyboard_Button loop
                     declare
                        Track : constant Tracks := To_Value (B);
                        PH    : constant Project.Playhead :=
                          Project.Step_Sequencer.Playing_Step (Track);
                     begin
                        if Project.Set (Track, PH) then
                           LEDs.Turn_On (B);
                        end if;
                     end;
                  end loop;
               end if;

            when Step_Mode =>
               LEDs.Set_Hue (LEDs.Step);
               LEDs.Turn_On (Step_Button);

               if Recording then
                  --  Red means editing
                  LEDs.Set_Hue (LEDs.Recording);
               end if;

               --  Active steps
               for B in Keyboard_Button loop
                  if Project.Set (Step => To_Value (B)) then
                     LEDs.Turn_On (B);
                  end if;
               end loop;

            when FX_Mode =>
               LEDs.Set_Hue (LEDs.Violet);

               LEDs.Turn_On (Func);
               null;

            when Sample_Edit_Mode =>

               declare
                  use type WNM.Project.Octave_Offset;
                  Octave : constant WNM.Project.Octave_Offset :=
                    WNM.Project.Step_Sequencer.Keyboard_Octave;

                  Oct_Hue : constant WNM.LEDs.Hue :=
                    (case abs Octave is
                        when      1 => LEDs.Spring_Green,
                        when      2 => LEDs.Cyan,
                        when      3 => LEDs.Azure,
                        when      4 => LEDs.Blue,
                        when      5 => LEDs.Violet,
                        when      6 => LEDs.Magenta,
                        when      7 => LEDs.Rose,
                        when others => LEDs.Red);
               begin
                  LEDs.Set_Hue (Oct_Hue);
                  if Octave < 0 then
                     LEDs.Turn_On (B1);
                  elsif Octave > 0 then
                     LEDs.Turn_On (B8);
                  end if;
               end;

               LEDs.Set_Hue (LEDs.Yellow);

               for B in B9 .. B16 loop
                  LEDs.Turn_On (B);
               end loop;
               LEDs.Turn_On (B2);
               LEDs.Turn_On (B3);
               LEDs.Turn_On (B5);
               LEDs.Turn_On (B6);
               LEDs.Turn_On (B7);
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
   is (if In_Solo
       then Solo /= Track
       else Track_Muted (Track));

   -----------------
   -- Toggle_Solo --
   -----------------

   procedure Toggle_Solo (Track : WNM.Tracks) is
   begin
      if Solo_Mode_Enabled then
         if Solo_Track = Track then
            Solo_Mode_Enabled := False;
         else
            Solo_Track := Track;
         end if;
      else
         Solo_Mode_Enabled := True;
         Solo_Track := Track;
      end if;
   end Toggle_Solo;

   -------------
   -- In_Solo --
   -------------

   function In_Solo return Boolean
   is (Solo_Mode_Enabled);

   ----------
   -- Solo --
   ----------

   function Solo return WNM.Tracks
   is (Solo_Track);

end WNM.UI;
