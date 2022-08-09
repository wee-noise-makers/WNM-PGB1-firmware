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

with WNM.Sequencer;              use WNM.Sequencer;
with WNM.Synth;                  use WNM.Synth;
with WNM.Master_Volume;
with WNM.Pattern_Sequencer;
with WNM.GUI.Menu;
with WNM.GUI.Menu.Root;
with WNM.Buttons;
with WNM.LED;
with HAL; use HAL;

with WNM.GUI.Popup;

package body WNM.UI is

   procedure Signal_Event (B : Button; Evt : Buttton_Event);

   procedure Toggle_FX (B : Keyboard_Button);
   procedure Mute (Track : WNM.Tracks);
   procedure Unmute (Track : WNM.Tracks);
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
      if Current_Input_Mode in Pattern_Select | Track_Select | Step_Select then
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

   procedure Signal_Event (B : Button; Evt : Buttton_Event) is
   begin
      if GUI.Menu.In_Menu and then Evt = On_Press then
         if B = Encoder_L then
            GUI.Menu.On_Event ((Kind => GUI.Menu.Left_Press));
            return;
         end if;

         if B = Encoder_R then
            GUI.Menu.On_Event ((Kind => GUI.Menu.Right_Press));
            return;
         end if;
      end if;

      case Current_Input_Mode is

         when Main_Modes =>
            case Evt is
               when On_Press=>
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

                  when Func =>
                     --  Switch to Func mode
                     Current_Input_Mode := FX_Alt;

                  when Menu =>
                     GUI.Menu.Open (GUI.Menu.Main_Menu);

                  when Play =>
                     Sequencer.Play_Pause;

                  when Rec =>
                     if Current_Input_Mode = Pattern_Mode then
                        if Recording then
                           Pattern_Sequencer.End_Recording;
                        else
                           Pattern_Sequencer.Start_Recording;
                        end if;
                     end if;

                     Recording_On := not Recording_On;

                  when Keyboard_Button =>

                     Sequencer.On_Press (B, Current_Input_Mode);

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
                     Sequencer.On_Release (B, Current_Input_Mode);
                  when others => null;
                  end case;
               when others => null;
            end case;

         when Pattern_Select =>
            case B is
               when Keyboard_Button =>
                  Sequencer.Set_Editing_Pattern (To_Value (B));
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
                  Sequencer.Set_Editing_Track (To_Value (B));
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
                  Sequencer.Set_Editing_Step (To_Value (B));
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

         when FX_Alt =>
            case Evt is
               when On_Press =>
                  case B is
                     when Keyboard_Button =>
                        Toggle_FX (B);
                     when Pattern_Button =>
                        Copy_T := WNM.Sequence_Copy.Start_Copy_Pattern;
                        Current_Input_Mode := Copy;
                     when Track_Button =>
                        Copy_T := WNM.Sequence_Copy.Start_Copy_Track
                          (Sequencer.Editing_Pattern);

                        Current_Input_Mode := Copy;
                     when Step_Button =>
                        Copy_T := WNM.Sequence_Copy.Start_Copy_Step
                          (Sequencer.Editing_Pattern,
                           Sequencer.Editing_Track);

                        Current_Input_Mode := Copy;
                     when others =>
                        null;
                  end case;
               when On_Release =>
                  if B = Func then
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
                  WNM.Sequencer.Do_Copy (Copy_T);
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


         when others =>
            null;
      end case;

      --  case Current_Input_Mode is
      --
      --     when Pattern_Mode =>
      --        if B in B1 .. B16 and then Evt = On_Press then
      --           --  Play the pattern coresponding to the button
      --           Pattern_Sequencer.Add_To_Sequence (To_Value (B));
      --           Pattern_Sequencer.End_Sequence_Edit;
      --        elsif B = Rec and then Evt = On_Press then
      --           Current_Input_Mode := Pattern_Chaining;
      --        end if;
      --
      --     when Track_Mode =>
      --        null;
      --
      --     when Step_Mode =>
      --        null;
      --
      --  when Note =>
      --     case Evt is
      --        when On_Press =>
      --           case B is
      --              when Func =>
      --                 --  Switch to Func mode
      --                    Current_Input_Mode := FX_Alt;
      --              when Play =>
      --                 Sequencer.Play_Pause;
      --              when Rec =>
      --                 Sequencer.Rec_Pressed;
      --              when Keyboard_Button =>
      --                 if Sequencer.State
      --                   in Sequencer.Edit | Sequencer.Play_And_Edit
      --                 then
      --                    Editing_Step := To_Value (B);
      --                 end if;
      --
      --                 Sequencer.On_Press (B);
      --              when Step_Button =>
      --                 GUI.Menu.Open (GUI.Menu.Step_Menu);
      --                 Current_Input_Mode := Step_Select;
      --              when Track_Button =>
      --                 GUI.Menu.Open (GUI.Menu.Track_Menu);
      --                 Current_Input_Mode := Track_Select;
      --              when Pattern_Button =>
      --                 GUI.Menu.Open (GUI.Menu.Pattern_Menu);
      --                 Current_Input_Mode := Pattern_Mode;
      --              when Menu =>
      --                 GUI.Menu.Root.Push_Root_Window;
      --              when others => null;
      --           end case;
      --        when On_Long_Press =>
      --           case B is
      --              when Play =>
      --                 --  Switch to volume/BPM config mode
      --                 Current_Input_Mode := Volume_BPM;
      --              when Rec =>
      --                 --  Switch to squence edition mode
      --                 Sequencer.Rec_Long;
      --              when B1 .. B16 =>
      --
      --                 GUI.Menu.Open (GUI.Menu.Step_Menu);
      --                 Editing_Step := To_Value (B);
      --
      --              when Pattern_Button =>
      --                 GUI.Menu.Open (GUI.Menu.Pattern_Menu);
      --                 Current_Input_Mode := Pattern_Select;
      --              when others => null;
      --           end case;
      --        when On_Release =>
      --           case B is
      --              when Keyboard_Button =>
      --                 --  Release note or octave Up/Down
      --                 Sequencer.On_Release (B);
      --              when Rec =>
      --                 Sequencer.Rec_Release;
      --              when others => null;
      --           end case;
      --        when others => null;
      --     end case;
      --
      --     when Volume_BPM =>
      --        if B = Play and Evt = On_Release then
      --           Current_Input_Mode := Last_Main_Mode;
      --           WNM.Pattern_Sequencer.End_Sequence_Edit;
      --        end if;
      --
      --        if B in B1 .. B16 and Evt = On_Press then
      --           WNM.Synth.Toggle_Mute (To_Value (B));
      --        end if;
      --
      --     when FX_Alt =>
      --        case Evt is
      --           when On_Press =>
      --              case B is
      --                 when Keyboard_Button =>
      --                    Toggle_FX (B);
      --                 when Pattern_Button =>
      --                    Copy_T := WNM.Sequence_Copy.Start_Copy_Pattern;
      --                    Current_Input_Mode := Copy;
      --                 when Track_Button =>
      --                    Copy_T := WNM.Sequence_Copy.Start_Copy_Track
      --                      (WNM.Pattern_Sequencer.Current_Pattern);
      --
      --                    Current_Input_Mode := Copy;
      --                 when Step_Button =>
      --                    Copy_T := WNM.Sequence_Copy.Start_Copy_Step
      --                      (WNM.Pattern_Sequencer.Current_Pattern, Track);
      --
      --                    Current_Input_Mode := Copy;
      --                 when others =>
      --                    null;
      --              end case;
      --           when On_Release =>
      --              if B = Func then
      --                 Current_Input_Mode := Last_Main_Mode;
      --              end if;
      --           when others =>
      --              null;
      --        end case;
      --
      --     when Copy =>
      --        if Evt = On_Release and then B = Func then
      --           Current_Input_Mode := Last_Main_Mode;
      --        elsif Evt = On_Press then
      --           WNM.Sequence_Copy.Apply (Copy_T, B);
      --           if WNM.Sequence_Copy.Is_Complete (Copy_T) then
      --              WNM.GUI.Popup.Display ("  copied  ", 500);
      --              WNM.Sequencer.Do_Copy (Copy_T);
      --           end if;
      --        end if;
      --
      --     when Step_Select =>
      --        if B in B1 .. B16 and then Evt = On_Press then
      --           Editing_Step := To_Value (B);
      --        elsif B = Step_Button and then Evt = On_Release then
      --           Current_Input_Mode := Last_Main_Mode;
      --        end if;
      --
      --     when Track_Select =>
      --        if B in B1 .. B16 and then Evt = On_Press then
      --           Sequencer.Select_Track (To_Value (B));
      --        elsif B = Track_Button and then Evt = On_Release then
      --           Current_Input_Mode := Last_Main_Mode;
      --        end if;
      --
      --     when Pattern_Select =>
      --        if B in B1 .. B16 and then Evt = On_Press then
      --           Editing_Pattern := To_Value (B);
      --        elsif B = Rec and then Evt = On_Press then
      --           Current_Input_Mode := Pattern_Chaining;
      --        elsif B = Pattern_Button and then Evt = On_Release then
      --           Current_Input_Mode := Last_Main_Mode;
      --        end if;
      --
      --     when Pattern_Chaining =>
      --        if B in B1 .. B16 and then Evt = On_Press then
      --           Pattern_Sequencer.Add_To_Sequence (To_Value (B));
      --        elsif B = Pattern_Button and then Evt = On_Release then
      --           Pattern_Sequencer.End_Sequence_Edit;
      --           Current_Input_Mode := Last_Main_Mode;
      --        end if;
      --  end case;
   end Signal_Event;

   ---------------
   -- Toggle_FX --
   ---------------

   procedure Toggle_FX (B : Keyboard_Button) is
   begin
      FX_Is_On (B) := not FX_Is_On (B);
   end Toggle_FX;

   -----------------------------
   -- Current_Editing_Pattern --
   -----------------------------

   function Current_Editing_Pattern return Patterns
   is (Editing_Pattern);

   ---------------------------
   -- Current_Editing_Trig --
   ---------------------------

   function Current_Editing_Trig return Sequencer_Steps
   is (Editing_Step);

   -----------
   -- FX_On --
   -----------

   function FX_On (B : Keyboard_Button) return Boolean
   is (FX_Is_On (B));

   --------------------
   -- Has_Long_Press --
   --------------------

   function Has_Long_Press (B : Button) return Boolean is
   begin
      return (case B is
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
              when Menu           => False,
              when Encoder_L      => True,
              when Encoder_R      => True);
   end Has_Long_Press;


   Last_State    : array (Button) of Buttons.Raw_Button_State := (others => WNM.Buttons.Up);
   Pressed_Since : array (Button) of WNM.Time.Time_Microseconds := (others => 0);
   Last_Event    : array (Button) of Buttton_Event := (others => On_Release);
   Next_Start    : Time.Time_Microseconds := Time.Time_Microseconds'First;

   ------------
   -- Update --
   ------------

   function Update return Time.Time_Microseconds is
      use Buttons;

      L_Enco : Integer;
      R_Enco : Integer;

      Now : constant Time.Time_Microseconds := Time.Clock;
   begin
      if Now < Next_Start then
         return Next_Start;
      end if;

      Next_Start := Next_Start + UI_Task_Period_Microseconds;

      Buttons.Scan;

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
                Pressed_Since (B) + Long_Press_Time_Span_Microseconds < Now
            then
               Last_Event (B) := On_Long_Press;
               Signal_Event (B, Last_Event (B));
            end if;

         elsif State (B) = Down then
            --  Button was justed pressed

            if Has_Long_Press (B) then
               --  If this button has long press event we don't signal the
               --  On_Press right now, but we record the time at wich it was
               --  pressed.

               Last_Event (B) := Waiting_For_Long_Press;
               Pressed_Since (B) := Now;
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

      --------------
      -- Encoders --
      --------------

      L_Enco := WNM.Buttons.Left_Diff;
      R_Enco := WNM.Buttons.Right_Diff;

      case Current_Input_Mode is
         when Volume_BPM_Mute | Volume_BPM_Solo =>
            WNM.Sequencer.Change_BPM (R_Enco);
            WNM.Master_Volume.Change (L_Enco);
         when others =>
            if L_Enco /= 0 then
               GUI.Menu.On_Event ((Kind  => GUI.Menu.Encoder_Left,
                                   Value => L_Enco));
            end if;
            if R_Enco /= 0 then
            GUI.Menu.On_Event ((Kind  => GUI.Menu.Encoder_Right,
                                Value => R_Enco));
            end if;
      end case;

      --------------
      -- Set LEDs --
      --------------

      LED.Turn_Off_All;

      -- Rec LED --
      if Recording then
         LED.Turn_On (Rec);
      end if;

      -- Play LED --
      if Pattern_Sequencer.Playing then
         LED.Turn_On (Play);
         if Sequencer.Playing_Step in 1 | 5 | 9 | 13 then
            LED.Turn_On (Play);
         end if;
      end if;

      --  B1 .. B16 LEDs --
      case Current_Input_Mode is

         -- FX selection mode --
         when FX_Alt =>
            --  The FX LED will be on if there's at least one FX enabled

            for B in B1 .. B16 loop
               if FX_Is_On (B) then
                  LED.Turn_On (B);
               end if;
            end loop;

            -- Step select mode --
         when Step_Select =>
            for B in B1 .. B16 loop
               if Editing_Step = To_Value (B) then
                  LED.Turn_On (B);
               end if;
            end loop;

            -- Track assign mode --
         when Track_Select =>
            for B in B1 .. B16 loop
               if Editing_Track = To_Value (B) then
                  LED.Turn_On (B);
               end if;
            end loop;

            --  Pattern select --
         when Pattern_Select =>
            for B in B1 .. B16 loop
               if Editing_Pattern = To_Value (B) then
                  LED.Turn_On (B);
               end if;
            end loop;

         --  Volume and BPM mode --
         when Volume_BPM_Mute | Volume_BPM_Solo =>
            if Solo_Mode_Enabled then
               LED.Turn_On (To_Button (Solo));
            else
               for B in B1 .. B16 loop
                  if not Muted (To_Value (B)) then
                     LED.Turn_On (B);
                  end if;
               end loop;
            end if;

         when Pattern_Chaining =>
            for B in B1 .. B16 loop
               if Pattern_Sequencer.Playing_Pattern = To_Value (B) then
                  LED.Turn_On (B);
               end if;
               if Pattern_Sequencer.Is_In_Pattern_Sequence (To_Value (B)) then
                  LED.Turn_On (B);
               end if;
            end loop;

         when others =>
            case Last_Main_Mode is
            when Pattern_Mode =>
               for B in B1 .. B16 loop
                  if Pattern_Sequencer.Is_In_Pattern_Sequence (To_Value (B)) then
                     LED.Turn_On (B);
                  end if;
               end loop;

               --  Blinking playing pattern
               if Pattern_Sequencer.Playing then
                  if Sequencer.Playing_Step in 1 | 5 | 9 | 13 then
                     LED.Turn_On
                       (To_Button (Pattern_Sequencer.Playing_Pattern));
                  else
                     LED.Turn_Off
                       (To_Button (Pattern_Sequencer.Playing_Pattern));
                  end if;
               end if;

            when Track_Mode | Step_Mode =>
               if Pattern_Sequencer.Playing  then
                  LED.Turn_On (To_Button (Sequencer.Playing_Step));
               end if;

               if Last_Main_Mode = Step_Mode or else Recording then
                  for B in Keyboard_Button loop
                     if Sequencer.Set (To_Value (B)) then
                        LED.Turn_On (B);
                     end if;
                  end loop;
               else
                  for B in Keyboard_Button loop
                     if Sequencer.Set (To_Value (B), Sequencer.Playing_Step) then
                        LED.Turn_On (B);
                     end if;
                  end loop;
               end if;

            end case;
      end case;

      return Next_Start;
   end Update;

   ----------
   -- Mute --
   ----------

   procedure Mute (Track : WNM.Tracks) is
   begin
      Track_Muted (Track) := True;
   end Mute;

   ------------
   -- Unmute --
   ------------

   procedure Unmute (Track : WNM.Tracks) is
   begin
      Track_Muted (Track) := False;
   end Unmute;

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
