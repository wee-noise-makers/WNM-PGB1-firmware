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
with WNM.Synth;
with WNM.Sequencer;        use WNM.Sequencer;
with WNM.GUI.Popup;
with WNM.GUI.Menu.Text_Dialog;
with WNM.Arpeggiator;

package body WNM.GUI.Menu.Track_Settings is

   Track_Settings_Singleton : aliased Track_Settings_Menu;

   Settings_Count_Cache : array (Track_Mode_Kind) of Natural :=
     (others => 0);

   Settings_Position_Cache : array (Track_Mode_Kind, Settings) of Integer :=
     (others => (others => -1));

   procedure Fix_Current_Setting (This : in out Track_Settings_Menu) is
   begin
      if not Valid_Setting (This.Current_Setting, Mode (Editing_Track)) then
         Prev_Valid_Setting (This.Current_Setting, Mode (Editing_Track));
      end if;
   end Fix_Current_Setting;

   --------------------
   -- Settings_Count --
   --------------------

   function Settings_Count (M : Sequencer.Track_Mode_Kind) return Positive is
   begin
      if Settings_Count_Cache (M) = 0 then
         --  I don't think there is a way to know the number of settings per
         --  mode at compile time because it depends on the results of the
         --  Valid_Setting function. So we compute the numbers here and cache
         --  the result for futre calls.

         for S in Settings loop
            if Valid_Setting (S, M) then
               Settings_Count_Cache (M) := Settings_Count_Cache (M) + 1;
            end if;
         end loop;
      end if;

      return Settings_Count_Cache (M);
   end Settings_Count;

   ----------------------
   -- Setting_Position --
   ----------------------

   function Setting_Position (S : Settings;
                              M : Sequencer.Track_Mode_Kind)
                              return Natural
   is
   begin
      if Settings_Position_Cache (M, S) = -1 then
         --  Same as Settings_Count, we compute the numbers here and cache the
         --  result for futre calls.

         declare
            Cnt : Natural := 0;
         begin
            for S in Settings loop
               if Valid_Setting (S, M) then
                  Settings_Position_Cache (M, S) := Cnt;
                  Cnt := Cnt + 1;
               end if;
            end loop;
         end;
      end if;

      return Settings_Position_Cache (M, S);
   end Setting_Position;

   ------------------------
   -- Next_Valid_Setting --
   ------------------------

   procedure Next_Valid_Setting (S : in out Settings;
                                 M : Sequencer.Track_Mode_Kind)
   is
      Result : Settings := S;
   begin
      while Result /= Settings'Last loop
         Result := Settings'Succ (Result);
         if Valid_Setting (Result, M) then
            S := Result;
            return;
         end if;
      end loop;

      if Valid_Setting (Result, M) then
         S := Result;
      end if;
   end Next_Valid_Setting;

   ------------------------
   -- Prev_Valid_Setting --
   ------------------------

   procedure Prev_Valid_Setting (S : in out Settings;
                                 M : Sequencer.Track_Mode_Kind)
   is
      Result : Settings := S;
   begin
      while Result /= Settings'First loop
         Result := Settings'Pred (Result);
         if Valid_Setting (Result, M) then
            S := Result;
            return;
         end if;
      end loop;

      if Valid_Setting (Result, M) then
         S := Result;
      end if;
   end Prev_Valid_Setting;

   --------------
   -- To_CC_Id --
   --------------

   function To_CC_Id (S : Settings) return Sequencer.CC_Id
   is (case S is
          when CC_A | CC_Label_A => A,
          when CC_B | CC_Label_B => B,
          when CC_C | CC_Label_C => C,
          when CC_D | CC_Label_D => D,
          when others => raise Program_Error);

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Track_Settings_Singleton'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Track_Settings_Menu)
   is
   begin
      This.Fix_Current_Setting;

      Draw_Menu_Box ("Track settings",
                     Count => Settings_Count (Mode (Editing_Track)),
                     Index => Setting_Position (This.Current_Setting,
                                                Mode (Editing_Track)));
      case This.Current_Setting is
         when Track_Mode =>
            Draw_Title ("Track mode:", "");
            Draw_Value (Img (Mode (Editing_Track)));

         when Volume =>
            Draw_Precentage ("Volume:",
                             WNM.Synth.Volume (Editing_Track));

         when Pan    =>
            Draw_Pan ("Pan:", WNM.Synth.Pan (Editing_Track) / 2);

         when Arp_Mode =>
            Draw_Title ("Arpeggiator mode:", "");
            Draw_Value (Arpeggiator.Img (Arpeggiator.Mode));

         when Arp_Notes =>
            Draw_Title ("Arpeggiator notes:", "");
            Draw_Value (Arpeggiator.Img (Arpeggiator.Notes));

         when MIDI_Chan =>
            Draw_Title ("MIDI Channel:", "");
            Draw_Value (Sequencer.MIDI_Chan (Editing_Track)'Img);

         when MIDI_Instrument =>
            Draw_Title ("MIDI instrument:", "");
            Draw_Value (Builtin_Instruments (This.Instrument).Name);

         when Sample =>

            Draw_Sample_Select (Sequencer.Selected_Sample (Editing_Track));

         when Speech_Word =>

            Draw_Word_Select (Sequencer.Selected_Word (Editing_Track));

         when CC_A | CC_B | CC_C | CC_D =>
            declare
               CC : constant Sequencer.CC_Id :=
                 To_CC_Id (This.Current_Setting);
            begin
               Draw_Title ("MIDI CC " & Sequencer.CC_Letter (CC) & ":", "");
               Draw_Value ("Controller:" &
                             Sequencer.CC_Controller (Editing_Track, CC)'Img);
            end;

         when CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D =>
            declare
               CC : constant Sequencer.CC_Id :=
                 To_CC_Id (This.Current_Setting);
            begin
               Draw_Title ("MIDI CC " & Sequencer.CC_Letter (CC) & " Label:",
                           "");
               Draw_Value (Sequencer.CC_Controller_Label (Editing_Track, CC));
            end;
      end case;

   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Track_Settings_Menu;
      Event : Menu_Event)
   is
   begin
      This.Fix_Current_Setting;

      case Event.Kind is
         when Left_Press =>
            case This.Current_Setting is
               when MIDI_Instrument =>

                  --  Apply selected instrument settings
                  declare
                     I : MIDI_Instrument_Settings renames
                       Builtin_Instruments (This.Instrument);
                  begin
                     Set_CC_Controller (Editing_Track, A, I.CC_A);
                     Set_CC_Controller (Editing_Track, B, I.CC_B);
                     Set_CC_Controller (Editing_Track, C, I.CC_C);
                     Set_CC_Controller (Editing_Track, D, I.CC_D);

                     Set_CC_Controller_Label (Editing_Track, A, I.CC_A_Label);
                     Set_CC_Controller_Label (Editing_Track, B, I.CC_B_Label);
                     Set_CC_Controller_Label (Editing_Track, C, I.CC_C_Label);
                     Set_CC_Controller_Label (Editing_Track, D, I.CC_D_Label);
                  end;

               when CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D =>

                  --  Push text edit dialog

                  declare
                     CC : constant Sequencer.CC_Id :=
                       To_CC_Id (This.Current_Setting);
                  begin
                     WNM.GUI.Menu.Text_Dialog.Set_Title
                       ("CC " & Sequencer.CC_Letter (CC) & " Label");
                     WNM.GUI.Menu.Text_Dialog.Push_Window
                       (Sequencer.CC_Controller_Label (Editing_Track, CC));
                  end;

               when others =>
                  null;
            end case;

         when Right_Press =>
            --  Never exit the step settings
            null;
         when Encoder_Right =>
            case This.Current_Setting is
               when Track_Mode =>
                  if Event.Value > 0 then
                     Sequencer.Mode_Next (Editing_Track);
                  else
                     Sequencer.Mode_Prev (Editing_Track);
                  end if;

               when Volume =>
                  WNM.Synth.Change_Volume (Editing_Track, Event.Value);

               when Pan =>
                  Synth.Change_Pan (Editing_Track, Event.Value);

               when Arp_Mode =>
                  if Event.Value > 0 then
                     Arpeggiator.Mode_Next;
                  else
                     Arpeggiator.Mode_Prev;
                  end if;

               when Arp_Notes =>
                  if Event.Value > 0 then
                     Arpeggiator.Notes_Next;
                  else
                     Arpeggiator.Notes_Prev;
                  end if;

               when MIDI_Chan =>
                  if Event.Value > 0 then
                     Sequencer.MIDI_Chan_Next (Editing_Track);
                  else
                     Sequencer.MIDI_Chan_Prev (Editing_Track);
                  end if;

               when MIDI_Instrument =>
                  if Event.Value > 0 then
                     if This.Instrument < Builtin_Instruments'Last then
                        This.Instrument := This.Instrument + 1;
                     end if;
                  else
                     if This.Instrument > Builtin_Instruments'First then
                        This.Instrument := This.Instrument - 1;
                     end if;
                  end if;

               when Sample =>
                  if Event.Value > 0 then
                     Sequencer.Next_Sample (Editing_Track);
                  else
                     Sequencer.Prev_Sample (Editing_Track);
                  end if;

               when Speech_Word =>
                  if Event.Value > 0 then
                     Sequencer.Next_Word (Editing_Track);
                  else
                     Sequencer.Prev_Word (Editing_Track);
                  end if;

               when CC_A | CC_B | CC_C | CC_D =>
                  declare
                     CC : constant Sequencer.CC_Id :=
                       To_CC_Id (This.Current_Setting);
                  begin

                     if Event.Value > 0 then
                        Sequencer.CC_Controller_Next (Editing_Track, CC);
                     else
                        Sequencer.CC_Controller_Prev (Editing_Track, CC);
                     end if;
                  end;

               when CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D =>
                  GUI.Popup.Display ("L press to edit ", 500_000);

            end case;
         when Encoder_Left =>
            if Event.Value > 0 then
               Next_Valid_Setting (This.Current_Setting,
                                   Mode (Editing_Track));
            elsif Event.Value < 0 then
               Prev_Valid_Setting (This.Current_Setting,
                                   Mode (Editing_Track));
            end if;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Track_Settings_Menu)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus
     (This       : in out Track_Settings_Menu;
      Exit_Value : Window_Exit_Value)
   is
   begin
      This.Fix_Current_Setting;

      if This.Current_Setting
          in CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D
      then

         --  Return from controller label text edit

         if Exit_Value = Success then
            declare
               CC : constant CC_Id := To_CC_Id (This.Current_Setting);
               Output : constant String := WNM.GUI.Menu.Text_Dialog.Value;
               Label : Controller_Label := Empty_Controller_Label;
            begin
               if Output'Length > Label'Length then
                  Label := Output
                    (Output'First .. Output'First + Label'Length - 1);
               else
                  Label (Label'First .. Label'First + Output'Length - 1) :=
                 Output;
               end if;
               Sequencer.Set_CC_Controller_Label (Editing_Track, CC, Label);
            end;
         end if;
      else
         raise Program_Error;
      end if;
   end On_Focus;

end WNM.GUI.Menu.Track_Settings;
