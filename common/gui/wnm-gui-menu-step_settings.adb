-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2020 Fabien Chouteau                    --
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


with HAL.Bitmap;           use HAL.Bitmap;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Sequencer;        use WNM.Sequencer;
with WNM.UI;

package body WNM.GUI.Menu.Step_Settings is

   package Top_Settings_Next is new Enum_Next (Top_Settings,
                                               Wrap => False);
   use Top_Settings_Next;
   package Sub_Settings_Next is new Enum_Next (Sub_Settings,
                                               Wrap => False);
   use Sub_Settings_Next;

   Step_Settings_Singleton : aliased Step_Settings_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Step_Settings_Singleton'Access);
   end Push_Window;

   ------------
   -- To_Top --
   ------------

   function To_Top (S : Sub_Settings) return Top_Settings
   is (case S is
          when Sequencer.Condition => Condition,
          when Sequencer.Note => Note,
          when Sequencer.Duration => Note,
          when Sequencer.Velo => Note,
          when Sequencer.Repeat => Repeat,
          when Sequencer.Repeat_Rate => Repeat,
          when Sequencer.CC_A => CC_A,
          when Sequencer.CC_B => CC_B,
          when Sequencer.CC_C => CC_C,
          when Sequencer.CC_D => CC_D);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Step_Settings_Menu)
   is

      Trig : constant Sequencer_Steps := Sequencer.Editing_Step;

      Top_Setting : constant Top_Settings := To_Top (This.Current_Setting);
   begin
      Draw_Menu_Box ("Step settings",
                     Count => Top_Settings_Count,
                     Index => Top_Settings'Pos (To_Top (This.Current_Setting)));

      case Top_Setting is
         when Condition =>
            Draw_Title ("Condition", "");
            Draw_Value (Img (Sequencer.Trig (Trig)));

         when Note      =>
            case This.Current_Setting is
               when Note =>
                  Draw_Title (Img (Sequencer.Note_Mode (Trig)), "");
               when Sequencer.Duration =>
                  Draw_Title ("Duration", "");
               when Velo =>
                  Draw_Title ("Velocity", "");
               when others =>
                  raise Program_Error;
            end case;

            case Sequencer.Note_Mode (Trig) is
               when Note | Note_In_Chord | Note_In_Scale =>
                  Draw_MIDI_Note (Sequencer.Note (Trig),
                                  This.Current_Setting = Note);
               when Chord =>
                  Draw_Value (Sequencer.Note (Trig)'Img);

               when Arp =>
                  Draw_Value ("---");
            end case;

            Draw_Duration (Sequencer.Duration (Trig),
                           This.Current_Setting = Sequencer.Duration);

            Draw_MIDI_Val (Sequencer.Velo (Trig),
                           This.Current_Setting = Velo);

         when Repeat =>
            case This.Current_Setting is
               when Repeat =>
                  Draw_Title ("Repeat Count", "");
               when Sequencer.Repeat_Rate =>
                  Draw_Title ("Repeat Rate", "");
               when others =>
                  raise Program_Error;
            end case;

            Draw_Value (Sequencer.Repeat (Trig)'Img,
                        Selected => This.Current_Setting = Repeat);

            Draw_Value_Left (Img (Sequencer.Repeat_Rate (Trig)),
                             Selected => This.Current_Setting = Sequencer.Repeat_Rate);

         when CC_A .. CC_D =>
            declare
               Id : constant Sequencer.CC_Id :=
                 (case This.Current_Setting is
                  when CC_A => Sequencer.A,
                  when CC_B => Sequencer.B,
                  when CC_C => Sequencer.C,
                  when others => Sequencer.D);
            begin
               Draw_Title (Sequencer.CC_Controller_Label (Editing_Track, Id),
                           "");

               if Sequencer.CC_Enabled (Trig, Id) then
                  Draw_MIDI_Val (Sequencer.CC_Value (Trig, Id),
                                Selected => False);
               else
                  Draw_Value ("- Disabled -");
               end if;
            end;
      end case;
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Step_Settings_Menu;
      Event : Menu_Event)
   is
      Trig : constant Sequencer_Steps := Sequencer.Editing_Step;
   begin
      case Event.Kind is
         when Left_Press =>
            null;
         when Right_Press =>
            case This.Current_Setting is
               when CC_A => Sequencer.CC_Toggle (Trig, A);
               when CC_B => Sequencer.CC_Toggle (Trig, B);
               when CC_C => Sequencer.CC_Toggle (Trig, C);
               when CC_D => Sequencer.CC_Toggle (Trig, D);

               when Note => Sequencer.Note_Mode_Next (Trig);

               when others => null;
            end case;

         when Encoder_Right =>
            if Event.Value > 0 then
               WNM.Sequencer.Next_Value (This.Current_Setting);
            else
               WNM.Sequencer.Prev_Value (This.Current_Setting);
            end if;

         when Encoder_Left =>
            if Event.Value > 0 then
               Next (This.Current_Setting);
            elsif Event.Value < 0 then
               Prev (This.Current_Setting);
            end if;
      end case;
   end On_Event;

      ---------------
   -- On_Pushed --
   ---------------

   overriding procedure On_Pushed
     (This  : in out Step_Settings_Menu)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding procedure On_Focus
     (This       : in out Step_Settings_Menu;
      Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Step_Settings;
