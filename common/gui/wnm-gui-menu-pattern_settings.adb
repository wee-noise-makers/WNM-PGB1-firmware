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

with HAL.Bitmap;           use HAL.Bitmap;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;

with WNM.Chord_Sequencer;

package body WNM.GUI.Menu.Pattern_Settings is

   package Top_Settings_Next is new Enum_Next (Top_Settings,
                                               Wrap => False);
   use Top_Settings_Next;
   package Sub_Settings_Next is new Enum_Next (Sub_Settings,
                                               Wrap => False);
   use Sub_Settings_Next;

   Pattern_Menu_Singleton : aliased Pattern_Settings_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Pattern_Menu_Singleton'Access);
   end Push_Window;

   ------------
   -- To_Top --
   ------------

   function To_Top (S : Sub_Settings) return Top_Settings
   is (case S is
          when Magic_Hat => Magic_Hat,
          when Scale_Key => Scale,
          when Scale_Mode => Scale,
          when Progression_Kind => Progression,
          when Progression_Dur => Progression,
          when Progression_Add => Progression);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Pattern_Settings_Menu)
   is
      use Chord_Sequencer;
      Top_Setting : constant Top_Settings := To_Top (This.Current_Setting);
   begin
      Draw_Menu_Box ("Pattern settings",
                     Count => Top_Settings_Count,
                     Index => Top_Settings'Pos (To_Top (This.Current_Setting)));

      case Top_Setting is
         when Magic_Hat =>
            Draw_Title ("Magic Hat Of Chord",
                        "Progressions");

         when Scale =>
            Draw_Title ("Key", "");
            Draw_MIDI_Note (Current_Scale_Key,
                            This.Current_Setting = Scale_Key);
            Draw_Scale_Mode (Chord_Sequencer.Current_Scale_Name,
                             This.Current_Setting = Scale_Mode);
         when Progression =>
            if This.Current_Setting = Progression_Add then
               Draw_Title ("Progression:",
                           "Press L to add");
               Draw_Value ("chord.");
            else
               Draw_Title ("Progression:" & Chord_Sequencer.Cursor'Img, "");

               Draw_Chord_Kind
                 (Chord_Sequencer.Chord_Kind,
                  Selected => This.Current_Setting = Progression_Kind);

               Draw_Chord_Duration
                 (Chord_Sequencer.Duration,
                  Selected => This.Current_Setting = Progression_Dur);
            end if;
      end case;

   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Pattern_Settings_Menu;
                       Event : Menu_Event)
   is
      use Chord_Sequencer;
   begin
      case Event.Kind is
         when Left_Press =>
            case This.Current_Setting is
               when Magic_Hat =>
                  Chord_Sequencer.Randomly_Pick_A_Progression;

               when Progression_Add =>
                  Chord_Sequencer.Add_Chord;
                  This.Current_Setting := Progression_Dur;

               when others =>
                  null;
            end case;

         when Right_Press =>
            if This.Current_Setting in Progression_Dur | Progression_Kind then
               Chord_Sequencer.Remove_Chord;
            end if;

         when Encoder_Right =>
            case This.Current_Setting is
               when Magic_Hat =>
                  null;

               when Scale_Key =>
                  if Event.Value > 0 then
                     Chord_Sequencer.Scale_Key_Next;
                  else
                     Chord_Sequencer.Scale_Key_Prev;
                  end if;

               when Scale_Mode =>
                  if Event.Value > 0 then
                     Chord_Sequencer.Scale_Next;
                  else
                     Chord_Sequencer.Scale_Prev;
                  end if;

               when Progression_Kind =>
                  if Event.Value > 0 then
                     Chord_Sequencer.Chord_Kind_Next;
                  else
                     Chord_Sequencer.Chord_Kind_Prev;
                  end if;

               when Progression_Dur =>
                  if Event.Value > 0 then
                     Chord_Sequencer.Duration_Next;
                  else
                     Chord_Sequencer.Duration_Prev;
                  end if;

               when Progression_Add =>
                  null;
            end case;

         when Encoder_Left =>
            case This.Current_Setting is

               when Progression_Kind =>

                  if Event.Value > 0 then
                     Next (This.Current_Setting);

                  elsif Event.Value < 0 then
                     if Chord_Sequencer.Cursor = Progression_Range'First
                     then
                        Prev (This.Current_Setting);
                     else
                        This.Current_Setting := Progression_Dur;
                        Chord_Sequencer.Cursor_Prev;
                     end if;
                  end if;

               when Progression_Dur =>

                  if Event.Value > 0 then
                     if Chord_Sequencer.Cursor = Progression_Length
                     then
                     Next (This.Current_Setting);
                     else
                        This.Current_Setting := Progression_Kind;
                        Chord_Sequencer.Cursor_Next;
                     end if;

                  elsif Event.Value < 0 then
                     This.Current_Setting := Prev (This.Current_Setting);
                  end if;

               when others =>
                  if Event.Value > 0 then
                     This.Current_Setting := Next (This.Current_Setting);
                  elsif Event.Value < 0 then
                     This.Current_Setting := Prev (This.Current_Setting);
                  end if;
            end case;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed (This  : in out Pattern_Settings_Menu)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus (This       : in out Pattern_Settings_Menu;
                       Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Pattern_Settings;
