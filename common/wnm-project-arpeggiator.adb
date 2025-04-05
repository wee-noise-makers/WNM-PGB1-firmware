-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2022 Fabien Chouteau                    --
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

with WNM.Chord_Settings;

package body WNM.Project.Arpeggiator is

   type Arp_Pattern is array (Natural range <>) of
     WNM.Chord_Settings.Chord_Index_Range;
   type Arp_Pattern_Acc is not null access constant Arp_Pattern;

   Arp_Up : aliased constant Arp_Pattern :=
     (0, 1, 2, 3);

   pragma Compile_Time_Error
     (Arp_Up'Length /= WNM.Chord_Settings.Chord_Notes'Length,
      "Number of notes in chords changed");

   Arp_Down : aliased constant Arp_Pattern :=
     (3, 2, 1, 0);

   pragma Compile_Time_Error
     (Arp_Down'Length /= WNM.Chord_Settings.Chord_Notes'Length,
      "Number of notes in chords changed");

   Arp_Up_Down : aliased constant Arp_Pattern :=
     (0, 1, 2, 3, 2, 1);

   pragma Compile_Time_Error
     (Arp_Up_Down'Length /= (2 * WNM.Chord_Settings.Chord_Notes'Length) - 2,
      "Number of notes in chords changed");

   Arp_Pat1 : aliased constant Arp_Pattern :=
     (0, 3, 2, 3, 1, 3, 2, 3);

   Arp_Pat2 : aliased constant Arp_Pattern :=
     (0, 1, 2, 3, 1, 3, 2, 3);

   Arp_Pat3 : aliased constant Arp_Pattern :=
     (0, 2, 1, 3, 2, 1, 2);

   ---------------
   -- Next_Note --
   ---------------

   function Next_Note (T     : Tracks;
                       Mode  : Arp_Mode_Kind;
                       Chord : WNM.Chord_Settings.Chord_Notes)
                       return MIDI.MIDI_Key
   is
      use WNM.Chord_Settings;

      Track : Track_Rec renames G_Project.Tracks (T);
      Arp : Arpeggiator_Rec renames Arpeggiators (T);

      Note_Index : Chord_Index_Range;
   begin

      case Mode is
         when Random =>
            Note_Index :=
              Chord_Index_Range (Natural (WNM.Random) mod
                                     Natural (Chord_Index_Range'Last + 1));
         when others =>
            declare
               Pat : constant Arp_Pattern_Acc :=
                 (case Mode is
                     when Up       => Arp_Up'Access,
                     when Down     => Arp_Down'Access,
                     when Up_Down  => Arp_Up_Down'Access,
                     when Pattern1 => Arp_Pat1'Access,
                     when Pattern2 => Arp_Pat2'Access,
                     when Pattern3 => Arp_Pat3'Access,
                     when Random   => raise Program_Error);
            begin
               if Arp.Next_Index not in Pat'Range then
                  Arp.Next_Index := Pat'First;
               end if;
               Note_Index := Pat (Arp.Next_Index);
               Arp.Next_Index := Arp.Next_Index + 1;
            end;
      end case;

      return Chord (Note_Index);
   end Next_Note;

   -----------------------------
   -- Signal_Start_Of_Pattern --
   -----------------------------

   procedure Signal_Start_Of_Pattern (T : Tracks) is
   begin

      --  Reset arp before the start of next pattern
      Arpeggiators (T).Next_Index := 0;
   end Signal_Start_Of_Pattern;

end WNM.Project.Arpeggiator;
