-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with WNM.MIDI;
with WNM.Sequencer; use WNM.Sequencer;

package WNM.Arpeggiator is

   type Arp_Mode is (Up, Down, Up_Down, Random);
   type Arp_Notes is (Chord, Scale);

   function Mode (T : Tracks := Editing_Track) return Arp_Mode;
   procedure Mode_Next (T : Tracks := Editing_Track);
   procedure Mode_Prev (T : Tracks := Editing_Track);


   function Notes (T : Tracks := Editing_Track) return Arp_Notes;
   procedure Notes_Next (T : Tracks := Editing_Track);
   procedure Notes_Prev (T : Tracks := Editing_Track);

   function Next_Note (T : Tracks ) return MIDI.MIDI_Key;


   function Img (M : Arp_Mode) return String
   is (case M is
          when Up => "Up",
          when Down => "Down",
          when Up_Down => "Up and Down",
          when Random => "Random");

   function Img (N : Arp_Notes) return String
   is (case N is
          when Chord => "Notes of chord",
          when Scale => "Notes of scale");

private

   package Arp_Notes_Next is new Enum_Next (Arp_Notes);
   use Arp_Notes_Next;

   package Arp_Mode_Next is new Enum_Next (Arp_Mode);
   use Arp_Mode_Next;

   type Arpeggiator_Rec is record
      Mode : Arp_Mode := Up;
      Notes : Arp_Notes := Chord;

      Going_Up : Boolean := True;

      Next_Index : Natural := 0;
   end record;

   Arpeggiators : array (Tracks) of Arpeggiator_Rec;

end WNM.Arpeggiator;
