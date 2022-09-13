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
with WNM.Project.Chord_Sequencer;

package body WNM.Project.Arpeggiator is

   ---------------
   -- Next_Note --
   ---------------

   function Next_Note (T : Tracks) return MIDI.MIDI_Key is
      use WNM.Chord_Settings;
      use WNM.Project.Chord_Sequencer;

      Track : Track_Rec renames G_Project.Tracks (T);
      Arp : Arpeggiator_Rec renames Arpeggiators (T);
      Result : MIDI.MIDI_Key;
      Last : Natural;
   begin
      case Track.Arp_Notes is
         when Chord =>
            Result :=  Current_Chord (Chord_Index_Range (Arp.Next_Index));
            Last := Natural (Chord_Index_Range'Last);
      end case;

      case Track.Arp_Mode is
         when Up =>
            if Arp.Next_Index = Last then
               Arp.Next_Index := 0;
            else
               Arp.Next_Index := Arp.Next_Index + 1;
            end if;

         when Down =>
            if Arp.Next_Index = 0 then
               Arp.Next_Index := Last;
            else
               Arp.Next_Index := Arp.Next_Index - 1;
            end if;

         when Up_Down =>
            if Arp.Going_Up then
               if Arp.Next_Index = Last then
                  Arp.Going_Up := False;
                  Arp.Next_Index := Last - 1;
               else
                  Arp.Next_Index := Arp.Next_Index + 1;
               end if;
            else
               if Arp.Next_Index = 0 then
                  Arp.Next_Index := 1;
                  Arp.Going_Up := True;
               else
                  Arp.Next_Index := Arp.Next_Index - 1;
               end if;
            end if;

         when Random =>
            Arp.Next_Index := Natural (WNM.Random) mod (Last + 1);
      end case;

      return Result;

   end Next_Note;

end WNM.Project.Arpeggiator;
