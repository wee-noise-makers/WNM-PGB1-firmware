-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2023 Fabien Chouteau                    --
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

with MIDI;
with WNM.Time;

package WNM.Note_Off_Sequencer is

   --  Circular buffer of notes to turn off at a given date

   procedure Note_Off (Target     : MIDI_Target;
                       Chan       : MIDI.MIDI_Channel;
                       Key        : MIDI.MIDI_Key;
                       Expiration : Time.Time_Microseconds);
   --  Check if there's already a note off for that key and update its
   --  expiration time. Otherwise, add a new note off event. If buffer full
   --  -> kill oldest note.

   procedure Update (Now : Time.Time_Microseconds);

private

   Notes_Per_Track : constant := 8;
   type Event_Index is range 1 .. Notes_Per_Track;

   type Event is record
      Key        : MIDI.MIDI_Key := 0;
      Expiration : Time.Time_Microseconds := 0;
   end record;

   Events : array (MIDI_Target, MIDI.MIDI_Channel, Event_Index) of Event;

end WNM.Note_Off_Sequencer;
