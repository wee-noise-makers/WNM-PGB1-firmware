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

with MIDI;
with WNM.Time;

package WNM.Short_Term_Sequencer is

   type Event_Data is record
      Target   : MIDI_Target := External;
      Chan     : MIDI.MIDI_Channel;
      Key      : MIDI.MIDI_Key;
      Velocity : MIDI.MIDI_Data;
      Duration : Time.Time_Microseconds;
   end record
     with Pack;

   procedure Play_At (Start    : Time.Time_Microseconds;
                      Target   : MIDI_Target;
                      Chan     : MIDI.MIDI_Channel;
                      Key      : MIDI.MIDI_Key;
                      Velocity : MIDI.MIDI_Data;
                      Duration : Time.Time_Microseconds);

   procedure Update (Now : Time.Time_Microseconds);

   procedure Halt;
   --  Halt the short term sequencer and make sure the shared buffer data is
   --  not used.

   procedure Restart;
   --  Restart the short term sequencer and reclain shared bufffer data

private

   type Event;

   type Event_Access is access all Event;

   type Event is record
      D : Event_Data;
      Expiration : Time.Time_Microseconds;
      Next : Event_Access;
   end record
     with Pack;

end WNM.Short_Term_Sequencer;
