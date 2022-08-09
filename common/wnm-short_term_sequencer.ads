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

with WNM.Synth;
with WNM.MIDI;
with WNM.Time;

package WNM.Short_Term_Sequencer is

   subtype Data is WNM.MIDI.Message;

   subtype Expiration_Time is Time.Time_Microseconds;

   procedure Push (D : Data; Expiration : Expiration_Time);
   procedure Pop (Now : Expiration_Time; D : out Data; Success : out Boolean);

   --  procedure Print_Queue;

private

   Max_Number_Of_Tracks  : constant := 16;
   Max_Number_Of_Repeats : constant := 8;
   Max_Number_Of_Notes   : constant := 4;

   MAX_EVENT_NUMBER : constant :=
     Max_Number_Of_Tracks * Max_Number_Of_Repeats * Max_Number_Of_Notes * 2;

   type Event;

   type Event_Access is access all Event;

   type Event is record
      D : Data;
      Expiration : Expiration_Time;
      Next : Event_Access := null;
   end record;

end WNM.Short_Term_Sequencer;
