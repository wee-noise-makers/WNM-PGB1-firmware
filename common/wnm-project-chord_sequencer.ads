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

with WNM.Chord_Settings; use WNM.Chord_Settings;

package WNM.Project.Chord_Sequencer is

   function Current_Tonic return MIDI.MIDI_Key;
   function Current_Chord_Name return Chord_Name;
   function Current_Chord_Intervals return Chord_Intervals;
   function Current_Chord return Chord_Notes;

   function Chords_In_Progression return Chord_Slot_Id;
   function Current_Chord_Index return Chord_Slot_Id;
   function Shadow_Chord_Index return Chord_Slot_Id;

   pragma Inline (Current_Tonic);
   pragma Inline (Current_Chord_Name);
   pragma Inline (Current_Chord_Intervals);
   pragma Inline (Current_Chord);

end WNM.Project.Chord_Sequencer;
