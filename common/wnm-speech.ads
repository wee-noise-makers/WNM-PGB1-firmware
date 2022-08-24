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

with WNM.MIDI;
with WNM.Audio;

package WNM.Speech is

   type Word is new WNM.MIDI.MIDI_Key;

   function Img (W : Word) return String;

   procedure Start (T : Tracks;
                    W : Word;
                    K : MIDI.MIDI_Key);

   procedure Stop (T : Tracks);

   procedure Next_Points (Buffer : in out Audio.Stereo_Buffer);

   type Speech_Event_Rec is record
      On       : Boolean;
      Track    : Tracks;
      W        : Word;
      Key      : MIDI.MIDI_Key;
   end record
     with Pack, Size => 20;

end WNM.Speech;
