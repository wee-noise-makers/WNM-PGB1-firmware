-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2025 Fabien Chouteau                    --
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

with MIDI; use MIDI;

with WNM.Persistent;

package body WNM.MIDI_Utils is

   ---------------------------
   -- Filter_External_Clock --
   ---------------------------

   procedure Filter_External_Clock (Msg : MIDI.Message) is
      Ext_Clock : constant Boolean :=
          Msg.Kind = Sys
        and then
          Msg.Cmd in Start_Song | Continue_Song | Timming_Tick;
   begin

      --  Filter external clock events based on user settings

      if not Ext_Clock or else Persistent.Data.MIDI_Clock_Output then
         WNM_HAL.Send_External (Msg);
      end if;
   end Filter_External_Clock;

end WNM.MIDI_Utils;
