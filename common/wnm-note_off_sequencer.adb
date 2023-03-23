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

with HAL; use HAL;

with WNM.MIDI_Queues;
with WNM.Coproc;

package body WNM.Note_Off_Sequencer is

   -------------------
   -- Send_Note_Off --
   -------------------

   procedure Send_Note_Off (Target : MIDI_Target;
                            Chan   : MIDI.MIDI_Channel;
                            Key    : MIDI.MIDI_Key)
   is
   begin
      case Target is
         when External =>
            WNM.MIDI_Queues.Send_External
              ((MIDI.Note_Off, Chan, Key, 0));

         when Internal =>
            WNM.Coproc.Push
              ((WNM.Coproc.MIDI_Event,
               (MIDI.Note_Off, Chan, Key, 0)));
      end case;
   end Send_Note_Off;

   --------------
   -- Note_Off --
   --------------

   procedure Note_Off (Target     : MIDI_Target;
                       Chan       : MIDI.MIDI_Channel;
                       Key        : MIDI.MIDI_Key;
                       Expiration : Time.Time_Microseconds)
   is
      use MIDI;

      To_Kill : Event_Index := Event_Index'First;
      Sooner  : Time.Time_Microseconds := Time.Time_Microseconds'Last;
   begin
      for Index in Event_Index loop

         if Events (Target, Chan, To_Kill).Expiration /= 0
           and then
             Events (Target, Chan, Index).Key = Key
         then
            --  Update an existing event
            Events (Target, Chan, Index).Expiration := Expiration;
            return;

         elsif Events (Target, Chan, Index).Expiration < Sooner then
            --  Record the event to kill in case we don't update an existing
            --  one.
            To_Kill := Index;
            Sooner := Events (Target, Chan, Index).Expiration;
         end if;
      end loop;

      if Events (Target, Chan, To_Kill).Expiration /= 0 then
         Send_Note_Off (Target, Chan, Events (Target, Chan, To_Kill).Key);
      end if;

      Events (Target, Chan, To_Kill).Key := Key;
      Events (Target, Chan, To_Kill).Expiration := Expiration;
   end Note_Off;

   ------------
   -- Update --
   ------------

   procedure Update (Now : Time.Time_Microseconds) is
   begin
      for Target in MIDI_Target loop
         for Chan in MIDI.MIDI_Channel loop
            for Index in Event_Index loop
               if Events (Target, Chan, Index).Expiration /= 0
                 and then
                   Events (Target, Chan, Index).Expiration <= Now
               then
                  Send_Note_Off
                    (Target, Chan, Events (Target, Chan, Index).Key);
                  Events (Target, Chan, Index).Expiration := 0;
               end if;
            end loop;
         end loop;
      end loop;
   end Update;

end WNM.Note_Off_Sequencer;
