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

with WNM_Configuration;
with WNM_HAL;

with WNM.Sample_Stream;
with WNM.Speech;

with WNM.MIDI;

private with Ada.Unchecked_Conversion;

package WNM.Coproc is

   type Message_Kind is (Sampler_Event,
                         Speech_Event,
                         Speech_CC_Event,
                         Track_Vol_Pan,
                         Synth_Event)
     with Size => 4;

   type Synth_Voice_Id is (Kick, Snare, Cymbal, Lead)
     with Size => 2;

   type Synth_Event_Rec is record
      Voice    : Synth_Voice_Id;
      Trigger  : Boolean;
      Key      : MIDI.MIDI_Key;
      P1       : MIDI.MIDI_Data;
      P2       : MIDI.MIDI_Data;
   end record
     with Pack, Size => 24;

   type Message (Kind : Message_Kind := Sampler_Event) is record
      case Kind is
         when Sampler_Event =>
            Sampler_Evt : Sample_Stream.Sampler_Event_Rec;

         when Speech_Event =>
            Speech_Evt : Speech.Speech_Event_Rec;

         when Speech_CC_Event =>
            Speech_CC_Evt : Speech.Speech_CC_Event_Rec;

         when Track_Vol_Pan =>
            TVP_Track : Tracks;
            TVP_Vol : WNM_HAL.Audio_Volume;
            TVP_Pan : WNM_HAL.Audio_Pan;

         when Synth_Event =>
            Synth_Evt : Synth_Event_Rec;
      end case;
   end record
     with Size => WNM_Configuration.Coproc_Data_Size;

   for Message use record
      Kind        at 0 range 0 .. 3;
      Sampler_Evt at 0 range 6 .. 31;
      Speech_Evt  at 0 range 6 .. 31;
      Synth_Evt   at 0 range 6 .. 31;
   end record;

   procedure Push (Msg : Message);
   --  Send a message to the synth coprocessor. Fails silently if the message
   --  cannot be pushed (e.g. queue is full).

   procedure Pop (Msg : out Message; Success : out Boolean);
   --  Tentatively get a message for the synth coprocessor. Success is False
   --  if no message is available.

private

   function To_Coproc_Data
   is new Ada.Unchecked_Conversion (Message, WNM_HAL.Coproc_Data);

   function From_Coproc_Data
   is new Ada.Unchecked_Conversion (WNM_HAL.Coproc_Data, Message);

end WNM.Coproc;
