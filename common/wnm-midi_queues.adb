with MIDI.Encoder;
with WNM_HAL;

package body WNM.MIDI_Queues is

   --------------------
   -- Sequencer_Push --
   --------------------

   procedure Send_External (Msg : MIDI.Message) is
   begin
      WNM_HAL.Send_MIDI (MIDI.Encoder.Encode (Msg));
   end Send_External;

end WNM.MIDI_Queues;
