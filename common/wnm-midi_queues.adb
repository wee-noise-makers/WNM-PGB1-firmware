with System;
with System.Storage_Elements;

with WNM_HAL;

package body WNM.MIDI_Queues is

   --------------------
   -- Sequencer_Push --
   --------------------

   procedure Send_External (Msg : MIDI.Message) is
      Data : System.Storage_Elements.Storage_Array (1 .. 3)
        with Address => Msg'Address;
   begin
      WNM_HAL.Send_MIDI (Data);
   end Send_External;

end WNM.MIDI_Queues;
