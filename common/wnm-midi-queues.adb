with System;
with System.Storage_Elements;

with WNM_HAL;

package body WNM.MIDI.Queues is

   --------------------
   -- Sequencer_Push --
   --------------------

   procedure Sequencer_Push (Msg : Message) is
      Data : System.Storage_Elements.Storage_Array (1 .. 3)
        with Address => Msg'Address;
   begin
      WNM_HAL.Send_MIDI (Data);
   end Sequencer_Push;

end WNM.MIDI.Queues;
