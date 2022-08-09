with BBqueue.Buffers;

package WNM.MIDI.Queues is

   procedure Sequencer_Push (Msg : Message);

   generic
      with procedure Process (Msg : Message);
   procedure Synth_Pop;

   procedure MIDI_Out_Read (G   : in out BBqueue.Buffers.Read_Grant;
                            Max :        BBqueue.Count := BBqueue.Count'Last);
   procedure MIDI_Out_Release (G : in out BBqueue.Buffers.Read_Grant);

private

   type Message_Array is array (Natural range <>) of Message;

   type Message_Queue (Length : Natural) is record
      Arr : Message_Array (1 .. Length);
      Next_In : Natural := 1;
      Next_Out : Natural := 1;
   end record;

   procedure Inc_In (Q : in out Message_Queue);
   procedure Inc_Out (Q : in out Message_Queue);

   function Empty (Q : Message_Queue) return Boolean;
   procedure Push (Q : in out Message_Queue; Msg : Message);
   function Pop (Q : in out Message_Queue) return Message
     with Pre => not Empty (Q);

   Synth_Queue : Message_Queue (256);
   MIDI_Out_Queue : BBqueue.Buffers.Buffer (1024);

end WNM.MIDI.Queues;
