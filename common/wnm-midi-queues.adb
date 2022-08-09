with System;

package body WNM.MIDI.Queues is

   procedure Push_To_Out_Queue (Msg : Message);

   --------------------
   -- Sequencer_Push --
   --------------------

   procedure Sequencer_Push (Msg : Message) is
   begin
      Push (Synth_Queue, Msg);
      Push_To_Out_Queue (Msg);
   end Sequencer_Push;

   ---------------
   -- Synth_Pop --
   ---------------

   procedure Synth_Pop is
   begin
      while not Empty (Synth_Queue) loop
         Process (Pop (Synth_Queue));
      end loop;
   end Synth_Pop;

   ------------
   -- Inc_In --
   ------------

   procedure Inc_In (Q : in out Message_Queue) is
   begin
      if Q.Next_In = Q.Arr'Last then
         Q.Next_In := Q.Arr'First;
      else
         Q.Next_In := Q.Next_In + 1;
      end if;
   end Inc_In;

   -------------
   -- Inc_Out --
   -------------

   procedure Inc_Out (Q : in out Message_Queue) is
   begin
      if Q.Next_Out = Q.Arr'Last then
         Q.Next_Out := Q.Arr'First;
      else
         Q.Next_Out := Q.Next_Out + 1;
      end if;
   end Inc_Out;

   -----------
   -- Empty --
   -----------

   function Empty (Q : Message_Queue) return Boolean is
   begin
      return Q.Next_In = Q.Next_Out;
   end Empty;

   ----------
   -- Push --
   ----------

   procedure Push (Q : in out Message_Queue; Msg : Message) is
   begin
      Q.Arr (Q.Next_In) := Msg;

      Inc_In (Q);

      --  If after the push In = Out, it means the buffer was full and an old
      --  message was replaced. Move Out to skip.
      if Q.Next_In = Q.Next_Out then
         Inc_Out (Q);
      end if;
   end Push;

   ---------
   -- Pop --
   ---------

   function Pop (Q : in out Message_Queue) return Message is
      Ret : Message;
   begin
      Ret := Q.Arr (Q.Next_Out);
      Inc_Out (Q);
      return Ret;
   end Pop;

   -----------------------
   -- Push_To_Out_Queue --
   -----------------------

   procedure Push_To_Out_Queue (Msg : Message) is
      use BBqueue;
      use BBqueue.Buffers;

      WG : BBqueue.Buffers.Write_Grant;
   begin
      Grant (MIDI_Out_Queue, WG, Size => 3);

      if State (WG) = Valid then
         declare
            Addr : constant System.Address := Slice (WG).Addr;
            Dst : Message with Import, Address => Addr;
         begin
            Dst := Msg;
         end;
         Commit (MIDI_Out_Queue, WG);
      end if;
   end Push_To_Out_Queue;

   -------------------
   -- MIDI_Out_Read --
   -------------------

   procedure MIDI_Out_Read (G   : in out BBqueue.Buffers.Read_Grant;
                            Max :        BBqueue.Count := BBqueue.Count'Last)
   is
   begin
      BBqueue.Buffers.Read (MIDI_Out_Queue, G, Max);
   end MIDI_Out_Read;

   ----------------------
   -- MIDI_Out_Release --
   ----------------------

   procedure MIDI_Out_Release (G : in out BBqueue.Buffers.Read_Grant) is
   begin
      BBqueue.Buffers.Release (MIDI_Out_Queue, G);
   end MIDI_Out_Release;

end WNM.MIDI.Queues;
