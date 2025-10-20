package body WNM.Synth_Engines is

   Internal_MIDI_To_Tresses : array (MIDI_Data) of Tresses.Engines :=
     (others => Tresses.Voice_Saw_Swarm);

   function MIDI_To_Tresses (E : MIDI_Data) return Tresses.Engines
   is (Internal_MIDI_To_Tresses (E));
   --  Function to access the array, makes it read-only outside of this
   --  package body.

begin
   for E in Tresses.Engines loop
      if Tresse_To_MIDI (E) /= MIDI_Data'Last then
         Internal_MIDI_To_Tresses (Tresse_To_MIDI (E)) := E;
      end if;
   end loop;
end WNM.Synth_Engines;
