with MIDI.Time.Generic_Clock;

with WNM.Time;
with WNM.Project;
with WNM.Project.Step_Sequencer;
with WNM.MIDI_Queues;

package WNM.MIDI_Clock
is new MIDI.Time.Generic_Clock
  (Microseconds_Count => WNM.Time.Time_Microseconds,
   Clock              => WNM.Time.Clock,
   BPM                => WNM.Project.Get_BPM,
   Tick_Callback      => WNM.Project.Step_Sequencer.MIDI_Clock_Tick,
   Start_Callback     => WNM.Project.Step_Sequencer.MIDI_Song_Start,
   Continue_Callback  => WNM.Project.Step_Sequencer.MIDI_Song_Continue,
   Stop_Callback      => WNM.Project.Step_Sequencer.MIDI_Song_Stop,
   Send_Message       => WNM.MIDI_Queues.Send_External);
