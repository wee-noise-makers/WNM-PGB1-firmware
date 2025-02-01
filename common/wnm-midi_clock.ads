with MIDI.Time.Generic_Clock;

with WNM.Time;
with WNM.Project;
with WNM.Project.Step_Sequencer;
with WNM.Song_Start_Broadcast;
with WNM.Song_Stop_Broadcast;
with WNM.Song_Continue_Broadcast;
with WNM.MIDI_Utils;

package WNM.MIDI_Clock
is new MIDI.Time.Generic_Clock
  (Microseconds_Count         => WNM.Time.Time_Microseconds,
   Clock                      => WNM.Time.Clock,
   BPM                        => WNM.Project.Get_BPM,
   Tick_Callback              => WNM.Project.Step_Sequencer.MIDI_Clock_Tick,
   Start_Callback             => WNM.Song_Start_Broadcast.Broadcast,
   Continue_Callback          => WNM.Song_Continue_Broadcast.Broadcast,
   Stop_Callback              => WNM.Song_Stop_Broadcast.Broadcast,
   Send_Message               => WNM.MIDI_Utils.Filter_External_Clock,
   Propagate_External_Message => False);
