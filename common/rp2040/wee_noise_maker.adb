with WNM.Sequencer;
with WNM.UI;
with WNM.LED;
with WNM.GUI.Update;
--  with WNM.Master_Volume;
--  with WNM.Sample_Library;
--  with WNM.Audio;
--  with WNM.Synth;
with WNM.Time; use WNM.Time;

with WNM.RP2040.MIDI;
with WNM.Screen;

with WNM.GUI.Menu.Track_Settings;

procedure Wee_Noise_Maker is
   Next_Start : Time_Microseconds;
begin

   --  WNM.Sample_Library.Load;
   --  WNM.Synth.Load_Samples;
   --
   WNM.GUI.Menu.Track_Settings.Push_Window;

   loop
      Next_Start := Time_Microseconds'Last;

      --  Next_Start := Time_Ms'Min (WNM.Synth.Update, Next_Start);
      Next_Start := Time_Microseconds'Min (WNM.Sequencer.Update, Next_Start);
      Next_Start := Time_Microseconds'Min (WNM.UI.Update, Next_Start);
      Next_Start := Time_Microseconds'Min (WNM.LED.Update, Next_Start);
      WNM.RP2040.MIDI.Update;
      Next_Start := Time_Microseconds'Min (WNM.GUI.Update.Update, Next_Start);

      --  WNM.Audio.Set_Volume (50);
      --  WNM.Master_Volume.Update;

      --  Delay_Ms (Next_Start);
   end loop;

end Wee_Noise_Maker;
