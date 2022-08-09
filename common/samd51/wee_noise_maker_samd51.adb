
with WNM.Sequencer;
with WNM.UI;
with WNM.GUI.Update;
with WNM.Master_Volume;
with WNM.Sample_Library;
with WNM.Audio;
with WNM.Synth;
--  with WNM.FM_Tuner;
with WNM.Time; use WNM.Time;
with WNM.LED;
with WNM.Samd51.MIDI;

procedure Wee_Noise_Maker_Samd51 is
   Next_Start : Time_Ms;
begin

   WNM.Sample_Library.Load;

   WNM.Synth.Load_Samples;

   --  WNM.Synth.Set_Passthrough (WNM.Audio.FM);
   --  WNM.FM_Tuner.Seek (WNM.FM_Tuner.Up);
   --  WNM.Sequencer.Play_Pause;

   loop
      Next_Start := Time_Ms'Last;

      for X in 1 .. 10 loop
         Next_Start := Time_Ms'Min (WNM.Synth.Update, Next_Start);
      end loop;

      Next_Start := Time_Ms'Min (WNM.Sequencer.Update, Next_Start);
      Next_Start := Time_Ms'Min (WNM.UI.Update, Next_Start);
      Next_Start := Time_Ms'Min (WNM.GUI.Update.Update, Next_Start);
      Next_Start := Time_Ms'Min (WNM.LED.Update, Next_Start);
      Next_Start := Time_Ms'Min (WNM.Samd51.MIDI.Update, Next_Start);

      WNM.Audio.Set_Volume (50);
      WNM.Master_Volume.Update;

      Delay_Ms (Next_Start);
   end loop;

end Wee_Noise_Maker_Samd51;
