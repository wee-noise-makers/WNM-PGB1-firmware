with Ada.Text_IO;
with Ada.Exceptions;
with GNAT.OS_Lib;

with WNM.Sequencer;
with WNM.UI;
with WNM.LED;
with WNM.GUI.Update;
with WNM.Master_Volume;
with WNM.Audio;
with WNM.Synth;
with WNM.File_System;
with WNM.Sample_Library;
with WNM.Time; use WNM.Time;

with WNM.GUI.Menu.Track_Settings;

with WNM_Sim;
pragma Unreferenced (WNM_Sim);

procedure Wee_Noise_Maker_Sim is
   Next_Start : Time_Microseconds;

begin

   WNM.File_System.Mount;
   WNM.Sample_Library.Load;
   --  WNM.Synth.Load_Samples;

   WNM.GUI.Menu.Track_Settings.Push_Window;

   --  Test_Executor.Start;

   loop
      Next_Start := Time_Microseconds'Last;

      Next_Start := Time_Microseconds'Min (WNM.Synth.Update, Next_Start);
      Next_Start := Time_Microseconds'Min (WNM.Sequencer.Update, Next_Start);
      Next_Start := Time_Microseconds'Min (WNM.UI.Update, Next_Start);
      Next_Start := Time_Microseconds'Min (WNM.LED.Update, Next_Start);
      Next_Start := Time_Microseconds'Min (WNM.GUI.Update.Update, Next_Start);

      WNM.Audio.Set_Volume (50);
      WNM.Master_Volume.Update;

      Delay_Microseconds (Next_Start);
   end loop;

exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      GNAT.OS_Lib.OS_Exit (1);
end Wee_Noise_Maker_Sim;
