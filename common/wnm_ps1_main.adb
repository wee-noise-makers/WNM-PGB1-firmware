with Ada.Text_IO;
with Ada.Exceptions;
with GNAT.OS_Lib;

with WNM.Project.Step_Sequencer;
with WNM.UI;
with WNM.LEDs;
with WNM.GUI.Update;
with WNM.Synth;
with WNM.File_System;
with WNM.Sample_Library;
with WNM.Time; use WNM.Time;

with WNM.GUI.Menu.Track_Settings;

procedure WNM_PS1_Main is
   Next_Start : Time_Microseconds;

begin

   WNM.File_System.Mount;
   WNM.Sample_Library.Load;

   WNM.GUI.Menu.Track_Settings.Push_Window;

   --  Test_Executor.Start;

   loop
      Next_Start := Time_Microseconds'Last;

      Next_Start := Time_Microseconds'Min (WNM.Synth.Update, Next_Start);
      Next_Start := Time_Microseconds'Min (WNM.Project.Step_Sequencer.Update,
                                           Next_Start);
      Next_Start := Time_Microseconds'Min (WNM.UI.Update, Next_Start);
      Next_Start := Time_Microseconds'Min (WNM.LEDs.Update, Next_Start);
      Next_Start := Time_Microseconds'Min (WNM.GUI.Update.Update, Next_Start);

      Delay_Microseconds (Next_Start);
   end loop;

exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      GNAT.OS_Lib.OS_Exit (1);
end WNM_PS1_Main;

--  Unused : WNM.Time.Time_Microseconds;
--  begin
--     loop
--        WNM_PS1_HAL.Clear_LEDs;
--        WNM_PS1_HAL.Clear_Pixels;
--
--        Unused := WNM.UI.Update;
--        Unused := WNM.UI.;
--
--        declare
--           State : constant WNM_PS1_HAL.Buttons_State :=
--             WNM_PS1_HAL.State;
--        begin
--           for Id in LED loop
--              if State (Id) = Down then
--                 WNM_PS1_HAL.Set (Id, 0, 255, 0);
--                 WNM_PS1_HAL.Set_Pixel (10, 10);
--              end if;
--           end loop;
--        end;
--
--        WNM_PS1_HAL.Update_LEDs;
--        WNM_PS1_HAL.Delay_Milliseconds (100);
--
--        WNM_PS1_HAL.Update_Screen;
--
--     end loop;
--  end WNM_PS1_Main;
