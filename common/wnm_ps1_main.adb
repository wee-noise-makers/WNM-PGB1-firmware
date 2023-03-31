with Ada.Text_IO;
with Ada.Exceptions;
with GNAT.OS_Lib;

with HAL; use HAL;

with WNM.Short_Term_Sequencer;
with WNM.Note_Off_Sequencer;

with WNM.UI;
with WNM.File_System;
with WNM.Sample_Library;
with WNM.Project.Library;
with WNM.Persistent;
with WNM.MIDI_Clock;
with WNM.Time; use WNM.Time;

with WNM.GUI.Menu.Track_Settings;

procedure WNM_PS1_Main is
   Next_Start : Time_Microseconds;

   Loop_Cnt : UInt32 := 0;
begin

   WNM.File_System.Mount;
   WNM.Persistent.Load;
   WNM.Sample_Library.Load;
   WNM.Project.Library.Load_Library;

   WNM.GUI.Menu.Track_Settings.Push_Window;

   --  Test_Executor.Start;

   Next_Start := Clock;

   loop
      Next_Start := Next_Start + Milliseconds (1);
      Loop_Cnt := Loop_Cnt + 1;

      WNM.MIDI_Clock.Update;
      WNM.Short_Term_Sequencer.Update (Clock);
      WNM.Note_Off_Sequencer.Update (Clock);

      if (Loop_Cnt mod 50) = 0 then
         WNM.UI.Update;
      end if;

      if (Loop_Cnt mod 100) = 0 then
         WNM.UI.Update_LEDs;
      end if;

      Delay_Until (Next_Start);
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
