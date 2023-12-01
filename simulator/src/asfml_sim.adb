with Interfaces.C; use Interfaces.C;
with AAA.Debug;
with Ada.Exceptions;
with GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with Sf.Window.Event; use Sf.Window.Event;
with HAL;

with WNM.GUI.Update;
with WNM.Tasks;
with ASFML_Sim.Window;

with ASFML_SIM_Menu;
with Ada.Real_Time; use Ada.Real_Time;

with Tresses.Resources;

package body ASFML_Sim is

   Last_Buffer : System.Address := System.Null_Address;

   ------------- Audio

   procedure RTaudio_Callback (Buf    : System.Address;
                               Frames : Interfaces.C.unsigned);
   pragma Export (C, RTaudio_Callback, "wnm_rtaudio_callback");

   ----------------------
   -- RTaudio_Callback --
   ----------------------

   procedure RTaudio_Callback (Buf : System.Address;
                               Frames : Interfaces.C.unsigned)
   is
      use HAL;
      use System;

      Buffer : System.Address;
      Len    : UInt32;

   begin
      if Frames /= WNM_Configuration.Audio.Samples_Per_Buffer then
         raise Program_Error with "Invalid buffer size from RTAudio: " &
           Frames'Img;
      end if;

      loop
         WNM.Tasks.Synth_Next_Buffer (Buffer, Len);
         exit when Buffer /= Last_Buffer;
      end loop;

      Last_Buffer := Buffer;

      if Len /= WNM_HAL.Stereo_Buffer'Length then
         raise Program_Error with "Invalid buffer size from synth task";
      end if;

      declare
         Synth_Buffer : WNM_HAL.Stereo_Buffer
           with Address => Buffer;
         Out_Buffer : WNM_HAL.Stereo_Buffer
           with Address => Buf;

         subtype S16 is Integer_16;
         subtype S32 is Integer_32;
      begin
         for Idx in Out_Buffer'Range loop
            Out_Buffer (Idx).L :=
              S16 ((S32 (Synth_Buffer (Idx).L) * S32 (Main_Volume)) / 2**15);

            Out_Buffer (Idx).R :=
              S16 ((S32 (Synth_Buffer (Idx).R) * S32 (Main_Volume)) / 2**15);
         end loop;

         if not Audio_Block_Queue.Full then
            Audio_Block_Queue.Insert (Out_Buffer);
         end if;
      end;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         GNAT.OS_Lib.OS_Exit (42);
   end RTaudio_Callback;

   task Periodic_Update is
      entry Start;
      entry Take_Screenshot (Path : String);
   end Periodic_Update;

   task Seq_1kHz_Task is
      entry Start;
   end Seq_1kHz_Task;

   task Core0_Task is
      entry Start;
   end Core0_Task;

   task Core1_Task is
      entry Start;
   end Core1_Task;

   -------------------
   -- Seq_1kHz_Task --
   -------------------

   task body Seq_1kHz_Task is
      Period : constant Time_Span := Milliseconds (1);

      Next_Release : Time;
   begin
      accept Start;

      loop
         Next_Release := Clock + Period;
         delay until Next_Release;

         WNM.Tasks.Sequencer_1khz_Tick;
      end loop;
   end Seq_1kHz_Task;

   ----------------
   -- Core0_Task --
   ----------------

   task body Core0_Task is
   begin
      accept Start;
      WNM.Tasks.Sequencer_Core;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("=== Core0 Task exception ===");
         AAA.Debug.Put_Exception (E);
         GNAT.OS_Lib.OS_Exit (1);
   end Core0_Task;

   ----------------
   -- Core1_Task --
   ----------------

   task body Core1_Task is
   begin
      accept Start;
      WNM.Tasks.Synth_Core;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("=== Core1 Task exception ===");
         AAA.Debug.Put_Exception (E);
         GNAT.OS_Lib.OS_Exit (1);
   end Core1_Task;

   ---------------------
   -- Periodic_Update --
   ---------------------

   task body Periodic_Update is
      Event   : sfEvent;

      Period : constant Time_Span := Milliseconds (1000 / 60);
      Next_Release : Time := Clock + Period;

      Sim_Window : ASFML_Sim.Window.Instance;
   begin

      accept Start;

      Sim_Window.Init;

      loop
         delay until Next_Release;
         Next_Release := Next_Release + Period;

         while Sim_Window.Poll_Event (Event) loop

            if Event.eventType = sfEvtClosed then
               Sim_Window.Close;
               Put_Line ("Attempting to close");
               GNAT.OS_Lib.OS_Exit (0);
            end if;

            if Event.eventType = sfEvtResized then
               Sim_Window.Resize;
            end if;

            if Sim_Clock.Is_Held then
               Sim_Window.Show_Menu (True);
               case ASFML_SIM_Menu.Event_Handler (Event) is
                  when ASFML_SIM_Menu.None =>
                     null;
                  when ASFML_SIM_Menu.Resume =>
                     Sim_Clock.Release;
                  when ASFML_SIM_Menu.Quit =>
                     Sim_Window.Close;
                     Put_Line ("Attempting to close");
                     GNAT.OS_Lib.OS_Exit (0);
               end case;

            else
               Sim_Window.Show_Menu (False);

               if Event.eventType in sfEvtKeyPressed then
                  if Event.key.code = sfKeyEscape then
                     Sim_Clock.Hold;

                  elsif Event.key.code = sfKeyF1 then
                     Sim_Window.Toggle_Side_Panel;

                  elsif Event.key.code = Change_Keyboard_Layout_Key then
                     Next (Current_Layout);

                  elsif Event.key.code = sfKeyF12 then
                     Sim_Window.Screenshot ("WNM-screenshot.png");
                  end if;
               end if;

               if Event.eventType in sfEvtKeyPressed | sfEvtKeyReleased then
                  for K in WNM_Configuration.Button loop
                     if Event.key.code = To_SFML_Evt (K) then
                        SFML_Pressed (K) := Event.eventType = sfEvtKeyPressed;
                     end if;
                  end loop;
               end if;
            end if;
         end loop;

         WNM.GUI.Update.Update;

         select
            accept Take_Screenshot (Path : String) do
               Sim_Window.Screenshot (Path);
            end Take_Screenshot;
         else
            null;
         end select;

         --  if Sim_Clock.Is_Held then
         --     ASFML_SIM_Menu.Render (Window, Font);
         --  end if;

         Sim_Window.Update;

         --  Print_MIDI_Out;

      end loop;
   exception
      when E : others =>
         AAA.Debug.Put_Exception (E);
         GNAT.OS_Lib.OS_Exit (1);
   end Periodic_Update;

   ----------------
   -- Init_Audio --
   ----------------

   procedure Init_Audio is
      function RTaudio_Init (Sample_Rate : Interfaces.C.unsigned;
                             Frames      : Interfaces.C.unsigned)
                             return Interfaces.C.int;
      pragma Import (C, RTaudio_Init, "wnm_rtaudio_init");
   begin

      if GNAT.OS_Lib.Getenv ("OS").all = "Windows_NT" then
         --  Select driver for openal on Windows
         GNAT.OS_Lib.Setenv ("ALSOFT_DRIVERS", "dsound");
      end if;

      if RTaudio_Init (Tresses.Resources.SAMPLE_RATE,
                       WNM_Configuration.Audio.Samples_Per_Buffer) /= 0
      then
         Ada.Text_IO.Put_Line ("rtaudio init error");
         GNAT.OS_Lib.OS_Exit (42);
      end if;

   end Init_Audio;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Sim_Clock.Reset;
      Sim_Clock.Hold;

      Init_Audio;

      Periodic_Update.Start;
      Seq_1kHz_Task.Start;
      Core0_Task.Start;
      Core1_Task.Start;
   end Start;

   ---------------------
   -- Take_Screenshot --
   ---------------------

   procedure Take_Screenshot (Path : String) is
   begin
      Periodic_Update.Take_Screenshot (Path);
   end Take_Screenshot;

end ASFML_Sim;
