with Interfaces.C; use Interfaces.C;
with AAA.Debug;
with Ada.Exceptions;
with GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with Sf.Window.Event; use Sf.Window.Event;

with WNM.Synth;
with WNM.GUI.Update;

with ASFML_Sim.Window;

with ASFML_SIM_Menu;
with Ada.Real_Time; use Ada.Real_Time;

with Tresses.Resources;

package body ASFML_Sim is

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
      In_Buffer : constant WNM_HAL.Stereo_Buffer := (others => (0, 0));

      Out_Buffer : WNM_HAL.Stereo_Buffer
        with Address => Buf;
   begin
      if Frames /= WNM_Configuration.Audio.Samples_Per_Buffer then
         raise Program_Error with "Invalid buffer size from RTAudio: " &
           Frames'Img;
      end if;

      WNM.Synth.Next_Points (Out_Buffer, In_Buffer);

      declare
         subtype S16 is Integer_16;
         subtype S32 is Integer_32;
      begin
         for Elt of Out_Buffer loop
            Elt.L := S16 ((S32 (Elt.L) * S32 (Main_Volume)) / 2**15);
            Elt.R := S16 ((S32 (Elt.R) * S32 (Main_Volume)) / 2**15);
         end loop;
      end;

      if not Audio_Block_Queue.Full then
         Audio_Block_Queue.Insert (Out_Buffer);
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         GNAT.OS_Lib.OS_Exit (42);
   end RTaudio_Callback;

   task Periodic_Update is
      entry Start;
      entry Take_Screenshot (Path : String);
   end Periodic_Update;

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

                  elsif Event.key.code = sfKeyRight then
                     Encoder_Left := (if Event.key.shift then 2 else 1);
                  elsif Event.key.code = sfKeyLeft then
                     Encoder_Left := (if Event.key.shift then -2 else -1);
                  elsif Event.key.code = sfKeyDown then
                     Encoder_Right := (if Event.key.shift then -2 else -1);
                  elsif Event.key.code = sfKeyUp then
                     Encoder_Right := (if Event.key.shift then 2 else 1);
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
   end Start;

   ---------------------
   -- Take_Screenshot --
   ---------------------

   procedure Take_Screenshot (Path : String) is
   begin
      Periodic_Update.Take_Screenshot (Path);
   end Take_Screenshot;

end ASFML_Sim;
