-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2023 Fabien Chouteau                  --
--                                                                           --
--    Wee Noise Maker is free software: you can redistribute it and/or       --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Maker is distributed in the hope that it will be useful,     --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with We Noise Maker. If not, see <http://www.gnu.org/licenses/>. --
--                                                                           --
-------------------------------------------------------------------------------

with HAL; use HAL;

with WNM.MIDI_Clock;
with WNM.Time;
with WNM.Short_Term_Sequencer;
with WNM.Note_Off_Sequencer;
with WNM.UI;
with WNM.GUI.Update;
with WNM.GUI.Menu;
with WNM.Synth;
with WNM.File_System;
with WNM.Persistent;
with WNM.Sample_Library;
with WNM.Project.Library;
with WNM.Mixer;
with WNM.Coproc;
with WNM.Project_Load_Broadcast;
with WNM.Power_Control;
with WNM.Audio_Routing;

with MIDI;

package body WNM.Tasks is

   Systick_Count : UInt32 := 0;
   UI_Period_Miliseconds  : constant := 20;
   LED_Period_Miliseconds : constant := 30;
   HP_Detect_Period_Miliseconds : constant := 250;
   Rand_Update_Period : Rand_Percent := 0;

   --------------------
   -- Handle_MIDI_In --
   --------------------

   procedure Handle_MIDI_In is
      use MIDI;
      Msg : MIDI.Message;
      Success : Boolean;
   begin
      loop
         WNM_HAL.Get_External (Msg, Success);
         exit when not Success;

         case Msg.Kind is
            when MIDI.Sys =>
               case Msg.Cmd is
                  when MIDI.Start_Song =>

                     if Persistent.Data.MIDI_Clock_Input then
                        WNM.MIDI_Clock.External_Start;
                     end if;

                  when MIDI.Stop_Song =>
                     WNM.MIDI_Clock.External_Stop;
                  when MIDI.Continue_Song =>

                     if Persistent.Data.MIDI_Clock_Input then
                        WNM.MIDI_Clock.External_Continue;
                     end if;

                  when MIDI.Timming_Tick =>
                     WNM.MIDI_Clock.External_Tick;
                  when others =>
                     null;
               end case;

            when Note_On | Note_Off | Continous_Controller =>
               if Msg.Chan = 0 then
                  WNM.Project.Handle_MIDI (Msg);
               else
                  Coproc.Push_To_Synth ((Kind     => Coproc.MIDI_Event,
                                         MIDI_Evt => Msg));
               end if;

            when others =>
               null;

         end case;
      end loop;
   end Handle_MIDI_In;

   -------------------------
   -- Sequencer_1khz_Tick --
   -------------------------

   procedure Sequencer_1khz_Tick is
   begin
      WNM_HAL.Watchdog_Check;

      WNM.MIDI_Clock.Update;
      WNM.Short_Term_Sequencer.Update (Clock);
      WNM.Note_Off_Sequencer.Update (Clock);

      --  Tentatively adding more entropy by calling random at random
      --  intervals...
      if Systick_Count mod (UInt32 (Rand_Update_Period) + 1) = 0 then
         Rand_Update_Period := Random;
      end if;

      if (Systick_Count mod UI_Period_Miliseconds) = 0 then

         if WNM_HAL.Shutdown_Requested then
            WNM.Power_Control.Power_Down;
         else
            WNM.UI.Update;
         end if;
      end if;

      if (Systick_Count mod LED_Period_Miliseconds) = 0 then
         WNM.UI.Update_LEDs;
         WNM_HAL.Read_Battery_Voltage;
      end if;

      if (Systick_Count mod HP_Detect_Period_Miliseconds) = 0 then

         WNM.Audio_Routing.Periodic_Update;
      end if;

      Handle_MIDI_In;

      --  Try to flush MIDI external output after all clock, sequencer and UI
      --  updates that can all potentially send external MIDI data.
      WNM_HAL.Flush_Output;

      Systick_Count := Systick_Count + 1;
   end Sequencer_1khz_Tick;

   ------------------------------
   -- Sequencer_Coproc_Receive --
   ------------------------------

   procedure Sequencer_Coproc_Receive is
      use WNM.Coproc;

      Msg     : WNM.Coproc.Message;
      Success : Boolean;
   begin

      loop
         WNM.Coproc.Pop_For_Main (Msg, Success);

         exit when not Success;

         case Msg.Kind is
            when Coproc.Buffer_Available =>
               WNM.Mixer.Push_To_Mix (Msg.Buffer_Id);

            when Synth_CPU_Crash =>
               raise Program_Error with "Synth crash";

            when MIDI_Event =>
               raise Program_Error with "MIDI event in seq coproc queue";
         end case;
      end loop;
   end Sequencer_Coproc_Receive;

   --------------------------
   -- Synth_Coproc_Receive --
   --------------------------

   procedure Synth_Coproc_Receive is
      Msg     : WNM.Coproc.Message;
      Success : Boolean;
   begin

      loop
         WNM.Coproc.Pop_For_Synth (Msg, Success);
         exit when not Success;

         WNM.Synth.Push_Copro_Event (Msg);
      end loop;
   end Synth_Coproc_Receive;

   --------------------
   -- Sequencer_Core --
   --------------------

   procedure Sequencer_Core is
   begin

      if WNM_HAL.Get_LFS_Config /= null then
         WNM.File_System.Mount;
         WNM.Persistent.Load;
         WNM.Project.Library.Load_Library;
         WNM.Sample_Library.Load;
      end if;

      WNM.GUI.Menu.Open (WNM.GUI.Menu.Chord_Menu);

      WNM.Mixer.Start_Mixer;

      --  Trigger a project load event to setup various things like current
      --  chord or track settings.
      WNM.Project_Load_Broadcast.Broadcast;

      WNM_HAL.Start_Sequencer_Tick;
      WNM_HAL.Watchdog_Init;

      loop
         WNM.GUI.Update.Update;
         WNM.Time.Delay_Milliseconds (1000 / 30);
      end loop;
   end Sequencer_Core;

   ------------------------
   -- Next_Output_Buffer --
   ------------------------

   procedure Next_Output_Buffer (Buffer             : out System.Address;
                                Stereo_Point_Count : out HAL.UInt32)
   is
   begin
      WNM.Mixer.Next_Out_Buffer (Buffer, Stereo_Point_Count);
   end Next_Output_Buffer;

   -----------------------
   -- Next_Input_Buffer --
   -----------------------

   procedure Next_Input_Buffer (Buffer             : out System.Address;
                                Stereo_Point_Count : out HAL.UInt32)
   is
   begin
      WNM.Mixer.Next_In_Buffer (Buffer, Stereo_Point_Count);
   end Next_Input_Buffer;

   ----------------
   -- Synth_Core --
   ----------------

   procedure Synth_Core is
   begin
      loop
         --  During file-system operations, the synth CPU must not access the
         --  flash memory. This procedure will check if there is a request to
         --  hold the synth CPU for file-system operation.
         --  TODO: Run the hold check between every coproc messages, instead
         --  of handling messages in batches?
         WNM_HAL.Synth_CPU_Check_Hold;

         --  Read and process coproc events
         WNM.Synth.Process_Coproc_Events;
      end loop;
   end Synth_Core;

end WNM.Tasks;
