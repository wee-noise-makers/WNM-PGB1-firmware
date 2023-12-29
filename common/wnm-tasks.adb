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
with WNM.GUI.Menu.Track_Settings;
with WNM.Synth;
with WNM.File_System;
with WNM.Persistent;
with WNM.Sample_Library;
with WNM.Project.Library;
with WNM.Mixer;
with WNM.Coproc;

package body WNM.Tasks is

   Systick_Count : UInt32 := 0;
   UI_Period_Miliseconds : constant := 20;
   LED_Period_Miliseconds : constant := 100;

   -------------------------
   -- Sequencer_1khz_Tick --
   -------------------------

   procedure Sequencer_1khz_Tick is
   begin
      WNM_HAL.Watchdog_Check;

      WNM_HAL.Set_Indicator_IO (WNM_HAL.GP17);
      WNM.MIDI_Clock.Update;
      WNM.Short_Term_Sequencer.Update (Clock);
      WNM.Note_Off_Sequencer.Update (Clock);

      if (Systick_Count mod UI_Period_Miliseconds) = 0 then
         WNM.UI.Update;
      end if;

      if (Systick_Count mod LED_Period_Miliseconds) = 0 then
         WNM.UI.Update_LEDs;
      end if;

      Systick_Count := Systick_Count + 1;
      WNM_HAL.Clear_Indicator_IO (WNM_HAL.GP17);
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
               WNM_HAL.Set_Indicator_IO (WNM_HAL.GP16);
               WNM_HAL.Clear_Indicator_IO (WNM_HAL.GP16);
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
      use WNM.Coproc;

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
      end if;

      WNM.Sample_Library.Load;

      WNM.GUI.Menu.Track_Settings.Push_Window;

      WNM.Mixer.Start_Mixer;

      WNM_HAL.Start_Sequencer_Tick;
      WNM_HAL.Watchdog_Init;

      loop
         WNM.GUI.Update.Update;
         WNM.Time.Delay_Milliseconds (1000 / 30);
      end loop;
   end Sequencer_Core;

   -----------------------
   -- Synth_Next_Buffer --
   -----------------------

   procedure Synth_Next_Buffer (Buffer             : out System.Address;
                                Stereo_Point_Count : out HAL.UInt32)
   is
   begin
      WNM_HAL.Set_Indicator_IO (WNM_HAL.GP18);
      WNM_HAL.Clear_Indicator_IO (WNM_HAL.GP18);
      WNM.Mixer.Synth_Out_Buffer (Buffer, Stereo_Point_Count);
   end Synth_Next_Buffer;

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
