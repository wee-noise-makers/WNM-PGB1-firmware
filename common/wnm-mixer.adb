-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2017 Fabien Chouteau                  --
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

with System.Storage_Elements;
with Tresses.DSP;
with HAL; use HAL;
with WNM_HAL;
with WNM.Coproc;
with WNM.Synth; use WNM.Synth;
with WNM.Generic_Queue;
with WNM.Persistent;
with WNM.Sample_Recording;
with WNM.Audio_Routing;
with WNM.Utils;
with WNM.Short_Term_Sequencer;
with WNM.Shared_Buffers;
with BBqueue;

package body WNM.Mixer is

   package Master_FX_Next is new Enum_Next (T    => FX_Kind,
                                            Wrap => False);
   use Master_FX_Next;

   -- Output --

   Output_Id_Queue_Capacity : constant :=
     Mixer_Buffers'Length + 1;

   package Buffer_Id_Queues is new WNM.Generic_Queue
     (Mixer_Buffer_Index, "Mixer out ");

   Output_Id_Queue : Buffer_Id_Queues.Instance (Output_Id_Queue_Capacity);

   Output_Audio_Buffers : array (Mixer_Buffer_Index)
     of WNM_HAL.Stereo_Buffer;

   Current_Output_Id : Mixer_Buffer_Index with Volatile;
   Valid_Current_Output_Id : Boolean := False with Volatile;

   -- Input --

   subtype Input_Buffer_Index
     is System.Storage_Elements.Storage_Count
   range 1 .. WNM_Configuration.Audio.Input_Buffer_Count;

   Input_Audio_Buffers : array (Input_Buffer_Index) of WNM_HAL.Stereo_Buffer;
   Input_Queue : BBqueue.Offsets_Only (Input_Audio_Buffers'Length);
   Input_Queue_WG : BBqueue.Write_Grant;

   Mixer_Perf : WNM.Utils.Perf_Timer;
   Count_Missed_DAC_Deadlines : HAL.UInt32 := 0 with Volatile, Atomic;
   Count_Missed_Input_Deadlines : HAL.UInt32 := 0 with Volatile, Atomic;

   --  Sample recording

   Sample_Rec_State : Sample_Rec_Mode := None
     with Atomic, Volatile;

   --------------------
   -- Mixer_CPU_Load --
   --------------------

   function Mixer_CPU_Load return CPU_Load
   is (WNM.Utils.Load (Mixer_Perf));

   --------------------------
   -- Missed_DAC_Deadlines --
   --------------------------

   function Missed_DAC_Deadlines return HAL.UInt32
   is (Count_Missed_DAC_Deadlines);

   --------------------------------
   -- Clear_Missed_DAC_Deadlines --
   --------------------------------

   procedure Clear_Missed_DAC_Deadlines is
   begin
      Count_Missed_DAC_Deadlines := 0;
   end Clear_Missed_DAC_Deadlines;

   ----------------------------
   -- Missed_Input_Deadlines --
   ----------------------------

   function Missed_Input_Deadlines return HAL.UInt32
   is (Count_Missed_Input_Deadlines);

   ----------------------------------
   -- Clear_Missed_Input_Deadlines --
   ----------------------------------

   procedure Clear_Missed_Input_Deadlines is
   begin
      Count_Missed_Input_Deadlines := 0;
   end Clear_Missed_Input_Deadlines;

   -----------------
   -- Start_Mixer --
   -----------------

   procedure Start_Mixer is
   begin
      --  Send all FX buffers to the Synth coproc
      for Id in Mixer_Buffer_Index loop
         WNM.Coproc.Push_To_Synth ((Kind => WNM.Coproc.Buffer_Available,
                                    Buffer_Id => Id));
      end loop;
   end Start_Mixer;

   -------------------
   -- Process_Input --
   -------------------

   procedure Process_Input (L, R    : in out WNM_HAL.Mono_Buffer;
                            Discard :        Boolean := False)
   is
      use Tresses;
      use Tresses.DSP;
      use BBqueue;

      Input_RG : BBqueue.Read_Grant;
   begin
      --  Handle audio input
      Read (Input_Queue, Input_RG, 1);
      if State (Input_RG) = Valid then
         if not Discard then
            declare
               use System.Storage_Elements;
               Offset : constant Storage_Offset := Slice (Input_RG).From;
               In_Buffer : Stereo_Buffer renames
                 Input_Audio_Buffers (Input_Audio_Buffers'First + Offset);
            begin

               for Index in In_Buffer'Range loop
                  L (Index) :=
                    S16 (Clip_S16 (S32 (@) + S32 (In_Buffer (Index).L)));
                  R (Index) :=
                    S16 (Clip_S16 (S32 (@) + S32 (In_Buffer (Index).R)));
               end loop;
            end;
         end if;

         Release (Input_Queue, Input_RG, 1);
      end if;
   end Process_Input;

   -----------------
   -- Mix_Regular --
   -----------------

   type S32_Buffer is array (WNM_HAL.Mono_Buffer'Range) of Tresses.S32;
   L32, R32 : S32_Buffer;

   procedure Mix_Regular (Input  :        FX_Send_Buffers;
                          Output : in out WNM_HAL.Stereo_Buffer)
   is
      use Tresses;
      use Tresses.DSP;

      LB, RB : WNM_HAL.Mono_Buffer;
      Lane : Project.Project_Mixer_Settings;
   begin
      L32 := (others => 0);
      R32 := (others => 0);

      for Kind in FX_Kind loop
         LB := (others => 0);
         RB := (others => 0);

         for Id in Synth_Tracks loop
            declare
               Chan : constant MIDI.MIDI_Channel := MIDI.MIDI_Channel (Id);
            begin
               if Input.Routing (Chan) = Kind then
                  WNM_HAL.Mix (LB, RB,
                               Input.Buffers (Id),
                               Input.Volume (Chan),
                               Input.Pan (Chan),
                               L_Peak (Id), R_Peak (Id));

                  L_Peak_History (Id) :=
                    S16'Max (L_Peak_History (Id), L_Peak (Id));
                  R_Peak_History (Id) :=
                    S16'Max (R_Peak_History (Id), R_Peak (Id));
               end if;
            end;
         end loop;

         if Kind = Persistent.Data.Input_FX then
            Process_Input (LB, RB);
         end if;

         case Kind is
            when Overdrive =>
               --  Overdrive
               FX_Drive.Set_Param
                 (1, Input.Parameters (Overdrive)(Voice_Param_1_CC));
               FX_Drive.Set_Param
                 (2, Input.Parameters (Overdrive)(Voice_Param_2_CC));
               FX_Drive.Set_Param
                 (3, Input.Parameters (Overdrive)(Voice_Param_3_CC));
               FX_Drive.Set_Param
                 (4, Input.Parameters (Overdrive)(Voice_Param_4_CC));
               FX_Drive.Render (LB, RB);
               Lane := Project.Overdrive_Gain;
            when Reverb =>
               --  Reverb
               FX_Reverb.Set_Param
                 (1, Input.Parameters (Reverb)(Voice_Param_1_CC));
               FX_Reverb.Set_Param
                 (2, Input.Parameters (Reverb)(Voice_Param_2_CC));
               FX_Reverb.Set_Param
                 (3, Input.Parameters (Reverb)(Voice_Param_3_CC));
               FX_Reverb.Set_Param
                 (4, Input.Parameters (Reverb)(Voice_Param_4_CC));
               FX_Reverb.Render (LB, RB);
               Lane := Project.Reverb_Gain;

            when Bitcrusher =>
               --  Bitcrush
               FX_Bitcrush.Set_Param
                 (1, Input.Parameters (Bitcrusher)(Voice_Param_1_CC));
               FX_Bitcrush.Set_Param
                 (2, Input.Parameters (Bitcrusher)(Voice_Param_2_CC));
               FX_Bitcrush.Set_Param
                 (3, Input.Parameters (Bitcrusher)(Voice_Param_3_CC));
               FX_Bitcrush.Set_Param
                 (4, Input.Parameters (Bitcrusher)(Voice_Param_4_CC));
               FX_Bitcrush.Render (LB, RB);
               Lane := Project.Crusher_Gain;

            when Bypass =>
               Lane := Project.Bypass_Gain;
         end case;

         L_Mix_Peak (Lane) := 0;
         R_Mix_Peak (Lane) := 0;

         declare
            Boost : constant S32 :=
              S32 (32767.0 * (1.0 + 0.009999 * Float (Project.Get (Lane))));

            L, R : S32;
         begin

            for Index in Output'Range loop

               L := (S32 (LB (Index)) * Boost) / 2**15;
               L32 (Index) := L32 (Index) + L;

               L_Mix_Peak (Lane) :=
                 S16'Max (L_Mix_Peak (Lane), S16 (Clip_S16 (L)));

               R := (S32 (RB (Index)) * Boost) / 2**15;
               R32 (Index) := R32 (Index) + R;

               R_Mix_Peak (Lane) :=
                 S16'Max (R_Mix_Peak (Lane), S16 (Clip_S16 (R)));

            end loop;

            L_Mix_Peak_History (Lane) :=
              S16'Max (L_Mix_Peak_History (Lane), L_Mix_Peak (Lane));
            R_Mix_Peak_History (Lane) :=
              S16'Max (R_Mix_Peak_History (Lane), R_Mix_Peak (Lane));
         end;
      end loop;

      L_Mix_Peak (Project.Output_Gain) := 0;
      R_Mix_Peak (Project.Output_Gain) := 0;

      declare
         Boost : constant S32 :=
           S32 (32767.0 *
                (1.0 + 0.009999 * Float (Project.Get (Project.Output_Gain))));
      begin

         for Index in Output'Range loop

            L32 (Index) := Clip_S16 (L32 (Index));

            Output (Index).L :=
              S16 (Clip_S16 (((L32 (Index) * Boost) / 2**15)));

            L_Mix_Peak (Project.Output_Gain) :=
              S16'Max (L_Mix_Peak (Project.Output_Gain), Output (Index).L);

            R32 (Index) := Clip_S16 (R32 (Index));

            Output (Index).R :=
              S16 (Clip_S16 (((R32 (Index) * Boost) / 2**15)));

            R_Mix_Peak (Project.Output_Gain) :=
              S16'Max (R_Mix_Peak (Project.Output_Gain), Output (Index).R);
         end loop;

         L_Mix_Peak_History (Project.Output_Gain) :=
           S16'Max (L_Mix_Peak_History (Project.Output_Gain),
                    L_Mix_Peak (Project.Output_Gain));
         R_Mix_Peak_History (Project.Output_Gain) :=
           S16'Max (R_Mix_Peak_History (Project.Output_Gain),
                    R_Mix_Peak (Project.Output_Gain));
      end;

      Voices.Auto_Filter_FX.Render (FX_Auto_Filter, Output);

      Voices.Stutter_FX.Render (FX_Stutter, Output);
   end Mix_Regular;

   ------------------
   -- Mix_Sampling --
   ------------------

   procedure Mix_Sampling (Input  :        FX_Send_Buffers;
                           Output : in out WNM_HAL.Stereo_Buffer)
   is
      use Tresses;
      LB, RB : WNM_HAL.Mono_Buffer := (others => 0);
   begin
      --  Handle Audio Input

      case Sample_Rec_State is
         when Preview | Rec =>

            --  Pass the input to the output for preview of what we are
            --  recording.
            Process_Input (LB, RB, Discard => False);

            --  If in rec mode, save incoming data
            if Sample_Rec_State = Rec then
               declare
                  Mono : WNM_HAL.Mono_Buffer;
                  --  use Interfaces;
               begin
                  for Index in Output'Range loop
                     Mono (Index) :=
                       S16 ((S32 (LB (Index)) +
                                S32 (LB (Index))) / 2);

                     --  if ASFML_Sim.Put_Some_Noise then
                     --     Mono (Index) := S16 (Random) * 150;
                     --  end if;
                  end loop;
                  WNM.Sample_Recording.Record_Buffer (Mono);
               end;
            end if;

         when None | Play | Saving =>
            --  We don't use the input in this stage, but we still want to
            --  consume the buffer. So the input is read into the overdrive
            --  channel that is not used in this mode. We cannot use the
            --  bypass channel as it may contains sample playback comming
            --  from synth CPU.
            Process_Input (LB, RB, Discard => True);

            WNM_HAL.Mix (LB, RB,
                         Input.Buffers (Input.Buffers'First),
                         100, 50,
                         L_Peak (L_Peak'First), R_Peak (R_Peak'First));
      end case;

      --  Input.[L|R] (Bypass) contains either the input audio or playback
      --  from synth CPU.
      for Index in Output'Range loop
         Output (Index).L := LB (Index);
         Output (Index).R := RB (Index);
      end loop;
   end Mix_Sampling;

   -----------------
   -- Push_To_Mix --
   -----------------

   procedure Push_To_Mix (Id : Mixer_Buffer_Index) is
      Input : FX_Send_Buffers renames Mixer_Buffers (Id);
      Output : WNM_HAL.Stereo_Buffer renames Output_Audio_Buffers (Id);
   begin
      WNM.Utils.Start (Mixer_Perf);

      case Sample_Rec_State is
         when None =>
            Mix_Regular (Input, Output);
         when Preview | Rec | Play | Saving =>
            Mix_Sampling (Input, Output);
      end case;

      --  Push the mixed output buffer in ready queue
      Buffer_Id_Queues.Push (Output_Id_Queue, Id);

      WNM.Utils.Stop (Mixer_Perf);
   end Push_To_Mix;

   ---------------------
   -- Next_Out_Buffer --
   ---------------------

   procedure Next_Out_Buffer (Buffer             : out System.Address;
                               Stereo_Point_Count : out HAL.UInt32)
   is
      Id : Mixer_Buffer_Index;
      Success : Boolean;
   begin

      if Valid_Current_Output_Id then

         --  Send the buffer back to the synth to be filled again
         WNM.Coproc.Push_To_Synth ((Kind => WNM.Coproc.Buffer_Available,
                                    Buffer_Id => Current_Output_Id));

         Valid_Current_Output_Id := False;
      end if;

      Buffer_Id_Queues.Pop (Output_Id_Queue, Id, Success);

      if Success then

         Buffer := Output_Audio_Buffers (Id)'Address;
         Stereo_Point_Count := Output_Audio_Buffers (Id)'Length;

         Current_Output_Id := Id;
         Valid_Current_Output_Id := True;
      else

         --  We don't have a buffer available
         Buffer := System.Null_Address;
         Stereo_Point_Count := 0;

         Valid_Current_Output_Id := False;

         Count_Missed_DAC_Deadlines := Count_Missed_DAC_Deadlines + 1;
      end if;
   end Next_Out_Buffer;

   --------------------
   -- Next_In_Buffer --
   --------------------

   procedure Next_In_Buffer (Buffer             : out System.Address;
                             Stereo_Point_Count : out HAL.UInt32)
   is
      use System.Storage_Elements;
      use BBqueue;
   begin
      if State (Input_Queue_WG) = Valid then
         Commit (Input_Queue, Input_Queue_WG, 1);
      end if;

      Grant (Input_Queue, Input_Queue_WG, 1);

      if State (Input_Queue_WG) = Valid then
         declare
            Offset : constant Storage_Offset := Slice (Input_Queue_WG).From;
            In_Buffer : Stereo_Buffer renames
              Input_Audio_Buffers (Input_Audio_Buffers'First + Offset);
         begin
            Buffer := In_Buffer'Address;
            Stereo_Point_Count := In_Buffer'Length;
         end;

      else
         --  We don't have a buffer available
         Buffer := System.Null_Address;
         Stereo_Point_Count := 0;

         Count_Missed_Input_Deadlines := Count_Missed_Input_Deadlines + 1;
      end if;
   end Next_In_Buffer;

   -------------------
   -- Input_FX_Next --
   -------------------

   procedure Input_FX_Next is
   begin
      Next (WNM.Persistent.Data.Input_FX);
   end Input_FX_Next;

   -------------------
   -- Input_FX_Prev --
   -------------------

   procedure Input_FX_Prev is
   begin
      Prev (WNM.Persistent.Data.Input_FX);
   end Input_FX_Prev;

   --------------
   -- Input_FX --
   --------------

   function Input_FX return FX_Kind
   is (WNM.Persistent.Data.Input_FX);

   ---------------------
   -- Set_Auto_Filter --
   ---------------------

   procedure Set_Auto_Filter (Mode : Voices.Auto_Filter_FX.Mode_Kind) is
   begin
      Voices.Auto_Filter_FX.Set_Mode (FX_Auto_Filter, Mode);
   end Set_Auto_Filter;

   ----------------------
   -- Auto_Filter_Mode --
   ----------------------

   function Auto_Filter_Mode return Voices.Auto_Filter_FX.Mode_Kind
   is (Voices.Auto_Filter_FX.Mode (FX_Auto_Filter));

   -----------------
   -- Set_Stutter --
   -----------------

   procedure Set_Stutter (Mode : Voices.Stutter_FX.Mode_Kind) is
   begin
      Voices.Stutter_FX.Set_Mode (FX_Stutter, Mode);
   end Set_Stutter;

   ------------------
   -- Stutter_Mode --
   ------------------

   function Stutter_Mode return Voices.Stutter_FX.Mode_Kind
   is (Voices.Stutter_FX.Mode (FX_Stutter));

   ---------------------------
   -- Enter_Sample_Rec_Mode --
   ---------------------------

   procedure Enter_Sample_Rec_Mode (Mode : Sample_Rec_Mode) is
   begin
      if Mode = None and then Sample_Rec_State /= None then
         WNM.Audio_Routing.Leave_Sampling;
         WNM.Shared_Buffers.Clear_Synth_Buffers;
         WNM.Sample_Recording.Reset;
         WNM.Short_Term_Sequencer.Restart;

      elsif Mode /= None and then Sample_Rec_State = None then
         WNM.Short_Term_Sequencer.Halt;
         WNM.Audio_Routing.Enter_Sampling;

      end if;

      Sample_Rec_State := Mode;

      if Mode = Rec then
         --  Going back to record mode, we may have to disable the speaker
         --  when recording from internal mic.
         WNM.Audio_Routing.Periodic_Update;
      end if;

   end Enter_Sample_Rec_Mode;

   -------------------------
   -- Get_Sample_Rec_Mode --
   -------------------------

   function Get_Sample_Rec_Mode return Sample_Rec_Mode
   is (Sample_Rec_State);

end WNM.Mixer;
