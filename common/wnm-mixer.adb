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
with Interfaces;
with Tresses.DSP;
with HAL; use HAL;
with WNM_HAL;
with WNM.Coproc;
with WNM.Synth; use WNM.Synth;
with WNM.Generic_Queue;
with WNM.Persistent;
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
     is System.Storage_Elements.Storage_Count range 1 .. 3;
   Input_Audio_Buffers : array (Input_Buffer_Index) of WNM_HAL.Stereo_Buffer;
   Input_Queue : BBqueue.Offsets_Only (Input_Audio_Buffers'Length);
   Input_Queue_WG : BBqueue.Write_Grant;

   Count_Missed_DAC_Deadlines : HAL.UInt32 := 0 with Volatile, Atomic;
   Count_Missed_Input_Deadlines : HAL.UInt32 := 0 with Volatile, Atomic;

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

   -----------------
   -- Push_To_Mix --
   -----------------

   procedure Push_To_Mix (Id : Mixer_Buffer_Index) is
      use Tresses;
      use Tresses.DSP;
      use Interfaces;
         use BBqueue;

      L, R : S32;

      Input : FX_Send_Buffers renames Mixer_Buffers (Id);
      Output : WNM_HAL.Stereo_Buffer renames Output_Audio_Buffers (Id);

      Input_RG : BBqueue.Read_Grant;
   begin

      --  Handle audio input
      Read (Input_Queue, Input_RG, 1);
      if State (Input_RG) = Valid then
         declare
            use System.Storage_Elements;
            Offset : constant Storage_Offset := Slice (Input_RG).From;
            In_Buffer : Stereo_Buffer renames
              Input_Audio_Buffers (Input_Audio_Buffers'First + Offset);

            FX : constant FX_Kind := Persistent.Data.Input_FX;
         begin
            for Index in In_Buffer'Range loop
               Input.L (FX)(Index) :=
                 S16 (Clip_S16 (S32 (@) + S32 (In_Buffer (Index).L)));
               Input.R (FX)(Index) :=
                 S16 (Clip_S16 (S32 (@) + S32 (In_Buffer (Index).R)));
            end loop;
         end;

         Release (Input_Queue, Input_RG, 1);
      end if;

      --  Overdrive
      FX_Drive.Set_Param (1, Input.Parameters (Overdrive)(Voice_Param_1_CC));
      FX_Drive.Set_Param (2, Input.Parameters (Overdrive)(Voice_Param_2_CC));
      FX_Drive.Set_Param (3, Input.Parameters (Overdrive)(Voice_Param_3_CC));
      FX_Drive.Set_Param (4, Input.Parameters (Overdrive)(Voice_Param_4_CC));
      FX_Drive.Render (Input.L (Overdrive), Input.R (Overdrive));

      --  Reverb
      FX_Reverb.Set_Param (1, Input.Parameters (Reverb)(Voice_Param_1_CC));
      FX_Reverb.Set_Param (2, Input.Parameters (Reverb)(Voice_Param_2_CC));
      FX_Reverb.Set_Param (3, Input.Parameters (Reverb)(Voice_Param_3_CC));
      FX_Reverb.Set_Param (4, Input.Parameters (Reverb)(Voice_Param_4_CC));
      FX_Reverb.Render (Input.L (Reverb), Input.R (Reverb));

      --  Filter
      FX_Filter.Set_Param (1, Input.Parameters (Filter)(Voice_Param_1_CC));
      FX_Filter.Set_Param (2, Input.Parameters (Filter)(Voice_Param_2_CC));
      FX_Filter.Set_Param (3, Input.Parameters (Filter)(Voice_Param_3_CC));
      FX_Filter.Set_Param (4, Input.Parameters (Filter)(Voice_Param_4_CC));
      FX_Filter.Render (Input.L (Filter), Input.R (Filter));

      --  Bitcrush
      FX_Bitcrush.Set_Param (1,
                             Input.Parameters (Bitcrusher)(Voice_Param_1_CC));
      FX_Bitcrush.Set_Param (2,
                             Input.Parameters (Bitcrusher)(Voice_Param_2_CC));
      FX_Bitcrush.Set_Param (3,
                             Input.Parameters (Bitcrusher)(Voice_Param_3_CC));
      FX_Bitcrush.Set_Param (4,
                             Input.Parameters (Bitcrusher)(Voice_Param_4_CC));
      FX_Bitcrush.Render (Input.L (Bitcrusher), Input.R (Bitcrusher));

      for Index in Output'Range loop
         L := S32 (Input.L (Bypass)(Index)) +
           S32 (Input.L (Overdrive)(Index)) +
             S32 (Input.L (Reverb)(Index)) +
               S32 (Input.L (Bitcrusher)(Index)) +
                 S32 (Input.L (Filter)(Index));

         R := S32 (Input.R (Bypass)(Index)) +
           S32 (Input.R (Overdrive)(Index)) +
             S32 (Input.R (Reverb)(Index)) +
               S32 (Input.R (Bitcrusher)(Index)) +
                 S32 (Input.R (Filter)(Index));

         Output (Index).L := S16 (Clip_S16 (L));
         Output (Index).R := S16 (Clip_S16 (R));
      end loop;

      --  Push the mixed output buffer in ready queue
      Buffer_Id_Queues.Push (Output_Id_Queue, Id);

      --  Clear the FX buffer
      Input.R := (others => (others => 0));
      Input.L := (others => (others => 0));
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
   -- Volume_Change --
   -------------------

   procedure Volume_Change (V : in out Audio_Volume; Delt : Integer) is
      Res : Integer;
   begin
      Res := Integer (V) + Delt;
      if Res in Integer (Audio_Volume'First) .. Integer (Audio_Volume'Last)
      then
         V := Audio_Volume (Res);
      end if;
   end Volume_Change;

   ------------------------
   -- Change_Main_Volume --
   ------------------------

   procedure Change_Main_Volume (Volume_Delta : Integer) is
   begin
      Volume_Change (Persistent.Data.Main_Volume, Volume_Delta);
      WNM_HAL.Set_Main_Volume (Persistent.Data.Main_Volume);
   end Change_Main_Volume;

   ---------------------
   -- Set_Main_Volume --
   ---------------------

   procedure Set_Main_Volume (Volume : Audio_Volume) is
   begin
      Persistent.Data.Main_Volume := Volume;
      WNM_HAL.Set_Main_Volume (Persistent.Data.Main_Volume);
   end Set_Main_Volume;

   ---------------------
   -- Get_Main_Volume --
   ---------------------

   function Get_Main_Volume return Audio_Volume
   is (WNM.Persistent.Data.Main_Volume);

   --------------------------------
   -- Change_Internal_Mic_Volume --
   --------------------------------

   procedure Change_Internal_Mic_Volume (Volume_Delta : Integer) is
   begin
      Volume_Change (Persistent.Data.Internal_Mic_Volume, Volume_Delta);
      WNM_HAL.Set_Mic_Volumes (Persistent.Data.Headset_Mic_Volume,
                               Persistent.Data.Internal_Mic_Volume);
   end Change_Internal_Mic_Volume;

   -----------------------------
   -- Get_Internal_Mic_Volume --
   -----------------------------

   function Get_Internal_Mic_Volume return Audio_Volume
   is (WNM.Persistent.Data.Internal_Mic_Volume);

   -------------------------------
   -- Change_Headset_Mic_Volume --
   -------------------------------

   procedure Change_Headset_Mic_Volume (Volume_Delta : Integer) is
   begin
      Volume_Change (Persistent.Data.Headset_Mic_Volume, Volume_Delta);
      WNM_HAL.Set_Mic_Volumes (Persistent.Data.Headset_Mic_Volume,
                               Persistent.Data.Internal_Mic_Volume);
   end Change_Headset_Mic_Volume;

   ----------------------------
   -- Get_Headset_Mic_Volume --
   ----------------------------

   function Get_Headset_Mic_Volume return Audio_Volume
   is (WNM.Persistent.Data.Headset_Mic_Volume);

   ---------------------------
   -- Change_Line_In_Volume --
   ---------------------------

   procedure Change_Line_In_Volume (Volume_Delta : Integer) is
   begin
      Volume_Change (Persistent.Data.Line_In_Volume, Volume_Delta);
      WNM_HAL.Set_Line_In_Volume (Persistent.Data.Line_In_Volume);
   end Change_Line_In_Volume;

   ------------------------
   -- Get_Line_In_Volume --
   ------------------------

   function Get_Line_In_Volume return Audio_Volume
   is (WNM.Persistent.Data.Line_In_Volume);

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

end WNM.Mixer;
