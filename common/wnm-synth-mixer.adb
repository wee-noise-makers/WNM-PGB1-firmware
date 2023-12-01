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

with Interfaces;
with Tresses.DSP;
with HAL; use HAL;
with WNM_HAL;
with BBqueue;
with WNM.Coproc;

with System.Storage_Elements; use System.Storage_Elements;

package body WNM.Synth.Mixer is

   Output_Id_Queue_Capacity : constant :=
     Mixer_Buffers'Length + 1;

   Output_Id_Queue : BBqueue.Offsets_Only (Output_Id_Queue_Capacity);

   Output_Ids_Buffer : array (BBqueue.Buffer_Offset range
                                0 .. Output_Id_Queue_Capacity - 1)
       of Mixer_Buffer_Index;

   Output_Audio_Buffers : array (Mixer_Buffer_Index)
     of WNM_HAL.Stereo_Buffer;

   Current_Output_Id : Mixer_Buffer_Index;
   Valid_Current_Output_Id : Boolean := False;

   Zeroes : constant Stereo_Buffer := (others => (0, 0));
   Count_Missed_DAC_Deadlines : HAL.UInt32 := 0 with Volatile, Atomic;

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

      L, R : S32;

      Input : FX_Send_Buffers renames Mixer_Buffers (Id);
      Output : Stereo_Buffer renames Output_Audio_Buffers (Id);
   begin

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
      declare
         use BBqueue;

         WG : Write_Grant;
      begin

         Grant (Output_Id_Queue, WG, 1);

         if State (WG) = Valid then
            Output_Ids_Buffer (Slice (WG).From) := Id;
            Commit (Output_Id_Queue, WG, 1);
         else
            raise Program_Error with "Output buffer queue is full";
         end if;
      end;

      --  Clear the FX buffer
      Input.R := (others => (others => 0));
      Input.L := (others => (others => 0));
   end Push_To_Mix;

   ----------------------
   -- Synth_Out_Buffer --
   ----------------------

   procedure Synth_Out_Buffer (Buffer             : out System.Address;
                               Stereo_Point_Count : out HAL.UInt32)
   is
      use BBqueue;

      RG : Read_Grant;
      Id : Mixer_Buffer_Index;
   begin

      if Valid_Current_Output_Id then

         --  Send the buffer back to the synth to be filled again
         WNM.Coproc.Push_To_Synth ((Kind => WNM.Coproc.Buffer_Available,
                                    Buffer_Id => Current_Output_Id));

         Valid_Current_Output_Id := False;
      end if;

      Read (Output_Id_Queue, RG, 1);

      if State (RG) = Valid then
         Id := Output_Ids_Buffer (Slice (RG).From);

         Release (Output_Id_Queue, RG, 1);

         Buffer := Output_Audio_Buffers (Id)'Address;
         Stereo_Point_Count := Output_Audio_Buffers (Id)'Length;

         Current_Output_Id := Id;
         Valid_Current_Output_Id := True;
      else

         --  We don't have a buffer available, so we send silence...
         Buffer := Zeroes'Address;
         Stereo_Point_Count := Zeroes'Length;

         Valid_Current_Output_Id := False;

         Count_Missed_DAC_Deadlines := Count_Missed_DAC_Deadlines + 1;
      end if;
   end Synth_Out_Buffer;

end WNM.Synth.Mixer;
