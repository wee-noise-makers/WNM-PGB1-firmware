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

with System;
with HAL;

with WNM.Synth;
with WNM.Voices.Reverb_Voice;
with WNM.Voices.Drive_Voice;
with WNM.Voices.Bitcrusher_Voice;
with WNM.Voices.Auto_Filter_FX;
with WNM.Voices.Stutter_FX;

package WNM.Mixer is

   type FX_Buffers is array (FX_Kind) of WNM_HAL.Mono_Buffer;

   type FX_Parameters  is array (FX_Kind) of WNM.Synth.Voice_Parameters;

   type FX_Send_Buffers is record
      L, R : FX_Buffers;

      Parameters : FX_Parameters;
   end record;

   Mixer_Buffers : array (Mixer_Buffer_Index) of aliased FX_Send_Buffers;

   procedure Start_Mixer;

   procedure Push_To_Mix (Id : Mixer_Buffer_Index);

   procedure Next_Out_Buffer (Buffer             : out System.Address;
                              Stereo_Point_Count : out HAL.UInt32);

   procedure Next_In_Buffer (Buffer             : out System.Address;
                             Stereo_Point_Count : out HAL.UInt32);

   procedure Input_FX_Next;
   procedure Input_FX_Prev;
   function Input_FX return FX_Kind;

   function Mixer_CPU_Load return CPU_Load;
   function Missed_DAC_Deadlines return HAL.UInt32;
   procedure Clear_Missed_DAC_Deadlines;

   function Missed_Input_Deadlines return HAL.UInt32;
   procedure Clear_Missed_Input_Deadlines;

   FX_Reverb   : aliased WNM.Voices.Reverb_Voice.Instance;
   FX_Drive    : aliased WNM.Voices.Drive_Voice.Instance;
   FX_Bitcrush : aliased WNM.Voices.Bitcrusher_Voice.Instance;
   FX_Stutter  : WNM.Voices.Stutter_FX.Instance;
   FX_Auto_Filter : WNM.Voices.Auto_Filter_FX.Instance;

   procedure Set_Auto_Filter (Mode : Voices.Auto_Filter_FX.Mode_Kind);
   function Auto_Filter_Mode return Voices.Auto_Filter_FX.Mode_Kind;

   procedure Set_Stutter (Mode : Voices.Stutter_FX.Mode_Kind);
   function Stutter_Mode return Voices.Stutter_FX.Mode_Kind;

   type Sample_Rec_Mode is (None, Preview, Rec, Play, Saving);

   procedure Enter_Sample_Rec_Mode (Mode : Sample_Rec_Mode);
   function Get_Sample_Rec_Mode return Sample_Rec_Mode;

end WNM.Mixer;
