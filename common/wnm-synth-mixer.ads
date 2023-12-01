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

with WNM.Voices.Reverb_Voice;
with WNM.Voices.Filter_Voice;
with WNM.Voices.Drive_Voice;
with WNM.Voices.Bitcrusher_Voice;

package WNM.Synth.Mixer is

   type FX_Buffers
   is array (WNM.Synth.FX_Kind) of WNM_HAL.Mono_Buffer;

   type FX_Parameters
   is array (WNM.Synth.FX_Kind) of WNM.Synth.Voice_Parameters;

   type FX_Send_Buffers is record
      L, R : FX_Buffers;

      Parameters : FX_Parameters;
   end record;

   type Mixer_Buffer_Index is range 0 .. 20
     with Size => 8;

   Mixer_Buffers : array (Mixer_Buffer_Index) of aliased FX_Send_Buffers;

   procedure Start_Mixer;

   procedure Push_To_Mix (Id : Mixer_Buffer_Index);

   procedure Synth_Out_Buffer (Buffer             : out System.Address;
                               Stereo_Point_Count : out HAL.UInt32);

   function Missed_DAC_Deadlines return HAL.UInt32;
   procedure Clear_Missed_DAC_Deadlines;

   FX_Reverb   : aliased WNM.Voices.Reverb_Voice.Instance;
   FX_Filter   : aliased WNM.Voices.Filter_Voice.Instance;
   FX_Drive    : aliased WNM.Voices.Drive_Voice.Instance;
   FX_Bitcrush : aliased WNM.Voices.Bitcrusher_Voice.Instance;

end WNM.Synth.Mixer;
