-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                    Copyright (C) 2024 Fabien Chouteau                     --
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

with WNM_HAL;

package WNM.Audio_Routing is

   procedure Periodic_Update;

   procedure Enter_Sampling;
   procedure Select_Sampling_Input (Kind : WNM_HAL.Audio_Input_Kind);
   procedure Leave_Sampling;

   procedure Change_Main_Volume (Volume_Delta : Integer);
   procedure Set_Main_Volume (Volume : Audio_Volume);
   function Get_Main_Volume return Audio_Volume;

   procedure Change_ADC_Volume (Volume_Delta : Integer);
   function Get_ADC_Volume return Audio_Volume;

   procedure Toggle_Internal_Mic_Mute;
   function Get_Internal_Mic_Mute return Boolean;

   procedure Toggle_Headset_Mic_Mute;
   function Get_Headset_Mic_Mute return Boolean;

   procedure Toggle_Line_In_Mute;
   function Get_Line_In_Mute return Boolean;

end WNM.Audio_Routing;
