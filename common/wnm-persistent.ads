-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2022 Fabien Chouteau                    --
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

with WNM.Project.Library;
with HAL;

package WNM.Persistent is

   type Persistent_Data is record
      Last_Project        : WNM.Project.Library.Valid_Prj_Index;
      Main_Volume         : Audio_Volume;
      Line_In_Mute        : Boolean;
      Internal_Mic_Mute   : Boolean;
      Headset_Mic_Mute    : Boolean;
      ADC_Volume          : Audio_Volume;
      Input_FX            : FX_Kind;
      TP1_Threshold       : HAL.UInt32;
      TP2_Threshold       : HAL.UInt32;
      TP3_Threshold       : HAL.UInt32;
   end record;

   Default : constant Persistent_Data :=
     (Last_Project        => WNM.Project.Library.Valid_Prj_Index'First,
      Main_Volume         => Init_Volume,
      Line_In_Mute        => False,
      Internal_Mic_Mute   => True,
      Headset_Mic_Mute    => True,
      ADC_Volume          => Init_Input_Volume,
      Input_FX            => FX_Kind'First,
      TP1_Threshold       => 700,
      TP2_Threshold       => 800,
      TP3_Threshold       => 730);

   Data : Persistent_Data := Default;

   procedure Save;

   procedure Load;

end WNM.Persistent;
