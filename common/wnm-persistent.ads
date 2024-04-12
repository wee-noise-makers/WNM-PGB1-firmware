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

package WNM.Persistent is

   type Persistent_Data is record
      Last_Project        : WNM.Project.Library.Prj_Index;
      Main_Volume         : Audio_Volume := Init_Volume;
      Line_In_Volume      : Audio_Volume := Init_Input_Volume;
      Internal_Mic_Volume : Audio_Volume := Init_Input_Volume;
      Headset_Mic_Volume  : Audio_Volume := Init_Input_Volume;
      ADC_Volume          : Audio_Volume := Init_Input_Volume;
      Input_FX            : FX_Kind      := FX_Kind'First;
   end record;

   Default : constant Persistent_Data :=
     (Last_Project        => WNM.Project.Library.Invalid_Prj_Entry,
      Main_Volume         => Init_Volume,
      Line_In_Volume      => Init_Input_Volume,
      Internal_Mic_Volume => Init_Input_Volume,
      Headset_Mic_Volume  => Init_Input_Volume,
      ADC_Volume          => Init_Input_Volume,
      Input_FX            => FX_Kind'First);

   Data : Persistent_Data := Default;

   procedure Save;

   procedure Load;

end WNM.Persistent;
