-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2025 Fabien Chouteau                  --
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

package WNM.GUI.Menu.User_Waveform_Load is

   procedure Push_Window;

private
   type Menu_Items is (WAV_Sine,
                       WAV_Sine_Warp1,
                       WAV_Sine_Warp2,
                       WAV_Sine_Warp3,
                       WAV_Sine2_Warp1,
                       WAV_Sine2_Warp2,
                       WAV_Sine2_Warp3,
                       WAV_Sawtooth,
                       WAV_Screech,
                       WAV_Triangle,
                       WAV_Chip_Triangle,
                       WAV_Chip_Pulse_50,
                       WAV_Chip_Pulse_25,
                       WAV_Combined_Sin_Saw,
                       WAV_Combined_Saw_Sin,
                       WAV_Combined_Trig_Sin,
                       WAV_Combined_Sin_Square,
                       WAV_Combined_Square_Sin,
                       WAV_Combined_Square_Full_Sin,
                       WAV_Bandlimited_Comb);

   function Menu_Items_Count is new Enum_Count (Menu_Items);

   type Instance is new Menu_Window with record
      Item : Menu_Items;
   end record;

   overriding
   procedure Draw (This   : in out Instance);

   overriding
   procedure On_Event (This  : in out Instance;
                       Event : Menu_Event);

   overriding
   procedure On_Pushed (This  : in out Instance) is null;

   overriding
   procedure On_Focus (This       : in out Instance;
                       Exit_Value : Window_Exit_Value) is null;

end WNM.GUI.Menu.User_Waveform_Load;
