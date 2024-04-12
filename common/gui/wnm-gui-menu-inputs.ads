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

package WNM.GUI.Menu.Inputs is

   procedure Push_Window;

private

   type Top_Settings is (Audio_In,
                         Audio_In_FX,
                         MIDI_In);

   function Top_Settings_Count is new Enum_Count (Top_Settings);

   type Sub_Settings is
     (Line_In_Volume,
      Internal_Mic_Volume,
      Headset_Mic_Volume,
      ADC_Volume,
      Input_FX,
      MIDI_In_Mode,
      MIDI_In_Clock);

   function Sub_Settings_Count is new Enum_Count (Sub_Settings);

   package Sub_Settings_Next is new Enum_Next (Sub_Settings);
   use Sub_Settings_Next;

   type Instance is new Menu_Window with record
      Current_Setting : Sub_Settings := Sub_Settings'First;
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

end WNM.GUI.Menu.Inputs;
