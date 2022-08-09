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

private with WNM.Sequencer;
private with WNM.MIDI;

package WNM.GUI.Menu.Track_Settings is

   procedure Push_Window;

private

   type Settings is (Volume,
                     Pan,
                     Arp_Mode,
                     Arp_Notes,
                     MIDI_Chan,
                     MIDI_Instrument,
                     Sample,
                     CC_A, CC_Label_A,
                     CC_B, CC_Label_B,
                     CC_C, CC_Label_C,
                     CC_D, CC_Label_D);

   function Settings_Count is new Enum_Count (Settings);

   type Track_Settings_Menu is new Menu_Window with record
      Current_Setting : Settings := Settings'First;
      Instrument : Natural := 0;
   end record;

   overriding
   procedure Draw (This : in out Track_Settings_Menu);

   overriding
   procedure On_Event (This  : in out Track_Settings_Menu;
                       Event : Menu_Event);

   overriding
   procedure On_Pushed (This  : in out Track_Settings_Menu);

   overriding
   procedure On_Focus (This       : in out Track_Settings_Menu;
                       Exit_Value : Window_Exit_Value);


   type MIDI_Instrument_Settings is record
      Name : Sequencer.Controller_Label;
      CC_A : MIDI.MIDI_Data;
      CC_A_Label : Sequencer.Controller_Label;
      CC_B : MIDI.MIDI_Data;
      CC_B_Label : Sequencer.Controller_Label;
      CC_C : MIDI.MIDI_Data;
      CC_C_Label : Sequencer.Controller_Label;
      CC_D : MIDI.MIDI_Data;
      CC_D_Label : Sequencer.Controller_Label;
   end record;

   Builtin_Instruments : array (Natural range <>) of MIDI_Instrument_Settings
     := (0 => ("Volca Keys       ",
               44, "Cutoff           ",
               45, "VCF EG INT       ",
               42, "Detune           ",
               43, "VCO EG INT       "),
         1 => ("Volca Bass       ",
               41, "LFO RATE         ",
               42, "LFO INT          ",
               46, "EG ATTACK        ",
               48, "CUTOFF EG INT    "),
         2 => ("Volca Sample     ",
                7, "LEVEL            ",
               10, "PAN              ",
               43, "SPEED            ",
               42, "HI CUT           ")
        );

end WNM.GUI.Menu.Track_Settings;
