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

   type Settings is (Track_Mode,
                     Sample,
                     Volume,
                     Pan,
                     Arp_Mode,
                     Arp_Notes,
                     MIDI_Chan,
                     MIDI_Instrument,
                     CC_A, CC_Label_A,
                     CC_B, CC_Label_B,
                     CC_C, CC_Label_C,
                     CC_D, CC_Label_D);

   function Settings_Count (M : Sequencer.Track_Mode_Kind) return Positive;
   function Setting_Position (S : Settings;
                              M : Sequencer.Track_Mode_Kind)
                              return Natural;

   function Valid_Setting (S : Settings;
                           M : Sequencer.Track_Mode_Kind)
                           return Boolean
   is (case M is
          when Sequencer.Sample_Mode =>
             S in Track_Mode | Sample | Volume | Pan | Arp_Mode | Arp_Notes,

          when Sequencer.MIDI_Mode =>
             S in Track_Mode | MIDI_Chan | MIDI_Instrument | Arp_Mode |
                  Arp_Notes | CC_A .. CC_Label_D,

          when Sequencer.Speak_Mode =>
             S in Track_Mode | Volume | Pan | Arp_Mode | Arp_Notes);
   --  Return True if the given setting is available for the given track mode.
   --  For instance, volume setting is not available in MIDI mode.

   procedure Next_Valid_Setting (S : in out Settings;
                                 M : Sequencer.Track_Mode_Kind);
   procedure Prev_Valid_Setting (S : in out Settings;
                                 M : Sequencer.Track_Mode_Kind);

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

   procedure Fix_Current_Setting (This : in out Track_Settings_Menu);
   --  When the user changes current editing track it is possible that the
   --  Current_Setting is invalid for the track mode of the new editing track.
   --  For instance switching from a Sample track to a MIDI track when
   --  Volume is the current setting.
   --
   --  This procedure will switch the Current_Setting to a valid setting for
   --  the editing track.


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
