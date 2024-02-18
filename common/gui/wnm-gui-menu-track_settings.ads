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

with WNM.Project; use WNM.Project;

private with MIDI;

package WNM.GUI.Menu.Track_Settings is

   procedure Push_Window;

private

   type Top_Settings is (Engine,
                         CC_Default,
                         LFO,
                         Volume,
                         Pan,
                         Master_FX,
                         Arp_Mode,
                         Arp_Notes,
                         Notes_Per_Chord,
                         MIDI_Chan,
                         MIDI_Instrument,
                         CC_Ctrl_A, CC_Label_A,
                         CC_Ctrl_B, CC_Label_B,
                         CC_Ctrl_C, CC_Label_C,
                         CC_Ctrl_D, CC_Label_D,
                         Track_Mode);

   subtype Sub_Settings is Project.User_Track_Settings;

   function Top_Setting_Position (S : Top_Settings;
                                  M : Project.Track_Mode_Kind)
                                  return Natural;

   function Valid_Setting (M : Project.Track_Mode_Kind;
                           S : Sub_Settings)
                           return Boolean
   is (case M is
          when Project.MIDI_Mode =>
             S in Track_Mode | CC_Default_A .. CC_Default_D |
                  MIDI_Chan | MIDI_Instrument | Arp_Mode |
                  Arp_Notes | Notes_Per_Chord | CC_Ctrl_A .. CC_Label_D,

          --  Speech
          when Project.Speech_Mode =>
             S in Track_Mode | Volume | Pan | Master_FX | Arp_Mode |
                  Arp_Notes | CC_Default_A .. CC_Default_B,

          --  Single engine
          when Project.Kick_Mode | Project.Cymbal_Mode | Project.Chord_Mode |
               Project.Sample1_Mode | Project.Sample2_Mode =>
             S in Track_Mode | Volume | Pan | Master_FX | Arp_Mode |
                  Arp_Notes | CC_Default_A .. CC_Default_D |
                  LFO_Rate | LFO_Amplitude | LFO_Target | LFO_Shape,

          --  Multi engines
          when Project.Lead_Mode | Project.Bass_Mode | Project.Snare_Mode =>
             S in Track_Mode | Engine | Volume | Pan | Master_FX | Arp_Mode |
                  Arp_Notes | CC_Default_A .. CC_Default_D |
                  LFO_Rate | LFO_Amplitude | LFO_Target | LFO_Shape,

          --  Effects
          when Project.Reverb_Mode | Project.Filter_Mode |
               Project.Drive_Mode | Project.Bitcrush_Mode =>
             S in Track_Mode | CC_Default_A .. CC_Default_D |
                  LFO_Rate | LFO_Amplitude | LFO_Target | LFO_Shape
      );
   --  Return True if the given setting is available for the given track mode.
   --  For instance, volume setting is not available in MIDI mode.

   procedure Next_Valid_Setting (M : Project.Track_Mode_Kind;
                                 S : in out Sub_Settings);
   procedure Prev_Valid_Setting (M : Project.Track_Mode_Kind;
                                 S : in out Sub_Settings);

   type Track_Settings_Menu is new Menu_Window with record
      Current_Setting : Sub_Settings := Sub_Settings'First;
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
      Name : Project.Controller_Label;
      CC_Target_A : MIDI.MIDI_Data;
      CC_A_Label  : Project.Controller_Label;
      CC_Target_B : MIDI.MIDI_Data;
      CC_B_Label  : Project.Controller_Label;
      CC_Target_C : MIDI.MIDI_Data;
      CC_C_Label  : Project.Controller_Label;
      CC_Target_D : MIDI.MIDI_Data;
      CC_D_Label  : Project.Controller_Label;
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
