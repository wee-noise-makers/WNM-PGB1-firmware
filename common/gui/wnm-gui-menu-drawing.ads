-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2021 Fabien Chouteau                  --
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

with WNM.MIDI;
with WNM.Screen;
with WNM.GUI.Bitmap_Fonts;
with WNM.Chord_Sequencer;
with WNM.Sample_Library;

package WNM.GUI.Menu.Drawing is

   Box_Top    : constant := 22;
   Box_Bottom : constant := Screen.Height - 3;
   Box_Left   : constant := Bitmap_Fonts.Width;
   Box_Right  : constant := Screen.Width - Box_Left;

   Box_Center : constant Screen.Point := ((Box_Right + Box_Left) / 2,
                                          (Box_Top + Box_Bottom) / 2);


   procedure Draw_Menu_Box (Title : String;
                            Count : Positive;
                            Index : Natural);

   subtype Percentage is Natural range 0 .. 100;

   procedure Draw_Precentage (Title : String;
                              Val : Percentage);

   subtype Pan is Integer range -50 .. 50;

   procedure Draw_Pan (Title : String;
                       Val : Pan);

   procedure Draw_MIDI_Val (Val      : MIDI.MIDI_Data;
                            Selected : Boolean);

   procedure Draw_MIDI_Note (Key     : MIDI.MIDI_Key;
                            Selected : Boolean);

   procedure Draw_Duration (D        : Note_Duration;
                            Selected : Boolean);

   procedure Draw_Scale_Mode (M        : Chord_Sequencer.Scale_Name;
                              Selected : Boolean);

   procedure Draw_Chord_Duration (D        : Chord_Sequencer.Chord_Duration;
                                  Selected : Boolean);

   procedure Draw_Chord_Kind (Str      : String;
                              Selected : Boolean);

   procedure Draw_Title (Title : String;
                         Val   : String);

   procedure Draw_Value (Val      : String;
                         Selected : Boolean := False);

   procedure Draw_Value_Left (Val      : String;
                              Selected : Boolean := False);

   procedure Draw_Knob (Title : String;
                        Value : Natural);

   procedure Draw_Sample_Select (Val : Sample_Library.Valid_Sample_Index);

   procedure Draw_Waveform;

end WNM.GUI.Menu.Drawing;
