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
with WNM.Sample_Library;
with WNM.Speech;
with WNM.Project;
with WNM.Project.Library;

package WNM.GUI.Menu.Drawing is

   Box_Top    : constant := 22;
   Box_Bottom : constant := Screen.Height - 3;
   Box_Left   : constant := Bitmap_Fonts.Width;
   Box_Right  : constant := Screen.Width - Box_Left;

   Box_Center : constant Screen.Point := ((Box_Right + Box_Left) / 2,
                                          (Box_Top + Box_Bottom) / 2);

   procedure Draw_Menu_Box (Title : String;
                            Count : Natural;
                            Index : Natural);

   procedure Draw_Volume (Title : String;
                          Val : WNM_HAL.Audio_Volume);

   procedure Draw_Pan (Title : String;
                       Val : WNM_HAL.Audio_Pan);

   procedure Draw_MIDI_Val (Val      : MIDI.MIDI_Data;
                            Selected : Boolean);

   procedure Draw_MIDI_Note (Key     : MIDI.MIDI_Key;
                             Selected : Boolean);

   procedure Draw_Duration (D        : Project.Note_Duration;
                            Selected : Boolean);

   procedure Draw_Chord_Kind (Str      : String;
                              Selected : Boolean);

   procedure Draw_Title (Title : String;
                         Val   : String);

   procedure Draw_Value (Val      : String;
                         Selected : Boolean := False);

   type CC_Draw_Style is (Positive, Center, Negative);

   procedure Draw_CC_Value (Id    : WNM.Project.CC_Id;
                            Value : MIDI.MIDI_Data;
                            Label : String;
                            Selected : Boolean;
                            Enabled : Boolean := True;
                            Style   : CC_Draw_Style := Positive);

   procedure Draw_LFO_Shape (Id    : WNM.Project.CC_Id;
                             Label : String;
                             Selected : Boolean;
                             Shape    : WNM.Project.LFO_Shape_Kind;
                             Sync     : WNM.Project.LFO_Sync_Kind;
                             Loo      : WNM.Project.LFO_Loop_Kind);

   procedure Draw_Value_Left (Val      : String;
                              Selected : Boolean := False);

   procedure Draw_Knob (Title : String;
                        Value : Natural);

   procedure Draw_Sample_Select (Val : Sample_Library.Valid_Sample_Index);

   procedure Draw_Project_Select (Val : Project.Library.Valid_Prj_Index);

   procedure Draw_Word_Select (Word : Speech.Word);

   procedure Draw_Waveform;

end WNM.GUI.Menu.Drawing;
