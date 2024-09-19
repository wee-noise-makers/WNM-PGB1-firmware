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

with WNM.File_System.LEB128_File_Out;

private package WNM.Project.Storage.File_Out is

   subtype Parent is File_System.LEB128_File_Out.Instance;
   type Instance
   is new Parent with
   private;

   procedure Start_Global (This : in out Instance);

   procedure Start_Track_Settings (This : in out Instance;
                                   T : Tracks);
   procedure Start_Pattern_Settings (This : in out Instance;
                                     T : Tracks; P : Patterns);

   procedure Start_Part_Settings (This : in out Instance;
                                  P : Parts);

   procedure Start_Chord_Progression (This : in out Instance;
                                      P : Chord_Progressions);
   procedure Start_Chord_Settings (This : in out Instance);

   procedure Start_Sequence (This : in out Instance);
   procedure Start_Step_Settings (This : in out Instance;
                                  S : Sequencer_Steps);
   procedure Change_Pattern_In_Seq (This : in out Instance; P : Patterns);
   procedure Change_Track_In_Seq (This : in out Instance; T : Tracks);
   procedure End_Section (This : in out Instance);
   procedure End_File (This : in out Instance);

   procedure Push (This : in out Instance; A : Beat_Per_Minute);
   procedure Push (This : in out Instance; A : Step_Settings);
   procedure Push (This : in out Instance; A : Track_Settings);
   procedure Push (This : in out Instance; A : Pattern_Settings);
   procedure Push (This : in out Instance; A : Part_Settings);
   procedure Push (This : in out Instance; A : Chord_Setting_Kind);
   procedure Push (This : in out Instance; A : MIDI.MIDI_Data);
   procedure Push (This : in out Instance; A : WNM.Duration_In_Steps);
   procedure Push (This : in out Instance; A : WNM.Pattern_Length);

private

   type Instance
   is new Parent with null record;

   procedure Push (This : in out Instance; A : Token_Kind);

end WNM.Project.Storage.File_Out;
