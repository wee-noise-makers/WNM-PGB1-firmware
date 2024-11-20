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

with WNM.File_System;
with WNM.File_System.LEB128_File_Out; use WNM.File_System.LEB128_File_Out;

package body WNM.Project.Storage.File_Out is

   ------------------
   -- Start_Global --
   ------------------

   procedure Start_Global (This : in out Instance) is
   begin
      This.Push (Global_Section);
   end Start_Global;

   --------------------------
   -- Start_Track_Settings --
   --------------------------

   procedure Start_Track_Settings (This : in out Instance; T : Tracks) is
   begin
      This.Push (Track_Section);
      This.Push (Out_UInt (T'Enum_Rep));
   end Start_Track_Settings;

   ----------------------------
   -- Start_Pattern_Settings --
   ----------------------------

   procedure Start_Pattern_Settings (This : in out Instance;
                                     T    :        Tracks;
                                     P    :        Patterns)
   is
   begin
      This.Push (Pattern_Section);
      This.Push (Out_UInt (T'Enum_Rep));
      This.Push (Out_UInt (P'Enum_Rep));
   end Start_Pattern_Settings;

   -------------------------
   -- Start_Part_Settings --
   -------------------------

   procedure Start_Part_Settings (This : in out Instance;
                                  P : Parts)
   is
   begin
      This.Push (Part_Section);
      This.Push (Out_UInt (P'Enum_Rep));
   end Start_Part_Settings;

   -----------------------------
   -- Start_Chord_Progression --
   -----------------------------

   procedure Start_Chord_Progression (This : in out Instance;
                                      P : Chord_Progressions)
   is
   begin
      This.Push (Chord_Progression_Section);
      This.Push (Out_UInt (P'Enum_Rep));
   end Start_Chord_Progression;

   --------------------------
   -- Start_Chord_Settings --
   --------------------------

   procedure Start_Chord_Settings (This : in out Instance)
   is
   begin
      This.Push (Chord_Section);
   end Start_Chord_Settings;

   --------------------
   -- Start_Sequence --
   --------------------

   procedure Start_Sequence (This : in out Instance) is
   begin
      This.Push (Sequence_Section);
   end Start_Sequence;

   -------------------------
   -- Start_Step_Settings --
   -------------------------

   procedure Start_Step_Settings (This : in out Instance;
                                  S : Sequencer_Steps)
   is
   begin
      This.Push (Step_Section);
      This.Push (Out_UInt (S'Enum_Rep));
   end Start_Step_Settings;

   ---------------------------
   -- Change_Pattern_In_Seq --
   ---------------------------

   procedure Change_Pattern_In_Seq (This : in out Instance; P : Patterns)
   is
   begin
      This.Push (Seq_Change_Pattern);
      This.Push (Out_UInt (P'Enum_Rep));
   end Change_Pattern_In_Seq;

   -------------------------
   -- Change_Track_In_Seq --
   -------------------------

   procedure Change_Track_In_Seq (This : in out Instance; T : Tracks)
   is
   begin
      This.Push (Seq_Change_Track);
      This.Push (Out_UInt (T'Enum_Rep));
   end Change_Track_In_Seq;

   -----------------
   -- End_Section --
   -----------------

   procedure End_Section (This : in out Instance) is
   begin
      This.Push (End_Of_Section);
   end End_Section;

   --------------
   -- End_File --
   --------------

   procedure End_File (This : in out Instance) is
   begin
      This.Push (End_Of_File);
   end End_File;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Step_Settings) is
      procedure Push_G is new Push_Gen (Step_Settings);
   begin
      Push_G (Parent (This), A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Track_Settings) is
      procedure Push_G is new Push_Gen (Track_Settings);
   begin
      Push_G (Parent (This), A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Pattern_Settings) is
      procedure Push_G is new Push_Gen (Pattern_Settings);
   begin
      Push_G (Parent (This), A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Part_Settings) is
      procedure Push_G is new Push_Gen (Part_Settings);
   begin
      Push_G (Parent (This), A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Chord_Setting_Kind) is
      procedure Push_G is new Push_Gen (Chord_Setting_Kind);
   begin
      Push_G (Parent (This), A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Token_Kind) is
      procedure Push_G is new Push_Gen (Token_Kind);
   begin
      Push_G (Parent (This), A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : MIDI.MIDI_Data) is
      procedure Push_G is new Push_Gen (MIDI.MIDI_Data);
   begin
      Push_G (Parent (This), A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : WNM.Duration_In_Steps) is
      procedure Push_G is new Push_Gen (WNM.Duration_In_Steps);
   begin
      Push_G (Parent (This), A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : WNM.Pattern_Length) is
      procedure Push_G is new Push_Gen (WNM.Pattern_Length);
   begin
      Push_G (Parent (This), A);
   end Push;

end WNM.Project.Storage.File_Out;
