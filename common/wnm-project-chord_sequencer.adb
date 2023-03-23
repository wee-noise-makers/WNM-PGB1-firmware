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

with MIDI; use MIDI;

package body WNM.Project.Chord_Sequencer is

   procedure Update_Current;
   --  Update the current tonic, name, and chord notes

   C_Tonic : MIDI_Key := MIDI.C4;
   C_Chord_Name : Chord_Name := Chord_Name'First;
   C_Chord : Chord_Notes := (others => MIDI.C4);

   Init_Scale_Root : constant MIDI.MIDI_Key := MIDI.C4;
   Init_Scale      : constant Scale_Name := Minor_Scale;
   Init_Chords     : constant Chord_Arr :=
     (1 => (Init_Scale_Root + Scales (Init_Scale)(0),
            Substitutions (Scale_Chords (Init_Scale)(0)).Sub (1)),

      2 => (Init_Scale_Root + Scales (Init_Scale)(1),
            Substitutions (Scale_Chords (Init_Scale)(1)).Sub (1)),

      3 => (Init_Scale_Root + Scales (Init_Scale)(2),
            Substitutions (Scale_Chords (Init_Scale)(2)).Sub (1)),

      4 => (Init_Scale_Root + Scales (Init_Scale)(3),
            Substitutions (Scale_Chords (Init_Scale)(3)).Sub (1)),

      5 => (Init_Scale_Root + Scales (Init_Scale)(4),
            Substitutions (Scale_Chords (Init_Scale)(4)).Sub (1)),

      6 => (Init_Scale_Root + Scales (Init_Scale)(5),
            Substitutions (Scale_Chords (Init_Scale)(5)).Sub (1)),

      7 => (Init_Scale_Root + Scales (Init_Scale)(6),
            Substitutions (Scale_Chords (Init_Scale)(6)).Sub (1)),

      8 => (Init_Scale_Root + Scales (Init_Scale)(0),
            Substitutions (Scale_Chords (Init_Scale)(0)).Sub (3)),

      9 => (Init_Scale_Root + Scales (Init_Scale)(0),
            Substitutions (Scale_Chords (Init_Scale)(0)).Sub (2)),

      10 => (Init_Scale_Root + Scales (Init_Scale)(1),
            Substitutions (Scale_Chords (Init_Scale)(1)).Sub (2)),

      11 => (Init_Scale_Root + Scales (Init_Scale)(2),
            Substitutions (Scale_Chords (Init_Scale)(2)).Sub (2)),

      12 => (Init_Scale_Root + Scales (Init_Scale)(3),
            Substitutions (Scale_Chords (Init_Scale)(3)).Sub (2)),

      13 => (Init_Scale_Root + Scales (Init_Scale)(4),
            Substitutions (Scale_Chords (Init_Scale)(4)).Sub (2)),

      14 => (Init_Scale_Root + Scales (Init_Scale)(5),
            Substitutions (Scale_Chords (Init_Scale)(5)).Sub (2)),

      15 => (Init_Scale_Root + Scales (Init_Scale)(6),
            Substitutions (Scale_Chords (Init_Scale)(6)).Sub (2)),

      16 => (Init_Scale_Root + Scales (Init_Scale)(0),
            Substitutions (Scale_Chords (Init_Scale)(0)).Sub (4))
     );

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Chain.Start;
      Update_Current;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Chain.Stop;
   end Stop;

   ---------------------------
   -- Signal_End_Of_Pattern --
   ---------------------------

   procedure Signal_End_Of_Pattern is
   begin
      Chain.Signal_End_Of_Pattern;
      Update_Current;
   end Signal_End_Of_Pattern;

   ------------------------
   -- Signal_Mid_Pattern --
   ------------------------

   procedure Signal_Mid_Pattern is
   begin
      Chain.Signal_Mid_Pattern;
      Update_Current;
   end Signal_Mid_Pattern;

   -------------------
   -- Current_Tonic --
   -------------------

   function Current_Tonic return MIDI.MIDI_Key
   is (C_Tonic);

   ------------------------
   -- Current_Chord_Name --
   ------------------------

   function Current_Chord_Name return Chord_Name
   is (C_Chord_Name);

   -----------------------------
   -- Current_Chord_Intervals --
   -----------------------------

   function Current_Chord_Intervals return Chord_Intervals is
   begin
      return WNM.Chord_Settings.Chords (Current_Chord_Name);
   end Current_Chord_Intervals;

   -------------------
   -- Current_Chord --
   -------------------

   function Current_Chord return Chord_Notes
   is (C_Chord);

   --------------------
   -- Update_Current --
   --------------------

   procedure Update_Current is
      C : constant WNM.Chords := Chain.Playing;
   begin
      C_Tonic := G_Project.Chords (C).Tonic;
      C_Chord_Name := G_Project.Chords (C).Name;
      C_Chord := C_Tonic + WNM.Chord_Settings.Chords (C_Chord_Name);
   end Update_Current;

begin
   --  Set default values for chords
   G_Project.Chords := Init_Chords;
end WNM.Project.Chord_Sequencer;
