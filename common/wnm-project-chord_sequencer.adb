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
with WNM.Step_Event_Broadcast;

package body WNM.Project.Chord_Sequencer is

   procedure Step_Callback (Step : Sequencer_Steps);

   Step_Listener : aliased Step_Event_Broadcast.Listener
     (Step_Callback'Access);

   procedure Update_Current;
   --  Update the current tonic, name, and chord notes

   C_Tonic : MIDI_Key := MIDI.C4;
   C_Chord_Name : Chord_Name := Chord_Name'First;
   C_Chord : Chord_Notes := (others => MIDI.C4);

   G_Bars_Count : Natural := 0;

   Init_Scale_Root : constant MIDI.MIDI_Key := MIDI.C4;
   Init_Scale      : constant Scale_Name := Minor_Scale;
   Init_Chords     : constant Chord_Arr :=
     (1 => (Init_Scale_Root + Scales (Init_Scale)(0),
            Substitutions (Scale_Chords (Init_Scale)(0)).Sub (1),
            4),

      2 => (Init_Scale_Root + Scales (Init_Scale)(1),
            Substitutions (Scale_Chords (Init_Scale)(1)).Sub (1),
            4),

      3 => (Init_Scale_Root + Scales (Init_Scale)(2),
            Substitutions (Scale_Chords (Init_Scale)(2)).Sub (1),
            4),

      4 => (Init_Scale_Root + Scales (Init_Scale)(3),
            Substitutions (Scale_Chords (Init_Scale)(3)).Sub (1),
            4),

      5 => (Init_Scale_Root + Scales (Init_Scale)(4),
            Substitutions (Scale_Chords (Init_Scale)(4)).Sub (1),
            4),

      6 => (Init_Scale_Root + Scales (Init_Scale)(5),
            Substitutions (Scale_Chords (Init_Scale)(5)).Sub (1),
            4),

      7 => (Init_Scale_Root + Scales (Init_Scale)(6),
            Substitutions (Scale_Chords (Init_Scale)(6)).Sub (1),
            4),

      8 => (Init_Scale_Root + Scales (Init_Scale)(0),
            Substitutions (Scale_Chords (Init_Scale)(0)).Sub (3),
            4),

      9 => (Init_Scale_Root + Scales (Init_Scale)(0),
            Substitutions (Scale_Chords (Init_Scale)(0)).Sub (2),
            4),

      10 => (Init_Scale_Root + Scales (Init_Scale)(1),
             Substitutions (Scale_Chords (Init_Scale)(1)).Sub (2),
             4),

      11 => (Init_Scale_Root + Scales (Init_Scale)(2),
             Substitutions (Scale_Chords (Init_Scale)(2)).Sub (2),
             4),

      12 => (Init_Scale_Root + Scales (Init_Scale)(3),
             Substitutions (Scale_Chords (Init_Scale)(3)).Sub (2),
             4),

      13 => (Init_Scale_Root + Scales (Init_Scale)(4),
             Substitutions (Scale_Chords (Init_Scale)(4)).Sub (2),
             4),

      14 => (Init_Scale_Root + Scales (Init_Scale)(5),
             Substitutions (Scale_Chords (Init_Scale)(5)).Sub (2),
             4),

      15 => (Init_Scale_Root + Scales (Init_Scale)(6),
             Substitutions (Scale_Chords (Init_Scale)(6)).Sub (2),
             4),

      16 => (Init_Scale_Root + Scales (Init_Scale)(0),
             Substitutions (Scale_Chords (Init_Scale)(0)).Sub (4),
             4)
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

   -------------------------
   -- Signal_End_Of_Chord --
   -------------------------

   procedure Signal_End_Of_Chord is
   begin
      Chain.Goto_Next;
      Update_Current;
   end Signal_End_Of_Chord;

   -------------------
   -- Step_Callback --
   -------------------

   procedure Step_Callback (Step : Sequencer_Steps) is
   begin
      if Step in 4 | 8 | 12 | 16 then
         declare
            C : constant WNM.Chords := Chain.Playing;
         begin

            G_Bars_Count := G_Bars_Count + 1;

            if G_Bars_Count >= Natural (G_Project.Chords (C).Duration) then
               Signal_End_Of_Chord;
            end if;
         end;
      end if;
   end Step_Callback;

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
      G_Bars_Count := 0;
      C_Tonic := G_Project.Chords (C).Tonic;
      C_Chord_Name := G_Project.Chords (C).Name;
      C_Chord := C_Tonic + WNM.Chord_Settings.Chords (C_Chord_Name);
   end Update_Current;

begin
   --  Set default values for chords
   G_Project.Chords := Init_Chords;

   Step_Event_Broadcast.Register (Step_Listener'Access);
end WNM.Project.Chord_Sequencer;
