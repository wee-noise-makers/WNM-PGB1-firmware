-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with HAL; use HAL;

with WNM.MIDI; use WNM.MIDI;
with WNM.Chord_Sequencer;

package body WNM.Chord_Settings is

   procedure Update_Current;
   --  Update the current tonic, name, and chord notes

   function "+" (K : MIDI_Key; I : Interval) return MIDI_Key
     with Inline;
   function "+" (K : MIDI_Key; I : Chord_Intervals) return Chord_Notes
     with Inline;

   C_Tonic : MIDI_Key := MIDI.C4;
   C_Chord_Name : Chord_Name := Chord_Name'First;
   C_Chord : Chord_Notes := (others => MIDI.C4);

   ---------
   -- "+" --
   ---------

   function "+" (K : MIDI_Key; I : Interval) return MIDI_Key
   is (K + I'Enum_Rep);

   ---------
   -- "+" --
   ---------

   function "+" (K : MIDI_Key; I : Chord_Intervals) return Chord_Notes is
      Result : Chord_Notes;
   begin
      for X in Result'Range loop
         Result (X) := K + I (X);
      end loop;
      return Result;
   end "+";

   ----------------
   -- Play_Pause --
   ----------------

   procedure Play_Pause is
   begin
      Update_Current;
   end Play_Pause;

   ---------------------------
   -- Signal_End_Of_Pattern --
   ---------------------------

   procedure Signal_End_Of_Pattern is
   begin
      Update_Current;
   end Signal_End_Of_Pattern;

   ------------------------
   -- Signal_Mid_Pattern --
   ------------------------

   procedure Signal_Mid_Pattern is
   begin
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
      return Chords (Current_Chord_Name);
   end Current_Chord_Intervals;

   -------------------
   -- Current_Chord --
   -------------------

   function Current_Chord return Chord_Notes
   is (C_Chord);

   ---------------------------------
   -- Randomly_Pick_A_Progression --
   ---------------------------------

   procedure Randomly_Pick_A_Progression is
      --  R     : constant Rand_Percent := Random;
      --  Len   : constant Natural := Builtin_Chord_Progressions'Length;
      --  First : constant Natural := Builtin_Chord_Progressions'First;
      --  Index : constant Natural := First + (Natural (R) mod Len);
      --
      --  Key : constant MIDI.MIDI_Key := MIDI.C4 + MIDI_Key (Random mod 12);
   begin
      --  Progression := Builtin_Chord_Progressions (Index);
      --  Progression.Key := Key;
      raise Program_Error with "TODO...";
   end Randomly_Pick_A_Progression;

   -- Settings --

   type Settings_Rec is record
      Tonic : MIDI.MIDI_Key := MIDI.C4;
      Name  : Chord_Name := Chord_Name'First;
   end record;

   Settings_Arr : array (WNM.Chords) of Settings_Rec;

   --------------------
   -- Update_Current --
   --------------------

   procedure Update_Current is
      C : constant WNM.Chords := Chord_Sequencer.Playing;
   begin
      C_Tonic := Settings_Arr (C).Tonic;
      C_Chord_Name := Settings_Arr (C).Name;
      C_Chord := C_Tonic + Chords (C_Chord_Name);
   end Update_Current;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (S : User_Chord_Settings) is
      C : constant WNM.Chords := WNM.Sequencer.Editing_Chord;
   begin
      case S is
         when Tonic =>
            if Settings_Arr (C).Tonic /= MIDI_Key'Last then
               Settings_Arr (C).Tonic := Settings_Arr (C).Tonic + 1;
            end if;

         when Name =>
            Next (Settings_Arr (C).Name);
      end case;
   end Next_Value;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value (S : User_Chord_Settings) is
      C : constant WNM.Chords := WNM.Sequencer.Editing_Chord;
   begin
      case S is
         when Tonic =>
            if Settings_Arr (C).Tonic /= MIDI_Key'First then
               Settings_Arr (C).Tonic := Settings_Arr (C).Tonic - 1;
            end if;

         when Name =>
            Prev (Settings_Arr (C).Name);
      end case;
   end Prev_Value;

   --------------------
   -- Selected_Tonic --
   --------------------

   function Selected_Tonic (C : WNM.Chords := WNM.Sequencer.Editing_Chord)
                            return MIDI.MIDI_Key
   is (Settings_Arr (C).Tonic);

   -------------------
   -- Selected_Name --
   -------------------

   function Selected_Name (C : WNM.Chords := WNM.Sequencer.Editing_Chord)
                           return Chord_Name
   is (Settings_Arr (C).Name);

end WNM.Chord_Settings;
