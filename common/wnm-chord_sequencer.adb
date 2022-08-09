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

package body WNM.Chord_Sequencer is

   type Progression_Elt is record
      Index : Scale_Range := Scale_Range'First;
      Sub   : Substitution_Index := Substitution_Index'First;
      Dur   : Chord_Duration := Chord_Duration'First;
   end record;

   type Progression_Array is array (Progression_Range) of Progression_Elt;
   type Progression_Rec is record
      Prog   : Progression_Array;
      Length : Progression_Range := Progression_Range'First;
      Scale  : Scale_Name := Minor_Scale;
      Key    : MIDI_Key := MIDI.C4;
   end record;

   One   : constant Scale_Range := 0;
   Two   : constant Scale_Range := 1;
   Three : constant Scale_Range := 2;
   Four  : constant Scale_Range := 3;
   Five  : constant Scale_Range := 4;
   Six   : constant Scale_Range := 5;
   Seven : constant Scale_Range := 6;

   Builtin_Chord_Progressions : array (Natural range <>) of Progression_Rec :=
     (0 => (Prog => ((One, 1, Whole_Bar),
                     (Five, 1, Whole_Bar),
                     (Two, 1, Half_Bar),
                     (One, 4, Half_Bar),
                     (One, 1, Whole_Bar),
                     others => (others => <>)),
            Length => 5,
            Scale  => Minor_Scale,
            Key    => MIDI.C4),
      1 => (Prog => ((Four, 1, Whole_Bar),
                     (Seven, 1, Whole_Bar),
                     (One, 1, Half_Bar),
                     (Three, 1, Half_Bar),
                     (Five, 1, Whole_Bar),
                     others => (others => <>)),
            Length => 5,
            Scale  => Minor_Scale,
            Key    => MIDI.C4),
      2 => (Prog => ((One, 1, Whole_Bar),
                     (Five, 1, Whole_Bar),
                     (Six, 1, Whole_Bar),
                     (Four, 1, Whole_Bar),
                     others => (others => <>)),
            Length => 4,
            Scale  => Major_Scale,
            Key    => MIDI.C4),
      3 => (Prog => ((One, 1, Whole_Bar),
                     (Four, 1, Whole_Bar),
                     (Five, 1, Whole_Bar),
                     (Four, 1, Whole_Bar),
                     others => (others => <>)),
            Length => 4,
            Scale  => Minor_Scale,
            Key    => MIDI.C4));

   Progression : Progression_Rec :=
     (Builtin_Chord_Progressions (Builtin_Chord_Progressions'First));

   Edit_Cursor : Progression_Range := Progression_Range'First;

   Seq_Index : Progression_Range := Progression_Range'First;

   ---------
   -- "+" --
   ---------

   function "+" (K : MIDI_Key; I : Interval) return MIDI_Key
   is (K + I'Enum_Rep);

   ---------
   -- "+" --
   ---------

   function "+" (I : Interval; K : MIDI_Key) return MIDI_Key
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

   ---------
   -- "+" --
   ---------

   function "+" (K : MIDI_Key; I : Scale_Intervals) return Scale_Notes is
      Result : Scale_Notes;
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
      raise Program_Error with "Unimplemented procedure Play_Pause";
   end Play_Pause;

   -------------
   -- Playing --
   -------------

   function Playing return Boolean is
   begin
      return raise Program_Error with "Unimplemented function Playing";
   end Playing;

   ---------------------------
   -- Signal_End_Of_Pattern --
   ---------------------------

   procedure Signal_End_Of_Pattern is
   begin
      if Seq_Index >= Progression.Length then
         Seq_Index := Progression_Range'First;
      else
         Seq_Index := Seq_Index + 1;
      end if;
   end Signal_End_Of_Pattern;

   ------------------------
   -- Signal_Mid_Pattern --
   ------------------------

   procedure Signal_Mid_Pattern is
   begin
      if Progression.Prog (Seq_Index).Dur = Half_Bar then
         if Seq_Index >= Progression.Length then
            Seq_Index := Progression_Range'First;
         else
            Seq_Index := Seq_Index + 1;
         end if;
      end if;
   end Signal_Mid_Pattern;

   -----------------------
   -- Current_Scale_Key --
   -----------------------

   function Current_Scale_Key return MIDI.MIDI_Key
   is (Progression.Key);

   --------------------
   -- Scale_Key_Next --
   --------------------

   procedure Scale_Key_Next is
   begin
      if Progression.Key /= MIDI_Key'Last then
         Progression.Key := Progression.Key + 1;
      end if;
   end Scale_Key_Next;

   --------------------
   -- Scale_Key_Prev --
   --------------------

   procedure Scale_Key_Prev is
   begin
      if Progression.Key /= MIDI_Key'First then
         Progression.Key := Progression.Key - 1;
      end if;
   end Scale_Key_Prev;

   ------------------------
   -- Current_Scale_Name --
   ------------------------

   function Current_Scale_Name return Scale_Name
   is (Progression.Scale);

   ----------------
   -- Scale_Next --
   ----------------

   procedure Scale_Next is
   begin
      Progression.Scale := Next (Progression.Scale);
   end Scale_Next;

   ----------------
   -- Scale_Prev --
   ----------------

   procedure Scale_Prev is
   begin
      Progression.Scale := Prev (Progression.Scale);
   end Scale_Prev;

   -------------------
   -- Current_Tonic --
   -------------------

   function Current_Tonic return MIDI.MIDI_Key
   is (Current_Scale_Key +
         Scales (Current_Scale_Name) (Progression.Prog (Seq_Index).Index));

   ------------------------
   -- Current_Chord_Name --
   ------------------------

   function Current_Chord_Name return Chord_Name is
      T : Triads := Scale_Chords (Progression.Scale)
        (Progression.Prog (Seq_Index).Index);
   begin
      return Substitutions (T).Sub (Progression.Prog (Seq_Index).Sub);
   end Current_Chord_Name;

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

   function Current_Chord return Chord_Notes is
   begin
      return Current_Tonic + Current_Chord_Intervals;
   end Current_Chord;

   -----------------------------
   -- Current_Scale_Intervals --
   -----------------------------

   function Current_Scale_Intervals return Scale_Intervals is
   begin
      return Scales (Current_Scale_Name);
   end Current_Scale_Intervals;

   -------------------
   -- Current_Scale --
   -------------------

   function Current_Scale return Scale_Notes is
   begin
      return Current_Scale_Key + Current_Scale_Intervals;
   end Current_Scale;

   --  Progression edition --

   function Progression_Length return Progression_Range
   is (Progression.Length);

   ------------
   -- Cursor --
   ------------

   function Cursor return Progression_Range
   is (Edit_Cursor);

   -----------------
   -- Cursor_Next --
   -----------------

   procedure Cursor_Next is
   begin
      if Edit_Cursor < Progression_Length then
         Edit_Cursor := Edit_Cursor + 1;
      end if;
   end Cursor_Next;

   -----------------
   -- Cursor_Prev --
   -----------------

   procedure Cursor_Prev is
   begin
      if Edit_Cursor > Progression_Range'First then
         Edit_Cursor := Edit_Cursor - 1;
      end if;
   end Cursor_Prev;

   ----------------
   -- Chord_Kind --
   ----------------

   function Chord_Kind return String is
      P : Progression_Elt renames Progression.Prog (Edit_Cursor);
      T : constant Triads := Scale_Chords (Progression.Scale) (P.Index);
      Sub_Str : constant String := Img (Substitutions (T).Sub (P.Sub));
   begin
      return (case T is
                 when Maj_Triad =>
                   (case P.Index is
                       when 0 => "I",
                       when 1 => "II",
                       when 2 => "III",
                       when 3 => "IV",
                       when 4 => "V",
                       when 5 => "VI",
                       when 6 => "VII"),
                 when Min_Triad | Dim_Triad =>
                   (case P.Index is
                       when 0 => "i",
                       when 1 => "ii",
                       when 2 => "iii",
                       when 3 => "iv",
                       when 4 => "v",
                       when 5 => "vi",
                       when 6 => "vii")
             ) & Sub_Str;
   end Chord_Kind;

   ---------------------
   -- Chord_Kind_Next --
   ---------------------

   procedure Chord_Kind_Next is
      P : Progression_Elt renames Progression.Prog (Edit_Cursor);
      T : Triads := Scale_Chords (Progression.Scale) (P.Index);
      Last : Substitution_Index := Substitutions (T).Last;
   begin
      if P.Sub = Last then
         P.Sub := Substitution_Index'First;
         if P.Index /= Scale_Range'Last then
            P.Index := P.Index + 1;
         else
            P.Index := Scale_Range'First;
         end if;
      else
         P.Sub := P.Sub + 1;
      end if;
   end Chord_Kind_Next;

   ---------------------
   -- Chord_Kind_Prev --
   ---------------------

   procedure Chord_Kind_Prev is
      P : Progression_Elt renames Progression.Prog (Edit_Cursor);
      T : Triads := Scale_Chords (Progression.Scale) (P.Index);
      Last : Substitution_Index := Substitutions (T).Last;
   begin
      if P.Sub = Substitution_Index'First then
         if P.Index /= Scale_Range'First then
            P.Index := P.Index - 1;
         else
            P.Index := Scale_Range'Last;
         end if;

         P.Sub := Last;
      else
         P.Sub := P.Sub - 1;
      end if;
   end Chord_Kind_Prev;

   --------------
   -- Duration --
   --------------

   function Duration return Chord_Duration
   is (Progression.Prog (Edit_Cursor).Dur);

   -------------------
   -- Duration_Next --
   -------------------

   procedure Duration_Next is
   begin
      Progression.Prog (Edit_Cursor).Dur :=
        Next (Progression.Prog (Edit_Cursor).Dur);
   end Duration_Next;

   -------------------
   -- Duration_Prev --
   -------------------

   procedure Duration_Prev is
   begin
      Progression.Prog (Edit_Cursor).Dur :=
        Prev (Progression.Prog (Edit_Cursor).Dur);
   end Duration_prev;

   ---------------
   -- Add_Chord --
   ---------------

   procedure Add_Chord is
   begin
      if Progression.Length /= Progression_Range'Last then
         Progression.Length := Progression.Length + 1;
         Progression.Prog (Progression.Length) := (others => <>);
         Edit_Cursor := Progression.Length;
      end if;
   end Add_Chord;

   ------------------
   -- Remove_Chord --
   ------------------

   procedure Remove_Chord is
   begin
      if Progression.Length = 1 then
         --  We always keep at least one chord in the progression
         return;
      end if;

      for Index in Edit_Cursor .. Progression.Length - 1 loop
         Progression.Prog (Index) := Progression.Prog (Index + 1);
      end loop;
      Progression.Length := Progression.Length - 1;
      if Edit_Cursor > Progression.Length then
         Edit_Cursor := Progression.Length;
      end if;
   end Remove_Chord;

   ---------------------------------
   -- Randomly_Pick_A_Progression --
   ---------------------------------

   procedure Randomly_Pick_A_Progression is
      R     : constant Rand_Percent := Random;
      Len   : constant Natural := Builtin_Chord_Progressions'Length;
      First : constant Natural := Builtin_Chord_Progressions'First;
      Index : constant Natural := First + (Natural (R) mod Len);

      Key : constant MIDI.MIDI_Key := MIDI.C4 + MIDI_Key (Random mod 12);
   begin
      Progression := Builtin_Chord_Progressions (Index);
      Progression.Key := Key;
   end Randomly_Pick_A_Progression;

end WNM.Chord_Sequencer;
