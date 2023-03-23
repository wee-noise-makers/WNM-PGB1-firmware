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

with MIDI; use MIDI;

package WNM.Chord_Settings is

   type Interval is (P1, min2, Maj2, min3, Maj3, P4,
                     d5, P5, min6, d7, min7, Maj7, Octave,
                     min9, Maj9, min10, Maj10, P11, P12, min13, Maj13,
                     min14, Maj14, P15);

   for Interval use
     (P1     => 0,
      min2   => 1,
      Maj2   => 2,
      min3   => 3,
      Maj3   => 4,
      P4     => 5,
      d5     => 6,
      P5     => 7,
      min6   => 8,
      d7     => 9,
      min7   => 10,
      Maj7   => 11,
      Octave => 12,
      min9   => 13,
      Maj9   => 14,
      min10  => 15,
      Maj10  => 16,
      P11    => 17,
      P12    => 19,
      min13  => 20,
      Maj13  => 21,
      min14  => 22,
      Maj14  => 23,
      P15    => 24);

   type Chord_Index_Range is range 0 .. 3;
   --  Not trying to model the entire music theory, limiting chords to 4 notes
   --  is good enough here.

   type Chord_Intervals is array (Chord_Index_Range) of Interval;
   type Chord_Notes is array (Chord_Index_Range) of MIDI.MIDI_Key;

   type Scale_Range is range 0 .. 6;
   type Scale_Intervals is array (Scale_Range) of Interval;
   type Scale_Notes is array (Scale_Range) of MIDI.MIDI_Key;

   type Chord_Name is (Maj_Triad, Min_Triad, Dim_Triad,
                       Maj_7th, Min_7th, Dim_7th,
                       Sus2, Sus4,
                       Maj_Inv1, Maj_Inv2,
                       Min_Inv1, Min_Inv2);

   subtype Triads is Chord_Name range Maj_Triad .. Dim_Triad;

   type Scale_Chords_Array is array (Scale_Range) of Chord_Name;
   type Triads_Array is array (Scale_Range) of Triads;
   type Scale_Name is (Major_Scale, Minor_Scale);

   Scale_Chords : constant array (Scale_Name) of Triads_Array :=
     (Major_Scale => (Maj_Triad, Min_Triad, Min_Triad,
                      Maj_Triad, Maj_Triad, Min_Triad, Dim_Triad),

      Minor_Scale => (Min_Triad, Dim_Triad, Maj_Triad,
                      Min_Triad, Min_Triad, Maj_Triad, Maj_Triad)
     );

   Scales : constant array (Scale_Name) of Scale_Intervals :=
     (Major_Scale => (P1, Maj2, Maj3, P4, P5, d7, Maj7),
      Minor_Scale => (P1, Maj2, min3, P4, P5, min6, min7));

   Chords : constant array (Chord_Name) of Chord_Intervals :=
     (Maj_Triad => (P1, Maj3, P5, Octave),
      Min_Triad => (P1, min3, P5, Octave),
      Dim_Triad => (P1, min3, d5, Octave),
      Maj_7th   => (P1, Maj3, P5, Maj7),
      Min_7th   => (P1, min3, P5, min7),
      Dim_7th   => (P1, min3, d5, d7),
      Sus2      => (P1, Maj2, P5, Octave),
      Sus4      => (P1, P4, P5, Octave),
      Maj_Inv1  => (Maj3, P5, Octave, Maj10),
      Maj_Inv2  => (P5, Octave, Maj10, P12),
      Min_Inv1  => (min3, P5, Octave, min10),
      Min_Inv2  => (P5, Octave, min10, P12));
   --  https://en.wikipedia.org/wiki/Chord_(music)

   type Substitution_Index is range 1 .. 5;
   type Substitution_Array is array (Substitution_Index) of Chord_Name;
   type Substitution_Rec is record
      Last : Substitution_Index;
      Sub : Substitution_Array;
   end record;

   Substitutions : array (Triads) of Substitution_Rec
     := (Maj_Triad => (4, (Maj_Triad, Maj_7th, Maj_Inv2, Sus2, Sus4)),
         Min_Triad => (4, (Min_Triad, Min_7th, Min_Inv2, Sus2, Sus4)),
         Dim_Triad => (2, (Dim_Triad, Dim_7th, Dim_7th, Dim_7th, Dim_7th)));

   --  procedure Play_Pause;
   --  procedure Signal_End_Of_Pattern;
   --  procedure Signal_Mid_Pattern;
   --
   --  function Current_Tonic return MIDI.MIDI_Key;
   --  function Current_Chord_Name return Chord_Name;
   --  function Current_Chord_Intervals return Chord_Intervals;
   --  function Current_Chord return Chord_Notes;

   --  -- Settings --
   --
   --  type Chord_Settings is (Tonic,
   --                          Name,
   --                          Extended);
   --
   --  for Chord_Settings'Size use 4;
   --  for Chord_Settings use (Tonic       => 0,
   --                          Name        => 1,
   --                          Extended    => 15);
   --
   --  subtype User_Chord_Settings is Chord_Settings range Tonic .. Name;
   --
   --  procedure Next_Value (S : User_Chord_Settings);
   --  procedure Prev_Value (S : User_Chord_Settings);
   --
   --  function Selected_Tonic (C : WNM.Chords := WNM.Sequencer.Editing_Chord)
   --                           return MIDI.MIDI_Key;
   --  function Selected_Name (C : WNM.Chords :=  WNM.Sequencer.Editing_Chord)
   --                          return Chord_Name;
   --
   --  procedure Randomly_Pick_A_Progression;
   --  --  a.k.a. The Magic Hat of Chord Progression
   --
   function Img (S : Scale_Name) return String
   is (case S is
          when Major_Scale => "Major",
          when Minor_Scale => "Minor");

   function Img (N : Chord_Name) return String
   is (case N is
          when Maj_Triad => "M",
          when Min_Triad => "m",
          when Dim_Triad => "*",
          when Maj_7th   => "M7",
          when Min_7th   => "m7",
          when Dim_7th   => "*7",
          when Sus2      => "sus2",
          when Sus4      => "sus4",
          when Maj_Inv1  => "M/1",
          when Maj_Inv2  => "M/2",
          when Min_Inv1  => "m/1",
          when Min_Inv2  => "m/2");

   --  type Chord_Duration is (Whole_Bar, Half_Bar);
   --
   --  function Img (D : Chord_Duration) return String
   --  is (case D is
   --         when Whole_Bar => "1 Bar",
   --         when Half_Bar  => "1/2 Bar");
   --
   --  package Chord_Name_Next is new Enum_Next (Chord_Name);
   --  use Chord_Name_Next;

   function "+" (K : MIDI.MIDI_Key; I : Interval)
                 return MIDI.MIDI_Key
   is (K + I'Enum_Rep)
   with Inline;

   function "+" (K : MIDI.MIDI_Key; I : Chord_Intervals)
                 return Chord_Notes
   is ((K + I (0), K + I (1), K + I (2), K + I (3)))
     with Inline;

end WNM.Chord_Settings;
