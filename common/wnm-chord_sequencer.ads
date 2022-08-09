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

with WNM.MIDI;

package WNM.Chord_Sequencer is

   subtype Tonic is MIDI.MIDI_Key;

   type Interval is (P1, min2, Maj2, min3, Maj3, P4,
                     d5, P5, min6, d7, min7, Maj7, Octave);

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
      Octave => 12);

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
                       Sus2, Sus4);

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
      Sus4      => (P1, P4, P5, Octave));
   --  https://en.wikipedia.org/wiki/Chord_(music)

   type Substitution_Index is range 1 .. 4;
   type Substitution_Array is array (Substitution_Index) of Chord_Name;
   type Substitution_Rec is record
      Last : Substitution_Index;
      Sub : Substitution_Array;
   end record;

   Substitutions : array (Triads) of Substitution_Rec
     := (Maj_Triad => (4, (Maj_Triad, Maj_7th, Sus2, Sus4)),
         Min_Triad => (4, (Min_Triad, Min_7th, Sus2, Sus4)),
         Dim_Triad => (2, (Dim_Triad, Dim_7th, Dim_7th, Dim_7th)));

   procedure Play_Pause;
   function Playing return Boolean;

   procedure Signal_End_Of_Pattern;
   procedure Signal_Mid_Pattern;

   function Current_Scale_Key return MIDI.MIDI_Key;
   procedure Scale_Key_Next;
   procedure Scale_Key_Prev;

   function Current_Scale_Name return Scale_Name;
   procedure Scale_Next;
   procedure Scale_Prev;

   function Current_Tonic return MIDI.MIDI_Key;
   function Current_Chord_Name return Chord_Name;
   function Current_Chord_Intervals return Chord_Intervals;
   function Current_Chord return Chord_Notes;
   function Current_Scale_Intervals return Scale_Intervals;
   function Current_Scale return Scale_Notes;

   --  Progression edition --

   type Progression_Range is range 1 .. 12;
   function Progression_Length return Progression_Range;
   function Cursor return Progression_Range;
   procedure Cursor_Next;
   procedure Cursor_Prev;

   function Chord_Kind return String;
   procedure Chord_Kind_Next;
   procedure Chord_Kind_Prev;

   type Chord_Duration is (Whole_Bar, Half_Bar);
   function Duration return Chord_Duration;
   procedure Duration_Next;
   procedure Duration_Prev;

   procedure Add_Chord;
   --  Add a chord at the end of the progression

   procedure Remove_Chord;
   --  Remove the currently selected chord in the progression

   procedure Randomly_Pick_A_Progression;
   --  a.k.a. The Magic Hat of Chord Progression

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
       when Sus4      => "sus4");

   function Img (D : Chord_Duration) return String
   is (case D is
          when Whole_Bar => "1 Bar",
          when Half_Bar  => "1/2 Bar");

   package Scale_Name_Next is new Enum_Next (Scale_Name);
   use Scale_Name_Next;

   package Chord_Duration_Next is new Enum_Next (Chord_Duration);
   use Chord_Duration_Next;

   pragma Inline (Current_Scale_Key);
   pragma Inline (Scale_Key_Prev);
   pragma Inline (Scale_Key_Next);
   pragma Inline (Current_Scale_Name);
   pragma Inline (Current_Scale);
   pragma Inline (Scale_Next);
   pragma Inline (Scale_Prev);
   pragma Inline (Current_Tonic);
   pragma Inline (Current_Chord_Intervals);
   pragma Inline (Current_Chord);
end WNM.Chord_Sequencer;
