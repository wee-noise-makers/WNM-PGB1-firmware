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

with WNM.GUI.Bitmap_Fonts;

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

   type Scale_Range is range 1 .. 7;
   type Scale_Intervals is array (Scale_Range) of Interval;
   type Scale_Notes is array (Scale_Range) of MIDI.MIDI_Key;

   type Chord_Name is (Maj_Triad, Min_Triad, Dim_Triad,
                       Maj_7th, Min_7th, Dim_7th,
                       Min6,
                       Sus2, Sus4, Five,
                       Maj_Inv1, Maj_Inv2,
                       Min_Inv1, Min_Inv2);

   function Substitution (Name : Chord_Name) return Chord_Name
   is (case Name is
          when Maj_Triad => Maj_7th,
          when Min_Triad => Min_7th,
          when Maj_7th => Maj_Triad,
          when Min_7th => Min_Triad,
          when Maj_Inv1 => Maj_Inv2,
          when Maj_Inv2 => Maj_Inv1,
          when Min_Inv1 => Min_Inv2,
          when Min_Inv2 => Min_Inv1,
          when Dim_7th => Dim_Triad,
          when Dim_Triad => Dim_7th,
          when Sus2 => Sus4,
          when Sus4 => Sus2,
          when Min6 => Min6,
          when Five => Five);

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
     (Maj_Triad    => (P1, Maj3, P5, Octave),
      Min_Triad    => (P1, min3, P5, Octave),
      Dim_Triad    => (P1, min3, d5, Octave),
      Maj_7th      => (P1, Maj3, P5, Maj7),
      Min_7th      => (P1, min3, P5, min7),
      Dim_7th      => (P1, min3, d5, d7),
      Min6         => (P1, min3, P5, d7),
      Sus2         => (P1, Maj2, P5, Octave),
      Sus4         => (P1, P4, P5, Octave),
      Five         => (P1, P5, Octave, min13),
      Maj_Inv1     => (Maj3, P5, Octave, Maj10),
      Maj_Inv2     => (P5, Octave, Maj10, P12),
      Min_Inv1     => (min3, P5, Octave, min10),
      Min_Inv2     => (P5, Octave, min10, P12));
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

   type Accidentals is (Flat, Natural, Sharp);

   type Roman_Numeral_Notation is record
      Degree : Scale_Range;
      Accidental : Accidentals;
      Harmonic_Function : Chord_Name;
   end record;

   type Roman_Numeral_Progression
   is array (Integer range <>) of Roman_Numeral_Notation;

   type Progression_Collection
   is array (Integer range <>)
     of not null access constant Roman_Numeral_Progression;

   function Img (S : Scale_Name) return String
   is (case S is
          when Major_Scale => "Major",
          when Minor_Scale => "Minor");

   function Img (N : Chord_Name) return String
   is (case N is
          when Maj_Triad => "Major",
          when Min_Triad => "minor",
          when Dim_Triad => "Dim (" & GUI.Bitmap_Fonts.Dim & ")",
          when Maj_7th   => "Major7 (" & GUI.Bitmap_Fonts.Seven & ")",
          when Min_7th   => "minor7 (" & GUI.Bitmap_Fonts.Seven & ")",
          when Dim_7th   => "Dim7 (" & GUI.Bitmap_Fonts.Dim &
                                       GUI.Bitmap_Fonts.Seven & ")",
          when Min6      => "m6",
          when Sus2      => "Sus2 (" & GUI.Bitmap_Fonts.Sus2 & ")",
          when Sus4      => "Sus4 (" & GUI.Bitmap_Fonts.Sus4 & ")",
          when Five      => "5",
          when Maj_Inv1  => "Maj/inv1",
          when Maj_Inv2  => "Maj/inv2",
          when Min_Inv1  => "Min/inv1",
          when Min_Inv2  => "Min/inv2");

   function Img (C : Chord_Notes) return String
   is (Key_Img (C (0)) & " " &
       Key_Img (C (1)) & " " &
       Key_Img (C (2)) & " " &
       Key_Img (C (3)));

   function Img (A : Accidentals) return String
   is (case A is
          when Natural => "",
          when Sharp => "#",
          when Flat => "b");

   function Img (Degree : Scale_Range; Major : Boolean := True) return String
   is (if Major
       then (case Degree is
            when 1 => "I",
            when 2 => "II",
            when 3 => "III",
            when 4 => "IV",
            when 5 => "V",
            when 6 => "VI",
            when 7 => "VII")
       else (case Degree is
            when 1 => "i",
            when 2 => "ii",
            when 3 => "iii",
            when 4 => "iv",
            when 5 => "v",
            when 6 => "vi",
            when 7 => "vii"));

   function Img (C : Roman_Numeral_Notation) return String
   is (Img (C.Accidental) & Img (C.Degree) & Img (C.Harmonic_Function));

   function "+" (K : MIDI.MIDI_Key; I : Interval)
                 return MIDI.MIDI_Key
   is (if K <= MIDI.MIDI_Key'Last - I'Enum_Rep
       then K + I'Enum_Rep
       else K)
   with Inline;

   function "+" (K : MIDI.MIDI_Key; A : Accidentals)
                 return MIDI.MIDI_Key
   is (case A is
          when Flat    => (if K = MIDI.MIDI_Key'First
                           then MIDI.MIDI_Key'First
                           else K - 1),
          when Natural => K,
          when Sharp   => (if K = MIDI.MIDI_Key'Last
                           then MIDI.MIDI_Key'Last
                           else K + 1))
   with Inline;

   function "+" (K : MIDI.MIDI_Key; I : Chord_Intervals)
                 return Chord_Notes
   is ((K + I (0), K + I (1), K + I (2), K + I (3)))
     with Inline;

   function To_Notes (R     : Roman_Numeral_Notation;
                      Key   : MIDI.MIDI_Key;
                      Scale : Scale_Name)
                      return Chord_Notes
   is (((Key + Scales (Scale)(R.Degree)) + R.Accidental) +
         Chords (R.Harmonic_Function));

   function Tonic (R     : Roman_Numeral_Notation;
                   Key   : MIDI.MIDI_Key;
                   Scale : Scale_Name)
                   return MIDI.MIDI_Key
   is ((Key + Scales (Scale)(R.Degree)) + R.Accidental);

   --  min
   bImin : constant Roman_Numeral_Notation := (1, Flat, Min_Triad);
   bIImin : constant Roman_Numeral_Notation := (2, Flat, Min_Triad);
   bIIImin : constant Roman_Numeral_Notation := (3, Flat, Min_Triad);
   bIVmin : constant Roman_Numeral_Notation := (4, Flat, Min_Triad);
   bVmin : constant Roman_Numeral_Notation := (5, Flat, Min_Triad);
   bVImin : constant Roman_Numeral_Notation := (6, Flat, Min_Triad);
   bVIImin : constant Roman_Numeral_Notation := (7, Flat, Min_Triad);
   Imin : constant Roman_Numeral_Notation := (1, Natural, Min_Triad);
   IImin : constant Roman_Numeral_Notation := (2, Natural, Min_Triad);
   IIImin : constant Roman_Numeral_Notation := (3, Natural, Min_Triad);
   IVmin : constant Roman_Numeral_Notation := (4, Natural, Min_Triad);
   Vmin : constant Roman_Numeral_Notation := (5, Natural, Min_Triad);
   VImin : constant Roman_Numeral_Notation := (6, Natural, Min_Triad);
   VIImin : constant Roman_Numeral_Notation := (7, Natural, Min_Triad);
   sImin : constant Roman_Numeral_Notation := (1, Sharp, Min_Triad);
   sIImin : constant Roman_Numeral_Notation := (2, Sharp, Min_Triad);
   sIIImin : constant Roman_Numeral_Notation := (3, Sharp, Min_Triad);
   sIVmin : constant Roman_Numeral_Notation := (4, Sharp, Min_Triad);
   sVmin : constant Roman_Numeral_Notation := (5, Sharp, Min_Triad);
   sVImin : constant Roman_Numeral_Notation := (6, Sharp, Min_Triad);
   sVIImin : constant Roman_Numeral_Notation := (7, Sharp, Min_Triad);
   --  maj
   bImaj : constant Roman_Numeral_Notation := (1, Flat, Maj_Triad);
   bIImaj : constant Roman_Numeral_Notation := (2, Flat, Maj_Triad);
   bIIImaj : constant Roman_Numeral_Notation := (3, Flat, Maj_Triad);
   bIVmaj : constant Roman_Numeral_Notation := (4, Flat, Maj_Triad);
   bVmaj : constant Roman_Numeral_Notation := (5, Flat, Maj_Triad);
   bVImaj : constant Roman_Numeral_Notation := (6, Flat, Maj_Triad);
   bVIImaj : constant Roman_Numeral_Notation := (7, Flat, Maj_Triad);
   Imaj : constant Roman_Numeral_Notation := (1, Natural, Maj_Triad);
   IImaj : constant Roman_Numeral_Notation := (2, Natural, Maj_Triad);
   IIImaj : constant Roman_Numeral_Notation := (3, Natural, Maj_Triad);
   IVmaj : constant Roman_Numeral_Notation := (4, Natural, Maj_Triad);
   Vmaj : constant Roman_Numeral_Notation := (5, Natural, Maj_Triad);
   VImaj : constant Roman_Numeral_Notation := (6, Natural, Maj_Triad);
   VIImaj : constant Roman_Numeral_Notation := (7, Natural, Maj_Triad);
   sImaj : constant Roman_Numeral_Notation := (1, Sharp, Maj_Triad);
   sIImaj : constant Roman_Numeral_Notation := (2, Sharp, Maj_Triad);
   sIIImaj : constant Roman_Numeral_Notation := (3, Sharp, Maj_Triad);
   sIVmaj : constant Roman_Numeral_Notation := (4, Sharp, Maj_Triad);
   sVmaj : constant Roman_Numeral_Notation := (5, Sharp, Maj_Triad);
   sVImaj : constant Roman_Numeral_Notation := (6, Sharp, Maj_Triad);
   sVIImaj : constant Roman_Numeral_Notation := (7, Sharp, Maj_Triad);
   --  dim
   bIdim : constant Roman_Numeral_Notation := (1, Flat, Dim_Triad);
   bIIdim : constant Roman_Numeral_Notation := (2, Flat, Dim_Triad);
   bIIIdim : constant Roman_Numeral_Notation := (3, Flat, Dim_Triad);
   bIVdim : constant Roman_Numeral_Notation := (4, Flat, Dim_Triad);
   bVdim : constant Roman_Numeral_Notation := (5, Flat, Dim_Triad);
   bVIdim : constant Roman_Numeral_Notation := (6, Flat, Dim_Triad);
   bVIIdim : constant Roman_Numeral_Notation := (7, Flat, Dim_Triad);
   Idim : constant Roman_Numeral_Notation := (1, Natural, Dim_Triad);
   IIdim : constant Roman_Numeral_Notation := (2, Natural, Dim_Triad);
   IIIdim : constant Roman_Numeral_Notation := (3, Natural, Dim_Triad);
   IVdim : constant Roman_Numeral_Notation := (4, Natural, Dim_Triad);
   Vdim : constant Roman_Numeral_Notation := (5, Natural, Dim_Triad);
   VIdim : constant Roman_Numeral_Notation := (6, Natural, Dim_Triad);
   VIIdim : constant Roman_Numeral_Notation := (7, Natural, Dim_Triad);
   sIdim : constant Roman_Numeral_Notation := (1, Sharp, Dim_Triad);
   sIIdim : constant Roman_Numeral_Notation := (2, Sharp, Dim_Triad);
   sIIIdim : constant Roman_Numeral_Notation := (3, Sharp, Dim_Triad);
   sIVdim : constant Roman_Numeral_Notation := (4, Sharp, Dim_Triad);
   sVdim : constant Roman_Numeral_Notation := (5, Sharp, Dim_Triad);
   sVIdim : constant Roman_Numeral_Notation := (6, Sharp, Dim_Triad);
   sVIIdim : constant Roman_Numeral_Notation := (7, Sharp, Dim_Triad);
   --  sus4
   bIsus4 : constant Roman_Numeral_Notation := (1, Flat, Sus4);
   bIIsus4 : constant Roman_Numeral_Notation := (2, Flat, Sus4);
   bIIIsus4 : constant Roman_Numeral_Notation := (3, Flat, Sus4);
   bIVsus4 : constant Roman_Numeral_Notation := (4, Flat, Sus4);
   bVsus4 : constant Roman_Numeral_Notation := (5, Flat, Sus4);
   bVIsus4 : constant Roman_Numeral_Notation := (6, Flat, Sus4);
   bVIIsus4 : constant Roman_Numeral_Notation := (7, Flat, Sus4);
   Isus4 : constant Roman_Numeral_Notation := (1, Natural, Sus4);
   IIsus4 : constant Roman_Numeral_Notation := (2, Natural, Sus4);
   IIIsus4 : constant Roman_Numeral_Notation := (3, Natural, Sus4);
   IVsus4 : constant Roman_Numeral_Notation := (4, Natural, Sus4);
   Vsus4 : constant Roman_Numeral_Notation := (5, Natural, Sus4);
   VIsus4 : constant Roman_Numeral_Notation := (6, Natural, Sus4);
   VIIsus4 : constant Roman_Numeral_Notation := (7, Natural, Sus4);
   sIsus4 : constant Roman_Numeral_Notation := (1, Sharp, Sus4);
   sIIsus4 : constant Roman_Numeral_Notation := (2, Sharp, Sus4);
   sIIIsus4 : constant Roman_Numeral_Notation := (3, Sharp, Sus4);
   sIVsus4 : constant Roman_Numeral_Notation := (4, Sharp, Sus4);
   sVsus4 : constant Roman_Numeral_Notation := (5, Sharp, Sus4);
   sVIsus4 : constant Roman_Numeral_Notation := (6, Sharp, Sus4);
   sVIIsus4 : constant Roman_Numeral_Notation := (7, Sharp, Sus4);
   --  sus2
   bIsus2 : constant Roman_Numeral_Notation := (1, Flat, Sus2);
   bIIsus2 : constant Roman_Numeral_Notation := (2, Flat, Sus2);
   bIIIsus2 : constant Roman_Numeral_Notation := (3, Flat, Sus2);
   bIVsus2 : constant Roman_Numeral_Notation := (4, Flat, Sus2);
   bVsus2 : constant Roman_Numeral_Notation := (5, Flat, Sus2);
   bVIsus2 : constant Roman_Numeral_Notation := (6, Flat, Sus2);
   bVIIsus2 : constant Roman_Numeral_Notation := (7, Flat, Sus2);
   Isus2 : constant Roman_Numeral_Notation := (1, Natural, Sus2);
   IIsus2 : constant Roman_Numeral_Notation := (2, Natural, Sus2);
   IIIsus2 : constant Roman_Numeral_Notation := (3, Natural, Sus2);
   IVsus2 : constant Roman_Numeral_Notation := (4, Natural, Sus2);
   Vsus2 : constant Roman_Numeral_Notation := (5, Natural, Sus2);
   VIsus2 : constant Roman_Numeral_Notation := (6, Natural, Sus2);
   VIIsus2 : constant Roman_Numeral_Notation := (7, Natural, Sus2);
   sIsus2 : constant Roman_Numeral_Notation := (1, Sharp, Sus2);
   sIIsus2 : constant Roman_Numeral_Notation := (2, Sharp, Sus2);
   sIIIsus2 : constant Roman_Numeral_Notation := (3, Sharp, Sus2);
   sIVsus2 : constant Roman_Numeral_Notation := (4, Sharp, Sus2);
   sVsus2 : constant Roman_Numeral_Notation := (5, Sharp, Sus2);
   sVIsus2 : constant Roman_Numeral_Notation := (6, Sharp, Sus2);
   sVIIsus2 : constant Roman_Numeral_Notation := (7, Sharp, Sus2);
   --  7
   bI7 : constant Roman_Numeral_Notation := (1, Flat, Maj_7th);
   bII7 : constant Roman_Numeral_Notation := (2, Flat, Maj_7th);
   bIII7 : constant Roman_Numeral_Notation := (3, Flat, Maj_7th);
   bIV7 : constant Roman_Numeral_Notation := (4, Flat, Maj_7th);
   bV7 : constant Roman_Numeral_Notation := (5, Flat, Maj_7th);
   bVI7 : constant Roman_Numeral_Notation := (6, Flat, Maj_7th);
   bVII7 : constant Roman_Numeral_Notation := (7, Flat, Maj_7th);
   I7 : constant Roman_Numeral_Notation := (1, Natural, Maj_7th);
   II7 : constant Roman_Numeral_Notation := (2, Natural, Maj_7th);
   III7 : constant Roman_Numeral_Notation := (3, Natural, Maj_7th);
   IV7 : constant Roman_Numeral_Notation := (4, Natural, Maj_7th);
   V7 : constant Roman_Numeral_Notation := (5, Natural, Maj_7th);
   VI7 : constant Roman_Numeral_Notation := (6, Natural, Maj_7th);
   VII7 : constant Roman_Numeral_Notation := (7, Natural, Maj_7th);
   sI7 : constant Roman_Numeral_Notation := (1, Sharp, Maj_7th);
   sII7 : constant Roman_Numeral_Notation := (2, Sharp, Maj_7th);
   sIII7 : constant Roman_Numeral_Notation := (3, Sharp, Maj_7th);
   sIV7 : constant Roman_Numeral_Notation := (4, Sharp, Maj_7th);
   sV7 : constant Roman_Numeral_Notation := (5, Sharp, Maj_7th);
   sVI7 : constant Roman_Numeral_Notation := (6, Sharp, Maj_7th);
   sVII7 : constant Roman_Numeral_Notation := (7, Sharp, Maj_7th);
   --  m7
   bIm7 : constant Roman_Numeral_Notation := (1, Flat, Min_7th);
   bIIm7 : constant Roman_Numeral_Notation := (2, Flat, Min_7th);
   bIIIm7 : constant Roman_Numeral_Notation := (3, Flat, Min_7th);
   bIVm7 : constant Roman_Numeral_Notation := (4, Flat, Min_7th);
   bVm7 : constant Roman_Numeral_Notation := (5, Flat, Min_7th);
   bVIm7 : constant Roman_Numeral_Notation := (6, Flat, Min_7th);
   bVIIm7 : constant Roman_Numeral_Notation := (7, Flat, Min_7th);
   Im7 : constant Roman_Numeral_Notation := (1, Natural, Min_7th);
   IIm7 : constant Roman_Numeral_Notation := (2, Natural, Min_7th);
   IIIm7 : constant Roman_Numeral_Notation := (3, Natural, Min_7th);
   IVm7 : constant Roman_Numeral_Notation := (4, Natural, Min_7th);
   Vm7 : constant Roman_Numeral_Notation := (5, Natural, Min_7th);
   VIm7 : constant Roman_Numeral_Notation := (6, Natural, Min_7th);
   VIIm7 : constant Roman_Numeral_Notation := (7, Natural, Min_7th);
   sIm7 : constant Roman_Numeral_Notation := (1, Sharp, Min_7th);
   sIIm7 : constant Roman_Numeral_Notation := (2, Sharp, Min_7th);
   sIIIm7 : constant Roman_Numeral_Notation := (3, Sharp, Min_7th);
   sIVm7 : constant Roman_Numeral_Notation := (4, Sharp, Min_7th);
   sVm7 : constant Roman_Numeral_Notation := (5, Sharp, Min_7th);
   sVIm7 : constant Roman_Numeral_Notation := (6, Sharp, Min_7th);
   sVIIm7 : constant Roman_Numeral_Notation := (7, Sharp, Min_7th);
   --  5
   bI5 : constant Roman_Numeral_Notation := (1, Flat, Five);
   bII5 : constant Roman_Numeral_Notation := (2, Flat, Five);
   bIII5 : constant Roman_Numeral_Notation := (3, Flat, Five);
   bIV5 : constant Roman_Numeral_Notation := (4, Flat, Five);
   bV5 : constant Roman_Numeral_Notation := (5, Flat, Five);
   bVI5 : constant Roman_Numeral_Notation := (6, Flat, Five);
   bVII5 : constant Roman_Numeral_Notation := (7, Flat, Five);
   I5 : constant Roman_Numeral_Notation := (1, Natural, Five);
   II5 : constant Roman_Numeral_Notation := (2, Natural, Five);
   III5 : constant Roman_Numeral_Notation := (3, Natural, Five);
   IV5 : constant Roman_Numeral_Notation := (4, Natural, Five);
   V5 : constant Roman_Numeral_Notation := (5, Natural, Five);
   VI5 : constant Roman_Numeral_Notation := (6, Natural, Five);
   VII5 : constant Roman_Numeral_Notation := (7, Natural, Five);
   sI5 : constant Roman_Numeral_Notation := (1, Sharp, Five);
   sII5 : constant Roman_Numeral_Notation := (2, Sharp, Five);
   sIII5 : constant Roman_Numeral_Notation := (3, Sharp, Five);
   sIV5 : constant Roman_Numeral_Notation := (4, Sharp, Five);
   sV5 : constant Roman_Numeral_Notation := (5, Sharp, Five);
   sVI5 : constant Roman_Numeral_Notation := (6, Sharp, Five);
   sVII5 : constant Roman_Numeral_Notation := (7, Sharp, Five);
   Major_Progression_0 : aliased constant Roman_Numeral_Progression :=
     (Imaj, Imaj, IVmaj, IIImin);
   --  I I IV iii

   Major_Progression_1 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IIImin, IVmaj, VImin);
   --  I iii IV vi

   Major_Progression_2 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IIImin, VImin, Isus4);
   --  I iii vi Isus4

   Major_Progression_3 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IIImin, VImin, IVmaj);
   --  I iii vi IV

   Major_Progression_4 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IVmaj, IImin, Vmaj);
   --  I IV ii V

   Major_Progression_5 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IVmaj, Isus2, IVmaj);
   --  I IV Isus2 IV

   Major_Progression_6 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IVmaj, Vmaj, IVmaj);
   --  I IV V IV

   Major_Progression_7 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IVmaj, Vmaj, Vmaj);
   --  I IV V V

   Major_Progression_8 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IVmaj, VImin, Vmaj);
   --  I IV vi V

   Major_Progression_9 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IVmaj, VIImin, IIImin, VImin, IImin, Vmaj, Imaj);
   --  I IV vii iii vi ii V I

   Major_Progression_10 : aliased constant Roman_Numeral_Progression :=
     (Imaj, Vmaj, Imaj, IVmaj);
   --  I V I IV

   Major_Progression_11 : aliased constant Roman_Numeral_Progression :=
     (Imaj, Vmaj, IVmaj, VImin);
   --  I V IV vi

   Major_Progression_12 : aliased constant Roman_Numeral_Progression :=
     (Imaj, Vmaj, VImin, IImin);
   --  I V vi ii

   Major_Progression_13 : aliased constant Roman_Numeral_Progression :=
     (Imaj, Vmaj, VImin, IIImin, IVmaj, Imaj, IVmaj, Vmaj);
   --  I V vi iii IV I IV V

   Major_Progression_14 : aliased constant Roman_Numeral_Progression :=
     (Imaj, Vmaj, VImin, IIImin, IVmaj);
   --  I V vi iii IV

   Major_Progression_15 : aliased constant Roman_Numeral_Progression :=
     (Imaj, Vmaj, VImin, IVmaj);
   --  I V vi IV

   Major_Progression_16 : aliased constant Roman_Numeral_Progression :=
     (Imaj, Vmaj, VImin, Vmaj);
   --  I V vi V

   Major_Progression_17 : aliased constant Roman_Numeral_Progression :=
     (Imaj, VImin, Imaj, IVmaj);
   --  I vi I IV

   Major_Progression_18 : aliased constant Roman_Numeral_Progression :=
     (Imaj, VImin, IImin, IVmaj);
   --  I vi ii IV

   Major_Progression_19 : aliased constant Roman_Numeral_Progression :=
     (Imaj, VImin, IImin, Vmaj);
   --  I vi ii V

   Major_Progression_20 : aliased constant Roman_Numeral_Progression :=
     (Imaj, VImin, IVmaj, IIImin);
   --  I vi IV iii

   Major_Progression_21 : aliased constant Roman_Numeral_Progression :=
     (Imaj, VImin, IVmaj, Vmaj);
   --  I vi IV V

   Major_Progression_22 : aliased constant Roman_Numeral_Progression :=
     (IImin, IVmaj, Vmaj, Vmaj);
   --  ii IV V V

   Major_Progression_23 : aliased constant Roman_Numeral_Progression :=
     (IImin, IVmaj, VImin, Vmaj);
   --  ii IV vi V

   Major_Progression_24 : aliased constant Roman_Numeral_Progression :=
     (IImin, Vmaj, Imaj, Imaj);
   --  ii V I I

   Major_Progression_25 : aliased constant Roman_Numeral_Progression :=
     (IImin, Vmaj, Imaj, IVmaj);
   --  ii V I IV

   Major_Progression_26 : aliased constant Roman_Numeral_Progression :=
     (IIImin, VImin, IVmaj, Imaj);
   --  iii vi IV I

   Major_Progression_27 : aliased constant Roman_Numeral_Progression :=
     (IIm7, V7, IIIm7, VIm7, IIm7, V7);
   --  iim7 V7 iiim7 vi7 iim7 V7

   Major_Progression_28 : aliased constant Roman_Numeral_Progression :=
     (Isus2, Imaj, VIm7, VIsus4);
   --  Isus2 I vi7 visus4

   Major_Progression_29 : aliased constant Roman_Numeral_Progression :=
     (IVmaj, Imaj, IImin, VImin);
   --  IV I ii vi

   Major_Progression_30 : aliased constant Roman_Numeral_Progression :=
     (IVmaj, Imaj, IIImin, IVmaj);
   --  IV I iii IV

   Major_Progression_31 : aliased constant Roman_Numeral_Progression :=
     (IVmaj, Imaj, Vmaj, VImin);
   --  IV I V vi

   Major_Progression_32 : aliased constant Roman_Numeral_Progression :=
     (IVmaj, IVmaj, Imaj, Vmaj);
   --  IV IV I V

   Major_Progression_33 : aliased constant Roman_Numeral_Progression :=
     (IVmaj, VImin, Imaj, Vmaj);
   --  IV vi I V

   Major_Progression_34 : aliased constant Roman_Numeral_Progression :=
     (IVmaj, VImin, IIImin, Imaj);
   --  IV vi iii I

   Major_Progression_35 : aliased constant Roman_Numeral_Progression :=
     (IVmaj, VImin, IVmaj, VImin);
   --  IV vi IV vi

   Major_Progression_36 : aliased constant Roman_Numeral_Progression :=
     (Vmaj, Imaj, VImin, Vmaj);
   --  V I vi V

   Major_Progression_37 : aliased constant Roman_Numeral_Progression :=
     (Vmaj, IVmaj, VImin, Imaj);
   --  V IV vi I

   Major_Progression_38 : aliased constant Roman_Numeral_Progression :=
     (Vmaj, VImin, IVmaj, Imaj);
   --  V vi IV I

   Major_Progression_39 : aliased constant Roman_Numeral_Progression :=
     (VImin, IImin, Vmaj, Imaj);
   --  vi ii V I

   Major_Progression_40 : aliased constant Roman_Numeral_Progression :=
     (VImin, IVmaj, Imaj, Vmaj);
   --  vi IV I V

   Major_Progression_41 : aliased constant Roman_Numeral_Progression :=
     (VImin, Vmaj, IVmaj, Vmaj, IImin, Vmaj, Imaj, Imaj);
   --  vi V IV V ii V I I

   Major_Progression_42 : aliased constant Roman_Numeral_Progression :=
     (VImin, Vmaj, IVmaj, Vmaj);
   --  vi V IV V

   Major_Progressions : aliased constant Progression_Collection := (
     Major_Progression_0'Access,
     Major_Progression_1'Access,
     Major_Progression_2'Access,
     Major_Progression_3'Access,
     Major_Progression_4'Access,
     Major_Progression_5'Access,
     Major_Progression_6'Access,
     Major_Progression_7'Access,
     Major_Progression_8'Access,
     Major_Progression_9'Access,
     Major_Progression_10'Access,
     Major_Progression_11'Access,
     Major_Progression_12'Access,
     Major_Progression_13'Access,
     Major_Progression_14'Access,
     Major_Progression_15'Access,
     Major_Progression_16'Access,
     Major_Progression_17'Access,
     Major_Progression_18'Access,
     Major_Progression_19'Access,
     Major_Progression_20'Access,
     Major_Progression_21'Access,
     Major_Progression_22'Access,
     Major_Progression_23'Access,
     Major_Progression_24'Access,
     Major_Progression_25'Access,
     Major_Progression_26'Access,
     Major_Progression_27'Access,
     Major_Progression_28'Access,
     Major_Progression_29'Access,
     Major_Progression_30'Access,
     Major_Progression_31'Access,
     Major_Progression_32'Access,
     Major_Progression_33'Access,
     Major_Progression_34'Access,
     Major_Progression_35'Access,
     Major_Progression_36'Access,
     Major_Progression_37'Access,
     Major_Progression_38'Access,
     Major_Progression_39'Access,
     Major_Progression_40'Access,
     Major_Progression_41'Access,
     Major_Progression_42'Access
     );
   Minor_Progression_0 : aliased constant Roman_Numeral_Progression :=
     (Imin, IImin, Vmin, Imin);
   --  i ii v i

   Minor_Progression_1 : aliased constant Roman_Numeral_Progression :=
     (Imin, IIImaj, IVmin, VImaj);
   --  i III iv VI

   Minor_Progression_2 : aliased constant Roman_Numeral_Progression :=
     (Imin, IIImaj, VIImaj, VImaj);
   --  i III VII VI

   Minor_Progression_3 : aliased constant Roman_Numeral_Progression :=
     (Imin, IImin, Vmin, IIImaj, Imin, IImin, Vmin, VIImaj);
   --  i ii v III i ii v VII

   Minor_Progression_4 : aliased constant Roman_Numeral_Progression :=
     (Imin, IVmin, IIImaj, VImaj);
   --  i iv III VI

   Minor_Progression_5 : aliased constant Roman_Numeral_Progression :=
     (Imin, IVmin, Vmin, IVmin);
   --  i iv v iv

   Minor_Progression_6 : aliased constant Roman_Numeral_Progression :=
     (Imin, IVmin, Vmin, Vmin);
   --  i iv v v

   Minor_Progression_7 : aliased constant Roman_Numeral_Progression :=
     (Imin, IVmin, VImaj, Vmin);
   --  i iv VI v

   Minor_Progression_8 : aliased constant Roman_Numeral_Progression :=
     (Imin, IVmin, VIImaj, Imin);
   --  i iv VII i

   Minor_Progression_9 : aliased constant Roman_Numeral_Progression :=
     (Imin, IVmin, VIImaj, Vmin, Imin, Imin, IImin, Vmaj);
   --  i iv VII v i i ii V

   Minor_Progression_10 : aliased constant Roman_Numeral_Progression :=
     (Imin, Vmin, IVmin, VIImaj);
   --  i v iv VII

   Minor_Progression_11 : aliased constant Roman_Numeral_Progression :=
     (Imin, Vdim, IVmin, VImaj);
   --  i vdim iv VI

   Minor_Progression_12 : aliased constant Roman_Numeral_Progression :=
     (Imin, VImaj, IIImaj, VIImaj);
   --  i VI III VII

   Minor_Progression_13 : aliased constant Roman_Numeral_Progression :=
     (Imin, VImaj, IVmin, IImin);
   --  i VI iv ii

   Minor_Progression_14 : aliased constant Roman_Numeral_Progression :=
     (Imin, VImaj, IVmin, IIImaj);
   --  i VI iv III

   Minor_Progression_15 : aliased constant Roman_Numeral_Progression :=
     (Imin, VImaj, IVmin, Vmin);
   --  i VI iv v

   Minor_Progression_16 : aliased constant Roman_Numeral_Progression :=
     (Imin, VImaj, VIImaj, IVmin);
   --  i VI VII iv

   Minor_Progression_17 : aliased constant Roman_Numeral_Progression :=
     (Imin, VImaj, VIImaj, Vmin);
   --  i VI VII v

   Minor_Progression_18 : aliased constant Roman_Numeral_Progression :=
     (Imin, VImaj, VIImaj, VIImaj);
   --  i VI VII VII

   Minor_Progression_19 : aliased constant Roman_Numeral_Progression :=
     (Imin, VIImaj, Imin, Vmin, IIImaj, VIImaj, Imin, Vmin, Imin);
   --  i VII i v III VII i v i

   Minor_Progression_20 : aliased constant Roman_Numeral_Progression :=
     (Imin, VIImaj, Imin, Vmin);
   --  i VII i v

   Minor_Progression_21 : aliased constant Roman_Numeral_Progression :=
     (Imin, VIImaj, IIImaj, VImaj);
   --  i VII III VI

   Minor_Progression_22 : aliased constant Roman_Numeral_Progression :=
     (Imin, VIImaj, Vmin, VImaj);
   --  i VII v VI

   Minor_Progression_23 : aliased constant Roman_Numeral_Progression :=
     (Imin, VIImaj, VImaj, IIImaj, IVmin, VImaj, VIImaj, Imin);
   --  i VII VI III iv VI VII i

   Minor_Progression_24 : aliased constant Roman_Numeral_Progression :=
     (Imin, VIImaj, VImaj, IIImaj);
   --  i VII VI III

   Minor_Progression_25 : aliased constant Roman_Numeral_Progression :=
     (Imin, VIImaj, VImaj, IVmin);
   --  i VII VI iv

   Minor_Progression_26 : aliased constant Roman_Numeral_Progression :=
     (Imin, VIImaj, VImaj, VIImaj);
   --  i VII VI VII

   Minor_Progression_27 : aliased constant Roman_Numeral_Progression :=
     (Im7, VImaj, III7, VIImaj, Imin, Im7, III7, IVm7);
   --  i7 VI III7 VII6 i i7 III7 iv7

   Minor_Progression_28 : aliased constant Roman_Numeral_Progression :=
     (IImin, Vmin, Imin, Imin);
   --  ii v i i

   Minor_Progression_29 : aliased constant Roman_Numeral_Progression :=
     (IImin, Vmin, Imin, IVmin);
   --  ii v i iv

   Minor_Progression_30 : aliased constant Roman_Numeral_Progression :=
     (IImin, VImaj, Imin, IVmin);
   --  ii VI i iv

   Minor_Progression_31 : aliased constant Roman_Numeral_Progression :=
     (Im7, IVsus4, Vm7, Isus4);
   --  im7 ivsus4 v7 isus4

   Minor_Progression_32 : aliased constant Roman_Numeral_Progression :=
     (IVmin, Imin, Vmin, VImaj);
   --  iv i v VI

   Minor_Progression_33 : aliased constant Roman_Numeral_Progression :=
     (IVmin, IIImaj, VIImaj, Imin);
   --  iv III VII i

   Minor_Progression_34 : aliased constant Roman_Numeral_Progression :=
     (IVmin, IIImaj, Vsus4, VImaj, IVmin, Imin, IIImaj, VImaj);
   --  iv III vsus4 VI iv i III VI

   Minor_Progression_35 : aliased constant Roman_Numeral_Progression :=
     (IVmin, Vmin, VImaj, VIImaj);
   --  iv v VI VII

   Minor_Progression_36 : aliased constant Roman_Numeral_Progression :=
     (IVmin, VImaj, Vmin, VIImaj);
   --  iv VI v VII

   Minor_Progression_37 : aliased constant Roman_Numeral_Progression :=
     (IVmin, VImaj, VIImaj, Imin);
   --  iv VI VII i

   Minor_Progression_38 : aliased constant Roman_Numeral_Progression :=
     (Vmin, Imin, IVmin, VIImaj);
   --  v i iv VII

   Minor_Progression_39 : aliased constant Roman_Numeral_Progression :=
     (Vmin, IVmin, Imin, Imin);
   --  v iv i i

   Minor_Progression_40 : aliased constant Roman_Numeral_Progression :=
     (Vmin, VImaj, IIImaj, Imin);
   --  v VI III i

   Minor_Progression_41 : aliased constant Roman_Numeral_Progression :=
     (Vmin, VImaj, Vmin, Imin);
   --  v VI v i

   Minor_Progression_42 : aliased constant Roman_Numeral_Progression :=
     (VImaj, Imin, Vmin, IIImaj);
   --  VI i v III

   Minor_Progression_43 : aliased constant Roman_Numeral_Progression :=
     (VImaj, Imin, Vmin, Vmin);
   --  VI i v v

   Minor_Progression_44 : aliased constant Roman_Numeral_Progression :=
     (VImaj, IIImaj, Imin, Vmin);
   --  VI III i v

   Minor_Progression_45 : aliased constant Roman_Numeral_Progression :=
     (VImaj, IVmin, Imin, Vmin);
   --  VI iv i v

   Minor_Progression_46 : aliased constant Roman_Numeral_Progression :=
     (VImaj, VImaj, Imin, VIImaj);
   --  VI VI i VII

   Minor_Progression_47 : aliased constant Roman_Numeral_Progression :=
     (VImaj, VIImaj, Imin, IIImaj);
   --  VI VII i III

   Minor_Progression_48 : aliased constant Roman_Numeral_Progression :=
     (VImaj, VIImaj, Vmin, IIImaj);
   --  VI VII v III

   Minor_Progression_49 : aliased constant Roman_Numeral_Progression :=
     (VIImaj, IVmin, Vmin, Imin);
   --  VII iv v i

   Minor_Progression_50 : aliased constant Roman_Numeral_Progression :=
     (VIImaj, IVmin, VIImaj, Imin);
   --  VII iv VII i

   Minor_Progressions : aliased constant Progression_Collection := (
     Minor_Progression_0'Access,
     Minor_Progression_1'Access,
     Minor_Progression_2'Access,
     Minor_Progression_3'Access,
     Minor_Progression_4'Access,
     Minor_Progression_5'Access,
     Minor_Progression_6'Access,
     Minor_Progression_7'Access,
     Minor_Progression_8'Access,
     Minor_Progression_9'Access,
     Minor_Progression_10'Access,
     Minor_Progression_11'Access,
     Minor_Progression_12'Access,
     Minor_Progression_13'Access,
     Minor_Progression_14'Access,
     Minor_Progression_15'Access,
     Minor_Progression_16'Access,
     Minor_Progression_17'Access,
     Minor_Progression_18'Access,
     Minor_Progression_19'Access,
     Minor_Progression_20'Access,
     Minor_Progression_21'Access,
     Minor_Progression_22'Access,
     Minor_Progression_23'Access,
     Minor_Progression_24'Access,
     Minor_Progression_25'Access,
     Minor_Progression_26'Access,
     Minor_Progression_27'Access,
     Minor_Progression_28'Access,
     Minor_Progression_29'Access,
     Minor_Progression_30'Access,
     Minor_Progression_31'Access,
     Minor_Progression_32'Access,
     Minor_Progression_33'Access,
     Minor_Progression_34'Access,
     Minor_Progression_35'Access,
     Minor_Progression_36'Access,
     Minor_Progression_37'Access,
     Minor_Progression_38'Access,
     Minor_Progression_39'Access,
     Minor_Progression_40'Access,
     Minor_Progression_41'Access,
     Minor_Progression_42'Access,
     Minor_Progression_43'Access,
     Minor_Progression_44'Access,
     Minor_Progression_45'Access,
     Minor_Progression_46'Access,
     Minor_Progression_47'Access,
     Minor_Progression_48'Access,
     Minor_Progression_49'Access,
     Minor_Progression_50'Access
     );
   Modal_Progression_0 : aliased constant Roman_Numeral_Progression :=
     (bIIImaj, IImin, bIImaj, Imaj);
   --  bIIIM ii bIIM I

   Modal_Progression_1 : aliased constant Roman_Numeral_Progression :=
     (bIImaj, bVImaj, bIIImin, bVIImin);
   --  bIIM bVIM biii bviim

   Modal_Progression_2 : aliased constant Roman_Numeral_Progression :=
     (bIImaj, IVmin, bIIImin, Imin);
   --  bIIM ivm biii im

   Modal_Progression_3 : aliased constant Roman_Numeral_Progression :=
     (bVIImaj, bIImaj, bIIImaj, Imin);
   --  bVIIM bIIM bIIIM im

   Modal_Progression_4 : aliased constant Roman_Numeral_Progression :=
     (bVImaj, bIIImaj, bVIImaj, IVmaj, Imaj);
   --  bVIM bIIIM bVIIM IV I

   Modal_Progression_5 : aliased constant Roman_Numeral_Progression :=
     (bVImaj, bVIImin, Imin, bIImaj);
   --  bVIM bVIIm im bIIM

   Modal_Progression_6 : aliased constant Roman_Numeral_Progression :=
     (bVImaj, VImin, Imin, bVIImaj);
   --  bVIM vi im bVIIM

   Modal_Progression_7 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bIIImaj, bVIImaj, Imaj);
   --  I bIIIM bVIIM I

   Modal_Progression_8 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bIIImaj, bVIImaj, IVmaj);
   --  I bIIIM bVIIM IV

   Modal_Progression_9 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bIIImaj, bVImaj, bVIImaj);
   --  I bIIIM bVIM bVIIM

   Modal_Progression_10 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bIIImaj, IVmaj, Imaj);
   --  I bIIIM IV I

   Modal_Progression_11 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bIImaj, Imaj, IIImin);
   --  I bIIM I iii

   Modal_Progression_12 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bIImaj, bIIImaj, bIImaj, Imaj, Imin, bVIImaj, bIImaj);
   --  I bIIM7 bIIIM6 bIIM7 I im bVIIM bIIM

   Modal_Progression_13 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bVIImaj, bVImaj, bIImaj);
   --  I bVIIM bVIM bIIM

   Modal_Progression_14 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bVIImaj, bVImaj, IVmaj, IVsus4, IVmaj);
   --  I bVIIM bVIM IV IVsus4 IV

   Modal_Progression_15 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bVIImaj, Imaj, Imaj, bVImaj, Vmaj);
   --  I bVIIM I I bVIM V

   Modal_Progression_16 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bVIImaj, IVmaj, Vmaj);
   --  I bVIIM IV V

   Modal_Progression_17 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bVImaj, Imaj, bIImaj);
   --  I bVIM I bIIM

   Modal_Progression_18 : aliased constant Roman_Numeral_Progression :=
     (Imaj, bVImaj, IVmaj, bIIImaj, bVIImaj);
   --  I bVIM IV bIIIM bVIIM

   Modal_Progression_19 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IIImaj, VImin, Vmaj);
   --  I IIIM vi V

   Modal_Progression_20 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IImaj, IIImin, Vmaj);
   --  I IIM iii V6

   Modal_Progression_21 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IImaj, IVmaj, Imaj);
   --  I IIM IV I

   Modal_Progression_22 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IVmaj, bIIImaj, bVImaj);
   --  I IV bIIIM bVIM

   Modal_Progression_23 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IVmaj, bVIImaj, IVmaj);
   --  I IV bVIIM IV

   Modal_Progression_24 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IVmaj, Vmaj, bVIImaj);
   --  I IV V bVIIM

   Modal_Progression_25 : aliased constant Roman_Numeral_Progression :=
     (Imaj, IVmin, bIIImaj, bVIImaj);
   --  I ivm bIIIM bVIIM

   Modal_Progression_26 : aliased constant Roman_Numeral_Progression :=
     (Imaj, Vmaj, bVIImaj, IVmaj);
   --  I V bVIIM IV

   Modal_Progression_27 : aliased constant Roman_Numeral_Progression :=
     (Imaj, Vmaj, IVmin, bVImaj);
   --  I V ivm bVIM

   Modal_Progression_28 : aliased constant Roman_Numeral_Progression :=
     (I5, IIImin, II5, sIVmin, IV5, VImin, V5, VIImin);
   --  I5 iii II5 #IVm IV5 vi V5 viim

   Modal_Progression_29 : aliased constant Roman_Numeral_Progression :=
     (IImin, bIImaj, Imaj, bVIImaj);
   --  ii bIIM I bVIIM

   Modal_Progression_30 : aliased constant Roman_Numeral_Progression :=
     (IImin, bVIImaj, Imaj);
   --  ii bVIIM7 I

   Modal_Progression_31 : aliased constant Roman_Numeral_Progression :=
     (IImin, IVmaj, Vmin, bVIImaj);
   --  ii IVM vm bVIIM

   Modal_Progression_32 : aliased constant Roman_Numeral_Progression :=
     (IIImaj, Vmaj, VIsus4, VImaj, Imaj, IImaj);
   --  IIIM V VIsus4 VIM I IIM

   Modal_Progression_33 : aliased constant Roman_Numeral_Progression :=
     (Imin, bIIImaj, bVIImaj, IVmaj);
   --  im bIIIM bVIIM IV

   Modal_Progression_34 : aliased constant Roman_Numeral_Progression :=
     (Imin, bIIImaj, bVImaj, Vmaj);
   --  im bIIIM bVIM V

   Modal_Progression_35 : aliased constant Roman_Numeral_Progression :=
     (Imin, bIIImaj, IVmaj, bVImaj);
   --  im bIIIM IV bVIM

   Modal_Progression_36 : aliased constant Roman_Numeral_Progression :=
     (Imin, bIImaj, bIIImaj, bIImaj);
   --  im bIIM bIIIM bIIM

   Modal_Progression_37 : aliased constant Roman_Numeral_Progression :=
     (Imin, bIImaj, bIImin, IVmin);
   --  im bIIM biim6 ivm

   Modal_Progression_38 : aliased constant Roman_Numeral_Progression :=
     (Imin, bIImaj, Im7, bVIImin);
   --  im bIIM im7 bviim

   Modal_Progression_39 : aliased constant Roman_Numeral_Progression :=
     (Imin, bIImaj, IVmin, IIImaj, bIImaj, IVmin, IIImaj, IIImaj);
   --  im bIIM ivm IIIM bIIM ivm IIIM IIIM

   Modal_Progression_40 : aliased constant Roman_Numeral_Progression :=
     (Imin, bIImaj, Vmin, Im7);
   --  im bIIM vm im7

   Modal_Progression_41 : aliased constant Roman_Numeral_Progression :=
     (Imin, bVIImaj, bIImaj, Vmin);
   --  im bVIIM bIIM vm

   Modal_Progression_42 : aliased constant Roman_Numeral_Progression :=
     (Imin, bVIImin, bVImaj, bIImaj);
   --  im bviim bVIM bIIM

   Modal_Progression_43 : aliased constant Roman_Numeral_Progression :=
     (Imin, bVImaj, IVmin, Vmaj);
   --  im bVIM ivm V

   Modal_Progression_44 : aliased constant Roman_Numeral_Progression :=
     (Imin, IImin, Vmin, IVmaj);
   --  im ii vm IV

   Modal_Progression_45 : aliased constant Roman_Numeral_Progression :=
     (Imin, IVmin, bIImaj, Imin, Vmin, IVm7, bIImaj, Im7);
   --  im ivm9 bIIM im vm ivm7 bIIM im7

   Modal_Progression_46 : aliased constant Roman_Numeral_Progression :=
     (Imin, IVmin, bIImaj, Imin);
   --  im ivm9 bIIM im

   Modal_Progression_47 : aliased constant Roman_Numeral_Progression :=
     (Imin, Vmaj, bVIImaj, IVmaj, bVImaj, bIIImaj, IVmin, Vmaj);
   --  im V bVIIM IV bVIM bIIIM ivm V

   Modal_Progression_48 : aliased constant Roman_Numeral_Progression :=
     (Imin, VImaj, bImin, Vmaj);
   --  im VIM bi V

   Modal_Progression_49 : aliased constant Roman_Numeral_Progression :=
     (Imin, VImaj, IIImaj, bIImaj);
   --  im VIM IIIM bIIM

   Modal_Progression_50 : aliased constant Roman_Numeral_Progression :=
     (Imin, Vmin, bVImaj, bIImaj);
   --  im vm bVIM bIIM

   Modal_Progression_51 : aliased constant Roman_Numeral_Progression :=
     (Imin, Vmin, IVmin, bIImaj);
   --  im vm ivm bIIM7

   Modal_Progression_52 : aliased constant Roman_Numeral_Progression :=
     (IVmaj, Vmaj, IImin, Imin, bIIImaj, IVmaj);
   --  IV V ii im bIIIM IV

   Modal_Progression_53 : aliased constant Roman_Numeral_Progression :=
     (IVmin, bIIImaj, IIm7, Vmaj);
   --  ivm bIIIM iim7 V

   Modal_Progression_54 : aliased constant Roman_Numeral_Progression :=
     (IVmin, Imin, bVIImin, bVImaj);
   --  ivm im bviim bVIM

   Modal_Progression_55 : aliased constant Roman_Numeral_Progression :=
     (Vdim, Vdim, IVmin, bIIImaj);
   --  vdim vdim ivm bIIIM

   Modal_Progression_56 : aliased constant Roman_Numeral_Progression :=
     (VImin, bVImaj, bVIImaj, Imaj);
   --  vi bVIM bVIIM I

   Modal_Progression_57 : aliased constant Roman_Numeral_Progression :=
     (VImin, VIImin, Vmaj, VImin, sIVdim, Vmaj);
   --  vi vii V vi #IVdim V

   Modal_Progression_58 : aliased constant Roman_Numeral_Progression :=
     (VImaj, bVImaj, Imin, bVIImaj);
   --  VIM bVIM im bVIIM

   Modal_Progression_59 : aliased constant Roman_Numeral_Progression :=
     (bIIImaj, V7, Imaj);
   --  bIIIM V7 I

   Modal_Progression_60 : aliased constant Roman_Numeral_Progression :=
     (bVIImaj, V7, Imaj);
   --  bVIIM V7 I

   Modal_Progression_61 : aliased constant Roman_Numeral_Progression :=
     (Imin, bVIImaj, IVmaj, Imin);
   --  im bVIIM IV im

   Modal_Progression_62 : aliased constant Roman_Numeral_Progression :=
     (IVmin, bIIImaj, bIImaj, Imaj);
   --  ivm bIIIM bIIM I

   Modal_Progression_63 : aliased constant Roman_Numeral_Progression :=
     (IVmin, IIImaj, bIImaj, Imaj);
   --  ivm IIIM bIIM I

   Modal_Progression_64 : aliased constant Roman_Numeral_Progression :=
     (IVmin, bIIImaj, bVImaj, Imaj);
   --  ivm bIIIM bVIM I

   Modal_Progressions : aliased constant Progression_Collection := (
     Modal_Progression_0'Access,
     Modal_Progression_1'Access,
     Modal_Progression_2'Access,
     Modal_Progression_3'Access,
     Modal_Progression_4'Access,
     Modal_Progression_5'Access,
     Modal_Progression_6'Access,
     Modal_Progression_7'Access,
     Modal_Progression_8'Access,
     Modal_Progression_9'Access,
     Modal_Progression_10'Access,
     Modal_Progression_11'Access,
     Modal_Progression_12'Access,
     Modal_Progression_13'Access,
     Modal_Progression_14'Access,
     Modal_Progression_15'Access,
     Modal_Progression_16'Access,
     Modal_Progression_17'Access,
     Modal_Progression_18'Access,
     Modal_Progression_19'Access,
     Modal_Progression_20'Access,
     Modal_Progression_21'Access,
     Modal_Progression_22'Access,
     Modal_Progression_23'Access,
     Modal_Progression_24'Access,
     Modal_Progression_25'Access,
     Modal_Progression_26'Access,
     Modal_Progression_27'Access,
     Modal_Progression_28'Access,
     Modal_Progression_29'Access,
     Modal_Progression_30'Access,
     Modal_Progression_31'Access,
     Modal_Progression_32'Access,
     Modal_Progression_33'Access,
     Modal_Progression_34'Access,
     Modal_Progression_35'Access,
     Modal_Progression_36'Access,
     Modal_Progression_37'Access,
     Modal_Progression_38'Access,
     Modal_Progression_39'Access,
     Modal_Progression_40'Access,
     Modal_Progression_41'Access,
     Modal_Progression_42'Access,
     Modal_Progression_43'Access,
     Modal_Progression_44'Access,
     Modal_Progression_45'Access,
     Modal_Progression_46'Access,
     Modal_Progression_47'Access,
     Modal_Progression_48'Access,
     Modal_Progression_49'Access,
     Modal_Progression_50'Access,
     Modal_Progression_51'Access,
     Modal_Progression_52'Access,
     Modal_Progression_53'Access,
     Modal_Progression_54'Access,
     Modal_Progression_55'Access,
     Modal_Progression_56'Access,
     Modal_Progression_57'Access,
     Modal_Progression_58'Access,
     Modal_Progression_59'Access,
     Modal_Progression_60'Access,
     Modal_Progression_61'Access,
     Modal_Progression_62'Access,
     Modal_Progression_63'Access,
     Modal_Progression_64'Access
     );

end WNM.Chord_Settings;
