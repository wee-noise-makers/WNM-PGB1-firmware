-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2017 Fabien Chouteau                  --
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

with MIDI;

pragma Warnings (Off);
with WNM_Configuration; use WNM_Configuration;
with WNM_HAL;           use WNM_HAL;
with Enum_Next;
pragma Warnings (On);

package WNM is

   subtype Keyboard_Button is Button range B1 .. B16;

   type Keyboard_Value is range 1 .. 16;

   function To_Value (B : Keyboard_Button) return Keyboard_Value;
   function To_Button (V : Keyboard_Value) return Keyboard_Button;

   subtype Tracks is Keyboard_Value;
   subtype Patterns is Keyboard_Value;
   subtype Chords is Keyboard_Value;

   subtype Beat_Per_Minute is Positive range 50 .. 200;
   subtype Sequencer_Steps is Keyboard_Value;

   Steps_Per_Beat      : constant := 4;
   Max_Events_Per_Step : constant := 6;

   UI_Task_Period_Microseconds  : constant := 50 * 1_000;
   GUI_Task_Period_Microseconds : constant := 50 * 1_000;
   LED_Task_Period_Microseconds : constant := 50 * 1_000;

   Long_Press_Time_Span_Microseconds : constant := 300 * 1_000;
   --  How much time (in miliseconds) users have to press a button to get the
   --  alternative function.

   Audio_Queue_Size : constant := 3;

   function Img (V : Keyboard_Value) return String
   is (case V is
          when 1 => "01",
          when 2 => "02",
          when 3 => "03",
          when 4 => "04",
          when 5 => "05",
          when 6 => "06",
          when 7 => "07",
          when 8 => "08",
          when 9 => "09",
          when 10 => "10",
          when 11 => "11",
          when 12 => "12",
          when 13 => "13",
          when 14 => "14",
          when 15 => "15",
          when 16 => "16")
   with Inline_Always;

   type Rand_Percent is range 0 .. 100;
   function Random return Rand_Percent;

   generic
      type T is (<>);
   function Enum_Count return Natural;

   type MIDI_Target is (External, Internal);

   type CPU_Load is new Float range 0.0 .. Float'Last;

   function Img (L : CPU_Load) return String;

   subtype Key_Img_Str is String (1 .. 3);
   function Key_Img (V : MIDI.MIDI_Key) return Key_Img_Str
   is (case V is
          when MIDI.A0  => "A0 ",
          when MIDI.As0 => "A#0",
          when MIDI.B0  => "B0 ",
          when MIDI.C1  => "C1 ",
          when MIDI.Cs1 => "C#1",
          when MIDI.D1  => "D1 ",
          when MIDI.Ds1 => "D#1",
          when MIDI.E1  => "E1 ",
          when MIDI.F1  => "F1 ",
          when MIDI.Fs1 => "F#1",
          when MIDI.G1  => "G1 ",
          when MIDI.Gs1 => "G#1",
          when MIDI.A1  => "A1 ",
          when MIDI.As1 => "A#1",
          when MIDI.B1  => "B1 ",
          when MIDI.C2  => "C2 ",
          when MIDI.Cs2 => "C#2",
          when MIDI.D2  => "D2 ",
          when MIDI.Ds2 => "D#2",
          when MIDI.E2  => "E2 ",
          when MIDI.F2  => "F2 ",
          when MIDI.Fs2 => "F#2",
          when MIDI.G2  => "G2 ",
          when MIDI.Gs2 => "G#2",
          when MIDI.A2  => "A2 ",
          when MIDI.As2 => "A#2",
          when MIDI.B2  => "B2 ",
          when MIDI.C3  => "C3 ",
          when MIDI.Cs3 => "C#3",
          when MIDI.D3  => "D3 ",
          when MIDI.Ds3 => "D#3",
          when MIDI.E3  => "E3 ",
          when MIDI.F3  => "F3 ",
          when MIDI.Fs3 => "F#3",
          when MIDI.G3  => "G3 ",
          when MIDI.Gs3 => "G#3",
          when MIDI.A3  => "A3 ",
          when MIDI.As3 => "A#3",
          when MIDI.B3  => "B3 ",
          when MIDI.C4  => "C4 ",
          when MIDI.Cs4 => "C#4",
          when MIDI.D4  => "D4 ",
          when MIDI.Ds4 => "D#4",
          when MIDI.E4  => "E4 ",
          when MIDI.F4  => "F4 ",
          when MIDI.Fs4 => "F#4",
          when MIDI.G4  => "G4 ",
          when MIDI.Gs4 => "G#4",
          when MIDI.A4  => "A4 ",
          when MIDI.As4 => "A#4",
          when MIDI.B4  => "B4 ",
          when MIDI.C5  => "C5 ",
          when MIDI.Cs5 => "C#5",
          when MIDI.D5  => "D5 ",
          when MIDI.Ds5 => "D#5",
          when MIDI.E5  => "E5 ",
          when MIDI.F5  => "F5 ",
          when MIDI.Fs5 => "F#5",
          when MIDI.G5  => "G5 ",
          when MIDI.Gs5 => "G#5",
          when MIDI.A5  => "A5 ",
          when MIDI.As5 => "A#5",
          when MIDI.B5  => "B5 ",
          when MIDI.C6  => "C6 ",
          when MIDI.Cs6 => "C#6",
          when MIDI.D6  => "D6 ",
          when MIDI.Ds6 => "D#6",
          when MIDI.E6  => "E6 ",
          when MIDI.F6  => "F6 ",
          when MIDI.Fs6 => "F#6",
          when MIDI.G6  => "G6 ",
          when MIDI.Gs6 => "G#6",
          when MIDI.A6  => "A6 ",
          when MIDI.As6 => "A#6",
          when MIDI.B6  => "B6 ",
          when MIDI.C7  => "C7 ",
          when MIDI.Cs7 => "C#7",
          when MIDI.D7  => "D7 ",
          when MIDI.Ds7 => "D#7",
          when MIDI.E7  => "E7 ",
          when MIDI.F7  => "F7 ",
          when MIDI.Fs7 => "F#7",
          when MIDI.G7  => "G7 ",
          when MIDI.Gs7 => "G#7",
          when MIDI.A7  => "A7 ",
          when MIDI.As7 => "A#7",
          when MIDI.B7  => "B7 ",
          when MIDI.C8  => "C8 ",
          when MIDI.Cs8 => "C#8",
          when MIDI.D8  => "D8 ",
          when MIDI.Ds8 => "D#8",
          when MIDI.E8  => "E8 ",
          when MIDI.F8  => "F8 ",
          when MIDI.Fs8 => "F#8",
          when MIDI.G8  => "G8 ",
          when MIDI.Gs8 => "G#8",
          when MIDI.A8  => "A8 ",
          when MIDI.As8 => "A#8",
          when MIDI.B8  => "B8 ",
          when others => "---");

   Key_To_Frequency : constant array (MIDI.MIDI_Key) of Float :=
     (0   => 8.1757989156,
      1   => 8.6619572180,
      2   => 9.1770239974,
      3   => 9.7227182413,
      4   => 10.3008611535,
      5   => 10.9133822323,
      6   => 11.5623257097,
      7   => 12.2498573744,
      8   => 12.9782717994,
      9   => 13.7500000000,
      10  => 14.5676175474,
      11  => 15.4338531643,
      12  => 16.3515978313,
      13  => 17.3239144361,
      14  => 18.3540479948,
      15  => 19.4454364826,
      16  => 20.6017223071,
      17  => 21.8267644646,
      18  => 23.1246514195,
      19  => 24.4997147489,
      20  => 25.9565435987,
      21  => 27.5000000000,
      22  => 29.1352350949,
      23  => 30.8677063285,
      24  => 32.7031956626,
      25  => 34.6478288721,
      26  => 36.7080959897,
      27  => 38.8908729653,
      28  => 41.2034446141,
      29  => 43.6535289291,
      30  => 46.2493028390,
      31  => 48.9994294977,
      32  => 51.9130871975,
      33  => 55.0000000000,
      34  => 58.2704701898,
      35  => 61.7354126570,
      36  => 65.4063913251,
      37  => 69.2956577442,
      38  => 73.4161919794,
      39  => 77.7817459305,
      40  => 82.4068892282,
      41  => 87.3070578583,
      42  => 92.4986056779,
      43  => 97.9988589954,
      44  => 103.8261743950,
      45  => 110.0000000000,
      46  => 116.5409403795,
      47  => 123.4708253140,
      48  => 130.8127826503,
      49  => 138.5913154884,
      50  => 146.8323839587,
      51  => 155.5634918610,
      52  => 164.8137784564,
      53  => 174.6141157165,
      54  => 184.9972113558,
      55  => 195.9977179909,
      56  => 207.6523487900,
      57  => 220.0000000000,
      58  => 233.0818807590,
      59  => 246.9416506281,
      60  => 261.6255653006,
      61  => 277.1826309769,
      62  => 293.6647679174,
      63  => 311.1269837221,
      64  => 329.6275569129,
      65  => 349.2282314330,
      66  => 369.9944227116,
      67  => 391.9954359817,
      68  => 415.3046975799,
      69  => 440.0000000000,
      70  => 466.1637615181,
      71  => 493.8833012561,
      72  => 523.2511306012,
      73  => 554.3652619537,
      74  => 587.3295358348,
      75  => 622.2539674442,
      76  => 659.2551138257,
      77  => 698.4564628660,
      78  => 739.9888454233,
      79  => 783.9908719635,
      80  => 830.6093951599,
      81  => 880.0000000000,
      82  => 932.3275230362,
      83  => 987.7666025122,
      84  => 1046.5022612024,
      85  => 1108.7305239075,
      86  => 1174.6590716696,
      87  => 1244.5079348883,
      88  => 1318.5102276515,
      89  => 1396.9129257320,
      90  => 1479.9776908465,
      91  => 1567.9817439270,
      92  => 1661.2187903198,
      93  => 1760.0000000000,
      94  => 1864.6550460724,
      95  => 1975.5332050245,
      96  => 2093.0045224048,
      97  => 2217.4610478150,
      98  => 2349.3181433393,
      99  => 2489.0158697766,
      100 => 2637.0204553030,
      101 => 2793.8258514640,
      102 => 2959.9553816931,
      103 => 3135.9634878540,
      104 => 3322.4375806396,
      105 => 3520.0000000000,
      106 => 3729.3100921447,
      107 => 3951.0664100490,
      108 => 4186.0090448096,
      109 => 4434.9220956300,
      110 => 4698.6362866785,
      111 => 4978.0317395533,
      112 => 5274.0409106059,
      113 => 5587.6517029281,
      114 => 5919.9107633862,
      115 => 6271.9269757080,
      116 => 6644.8751612791,
      117 => 7040.0000000000,
      118 => 7458.6201842894,
      119 => 7902.1328200980,
      120 => 8372.0180896192,
      121 => 8869.8441912599,
      122 => 9397.2725733570,
      123 => 9956.0634791066,
      124 => 10548.0818212118,
      125 => 11175.3034058561,
      126 => 11839.8215267723,
      127 => 12543.8539514160);

end WNM;
