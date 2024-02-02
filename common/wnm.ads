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
   subtype Song_Element is Keyboard_Value;
   subtype Parts is Song_Element range 1 .. 8;
   subtype Chord_Progressions is Song_Element range 9 .. 16;

   -- Mixer --
   type FX_Kind is (Bypass, Overdrive, Reverb, Filter, Bitcrusher);

   function Img (M : FX_Kind) return String
   is (case M is
          when Bypass     => "Bypass",
          when Overdrive  => "Overdrive",
          when Bitcrusher => "Bitcrusher",
          when Reverb     => "Reverb",
          when Filter     => "Filter");

   type Mixer_Buffer_Index is range 1 .. Audio.Mixer_Buffer_Count
     with Size => 8;

   subtype Beat_Per_Minute is Positive range 50 .. 200;
   subtype Sequencer_Steps is Keyboard_Value;

   Steps_Per_Beat      : constant := 4;
   Max_Events_Per_Step : constant := 6;

   Long_Press_Time_Span_Microseconds : constant := 300 * 1_000;
   --  How much time (in microseconds) users have to press a button to get the
   --  alternative function.

   Repeat_Press_Time_Span_Microseconds : constant := 30 * 1_000;
   --  How much time (in microseconds) a button press event will be repeated
   --  when the button is held down.

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

end WNM;
