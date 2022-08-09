with HAL; use HAL;

package WNM.MIDI is

   type Command_Kind is (Note_Off,
                         Note_On,
                         Aftertouch,
                         Continous_Controller,
                         Patch_Change,
                         Channel_Pressure,
                         Pitch_Bend)
     with Size => 4;

   for Command_Kind use (Note_Off             => 16#8#,
                         Note_On              => 16#9#,
                         Aftertouch           => 16#A#,
                         Continous_Controller => 16#B#,
                         Patch_Change         => 16#C#,
                         Channel_Pressure     => 16#D#,
                         Pitch_Bend           => 16#E#);

   subtype MIDI_Data is UInt8 range 2#0000_0000# .. 2#0111_1111#;
   subtype MIDI_Key is MIDI_Data;
   type MIDI_Channel is mod 2**4 with Size => 4;

   type Message (Kind : Command_Kind := Note_On) is record
      Chan : MIDI_Channel;
      case Kind is
         when Note_On | Note_Off | Aftertouch =>
            Key      : MIDI_Key;
            Velocity : MIDI_Data;
         when Continous_Controller =>
            Controller       : MIDI_Data;
            Controller_Value : MIDI_Data;
         when Patch_Change =>
            Instrument : MIDI_Data;
         when Channel_Pressure =>
            Pressure : MIDI_Data;
         when Pitch_Bend =>
            Bend : MIDI_Data;
      end case;
   end record
     with Size => 3 * 8;

   for Message use record
      Kind             at 0 range 4 .. 7;
      Chan             at 0 range 0 .. 3;
      Key              at 1 range 0 .. 7;
      Velocity         at 2 range 0 .. 7;
      Controller       at 1 range 0 .. 7;
      Controller_Value at 2 range 0 .. 7;
      Instrument       at 1 range 0 .. 7;
      Pressure         at 1 range 0 .. 7;
      Bend             at 1 range 0 .. 7;
   end record;

   type Octaves is new UInt8 range 1 .. 8;
   type Notes is (C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B);

   subtype MIDI_USB_Event is HAL.UInt32;

   function Make_Note_On (C : MIDI_Channel;
                          K : MIDI_Key;
                          V : MIDI_Data)
                          return MIDI_USB_Event;

   function Make_Note_On (C : MIDI_Channel;
                          N : Notes;
                          O : Octaves;
                          V : MIDI_Data)
                          return MIDI_USB_Event;

   function Make_Note_Off (C : MIDI_Channel;
                           K : MIDI_Key;
                           V : MIDI_Data)
                          return MIDI_USB_Event;

   function Make_Note_Off (C : MIDI_Channel;
                           N : Notes;
                           O : Octaves;
                           V : MIDI_Data)
                           return MIDI_USB_Event;

   function Make_CC (C          : MIDI_Channel;
                     Controller : MIDI_Data;
                     Value      : MIDI_Data)
                     return MIDI_USB_Event;

   --------------
   -- To_Track --
   --------------

   function To_Track (Chan : MIDI_Channel) return Tracks
   is (Tracks (Integer (Chan) + 1));

   --------------------
   -- To_MIDI_Channel --
   ---------------------

   function To_MIDI_Channel (Chan : Tracks) return MIDI_Channel
   is (MIDI_Channel (Integer (Chan) - 1));

   function Key (Oct : Octaves; N : Notes) return MIDI_Key
   is (MIDI_Key (12 + Natural (Oct) * 12 +
                   Notes'Pos (N) - Notes'Pos (Notes'First)));

   A0  : constant MIDI_Key := 16#15#;
   As0 : constant MIDI_Key := 16#16#;
   B0  : constant MIDI_Key := 16#17#;
   C1  : constant MIDI_Key := 16#18#;
   Cs1 : constant MIDI_Key := 16#19#;
   D1  : constant MIDI_Key := 16#1a#;
   Ds1 : constant MIDI_Key := 16#1b#;
   E1  : constant MIDI_Key := 16#1c#;
   F1  : constant MIDI_Key := 16#1d#;
   Fs1 : constant MIDI_Key := 16#1e#;
   G1  : constant MIDI_Key := 16#1f#;
   Gs1 : constant MIDI_Key := 16#20#;
   A1  : constant MIDI_Key := 16#21#;
   As1 : constant MIDI_Key := 16#22#;
   B1  : constant MIDI_Key := 16#23#;
   C2  : constant MIDI_Key := 16#24#;
   Cs2 : constant MIDI_Key := 16#25#;
   D2  : constant MIDI_Key := 16#26#;
   Ds2 : constant MIDI_Key := 16#27#;
   E2  : constant MIDI_Key := 16#28#;
   F2  : constant MIDI_Key := 16#29#;
   Fs2 : constant MIDI_Key := 16#2a#;
   G2  : constant MIDI_Key := 16#2b#;
   Gs2 : constant MIDI_Key := 16#2c#;
   A2  : constant MIDI_Key := 16#2d#;
   As2 : constant MIDI_Key := 16#2e#;
   B2  : constant MIDI_Key := 16#2f#;
   C3  : constant MIDI_Key := 16#30#;
   Cs3 : constant MIDI_Key := 16#31#;
   D3  : constant MIDI_Key := 16#32#;
   Ds3 : constant MIDI_Key := 16#33#;
   E3  : constant MIDI_Key := 16#34#;
   F3  : constant MIDI_Key := 16#35#;
   Fs3 : constant MIDI_Key := 16#36#;
   G3  : constant MIDI_Key := 16#37#;
   Gs3 : constant MIDI_Key := 16#38#;
   A3  : constant MIDI_Key := 16#39#;
   As3 : constant MIDI_Key := 16#3a#;
   B3  : constant MIDI_Key := 16#3b#;
   C4  : constant MIDI_Key := 16#3c#;
   Cs4 : constant MIDI_Key := 16#3d#;
   D4  : constant MIDI_Key := 16#3e#;
   Ds4 : constant MIDI_Key := 16#3f#;
   E4  : constant MIDI_Key := 16#40#;
   F4  : constant MIDI_Key := 16#41#;
   Fs4 : constant MIDI_Key := 16#42#;
   G4  : constant MIDI_Key := 16#43#;
   Gs4 : constant MIDI_Key := 16#44#;
   A4  : constant MIDI_Key := 16#45#;
   As4 : constant MIDI_Key := 16#46#;
   B4  : constant MIDI_Key := 16#47#;
   C5  : constant MIDI_Key := 16#48#;
   Cs5 : constant MIDI_Key := 16#49#;
   D5  : constant MIDI_Key := 16#4a#;
   Ds5 : constant MIDI_Key := 16#4b#;
   E5  : constant MIDI_Key := 16#4c#;
   F5  : constant MIDI_Key := 16#4d#;
   Fs5 : constant MIDI_Key := 16#4e#;
   G5  : constant MIDI_Key := 16#4f#;
   Gs5 : constant MIDI_Key := 16#50#;
   A5  : constant MIDI_Key := 16#51#;
   As5 : constant MIDI_Key := 16#52#;
   B5  : constant MIDI_Key := 16#53#;
   C6  : constant MIDI_Key := 16#54#;
   Cs6 : constant MIDI_Key := 16#55#;
   D6  : constant MIDI_Key := 16#56#;
   Ds6 : constant MIDI_Key := 16#57#;
   E6  : constant MIDI_Key := 16#58#;
   F6  : constant MIDI_Key := 16#59#;
   Fs6 : constant MIDI_Key := 16#5a#;
   G6  : constant MIDI_Key := 16#5b#;
   Gs6 : constant MIDI_Key := 16#5c#;
   A6  : constant MIDI_Key := 16#5d#;
   As6 : constant MIDI_Key := 16#5e#;
   B6  : constant MIDI_Key := 16#5f#;
   C7  : constant MIDI_Key := 16#60#;
   Cs7 : constant MIDI_Key := 16#61#;
   D7  : constant MIDI_Key := 16#62#;
   Ds7 : constant MIDI_Key := 16#63#;
   E7  : constant MIDI_Key := 16#64#;
   F7  : constant MIDI_Key := 16#65#;
   Fs7 : constant MIDI_Key := 16#66#;
   G7  : constant MIDI_Key := 16#67#;
   Gs7 : constant MIDI_Key := 16#68#;
   A7  : constant MIDI_Key := 16#69#;
   As7 : constant MIDI_Key := 16#6a#;
   B7  : constant MIDI_Key := 16#6b#;
   C8  : constant MIDI_Key := 16#6c#;
   Cs8 : constant MIDI_Key := 16#6d#;
   D8  : constant MIDI_Key := 16#6e#;
   Ds8 : constant MIDI_Key := 16#6f#;
   E8  : constant MIDI_Key := 16#70#;
   F8  : constant MIDI_Key := 16#71#;
   Fs8 : constant MIDI_Key := 16#72#;
   G8  : constant MIDI_Key := 16#73#;
   Gs8 : constant MIDI_Key := 16#74#;
   A8  : constant MIDI_Key := 16#75#;
   As8 : constant MIDI_Key := 16#76#;
   B8  : constant MIDI_Key := 16#77#;


   subtype Key_Img_Str is String (1 .. 3);
   function Key_Img (V : MIDI_Key) return Key_Img_Str
   is (case V is
          when A0  => "A0 ",
          when As0 => "A#0",
          when B0  => "B0 ",
          when C1  => "C1 ",
          when Cs1 => "C#1",
          when D1  => "D1 ",
          when Ds1 => "D#1",
          when E1  => "E1 ",
          when F1  => "F1 ",
          when Fs1 => "F#1",
          when G1  => "G1 ",
          when Gs1 => "G#1",
          when A1  => "A1 ",
          when As1 => "A#1",
          when B1  => "B1 ",
          when C2  => "C2 ",
          when Cs2 => "C#2",
          when D2  => "D2 ",
          when Ds2 => "D#2",
          when E2  => "E2 ",
          when F2  => "F2 ",
          when Fs2 => "F#2",
          when G2  => "G2 ",
          when Gs2 => "G#2",
          when A2  => "A2 ",
          when As2 => "A#2",
          when B2  => "B2 ",
          when C3  => "C3 ",
          when Cs3 => "C#3",
          when D3  => "D3 ",
          when Ds3 => "D#3",
          when E3  => "E3 ",
          when F3  => "F3 ",
          when Fs3 => "F#3",
          when G3  => "G3 ",
          when Gs3 => "G#3",
          when A3  => "A3 ",
          when As3 => "A#3",
          when B3  => "B3 ",
          when C4  => "C4 ",
          when Cs4 => "C#4",
          when D4  => "D4 ",
          when Ds4 => "D#4",
          when E4  => "E4 ",
          when F4  => "F4 ",
          when Fs4 => "F#4",
          when G4  => "G4 ",
          when Gs4 => "G#4",
          when A4  => "A4 ",
          when As4 => "A#4",
          when B4  => "B4 ",
          when C5  => "C5 ",
          when Cs5 => "C#5",
          when D5  => "D5 ",
          when Ds5 => "D#5",
          when E5  => "E5 ",
          when F5  => "F5 ",
          when Fs5 => "F#5",
          when G5  => "G5 ",
          when Gs5 => "G#5",
          when A5  => "A5 ",
          when As5 => "A#5",
          when B5  => "B5 ",
          when C6  => "C6 ",
          when Cs6 => "C#6",
          when D6  => "D6 ",
          when Ds6 => "D#6",
          when E6  => "E6 ",
          when F6  => "F6 ",
          when Fs6 => "F#6",
          when G6  => "G6 ",
          when Gs6 => "G#6",
          when A6  => "A6 ",
          when As6 => "A#6",
          when B6  => "B6 ",
          when C7  => "C7 ",
          when Cs7 => "C#7",
          when D7  => "D7 ",
          when Ds7 => "D#7",
          when E7  => "E7 ",
          when F7  => "F7 ",
          when Fs7 => "F#7",
          when G7  => "G7 ",
          when Gs7 => "G#7",
          when A7  => "A7 ",
          when As7 => "A#7",
          when B7  => "B7 ",
          when C8  => "C8 ",
          when Cs8 => "C#8",
          when D8  => "D8 ",
          when Ds8 => "D#8",
          when E8  => "E8 ",
          when F8  => "F8 ",
          when Fs8 => "F#8",
          when G8  => "G8 ",
          when Gs8 => "G#8",
          when A8  => "A8 ",
          when As8 => "A#8",
          when B8  => "B8 ",
          when others => "---");

   function Img (M : Message) return String
   is (case M.Kind is
          when Note_On => "On " & Key_Img (M.Key) & " " & M.Velocity'Img,
          when Note_Off => "Off " & Key_Img (M.Key) & " " & M.Velocity'Img,
          when Aftertouch => "Aftertouch " & Key_Img (M.Key) & " " & M.Velocity'Img,
          when Continous_Controller => "CC " & M.Controller'Img & M.Controller_Value'Img,
          when Patch_Change => "Patch change" & M.Instrument'Img,
          when Channel_Pressure => "Pressure" & M.Pressure'Img,
          when Pitch_Bend => "Pitch Bend" & M.Bend'Img
      );

   Key_To_Frequency : constant array (MIDI_Key) of Float :=
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
      127 =>  12543.8539514160);

end WNM.MIDI;
