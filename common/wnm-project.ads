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

with WNM.MIDI;
with WNM.Sequence_Copy;
with WNM.Sample_Library;
with WNM.Speech;
with WNM.Chord_Settings;
with WNM.Synth;
with WNM.Time;

private with Enum_Next;

package WNM.Project is

   Editing_Step : Sequencer_Steps := Sequencer_Steps'First;
   Editing_Track : Tracks := Tracks'First;
   Editing_Pattern : Patterns := Patterns'First;
   Editing_Chord : Chords := Chords'First;

   procedure Do_Copy (T : in out WNM.Sequence_Copy.Copy_Transaction)
     with Pre => WNM.Sequence_Copy.Is_Complete (T);

   procedure Set_BPM (BPM : Beat_Per_Minute);
   procedure Change_BPM (BPM_Delta : Integer);
   function Get_BPM return Beat_Per_Minute;
   function Samples_Per_Beat return Synth.Sample_Time;
   function Microseconds_Per_Beat return Time.Time_Microseconds;

   ----------
   -- Step --
   ----------

   type Trigger_Kind is (None, Always, Fill, Not_Fill,
                         Percent_25, Percent_50, Percent_75,
                         One_Of_Two, One_Of_Three, One_Of_Four, One_Of_Five);

   function Img (T : Trigger_Kind) return String
   is (case T is
          when None         => "None",
          when Always       => "Always",
          when Fill         => "Fill",
          when Not_Fill     => "Not Fill",
          when Percent_25   => "25%",
          when Percent_50   => "50%",
          when Percent_75   => "75%",
          when One_Of_Two   => "1/2",
          when One_Of_Three => "1/3",
          when One_Of_Four  => "1/4",
          when One_Of_Five  => "1/5");

   type Note_Duration is (Double, Whole, Half, Quarter, N_8th,
                          N_16th, N_32nd);
   function Img (D : Note_Duration) return String
   is (case D is
          when Double  => "Double",
          when Whole   => "Whole",
          when Half    => "Half",
          when Quarter => "Quarter",
          when N_8th   => "8th",
          when N_16th  => "16th",
          when N_32nd  => "32nd");

   type Repeat_Cnt is mod 8;

   type Repeat_Rate_Kind is (Rate_1_2, Rate_1_3, Rate_1_4, Rate_1_5,
                             Rate_1_6, Rate_1_8, Rate_1_10, Rate_1_12,
                             Rate_1_16,
                             Rate_1_20, Rate_1_24, Rate_1_32);
   function Img (R : Repeat_Rate_Kind) return String
   is (case R is
          when Rate_1_2  => "1/2",
          when Rate_1_3  => "1/3",
          when Rate_1_4  => "1/4",
          when Rate_1_5  => "1/5",
          when Rate_1_6  => "1/6",
          when Rate_1_8  => "1/8",
          when Rate_1_10 => "1/10",
          when Rate_1_12 => "1/12",
          when Rate_1_16 => "1/16",
          when Rate_1_20 => "1/20",
          when Rate_1_24 => "1/24",
          when Rate_1_32 => "1/32");

   type Note_Mode_Kind is (Note, Chord,
                           Note_In_Chord,
                           Arp);
   package Note_Mode_Kind_Next is new Enum_Next (Note_Mode_Kind);
   use Note_Mode_Kind_Next;

   function Img (M : Note_Mode_Kind) return String
   is (case M is
          when Note          => "Note",
          when Chord         => "Chord",
          when Note_In_Chord => "Note in chord",
          when Arp           => "Arpeggiator");

   type CC_Id is (A, B, C, D);

   function CC_Letter (ID : CC_Id) return String
   is (case ID is
          when A => "A",
          when B => "B",
          when C => "C",
          when D => "D");

   -- Step getters --
   function Set (Step : Sequencer_Steps) return Boolean;

   function Set (Track : Tracks; Step : Sequencer_Steps) return Boolean;

   function Trigger (Step : Sequencer_Steps := Editing_Step)
                     return Trigger_Kind;
   function Repeat (Step : Sequencer_Steps := Editing_Step)
                    return Repeat_Cnt;
   function Repeat_Rate (Step : Sequencer_Steps := Editing_Step)
                         return Repeat_Rate_Kind;
   function Note_Mode (Step : Sequencer_Steps := Editing_Step)
                       return Note_Mode_Kind;
   function Note_Img (Step : Sequencer_Steps := Editing_Step)
                      return String;
   function Duration (Step : Sequencer_Steps := Editing_Step)
                      return Note_Duration;
   function Velocity (Step : Sequencer_Steps := Editing_Step)
                      return MIDI.MIDI_Data;
   function CC_Enabled (Step : Sequencer_Steps := Editing_Step;
                        Id : CC_Id) return Boolean;
   function CC_Value (Step : Sequencer_Steps := Editing_Step;
                      Id : CC_Id)
                      return MIDI.MIDI_Data;

   function CC_Image (Step : Sequencer_Steps := Editing_Step;
                      Id : CC_Id)
                      return String;

   procedure Note_Mode_Next (Step : Sequencer_Steps := Editing_Step);

   procedure CC_Toggle (Step : Sequencer_Steps; Id : CC_Id);

   type Step_Settings is (Condition,
                          Note,
                          Duration,
                          Velo,
                          Repeat,
                          Repeat_Rate,
                          CC_A,
                          CC_B,
                          CC_C,
                          CC_D,
                          Note_Mode,
                          Octave_Shift);

   for Step_Settings'Size use 8;
   for Step_Settings use (Condition    => 0,
                          Note         => 1,
                          Duration     => 2,
                          Velo         => 3,
                          Repeat       => 4,
                          Repeat_Rate  => 5,
                          CC_A         => 6,
                          CC_B         => 7,
                          CC_C         => 8,
                          CC_D         => 9,
                          Note_Mode    => 10,
                          Octave_Shift => 11);

   subtype User_Step_Settings is Step_Settings range Condition .. CC_D;

   procedure Next_Value (S : User_Step_Settings);
   procedure Prev_Value (S : User_Step_Settings);
   procedure Next_Value_Fast (S : User_Step_Settings);
   procedure Prev_Value_Fast (S : User_Step_Settings);

   -----------
   -- Track --
   -----------

   type Track_Mode_Kind is (Sample_Mode, MIDI_Mode, Speech_Mode,
                            Kick_Mode, Snare_Mode, Cymbal_Mode, Lead_Mode);
   package Track_Mode_Kind_Next is new Enum_Next (Track_Mode_Kind);
   use Track_Mode_Kind_Next;

   function Img (M : Track_Mode_Kind) return String
   is (case M is
          when Sample_Mode => "Sample",
          when MIDI_Mode   => "MIDI",
          when Speech_Mode => "Speech",
          when Kick_Mode   => "Kick",
          when Snare_Mode  => "Snare",
          when Cymbal_Mode => "Symbal",
          when Lead_Mode => "Lead");

   subtype Synth_Track_Mode_Kind is
     Track_Mode_Kind range Kick_Mode .. Lead_Mode;

   function Voice_MIDI_Chan (Voice : Synth_Track_Mode_Kind)
                             return MIDI.MIDI_Channel
   is (case Voice is
          when Kick_Mode   => 1,
          when Snare_Mode  => 2,
          when Cymbal_Mode => 3,
          when Lead_Mode   => 4);

   subtype Controller_Label is String (1 .. 17);
   Empty_Controller_Label : constant Controller_Label := (others => ' ');

   type Arp_Mode_Kind is (Up, Down, Up_Down, Pattern1, Pattern2, Pattern3,
                          Random);
   type Arp_Notes_Kind is (Chord);

   function Img (M : Arp_Mode_Kind) return String
   is (case M is
          when Up => "Up",
          when Down => "Down",
          when Up_Down => "Up and Down",
          when Pattern1 => "Pattern 1",
          when Pattern2 => "Pattern 2",
          when Pattern3 => "Pattern 3",
          when Random => "Random");

   function Img (N : Arp_Notes_Kind) return String
   is (case N is
          when Chord => "Notes of chord");

   -- Track Getters --
   function Mode (T : Tracks := Editing_Track) return Track_Mode_Kind;
   function MIDI_Chan (T : Tracks := Editing_Track) return MIDI.MIDI_Channel;
   function Track_Volume (T : Tracks := Editing_Track) return Audio_Volume;
   function Track_Pan (T : Tracks := Editing_Track) return Audio_Pan;
   function CC_Default (T : Tracks := Editing_Track;
                        Id : CC_Id)
                        return MIDI.MIDI_Data;

   function CC_Value_To_Use (P : Patterns; T : Tracks; S : Sequencer_Steps;
                             Id : CC_Id)
                             return MIDI.MIDI_Data;
   --  Return the value for the given step if any, otherwise the default value

   function CC_Controller (T : Tracks := Editing_Track;
                           Id : CC_Id)
                           return MIDI.MIDI_Data;
   function CC_Controller_Label (T    : Tracks := Editing_Track;
                                 Id   : CC_Id)
                                 return Controller_Label;
   function Selected_Sample (T : Tracks := Editing_Track)
                             return Sample_Library.Valid_Sample_Index;
   function Selected_Word (T : Tracks := Editing_Track) return Speech.Word;
   function Selected_Engine (T : Tracks := Editing_Track)
                             return MIDI.MIDI_Data;
   function Selected_Engine_Img (T : Tracks := Editing_Track)
                                 return String;
   function Arp_Mode (T : Tracks := Editing_Track) return Arp_Mode_Kind;
   function Arp_Notes (T : Tracks := Editing_Track) return Arp_Notes_Kind;
   function Notes_Per_Chord (T : Tracks := Editing_Track)
                             return Natural;

   type Track_Settings is (Track_Mode,
                           Sample,
                           Speech_Word,
                           Engine,
                           CC_Default_A,
                           CC_Default_B,
                           CC_Default_C,
                           CC_Default_D,
                           Volume,
                           Pan,
                           Arp_Mode,
                           Arp_Notes,
                           Notes_Per_Chord,
                           MIDI_Chan,
                           MIDI_Instrument,
                           CC_Ctrl_A, CC_Label_A,
                           CC_Ctrl_B, CC_Label_B,
                           CC_Ctrl_C, CC_Label_C,
                           CC_Ctrl_D, CC_Label_D);

   for Track_Settings'Size use 8;
   for Track_Settings use (Track_Mode      => 0,
                           Sample          => 1,
                           Speech_Word     => 2,
                           Engine          => 3,
                           CC_Default_A    => 4,
                           CC_Default_B    => 5,
                           CC_Default_C    => 6,
                           CC_Default_D    => 7,
                           Volume          => 8,
                           Pan             => 9,
                           Arp_Mode        => 10,
                           Arp_Notes       => 11,
                           Notes_Per_Chord => 12,
                           MIDI_Chan       => 13,
                           MIDI_Instrument => 14,
                           CC_Ctrl_A       => 15,
                           CC_Label_A      => 16,
                           CC_Ctrl_B       => 17,
                           CC_Label_B      => 18,
                           CC_Ctrl_C       => 19,
                           CC_Label_C      => 20,
                           CC_Ctrl_D       => 21,
                           CC_Label_D      => 22);

   subtype User_Track_Settings
     is Track_Settings range Track_Mode .. CC_Label_D;

   procedure Next_Value (S : User_Track_Settings);
   procedure Prev_Value (S : User_Track_Settings);
   procedure Next_Value_Fast (S : User_Track_Settings);
   procedure Prev_Value_Fast (S : User_Track_Settings);

   procedure Set_CC_Controller (T : Tracks; Id : CC_Id; C : MIDI.MIDI_Data);

   procedure Set_CC_Controller_Label (T    : Tracks;
                                      Id   : CC_Id;
                                      Label : Controller_Label);

   -----------
   -- Chord --
   -----------

   type Chord_Setting_Kind is (Tonic,
                               Name);

   for Chord_Setting_Kind'Size use 8;
   for Chord_Setting_Kind use (Tonic => 0,
                               Name  => 1);

   subtype User_Chord_Settings is Chord_Setting_Kind range Tonic .. Name;

   -- Chord Getters --

   function Selected_Tonic (C : WNM.Chords := Editing_Chord)
                            return MIDI.MIDI_Key;
   function Selected_Name (C : WNM.Chords :=  Editing_Chord)
                           return WNM.Chord_Settings.Chord_Name;

   procedure Next_Value (S : User_Chord_Settings);
   procedure Prev_Value (S : User_Chord_Settings);

   procedure Next_Value_Fast (S : User_Chord_Settings);
   procedure Prev_Value_Fast (S : User_Chord_Settings);

   procedure Randomly_Pick_A_Progression;
   --  a.k.a. The Magic Hat of Chord Progression

private

   type Global_Settings is (BPM);
   for Global_Settings use (BPM => 0);

   package Trigger_Kind_Next is new Enum_Next (Trigger_Kind);
   use Trigger_Kind_Next;

   package Repeat_Rate_Kind_Next is new Enum_Next (Repeat_Rate_Kind);
   use Repeat_Rate_Kind_Next;

   package Repeat_Cnt_Next is new Enum_Next (Repeat_Cnt);
   use Repeat_Cnt_Next;

   package Note_Duration_Next is new Enum_Next (Note_Duration);
   use Note_Duration_Next;

   package MIDI_Data_Next is new Enum_Next (T    => MIDI.MIDI_Data,
                                            Wrap => False);
   use MIDI_Data_Next;

   package MIDI_Chan_Next is new Enum_Next (T    => MIDI.MIDI_Channel,
                                            Wrap => False);
   use MIDI_Chan_Next;

   package Audio_Volume_Next is new Enum_Next (T    => Audio_Volume,
                                               Wrap => False);
   use Audio_Volume_Next;

   package Audio_Pan_Next is new Enum_Next (T    => Audio_Pan,
                                            Wrap => False);
   use Audio_Pan_Next;

   package Valid_Sample_Index_Next
   is new Enum_Next (Sample_Library.Valid_Sample_Index);
   use Valid_Sample_Index_Next;

   package Speech_Word_Next
   is new Enum_Next (Speech.Word);
   use Speech_Word_Next;

   package Arp_Mode_Next is new Enum_Next (Arp_Mode_Kind);
   use Arp_Mode_Next;

   package Arp_Notes_Next is new Enum_Next (Arp_Notes_Kind);
   use Arp_Notes_Next;

   package Chord_Name_Next is new Enum_Next (WNM.Chord_Settings.Chord_Name);
   use Chord_Name_Next;

   package Notes_Per_Chord_Next
   is new Enum_Next (Chord_Settings.Chord_Index_Range, Wrap => False);
   use Notes_Per_Chord_Next;

   type CC_Val_Array is array (CC_Id) of MIDI.MIDI_Data;
   type CC_Ena_Array is array (CC_Id) of Boolean;

   type Octave_Offset is range -8 .. 8;
   for Octave_Offset'Size use 5;

   type Step_Rec is record
      Trig        : Trigger_Kind;
      Repeat      : Repeat_Cnt;
      Repeat_Rate : Repeat_Rate_Kind;

      Note_Mode : Note_Mode_Kind;
      Note      : MIDI.MIDI_Key;
      Oct       : Octave_Offset;
      Duration  : Note_Duration;
      Velo      : MIDI.MIDI_Data;
      CC_Ena    : CC_Ena_Array;
      CC_Val    : CC_Val_Array;
   end record;

   Default_Step : constant Step_Rec :=
     (Trig => None,
      Repeat => 0,
      Repeat_Rate => Rate_1_8,
      Note_Mode => Note,
      Note => MIDI.C4,
      Oct => 0,
      Duration => Quarter,
      Velo => MIDI.MIDI_Data'Last,
      CC_Ena => (others => False),
      CC_Val => (others => 0));

   type Sequence is array (Sequencer_Steps) of Step_Rec with Pack;
   type Pattern is array (Tracks) of Sequence;
   type All_Patterns is array (Patterns) of Pattern;

   type CC_Setting is record
      Controller : MIDI.MIDI_Data := 0;
      Value      : MIDI.MIDI_Data := 63; --  MIDI_Data'Last / 2
      Label      : Controller_Label := "Noname Controller";
   end record;
   type CC_Setting_Array is array (CC_Id) of CC_Setting;

   type Track_Rec is record
      Mode : Track_Mode_Kind := Sample_Mode;
      Chan : MIDI.MIDI_Channel := 0;
      Volume : Audio_Volume := Init_Volume;
      Pan : Audio_Pan := Init_Pan;
      CC : CC_Setting_Array;
      Sample : Sample_Library.Valid_Sample_Index := 1;
      Word   : Speech.Word := Speech.Word'First;
      Engine : MIDI.MIDI_Data := 0;
      Arp_Mode : Arp_Mode_Kind := Arp_Mode_Kind'First;
      Arp_Notes : Arp_Notes_Kind := Arp_Notes_Kind'First;
      Notes_Per_Chord : Chord_Settings.Chord_Index_Range :=
        Chord_Settings.Chord_Index_Range'Last;
   end record;

   type Track_Arr is array (Tracks) of Track_Rec;

   Default_Track : constant Track_Rec :=
     (Mode => MIDI_Mode,
      Chan => 0,
      Volume => Init_Volume,
      Pan => Init_Pan,
      CC => ((0, 63, "Control 0        "),
             (1, 63, "Control 1        "),
             (2, 63, "Control 2        "),
             (3, 63, "Control 3        ")
            ),
      Sample => Sample_Library.Valid_Sample_Index'First,
      Word => Speech.Word'First,
      Engine => 0,
      Arp_Mode => Arp_Mode_Kind'First,
      Arp_Notes => Arp_Notes_Kind'First,
      Notes_Per_Chord => Chord_Settings.Chord_Index_Range'Last
     );

   type Chord_Rec is record
      Tonic : MIDI.MIDI_Key := MIDI.C4;
      Name  : WNM.Chord_Settings.Chord_Name :=
        WNM.Chord_Settings.Chord_Name'First;
   end record;

   type Chord_Arr is array (WNM.Chords) of Chord_Rec;

   Default_Chord : constant Chord_Rec :=
     (Tonic => MIDI.C4,
      Name => WNM.Chord_Settings.Chord_Name'First);

   type Project_Rec is record
      BPM : Beat_Per_Minute := 120;
      Seqs : All_Patterns := (others => (others => (others => Default_Step)));
      Tracks : Track_Arr := (others => Default_Track);
      Chords : Chord_Arr := (others => Default_Chord);
   end record;

   G_Project : Project_Rec := (others => <>);

   procedure Synchronize_Voice_Settings (T : Tracks);
   --  Send all the synth voice settings to the coprocessor to update the

   procedure Synchronize_Voice_Engine (T : Tracks);
   --  Send a message to coprocessor with selector engine for the track

   procedure Synchronize_Coproc_Vol_Pan (T : Tracks);
   --  Send a message to coprocessor with Volume and pan value for the track

end WNM.Project;
