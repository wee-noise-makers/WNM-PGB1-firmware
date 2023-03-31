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

with MIDI;
with WNM.Sequence_Copy;
with WNM.Chord_Settings;
with WNM.Synth;
with WNM.Time;

with Tresses;

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

   type Track_Mode_Kind is (MIDI_Mode, Sample1_Mode, Sample2_Mode,
                            Speech_Mode, Kick_Mode, Snare_Mode, Cymbal_Mode,
                            Bass_Mode, Lead_Mode);

   function Img (M : Track_Mode_Kind) return String
   is (case M is
          when Sample1_Mode => "Sample 1",
          when Sample2_Mode => "Sample 2",
          when MIDI_Mode   => "MIDI",
          when Speech_Mode => "Speech",
          when Kick_Mode   => "Kick",
          when Snare_Mode  => "Snare",
          when Cymbal_Mode => "Cymbal",
          when Lead_Mode   => "Lead",
          when Bass_Mode   => "Bass");

   subtype Synth_Track_Mode_Kind is
     Track_Mode_Kind range Sample1_Mode .. Lead_Mode;

   function Voice_MIDI_Chan (Voice : Synth_Track_Mode_Kind)
                             return MIDI.MIDI_Channel
   is (case Voice is
          when Sample1_Mode => Synth.Sample1_Channel,
          when Sample2_Mode => Synth.Sample2_Channel,
          when Speech_Mode => Synth.Speech_Channel,
          when Kick_Mode   => Synth.Kick_Channel,
          when Snare_Mode  => Synth.Snare_Channel,
          when Cymbal_Mode => Synth.Cymbal_Channel,
          when Bass_Mode   => Synth.Bass_Channel,
          when Lead_Mode   => Synth.Lead_Channel);

   FX_MIDI_Chan : constant MIDI.MIDI_Channel := Synth.FX_Settings_Channel;

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

   type Master_FX_Kind is (Bypass, Overdrive, Delayline, Filter);

   function Img (M : Master_FX_Kind) return String
   is (case M is
          when Bypass    => "Bypass",
          when Overdrive => "Overdrive",
          when Delayline => "Delay",
          when Filter    => "Filter");

   -- Track Getters --
   function Mode (T : Tracks := Editing_Track) return Track_Mode_Kind;
   function MIDI_Chan (T : Tracks := Editing_Track) return MIDI.MIDI_Channel;
   function Track_Name (T : Tracks := Editing_Track) return String;
   function Track_Volume (T : Tracks := Editing_Track) return Audio_Volume;
   function Track_Pan (T : Tracks := Editing_Track) return Audio_Pan;
   function CC_Default (T : Tracks := Editing_Track;
                        Id : CC_Id)
                        return MIDI.MIDI_Data;
   function Master_FX (T : Tracks := Editing_Track) return Master_FX_Kind;

   function LFO_Rate (T : Tracks := Editing_Track) return MIDI.MIDI_Data;
   function LFO_Amp (T : Tracks := Editing_Track) return MIDI.MIDI_Data;

   type LFO_Target_Kind is (P1, P2, P3, P4, Vol, Pan, None);
   for LFO_Target_Kind use (P1 => Synth.Voice_Param_1_CC,
                            P2 => Synth.Voice_Param_2_CC,
                            P3 => Synth.Voice_Param_3_CC,
                            P4 => Synth.Voice_Param_4_CC,
                            Vol => Synth.Voice_Volume_CC,
                            Pan => Synth.Voice_Pan_CC,
                            None => MIDI.MIDI_Data'Last);
   function LFO_Target (T : Tracks := Editing_Track) return LFO_Target_Kind;

   type LFO_Shape_Kind is (Sine, Triangle, Ramp_Up, Ramp_Down,
                           Exp_Up, Exp_Down);
   for LFO_Shape_Kind use (Sine      => 0,
                           Triangle  => 1,
                           Ramp_Up   => 2,
                           Ramp_Down => 3,
                           Exp_Up    => 4,
                           Exp_Down  => 5);
   function LFO_Shape (T : Tracks := Editing_Track) return LFO_Shape_Kind;

   type LFO_Sync_Kind is (Off, On);
   for LFO_Sync_Kind use (Off => 0, On => 1);
   function LFO_Sync (T : Tracks := Editing_Track) return LFO_Sync_Kind;

   type LFO_Loop_Kind is (Off, On);
   for LFO_Loop_Kind use (Off => 0, On => 1);
   function LFO_Loop (T : Tracks := Editing_Track) return LFO_Loop_Kind;

   type LFO_Amp_Kind is (Positive, Center, Negative);
   for LFO_Amp_Kind use (Positive => 0,
                         Center   => 1,
                         Negative => 2);
   function LFO_Amp_Mode (T : Tracks := Editing_Track) return LFO_Amp_Kind;

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
   function CC_Controller_Short_Label (T    : Tracks := Editing_Track;
                                       Id   : CC_Id)
                                       return Tresses.Short_Label;
   function Selected_Engine (T : Tracks := Editing_Track)
                             return MIDI.MIDI_Data;
   function Selected_Engine_Img (T : Tracks := Editing_Track)
                                 return String;
   function Arp_Mode (T : Tracks := Editing_Track) return Arp_Mode_Kind;
   function Arp_Notes (T : Tracks := Editing_Track) return Arp_Notes_Kind;
   function Notes_Per_Chord (T : Tracks := Editing_Track)
                             return Natural;

   type Track_Settings is (Track_Mode,
                           Engine,
                           CC_Default_A,
                           CC_Default_B,
                           CC_Default_C,
                           CC_Default_D,
                           LFO_Rate,
                           LFO_Amplitude,
                           LFO_Shape,
                           LFO_Target,
                           Volume,
                           Pan,
                           Master_FX,
                           Arp_Mode,
                           Arp_Notes,
                           Notes_Per_Chord,
                           MIDI_Chan,
                           MIDI_Instrument,
                           CC_Ctrl_A, CC_Label_A,
                           CC_Ctrl_B, CC_Label_B,
                           CC_Ctrl_C, CC_Label_C,
                           CC_Ctrl_D, CC_Label_D,

                           LFO_Amp_Mode,
                           LFO_Loop,
                           LFO_Sync);

   for Track_Settings'Size use 8;
   for Track_Settings use (Track_Mode      => 0,
                           Engine          => 1,
                           CC_Default_A    => 2,
                           CC_Default_B    => 3,
                           CC_Default_C    => 4,
                           CC_Default_D    => 5,
                           LFO_Rate        => 6,
                           LFO_Amplitude   => 7,
                           LFO_Shape       => 8,
                           LFO_Target      => 9,
                           Volume          => 10,
                           Pan             => 11,
                           Master_FX       => 12,
                           Arp_Mode        => 13,
                           Arp_Notes       => 14,
                           Notes_Per_Chord => 15,
                           MIDI_Chan       => 16,
                           MIDI_Instrument => 17,
                           CC_Ctrl_A       => 18,
                           CC_Label_A      => 19,
                           CC_Ctrl_B       => 20,
                           CC_Label_B      => 21,
                           CC_Ctrl_C       => 22,
                           CC_Label_C      => 23,
                           CC_Ctrl_D       => 24,
                           CC_Label_D      => 25,

                           LFO_Amp_Mode    => 26,
                           LFO_Loop        => 27,
                           LFO_Sync        => 28);

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

   --------
   -- FX --
   --------

   type FX_Setting_Kind is (Drive_Amount,
                            Delay_Time,
                            Delay_Feedback,
                            Filter_Mode,
                            Filter_Cutoff,
                            Filter_Reso);

   for FX_Setting_Kind use (Drive_Amount   => 0,
                            Delay_Time     => 1,
                            Delay_Feedback => 2,
                            Filter_Mode    => 3,
                            Filter_Cutoff  => 4,
                            Filter_Reso    => 5);

   subtype User_FX_Settings is FX_Setting_Kind;

   -- FX Getters --

   function Drive_Amount_Value return MIDI.MIDI_Data;
   function Delay_Time_Value return MIDI.MIDI_Data;
   function Delay_Feedback_Value return MIDI.MIDI_Data;
   function Filter_Cutoff_Value return MIDI.MIDI_Data;
   function Filter_Reso_Value return MIDI.MIDI_Data;

   type Filter_Mode_Kind is (Low_Pass, Band_Pass, High_Pass);
   function Filter_Mode_Value return Filter_Mode_Kind;

   procedure Next_Value (S : User_FX_Settings);
   procedure Prev_Value (S : User_FX_Settings);

   procedure Next_Value_Fast (S : User_FX_Settings);
   procedure Prev_Value_Fast (S : User_FX_Settings);

private

   type Global_Settings is (BPM);
   for Global_Settings use (BPM => 0);

   package Boolean_Next is new Enum_Next (Boolean);
   use Boolean_Next;

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

   package Master_FX_Next is new Enum_Next (T    => Master_FX_Kind,
                                            Wrap => False);
   use Master_FX_Next;

   package Arp_Mode_Next is new Enum_Next (Arp_Mode_Kind);
   use Arp_Mode_Next;

   package Arp_Notes_Next is new Enum_Next (Arp_Notes_Kind);
   use Arp_Notes_Next;

   package Chord_Name_Next is new Enum_Next (WNM.Chord_Settings.Chord_Name);
   use Chord_Name_Next;

   package Notes_Per_Chord_Next
   is new Enum_Next (Chord_Settings.Chord_Index_Range, Wrap => False);
   use Notes_Per_Chord_Next;

   package Filter_Mode_Next is new Enum_Next (T    => Filter_Mode_Kind,
                                              Wrap => False);
   use Filter_Mode_Next;

   package LFO_Shape_Next is new Enum_Next (T    => LFO_Shape_Kind,
                                            Wrap => True);
   use LFO_Shape_Next;

   package LFO_Target_Next is new Enum_Next (T    => LFO_Target_Kind,
                                             Wrap => True);
   use LFO_Target_Next;

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

   Default_Step_Lead : constant Step_Rec :=
     (Trig => None,
      Repeat => 0,
      Repeat_Rate => Rate_1_8,
      Note_Mode => Arp,
      Note => 0,
      Oct => 0,
      Duration => Quarter,
      Velo => MIDI.MIDI_Data'Last,
      CC_Ena => (others => False),
      CC_Val => (others => 0));

   Default_Step_Bass : constant Step_Rec :=
     (Trig => None,
      Repeat => 0,
      Repeat_Rate => Rate_1_8,
      Note_Mode => Note_In_Chord,
      Note => 0,
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
      MIDI_Enabled : Boolean := False;
      Chan : MIDI.MIDI_Channel := 0;
      Volume : Audio_Volume := Init_Volume;
      Pan : Audio_Pan := Init_Pan;
      FX_Kind : Master_FX_Kind := Bypass;
      LFO_Rate : MIDI.MIDI_Data := 0;
      LFO_Amp : MIDI.MIDI_Data := 0;
      LFO_Shape : LFO_Shape_Kind := LFO_Shape_Kind'First;
      LFO_Target : LFO_Target_Kind := LFO_Target_Kind'Last;
      LFO_Amp_Mode : LFO_Amp_Kind := LFO_Amp_Kind'First;
      LFO_Sync : LFO_Sync_Kind := Off;
      LFO_Loop : LFO_Loop_Kind := On;
      CC : CC_Setting_Array;
      Engine : MIDI.MIDI_Data := 0;
      Arp_Mode : Arp_Mode_Kind := Arp_Mode_Kind'First;
      Arp_Notes : Arp_Notes_Kind := Arp_Notes_Kind'First;
      Notes_Per_Chord : Chord_Settings.Chord_Index_Range :=
        Chord_Settings.Chord_Index_Range'Last;
   end record;

   Kick_Track    : constant Tracks := 1;
   Snare_Track   : constant Tracks := 2;
   Cymbal_Track  : constant Tracks := 3;
   Bass_Track    : constant Tracks := 4;
   Lead_Track    : constant Tracks := 5;
   Sample1_Track : constant Tracks := 6;
   Sample2_Track : constant Tracks := 7;
   Speech_Track  : constant Tracks := 8;

   type Track_Arr is array (Tracks) of Track_Rec;

   Default_Track : constant Track_Rec :=
     (MIDI_Enabled => False,
      Chan => 0,
      Volume => Init_Volume,
      Pan => Init_Pan,
      FX_Kind => Bypass,
      LFO_Rate => 63,
      LFO_Amp => 63,
      LFO_Shape => LFO_Shape_Kind'First,
      LFO_Target => LFO_Target_Kind'Last,
      LFO_Amp_Mode => LFO_Amp_Kind'First,
      LFO_Sync => Off,
      LFO_Loop => On,
      CC => ((0, 63, "CC0              "),
             (1, 63, "CC1              "),
             (2, 63, "CC2              "),
             (3, 63, "CC3              ")
            ),
      Engine => 0,
      Arp_Mode => Arp_Mode_Kind'First,
      Arp_Notes => Arp_Notes_Kind'First,
      Notes_Per_Chord => Chord_Settings.Chord_Index_Range'Last
     );

   Default_Kick_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 63, "CC0              "),
                                      (1, 63, "CC1              "),
                                      (2, 63, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Snare_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 63, "CC0              "),
                                      (1, 63, "CC1              "),
                                      (2, 63, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Cymbal_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 127, "CC0              "),
                                      (1, 16, "CC1              "),
                                      (2, 0, "CC2              "),
                                      (3, 0, "CC3              ")
                                     ));

   Default_Bass_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 63, "CC0              "),
                                      (1, 63, "CC1              "),
                                      (2, 63, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Lead_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 63, "CC0              "),
                                      (1, 63, "CC1              "),
                                      (2, 63, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Sample1_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 63, "CC0              "),
                                      (1, 0, "CC1              "),
                                      (2, 127, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Sample2_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 63, "CC0              "),
                                      (1, 0, "CC1              "),
                                      (2, 127, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Speech_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 63, "CC0              "),
                                      (1, 63, "CC1              "),
                                      (2, 63, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   type Chord_Rec is record
      Tonic : MIDI.MIDI_Key := MIDI.C4;
      Name  : WNM.Chord_Settings.Chord_Name :=
        WNM.Chord_Settings.Chord_Name'First;
   end record;

   type Chord_Arr is array (WNM.Chords) of Chord_Rec;

   Default_Chord : constant Chord_Rec :=
     (Tonic => MIDI.C4,
      Name => WNM.Chord_Settings.Chord_Name'First);

   type FX_Rec is record
      Drive_Amt : MIDI.MIDI_Data := 60;
      Delay_Time : MIDI.MIDI_Data := 60;
      Delay_Feedback : MIDI.MIDI_Data := 60;
      Filter_Mode : Filter_Mode_Kind := Filter_Mode_Kind'First;
      Filter_Cutoff : MIDI.MIDI_Data := 60;
      Filter_Reso : MIDI.MIDI_Data := 60;
   end record;

   Default_FX : constant FX_Rec := (Drive_Amt => 60,
                                    Delay_Time => 60,
                                    Delay_Feedback => 60,
                                    Filter_Mode => Filter_Mode_Kind'First,
                                    Filter_Cutoff => 60,
                                    Filter_Reso => 60);

   type Project_Rec is record
      BPM : Beat_Per_Minute := 120;

      Seqs : All_Patterns :=
        (others => (4 => (others => Default_Step_Bass),
                    5 => (others => Default_Step_Lead),
                    others => (others => Default_Step)));

      Tracks : Track_Arr := (Kick_Track    => Default_Kick_Track,
                             Snare_Track   => Default_Snare_Track,
                             Cymbal_Track  => Default_Cymbal_Track,
                             Bass_Track    => Default_Bass_Track,
                             Lead_Track    => Default_Lead_Track,
                             Sample1_Track => Default_Sample1_Track,
                             Sample2_Track => Default_Sample2_Track,
                             Speech_Track  => Default_Speech_Track,
                             others        => Default_Track);
      Chords : Chord_Arr := (others => Default_Chord);
      FX     : FX_Rec := Default_FX;
   end record;

   G_Project : Project_Rec := (others => <>);

   procedure Synchronize_Voice_Settings (T : Tracks);
   --  Send all the synth voice settings to the coprocessor to update it

   procedure Synchronize_Voice_Engine (T : Tracks);
   --  Send a message to coprocessor with selector engine for the track

   procedure Synchronize_Track_Mix_Settings (T : Tracks);
   --  Send a message to coprocessor with Volume, Pan, and FX values for the
   --  track

   procedure Synchronize_FX_Setting (S : User_FX_Settings);
   --  Send one of the FX settings to the coprocessor

   procedure Synchronize_All_FX_Settings;
   --  Send all the FX settings to the coprocessor

end WNM.Project;
