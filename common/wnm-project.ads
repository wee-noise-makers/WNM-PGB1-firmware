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

   Editing_Step     : Sequencer_Steps := Sequencer_Steps'First;
   Editing_Track    : Tracks          := Tracks'First;
   Editing_Pattern  : Patterns        := Patterns'First;
   Editing_Song_Elt : Song_Element    := Song_Element'First;

   type Playhead is record
      P : Patterns := Patterns'First;
      Steps_Count : Natural := 0;
   end record;

   procedure Do_Copy (T : in out WNM.Sequence_Copy.Copy_Transaction)
     with Pre => WNM.Sequence_Copy.Is_Complete (T);

   procedure Set_BPM (BPM : Beat_Per_Minute);
   procedure Change_BPM (BPM_Delta : Integer);
   function Get_BPM return Beat_Per_Minute;
   function Samples_Per_Beat return Synth.Sample_Time;
   function Microseconds_Per_Beat return Time.Time_Microseconds;

   procedure Clear;
   --  Reset the current project to default values

   procedure Handle_MIDI (Msg : MIDI.Message);

   ----------
   -- Step --
   ----------

   type Trigger_Kind is (None, Always, Fill, Not_Fill,
                         Percent_25, Percent_50, Percent_75,
                         One_Of_Two, One_Of_Three, One_Of_Four, One_Of_Five);

   function Img (T : Trigger_Kind) return String
   is (case T is
          when None         => "Never",
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
   function Set (Track : Tracks; PH : Playhead) return Boolean;
   function Set (Track   : Tracks := Editing_Track;
                 Pattern : Patterns := Editing_Pattern;
                 Step    : Sequencer_Steps)
                 return Boolean;

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

   procedure Set (S : User_Step_Settings; V : WNM_HAL.Touch_Value);
   procedure Next_Value (S : User_Step_Settings);
   procedure Prev_Value (S : User_Step_Settings);
   procedure Next_Value_Fast (S : User_Step_Settings);
   procedure Prev_Value_Fast (S : User_Step_Settings);

   -----------
   -- Track --
   -----------

   type Track_Mode_Kind is (MIDI_Mode, Sample1_Mode, Sample2_Mode,
                            Speech_Mode, Kick_Mode, Snare_Mode, Hihat_Mode,
                            Bass_Mode, Lead_Mode, Chord_Mode, Reverb_Mode,
                            Filter_Mode, Drive_Mode, Bitcrush_Mode);

   function Img (M : Track_Mode_Kind) return String
   is (case M is
          when Sample1_Mode  => "Sample 1",
          when Sample2_Mode  => "Sample 2",
          when MIDI_Mode     => "MIDI",
          when Speech_Mode   => "Speech",
          when Kick_Mode     => "Kick",
          when Snare_Mode    => "Snare",
          when Hihat_Mode    => "Hi-hat",
          when Lead_Mode     => "Lead",
          when Bass_Mode     => "Bass",
          when Chord_Mode    => "Chord",
          when Reverb_Mode   => "FX Reverb",
          when Filter_Mode   => "FX Filter",
          when Drive_Mode    => "FX Overdrive",
          when Bitcrush_Mode => "FX Bitcrusher");

   subtype Synth_Track_Mode_Kind is
     Track_Mode_Kind range Sample1_Mode .. Bitcrush_Mode;

   function Voice_MIDI_Chan (Voice : Synth_Track_Mode_Kind)
                             return MIDI.MIDI_Channel
   is (case Voice is
          when Sample1_Mode  => Synth.Sample1_Channel,
          when Sample2_Mode  => Synth.Sample2_Channel,
          when Speech_Mode   => Synth.Speech_Channel,
          when Kick_Mode     => Synth.Kick_Channel,
          when Snare_Mode    => Synth.Snare_Channel,
          when Hihat_Mode   => Synth.Hihat_Channel,
          when Bass_Mode     => Synth.Bass_Channel,
          when Lead_Mode     => Synth.Lead_Channel,
          when Chord_Mode    => Synth.Chord_Channel,
          when Reverb_Mode   => Synth.Reverb_Channel,
          when Filter_Mode   => Synth.Filter_Channel,
          when Drive_Mode    => Synth.Drive_Channel,
          when Bitcrush_Mode => Synth.Bitcrusher_Channel);

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

   type Octave_Offset is range -8 .. 8;
   function Add_Sat (A, B : Octave_Offset) return Octave_Offset;

   -- Track Getters --
   function Mode (T : Tracks := Editing_Track) return Track_Mode_Kind;
   function MIDI_Chan (T : Tracks := Editing_Track) return MIDI.MIDI_Channel;
   function Track_Name (T : Tracks := Editing_Track) return String;
   function Track_Volume (T : Tracks := Editing_Track) return Audio_Volume;
   function Track_Pan (T : Tracks := Editing_Track) return Audio_Pan;
   function Track_Offset (T : Tracks := Editing_Track) return Octave_Offset;

   type Shuffle_Value is range 0 .. 100;
   function Track_Shuffle (T : Tracks := Editing_Track) return Shuffle_Value;
   function CC_Default (T : Tracks := Editing_Track;
                        Id : CC_Id)
                        return MIDI.MIDI_Data;
   function Master_FX (T : Tracks := Editing_Track) return FX_Kind;

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

   type Track_Settings is (Engine,
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
                           Track_Octave_Offset,
                           Shuffle,
                           Arp_Mode,
                           Arp_Notes,
                           Notes_Per_Chord,
                           MIDI_Chan,
                           MIDI_Instrument,
                           CC_Ctrl_A, CC_Label_A,
                           CC_Ctrl_B, CC_Label_B,
                           CC_Ctrl_C, CC_Label_C,
                           CC_Ctrl_D, CC_Label_D,
                           Track_Mode,

                           LFO_Amp_Mode,
                           LFO_Loop,
                           LFO_Sync);

   for Track_Settings'Size use 8;
   for Track_Settings use (Engine              => 1,
                           CC_Default_A        => 2,
                           CC_Default_B        => 3,
                           CC_Default_C        => 4,
                           CC_Default_D        => 5,
                           LFO_Rate            => 6,
                           LFO_Amplitude       => 7,
                           LFO_Shape           => 8,
                           LFO_Target          => 9,
                           Volume              => 10,
                           Pan                 => 11,
                           Master_FX           => 12,
                           Track_Octave_Offset => 13,
                           Shuffle             => 14,
                           Arp_Mode            => 15,
                           Arp_Notes           => 16,
                           Notes_Per_Chord     => 17,
                           MIDI_Chan           => 18,
                           MIDI_Instrument     => 19,
                           CC_Ctrl_A           => 20,
                           CC_Label_A          => 21,
                           CC_Ctrl_B           => 22,
                           CC_Label_B          => 23,
                           CC_Ctrl_C           => 24,
                           CC_Label_C          => 25,
                           CC_Ctrl_D           => 26,
                           CC_Label_D          => 27,
                           Track_Mode          => 28,
                           LFO_Amp_Mode        => 29,
                           LFO_Loop            => 30,
                           LFO_Sync            => 31);

   subtype User_Track_Settings
     is Track_Settings range Engine .. Track_Mode;

   procedure Set (T : Tracks;
                  S : User_Track_Settings;
                  V : WNM_HAL.Touch_Value);

   procedure Next_Value (S : User_Track_Settings);
   procedure Prev_Value (S : User_Track_Settings);
   procedure Next_Value_Fast (S : User_Track_Settings);
   procedure Prev_Value_Fast (S : User_Track_Settings);

   procedure Set_CC_Controller (T : Tracks; Id : CC_Id; C : MIDI.MIDI_Data);

   procedure Set_CC_Controller_Label (T    : Tracks;
                                      Id   : CC_Id;
                                      Label : Controller_Label);

   -------------
   -- Pattern --
   -------------

   function Link (T : Tracks := Editing_Track;
                  P : Patterns := Editing_Pattern)
                  return Boolean;

   function Pattern_Length (T : Tracks := Editing_Track;
                            P : Patterns := Editing_Pattern)
                            return WNM.Pattern_Length;

   type Pattern_Settings is (Length,
                             Has_Link);

   for Pattern_Settings'Size use 8;
   for Pattern_Settings use (Length    => 0,
                             Has_Link  => 1);

   subtype User_Pattern_Settings is Pattern_Settings range Length .. Has_Link;

   procedure Set (S : User_Pattern_Settings; V : WNM_HAL.Touch_Value);
   procedure Next_Value (S : User_Pattern_Settings);
   procedure Prev_Value (S : User_Pattern_Settings);

   -----------
   -- Chord --
   -----------

   type Chord_Slot_Id is range 1 .. 16;

   type Chord_Setting_Kind is (Tonic,
                               Name,
                               Duration);

   for Chord_Setting_Kind'Size use 8;
   for Chord_Setting_Kind use (Tonic    => 0,
                               Name     => 1,
                               Duration => 2);

   subtype User_Chord_Settings is Chord_Setting_Kind range Tonic .. Duration;

   -- Chord Getters --

   function Progression_Length (C : WNM.Chord_Progressions)
                                return Chord_Slot_Id;

   function Selected_Tonic (C  : WNM.Chord_Progressions;
                            Id : Chord_Slot_Id)
                            return MIDI.MIDI_Key;
   function Selected_Name (C : WNM.Chord_Progressions;
                           Id : Chord_Slot_Id)
                           return WNM.Chord_Settings.Chord_Name;
   function Selected_Duration (C : WNM.Chord_Progressions;
                               Id : Chord_Slot_Id)
                               return Duration_In_Steps;

   procedure Increase_Progession_Length (Prog : WNM.Chord_Progressions);
   procedure Decrease_Progession_Length (Prog : WNM.Chord_Progressions);

   procedure Set (C  : WNM.Chord_Progressions;
                  Id : Chord_Slot_Id;
                  S  : User_Chord_Settings;
                  V  : WNM_HAL.Touch_Value);
   procedure Next_Value (C  : WNM.Chord_Progressions;
                         Id : Chord_Slot_Id;
                         S  : User_Chord_Settings);
   procedure Prev_Value (C  : WNM.Chord_Progressions;
                         Id : Chord_Slot_Id;
                         S  : User_Chord_Settings);

   procedure Next_Value_Fast (C  : WNM.Chord_Progressions;
                              Id : Chord_Slot_Id;
                              S  : User_Chord_Settings);
   procedure Prev_Value_Fast (C  : WNM.Chord_Progressions;
                              Id : Chord_Slot_Id;
                              S  : User_Chord_Settings);

   type Scale_Choice is (Random, Major, Minor);
   type Key_Choice is (Random, C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B);
   procedure Randomly_Pick_A_Progression (Prog_Id : WNM.Chord_Progressions;
                                          Scale_C : Scale_Choice;
                                          Key_C   : Key_Choice);
   --  a.k.a. The Magic Hat of Chord Progression

   -----------
   -- Parts --
   -----------

   type Part_Settings is (Part_Patterns,
                          Part_Length,
                          Part_Progression,
                          Part_Link);

   for Part_Settings'Size use 8;
   for Part_Settings use (Part_Patterns    => 0,
                          Part_Length      => 1,
                          Part_Progression => 2,
                          Part_Link        => 3);

   subtype User_Part_Settings
     is Part_Settings range Part_Patterns .. Part_Link;

   function Part_Pattern (P : Parts;
                          T : Tracks)
                          return Patterns;

   function Part_Muted (P : Parts;
                        T : Tracks)
                        return Boolean;

   function Part_Chords (P : Parts) return Chord_Progressions;
   function Part_Link (P : Parts) return Boolean;
   function Part_Length (P : Parts) return Duration_In_Steps;

   procedure Toggle_Mute (P : Parts; T : Tracks);

   procedure Pattern_Next (P : Parts; T : Tracks);
   procedure Pattern_Prev (P : Parts; T : Tracks);

   procedure Part_Chords_Next (P : Parts);
   procedure Part_Chords_Prev (P : Parts);
   procedure Toggle_Link (P : Parts);

   procedure Part_Len_Next (P : Parts);
   procedure Part_Len_Prev (P : Parts);

   procedure Set_Origin (P : Parts);

   --------
   -- FX --
   --------

   type Filter_Mode_Kind is (Low_Pass, Band_Pass, High_Pass);

   ------------
   -- Slider --
   ------------

   --  "Alt Slider" is controlled by touch slider during Alt/FX mode
   type Alt_Slider_Control is (Alt_Sld_CC_Default_A,
                               Alt_Sld_CC_Default_B,
                               Alt_Sld_CC_Default_C,
                               Alt_Sld_CC_Default_D,
                               Alt_Sld_LFO_Rate,
                               Alt_Sld_LFO_Amplitude,
                               Alt_Sld_Volume,
                               Alt_Sld_Pan,
                               Alt_Sld_Shuffle);
   function To_Track_Setting (Ctrl : Alt_Slider_Control)
                              return User_Track_Settings
   is (case Ctrl is
       when Alt_Sld_CC_Default_A  => CC_Default_A,
       when Alt_Sld_CC_Default_B  => CC_Default_B,
       when Alt_Sld_CC_Default_C  => CC_Default_C,
       when Alt_Sld_CC_Default_D  => CC_Default_D,
       when Alt_Sld_LFO_Rate      => LFO_Rate,
       when Alt_Sld_LFO_Amplitude => LFO_Amplitude,
       when Alt_Sld_Volume        => Volume,
       when Alt_Sld_Pan           => Pan,
       when Alt_Sld_Shuffle       => Shuffle);

   function Alt_Slider_Target return Alt_Slider_Control;
   function Alt_Slider_Target_Img return String;
   function Alt_Slider_Track return Tracks;

   procedure Alt_Slider_Set (Val : WNM_HAL.Touch_Value);
   procedure Alt_Slider_Target_Next;
   procedure Alt_Slider_Target_Prev;
   procedure Alt_Slider_Track_Next;
   procedure Alt_Slider_Track_Prev;

   ----------
   -- Roll --
   ----------
   type Roll_Kind is (Off, Beat, Half, Quarter, Eighth);
   procedure Roll (Kind : Roll_Kind);
   function Roll_State return Roll_Kind;

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

   package Master_FX_Next is new Enum_Next (T    => FX_Kind,
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

   package LFO_Shape_Next is new Enum_Next (T    => LFO_Shape_Kind,
                                            Wrap => True);
   use LFO_Shape_Next;

   package LFO_Target_Next is new Enum_Next (T    => LFO_Target_Kind,
                                             Wrap => True);
   use LFO_Target_Next;

   package Alt_Slider_Control_Next is new Enum_Next (T  => Alt_Slider_Control,
                                                     Wrap => True);
   use Alt_Slider_Control_Next;

   package Tracks_Next is new Enum_Next (T    => WNM.Tracks,
                                         Wrap => True);
   use Tracks_Next;

   package Duration_In_Steps_Next is new Enum_Next
     (T    => WNM.Duration_In_Steps,
      Wrap => False);
   use Duration_In_Steps_Next;

   package Pattern_Length_Next is new Enum_Next
     (T    => WNM.Pattern_Length,
      Wrap => True);
   use Pattern_Length_Next;

   type CC_Val_Array is array (CC_Id) of MIDI.MIDI_Data;
   type CC_Ena_Array is array (CC_Id) of Boolean;

   for Octave_Offset'Size use 5;
   package Octave_Offset_Next is new Enum_Next (T    => Octave_Offset,
                                                Wrap => False);
   use Octave_Offset_Next;

   package Shuffle_Value_Next is new Enum_Next (T => Shuffle_Value,
                                                Wrap => False);
   use Shuffle_Value_Next;

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
      Duration => N_16th,
      Velo => MIDI.MIDI_Data'Last,
      CC_Ena => (others => False),
      CC_Val => (others => 0));

   Default_Step_Lead : constant Step_Rec :=
     (Default_Step with delta Note_Mode => Arp,
                              Note => 0);

   Default_Step_Bass : constant Step_Rec :=
     (Default_Step with delta Note_Mode => Note_In_Chord,
                              Note => 0);

   Default_Step_Chord : constant Step_Rec :=
     (Default_Step with delta Note_Mode => Chord,
                              Note => 0);

   type Pattern_Step_Arr is array (Sequencer_Steps) of Step_Rec
     with Pack;
   type Track_Pattern_Step_Arr is array (Patterns) of Pattern_Step_Arr
     with Pack;
   type All_Steps_Arr is array (Tracks) of Track_Pattern_Step_Arr
     with Pack;

   type Pattern_Rec is record
      Length   : WNM.Pattern_Length := Steps_Per_Bar;
      Has_Link : Boolean := False;
   end record;

   Default_Pattern : constant Pattern_Rec := (others => <>);

   type Track_Patterns_Arr is array (Patterns) of Pattern_Rec;
   type All_Pattern_Arr is array (Tracks) of Track_Patterns_Arr;

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
      Offset : Octave_Offset := 0;
      Shuffle : Shuffle_Value := Shuffle_Value'First;
      FX  : FX_Kind := Bypass;
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

   Kick_Track     : constant Tracks := 1;
   Snare_Track    : constant Tracks := 2;
   Cymbal_Track   : constant Tracks := 3;
   Bass_Track     : constant Tracks := 4;
   Lead_Track     : constant Tracks := 5;
   Chord_Track    : constant Tracks := 6;
   Sample1_Track  : constant Tracks := 7;
   Sample2_Track  : constant Tracks := 8;
   --  Speech_Track   : constant Tracks := 8;
   Reverb_Track   : constant Tracks := 9;
   Filter_Track   : constant Tracks := 10;
   Drive_Track    : constant Tracks := 11;
   Bitcrush_Track : constant Tracks := 12;

   type Track_Arr is array (Tracks) of Track_Rec;

   Default_Track : constant Track_Rec :=
     (MIDI_Enabled => False,
      Chan => 0,
      Volume => Init_Volume,
      Pan => Init_Pan,
      Offset => 0,
      Shuffle => Shuffle_Value'First,
      FX  => Bypass,
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
     (Default_Track with delta Offset => -4,
                               CC => ((0, 63, "CC0              "),
                                      (1, 63, "CC1              "),
                                      (2, 63, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Snare_Track : constant Track_Rec :=
     (Default_Track with delta Offset => -1,
                               CC => ((0, 63, "CC0              "),
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
     (Default_Track with delta Engine => 7,
                               Offset => -3,
                               FX => Reverb,
                               CC => ((0, 63, "CC0              "),
                                      (1, 63, "CC1              "),
                                      (2,  0, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Lead_Track : constant Track_Rec :=
     (Default_Track with delta Engine => 0,
                               FX => Reverb,
                               CC => ((0, 60, "CC0              "),
                                      (1, 40, "CC1              "),
                                      (2,  0, "CC2              "),
                                      (3, 60, "CC3              ")
                                     ));

   Default_Sample1_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 0, "CC0              "),
                                      (1, 0, "CC1              "),
                                      (2, 127, "CC2              "),
                                      (3, 0, "CC3              ")
                                     ));

   Default_Sample2_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 1, "CC0              "),
                                      (1, 0, "CC1              "),
                                      (2, 127, "CC2              "),
                                      (3, 0, "CC3              ")
                                     ));

   Default_Speech_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 63, "CC0              "),
                                      (1, 63, "CC1              "),
                                      (2, 63, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Chord_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 0, "CC0              "),
                                      (1, 63, "CC1              "),
                                      (2, 0, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Bitcrush_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 63, "CC0              "),
                                      (1, 103, "CC1              "),
                                      (2, 63, "CC2              "),
                                      (3, 119, "CC3              ")
                                     ));

   type Chord_Rec is record
      Tonic : MIDI.MIDI_Key := MIDI.C4;
      Name  : WNM.Chord_Settings.Chord_Name :=
        WNM.Chord_Settings.Chord_Name'First;
      Duration : Duration_In_Steps := Steps_Per_Bar;
   end record;

   Default_Chord : constant Chord_Rec :=
     (Tonic    => MIDI.C4,
      Name     => WNM.Chord_Settings.Chord_Name'First,
      Duration => 16);

   type Chord_Arr is array (Chord_Slot_Id) of Chord_Rec;

   type Chord_Progression_Rec is record
      Len    : Chord_Slot_Id := Chord_Slot_Id'First;
      Chords : Chord_Arr;
   end record;

   Default_Chord_Progression : constant Chord_Progression_Rec :=
     (others => <>);

   type Chord_Progression_Arr
   is array (WNM.Chord_Progressions) of Chord_Progression_Rec;

   type Pattern_Select_Arr is array (WNM.Tracks) of WNM.Patterns;
   type Track_Mute_Arr is array (WNM.Tracks) of Boolean;
   type Song_Part_Rec is record
      Pattern_Select : Pattern_Select_Arr := (others => 1);
      Track_Mute   : Track_Mute_Arr   := (others => False);
      Progression : Chord_Progressions := Chord_Progressions'First;
      Link : Boolean := False;
      Len : Duration_In_Steps := Steps_Per_Bar;
   end record;

   Default_Part : constant Song_Part_Rec := (others => <>);

   type Song_Part_Arr is array (WNM.Parts) of Song_Part_Rec;

   type Project_Rec is record
      BPM : Beat_Per_Minute := 120;

      Tracks : Track_Arr := (Kick_Track     => Default_Kick_Track,
                             Snare_Track    => Default_Snare_Track,
                             Cymbal_Track   => Default_Cymbal_Track,
                             Bass_Track     => Default_Bass_Track,
                             Lead_Track     => Default_Lead_Track,
                             Sample1_Track  => Default_Sample1_Track,
                             Sample2_Track  => Default_Sample2_Track,
                             Chord_Track    => Default_Chord_Track,
                             Bitcrush_Track => Default_Bitcrush_Track,
                             others         => Default_Track);

      Patterns : All_Pattern_Arr := (others => (others => Default_Pattern));

      Steps : All_Steps_Arr :=
        (Bass_Track  => (others => (others => Default_Step_Bass)),
         Lead_Track  => (others => (others => Default_Step_Lead)),
         Chord_Track => (others => (others => Default_Step_Chord)),
         others      => (others => (others => Default_Step)));

      Parts : Song_Part_Arr;

      --  The First part to play at the end of a link
      Part_Origin : WNM.Parts := WNM.Parts'First;

      Progressions : Chord_Progression_Arr;

      Alt_Slider_Track : WNM.Tracks := Lead_Track;
      Alt_Slider_Target : Alt_Slider_Control := Alt_Slider_Control'First;
   end record;

   G_Project : Project_Rec := (others => <>);

   procedure Synchronize_Synth_Settings (T : Tracks);
   --  Send all the synth voice settings to the coprocessor to update it

   type Playhead_Array is array (Tracks) of Playhead;

   type Play_State is record
      --  Step sequencer
      Playheads : Playhead_Array := (others => (1, 1));

      --  Part Sequencer
      Playing_Part     : WNM.Parts := WNM.Parts'First;
      Origin           : WNM.Parts := WNM.Parts'First;
      Part_Steps_Count : Natural := 0;

      --  Chord Sequencer
      Chord_Steps_Count : Natural := 0;
      Progression : WNM.Chord_Progressions := WNM.Chord_Progressions'First;
      Chord_Id    : Chord_Slot_Id := Chord_Slot_Id'First;
      Tonic       : MIDI.MIDI_Key := MIDI.C4;
      Chord_Name  : WNM.Chord_Settings.Chord_Name :=
        WNM.Chord_Settings.Chord_Name'First;
      Chord       : WNM.Chord_Settings.Chord_Notes := (others => 1);
   end record;

   G_Play_State      : Play_State := (others => <>);
   G_Play_State_Save : Play_State := (others => <>);

   procedure Save_Play_State;
   procedure Restore_Play_State;

   G_Roll_State      : Roll_Kind := Off;
   G_Roll_Step_Count : Natural := 0;
end WNM.Project;
