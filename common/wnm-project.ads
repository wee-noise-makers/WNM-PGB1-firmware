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
with WNM.Chord_Settings;
with WNM.Synth;
with WNM.Time;

with Tresses;

private with Enum_Next;

package WNM.Project is

   Editing_Chord : Chord_Button := Chord_Button'First;
   Editing_Lead : Lead_Button := Lead_Button'First;
   Editing_Track : Tracks := Tracks'First;

   type Playhead is record
      Steps_Count : Natural := 0;
   end record;

   procedure Set_BPM (BPM : Beat_Per_Minute);
   procedure Change_BPM (BPM_Delta : Integer);
   function Get_BPM return Beat_Per_Minute;

   function Samples_Per_Beat return Synth.Sample_Time;
   function Microseconds_Per_Beat return Time.Time_Microseconds;

   procedure Clear;
   --  Reset the current project to default values

   procedure Handle_MIDI (Msg : MIDI.Message);

   type CC_Id is (A, B, C, D);

   function CC_Letter (ID : CC_Id) return String
   is (case ID is
          when A => "A",
          when B => "B",
          when C => "C",
          when D => "D");

   type Chord_Play_Mode_Kind is (Press_Release, Hold, Beat);

   function Chord_Play_Mode return Chord_Play_Mode_Kind;
   procedure Chord_Play_Mode_Next;

   type Lead_Play_Mode_Kind is (Press_Release, Hold, Arp_16, Arp_8, Arp_4);

   function Lead_Play_Mode return Lead_Play_Mode_Kind;
   procedure Lead_Play_Mode_Next;

   type Scale_Choice is (Random, Major, Minor, Modal);
   subtype Valid_Scale_Choice is Scale_Choice range Major .. Modal;
   type Key_Choice is (Random, C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B);
   function Img (Key : Key_Choice) return String
   is (case Key is
          when Random => "Random",
          when C => "C",
          when Cs => "C#",
          when D => "D",
          when Ds => "D#",
          when E => "E",
          when F => "F",
          when Fs => "F#",
          when G => "G",
          when Gs => "G#",
          when A => "A",
          when As => "A#",
          when B => "B");

   procedure Randomly_Pick_A_Progression (Scale_C : Scale_Choice;
                                          Key_C   : Key_Choice);
   --  a.k.a. The Magic Hat of Chord Progression

   procedure Load_Progression (Scale_Choice : Valid_Scale_Choice;
                               Key          : MIDI.MIDI_Key;
                               Id           : Natural);

   -----------
   -- Track --
   -----------

   type Track_Mode_Kind is (MIDI_Mode, Sample1_Mode, Sample2_Mode,
                            Kick_Mode, Snare_Mode, Hihat_Mode,
                            Bass_Mode, Lead_Mode, Chord_Mode, Reverb_Mode,
                            Drive_Mode, Bitcrush_Mode);

   function Img (M : Track_Mode_Kind) return String
   is (case M is
          when Sample1_Mode  => "Sample 1",
          when Sample2_Mode  => "Sample 2",
          when MIDI_Mode     => "MIDI",
          when Kick_Mode     => "Kick",
          when Snare_Mode    => "Snare",
          when Hihat_Mode    => "Hi-hat",
          when Lead_Mode     => "Lead",
          when Bass_Mode     => "Bass",
          when Chord_Mode    => "Chord",
          when Reverb_Mode   => "FX Reverb",
          when Drive_Mode    => "FX Overdrive",
          when Bitcrush_Mode => "FX Bitcrusher");

   subtype Synth_Track_Mode_Kind is
     Track_Mode_Kind range Sample1_Mode .. Bitcrush_Mode;

   function Voice_MIDI_Chan (Voice : Synth_Track_Mode_Kind)
                             return MIDI.MIDI_Channel
   is (case Voice is
          when Sample1_Mode  => Synth.Sample1_Channel,
          when Sample2_Mode  => Synth.Sample2_Channel,
          when Kick_Mode     => Synth.Kick_Channel,
          when Snare_Mode    => Synth.Snare_Channel,
          when Hihat_Mode    => Synth.Hihat_Channel,
          when Bass_Mode     => Synth.Bass_Channel,
          when Lead_Mode     => Synth.Lead_Channel,
          when Chord_Mode    => Synth.Chord_Channel,
          when Reverb_Mode   => Synth.Reverb_Channel,
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
   function Mode (T : Tracks) return Track_Mode_Kind;
   function MIDI_Chan (T : Tracks) return MIDI.MIDI_Channel;
   function Track_Name (T : Tracks) return String;
   function Track_Volume (T : Tracks) return Audio_Volume;
   function Track_Pan (T : Tracks) return Audio_Pan;
   function Track_Offset (T : Tracks) return Octave_Offset;

   type Shuffle_Value is range 0 .. 100;
   function Track_Shuffle (T : Tracks) return Shuffle_Value;
   function CC_Default (T : Tracks;
                        Id : CC_Id)
                        return MIDI.MIDI_Data;
   function Master_FX (T : Tracks) return FX_Kind;

   function LFO_Rate (T : Tracks) return MIDI.MIDI_Data;
   function LFO_Amp (T : Tracks) return MIDI.MIDI_Data;

   type LFO_Target_Kind is (P1, P2, P3, P4, Vol, Pan, None);
   for LFO_Target_Kind use (P1 => Synth.Voice_Param_1_CC,
                            P2 => Synth.Voice_Param_2_CC,
                            P3 => Synth.Voice_Param_3_CC,
                            P4 => Synth.Voice_Param_4_CC,
                            Vol => Synth.Voice_Volume_CC,
                            Pan => Synth.Voice_Pan_CC,
                            None => MIDI.MIDI_Data'Last);
   function LFO_Target (T : Tracks) return LFO_Target_Kind;

   type LFO_Shape_Kind is (Sine, Triangle, Ramp_Up, Ramp_Down,
                           Exp_Up, Exp_Down);
   for LFO_Shape_Kind use (Sine      => 0,
                           Triangle  => 1,
                           Ramp_Up   => 2,
                           Ramp_Down => 3,
                           Exp_Up    => 4,
                           Exp_Down  => 5);
   function LFO_Shape (T : Tracks) return LFO_Shape_Kind;

   type LFO_Sync_Kind is (Off, On);
   for LFO_Sync_Kind use (Off => 0, On => 1);
   function LFO_Sync (T : Tracks) return LFO_Sync_Kind;

   type LFO_Loop_Kind is (Off, On);
   for LFO_Loop_Kind use (Off => 0, On => 1);
   function LFO_Loop (T : Tracks) return LFO_Loop_Kind;

   type LFO_Amp_Kind is (Positive, Center, Negative);
   for LFO_Amp_Kind use (Positive => 0,
                         Center   => 1,
                         Negative => 2);
   function LFO_Amp_Mode (T : Tracks) return LFO_Amp_Kind;

   function CC_Controller (T : Tracks;
                           Id : CC_Id)
                           return MIDI.MIDI_Data;
   function CC_Controller_Label (T    : Tracks;
                                 Id   : CC_Id)
                                 return Controller_Label;
   function CC_Controller_Short_Label (T    : Tracks;
                                       Id   : CC_Id)
                                       return Tresses.Short_Label;
   function Selected_Engine (T : Tracks)
                             return MIDI.MIDI_Data;
   function Selected_Engine_Img (T : Tracks)
                                 return String;
   function Arp_Mode (T : Tracks) return Arp_Mode_Kind;
   function Arp_Notes (T : Tracks) return Arp_Notes_Kind;
   function Notes_Per_Chord (T : Tracks)
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
     is Track_Settings range Engine .. Arp_Notes;

   procedure Set (S : User_Track_Settings;
                  V : WNM_HAL.Touch_Value;
                  T : Tracks);

   procedure Next_Value (S : User_Track_Settings;
                         T : Tracks);
   procedure Prev_Value (S : User_Track_Settings;
                         T : Tracks);
   procedure Next_Value_Fast (S : User_Track_Settings;
                              T : Tracks);
   procedure Prev_Value_Fast (S : User_Track_Settings;
                              T : Tracks);

   procedure Set_CC_Controller (T : Tracks; Id : CC_Id; C : MIDI.MIDI_Data);

   procedure Set_CC_Controller_Label (T    : Tracks;
                                      Id   : CC_Id;
                                      Label : Controller_Label);

   ------------
   -- Chords --
   ------------

   type Chord_Settings is (Root, Quality);
   subtype User_Chord_Settings is Chord_Settings;

   function Root_Note (C : Chord_Button) return MIDI.MIDI_Key;
   function Quality (C : Chord_Button) return WNM.Chord_Settings.Chord_Name;

   procedure Set (S : User_Chord_Settings;
                  V : WNM_HAL.Touch_Value;
                  C : Chord_Button);
   procedure Next_Value (S : User_Chord_Settings; C : Chord_Button);
   procedure Prev_Value (S : User_Chord_Settings; C : Chord_Button);

   ----------
   -- Lead --
   ----------

   type Lead_Evt is (None, N1, N2, N3, N4, Random);
   type Lead_Seq_Length is range 1 .. 64;

   type Lead_Tracks is (Lead, Bass);
   function Trigger (B    : Lead_Button;
                     LT   : Lead_Tracks;
                     Step : Lead_Seq_Length)
                     return Lead_Evt;
   procedure Trigger_Next (B    : Lead_Button;
                           LT : Lead_Tracks;
                           Step : Lead_Seq_Length);
   procedure Trigger_Prev (B    : Lead_Button;
                           LT : Lead_Tracks;
                           Step : Lead_Seq_Length);

   function Seq_Length (B : Lead_Button) return Lead_Seq_Length;
   procedure Incr_Seq_Length (B : Lead_Button);
   procedure Decr_Seq_Length (B : Lead_Button);

   type Lead_Settings is (Note);
   subtype User_Lead_Settings is Lead_Settings;

   --  function Note (C : Lead_Button) return MIDI.MIDI_Key;

   procedure Set (S : User_Lead_Settings;
                  V : WNM_HAL.Touch_Value;
                  C : Lead_Button);
   procedure Next_Value (S : User_Lead_Settings; C : Lead_Button);
   procedure Prev_Value (S : User_Lead_Settings; C : Lead_Button);

   -------------
   -- Pattern --
   -------------

   function Pattern_Length return WNM.Pattern_Length;
   procedure Incr_Pattern_Length;
   procedure Decr_Pattern_Length;

   type Trigger_Kind is (None, Ghost, Hit, Accent);
   type Drum_Tracks is (Kick, Snare, Hihat_Closed, Hihat_Open, Sample);

   function Trigger (DT : Drum_Tracks; Step : WNM.Pattern_Length)
                     return Trigger_Kind;

   procedure Trigger_Next (DT : Drum_Tracks; Step : WNM.Pattern_Length);
   procedure Trigger_Prev (DT : Drum_Tracks; Step : WNM.Pattern_Length);

   type Pattern_Settings is (Length);

   for Pattern_Settings'Size use 8;
   for Pattern_Settings use (Length    => 0);

   subtype User_Pattern_Settings is Pattern_Settings range Length .. Length;

   procedure Set (S : User_Pattern_Settings; V : WNM_HAL.Touch_Value);
   procedure Next_Value (S : User_Pattern_Settings);
   procedure Prev_Value (S : User_Pattern_Settings);

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
   function Alt_Slider_Value return MIDI.MIDI_Data;
   function Alt_Slider_Target_Label return String;

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

   ----------
   -- Fill --
   ----------

   type Auto_Fill_Kind is (Off, Auto_Low, Auto_High, Auto_Buildup);
   procedure Auto_Fill (Kind : Auto_Fill_Kind);
   function Auto_Fill_State return Auto_Fill_Kind;

   procedure Step_Fill_Toogle;
   function Step_Fill return Boolean;
private

   type Global_Settings is (BPM);
   for Global_Settings use (BPM => 0);

   --  package Boolean_Next is new Enum_Next (Boolean);
   --  use Boolean_Next;

   package Trigger_Kind_Next is new Enum_Next (Trigger_Kind,
                                               Wrap => False);
   use Trigger_Kind_Next;

   package MIDI_Data_Next is new Enum_Next (T    => MIDI.MIDI_Data,
                                            Wrap => False);
   use MIDI_Data_Next;

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

   package Lead_Seq_Trig is new Enum_Next (T    => Lead_Evt,
                                           Wrap => True);
   use Lead_Seq_Trig;

   --  package Duration_In_Steps_Next is new Enum_Next
   --    (T    => WNM.Duration_In_Steps,
   --     Wrap => False);
   --  use Duration_In_Steps_Next;
   --
   --  package Pattern_Length_Next is new Enum_Next
   --    (T    => WNM.Pattern_Length,
   --     Wrap => True);
   --  use Pattern_Length_Next;

   type CC_Val_Array is array (CC_Id) of MIDI.MIDI_Data;
   type CC_Ena_Array is array (CC_Id) of Boolean;

   for Octave_Offset'Size use 5;
   package Octave_Offset_Next is new Enum_Next (T    => Octave_Offset,
                                                Wrap => False);
   use Octave_Offset_Next;

   package Shuffle_Value_Next is new Enum_Next (T => Shuffle_Value,
                                                Wrap => False);
   use Shuffle_Value_Next;

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
      Notes_Per_Chord : WNM.Chord_Settings.Chord_Index_Range :=
        WNM.Chord_Settings.Chord_Index_Range'Last;
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
   Drive_Track    : constant Tracks := 10;
   Bitcrush_Track : constant Tracks := 11;

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
      Notes_Per_Chord => WNM.Chord_Settings.Chord_Index_Range'Last
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
     (Default_Track with delta Engine => 1,
                               CC => ((0, 127, "CC0              "),
                                      (1, 100, "CC1              "),
                                      (2, 29, "CC2              "),
                                      (3, 15, "CC3              ")
                                     ));

   Default_Bass_Track : constant Track_Rec :=
     (Default_Track with delta Engine => 7,
                               Offset => -2,
                               FX => Reverb,
                               CC => ((0, 63, "CC0              "),
                                      (1, 63, "CC1              "),
                                      (2, 10, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Lead_Track : constant Track_Rec :=
     (Default_Track with delta Engine => 0,
                               FX => Reverb,
                               CC => ((0, 60, "CC0              "),
                                      (1, 40, "CC1              "),
                                      (2, 10, "CC2              "),
                                      (3, 60, "CC3              ")
                                     ));

   Default_Sample1_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 0, "CC0              "),
                                      (1, 0, "CC1              "),
                                      (2, 0, "CC2              "),
                                      (3, 127, "CC3              ")
                                     ));

   Default_Sample2_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 4, "CC0              "),
                                      (1, 0, "CC1              "),
                                      (2, 0, "CC2              "),
                                      (3, 127, "CC3              ")
                                     ));

   Default_Speech_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 63, "CC0              "),
                                      (1, 63, "CC1              "),
                                      (2, 63, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Chord_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 0, "CC0              "),
                                      (1, 10, "CC1              "),
                                      (2, 10, "CC2              "),
                                      (3, 63, "CC3              ")
                                     ));

   Default_Bitcrush_Track : constant Track_Rec :=
     (Default_Track with delta CC => ((0, 63, "CC0              "),
                                      (1, 103, "CC1              "),
                                      (2, 63, "CC2              "),
                                      (3, 119, "CC3              ")
                                     ));

   Tracks_Defaults : constant Track_Arr :=
     (Kick_Track     => Default_Kick_Track,
      Snare_Track    => Default_Snare_Track,
      Cymbal_Track   => Default_Cymbal_Track,
      Bass_Track     => Default_Bass_Track,
      Lead_Track     => Default_Lead_Track,
      Sample1_Track  => Default_Sample1_Track,
      Sample2_Track  => Default_Sample2_Track,
      Chord_Track    => Default_Chord_Track,
      Bitcrush_Track => Default_Bitcrush_Track,
      others         => Default_Track);

   type Pattern_Arr is array (Drum_Tracks, WNM.Pattern_Length) of Trigger_Kind;

   type Chord_Rec is record
      Root : MIDI.MIDI_Key := MIDI.C4;
      Quality : WNM.Chord_Settings.Chord_Name := WNM.Chord_Settings.Maj_Triad;
   end record;

   type Chord_Arr is array (Chord_Button) of Chord_Rec;

   type Lead_Seq is array (Lead_Tracks, Lead_Seq_Length) of Lead_Evt;
   type Lead_Rec is record
      Len  : Lead_Seq_Length := 1;
      Seq : Lead_Seq := (others => (others => None));
   end record;

   type Lead_Arr is array (Lead_Button) of Lead_Rec;

   BPM_Default : constant := 90.0;
   type Project_Rec is record
      BPM : Beat_Per_Minute := BPM_Default;

      Tracks : Track_Arr := Tracks_Defaults;

      Chords : Chord_Arr;
      Leads  : Lead_Arr;

      Pattern_Len : WNM.Pattern_Length := 8;
      Pattern : Pattern_Arr := (others => (others => None));

      Alt_Slider_Track : WNM.Tracks := Lead_Track;
      Alt_Slider_Target : Alt_Slider_Control := Alt_Slider_Control'First;

      Chord_Play_Mode : Chord_Play_Mode_Kind := Chord_Play_Mode_Kind'First;
      Lead_Play_Mode  : Lead_Play_Mode_Kind := Lead_Play_Mode_Kind'First;
   end record;

   G_Project : Project_Rec := (others => <>);

   procedure Synchronize_Synth_Settings (T : Tracks);
   --  Send all the synth voice settings to the coprocessor to update it

   type Playhead_Array is array (Tracks) of Playhead;

   type Play_State is record
      Next_Step : WNM.Pattern_Length := 1;
   end record;

   G_Part_Origin_Query : Boolean := False with Atomic;

   G_Play_State   : Play_State := (others => <>);
   G_Play_State_Save : Play_State := (others => <>);

   procedure Save_Play_State;
   procedure Restore_Play_State;

   G_Roll_State        : Roll_Kind := Off with Atomic;
   G_Roll_Next_State   : Roll_Kind := Off with Atomic;
   G_Auto_Fill_State   : Auto_Fill_Kind := Off with Atomic;
   G_Fill_Buildup_Proba : Rand_Percent := Rand_Percent'First with Atomic;
   G_Step_Fill : Boolean := False with Atomic;

   G_Current_Chord : WNM.Chord_Settings.Chord_Notes := (others => MIDI.C4);

end WNM.Project;
