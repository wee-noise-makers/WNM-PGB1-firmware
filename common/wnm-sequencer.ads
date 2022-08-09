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

with WNM.Time;
with WNM.Synth;
with WNM.MIDI;
with WNM.Sequence_Copy;
with WNM.UI;
with WNM.Sample_Library;

package WNM.Sequencer is

   function Playing_Step return Sequencer_Steps;
   function Playing_Pattern return Patterns;

   function Editing_Step return Sequencer_Steps;
   function Editing_Track return Tracks;
   function Editing_Pattern return Patterns;

   procedure Set_Editing_Step    (S : Sequencer_Steps);
   procedure Set_Editing_Track   (T : Tracks);
   procedure Set_Editing_Pattern (P : Patterns);

   procedure Play_Pause;
   --  Use it to signal a play/pause event

   procedure On_Press (Button : Keyboard_Button;
                       Mode : WNM.UI.Main_Modes);

   procedure On_Release (Button : Keyboard_Button;
                         Mode : WNM.UI.Main_Modes);

   procedure Do_Copy (T : in out WNM.Sequence_Copy.Copy_Transaction)
     with Pre => WNM.Sequence_Copy.Is_Complete (T);

   procedure Set_BPM (BPM : Beat_Per_Minute);
   procedure Change_BPM (BPM_Delta : Integer);
   function BPM return Beat_Per_Minute;
   function Samples_Per_Beat return Synth.Sample_Time;
   function Microseconds_Per_Beat return Time.Time_Microseconds;

   procedure Execute_Step;

   function Update return Time.Time_Microseconds;

   function Set (Step : Sequencer_Steps) return Boolean;
   function Set (Track : Tracks; Step : Sequencer_Steps) return Boolean;

   function Trig (Step : Sequencer_Steps) return WNM.Trigger;
   procedure Trig_Next (Step : Sequencer_Steps);
   procedure Trig_Prev (Step : Sequencer_Steps);

   function Repeat (Step : Sequencer_Steps) return WNM.Repeat;
   procedure Repeat_Next (Step : Sequencer_Steps);
   procedure Repeat_Prev (Step : Sequencer_Steps);

   function Repeat_Rate (Step : Sequencer_Steps) return WNM.Repeat_Rate;
   procedure Repeat_Rate_Next (Step : Sequencer_Steps);
   procedure Repeat_Rate_Prev (Step : Sequencer_Steps);

   type Note_Mode_Kind is (Note, Chord,
                           Note_In_Chord, Note_In_Scale,
                           Arp);
   package Note_Mode_Kind_Next is new Enum_Next (Note_Mode_Kind);
   use Note_Mode_Kind_Next;

   function Img (M : Note_Mode_Kind) return String
   is (case M is
          when Note          => "Note",
          when Chord         => "Chord",
          when Note_In_Chord => "Note in chord",
          when Note_In_Scale => "Note in scale",
          when Arp           => "Arpeggiator");

   function Note_Mode (Step : Sequencer_Steps) return Note_Mode_Kind;
   procedure Note_Mode_Next (Step : Sequencer_Steps);

   function Note (Step : Sequencer_Steps) return MIDI.MIDI_Key;
   procedure Note_Next (Step : Sequencer_Steps);
   procedure Note_Prev (Step : Sequencer_Steps);

   function Duration (Step : Sequencer_Steps) return Note_Duration;
   procedure Duration_Next (Step : Sequencer_Steps);
   procedure Duration_Prev (Step : Sequencer_Steps);

   function Velo (Step : Sequencer_Steps) return MIDI.MIDI_Data;
   procedure Velo_Next (Step : Sequencer_Steps);
   procedure Velo_Prev (Step : Sequencer_Steps);

   function MIDI_Chan (T : Tracks) return MIDI.MIDI_Channel;
   procedure MIDI_Chan_Next (T : Tracks);
   procedure MIDI_Chan_Prev (T : Tracks);

   type CC_Id is (A, B, C, D);

   function CC_Letter (ID : Sequencer.CC_Id) return String
   is (case ID is
          when A => "A",
          when B => "B",
          when C => "C",
          when D => "D");

   function CC_Controller (T : Tracks; Id : CC_Id) return MIDI.MIDI_Data;
   procedure Set_CC_Controller (T : Tracks; Id : CC_Id; C : MIDI.MIDI_Data);
   procedure CC_Controller_Next (T : Tracks; Id : CC_Id);
   procedure CC_Controller_Prev (T : Tracks; Id : CC_Id);

   subtype Controller_Label is String (1 .. 17);
   Empty_Controller_Label : constant Controller_Label := (others => ' ');

   procedure Set_CC_Controller_Label (T    : Tracks;
                                      Id   : CC_Id;
                                      Label : Controller_Label);
   function CC_Controller_Label (T    : Tracks;
                                 Id   : CC_Id)
                                 return Controller_Label;

   function CC_Enabled (Step : Sequencer_Steps; Id : CC_Id) return Boolean;
   procedure CC_Toggle (Step : Sequencer_Steps; Id : CC_Id);
   function CC_Value (Step : Sequencer_Steps; Id : CC_Id) return MIDI.MIDI_Data;
   procedure CC_Value_Inc (Step : Sequencer_Steps; Id : CC_Id);
   procedure CC_Value_Dec (Step : Sequencer_Steps; Id : CC_Id);

   function Selected_Sample (T : Tracks)
                              return Sample_Library.Valid_Sample_Index;
   procedure Next_Sample (T : Tracks);
   procedure Prev_Sample (T : Tracks);

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
                          Extended,
                          Reserved);

   for Step_Settings'Size use 4;
   for Step_Settings use (Condition   => 0,
                          Note        => 1,
                          Duration    => 2,
                          Velo        => 3,
                          Repeat      => 4,
                          Repeat_Rate => 5,
                          CC_A        => 6,
                          CC_B        => 7,
                          CC_C        => 8,
                          CC_D        => 9,
                          Note_Mode   => 10,
                          Extended    => 14,
                          Reserved    => 15);

   subtype User_Step_Settings is Step_Settings range Condition .. CC_D;

   procedure Next_Value (S : User_Step_Settings);
   procedure Prev_Value (S : User_Step_Settings);
   procedure Next_Value_Fast (S : User_Step_Settings);
   procedure Prev_Value_Fast (S : User_Step_Settings);

   type Track_Setting_Kind is (Volume,
                               Pan,
                               Arp_Mode,
                               Arp_Notes,
                               MIDI_Chan,
                               CC_A_Id,
                               CC_B_Id,
                               CC_C_Id,
                               CC_D_Id,
                               CC_A_Label,
                               CC_B_Label,
                               CC_C_Label,
                               CC_D_Label,
                               Extended,
                               Reserved);

   for Track_Setting_Kind'Size use 4;

   for Track_Setting_Kind use (Volume     => 0,
                               Pan        => 1,
                               Arp_Mode   => 2,
                               Arp_Notes  => 3,
                               MIDI_Chan  => 4,
                               CC_A_Id    => 5,
                               CC_B_Id    => 6,
                               CC_C_Id    => 7,
                               CC_D_Id    => 8,
                               CC_A_Label => 9,
                               CC_B_Label => 10,
                               CC_C_Label => 11,
                               CC_D_Label => 12,
                               Extended   => 14,
                               Reserved   => 15);
private

   type CC_Val_Array is array (CC_Id) of MIDI.MIDI_Data;
   type CC_Ena_Array is array (CC_Id) of Boolean;

   type Step_Rec is record
      Trig        : Trigger;
      Repeat      : WNM.Repeat;
      Repeat_Rate : WNM.Repeat_Rate;

      Note_Mode : Note_Mode_Kind;
      Note      : MIDI.MIDI_Key;
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
      Duration => Quarter,
      Velo => MIDI.MIDI_Data'Last,
      CC_Ena => (others => False),
      CC_Val => (others => 0));

   type Sequence is array (Sequencer_Steps) of Step_Rec with Pack;
   type Pattern is array (Tracks) of Sequence;
   Sequences : array (Patterns) of Pattern :=
     (others => (others => (others => Default_Step)));

   type CC_Setting is record
      Controller : MIDI.MIDI_Data := 0;
      Label      : Controller_Label := "Noname Controller";
   end record;
   type CC_Setting_Array is array (CC_Id) of CC_Setting;

   type Track_Setting is record
      Chan : MIDI.MIDI_Channel := 0;
      CC : CC_Setting_Array;
      Sample : Sample_Library.Valid_Sample_Index := 1;
   end record;

   Track_Settings : array (Tracks) of Track_Setting
     := (others => (Chan => 0,
                    CC => ((0, "Control 0        "),
                           (1, "Control 1        "),
                           (2, "Control 2        "),
                           (3, "Control 3        ")
                          ),
                    Sample => 1
                   )
        );

   Sequencer_BPM : Beat_Per_Minute := 120;

   Current_Playing_Step : Sequencer_Steps := Sequencer_Steps'First with Atomic;

   --  Current_Seq_State : Sequencer_State := Pause with Atomic;
   Current_Track     : Tracks := Tracks'First with Atomic;

   Current_Editing_Pattern : Patterns := Patterns'First;
   Current_Editing_Track : Tracks := Tracks'First;
   Current_Editing_Step : Sequencer_Steps := Sequencer_Steps'First;

end WNM.Sequencer;
