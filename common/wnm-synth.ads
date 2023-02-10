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

with HAL;

with Interfaces;

with WNM_HAL;

with WNM.Time;
with WNM.Sample_Library;
with WNM.MIDI;

with Tresses;
with Tresses.LFO;

package WNM.Synth is

   Speech_Channel  : constant MIDI.MIDI_Channel := 1;
   Sample1_Channel : constant MIDI.MIDI_Channel := 2;
   Sample2_Channel : constant MIDI.MIDI_Channel := 3;
   Kick_Channel    : constant MIDI.MIDI_Channel := 4;
   Snare_Channel   : constant MIDI.MIDI_Channel := 5;
   Cymbal_Channel  : constant MIDI.MIDI_Channel := 6;
   Lead_Channel    : constant MIDI.MIDI_Channel := 7;
   Bass_Channel    : constant MIDI.MIDI_Channel := 8;
   Chord_Channel   : constant MIDI.MIDI_Channel := 9;

   Voice_Param_1_CC      : constant MIDI.MIDI_Data := 0;
   Voice_Param_2_CC      : constant MIDI.MIDI_Data := 1;
   Voice_Param_3_CC      : constant MIDI.MIDI_Data := 2;
   Voice_Param_4_CC      : constant MIDI.MIDI_Data := 3;
   Voice_Volume_CC       : constant MIDI.MIDI_Data := 4;
   Voice_Pan_CC          : constant MIDI.MIDI_Data := 5;
   Voice_Engine_CC       : constant MIDI.MIDI_Data := 6;
   Voice_FX_CC           : constant MIDI.MIDI_Data := 7;
   Voice_LFO_Rate_CC     : constant MIDI.MIDI_Data := 8;
   Voice_LFO_Amp_CC      : constant MIDI.MIDI_Data := 9;
   Voice_LFO_Amp_Mode_CC : constant MIDI.MIDI_Data := 10;
   Voice_LFO_Target_CC   : constant MIDI.MIDI_Data := 11;
   Voice_LFO_Shape_CC    : constant MIDI.MIDI_Data := 12;
   Voice_LFO_Loop_CC     : constant MIDI.MIDI_Data := 13;
   Voice_LFO_Sync_CC     : constant MIDI.MIDI_Data := 14;

   LFO_Amp_Mode_Positive : constant MIDI.MIDI_Data :=
     Tresses.LFO.Positive'Enum_Rep;
   LFO_Amp_Mode_Center   : constant MIDI.MIDI_Data :=
     Tresses.LFO.Center'Enum_Rep;
   LFO_Amp_Mode_Negative : constant MIDI.MIDI_Data :=
     Tresses.LFO.Negative'Enum_Rep;

   subtype LFO_Compatible_CC
     is MIDI.MIDI_Data range Voice_Param_1_CC .. Voice_Pan_CC;

   FX_Select_Bypass    : constant MIDI.MIDI_Data := 0;
   FX_Select_Overdrive : constant MIDI.MIDI_Data := 1;
   FX_Select_Delayline : constant MIDI.MIDI_Data := 2;
   FX_Select_Filter    : constant MIDI.MIDI_Data := 3;

   FX_Settings_Channel  : constant MIDI.MIDI_Channel := 15;
   FX_Drive_Amount_CC   : constant MIDI.MIDI_Data := 0;
   FX_Delay_Time_CC     : constant MIDI.MIDI_Data := 1;
   FX_Delay_Feedback_CC : constant MIDI.MIDI_Data := 2;
   FX_Filter_Mode_CC    : constant MIDI.MIDI_Data := 3;
   FX_Filter_Cutoff_CC  : constant MIDI.MIDI_Data := 4;
   FX_Filter_Reso_CC    : constant MIDI.MIDI_Data := 5;

   function Last_CPU_Load return CPU_Load;
   function Max_CPU_Load return CPU_Load;
   function Missed_Deadlines return HAL.UInt32;

   type Sample_Time is new Interfaces.Unsigned_64;

   function Sample_Clock return Sample_Time;
   --  How many audio samples have been sent to the DAC so far.
   --  This number can be used to count time between two events.

   procedure Next_Points (Output : out WNM_HAL.Stereo_Buffer;
                          Input  :     WNM_HAL.Stereo_Buffer);

   procedure Set_Passthrough (Kind : Audio_Input_Kind);
   function Get_Passthrough return Audio_Input_Kind;

   -----------
   -- Synth --
   -----------

   function Lead_Engine_Img (Engine : MIDI.MIDI_Data) return String;
   function Lead_Param_Label (Engine : MIDI.MIDI_Data;
                              Id : Tresses.Param_Id)
                              return String;
   function Lead_Param_Short_Label (Engine : MIDI.MIDI_Data;
                                    Id : Tresses.Param_Id)
                                    return Tresses.Short_Label;

   function Kick_Param_Label (Id : Tresses.Param_Id)
                              return String;
   function Kick_Param_Short_Label (Id : Tresses.Param_Id)
                                    return Tresses.Short_Label;

   function Snare_Engine_Img (Engine : MIDI.MIDI_Data) return String;
   function Snare_Param_Label (Id : Tresses.Param_Id)
                               return String;
   function Snare_Param_Short_Label (Id : Tresses.Param_Id)
                                     return Tresses.Short_Label;

   function Cymbal_Param_Label (Id : Tresses.Param_Id)
                                return String;
   function Cymbal_Param_Short_Label (Id : Tresses.Param_Id)
                                      return Tresses.Short_Label;

   ---------------
   -- Recording --
   ---------------

   type Rec_Source is (None, Line_In, Master_Output);

   function Now_Recording return Rec_Source;

   procedure Start_Recording (Filename : String;
                              Source   : Rec_Source;
                              Max_Size : Positive)
     with Pre => Now_Recording = None and then Source /= None;

   procedure Stop_Recording
     with Post => Now_Recording = None;

   function Record_Size return Natural;
   --  with Pre => Now_Recording /= None;

end WNM.Synth;
