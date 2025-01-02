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

with WNM.Coproc;

with HAL;

with Interfaces;

with MIDI;

with Tresses;
with Tresses.LFO;

package WNM.Synth is

   Kick_Channel                : constant MIDI.MIDI_Channel := 1;
   Snare_Channel               : constant MIDI.MIDI_Channel := 2;
   Hihat_Channel               : constant MIDI.MIDI_Channel := 3;
   Bass_Channel                : constant MIDI.MIDI_Channel := 4;
   Lead_Channel                : constant MIDI.MIDI_Channel := 5;
   Chord_Channel               : constant MIDI.MIDI_Channel := 6;
   Sample1_Channel             : constant MIDI.MIDI_Channel := 7;
   Sample2_Channel             : constant MIDI.MIDI_Channel := 8;
   Reverb_Channel              : constant MIDI.MIDI_Channel := 9;
   Drive_Channel               : constant MIDI.MIDI_Channel := 10;
   Bitcrusher_Channel          : constant MIDI.MIDI_Channel := 11;
   Speech_Channel              : constant MIDI.MIDI_Channel := 12;
   Sample_Rec_Playback_Channel : constant MIDI.MIDI_Channel := 13;

   subtype Sampler_Channels
     is MIDI.MIDI_Channel range Sample1_Channel .. Sample2_Channel;

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

   type Voice_Parameters is array (LFO_Compatible_CC) of Tresses.Param_Range;

   FX_Select_Bypass     : constant MIDI.MIDI_Data := 0;
   FX_Select_Overdrive  : constant MIDI.MIDI_Data := 1;
   FX_Select_Reverb     : constant MIDI.MIDI_Data := 2;
   FX_Select_Filter     : constant MIDI.MIDI_Data := 3;
   FX_Select_Bitcrusher : constant MIDI.MIDI_Data := 4;

   function Last_CPU_Load return CPU_Load;
   function Max_CPU_Load return CPU_Load;
   function Missed_Deadlines return HAL.UInt32;

   procedure Clear_Max_CPU_Load;
   procedure Clear_Missed_Deadlines;

   type Sample_Time is new Interfaces.Unsigned_64;

   procedure Push_Copro_Event (Msg : WNM.Coproc.Message);
   procedure Process_Coproc_Events;

   -----------
   -- Synth --
   -----------

   function Lead_Engine_Last return MIDI.MIDI_Data;
   function Lead_Engine_Img (Engine : MIDI.MIDI_Data) return String;
   function Lead_Param_Label (Engine : MIDI.MIDI_Data;
                              Id : Tresses.Param_Id)
                              return String;
   function Lead_Param_Short_Label (Engine : MIDI.MIDI_Data;
                                    Id : Tresses.Param_Id)
                                    return Tresses.Short_Label;

   function Kick_Engine_Last return MIDI.MIDI_Data;
   function Kick_Engine_Img (Engine : MIDI.MIDI_Data) return String;
   function Kick_Param_Label (Id : Tresses.Param_Id)
                              return String;
   function Kick_Param_Short_Label (Id : Tresses.Param_Id)
                                    return Tresses.Short_Label;

   function Snare_Engine_Last return MIDI.MIDI_Data;
   function Snare_Engine_Img (Engine : MIDI.MIDI_Data) return String;
   function Snare_Param_Label (Id : Tresses.Param_Id)
                               return String;
   function Snare_Param_Short_Label (Id : Tresses.Param_Id)
                                     return Tresses.Short_Label;

   function Hihat_Engine_Last return MIDI.MIDI_Data;
   function Hihat_Engine_Img (Engine : MIDI.MIDI_Data) return String;
   function Hihat_Param_Label (Id : Tresses.Param_Id)
                               return String;
   function Hihat_Param_Short_Label (Id : Tresses.Param_Id)
                                     return Tresses.Short_Label;

   function Sampler_Engine_Last return MIDI.MIDI_Data;
   function Sampler_Engine_Img (Engine : MIDI.MIDI_Data) return String;
   function Sampler_Param_Label (Chan : Sampler_Channels;
                                 Id   : Tresses.Param_Id)
                                 return String;
   function Sampler_Param_Short_Label (Chan : Sampler_Channels;
                                       Id   : Tresses.Param_Id)
                                       return Tresses.Short_Label;

   function Reverb_Param_Label (Id : Tresses.Param_Id)
                                return String;
   function Reverb_Param_Short_Label (Id : Tresses.Param_Id)
                                      return Tresses.Short_Label;

   function Drive_Param_Label (Id : Tresses.Param_Id)
                               return String;
   function Drive_Param_Short_Label (Id : Tresses.Param_Id)
                                     return Tresses.Short_Label;

   function Bitcrush_Param_Label (Id : Tresses.Param_Id)
                                  return String;
   function Bitcrush_Param_Short_Label (Id : Tresses.Param_Id)
                                        return Tresses.Short_Label;

   function Speech_Param_Label (Id : Tresses.Param_Id)
                                return String;
   function Speech_Param_Short_Label (Id : Tresses.Param_Id)
                                      return Tresses.Short_Label;

   function Chord_Engine_Last return MIDI.MIDI_Data;
   function Chord_Engine_Img (Engine : MIDI.MIDI_Data) return String;
   function Chord_Param_Label (Id : Tresses.Param_Id)
                               return String;
   function Chord_Param_Short_Label (Id : Tresses.Param_Id)
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
