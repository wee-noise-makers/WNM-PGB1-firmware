-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2023 Fabien Chouteau                    --
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

with Tresses;            use Tresses;
with Tresses.Interfaces; use Tresses.Interfaces;

private with Tresses.Envelopes.AR;
private with WNM.Sample_Library;

package WNM.Voices.Sampler_Voice is

   type Instance
   is new Four_Params_Voice
   with private;

   type Sampler_Engine is (Overdrive, Crusher, Glide,
                           Pitch_Down_1oct, Pitch_Down_2oct,
                           Pitch_Up_1oct, Pitch_Up_2oct);

   function Engine (This : Instance) return Sampler_Engine;
   procedure Set_Engine (This : in out Instance; E : Sampler_Engine);

   function Img (E : Sampler_Engine) return String
   is (case E is
          when Overdrive       => "Overdrive",
          when Crusher         => "Crusher",
          when Glide           => "Glide",
          when Pitch_Down_1oct => "Pitch Down (1-Octave)",
          when Pitch_Down_2oct => "Pitch Down (2-Octaves)",
          when Pitch_Up_1oct   => "Pitch Up (1-Octave)",
          when Pitch_Up_2oct   => "Pitch Up (2-Octaves)");

   procedure Set_Sample (This : in out Instance; Id : MIDI.MIDI_Data);

   procedure Init (This : in out Instance);

   procedure Render (This   : in out Instance;
                     Buffer :    out Tresses.Mono_Buffer);

   procedure Set_MIDI_Pitch (This : in out Instance;
                             Key  :        MIDI.MIDI_Key);

   P_Sample  : constant Tresses.Param_Id := 1;
   P_Drive   : constant Tresses.Param_Id := 2;
   P_Start   : constant Tresses.Param_Id := 3;
   P_Release : constant Tresses.Param_Id := 4;

   --  Interfaces --

   overriding
   function Param_Label (This : Instance; Id : Param_Id) return String
   is (case Id is
          when P_Sample  => "Sample",
          when P_Start   => "Start",
          when P_Release => "Release",
          when P_Drive   =>
         (case Engine (This) is
             when Overdrive  => "Drive",
             when Crusher    => "Crush",
             when Glide      => "Glide",
             when Pitch_Down_1oct | Pitch_Down_2oct => "Pitch Down",
             when Pitch_Up_1oct | Pitch_Up_2oct => "Pitch Up"));

   overriding
   function Param_Short_Label (This : Instance; Id : Param_Id)
                               return Short_Label
   is (case Id is
          when P_Sample  => "SMP",
          when P_Start   => "STR",
          when P_Release => "REL",
          when P_Drive   =>
         (case Engine (This) is
             when Overdrive => "DRV",
             when Crusher   => "CRH",
             when Glide     => "GLD",
             when Pitch_Down_1oct .. Pitch_Up_2oct => "PCH"));

   --  Special type to play the recorded sample in sample rec/edit mode
   type Sample_Rec_Playback_Instance
   is tagged private;

   procedure Note_On (This : in out Sample_Rec_Playback_Instance;
                      Key  :        MIDI.MIDI_Key);

   procedure Render (This   : in out Sample_Rec_Playback_Instance;
                     Buffer :    out Tresses.Mono_Buffer);

private

   subtype Sample_Phase is Tresses.U32;
   Phase_Integer_Bits : constant := 17;
   Phase_Frac_Bits : constant := Sample_Phase'Size - Phase_Integer_Bits;

   pragma Compile_Time_Error
     (2**Phase_Integer_Bits < WNM_Configuration.Samples.Points_Per_Sample,
      "Interger part too small for sample point count");

   type Instance
   is new Four_Params_Voice
   with record
      Sample_Id : Sample_Library.Sample_Index :=
        Sample_Library.Sample_Index'First;

      Engine : Sampler_Engine := Sampler_Engine'First;

      Phase : Sample_Phase := 0;
      Phase_Increment : Sample_Phase := 0;
      Start_Phase_Increment  : Sample_Phase := 0;
      Target_Phase_Increment : Sample_Phase := 0;

      Env, Env2 : Tresses.Envelopes.AR.Instance;

      Do_Init : Boolean := True;
   end record;

   type Sample_Rec_Playback_Instance
   is tagged
           record
              On                     : Boolean := False;
              Phase                  : Sample_Phase := 0;
              Phase_Increment        : Sample_Phase := 0;
           end record;
end WNM.Voices.Sampler_Voice;
