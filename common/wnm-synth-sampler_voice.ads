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

private package WNM.Synth.Sampler_Voice is

   type Instance
   is new Four_Params_Voice
   with private;

   procedure Set_Sample (This : in out Instance; Id : MIDI.MIDI_Data);

   procedure Init (This : in out Instance);

   procedure Render (This   : in out Instance;
                     Buffer :    out Tresses.Mono_Buffer);

   procedure Set_MIDI_Pitch (This : in out Instance;
                             Key  :        MIDI.MIDI_Key);

   P_Sample  : constant Tresses.Param_Id := 1;
   P_Start   : constant Tresses.Param_Id := 2;
   P_Release : constant Tresses.Param_Id := 3;
   P_Drive   : constant Tresses.Param_Id := 4;

   --  Interfaces --

   overriding
   function Param_Label (This : Instance; Id : Param_Id) return String
   is (case Id is
          when P_Sample  => "Sample",
          when P_Start   => "Start",
          when P_Release => "Release",
          when P_Drive   => "Drive");

   overriding
   function Param_Short_Label (This : Instance; Id : Param_Id)
                               return Short_Label
   is (case Id is
          when P_Sample  => "SMP",
          when P_Start   => "STR",
          when P_Release => "REL",
          when P_Drive   => "DRV");

private

   type Sample_Pitch
     is delta 0.00001
     range 0.0 .. Sample_Library.Single_Sample_Point_Cnt * 1.0 + 64.0;

   type Instance
   is new Four_Params_Voice
   with record
      Sample_Id : Sample_Library.Valid_Sample_Index :=
        Sample_Library.Valid_Sample_Index'First;

      Cursor : Sample_Pitch := 1.0;
      S_Pitch : Sample_Pitch := 1.0;

      Env : Tresses.Envelopes.AR.Instance;

      Do_Init : Boolean := True;
   end record;

end WNM.Synth.Sampler_Voice;
