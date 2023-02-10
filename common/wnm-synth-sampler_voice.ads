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

   --  Interfaces --

   overriding
   function Param_Label (This : Instance; Id : Param_Id) return String
   is (case Id is
          when 1 => "Sample",
          when 2 => "Start",
          when 3 => "End",
          when 4 => "Nothing...");

   overriding
   function Param_Short_Label (This : Instance; Id : Param_Id)
                               return Short_Label
   is (case Id is
          when 1 => "SMP",
          when 2 => "STR",
          when 3 => "END",
          when 4 => "N/A");

private

   type Instance
   is new Four_Params_Voice
   with record
      Sample_Id : Sample_Library.Valid_Sample_Index :=
        Sample_Library.Valid_Sample_Index'First;

      Cursor : Sample_Library.Sample_Point_Count := 0;

      Env : Tresses.Envelopes.AR.Instance;

      Do_Init : Boolean := True;
   end record;

end WNM.Synth.Sampler_Voice;
