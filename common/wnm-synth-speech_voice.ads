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

private with LPC_Synth;
private with WNM.Speech;

private package WNM.Synth.Speech_Voice is

   type Instance
   is new Four_Params_Voice
   with private;

   procedure Set_Word (This : in out Instance; Id : MIDI.MIDI_Data);

   procedure Init (This : in out Instance);

   procedure Render (This   : in out Instance;
                     Buffer :    out Tresses.Mono_Buffer);

   procedure Set_MIDI_Pitch (This : in out Instance;
                             Key  :        MIDI.MIDI_Key);

   P_Word : constant Tresses.Param_Id := 1;
   P_Time : constant Tresses.Param_Id := 2;

   --  Interfaces --

   overriding
   function Param_Label (This : Instance; Id : Param_Id) return String
   is (case Id is
          when P_Word => "Word",
          when P_Time => "Time Stretch",
          when 3      => "N/A",
          when 4      => "N/A");

   overriding
   function Param_Short_Label (This : Instance; Id : Param_Id)
                               return Short_Label
   is (case Id is
          when P_Word => "WRD",
          when P_Time => "Time",
          when 3      => "N/A",
          when 4      => "N/A");

private

   type Instance
   is new Four_Params_Voice
   with record
      LPC : LPC_Synth.Instance;
      Selected_Word : WNM.Speech.Word := 0;
      Speech_Pitch : Float := MIDI.Key_To_Frequency (MIDI.C4);
   end record;

end WNM.Synth.Speech_Voice;
