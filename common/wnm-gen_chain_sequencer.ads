-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2020 Fabien Chouteau                  --
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

--  This package provides a common implementation of chain sequencing that
--  will be used for patterns as well as chords (maybe more in the future).

with WNM.File_System.LEB128_File_Out;
with WNM.File_System.LEB128_File_In;

generic
   Max_Patterns_In_Sequence : Natural := 30;
package WNM.Gen_Chain_Sequencer is

   procedure Start_Recording;
   procedure End_Recording;

   procedure Play_Pause;
   function Playing return Boolean;

   procedure On_Press (Button : Keyboard_Button);

   procedure On_Release (Button : Keyboard_Button);

   procedure Single_Play (K : Keyboard_Value);

   function Playing return Keyboard_Value;
   function Is_In_Sequence (K : Keyboard_Value) return Boolean;

   procedure Signal_End_Of_Pattern;
   procedure Signal_Mid_Pattern;

   procedure Save
     (Output : in out File_System.LEB128_File_Out.Instance'Class);

   procedure Load
     (Input : in out File_System.LEB128_File_In.Instance'Class);

end WNM.Gen_Chain_Sequencer;
