-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with HAL; use HAL;

private with WNM.File_System;

package WNM.Projects is

   type File_Out (Buffer_Size : Positive := 2048)
   is tagged limited private;

   procedure Start_Pattern (This : in out File_Out; P : Patterns);
   procedure Start_Track (This : in out File_Out; T : Tracks);
   procedure Start_Sequence (This : in out File_Out);
   procedure Start_Step (This : in out File_Out; S : Sequencer_Steps);
   procedure Change_Pattern (This : in out File_Out; P : Patterns);
   procedure Change_Track (This : in out File_Out; T : Tracks);
   procedure End_Section (This : in out File_Out);

   procedure Push (This : in out File_Out; A : UInt8);
   procedure Push (This : in out File_Out; A : Character);
   procedure Push (This : in out File_Out; A, B : UInt4);

   procedure Save;

   procedure Load;

private

   type File_Out (Buffer_Size : Positive := 2048)
   is tagged limited record
      FD : WNM.File_System.File_Descriptor;
      Count : Natural := 0;
      Buffer : UInt8_Array (1 .. Buffer_Size);
   end record;

   procedure Open (This : in out File_Out; Filename : String);
   procedure Flush (This : in out File_Out);
   procedure Close (This : in out File_Out);


end WNM.Projects;
