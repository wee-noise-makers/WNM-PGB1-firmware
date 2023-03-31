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

with WNM.File_System;

package WNM.Project.Storage is

   function Save (Filename :     String;
                  Size     : out File_System.File_Signed_Size)
                  return File_System.Storage_Error;

   function Load (Filename :     String;
                  Size     : out File_System.File_Signed_Size)
                  return File_System.Storage_Error;

   End_Of_Section_Value : constant := 255;

private

   type Token_Kind is (Global_Section,
                       Track_Section,
                       Chord_Section,
                       FX_Section,
                       Step_Section,
                       Sequence_Section,

                       Chord_Chain_Section,
                       Pattern_Chain_Section,

                       Seq_Change_Pattern,
                       Seq_Change_Track,

                       End_Of_File,
                       End_Of_Section);

   for Token_Kind use (Global_Section        => 0,
                       Track_Section         => 1,
                       Chord_Section         => 2,
                       FX_Section            => 3,
                       Step_Section          => 4,
                       Sequence_Section      => 5,
                       Chord_Chain_Section   => 6,
                       Pattern_Chain_Section => 7,
                       Seq_Change_Pattern    => 8,
                       Seq_Change_Track      => 9,
                       End_Of_File           => 10,
                       End_Of_Section        => End_Of_Section_Value);

end WNM.Project.Storage;
