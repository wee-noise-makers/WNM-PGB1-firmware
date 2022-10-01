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

with HAL;

package WNM.File_System.LEB128_File_Out is

   type Out_UInt is new HAL.UInt32;
   Max_Str_Len_In_Storage : constant := 253;

   type Instance
   is tagged limited
   private;

   function Open (Filename : String) return Instance;

   procedure Close (This : in out Instance);

   function Status (This : Instance) return Storage_Error;

   generic
      type T is (<>);
   procedure Push_Gen (This : in out Instance; A : T);

   procedure Push (This : in out Instance; A : Out_UInt);
   procedure Push (This : in out Instance; A : String);

private

   type Instance
   is tagged limited
           record
              Error : Storage_Error := Ok;
           end record;

end WNM.File_System.LEB128_File_Out;
