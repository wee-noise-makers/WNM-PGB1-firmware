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
with WNM.File_System.LEB128_File_In;

private package WNM.Project.Storage.File_In is

   subtype Parent is File_System.LEB128_File_In.Instance;
   type Instance
   is new Parent with
   private;

   generic
      type T is range <>;
   procedure Read_Gen_Int (This : in out Instance; A : out T);

   generic
      type T is mod <>;
   procedure Read_Gen_Mod (This : in out Instance; A : out T);

   generic
      type T is (<>);
   procedure Read_Gen_Enum (This : in out Instance; A : out T);

   procedure Read (This : in out Instance; A : out Token_Kind);

private

   type Instance
   is new Parent with null record;

end WNM.Project.Storage.File_In;
