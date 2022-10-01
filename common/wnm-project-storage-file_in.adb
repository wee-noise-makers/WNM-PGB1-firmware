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

with WNM.File_System; use WNM.File_System;

package body WNM.Project.Storage.File_In is

   ------------------
   -- Read_Gen_Int --
   ------------------

   procedure Read_Gen_Int (This : in out Instance; A : out T) is
      procedure Read  is new LEB128_File_In.Read_Gen_Int (T);
   begin
      Read (Parent (This), A);
   end Read_Gen_Int;

   -------------------
   -- Read_Gen_UInt --
   -------------------

   procedure Read_Gen_Mod (This : in out Instance; A : out T) is
      procedure Read  is new LEB128_File_In.Read_Gen_Mod (T);
   begin
      Read (Parent (This), A);
   end Read_Gen_Mod;

   -------------------
   -- Read_Gen_Enum --
   -------------------

   procedure Read_Gen_Enum (This : in out Instance; A : out T) is
      procedure Read  is new LEB128_File_In.Read_Gen_Enum (T);
   begin
      Read (Parent (This), A);
   end Read_Gen_Enum;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Instance; A : out Token_Kind) is
      procedure Read_G is new Read_Gen_Enum (Token_Kind);
   begin
      Read_G (This, A);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Instance; A : out Keyboard_Value) is
      procedure Read_G is new Read_Gen_Int (Keyboard_Value);
   begin
      Read_G (This, A);
   end Read;

end WNM.Project.Storage.File_In;
