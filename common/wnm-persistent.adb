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

with WNM.File_System.LEB128_File_Out; use WNM.File_System.LEB128_File_Out;
with WNM.File_System.LEB128_File_In; use WNM.File_System.LEB128_File_In;
with WNM.File_System; use WNM.File_System;

package body WNM.Persistent is

   Filename : constant String := "persistent.leb128";

   type Persistent_Token is (P_Last_Project);

   for Persistent_Token use (P_Last_Project => 0);

   ----------
   -- Save --
   ----------

   procedure Save is
      Output : File_System.LEB128_File_Out.Instance :=
        File_System.LEB128_File_Out.Open (Filename);
   begin
      if Output.Status = Ok then
         Output.Push (Out_UInt (P_Last_Project'Enum_Rep));
         Output.Push (Out_UInt (Data.Last_Project));
      end if;

      Output.Close;
   end Save;

   ----------
   -- Load --
   ----------

   procedure Load is
      procedure To_P_Token is new Convert_To_Enum (Persistent_Token);
      procedure Read_Prj is new Read_Gen_Int (Project.Library.Prj_Index);

      Input : LEB128_File_In.Instance := LEB128_File_In.Open (Filename);
      Set : Persistent_Token;
      Raw : In_UInt;
      Success : Boolean;
   begin

      Data := Default;

      if Input.Status /= Ok then
         return;
      end if;

      loop
         Input.Read (Raw);

         exit when Input.Status /= Ok;

         To_P_Token (Raw, Set, Success);

         exit when not Success;

         case Set is
            when P_Last_Project => Read_Prj (Input, Data.Last_Project);
         end case;

         exit when Input.Status /= Ok;
      end loop;
   end Load;

end WNM.Persistent;
