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

with System.Storage_Elements;

with WNM.File_System; use WNM.File_System;
with WNM.Utils;

with Flux.Traits.LEB128;

package body WNM.File_System.LEB128_File_In is

   procedure Decode is new Flux.Traits.LEB128.Decode
     (In_UInt, WNM.File_System.Flux_Source);

   ----------
   -- Open --
   ----------

   function Open (Filename : String) return Instance is
   begin
      return Result : Instance do
         case File_System.Open_Read (Filename) is
            when File_System.Ok =>
               Result.Error := Ok;
            when others =>
               Result.Error := Unknown_Error;
         end case;
      end return;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (This : in out Instance) is
   begin
      File_System.Close;
   end Close;

   ------------
   -- Status --
   ------------

   function Status (This : Instance) return File_System.Storage_Error
   is (This.Error);

   ----------------------
   -- Set_Format_Error --
   ----------------------

   procedure Set_Format_Error (This : in out Instance) is
   begin
      This.Error := Format_Error;
   end Set_Format_Error;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Instance; A : out In_UInt) is
      Success : Boolean;
      Source : WNM.File_System.Flux_Source_Instance;
   begin
      if This.Error /= Ok then
         A := In_UInt'Last;
         return;
      end if;

      Decode (Source, A, Success);

      if not Success then
         A := In_UInt'Last;
         This.Error := Unknown_Error;
         return;
      end if;
   end Read;

   ------------------
   -- Read_Gen_Int --
   ------------------

   procedure Read_Gen_Int (This : in out Instance; A : out T) is
      Raw : In_UInt := 0;
   begin

      pragma Compile_Time_Error (T'Last'Enum_Rep > In_UInt'Last,
                                 "Invalid type size for storage input");

      pragma Compile_Time_Error (T'First'Enum_Rep < In_UInt'First,
                                 "Invalid type size for storage input");

      This.Read (Raw);

      if This.Error /= Ok then
         A := T'Last;
         return;
      end if;

      if Raw not in In_UInt (T'First'Enum_Rep) .. In_UInt (T'Last'Enum_Rep)
      then
         A := T'Last;
         This.Error := Unknown_Error;
         return;
      end if;

      declare
      begin
         A := T'Val (Raw);
      exception
         when Constraint_Error =>
            This.Error := Format_Error;
            A := T'Last;
      end;
   end Read_Gen_Int;

   -------------------
   -- Read_Gen_UInt --
   -------------------

   procedure Read_Gen_Mod (This : in out Instance; A : out T) is
      Raw : In_UInt := 0;
   begin

      pragma Compile_Time_Error (T'Last'Enum_Rep > In_UInt'Last,
                                 "Invalid type size for storage input");

      pragma Compile_Time_Error (T'First'Enum_Rep < In_UInt'First,
                                 "Invalid type size for storage input");

      This.Read (Raw);

      if This.Error /= Ok then
         A := T'Last;
         return;
      end if;

      if Raw not in In_UInt (T'First'Enum_Rep) .. In_UInt (T'Last'Enum_Rep)
      then
         A := T'Last;
         This.Error := Unknown_Error;
         return;
      end if;

      declare
      begin
         A := T'Val (Raw);
      exception
         when Constraint_Error =>
            This.Error := Format_Error;
            A := T'Last;
      end;
   end Read_Gen_Mod;

   -------------------
   -- Read_Gen_Enum --
   -------------------

   procedure Read_Gen_Enum (This : in out Instance; A : out T) is
      Raw : In_UInt := 0;
   begin

      pragma Compile_Time_Error (T'Last'Enum_Rep > In_UInt'Last,
                                 "Invalid type size for storage input");

      pragma Compile_Time_Error (T'First'Enum_Rep < In_UInt'First,
                                 "Invalid type size for storage input");

      This.Read (Raw);

      if This.Error /= Ok then
         A := T'Last;
         return;
      end if;

      if Raw not in In_UInt (T'First'Enum_Rep) .. In_UInt (T'Last'Enum_Rep)
      then
         A := T'Last;
         This.Error := Unknown_Error;
         return;
      end if;

      declare
      begin
         A := T'Enum_Val (Raw);
      exception
         when Constraint_Error =>
            This.Error := Format_Error;
            A := T'Last;
      end;
   end Read_Gen_Enum;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Instance; A : out String) is
      use System.Storage_Elements;

      Len : In_UInt := 0;
      Source : WNM.File_System.Flux_Source_Instance;
   begin
      This.Read (Len);

      if Len > File_System.LEB128_File_Out.Max_Str_Len_In_Storage then
         This.Error := Format_Error;
         return;
      end if;

      declare
         Str : String (1 .. Integer (Len));
         Elt : Storage_Element;
         Success : Boolean;
      begin
         for C of Str loop
            File_System.Read (Source, Elt, Success);

            if not Success then
               This.Error := Format_Error;
               return;
            else
               C := Character'Enum_Val (Elt);
            end if;
         end loop;

         Utils.Copy_Str (Str, A);
      end;
   end Read;

   ---------------------
   -- Convert_To_Enum --
   ---------------------

   procedure Convert_To_Enum (Raw     :     In_UInt;
                              V       : out T;
                              Success : out Boolean)
   is
   begin
      V := T'Enum_Val (Raw);
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end Convert_To_Enum;

end WNM.File_System.LEB128_File_In;
