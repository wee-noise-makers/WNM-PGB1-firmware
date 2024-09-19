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

with System;
with Littlefs;
with Interfaces;
with Interfaces.C;

with Flux.Traits.LEB128;

package body WNM.File_System.LEB128_File_Out is

   procedure Encode is new Flux.Traits.LEB128.Encode (Out_UInt,
                                                      File_System.Flux_Sink);

   Sink : File_System.Flux_Sink_Instance;

   function Write (Addr   : System.Address;
                   Len    : File_System.File_Size)
                   return Storage_Error;

   -----------
   -- Write --
   -----------

   function Write (Addr : System.Address;
                   Len  : File_System.File_Size)
                   return Storage_Error
   is
      use Littlefs;
      use Interfaces;

      Res : File_Signed_Size;
   begin
      Res := File_System.Write (Addr, Len);

      if Res < 0 then
         case Interfaces.C.int (Res) is
            when LFS_ERR_IO | LFS_ERR_CORRUPT | LFS_ERR_NOENT |
                 LFS_ERR_EXIST | LFS_ERR_NOTDIR | LFS_ERR_ISDIR |
                 LFS_ERR_NOTEMPTY | LFS_ERR_BADF | LFS_ERR_FBIG |
                 LFS_ERR_INVAL | LFS_ERR_NOMEM |
                 LFS_ERR_NOATTR | LFS_ERR_NAMETOOLONG =>

               return Disk_Error;

            when LFS_ERR_NOSPC =>

               return Out_Of_Space;

            when others =>
               return Unknown_Error;
         end case;

      elsif File_Size (Res) /= Len then
         return Unknown_Error;
      else
         return Ok;
      end if;
   end Write;

   ----------
   -- Open --
   ----------

   procedure Open (This : in out Instance; Filename : String) is
   begin
      case File_System.Open_Write (Filename) is
      when File_System.Ok =>
         This.Error := Ok;
      when others =>
         This.Error := Unknown_Error;
      end case;
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

   function Status (This : Instance) return Storage_Error
   is (This.Error);

   --------------
   -- Push_Gen --
   --------------

   procedure Push_Gen (This : in out Instance; A : T) is
      Success : Boolean;
   begin
      if This.Status /= Ok then
         return;
      end if;

      pragma Compile_Time_Error (T'Last'Enum_Rep > Out_UInt'Last,
                                 "Invalid type size for storage output");

      pragma Compile_Time_Error (T'First'Enum_Rep < Out_UInt'First,
                                 "Invalid type size for storage output");

      --  pragma Compile_Time_Error (T'Last'Enum_Rep >= End_Of_Section_Value,
      --                             "Invalid type size for storage output");

      Encode (Sink, Out_UInt (A'Enum_Rep), Success);

      if not Success then
         This.Error := Unknown_Error;
      else
         This.Error := Ok;
      end if;

   end Push_Gen;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Out_UInt) is
      procedure Push_G is new Push_Gen (Out_UInt);
   begin
      Push_G (This, A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : String) is
   begin
      if A'Length >= Max_Str_Len_In_Storage then
         raise Program_Error with "String too long for storage";
      end if;
      This.Push (Out_UInt (A'Length));
      This.Error := Write (A'Address, A'Length);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Boolean) is
   begin
      Instance'Class (This).Push (Out_UInt (if A then 1 else 0));
   end Push;

end WNM.File_System.LEB128_File_Out;
