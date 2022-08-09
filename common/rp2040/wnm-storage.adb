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

with System;
with Littlefs; use Littlefs;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with WNM.Time;

with HAL; use HAL;

with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;

package body WNM.Storage is

   Block_Size : constant := 4096;
   Page_Size : constant := 256;

   Config : aliased LFS_Config;

   FS_Image_Start : Integer;
   pragma Import (ASM, FS_Image_Start, "_fs_image_start");

   FS_Image_End : Integer;
   pragma Import (ASM, FS_Image_End, "_fs_image_end");

   function Read (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
     with Convention => C;

   function Prog (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
     with Convention => C;
   function Erase (C     : access constant LFS_Config;
                   Block : LFS_Block)
                   return int
     with Convention => C;
   function Sync (C : access constant LFS_Config) return int
     with Convention => C;

   ----------
   -- Read --
   ----------

   function Read (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
   is
      Offset : constant LFS_Offset := Off + C.Block_Size * LFS_Size (Block);

      Dev_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => To_Address
          (To_Integer (C.Context) + Integer_Address (Offset));

      Read_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => Buffer;
   begin
      Read_Buf := Dev_Buf;
      return 0;
   end Read;

   function Prog (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
   is
      Offset : constant LFS_Offset := Off + C.Block_Size * LFS_Size (Block);

      Dev_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => To_Address
          (To_Integer (C.Context) + Integer_Address (Offset));

      Read_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => Buffer;
   begin
      Dev_Buf := Read_Buf;
      return 0;
   end Prog;

   function Erase (C : access constant LFS_Config;
                   Block : LFS_Block)
                   return int
   is
      pragma Unreferenced (Block, C);
   begin
      return 0;
   end Erase;

   function Sync (C : access constant LFS_Config) return int is
      pragma Unreferenced (C);
   begin
      return 0;
   end Sync;

   type LFS_Config_Access is access all Littlefs.LFS_Config;
   type Storage_Array_Access is access all Storage_Array;

   --------------------
   -- Get_LFS_Config --
   --------------------

   function Get_LFS_Config return not null access Littlefs.LFS_Config is
   begin
      return Config'Access;
   end Get_LFS_Config;

begin
   Config.Context := FS_Image_Start'Address;
   Config.Read := Read'Access;
   Config.Prog := Prog'Access;
   Config.Erase := Erase'Access;
   Config.Sync := Sync'Access;
   Config.Read_Size := Block_Size;
   Config.Prog_Size := Block_Size;
   Config.Block_Size := Block_Size;
   Config.Block_Count := Size / Block_Size;
   Config.Block_Cycles := 700;
   Config.Cache_Size := Config.Block_Size;
   Config.Lookahead_Size := Config.Block_Size;
   Config.Read_Buffer := System.Null_Address;
   Config.Prog_Buffer := System.Null_Address;
   Config.Lookahead_Buffer := System.Null_Address;
   Config.Name_Max := 0;
   Config.File_Max := 0;
   Config.Attr_Max := 0;

end WNM.Storage;
