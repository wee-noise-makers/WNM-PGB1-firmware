-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2023 Fabien Chouteau                    --
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
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;

with System.Storage_Elements; use System.Storage_Elements;
with Littlefs; use Littlefs;

with RP.Flash; use RP.Flash;

package body WNM_HAL.File_System is

   Enable_Prog_Check : constant Boolean := False;

   Config : aliased LFS_Config;
   LFS_Block_Size : constant := WNM_Configuration.Storage.Sector_Byte_Size;
   LFS_Read_Buffer : Storage_Array (1 .. LFS_Block_Size);
   LFS_Prog_Buffer : Storage_Array (1 .. LFS_Block_Size);
   LFS_Lookahead_Buffer : Storage_Array (1 .. LFS_Block_Size);

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

   ------------------------------
   -- Block_ID_To_Flash_Offset --
   ------------------------------

   function Block_ID_To_Flash_Offset (Block : LFS_Block)
                                      return RP.Flash.Flash_Offset
   is
   begin
      return Flash_Offset
        (WNM_Configuration.Storage.FS_Offset + Block * LFS_Block_Size);
   end Block_ID_To_Flash_Offset;

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
      pragma Unreferenced (C);

      F_Offset : constant Flash_Offset :=
        Block_ID_To_Flash_Offset (Block) + Flash_Offset (Off);

      Src_Buffer : Storage_Array (1 .. Storage_Count (Size))
        with Address => To_Address (F_Offset);

      Dst_Buffer : Storage_Array (Src_Buffer'Range)
        with Address => Buffer;

   begin
      Dst_Buffer := Src_Buffer;
      return Littlefs.LFS_ERR_OK;
   end Read;

   ----------
   -- Prog --
   ----------

   function Prog (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
   is
      pragma Unreferenced (C);
   begin
      if Off /= 0 then
         raise Program_Error with "LFS Prog: Off /= 0";
      end if;

      WNM_HAL.Wait_Synth_CPU_Hold;

      RP.Flash.Program (Block_ID_To_Flash_Offset (Block),
                        Buffer, Natural (Size));

      WNM_HAL.Release_Synth_CPU_Hold;

      if Enable_Prog_Check then
         declare
            Flash_Data : Storage_Array (0 .. Storage_Count (Size) - 1)
              with Address => To_Address (Block_ID_To_Flash_Offset (Block));

            Src_Data : Storage_Array (Flash_Data'Range)
              with Address => Buffer;
         begin
            for Idx in Flash_Data'Range loop
               if Flash_Data (Idx) /= Src_Data (Idx) then
                  raise Program_Error with
                    "Flash Prog failed at block " & Block'Img &
                    " offset " & Idx'Img &
                    ". Got " & Flash_Data (Idx)'Img &
                    " (expected " & Src_Data (Idx)'Img & ")";
               end if;
            end loop;
         end;
      end if;

      return Littlefs.LFS_ERR_OK;
   end Prog;

   -----------
   -- Erase --
   -----------

   function Erase (C     : access constant LFS_Config;
                   Block : LFS_Block)
                   return int
   is
      pragma Unreferenced (C);
   begin
      WNM_HAL.Wait_Synth_CPU_Hold;

      RP.Flash.Erase (Block_ID_To_Flash_Offset (Block), LFS_Block_Size);

      WNM_HAL.Release_Synth_CPU_Hold;

      return Littlefs.LFS_ERR_OK;
   end Erase;

   ----------
   -- Sync --
   ----------

   function Sync (C : access constant LFS_Config) return int is
      pragma Unreferenced (C);
   begin
      return Littlefs.LFS_ERR_OK;
   end Sync;

   --------------------
   -- Get_LFS_Config --
   --------------------

   function Get_LFS_Config return access Littlefs.LFS_Config is
   begin
      return Config'Access;
   end Get_LFS_Config;

begin
   Config.Context := System.Null_Address;

   Config.Read  := Read'Access;
   Config.Prog  := Prog'Access;
   Config.Erase := Erase'Access;
   Config.Sync  := Sync'Access;

   Config.Block_Size := LFS_Block_Size;
   Config.Read_Size  := LFS_Block_Size;
   Config.Prog_Size  := LFS_Block_Size;

   Config.Block_Count := WNM_Configuration.Storage.FS_Sectors;

   Config.Block_Cycles     := 2000;
   Config.Cache_Size       := LFS_Block_Size;
   Config.Lookahead_Size   := LFS_Block_Size;
   Config.Read_Buffer      := LFS_Read_Buffer'Address;
   Config.Prog_Buffer      := LFS_Prog_Buffer'Address;
   Config.Lookahead_Buffer := LFS_Lookahead_Buffer'Address;

   Config.Name_Max := 0;
   Config.File_Max := 0;
   Config.Attr_Max := 0;

end WNM_HAL.File_System;
