with System;
with Littlefs; use Littlefs;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with WNM.Time;

with HAL; use HAL;
with SAM.Port;
with SAM.Device;
with SAM.Functions;
with SAM.Main_Clock;
with SAM.QSPI;
with SAM.CMCC;

with WNM.Samd51;
pragma Unreferenced (WNM.Samd51);
pragma Elaborate (WNM.Samd51);

package body WNM.Storage is

   Block_Size : constant := 4096;
   Page_Size : constant := 256;

   SCK : SAM.Port.GPIO_Point renames SAM.Device.PB10;
   CS : SAM.Port.GPIO_Point renames SAM.Device.PB11;
   IO0   : SAM.Port.GPIO_Point renames SAM.Device.PA08;
   IO1   : SAM.Port.GPIO_Point renames SAM.Device.PA09;
   IO2   : SAM.Port.GPIO_Point renames SAM.Device.PA10;
   IO3   : SAM.Port.GPIO_Point renames SAM.Device.PA11;

   Baud : constant := 120_000_000 / 4_000_000;

   pragma Warnings (Off, "not referenced");
   SFLASH_CMD_READ              : constant := 16#03#;
   SFLASH_CMD_FAST_READ         : constant := 16#0B#;
   SFLASH_CMD_QUAD_READ         : constant := 16#6B#;
   SFLASH_CMD_READ_JEDEC_ID     : constant := 16#9f#;
   SFLASH_CMD_PAGE_PROGRAM      : constant := 16#02#;
   SFLASH_CMD_QUAD_PAGE_PROGRAM : constant := 16#32#;
   SFLASH_CMD_READ_STATUS       : constant := 16#05#;
   SFLASH_CMD_READ_STATUS2      : constant := 16#35#;
   SFLASH_CMD_WRITE_STATUS      : constant := 16#01#;
   SFLASH_CMD_WRITE_STATUS2     : constant := 16#31#;
   SFLASH_CMD_ENABLE_RESET      : constant := 16#66#;
   SFLASH_CMD_RESET             : constant := 16#99#;
   SFLASH_CMD_WRITE_ENABLE      : constant := 16#06#;
   SFLASH_CMD_WRITE_DISABLE     : constant := 16#04#;
   SFLASH_CMD_ERASE_SECTOR      : constant := 16#20#;
   SFLASH_CMD_ERASE_BLOCK       : constant := 16#D8#;
   SFLASH_CMD_ERASE_CHIP        : constant := 16#C7#;
   SFLASH_CMD_4_BYTE_ADDR       : constant := 16#B7#;
   SFLASH_CMD_3_BYTE_ADDR       : constant := 16#E9#;
   pragma Warnings (On, "not referenced");

   procedure Init_QSPI;

   pragma Warnings (Off, "not referenced");
   function Status return UInt8;
   function Status2 return UInt8;
   procedure Write_Enable;
   procedure Write_Disable;
   procedure Wait_Util_Ready;
   pragma Warnings (On, "not referenced");


   Config : aliased LFS_Config;

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
      pragma Unreferenced (C);
      Data : UInt8_Array (1 .. Natural (Size)) with Address => Buffer;
   begin
      SAM.CMCC.Disable_Cache;
      SAM.CMCC.Invalidate_Cache;

      Wait_Util_Ready;
      SAM.QSPI.Read_Memory (UInt32 (Off + Block * Block_Size), Data);

      SAM.CMCC.Enable_Cache;
      return 0;
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
      Data : UInt8_Array (0 .. Natural (Size - 1)) with Address => Buffer;

      First : Natural := Data'First;
      Last : Natural;
   begin
      SAM.CMCC.Disable_Cache;
      SAM.CMCC.Invalidate_Cache;

      --  We have to write page by page
      while First in Data'Range loop
         Last := Natural'Min (First + Page_Size - 1, Data'Last);
         Wait_Util_Ready;
         Write_Enable;
         SAM.QSPI.Write_Memory
           (UInt32 (Off + Block * Block_Size) + UInt32 (First),
            Data (First .. Last));
         --  Write_Disable; -- not needed?

         First := Last + 1;
      end loop;

      SAM.CMCC.Enable_Cache;
      return 0;
   end Prog;

   -----------
   -- Erase --
   -----------

   function Erase (C : access constant LFS_Config;
                   Block : LFS_Block)
                   return int
   is
      pragma Unreferenced (C);
   begin
      SAM.CMCC.Disable_Cache;
      SAM.CMCC.Invalidate_Cache;

      Wait_Util_Ready;
      Write_Enable;
      SAM.QSPI.Erase (SFLASH_CMD_ERASE_SECTOR, UInt32 (Block * Block_Size));
      --  Write_Disable; -- not needed?

      SAM.CMCC.Enable_Cache;
      return 0;
   end Erase;

   ----------
   -- Sync --
   ----------

   function Sync (C : access constant LFS_Config) return int is
      pragma Unreferenced (C);
   begin
      return 0;
   end Sync;

   --------------------
   -- Get_LFS_Config --
   --------------------

   function Get_LFS_Config return not null access Littlefs.LFS_Config is
   begin
      return Config'Access;
   end Get_LFS_Config;

   ------------
   -- Status --
   ------------

   function Status return UInt8 is
      Status : UInt8_Array (1 .. 1);
   begin
      SAM.QSPI.Read (SFLASH_CMD_READ_STATUS, Status);
      return Status (Status'First);
   end Status;

   -------------
   -- Status2 --
   -------------

   function Status2 return UInt8 is
      Status : UInt8_Array (1 .. 1);
   begin
      SAM.CMCC.Disable_Cache;
      SAM.CMCC.Invalidate_Cache;

      SAM.QSPI.Read (SFLASH_CMD_READ_STATUS2, Status);

      SAM.CMCC.Enable_Cache;

      return Status (Status'First);
   end Status2;

   ------------------
   -- Write_Enable --
   ------------------

   procedure Write_Enable is
   begin
      SAM.QSPI.Run (SFLASH_CMD_WRITE_ENABLE);
   end Write_Enable;

   -------------------
   -- Write_Disable --
   -------------------

   procedure Write_Disable is
   begin
      SAM.QSPI.Run (SFLASH_CMD_WRITE_DISABLE);
   end Write_Disable;

   ---------------------
   -- Wait_Util_Ready --
   ---------------------

   procedure Wait_Util_Ready is
   begin
      while (Status and 16#03#) /= 0 loop
         null;
      end loop;
   end Wait_Util_Ready;

   ---------------
   -- Init_QSPI --
   ---------------

   procedure Init_QSPI is
      Jedec_IDs : UInt8_Array (0 .. 2);
   begin
      SAM.Main_Clock.QSPI_On;
      SAM.Main_Clock.QSPI_2X_On;

      SAM.QSPI.Reset;

      SCK.Set_Function (SAM.Functions.PB10_QSPI_SCK);
      CS.Set_Function (SAM.Functions.PB11_QSPI_CS);
      IO0.Set_Function (SAM.Functions.PA08_QSPI_DATA0);
      IO1.Set_Function (SAM.Functions.PA09_QSPI_DATA1);
      IO2.Set_Function (SAM.Functions.PA10_QSPI_DATA2);
      IO3.Set_Function (SAM.Functions.PA11_QSPI_DATA3);

      SAM.QSPI.Configure (Baud);

      SAM.QSPI.Enable;

      SAM.QSPI.Read (SFLASH_CMD_READ_JEDEC_ID, Jedec_IDs);
      if Jedec_IDs (0) /= 16#C8# then
         raise Program_Error;
      end if;

      --  We don't know what state the flash is in so wait for any remaining
      --  writes and then reset.

      --  The write in progress bit should be low.
      while (Status and 16#01#) /= 0 loop
         null;
      end loop;

      --  The suspended write/erase bit should be low.
      while (Status2 and 16#80#) /= 0 loop
         null;
      end loop;

      SAM.QSPI.Run (SFLASH_CMD_ENABLE_RESET);
      SAM.QSPI.Run (SFLASH_CMD_RESET);

      WNM.Time.Delay_Ms (30);

      --  Switch to GD25Q64C QSPI max clock speed (104Mhz) (can it go faster?)
      --  SAM.QSPI.Configure (120_000_000 / 104_000_000);
      SAM.QSPI.Configure (1);

      declare
         St : UInt8_Array (1 .. 1);
      begin
         if (Status2 and 16#02#) = 0 then
            Write_Enable;
            --  GD25Q64C quad_enable_bit_mask
            St (St'First) := 16#02#;
            SAM.QSPI.Write (SFLASH_CMD_WRITE_STATUS2, St);
         end if;
      end;
      Write_Disable;
      Wait_Util_Ready;
   end Init_QSPI;

begin
   Init_QSPI;

   Config.Context := System.Null_Address;
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
