with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO; use Ada.Text_IO;

with Littlefs; use Littlefs;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with WNM.Sample_Library;

with ASFML_Sim;

with WNM_PS1_HAL_Params;

package body ASFML_SIM_Storage is

   Sample_Data : WNM.Sample_Library.Global_Sample_Array;

   LFS_Block_Size : constant := WNM_PS1_HAL_Params.Storage.Sector_Size;
   LFS_Read_Buffer : Storage_Array (1 .. LFS_Block_Size);
   LFS_Prog_Buffer : Storage_Array (1 .. LFS_Block_Size);
   LFS_Lookahead_Buffer : Storage_Array (1 .. LFS_Block_Size);

   procedure Load_Sample_Data (FD : GNAT.OS_Lib.File_Descriptor);

   type LFS_Config_Access is access all Standard.Littlefs.LFS_Config;

   Config : LFS_Config_Access := null;
   FD : aliased GNAT.OS_Lib.File_Descriptor;

   package FD_Backend is
      function Create (FD : aliased GNAT.OS_Lib.File_Descriptor)
                       return LFS_Config_Access;

   end FD_Backend;

   package body FD_Backend is

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
         FD : GNAT.OS_Lib.File_Descriptor with Address => C.Context;
      begin
         GNAT.OS_Lib.Lseek (FD     => FD,
                            offset => Long_Integer (Offset),
                            origin => GNAT.OS_Lib.Seek_Set);

         if GNAT.OS_Lib.Read (FD, Buffer, Integer (Size)) = Integer (Size) then
            return 0;
         else
            return LFS_ERR_IO;
         end if;
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
         Offset : constant LFS_Offset := Off + C.Block_Size * LFS_Size (Block);
         FD : GNAT.OS_Lib.File_Descriptor with Address => C.Context;
      begin
         GNAT.OS_Lib.Lseek (FD     => FD,
                            offset => Long_Integer (Offset),
                            origin => GNAT.OS_Lib.Seek_Set);

         if GNAT.OS_Lib.Write (FD, Buffer, Integer (Size)) = Integer (Size)
         then
            return 0;
         else
            return LFS_ERR_IO;
         end if;
      end Prog;

      -----------
      -- Erase --
      -----------

      function Erase (C : access constant LFS_Config;
                      Block : LFS_Block)
                      return int
      is
         Offset : constant LFS_Offset := C.Block_Size * LFS_Size (Block);
         FD : GNAT.OS_Lib.File_Descriptor with Address => C.Context;

         Zeros : constant array (1 .. C.Block_Size) of Unsigned_8 :=
           (others => 0);

         Size : constant Integer := Integer (C.Block_Size);
      begin
         GNAT.OS_Lib.Lseek (FD     => FD,
                            offset => Long_Integer (Offset),
                            origin => GNAT.OS_Lib.Seek_Set);

         if GNAT.OS_Lib.Write (FD, Zeros'Address, Size) = Size then
            return 0;
         else
            return LFS_ERR_IO;
         end if;
      end Erase;

      ----------
      -- Sync --
      ----------

      function Sync (C : access constant LFS_Config) return int is
         pragma Unreferenced (C);
      begin
         return 0;
      end Sync;

      ------------
      -- Create --
      ------------

      function Create (FD : aliased GNAT.OS_Lib.File_Descriptor)
                       return LFS_Config_Access
      is
         Ret : constant LFS_Config_Access := new LFS_Config;
      begin
         Ret.Context := FD'Address;
         Ret.Read := Read'Access;
         Ret.Prog := Prog'Access;
         Ret.Erase := Erase'Access;
         Ret.Sync := Sync'Access;
         Ret.Block_Size := LFS_Block_Size;
         Ret.Read_Size := LFS_Block_Size;
         Ret.Prog_Size := LFS_Block_Size;

         Ret.Block_Count := LFS_Block_Size;

         Ret.Block_Cycles := 700;
         Ret.Cache_Size := LFS_Block_Size;
         Ret.Lookahead_Size := LFS_Block_Size;
         Ret.Read_Buffer := LFS_Read_Buffer'Address;
         Ret.Prog_Buffer := LFS_Prog_Buffer'Address;
         Ret.Lookahead_Buffer := LFS_Lookahead_Buffer'Address;
         Ret.Name_Max := 0;
         Ret.File_Max := 0;
         Ret.Attr_Max := 0;
         return Ret;
      end Create;

   end FD_Backend;

   --------------------
   -- Get_LFS_Config --
   --------------------

   function Get_LFS_Config return not null access Littlefs.LFS_Config is
   begin
      return Config;
   end Get_LFS_Config;

   ----------------------
   -- Sample_Data_Base --
   ----------------------

   function Sample_Data_Base return System.Address is
   begin
      return Sample_Data'Address;
   end Sample_Data_Base;

   ----------------------
   -- Load_Sample_Data --
   ----------------------

   procedure Load_Sample_Data (FD : GNAT.OS_Lib.File_Descriptor) is
   begin

      --  The sample data is located after the FS in ROM image

      GNAT.OS_Lib.Lseek
        (FD     => FD,
         offset => Long_Integer (WNM_PS1_HAL_Params.Storage.FS_Size),
         origin => GNAT.OS_Lib.Seek_Set);

      if GNAT.OS_Lib.Read
        (FD, Sample_Data'Address, Sample_Data'Size / 8) /= Sample_Data'Size / 8
      then
         Put_Line ("Cannot load sample data from ROM image");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end Load_Sample_Data;

begin

   if ASFML_Sim.Switch_Storage_Image = null
     or else
       ASFML_Sim.Switch_Storage_Image.all = ""
   then
      --  Create a temp image

      Create_Temp_File (FD, ASFML_Sim.Switch_Storage_Image);

      declare
         function ftruncate (FS : int;
                             Length : Long_Integer)
                             return int;
         pragma Import (C, ftruncate, "ftruncate");

      begin
         if ftruncate (int (FD),
                       Long_Integer
                         (WNM_PS1_HAL_Params.Storage.Total_Storage_Size)) /= 0
         then
            raise Program_Error with "ftruncate error: " &
              GNAT.OS_Lib.Errno_Message;
         end if;
      end;
      Close (FD);
   end if;

   declare
      Image_Path : constant String := ASFML_Sim.Switch_Storage_Image.all;
   begin
      --  The image file should exists and be writable

      if not Is_Regular_File (Image_Path) then
         Put_Line ("Image file '" & Image_Path & "' does not exists");
         GNAT.OS_Lib.OS_Exit (1);
      elsif not Is_Owner_Writable_File (Image_Path) then
         Put_Line ("Image file '" & Image_Path & "' is not writable");
         GNAT.OS_Lib.OS_Exit (1);
      else
         Put_Line ("Open image file '" & Image_Path & "'...");
         FD := Open_Read_Write (Image_Path, Binary);
      end if;

      if FD = Invalid_FD then
         Put_Line ("Cannot open image file '" & Image_Path & "': " &
                     GNAT.OS_Lib.Errno_Message);
         OS_Exit (1);
      end if;

      if File_Length (FD) /= WNM_PS1_HAL_Params.Storage.Total_Storage_Size
      then
         Put_Line ("Invalid size for image file '" & Image_Path & "'");
         OS_Exit (1);
      end if;
   end;

   Load_Sample_Data (FD);

   Config := FD_Backend.Create (FD);

end ASFML_SIM_Storage;
