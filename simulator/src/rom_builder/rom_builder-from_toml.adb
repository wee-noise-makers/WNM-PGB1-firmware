with Ada.Unchecked_Deallocation;
with Ada.Directories;
with Interfaces;
with Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;

with ROM_Builder.Sample_Library;
with ROM_Builder.File_System;

with TOML; use TOML;
with TOML.File_IO;

with UF2_Utils.File_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body ROM_Builder.From_TOML is

   ----------
   -- Free --
   ----------

   procedure Free (X : in out RAM_Image_Acc) is
      procedure Free_Img
      is new Ada.Unchecked_Deallocation (RAM_Image, RAM_Image_Acc);
   begin
      Free_Img (X);
   end Free;

   -------------
   -- FS_Addr --
   -------------

   function FS_Addr (Img : RAM_Image) return System.Address is
   begin
      return Img.Data (WNM_Configuration.Storage.FS_Base_Addr)'Address;
   end FS_Addr;

   ------------------
   -- Samples_Addr --
   ------------------

   function Samples_Addr (Img : RAM_Image) return System.Address is
   begin
      return Img.Data
        (WNM_Configuration.Storage.Sample_Library_Base_Addr)'Address;
   end Samples_Addr;

   --------------------
   -- Load_From_File --
   --------------------

   procedure Load_From_File (Img : in out RAM_Image;
                             FD : GNAT.OS_Lib.File_Descriptor)
   is
   begin
      if GNAT.OS_Lib.Read
        (FD, Img.Data'Address, Img.Data'Length) /= Img.Data'Length
      then
         raise Program_Error;
      end if;
   end Load_From_File;

   -------------------
   -- Load_From_UF2 --
   -------------------

   procedure Load_From_UF2 (Img : in out RAM_Image;
                            Filename : String)
   is
      use UF2_Utils;
      use Interfaces;

      FD : constant File_Descriptor := Open_Read (Filename, Binary);
      Block : aliased UF2_Utils.UF2_Block;
      RD_Cnt : Integer;

      Loaded_Bytes_Count : Natural := 0;
   begin

      if FD = Invalid_FD then
         raise Program_Error with Errno_Message;
      end if;

      loop
         RD_Cnt := Read (FD, Block'Address, Block'Size / 8);
         case RD_Cnt is
            when 0   => exit;
            when 512 => null;
            when others =>
               raise Program_Error with "Errno: " & Errno_Message;
         end case;

         if Block.Magic_Start_0 /= 16#0A324655# then
            raise Program_Error with "Invalid magic start 0 in " & Filename;
         elsif Block.Magic_Start_1 /= 16#9E5D5157# then
            raise Program_Error with "Invalid magic start 1 in " & Filename;
         elsif Block.Magic_End /= 16#0AB16F30# then
            raise Program_Error
              with "Invalid magic end in " & Filename & " -" &
              Block.Magic_End'Img;
         elsif Storage_Offset (Block.Target_Address) not in ROM_Addr_Range then
            --  Put_Line ("Block Addr (" &
            --              Block.Target_Address'Img & ") not in ROM range");
            null;
         else

            declare
               Offset : constant Unsigned_32 := Block.Target_Address;
            begin

               --  Put_Line ("Loading UF2 block from " & Filename & " at" &
               --              Block.Target_Address'Img);
               for Idx in 0 .. Block.Payload_Size - 1 loop
                  Img.Data (Storage_Offset (Offset + Idx)) :=
                    Block.Data (UF2_Payload_Count (Idx));
               end loop;
               Loaded_Bytes_Count := @ + Natural (Block.Payload_Size);
            end;
         end if;

      end loop;

      Close (FD);

      Put_Line (Loaded_Bytes_Count'Img & " bytes loaded from " & Filename);
   end Load_From_UF2;

   ----------------
   -- Open_Image --
   ----------------

   function Open_Image (Path_To_Output : String) return File_Descriptor is
      FD : File_Descriptor;
   begin
      if not Is_Regular_File (Path_To_Output) then

         --  The file doesn't exists, we try to create it
         FD := Create_File (Path_To_Output, Binary);

      elsif not GNAT.OS_Lib.Is_Owner_Writable_File (Path_To_Output) then

         raise Program_Error
           with "Image file '" & Path_To_Output & "' is not writable";

      else
         FD := Open_Read_Write (Path_To_Output, Binary);
      end if;

      if FD = Invalid_FD then
         raise Program_Error
           with "Cannot open image file '" & Path_To_Output & "'";
      end if;

      return FD;
   end Open_Image;

   ------------------
   -- Process_TOML --
   ------------------

   procedure Process_TOML (Root      :        TOML_Value;
                           TOML_Dir  :        String;
                           Img       : in out RAM_Image;
                           Format_FS :        Boolean)
   is
      Lib : constant Sample_Library.Acc_All := new Sample_Library.Instance;
      FS  : constant File_System.Acc_All := new File_System.Instance;
   begin
      if Root.Kind /= TOML_Table then
         raise Program_Error with "Invalid TOML file. Table expected";
      end if;

      if Format_FS then
         FS.Initialize;
      else
         FS.Initialize_From (Img);
      end if;

      Lib.Load_From_TOML (Root, TOML_Dir);

      FS.Load_From_TOML (Root, TOML_Dir);

      FS.Print_Tree;

      FS.Write_Data (Img);
      Lib.Write_Data (Img);

      Img.Close;
   end Process_TOML;

   ---------------------
   -- Build_From_TOML --
   ---------------------

   procedure Build_From_TOML (Img          : in out RAM_Image;
                              Path_To_TOML :        String;
                              Format_FS    :        Boolean)
   is
      Result : constant Read_Result := File_IO.Load_File (Path_To_TOML);

      TOML_Dir : constant String :=
        Ada.Directories.Containing_Directory (Path_To_TOML);
   begin
      if Result.Success then
         Process_TOML (Result.Value, TOML_Dir, Img, Format_FS);
      else
         raise Program_Error with Path_To_TOML & ":" & Format_Error (Result);
      end if;
   end Build_From_TOML;

   -------------------
   -- Write_To_File --
   -------------------

   procedure Write_To_File (Img : RAM_Image; Path_To_Output : String) is
      FD : File_Descriptor;
   begin
      FD := Open_Image (Path_To_Output);

      if GNAT.OS_Lib.Write
        (FD, Img.Data'Address, Img.Data'Length) /= Img.Data'Length
      then
         raise Program_Error;
      end if;

      Close (FD);
   end Write_To_File;

   ---------------
   -- Write_UF2 --
   ---------------

   procedure Write_UF2 (Img : RAM_Image;
                        Root_Dir : String;
                        Sample_Lib_Filename : String := "sample_library.uf2";
                        Filesystem_Filename : String := "file_system.uf2")
   is
      use WNM_Configuration.Storage;
      use UF2_Utils.File_IO;

      Sample_Lib_Data : Storage_Array (1 .. Sample_Library_Byte_Size)
        with Address => Img.Data (Sample_Library_Base_Addr)'Address;

      FS_Data : Storage_Array (1 .. FS_Byte_Size)
        with Address => Img.Data (FS_Base_Addr)'Address;

      Sample_Lib_File : UF2_Sequential_IO.File_Type;
      FS_File : UF2_Sequential_IO.File_Type;
   begin
      UF2_Sequential_IO.Create (Sample_Lib_File,
                                Name => Root_Dir & "/" & Sample_Lib_Filename);

      Write_UF2
        (Data => Sample_Lib_Data,
         Start_Address => Sample_Library_Base_Addr,
         File => Sample_Lib_File,
         Max_Block_Size => 256,
         Flags  => 16#00002000#,
         Family => UF2_Family);

      UF2_Sequential_IO.Close (Sample_Lib_File);

      UF2_Sequential_IO.Create (FS_File,
                                Name => Root_Dir & "/" & Filesystem_Filename);

      Write_UF2
        (Data => FS_Data,
         Start_Address => FS_Base_Addr,
         File => FS_File,
         Max_Block_Size => 256,
         Flags  => 16#00002000#,
         Family => UF2_Family);

      UF2_Sequential_IO.Close (FS_File);

   end Write_UF2;

   -----------
   -- Write --
   -----------

   overriding
   function Write (This : in out RAM_Image;
                   Addr :        System.Address;
                   Len  :        Natural)
                   return Natural
   is
      Src : Storage_Array (1 .. Storage_Offset (Len))
        with Address => Addr;
   begin
      This.Data
        (This.Next_In .. This.Next_In + Storage_Count (Len - 1)) := Src;
      This.Next_In := This.Next_In + Storage_Count (Len);
      return Len;
   end Write;

   ----------
   -- Read --
   ----------

   overriding
   function Read (This : in out RAM_Image;
                  Addr :        System.Address;
                  Len  :        Natural)
                  return Natural
   is
      Dst : Storage_Array (1 .. Storage_Offset (Len))
        with Address => Addr;
   begin
      Dst := This.Data
        (This.Next_Out .. This.Next_Out + Storage_Count (Len - 1));
      This.Next_Out := This.Next_Out + Storage_Count (Len);
      return Len;
   end Read;

   package RAM_Image_Backend is
      function Create (Img : aliased ROM_Builder.From_TOML.RAM_Image)
                       return LFS_Config_Access;

   end RAM_Image_Backend;

   package body RAM_Image_Backend is
      use Littlefs;
      use Interfaces;
      use Interfaces.C;

      pragma Warnings (Off, "lower bound test");

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

         Dst : Storage_Array (1 .. Storage_Offset (Size))
           with Address => Buffer;

         Src : Storage_Array (1 .. WNM_Configuration.Storage.FS_Byte_Size)
           with Address => C.Context;

      begin
         if Block not in 0 .. WNM_Configuration.Storage.FS_Sectors - 1 then
            raise Program_Error with "Invalid block to read: " & Block'Img;
         end if;

         Dst := Src (Src'First + Storage_Count (Offset) ..
                       Src'First + Storage_Count (Offset + Size - 1));
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
         Offset : constant LFS_Offset := Off + C.Block_Size * LFS_Size (Block);
         Src : Storage_Array (1 .. Storage_Offset (Size))
           with Address => Buffer;

         Dst : Storage_Array (1 .. WNM_Configuration.Storage.FS_Byte_Size)
           with Address => C.Context;

      begin

         if Off /= 0 then
            raise Program_Error;
         end if;

         if Block not in 0 .. WNM_Configuration.Storage.FS_Sectors - 1 then
            raise Program_Error with "Invalid block to program: " & Block'Img;
         end if;

         Dst (Dst'First + Storage_Count (Offset) ..
                Dst'First + Storage_Count (Offset + Size - 1)) := Src;
         return 0;
      end Prog;

      -----------
      -- Erase --
      -----------

      function Erase (C : access constant LFS_Config;
                      Block : LFS_Block)
                      return int
      is
         Size : constant LFS_Size := C.Block_Size;
         Offset : constant LFS_Offset := C.Block_Size * LFS_Size (Block);
         Src : constant Storage_Array (1 .. Storage_Offset (Size)) :=
           (others => 16#FF#);

         Dst : Storage_Array (1 .. WNM_Configuration.Storage.FS_Byte_Size)
           with Address => C.Context;

         First : constant Storage_Offset :=
           Dst'First + Storage_Count (Offset);
         Last : constant Storage_Offset :=
           Dst'First + Storage_Count (Offset + Size - 1);
      begin
         if Block not in 0 .. WNM_Configuration.Storage.FS_Sectors - 1 then
            raise Program_Error with "Invalid block to erase: " & Block'Img;
         end if;
         Dst (First .. Last) := Src;
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

      ------------
      -- Create --
      ------------

      function Create (Img : aliased ROM_Builder.From_TOML.RAM_Image)
                       return LFS_Config_Access
      is
         Ret : constant LFS_Config_Access := new LFS_Config;

         LFS_Block_Size : constant :=
           WNM_Configuration.Storage.Sector_Byte_Size;

         type Buffer is new Storage_Array (1 .. LFS_Block_Size);
         type SA_Access is access all Buffer;
         LFS_Read_Buffer : constant not null SA_Access := new Buffer;
         LFS_Prog_Buffer : constant not null SA_Access := new Buffer;
         LFS_Lookahead_Buffer : constant not null SA_Access := new Buffer;

      begin
         Ret.Context := Img.FS_Addr;
         Ret.Read := Read'Access;
         Ret.Prog := Prog'Access;
         Ret.Erase := Erase'Access;
         Ret.Sync := Sync'Access;
         Ret.Block_Size := LFS_Block_Size;
         Ret.Read_Size := LFS_Block_Size;
         Ret.Prog_Size := LFS_Block_Size;

         Ret.Block_Count := WNM_Configuration.Storage.FS_Sectors;

         Ret.Block_Cycles := 700;
         Ret.Cache_Size := LFS_Block_Size;
         Ret.Lookahead_Size := LFS_Block_Size;
         Ret.Read_Buffer := LFS_Read_Buffer.all'Address;
         Ret.Prog_Buffer := LFS_Prog_Buffer.all'Address;
         Ret.Lookahead_Buffer := LFS_Lookahead_Buffer.all'Address;
         Ret.Name_Max := 0;
         Ret.File_Max := 0;
         Ret.Attr_Max := 0;
         return Ret;
      end Create;

   end RAM_Image_Backend;

   -----------------------
   -- Create_LFS_Config --
   -----------------------

   function Create_LFS_Config (Img : aliased RAM_Image)
                               return LFS_Config_Access
   is (RAM_Image_Backend.Create (Img));

end ROM_Builder.From_TOML;
