with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO;
with Ada.Directories;

with Littlefs; use Littlefs;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with WNM.Sample_Library;

with WNM_Configuration;

with ASFML_Sim_Resources;
with ROM_Builder.From_TOML;
with ROM_Builder.Sample_Library;
with Wnm_Ps1_Simulator_Config;

package body ASFML_SIM_Storage is

   Sample_Data : WNM.Sample_Library.Global_Sample_Array;

   LFS_Block_Size : constant := WNM_Configuration.Storage.Sector_Byte_Size;
   LFS_Read_Buffer : Storage_Array (1 .. LFS_Block_Size);
   LFS_Prog_Buffer : Storage_Array (1 .. LFS_Block_Size);
   LFS_Lookahead_Buffer : Storage_Array (1 .. LFS_Block_Size);

   procedure Load_Sample_Data (Img : ROM_Builder.From_TOML.RAM_Image);
   procedure Save_Sample_Data (Img : ROM_Builder.From_TOML.RAM_Image);

   type LFS_Config_Access is access all Standard.Littlefs.LFS_Config;

   Config : LFS_Config_Access := null;
   Img : aliased ROM_Builder.From_TOML.RAM_Image;

   package RAM_Image_Backend is
      function Create (Img : aliased ROM_Builder.From_TOML.RAM_Image)
                       return LFS_Config_Access;

   end RAM_Image_Backend;

   package body RAM_Image_Backend is

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
         Ret.Read_Buffer := LFS_Read_Buffer'Address;
         Ret.Prog_Buffer := LFS_Prog_Buffer'Address;
         Ret.Lookahead_Buffer := LFS_Lookahead_Buffer'Address;
         Ret.Name_Max := 0;
         Ret.File_Max := 0;
         Ret.Attr_Max := 0;
         return Ret;
      end Create;

   end RAM_Image_Backend;

   --------------------
   -- Get_LFS_Config --
   --------------------

   function Get_LFS_Config return not null access Littlefs.LFS_Config is
   begin
      return Config;
   end Get_LFS_Config;

   -------------------
   -- Print_FS_Tree --
   -------------------

   procedure Print_FS_Tree is

      FS : aliased Littlefs.LFS_T;

      procedure Tree_Rec (Dir_Path : String) is
         Dir : aliased LFS_Dir;
         Err : int;
         Info : aliased Entry_Info;

      begin
         Ada.Text_IO.Put_Line ("Listing path: '" & Dir_Path & "'");

         Err := Open (FS, Dir, Dir_Path);
         if Err /= LFS_ERR_OK then
            raise Program_Error with "tree: " & Error_Img (Err);
         end if;

         loop
            Err := Read (FS, Dir, Info);

            case Err is
               when LFS_ERR_OK =>
                  exit; --  End of directory
               when int'First .. -1 =>
                  raise Program_Error with "tree: " & Error_Img (Err);
               when 1 .. int'Last  =>

                  declare
                     Name : constant String := Standard.Littlefs.Name (Info);
                  begin
                     if Name /= "." and then Name /= ".." then
                        case Kind (Info) is
                        when Register =>
                           Ada.Text_IO.Put_Line (Name);
                        when Directory =>
                           Ada.Text_IO.Put_Line (Name & "/");
                           Tree_Rec (Dir_Path & Name & "/");
                        end case;
                     end if;
                  end;
            end case;
         end loop;

         Err := Close (FS, Dir);
         if Err /= LFS_ERR_OK then
            raise Program_Error with "tree: " & Error_Img (Err);
         end if;
      end Tree_Rec;

      Err : int;
   begin

      Err := Littlefs.Mount (FS, Get_LFS_Config.all);
      if Err /= 0 then
         raise Program_Error with "Mount error: " & Error_Img (Err);
      end if;

      Tree_Rec ("/");
   end Print_FS_Tree;

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

   procedure Load_Sample_Data (Img : ROM_Builder.From_TOML.RAM_Image) is
      Src : Storage_Array (1 .. Sample_Data'Size / 8)
        with Address => Img.Samples_Addr;

      Dst : Storage_Array (Src'Range)
        with Address => Sample_Data'Address;
   begin
      Dst := Src;
   end Load_Sample_Data;

   ----------------------
   -- Save_Sample_Data --
   ----------------------

   procedure Save_Sample_Data (Img : ROM_Builder.From_TOML.RAM_Image) is
      Dst : Storage_Array (1 .. Sample_Data'Size / 8)
        with Address => Img.Samples_Addr;

      Src : Storage_Array (Dst'Range)
        with Address => Sample_Data'Address;
   begin
      Dst := Src;
   end Save_Sample_Data;

   --------------
   -- Load_ROM --
   --------------

   function Load_ROM (Path              : String;
                      Samples_From_TOML : Boolean)
                      return String
   is
      FD : aliased GNAT.OS_Lib.File_Descriptor;
   begin
      --  The image file should exists and be writable

      if not Is_Regular_File (Path) then
         return "Image file" & ASCII.LF &
           "'" & Path & "'" & ASCII.LF &
           "does not exists";
      elsif not Is_Owner_Writable_File (Path) then
         return "Image file" & ASCII.LF &
           "'" & Path & "'" & ASCII.LF &
           "is not writable";
      else
         Ada.Text_IO.Put_Line ("Open image file '" & Path & "'...");
         FD := Open_Read_Write (Path, Binary);
      end if;

      if FD = Invalid_FD then
         return "Cannot open image file" & ASCII.LF &
           "'" & Path & "':" & ASCII.LF &
           "" &
           GNAT.OS_Lib.Errno_Message;
      end if;

      if File_Length (FD) /= WNM_Configuration.Storage.Total_Storage_Byte_Size
      then
         return "Invalid size for image file" & ASCII.LF &
           "'" & Path & "'" & ASCII.LF &
           "Expected: " &
           WNM_Configuration.Storage.Total_Storage_Byte_Size'Img &
           " Actual: " & File_Length (FD)'Img;
      end if;

      Img.Load_From_File (FD);
      GNAT.OS_Lib.Close (FD);

      if Samples_From_TOML then
         ROM_Builder.From_TOML.Build_From_TOML (Img, TOML_Path,
                                                Format_FS => False);
      end if;

      Load_Sample_Data (Img);
      Config := RAM_Image_Backend.Create (Img);

      return "";
   end Load_ROM;

   ----------------
   -- Create_ROM --
   ----------------

   function Create_ROM return String is
   begin
      Ada.Text_IO.Put_Line ("Create ROM from '" & TOML_Path & "'...");
      ROM_Builder.From_TOML.Build_From_TOML (Img, TOML_Path,
                                             Format_FS => True);
      Load_Sample_Data (Img);
      Config := RAM_Image_Backend.Create (Img);
      return "";
   end Create_ROM;

   --------------
   -- Save_ROM --
   --------------

   function Save_ROM (Path : String) return String is
   begin
      Save_Sample_Data (Img);
      Img.Write_To_File (Path);

      for Id in WNM.Sample_Library.Valid_Sample_Index loop
         ROM_Builder.Sample_Library.Write_UF2_File
           (Id,
            WNM.Sample_Library.Sample_Data.all (Id),
            ROM_Dir);
      end loop;

      return "";
   end Save_ROM;

   --------------
   -- ROM_Path --
   --------------

   function ROM_Path return String is
   begin

      if Switch_Storage_ROM /= null
        and then
          Switch_Storage_ROM.all /= ""
      then
         return Switch_Storage_ROM.all;

      else
         declare
            Home_Dir : constant String :=
              (if Wnm_Ps1_Simulator_Config.Alire_Host_OS = "windows"
               then Getenv ("HOMEDRIVE").all & Getenv ("HOMEPATH").all
               else Getenv ("HOME").all);

            ROM_Dir : constant String := Home_Dir & "/.config/wnm-ps1/" &
              Wnm_Ps1_Simulator_Config.Crate_Version & "/";
         begin
            Ada.Directories.Create_Path (ROM_Dir);

            return ROM_Dir & "user_rom.wnm_rom";
         end;
      end if;
   end ROM_Path;

   -------------
   -- ROM_Dir --
   -------------

   function ROM_Dir return String is
   begin
      return Ada.Directories.Containing_Directory (ROM_Path);
   end ROM_Dir;

   ---------------
   -- TOML_Path --
   ---------------

   function TOML_Path return String is
   begin
      if Switch_Storage_TOML /= null
        and then
          Switch_Storage_TOML.all /= ""
      then
         return Switch_Storage_TOML.all;

      else
         return ASFML_Sim_Resources.Resource_Path & "/rom_desc.toml";
      end if;
   end TOML_Path;

end ASFML_SIM_Storage;
