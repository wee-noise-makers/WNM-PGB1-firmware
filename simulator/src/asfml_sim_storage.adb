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
with Wnm_Pgb1_Simulator_Config;

package body ASFML_SIM_Storage is

   Sample_Data : WNM.Sample_Library.Global_Sample_Array;

   procedure Load_Sample_Data (Img : ROM_Builder.From_TOML.RAM_Image);
   procedure Save_Sample_Data (Img : ROM_Builder.From_TOML.RAM_Image);

   Config : ROM_Builder.From_TOML.LFS_Config_Access := null;
   Img : aliased ROM_Builder.From_TOML.RAM_Image;

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

   --------------------------
   -- Write_To_Sample_Data --
   --------------------------

   procedure Write_To_Sample_Data (Id   : WNM_HAL.Sample_Sector_Id;
                                   Data : WNM_HAL.Storage_Sector_Data)
   is
      Addr : constant Integer_Address :=
        To_Integer (Sample_Data'Address) +
        Integer_Address (Id) * WNM_Configuration.Storage.Sector_Byte_Size;

      Dst : WNM_HAL.Storage_Sector_Data
        with Address => To_Address (Addr);
   begin
      Dst := Data;
   end Write_To_Sample_Data;

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
      Config := Img.Create_LFS_Config;

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
      Config := Img.Create_LFS_Config;
      return "";
   end Create_ROM;

   --------------
   -- Save_ROM --
   --------------

   function Save_ROM (Path : String) return String is
   begin
      Save_Sample_Data (Img);
      Img.Write_To_File (Path);
      Img.Write_UF2 (ROM_Dir);

      for Id in WNM.Sample_Library.Sample_Index loop
         ROM_Builder.Sample_Library.Write_UF2_Single
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
              (if Wnm_Pgb1_Simulator_Config.Alire_Host_OS = "windows"
               then Getenv ("HOMEDRIVE").all & Getenv ("HOMEPATH").all
               else Getenv ("HOME").all);

            ROM_Dir : constant String := Home_Dir & "/.config/wnm-ps1/" &
              Wnm_Pgb1_Simulator_Config.Crate_Version & "/";
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
