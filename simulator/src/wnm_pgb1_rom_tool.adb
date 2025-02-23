with ROM_Builder.From_TOML;
with Littlefs;
with Interfaces.C; use Interfaces.C;
with GNAT.OS_Lib;
with WNM_Configuration;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with AAA.Strings;

procedure WNM_PGB1_ROM_TOOL is

   ROM : constant not null ROM_Builder.From_TOML.RAM_Image_Acc
     := new ROM_Builder.From_TOML.RAM_Image;

   LFS_Config : constant ROM_Builder.From_TOML.LFS_Config_Access :=
     ROM.Create_LFS_Config;

   LFS : aliased Littlefs.LFS_T;

   --------------
   -- Tree_Rec --
   --------------

   procedure Tree_Rec (Dir_Path : String; Indent : String := " ") is
      use Littlefs;

      Dir : aliased LFS_Dir;
      Err : int;
      Info : aliased Entry_Info;

   begin
      Put_Line ("Listing path: '" & Dir_Path & "'");

      Err := Open (LFS, Dir, Dir_Path);
      if Err /= LFS_ERR_OK then
         raise Program_Error with "tree: " & Error_Img (Err);
      end if;

      loop
         Err := Read (LFS, Dir, Info);

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
                     Put_Line (Indent & " - '" & Name & "'");
                     case Kind (Info) is
                        when Register =>
                           null;
                        when Directory =>
                           Tree_Rec (Dir_Path & Name & "/", Indent & "'");
                     end case;
                  end if;
               end;
         end case;
      end loop;

      Err := Close (LFS, Dir);
      if Err /= LFS_ERR_OK then
         raise Program_Error with "tree: " & Error_Img (Err);
      end if;
   end Tree_Rec;

   -----------------
   -- Export_File --
   -----------------

   procedure Export_File (LFS_Path, Output_Path : String) is
      use Littlefs;
      use Interfaces;
      use GNAT.OS_Lib;

      File_Buffer : array (1 .. WNM_Configuration.Storage.Sector_Byte_Size)
        of unsigned_char;

      File_Conf : aliased constant Littlefs.lfs_file_config :=
        (Buffer => File_Buffer'Address,
         others => <>);

      File : aliased LFS_File;
      Err : int;

      FD : constant File_Descriptor := Create_File (Output_Path, Binary);

      Buffer : array (1 .. 4096) of unsigned_char;
      Read_Len : LFS_Signed_Size;
      Write_Len : Integer;
   begin

      if FD = Invalid_FD then
         raise Program_Error with "Cannot create file " & Output_Path & " " &
           Errno_Message;
      end if;

      Err := Opencfg (LFS, File, LFS_Path, LFS_O_RDONLY, File_Conf);
      if Err /= LFS_ERR_OK then
         raise Program_Error with "LFS Open: " & Error_Img (Err);
      end if;

      loop
         Read_Len := Read (LFS, File, Buffer'Address, Buffer'Length);

         exit when Read_Len = 0;

         Write_Len := Write (FD, Buffer'Address, Integer (Read_Len));

         if Write_Len /= Integer (Read_Len) then
            raise Program_Error with
              "Cannot write to file " & Output_Path;
         end if;
      end loop;

      Err := Close (LFS, File);
      if Err /= LFS_ERR_OK then
         raise Program_Error with "LFS Close: " & Error_Img (Err);
      end if;

      Close (FD);

   end Export_File;

   -----------------
   -- Extract_All --
   -----------------

   procedure Extract_All is
      use Littlefs;

      Dir : aliased LFS_Dir;
      Err : int;
      Info : aliased Entry_Info;

   begin
      Err := Open (LFS, Dir, "/");

      if Err /= LFS_ERR_OK then
         raise Program_Error with "tree: " & Error_Img (Err);
      end if;

      loop
         Err := Read (LFS, Dir, Info);

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
                           Export_File (Name, Name);
                        when Directory =>
                           raise Program_Error
                             with "Directory not expected...";
                     end case;
                  end if;
               end;
         end case;
      end loop;

      Err := Close (LFS, Dir);
      if Err /= LFS_ERR_OK then
         raise Program_Error with "tree: " & Error_Img (Err);
      end if;
   end Extract_All;

   --------------------
   -- Load_And_Mount --
   --------------------

   procedure Load_And_Mount (Filename : String) is
      use AAA.Strings;
      Err : Interfaces.C.int;
   begin

      if Has_Suffix (Filename, ".uf2")
        or else
         Has_Suffix (Filename, ".UF2")
      then
         ROM.Load_From_UF2 (Filename);

      elsif Has_Suffix (Filename, ".bin")
          or else
            Has_Suffix (Filename, ".BIN")
      then
         ROM.Load_From_Bin
           (Filename,
            Offset_In_File => WNM_Configuration.Storage.Code_Byte_Size);

      else
         raise Program_Error with "Unknown ROM file type: " & Filename;
      end if;

      Err := Littlefs.Mount (LFS, LFS_Config.all);
      if Err /= Littlefs.LFS_ERR_OK then
         raise Program_Error with "LFS Mount: " & Littlefs.Error_Img (Err);
      end if;
   end Load_And_Mount;

   -----------
   -- Usage --
   -----------

   type Command_Kind is (List, Extract, Extract_All, Build);

   function Img (C : Command_Kind) return String
   is (AAA.Strings.To_Lower_Case (C'Img));

   procedure Usage is
   begin
      Put_Line ("Commands:");
      Put_Line (" - " & Img (List) & " ROM_FILE_IN");
      Put_Line (" - " & Img (Extract) & " ROM_FILE_IN FILE_TO_EXTRACT");
      Put_Line (" - " & Img (Extract_All) & "ROM_FILE_IN");
      Put_Line (" - " & Img (Build)  & " TOML_DESC OUTPUT_PREFIX");
      GNAT.OS_Lib.OS_Exit (1);
   end Usage;

   Command : constant String := (if Argument_Count >= 1
                                 then Argument (1)
                                 else "");
   Arg1 : constant String := (if Argument_Count >= 2
                                 then Argument (2)
                                 else "");
   Arg2 : constant String := (if Argument_Count >= 3
                              then Argument (3)
                              else "");

   Cmd : Command_Kind;
begin

   declare
   begin
      Cmd := Command_Kind'Value (Command);
   exception
      when Constraint_Error =>
         Put_Line ("Unknown command: '" & Command & "'");
         Usage;
   end;

   case Cmd is
      when List | Extract | Extract_All =>
         if Arg1 = "" then
            Put_Line ("Missing ROM image (.uf2 or .bin)");
            Usage;
         else
            Load_And_Mount (Arg1);

            case Cmd is
               when List =>
                  Tree_Rec ("/");
               when Extract_All =>
                  Extract_All;
               when Extract =>
                  if Arg2 = "" then
                     Put_Line ("Missing name of file to extract");
                     Usage;
                  else
                     Export_File (Arg2, Arg2);
                  end if;
               when others =>
                  raise Program_Error; -- Unreachable...
            end case;
         end if;

      when Build =>
         if Arg1 = "" then
            Put_Line ("Missing ROM description TOML file");
            Usage;
         end if;

         if Arg2 = "" then
            Put_Line ("Missing UF2 output file prefix");
            Usage;
         end if;

         ROM_Builder.From_TOML.Build_From_TOML
           (ROM.all, Arg1, Format_FS => True);

         ROM.Write_UF2 (Root_Dir => ".",
                        Sample_Lib_Filename => Arg2 & "-sample-pack.uf2",
                        Filesystem_Filename => Arg2 & "-filesystem.uf2");
   end case;

exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Message (E));
      GNAT.OS_Lib.OS_Exit (1);
end WNM_PGB1_ROM_TOOL;
