with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Deallocation;
with Ada.Directories;

with ROM_Builder.Sample_Library;
with ROM_Builder.File_System;

with FSmaker.Source.Text_Buffer;

with TOML; use TOML;
with TOML.File_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Simple_Logging;

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
      return Img.Data (Img.Data'First)'Address;
   end FS_Addr;

   ------------------
   -- Samples_Addr --
   ------------------

   function Samples_Addr (Img : RAM_Image) return System.Address is
      Offset : constant Storage_Offset :=
        Img.Data'First + WNM_Configuration.Storage.FS_Byte_Size;
   begin
      return Img.Data (Offset)'Address;
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

   procedure Process_TOML (Root     :        TOML_Value;
                           TOML_Dir :        String;
                           Img      : in out RAM_Image)
   is
      Lib : constant Sample_Library.Acc_All := new Sample_Library.Instance;
      FS  : constant File_System.Acc_All := new File_System.Instance;
   begin
      if Root.Kind /= TOML_Table then
         raise Program_Error with "Invalid TOML file. Table expected";
      end if;

      FS.Initialize;

      Lib.Load_From_TOML (Root, TOML_Dir);

      declare
         TB : FSmaker.Source.Text_Buffer.Instance;
      begin
         Simple_Logging.Always ("Building sample entries file");
         Lib.Write_Entry_Info (TB);
         Simple_Logging.Always ("Writting sample entries file");
         FS.Import ("/sample_entries.txt", TB);
      end;

      FS.Print_Tree;

      FS.Write_Data (Img);
      Lib.Write_Data (Img);

      Img.Close;

   end Process_TOML;

   ---------------------
   -- Build_From_TOML --
   ---------------------

   procedure Build_From_TOML (Img : in out RAM_Image;
                              Path_To_TOML : String)
   is
      Result : constant Read_Result := File_IO.Load_File (Path_To_TOML);

      TOML_Dir : constant String :=
        Ada.Directories.Containing_Directory (Path_To_TOML);
   begin
      if Result.Success then
         Process_TOML (Result.Value, TOML_Dir, Img);
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

end ROM_Builder.From_TOML;
