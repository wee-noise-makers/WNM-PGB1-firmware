with System.Storage_Elements; use System.Storage_Elements;

with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with Littlefs; use Littlefs;

with WNM.Screen;
with WNM.GUI.Bitmap_Fonts;
with WNM.Sample_Library;

with Ada.Text_IO;

package body WNM.File_System is

   File_Buffer : Storage_Array
     (1 .. Storage_Count (Get_LFS_Config.Block_Size));

   File_Conf : aliased constant Littlefs.lfs_file_config :=
     (Buffer => File_Buffer'Address,
      others => <>);

   FS : aliased Littlefs.LFS_T;

   ---------------
   -- Error_Img --
   ---------------

   function Error_Img (Err : int) return String
   is (case Err is
          when LFS_ERR_OK          => "No error",
          when LFS_ERR_IO          => "Error during device operation",
          when LFS_ERR_CORRUPT     => "Corrupted",
          when LFS_ERR_NOENT       => "No directory entry",
          when LFS_ERR_EXIST       => "Entry already exists",
          when LFS_ERR_NOTDIR      => "Entry is not a dir",
          when LFS_ERR_ISDIR       => "Entry is a dir",
          when LFS_ERR_NOTEMPTY    => "Dir is not empty",
          when LFS_ERR_BADF        => "Bad file number",
          when LFS_ERR_FBIG        => "File too large",
          when LFS_ERR_INVAL       => "Invalid parameter",
          when LFS_ERR_NOSPC       => "No space left on device",
          when LFS_ERR_NOMEM       => "No more memory available",
          when LFS_ERR_NOATTR      => "No data/attr available",
          when LFS_ERR_NAMETOOLONG => "File name too long",
          when others              => "Unknown LFS error (" & Err'Img & ")");

   --------------------------
   -- Query_User_To_Format --
   --------------------------

   function Query_User_To_Format return Boolean is
      X : Integer;
      Play, Last_Play : Boolean := False;
      Rec, Last_Rec : Boolean := False;
   begin
      Screen.Clear;
      X := 1;
      GUI.Bitmap_Fonts.Print (X, 1, "No file-system found");

      X := 1;
      GUI.Bitmap_Fonts.Print (X, 9, "Click Play to format");
      X := 1;
      GUI.Bitmap_Fonts.Print (X, 18, "Click Rec to shutdown");

      Screen.Update;

      loop
         declare
            State : constant WNM_PS1_HAL.Buttons_State := WNM_PS1_HAL.State;
         begin
            Last_Play := Play;
            Last_Rec := Rec;
            Play := State (WNM_PS1_HAL_Params.Play) = Down;
            Rec := State (WNM_PS1_HAL_Params.Rec) = Down;
            if Last_Rec and then not Rec then
               return False;
            elsif Last_Play and then not Play then
               return True;
            end if;
         end;
      end loop;
   end Query_User_To_Format;

   -------------------
   -- Factory_Reset --
   -------------------

   procedure Factory_Reset is
      FD : aliased File_Descriptor;
   begin

      Create_File (FD, Sample_Library.Sample_Entries_Filename);
      Close (FD);

   end Factory_Reset;

   -----------
   -- Mount --
   -----------

   procedure Mount is
      Err : int;
   begin
      Err := Littlefs.Mount (FS, Get_LFS_Config.all);
      if Err /= 0 then
         if Query_User_To_Format then
            Err := Littlefs.Format (FS, Get_LFS_Config.all);
            if Err /= 0 then
               raise Program_Error with "Format error: " & Error_Img (Err);
            end if;

            Err := Littlefs.Mount (FS, Get_LFS_Config.all);
            if Err /= 0 then
               raise Program_Error with "Mount error after format: " &
                 Error_Img (Err);
            end if;

            Factory_Reset;
         else
            raise Program_Error
              with "No FS available, nothing we can do from here";
         end if;
      end if;

   end Mount;

   -----------
   -- Close --
   -----------

   procedure Close (FD : aliased in out File_Descriptor) is
      Result : constant int := Littlefs.Close (FS, FD);
   begin
      if Result /= 0 then
         raise Program_Error with "Close file error (" &
           Error_Img (Result) & ")";
      end if;
   end Close;

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File (FD : aliased in out File_Descriptor; Name : String)
   is
      Result : constant int :=
        Littlefs.Open (FS, FD, Name, LFS_O_WRONLY + LFS_O_CREAT);
   begin
      if Result /= 0 then
         raise Program_Error with "Create ('" & Name & "') file error (" &
           Error_Img (Result) & ")";
      end if;
   end Create_File;

   ---------------
   -- Open_Read --
   ---------------

   procedure Open_Read (FD : aliased in out File_Descriptor; Name : String) is
      Result : constant int :=
        Littlefs.Opencfg (FS, FD, Name, LFS_O_RDONLY, File_Conf);
   begin
      if Result /= 0 then
         raise Program_Error with "Open_Read ('" & Name & "') file error (" &
           Error_Img (Result) & ")...";
      end if;
   end Open_Read;

   ----------
   -- Size --
   ----------

   function Size (FD : aliased in out File_Descriptor) return File_Signed_Size
   is (Littlefs.Size (FS, FD));

   ----------
   -- Read --
   ----------

   function Read (FD : aliased in out File_Descriptor;
                  A  : System.Address;
                  N  : File_Size)
                  return File_Signed_Size
   is
   begin
      return Littlefs.Read (FS, FD, A, N);
   end Read;

   -----------
   -- Write --
   -----------

   function Write (FD : aliased in out File_Descriptor;
                   A  : System.Address;
                   N  : File_Size)
                   return File_Signed_Size
   is
   begin
      return Littlefs.Write (FS, FD, A, N);
   end Write;

   ----------
   -- Seek --
   ----------

   procedure Seek (FD     : aliased in out File_Descriptor;
                   Off    : Offset;
                   Whence : Seek_Whence)
   is
   begin
      if Littlefs.Seek (FS, FD, Off, Whence'Enum_Rep) < 0 then
         raise Program_Error with "Seek error...";
      end if;
   end Seek;

   ---------------
   -- Available --
   ---------------

   function Available return File_Signed_Size is
   begin
      return Storage.FS_Size - Littlefs.Size (FS) * Storage.Sector_Size;
   end Available;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (FD   : aliased in out File_Descriptor;
      Item : out String;
      Last : out Natural)
   is
   begin
      Last := Item'First - 1;
      for Elt of Item loop

         exit when Read (FD, Elt'Address, 1) /= 1;
         Last := Last + 1;
         exit when Elt = ASCII.LF;
      end loop;
   end Get_Line;

   --------------------------
   -- For_Each_File_In_Dir --
   --------------------------

   procedure For_Each_File_In_Dir (Dirpath : String) is
      Dir : aliased LFS_Dir;
      Err : int;
      Info : aliased Entry_Info;
   begin
      Err := Open (FS, Dir, Dirpath);

      if Err = 0 then
         while Read (FS, Dir, Info) > 0 loop
            declare
               Name : constant String := Littlefs.Name (Info);
            begin
               if Name /= "." and then Name /= ".." then
                  Process (Name);
               end if;
            end;
         end loop;
         Err := Close (FS, Dir);
      end if;
   end For_Each_File_In_Dir;

end WNM.File_System;
