with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with Littlefs; use Littlefs;
with WNM.Storage;
with WNM.Factory_Reset;

with WNM.Screen;
with WNM.GUI.Bitmap_Fonts;
with WNM.Buttons;

package body WNM.File_System is

   FS : aliased Littlefs.LFS_T;

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
         Buttons.Scan;
         Last_Play := Play;
         Last_Rec := Rec;
         Play := Buttons.Is_Pressed (WNM.Play);
         Rec := Buttons.Is_Pressed (WNM.Rec);
         if Last_Rec and then not Rec then
            return False;
         elsif Last_Play and then not Play then
            return True;
         end if;
      end loop;
   end Query_User_To_Format;

   -----------
   -- Mount --
   -----------

   procedure Mount is
      Err : int;

      Do_Reset : constant Boolean := False;
   begin
      Err := Littlefs.Mount (FS, WNM.Storage.Get_LFS_Config.all);
      if Err /= 0 then
         if Query_User_To_Format then
            Err := Littlefs.Format (FS, WNM.Storage.Get_LFS_Config.all);
            if Err /= 0 then
               raise Program_Error with "Format error: " & Err'Img;
            end if;

            Err := Littlefs.Mount (FS, WNM.Storage.Get_LFS_Config.all);
            if Err /= 0 then
               raise Program_Error with "Mount error after format: " & Err'Img;
            end if;

            if Do_Reset then
               WNM.Factory_Reset.Reset (FS);
            end if;
         else
            raise Program_Error with "No FS available, nothing we can do from here";
         end if;
      end if;


   end Mount;

   -----------
   -- Close --
   -----------

   procedure Close (FD : aliased in out File_Descriptor) is
   begin
      if Littlefs.Close (FS, FD) /= 0 then
         raise Program_Error with "Close file error...";
      end if;
   end Close;

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File (FD : aliased in out File_Descriptor; Name : String) is
   begin
      if Littlefs.Open (FS, FD, Name, LFS_O_WRONLY + LFS_O_CREAT) /= 0 then
         raise Program_Error with "Create ('" & Name & "') file error...";
      end if;
   end Create_File;

   ---------------
   -- Open_Read --
   ---------------

   procedure Open_Read (FD : aliased in out File_Descriptor; Name : String) is
      Result : constant int := Littlefs.Open (FS, FD, Name, LFS_O_RDONLY);
   begin
      if Result /= 0 then
         raise Program_Error with "Open_Read ('" & Name & "') file error (" &
           Result'Img & ")...";
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
      return WNM.Storage.FS_Size - Littlefs.Size (FS) * Storage.Sector_Size;
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
