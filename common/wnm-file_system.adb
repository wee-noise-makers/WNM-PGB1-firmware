with System.Storage_Elements; use System.Storage_Elements;

with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with Littlefs; use Littlefs;

with WNM.Screen;
with WNM.GUI.Bitmap_Fonts;

package body WNM.File_System is

   File_Buffer : Storage_Array
     (1 .. Storage_Count (Get_LFS_Config.Block_Size));

   File_Conf : aliased constant Littlefs.lfs_file_config :=
     (Buffer => File_Buffer'Address,
      others => <>);

   FS : aliased Littlefs.LFS_T;

   G_FD : aliased Littlefs.LFS_File;
   G_FD_Mode : File_Mode := Closed;

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
            State : constant WNM_HAL.Buttons_State := WNM_HAL.State;
         begin
            Last_Play := Play;
            Last_Rec := Rec;
            Play := State (WNM_Configuration.Play) = Down;
            Rec := State (WNM_Configuration.Rec) = Down;
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
   begin
      null;
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

   ------------
   -- Status --
   ------------

   function Status return File_Mode
   is (G_FD_Mode);

   ----------
   -- Size --
   ----------

   function Size return File_Signed_Size
   is (if Status = Closed then 0 else Littlefs.Size (FS, G_FD));

   ---------------
   -- Open_Read --
   ---------------

   function Open_Read (Path : String) return Open_Read_Result is
   begin
      if Status /= Closed then
         return Already_Open;
      end if;

      declare
         Result : constant int :=
           Littlefs.Opencfg (FS, G_FD, Path, LFS_O_RDONLY, File_Conf);
      begin
         case Result is
            when 0 =>
               G_FD_Mode := Read_Only;
               return Ok;
            when Littlefs.LFS_ERR_NOENT =>
               return Not_Found;
            when others =>
               return Cannot_Open;
         end case;
      end;
   end Open_Read;

   ----------------
   -- Open_Write --
   ----------------

   function Open_Write (Path : String) return Open_Read_Result is
   begin
      if Status /= Closed then
         return Already_Open;
      end if;

      declare
         Result : constant int :=
           Littlefs.Opencfg (FS, G_FD, Path, LFS_O_WRONLY + LFS_O_CREAT,
                             File_Conf);
      begin
         case Result is
            when 0 =>
               G_FD_Mode := Write_Only;
               return Ok;
            when others =>
               return Cannot_Open;
         end case;
      end;
   end Open_Write;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      if Status = Closed then
         return;
      end if;

      declare
         Result : constant int := Littlefs.Close (FS, G_FD);
      begin
         if Result /= 0 then
            raise Program_Error with "Close file error (" &
              Error_Img (Result) & ")";
         end if;
      end;

      G_FD_Mode := Closed;
   end Close;

   ----------
   -- Read --
   ----------

   function Read (A  : System.Address;
                  N  : File_Size)
                  return File_Signed_Size
   is (Littlefs.Read (FS, G_FD, A, N));

   ---------------
   -- Read_Line --
   ---------------

   procedure Read_Line (Item : out String;
                        Last : out Natural)
   is
   begin
      Last := Item'First - 1;
      for Elt of Item loop

         exit when Read (Elt'Address, 1) /= 1;
         Last := Last + 1;
         exit when Elt = ASCII.LF;
      end loop;
   end Read_Line;

   -----------
   -- Write --
   -----------

   function Write (A  : System.Address;
                   N  : File_Size)
                   return File_Signed_Size
   is (Littlefs.Write (FS, G_FD, A, N));

   ---------------
   -- Available --
   ---------------

   function Available return File_Signed_Size is
   begin
      return Storage.FS_Size - Littlefs.Size (FS) * Storage.Sector_Size;
   end Available;

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

   -----------
   -- Write --
   -----------

   procedure Write (This    : in out Flux_Sink_Instance;
                    Data    :        System.Storage_Elements.Storage_Element;
                    Success :    out Boolean)
   is
      pragma Unreferenced (This);
   begin
      if Status /= Write_Only then
         Success := False;
         return;
      end if;

      Success := Write (Data'Address, 1) = 1;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (This : in out Flux_Sink_Instance;
                    Data :        System.Storage_Elements.Storage_Array;
                    Last :    out System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (This);
      Result : File_Signed_Size;
   begin
      if Status /= Write_Only then
         Last := Data'First;
         return;
      end if;

      Result := Write (Data'Address, Data'Length);

      if Result < 0 then
         Last := Data'First;
      else
         Last := Data'First + Storage_Offset (Result);
      end if;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read (This    : in out Flux_Source_Instance;
                   Data    :    out System.Storage_Elements.Storage_Element;
                   Success :    out Boolean)
   is
      pragma Unreferenced (This);
   begin
      if Status /= Read_Only then
         Success := False;
         return;
      end if;

      Success := Read (Data'Address, 1) = 1;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Flux_Source_Instance;
                   Data :    out System.Storage_Elements.Storage_Array;
                   Last :    out System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (This);
      Result : File_Signed_Size;
   begin
      if Status /= Write_Only then
         Last := Data'First;
         return;
      end if;

      Result := Read (Data'Address, Data'Length);

      if Result < 0 then
         Last := Data'First;
      else
         Last := Data'First + Storage_Offset (Result);
      end if;
   end Read;

end WNM.File_System;
