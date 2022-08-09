with ROM_Builder.Sample_Library;
with ROM_Builder.File_System;

with FSmaker.Source.Text_Buffer;
with FSmaker.Sink.File;

with TOML; use TOML;
with TOML.File_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with CLIC.User_Input; use CLIC.User_Input;

with Simple_Logging;

package body ROM_Builder.From_TOML is

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

         Simple_Logging.Always ("Existing image file '" & Path_To_Output &
                                  "' will be overwritten.");
         if Query ("Do you want to continue?",
                   Valid    => (Yes | No => True, Always => False),
                   Default  => Yes) = Yes
         then
            FD := Open_Read_Write (Path_To_Output, Binary);
         else
            raise Program_Error with "Cannot overwrite existing file";
         end if;
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

   procedure Process_TOML (Root : TOML_Value; FD : File_Descriptor) is
      Lib : constant Sample_Library.Acc_All := new Sample_Library.Instance;
      FS  : constant File_System.Acc_All := new File_System.Instance;

      Output : FSmaker.Sink.Class := FSmaker.Sink.File.Create (FD);
   begin
      if Root.Kind /= TOML_Table then
         raise Program_Error with "Invalid TOML file. Table expected";
      end if;

      FS.Initialize;

      Lib.Load_From_TOML (Root);

      declare
         TB : FSmaker.Source.Text_Buffer.Instance;
      begin
         Lib.Write_Entry_Info (TB);
         FS.Import ("/sample_entries.txt", TB);
      end;

      FS.Write_Data (Output);
      Lib.Write_Data (Output);

      Output.Close;

   end Process_TOML;

   ---------------------
   -- Build_From_TOML --
   ---------------------

   procedure Build_From_TOML (Path_To_TOML, Path_To_Output : String) is

      Result : constant Read_Result := File_IO.Load_File (Path_To_TOML);
      FD : File_Descriptor;
   begin
      if Result.Success then
         FD := Open_Image (Path_To_Output);
         Process_TOML (Result.Value, FD);
      else
         raise Program_Error with Path_To_TOML & ":" & Format_Error (Result);
      end if;
   end Build_From_TOML;

end ROM_Builder.From_TOML;
