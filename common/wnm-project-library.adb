-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2022 Fabien Chouteau                    --
--                                                                           --
--    Wee Noise Maker is free software: you can redistribute it and/or       --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Maker is distributed in the hope that it will be useful,     --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with We Noise Maker. If not, see <http://www.gnu.org/licenses/>. --
--                                                                           --
-------------------------------------------------------------------------------

with WNM.File_System; use WNM.File_System;
with WNM.Utils; use WNM.Utils;
with WNM.Project.Storage;
with WNM.Persistent;

package body WNM.Project.Library is

   Prj_File_Ext : constant String := ".wnm_prj";
   Shutdown_Save_Filename : constant String := "shutdown.tmp_prj";

   G_Names : array (Valid_Prj_Index) of Prj_Entry_Name
     := (others => (others => ' '));

   G_Has_Prj : array (Valid_Prj_Index) of Boolean
     := (others => False);

   G_Saved_Size : File_System.File_Signed_Size := 0;
   G_Loaded_Size : File_System.File_Signed_Size := 0;

   ----------------------
   -- Project_Filename --
   ----------------------

   function Project_Filename (Index : Valid_Prj_Index;
                              Name  : Prj_Entry_Name)
                              return String
   is
      Idx_Img : constant String := Index'Img;
      Trim : constant String := Idx_Img (Idx_Img'First + 1 .. Idx_Img'Last);
      Pad  : constant String :=
        (if Trim'Length = 1
         then "0" & Trim
         else Trim);
   begin
      return Pad & Name & Prj_File_Ext;
   end Project_Filename;

   --------------------
   -- Entry_Filename --
   --------------------

   function Entry_Filename (Index : Valid_Prj_Index) return String is
   begin
      return Project_Filename (Index, G_Names (Index));
   end Entry_Filename;

   -----------------
   -- Last_Loaded --
   -----------------

   function Last_Loaded return Prj_Index
   is (Persistent.Data.Last_Project);

   -----------------
   -- Has_Project --
   -----------------

   function Has_Project (Index : Prj_Index) return Boolean
   is (G_Has_Prj (Index));

   ----------------
   -- Entry_Name --
   ----------------

   function Entry_Name (Index : Valid_Prj_Index) return Prj_Entry_Name
   is
   begin
      if G_Has_Prj (Index) then
         return G_Names (Index);
      else
         return "  -- Empty --  ";
      end if;
   end Entry_Name;

   ------------
   -- Rename --
   ------------

   procedure Rename (Index : Valid_Prj_Index; Name : String) is
   begin
      if Has_Project (Index) then
         declare
            Old_Name : constant Prj_Entry_Name := Entry_Name (Index);
            Old_File : constant String := Entry_Filename (Index);
         begin

            Copy_Str (Name, G_Names (Index));

            if not File_System.Move (Old_File,
                                     Entry_Filename (Index))
            then
               G_Names (Index) := Old_Name;
            end if;
         end;
      end if;
   end Rename;

   ------------
   -- Delete --
   ------------

   procedure Delete (Index : Valid_Prj_Index) is
   begin
      if Has_Project (Index) then
         if File_System.Remove (Entry_Filename (Index)) then
            G_Has_Prj (Index) := False;
         end if;
      end if;
   end Delete;

   ------------------
   -- Load_Library --
   ------------------

   procedure Load_Library is

      procedure Callback (File : String) is

         --  Filename format is XXYYYYY[...].wnm_prj Where XX are digits
         --  giving the project index and YYYY[...] the name of the project.
         --
         --  e.g. '05My Project.wnm_prj'

      begin
         if not Ends_With (File, Prj_File_Ext) then
            return;
         end if;

         declare
            Stem : constant String :=
              File (File'First .. File'Last - Prj_File_Ext'Length);

            Index : Integer;
         begin
            if Stem'Length < 3 then
               return;
            end if;

            --  Get the index numnber
            declare
               Index_Str : constant String :=
                 Stem (Stem'First .. Stem'First + 1);
            begin
               if (for some C of Index_Str => C not in '0' .. '9') then
                  return;
               end if;

               Index := Integer'Value (Index_Str);

               if Index not in
                 Integer (Valid_Prj_Index'First) ..
                 Integer (Valid_Prj_Index'Last)
               then
                  return;
               end if;
            end;

            --  Get Project name
            declare
               Name : constant String := Stem (Stem'First + 2 .. Stem'Last);
            begin
               Copy_Str (Name, G_Names (Valid_Prj_Index (Index)));
               G_Has_Prj (Valid_Prj_Index (Index)) := True;
            end;
         end;
      end Callback;

      procedure For_Each_File
      is new File_System.For_Each_File_In_Dir (Callback);

   begin
      For_Each_File ("/");

      --  Best effort at loading a project that was saved at shutdown
      declare
         Unused : File_System.Storage_Error;
         Unused_Size : File_System.File_Signed_Size;
      begin
         Unused := Storage.Load (Shutdown_Save_Filename, Unused_Size);
      end;
   end Load_Library;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project (Index : Valid_Prj_Index)
                          return File_System.Storage_Error
   is
      Result : File_System.Storage_Error;
   begin
      if not G_Has_Prj (Index) then
         return File_System.Project_Do_Not_Exist;
      end if;

      Result := Storage.Load (Entry_Filename (Index), G_Loaded_Size);

      if Result = Ok then
         Persistent.Data.Last_Project := Index;
      end if;

      return Result;
   end Load_Project;

   ------------------
   -- Save_Project --
   ------------------

   function Save_Project (Index : Valid_Prj_Index)
                          return File_System.Storage_Error
   is
      Tmp_Filename : constant String := "tmp.tmp_prj";
      Result : File_System.Storage_Error;
   begin

      Result := Storage.Save (Tmp_Filename, G_Saved_Size);

      if Result /= Ok then
         return Result;
      end if;

      if not File_System.Move (Tmp_Filename, Entry_Filename (Index)) then
         return Move_Error;
      end if;

      G_Has_Prj (Index) := True;
      Persistent.Data.Last_Project := Index;

      return Ok;
   end Save_Project;

   --------------------
   -- Rename_Project --
   --------------------

   function Rename_Project (Index : Valid_Prj_Index;
                            Name  : String)
                            return File_System.Storage_Error
   is
      New_Name : Prj_Entry_Name;
   begin
      if not G_Has_Prj (Index) then
         return File_System.Project_Do_Not_Exist;
      end if;

      Copy_Str (Name, New_Name);

      declare
         Current_File : constant String := Entry_Filename (Index);
         New_File     : constant String := Project_Filename (Index, New_Name);
      begin
         if not File_System.Move (Current_File, New_File) then
            return Move_Error;
         end if;

         G_Names (Index) := New_Name;
      end;

      return Ok;
   end Rename_Project;

   --------------------
   -- Delete_Project --
   --------------------

   function Delete_Project (Index : Valid_Prj_Index)
                            return File_System.Storage_Error is
   begin
      if not G_Has_Prj (Index) then
         return File_System.Project_Do_Not_Exist;
      end if;

      if not File_System.Remove (Entry_Filename (Index)) then
         return Remove_Error;
      end if;

      G_Names (Index) := (others => ' ');
      G_Has_Prj (Index) := False;
      return Ok;
   end Delete_Project;

   ----------------------------
   -- Save_Project_With_Name --
   ----------------------------

   function Save_Project_With_Name (Index : Valid_Prj_Index;
                                    Name  : String)
                                    return File_System.Storage_Error
   is
      Result : File_System.Storage_Error;
   begin
      if not Has_Project (Index) then
         Copy_Str (Name, G_Names (Index));
         return Save_Project (Index);
      else
         Result := Save_Project (Index);
         if Result = Ok then
            Rename (Index, Name);
         end if;
         return Result;
      end if;
   end Save_Project_With_Name;

   ---------------------------
   -- Try_Save_For_Shutdown --
   ---------------------------

   procedure Try_Save_For_Shutdown is
      Unused : File_System.Storage_Error;
   begin
      Unused := Storage.Save (Shutdown_Save_Filename, G_Saved_Size);
   end Try_Save_For_Shutdown;

   ----------------------
   -- Last_Loaded_Size --
   ----------------------

   function Last_Loaded_Size return File_System.File_Signed_Size
   is (G_Loaded_Size);

   ---------------------
   -- Last_Saved_Size --
   ---------------------

   function Last_Saved_Size return File_System.File_Signed_Size
   is (G_Saved_Size);

end WNM.Project.Library;
