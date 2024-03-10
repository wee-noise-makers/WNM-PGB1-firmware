with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Storage_Elements;
with Interfaces; use Interfaces;
with HAL;

with FSmaker.Source.File;
with Simple_Logging;
with WNM.Sample_Library; use WNM.Sample_Library;

with Ada.Text_IO;
with GNAT.OS_Lib;

with Ada.Strings.Fixed;
with UF2_Utils.File_IO;

package body ROM_Builder.Sample_Library is

   --------------------
   -- Load_From_File --
   --------------------

   procedure Load_From_File (This     : in out Instance;
                             Index    :        Valid_Sample_Index;
                             Filename :        String)
   is
      Src : FSmaker.Source.File.Instance :=
        FSmaker.Source.File.Create (Filename);

      Points : WNM.Sample_Library.Sample_Audio_Data := (others => 0);
      Len    : Natural;
   begin

      Len := Src.Read (Points'Address, Points'Size / 8);

      Simple_Logging.Debug ("Load sample data:" & Len'Img);

      This.Data (Index).Audio := Points;
      This.Data (Index).Len := HAL.UInt32 (Len / 2);

   end Load_From_File;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (This  : in out Instance;
                       Index :        Valid_Sample_Index;
                       Name  :        String)
   is
      Len : constant Natural := Natural'Min (Name'Length, Sample_Name_Length);
   begin
      if Name'Length > Sample_Name_Length then
         Simple_Logging.Warning ("Sample name too long: '" & Name  & "'");
      end if;

      This.Data (Index).Name (1 .. Len) :=
        Name (Name'First .. Name'First + Len - 1);

      This.Data (Index).Name (Len + 1 .. Sample_Name_Length) :=
        (others => ' ');
   end Set_Name;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length (This  : in out Instance;
                         Index :        Valid_Sample_Index;
                         Len   :        Sample_Point_Count)
   is
   begin
      This.Data (Index).Len := HAL.UInt32 (Len);
   end Set_Length;

   --------------------
   -- Load_From_TOML --
   --------------------

   procedure Load_From_TOML (This     : in out Instance;
                             Root     :        TOML.TOML_Value;
                             TOML_Dir :        String)
   is
      use TOML;
      Key : constant String := "samples";

      procedure Load_Single (Index : Valid_Sample_Index; Table : TOML_Value) is
         Name : constant TOML_Value := Table.Get_Or_Null ("name");
         File : constant TOML_Value := Table.Get_Or_Null ("file");
      begin
         if Name.Is_Null then
            raise Program_Error
              with "Missing sample name for index" & Index'Img;
         elsif Name.Kind /= TOML_String then
            raise Program_Error
              with "Missing sample name must be a TOML String";

         elsif File.Is_Null then
            raise Program_Error
              with "Missing sample file for index" & Index'Img;
         elsif File.Kind /= TOML_String then
            raise Program_Error
              with "Missing sample file must be a TOML String";
         end if;

         This.Set_Name (Index, Name.As_String);
         This.Load_From_File (Index, TOML_Dir & "/" & File.As_String);

      end Load_Single;

      Samples : constant TOML_Value := Root.Get_Or_Null (Key);

      Index : Valid_Sample_Index;
   begin
      for Elt of This.Data loop
         Elt.Len := 0;
      end loop;

      if Samples.Is_Null then
         Simple_Logging.Always ("No Samples");
         return;
      elsif Samples.Kind /= TOML_Table then
         raise Program_Error with
           "'[" & Key & "]' section should be a table (" &
           Samples.Kind'Img & ")";
      end if;

      for Elt of Samples.Iterate_On_Table loop
         declare
            Key : constant String := To_String (Elt.Key);
            Val : constant TOML_Value := Elt.Value;
         begin
            begin
               Index := Valid_Sample_Index'Value (Key);
            exception
               when Constraint_Error =>
                  raise Program_Error
                    with  "Invalid sample index: '" & Key & "'";
            end;

            if Val.Kind /= TOML_Table then
               raise Program_Error with
                 "'[" & Key & ".<Index>]' section should be a table (" &
                 Val.Kind'Img & ")";
            else
               Load_Single (Index, Val);
            end if;
         end;
      end loop;
   end Load_From_TOML;

   ----------------
   -- Write_Data --
   ----------------

   procedure Write_Data (This :        Instance;
                         File : in out FSmaker.Sink.Class)
   is
      Len : constant Natural := This.Data'Size / 8;
   begin
      if File.Write (This.Data'Address, Len) /= Len then
         raise Program_Error;
      end if;
   end Write_Data;

   ----------------------
   -- Write_UF2_Single --
   ----------------------

   procedure Write_UF2_Single (Id     : WNM.Sample_Library.Valid_Sample_Index;
                               Sample : WNM.Sample_Library.Single_Sample_Data;
                               Root_Dir : String)
   is
      use HAL;

      use UF2_Utils.File_IO;
      use Ada.Strings.Fixed;
      use System.Storage_Elements;

      Filename : constant String :=
        Root_Dir & "/" &
        Trim (Id'Img & "-" & Sample.Name, Ada.Strings.Both) &
        ".uf2_wnm_sample";

      Filename_Bin : constant String :=
        Root_Dir & "/" &
        Trim (Id'Img & "-" & Sample.Name, Ada.Strings.Both) &
        ".bin_wnm_sample";

      File : UF2_Sequential_IO.File_Type;
      Data : Storage_Array (1 .. Single_Sample_Data'Size / 8)
        with Address => Sample'Address;

   begin
      if Sample.Len /= 0 then

         declare
            use GNAT.OS_Lib;
            FD : constant File_Descriptor :=
              Create_File (Filename_Bin, Binary);
            Len : Integer;
         begin
            Ada.Text_IO.Put_Line (Filename_Bin);
            Len := Write (FD, Data'Address, Data'Size / 8);
            if Len /= Data'Size / 8 then
               Ada.Text_IO.Put_Line ("Cannot write '" & Filename_Bin & "'");
            end if;
            Close (FD);
         end;

         Ada.Text_IO.Put_Line (Filename);

         UF2_Sequential_IO.Create (File, Name => Filename);

         Write_UF2
           (Data => Data,
            Start_Address => Unsigned_32 (Entry_Device_Address (Id)),
            File => File,
            Max_Block_Size => 256,
            Flags  => 16#00002000#,
            Family => UF2_Family);

         UF2_Sequential_IO.Close (File);
      end if;
   end Write_UF2_Single;

end ROM_Builder.Sample_Library;
