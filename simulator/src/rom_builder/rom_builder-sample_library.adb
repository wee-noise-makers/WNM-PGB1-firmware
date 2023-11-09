with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with HAL;

with FSmaker.Source.File;
with Simple_Logging;
with WNM.Sample_Library; use WNM.Sample_Library;

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

      Points_As_Bytes :
        array (1 .. Single_Sample_Audio_Byte_Size) of HAL.UInt8
        with Address => This.Data (Index).Audio'Address;

      Len        : Natural;
   begin

      Len := Src.Read (Points_As_Bytes'Address,
                       Single_Sample_Audio_Byte_Size);

      Simple_Logging.Debug ("Load sample data:" & Len'Img);

      This.Data (Index).Len := HAL.UInt32 (Len / 2);

   end Load_From_File;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (This  : in out Instance;
                       Index :        Valid_Sample_Index;
                       Name  :        String)
   is
      Len : constant Natural := Natural'Min (Name'Length, Sample_Name_Lenght);
   begin
      if Name'Length > Sample_Name_Lenght then
         Simple_Logging.Warning ("Sample name too long: '" & Name  & "'");
      end if;

      This.Data (Index).Name (1 .. Len) :=
        Name (Name'First .. Name'First + Len - 1);

      This.Data (Index).Name (Len + 1 .. Sample_Name_Lenght) :=
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

   ----------------------
   -- Write_Entry_Info --
   ----------------------

   procedure Write_Entry_Info
     (This :        Instance;
      File : in out FSmaker.Source.Text_Buffer.Instance)
   is
   begin
      for X in Valid_Sample_Index loop
         File.Put (X'Img & ":" &
                     This.Data (X).Name & ":" &
                     This.Data (X).Len'Img &
                     ASCII.LF);
      end loop;
   end Write_Entry_Info;

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

end ROM_Builder.Sample_Library;
