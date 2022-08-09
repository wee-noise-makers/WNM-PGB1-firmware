with TOML;

with FSmaker.Sink;
with FSmaker.Source.Text_Buffer;

private with HAL;

package ROM_Builder.Sample_Library is

   Nbr_Samples             : constant := 50;
   Global_Sample_Byte_Size : constant := Sample_Sectors * Flash_Sector_Size;
   Sectors_Per_Sample      : constant := Sample_Sectors / Nbr_Samples;
   Single_Sample_Byte_Size : constant := Global_Sample_Byte_Size / Nbr_Samples;
   Single_Sample_Point_Cnt : constant := Single_Sample_Byte_Size / 2;
   Sample_Name_Lenght      : constant := 15;

   subtype Sample_Index is Natural range 0 .. Nbr_Samples;
   subtype Valid_Sample_Index is Sample_Index range 1 .. Sample_Index'Last;

   Invalid_Sample_Entry : constant Sample_Index := Sample_Index'First;

   type Sample_Point_Count is range 0 .. Single_Sample_Point_Cnt;
   subtype Sample_Point_Index
     is Sample_Point_Count range 1 .. Sample_Point_Count'Last;

   type Instance is tagged private;
   type Acc_All is access all Instance;

   procedure Load_From_File (This     : in out Instance;
                             Index    : Valid_Sample_Index;
                             Filename : String);

   procedure Set_Name (This  : in out Instance;
                       Index : Valid_Sample_Index;
                       Name  : String);

   procedure Set_Length (This  : in out Instance;
                         Index : Valid_Sample_Index;
                         Len   : Sample_Point_Count);

   procedure Load_From_TOML (This : in out Instance;
                             Root : TOML.TOML_Value);

   procedure Write_Entry_Info
     (This :        Instance;
      File : in out FSmaker.Source.Text_Buffer.Instance);

   procedure Write_Data (This :        Instance;
                         File : in out FSmaker.Sink.Class);

private

   type Sample_Points_Data is array (Sample_Point_Index) of HAL.UInt16
     with Size => Single_Sample_Byte_Size * 8;

   type Sample_Bytes_Data is array (1 .. Single_Sample_Byte_Size) of HAL.UInt8
     with Size => Single_Sample_Byte_Size * 8;

   type Single_Sample_Data (Kind : Boolean := False) is record
      case Kind is
         when True =>
            Points : Sample_Points_Data;
         when False =>
            Bytes  : Sample_Bytes_Data;
      end case;
   end record
     with Unchecked_Union, Size => Single_Sample_Byte_Size * 8;

      type Single_Sample_Data_Access is access all Single_Sample_Data;

   type Global_Sample_Array
   is array (Valid_Sample_Index) of aliased Single_Sample_Data
     with Size => Global_Sample_Byte_Size * 8;

   type Entry_Info is record
      Name : String (1 .. Sample_Name_Lenght) := (others => ' ');
      Len  : Sample_Point_Count := 0;
   end record;

   type Entry_Info_Array is array (Valid_Sample_Index) of Entry_Info;

   type Instance is tagged record
      Data : Global_Sample_Array;
      Info : Entry_Info_Array;
   end record;

end ROM_Builder.Sample_Library;
