with System;
with HAL;
with TOML;
with FSmaker.Sink;
with Tresses;
with WNM_Configuration;

package ROM_Builder.Sample_Library is
   use WNM_Configuration.Samples;

   type Instance is tagged private;
   type Acc_All is access all Instance;

   subtype Sample_Storage_Len is HAL.UInt32;

   pragma Compile_Time_Error
     (Sample_Storage_Len'Size /= Sample_Storage_Len_Size, "Invalid size");

   type Sample_Point_Count is range 0 .. Points_Per_Sample;
   subtype Sample_Point_Index
     is Sample_Point_Count range 0 .. Sample_Point_Count'Last - 1;

   type Sample_Audio_Data is array (Sample_Point_Index) of Tresses.Mono_Point
     with Size => Points_Per_Sample * 16;

   Sample_Id_Last : constant := Sample_Count - 1;
   type Sample_Index is new HAL.UInt7 range 0 .. Sample_Id_Last;

   subtype Sample_Entry_Name
     is String (1 .. WNM_Configuration.Storage.Sample_Name_Length);

   type Single_Sample_Data is record
      Audio    : Sample_Audio_Data;
      Name     : Sample_Entry_Name;
      Len      : Sample_Storage_Len;
   end record
     with Pack, Size => Single_Sample_Data_Byte_Size * 8;

   function Entry_Device_Address (Index : Sample_Index)
                                  return HAL.UInt32;
   --  Return the base address of an sample entry in the device memory space

   procedure Load_From_File (This     : in out Instance;
                             Index    : Sample_Index;
                             Filename : String);

   procedure Set_Name (This  : in out Instance;
                       Index : Sample_Index;
                       Name  : String);

   procedure Set_Length (This  : in out Instance;
                         Index : Sample_Index;
                         Len   : Sample_Point_Count);

   procedure Load_From_TOML (This     : in out Instance;
                             Root     :        TOML.TOML_Value;
                             TOML_Dir :        String);

   procedure Write_Data (This :        Instance;
                         File : in out FSmaker.Sink.Class);

   procedure Write_UF2_Single (Id       : Sample_Index;
                               Name     : String;
                               Addr     : System.Address;
                               Root_Dir : String);

private

   type Global_Sample_Array
   is array (Sample_Index) of aliased Single_Sample_Data
     with Size => WNM_Configuration.Storage.Sample_Library_Byte_Size * 8;

   type Instance is tagged record
      Data : Global_Sample_Array;
   end record;

end ROM_Builder.Sample_Library;
