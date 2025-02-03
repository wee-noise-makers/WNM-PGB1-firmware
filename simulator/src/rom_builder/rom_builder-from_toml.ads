
with System.Storage_Elements; use System.Storage_Elements;
with System;
with WNM_Configuration;
with GNAT.OS_Lib;
with FSmaker.Sink;
with FSmaker.Source;
with Littlefs;

package ROM_Builder.From_TOML is

   type RAM_Image
   is new FSmaker.Sink.Instance and FSmaker.Source.Instance
   with private;

   type RAM_Image_Acc is access all RAM_Image;

   procedure Free (X : in out RAM_Image_Acc);

   function FS_Addr (Img : RAM_Image) return System.Address;
   function Samples_Addr (Img : RAM_Image) return System.Address;

   procedure Load_From_File (Img : in out RAM_Image;
                             FD : GNAT.OS_Lib.File_Descriptor);

   procedure Load_From_UF2 (Img : in out RAM_Image;
                            Filename : String);

   procedure Build_From_TOML (Img          : in out RAM_Image;
                              Path_To_TOML :        String;
                              Format_FS    :        Boolean);

   procedure Write_To_File (Img : RAM_Image; Path_To_Output : String);

   procedure Write_UF2 (Img : RAM_Image;
                        Root_Dir : String;
                        Sample_Lib_Filename : String := "sample_library.uf2";
                        Filesystem_Filename : String := "file_system.uf2");

   type LFS_Config_Access is access all Littlefs.LFS_Config;

   function Create_LFS_Config (Img : aliased RAM_Image)
                               return LFS_Config_Access;

private

   Flash_Base : constant := WNM_Configuration.Storage.Flash_Base;
   FS_Base : constant := WNM_Configuration.Storage.FS_Base_Addr;
   Total_Storage_Byte_Size : constant :=
     WNM_Configuration.Storage.Total_Storage_Byte_Size;

   subtype ROM_Addr_Range
     is Storage_Offset range
       FS_Base .. Flash_Base + Total_Storage_Byte_Size - 1;

   type RAM_Image
   is new FSmaker.Sink.Instance and FSmaker.Source.Instance
   with record
      Data : System.Storage_Elements.Storage_Array (ROM_Addr_Range);
      Next_In : System.Storage_Elements.Storage_Count := FS_Base;
      Next_Out : System.Storage_Elements.Storage_Count := FS_Base;
   end record;

   overriding
   function Write (This : in out RAM_Image;
                   Addr :        System.Address;
                   Len  :        Natural)
                   return Natural;

   overriding
   procedure Close (This : in out RAM_Image) is null;

   overriding
   function Read (This : in out RAM_Image;
                  Addr :        System.Address;
                  Len  :        Natural)
                  return Natural;

end ROM_Builder.From_TOML;
