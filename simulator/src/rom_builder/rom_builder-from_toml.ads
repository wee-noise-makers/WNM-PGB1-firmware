
with System.Storage_Elements;
with System;
with WNM_Configuration;
with GNAT.OS_Lib;
with FSmaker.Sink;
with FSmaker.Source;

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

   procedure Build_From_TOML (Img          : in out RAM_Image;
                              Path_To_TOML :        String;
                              Format_FS    :        Boolean);

   procedure Write_To_File (Img : RAM_Image; Path_To_Output : String);

private

   type RAM_Image
   is new FSmaker.Sink.Instance and FSmaker.Source.Instance
   with record
      Data : System.Storage_Elements.Storage_Array
        (1 .. WNM_Configuration.Storage.Total_Storage_Byte_Size);
      Next_In : System.Storage_Elements.Storage_Count := 1;
      Next_Out : System.Storage_Elements.Storage_Count := 1;
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
