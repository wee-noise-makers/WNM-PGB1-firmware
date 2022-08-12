with Littlefs;
with System;

package WNM.File_System is

   procedure Mount;

   subtype File_Descriptor is Littlefs.LFS_File;
   subtype File_Size is Littlefs.LFS_Size;
   subtype File_Signed_Size is Littlefs.LFS_Signed_Size;

   procedure Close (FD : aliased in out File_Descriptor);

   procedure Create_File (FD : aliased in out File_Descriptor; Name : String);

   procedure Open_Read (FD : aliased in out File_Descriptor; Name : String);

   function Size (FD : aliased in out File_Descriptor)
                  return File_Signed_Size;

   function Read (FD : aliased in out File_Descriptor;
                  A  : System.Address;
                  N  : File_Size)
                  return File_Signed_Size;
   --  Read data from file
   --
   --  Takes a buffer and size indicating where to store the read data.
   --
   --  Returns the number of bytes read, or a negative error code on failure.

   function Write (FD : aliased in out File_Descriptor;
                   A  : System.Address;
                   N  : File_Size)
                   return File_Signed_Size;
   --  Write data to file
   --
   --  Takes a buffer and size indicating the data to write. The file will not
   --  actually be updated on the storage until either sync or close is called.
   --
   --  Returns the number of bytes written, or a negative error code on
   --  failure.

   subtype Offset is Littlefs.LFS_Signed_Offset;
   subtype Seek_Whence is Littlefs.LFS_Whence_Flags;

   procedure Seek (FD     : aliased in out File_Descriptor;
                   Off    : Offset;
                   Whence : Seek_Whence);

   function Available return File_Signed_Size;

   procedure Get_Line
     (FD   : aliased in out File_Descriptor;
      Item :            out String;
      Last :            out Natural);

   generic
      with procedure Process (Filename : String);
   procedure For_Each_File_In_Dir (Dirpath : String);

end WNM.File_System;
