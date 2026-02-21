with Littlefs;
with System;
with System.Storage_Elements;

with Flux.Traits.Sink;
with Flux.Traits.Source;

package WNM.File_System is

   procedure Mount;

   subtype File_Size is Littlefs.LFS_Size;
   subtype File_Signed_Size is Littlefs.LFS_Signed_Size;

   type File_Mode is (Closed, Read_Only, Write_Only);
   type Open_Read_Result is (Ok, Already_Open, Not_Found, Cannot_Open);
   type Open_Write_Result is (Ok, Already_Open, Cannot_Open);

   function Status return File_Mode;

   function Size return File_Signed_Size
     with Pre => Status /= Closed;

   function Open_Read (Path : String) return Open_Read_Result
     with Post => (if Open_Read'Result = Ok then Status = Read_Only);

   function Open_Write (Path : String) return Open_Write_Result
     with Post => (if Open_Write'Result = Ok then Status = Write_Only);

   procedure Close
     with Pre => Status /= Closed,
     Post => Status = Closed;

   function Read (A  : System.Address;
                  N  : File_Size)
                  return File_Signed_Size
     with Pre => Status = Read_Only;
   --  Read data from file
   --
   --  Takes a buffer and size indicating where to store the read data.
   --
   --  Returns the number of bytes read, or a negative error code on failure.

   procedure Read_Line (Item : out String;
                        Last : out Natural)
     with Pre => Status = Read_Only;

   function Write (A  : System.Address;
                   N  : File_Size)
                   return File_Signed_Size
     with Pre => Status = Write_Only;
   --  Write data to file
   --
   --  Takes a buffer and size indicating the data to write. The file will not
   --  actually be updated on the storage until either sync or close is called.
   --
   --  Returns the number of bytes written, or a negative error code on
   --  failure.

   function Available return File_Signed_Size;

   generic
      with procedure Process (Filename : String);
   procedure For_Each_File_In_Dir (Dirpath : String);

   function Remove (Filename : String) return Boolean;
   --  Remove/delete a file from the file system.
   --  Return True on success.

   function Move (From, To : String) return Boolean;
   --  Move a file From in To, replacing the content of To, if any.
   --  Return True on success.

   type Storage_Error is (Ok,
                          Out_Of_Space,
                          Project_Do_Not_Exist,
                          Disk_Error,
                          Move_Error,
                          Remove_Error,
                          Format_Error,
                          Already_Open,
                          Not_Found,
                          Cannot_Open,
                          Cannot_Decode,
                          Cannot_Encode,
                          Value_Not_In_Range,
                          Unknown_Error);

   function Img (S : Storage_Error) return String
   is (case S is
          when Ok                   => "Ok",
          when Out_Of_Space         => "No space on disk",
          when Project_Do_Not_Exist => "No project",
          when Disk_Error           => "Disk error",
          when Move_Error           => "Move error",
          when Remove_Error         => "Remove error",
          when Format_Error         => "Format Error",
          when Already_Open         => "Already Open",
          when Not_Found            => "Not Found",
          when Cannot_Open          => "Cannot Open",
          when Cannot_Decode        => "Cannot Decode",
          when Cannot_Encode        => "Cannot Encode",
          when Value_Not_In_Range   => "Value not in range",
          when Unknown_Error        => "Unknown_Error");

   ----------
   -- Flux --
   ----------

   type Flux_Sink_Instance is null record;

   procedure Write (This    : in out Flux_Sink_Instance;
                    Data    :        System.Storage_Elements.Storage_Element;
                    Success :    out Boolean);

   procedure Write (This : in out Flux_Sink_Instance;
                    Data :        System.Storage_Elements.Storage_Array;
                    Last :    out System.Storage_Elements.Storage_Count);

   package Flux_Sink is new Flux.Traits.Sink (Flux_Sink_Instance,
                                              Write,
                                              Write);

   type Flux_Source_Instance is null record;

   procedure Read (This    : in out Flux_Source_Instance;
                   Data    :    out System.Storage_Elements.Storage_Element;
                   Success :    out Boolean);

   procedure Read (This : in out Flux_Source_Instance;
                   Data :    out System.Storage_Elements.Storage_Array;
                   Last :    out System.Storage_Elements.Storage_Count);

   package Flux_Source is new Flux.Traits.Source (Flux_Source_Instance,
                                                  Read,
                                                  Read);

   File_Sector_Buffer  : WNM_HAL.Storage_Sector_Data
     with Alignment => 2;
   --  This buffer is kept public so we can use it for writing flash sectors
   --  in other parts of the code. Of course it should not be used while a
   --  file is open...

end WNM.File_System;
