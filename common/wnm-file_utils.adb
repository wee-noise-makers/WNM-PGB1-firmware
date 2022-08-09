-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2017 Fabien Chouteau                    --
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

with Managed_Buffers;       use Managed_Buffers;
with WNM.Buffer_Allocation; use WNM.Buffer_Allocation;

package body WNM.File_Utils is

   function Copy_File_Slice (Srcpath  : String;
                             From, To : Natural;
                             Dstpath  : String)
                             return Status_Code
   is
      Src, Dst         : File_Descriptor;
      Status           : Status_Code;
      Buffer           : constant Any_Managed_Buffer := Allocate (RAM, 1024);
      Src_Len, Dst_Len : File_Size;
   begin
      Status := Open (Src, Srcpath, Read_Only);
      if Status /= OK then
         return Status;
      end if;

      Src_Len := File_Size (From);
      Status := Seek (File   => Src,
                      Origin => From_Start,
                      Amount => Src_Len);

      if Status /= OK then
         Close (Src);
         return Status;
      end if;

      Status := Create_File (Dstpath);
      if Status /= OK then
         Close (Src);
         return Status;
      end if;

      Status := Open (Dst, Dstpath, Write_Only);
      if Status /= OK then
         Close (Src);
         return Status;
      end if;

      loop
         Src_Len := Read (Src, Buffer.Buffer_Address,
                          File_Size (Buffer.Buffer_Length));

         exit when Src_Len = 0;

         Dst_Len := Write (Dst, Buffer.Buffer_Address, Src_Len);

         if Dst_Len /= Src_Len then
            Close (Src);
            Close (Dst);
            return Input_Output_Error;
         end if;

         exit when Src_Len /= File_Size (Buffer.Buffer_Length)
           or else
             Offset (Src) >= File_Size (To);

      end loop;
      Close (Src);
      Close (Dst);
      Release_Buffer (Buffer);
      return OK;
   end Copy_File_Slice;

end WNM.File_Utils;
