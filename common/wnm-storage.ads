with System;

with Littlefs;

package WNM.Storage is

   Sector_Size : constant := 4096;

   Sample_Library_Sectors : constant := 2200;
   Sample_Library_Size : constant := Sector_Size * Sample_Library_Sectors;

   FS_Sectors : constant := 512;
   FS_Size    : constant := Sector_Size * FS_Sectors;

   Total_Storage_Size : constant := Sample_Library_Size + FS_Size;

   function Get_LFS_Config return not null access Littlefs.LFS_Config;

   function Sample_Data_Base return System.Address;

end WNM.Storage;
