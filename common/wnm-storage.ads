with System;

with Littlefs;

package WNM.Storage is

   Sector_Size : constant := 4096;

   function Get_LFS_Config return not null access Littlefs.LFS_Config;

   function Sample_Data_Base return System.Address;

end WNM.Storage;
