with System;
with Littlefs;

package ASFML_SIM_Storage is

   function Get_LFS_Config return not null access Littlefs.LFS_Config;

   function Sample_Data_Base return System.Address;

end ASFML_SIM_Storage;
