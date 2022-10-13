with System;
with Littlefs;

package ASFML_SIM_Storage is

   function Load_ROM (Path : String) return String;
   --  Load ROM from a binary file.
   --  Return empty string on success, an error message otherwise

   function Create_ROM return String;
   --  Create a ROM from TOML description file
   --  Return empty string on success, an error message otherwise

   function Save_ROM (Path : String) return String;
   --  Save ROM to a binary file.
   --  Return empty string on success, an error message otherwise

   function Get_LFS_Config return not null access Littlefs.LFS_Config;

   function Sample_Data_Base return System.Address;

end ASFML_SIM_Storage;
