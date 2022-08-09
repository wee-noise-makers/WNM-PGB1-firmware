package ROM_Builder is

   Flash_Sector_Size  : constant := 4096;
   Sample_Sectors     : constant := 2200;
   FS_Sectors         : constant := 512;

   ROM_Sectors        : constant := FS_Sectors + Sample_Sectors;
   ROM_Size           : constant := ROM_Sectors * Flash_Sector_Size;

end ROM_Builder;
