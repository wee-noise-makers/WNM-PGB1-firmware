with FSmaker.Source;
with FSmaker.Sink;

with WNM_Configuration;

private with FSmaker.Target.LittleFS;
private with FSmaker.Block_Device.RAM;

package ROM_Builder.File_System is

   type Instance is tagged private;
   type Acc_All is access all Instance;

   procedure Initialize (This : in out Instance);

   procedure Import (This : in out Instance;
                     Dst  :        String;
                     Src  : in out FSmaker.Source.Class);

   procedure Write_Data (This :        Instance;
                         File : in out FSmaker.Sink.Class);

   procedure Print_Tree (This : in out Instance);

private

   type Instance is tagged record
      Target : FSmaker.Target.LittleFS.Instance
        (WNM_Configuration.Storage.Sector_Byte_Size);
      BD     : aliased FSmaker.Block_Device.RAM.Instance
        (Block_Size       => WNM_Configuration.Storage.Sector_Byte_Size,
         Number_Of_Blocks => WNM_Configuration.Storage.FS_Sectors);
   end record;

end ROM_Builder.File_System;
