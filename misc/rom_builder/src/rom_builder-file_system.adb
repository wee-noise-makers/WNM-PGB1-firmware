package body ROM_Builder.File_System is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Instance) is
   begin
      This.Target.Format (This.BD'Unchecked_Access);
      This.Target.Mount (This.BD'Unchecked_Access);
   end Initialize;

   ------------
   -- Import --
   ------------

   procedure Import (This : in out Instance;
                     Dst  :        String;
                     Src  : in out FSmaker.Source.Class)
   is
   begin
      This.Target.Import (Path => FSmaker.To_Target_Path (Dst),
                          Src  => Src);
   end Import;

   ----------------
   -- Write_Data --
   ----------------

   procedure Write_Data (This :        Instance;
                         File : in out FSmaker.Sink.Class)
   is
   begin
      This.BD.Write_Data (File);
   end Write_Data;

end ROM_Builder.File_System;
