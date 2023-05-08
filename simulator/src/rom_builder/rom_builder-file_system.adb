package body ROM_Builder.File_System is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Instance) is
   begin
      This.Target.Format (This.BD'Unchecked_Access);
      This.Target.Mount (This.BD'Unchecked_Access);
   end Initialize;

   ---------------------
   -- Initialize_From --
   ---------------------

   procedure Initialize_From (This : in out Instance;
                              Img  : in out FSmaker.Source.Class)
   is
   begin
      This.Read_Data (Img);
      This.Target.Mount (This.BD'Unchecked_Access);
   end Initialize_From;

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

   ---------------
   -- Read_Data --
   ---------------

   procedure Read_Data (This :        Instance;
                        File : in out FSmaker.Source.Class)
   is
   begin
      This.BD.Read_Data (File);
   end Read_Data;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (This : in out Instance) is
   begin
      FSmaker.Pretty_Print (This.Target.Tree (FSmaker.Empty_Path));
   end Print_Tree;

end ROM_Builder.File_System;
