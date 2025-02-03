with Simple_Logging;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with FSmaker.Source.File;

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

   --------------------
   -- Load_From_TOML --
   --------------------

   procedure Load_From_TOML (This     : in out Instance;
                             Root     :        TOML.TOML_Value;
                             TOML_Dir :        String)
   is
      use TOML;
      Key : constant String := "files";

      procedure Load_Single (Host_Src, Target_Dst : String) is
         Host_File : FSmaker.Source.File.Instance
           := FSmaker.Source.File.Create (Host_Src);
      begin
         Simple_Logging.Always
           ("Import '" & Host_Src & "' to '" & Target_Dst & "'");
         This.Import (Target_Dst, Host_File);
      end Load_Single;

      Files : constant TOML_Value := Root.Get_Or_Null (Key);
   begin

      if Files.Is_Null then
         Simple_Logging.Always ("No Files");
         return;
      elsif Files.Kind /= TOML_Table then
         raise Program_Error with
           "'[" & Key & "]' section should be a table (" &
           Files.Kind'Img & ")";
      end if;

      for Elt of Files.Iterate_On_Table loop
         declare
            Key : constant String := To_String (Elt.Key);
            Val : constant TOML_Value := Elt.Value;
         begin

            if Val.Kind /= TOML_String then
               raise Program_Error with
                 "'[" & Key & ".<Index>]' section should be a table (" &
                 Val.Kind'Img & ")";
            else
               Load_Single (TOML_Dir & "/" & Val.As_String,
                            "/" & Key);
            end if;
         end;
      end loop;

   end Load_From_TOML;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (This : in out Instance) is
   begin
      FSmaker.Pretty_Print (This.Target.Tree (FSmaker.Empty_Path));
   end Print_Tree;

end ROM_Builder.File_System;
