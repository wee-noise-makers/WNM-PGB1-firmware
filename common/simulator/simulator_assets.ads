--  Advanced Resource Embedder 1.1.0
with Ada.Streams;
with Interfaces.C;
package simulator_assets is

   type Content_Access is access constant Ada.Streams.Stream_Element_Array;

   type Name_Access is access constant String;

   type Format_Type is (FILE_RAW, FILE_GZIP);

   type Content_Type is record
      Name    : Name_Access;
      Content : Content_Access;
      Modtime : Interfaces.C.long := 0;
      Format  : Format_Type := FILE_RAW;
   end record;

   Null_Content : constant Content_Type;

   --  Returns the data stream with the given name or null.
   function Get_Content (Name : String) return Content_Type;

private

   Null_Content : constant Content_Type := (others => <>);

end simulator_assets;
