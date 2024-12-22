with TOML;

with FSmaker.Sink;

with WNM.Sample_Library;

package ROM_Builder.Sample_Library is

   type Instance is tagged private;
   type Acc_All is access all Instance;

   procedure Load_From_File (This     : in out Instance;
                             Index    : WNM.Sample_Library.Sample_Index;
                             Filename : String);

   procedure Set_Name (This  : in out Instance;
                       Index : WNM.Sample_Library.Sample_Index;
                       Name  : String);

   procedure Set_Length (This  : in out Instance;
                         Index : WNM.Sample_Library.Sample_Index;
                         Len   : WNM.Sample_Library.Sample_Point_Count);

   procedure Load_From_TOML (This     : in out Instance;
                             Root     :        TOML.TOML_Value;
                             TOML_Dir :        String);

   procedure Write_Data (This :        Instance;
                         File : in out FSmaker.Sink.Class);

   procedure Write_UF2_Single (Id     : WNM.Sample_Library.Sample_Index;
                               Sample : WNM.Sample_Library.Single_Sample_Data;
                               Root_Dir : String);

private

   type Instance is tagged record
      Data : WNM.Sample_Library.Global_Sample_Array;
   end record;

end ROM_Builder.Sample_Library;
