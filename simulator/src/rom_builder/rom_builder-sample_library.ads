with TOML;

with FSmaker.Sink;

with WNM_Configuration;

with WNM.Sample_Library;

package ROM_Builder.Sample_Library is

   use WNM_Configuration.Storage;

   subtype Sample_Index is Natural range 0 .. Nbr_Samples;
   subtype Valid_Sample_Index is Sample_Index range 1 .. Sample_Index'Last;

   Invalid_Sample_Entry : constant Sample_Index := Sample_Index'First;

   type Sample_Point_Count is range 0 .. Single_Sample_Point_Cnt;
   subtype Sample_Point_Index
     is Sample_Point_Count range 1 .. Sample_Point_Count'Last;

   type Instance is tagged private;
   type Acc_All is access all Instance;

   procedure Load_From_File (This     : in out Instance;
                             Index    : Valid_Sample_Index;
                             Filename : String);

   procedure Set_Name (This  : in out Instance;
                       Index : Valid_Sample_Index;
                       Name  : String);

   procedure Set_Length (This  : in out Instance;
                         Index : Valid_Sample_Index;
                         Len   : Sample_Point_Count);

   procedure Load_From_TOML (This     : in out Instance;
                             Root     :        TOML.TOML_Value;
                             TOML_Dir :        String);

   procedure Write_Data (This :        Instance;
                         File : in out FSmaker.Sink.Class);

   procedure Write_UF2_File (Id     : WNM.Sample_Library.Valid_Sample_Index;
                             Sample : WNM.Sample_Library.Single_Sample_Data;
                             Root_Dir : String);

private

   type Instance is tagged record
      Data : WNM.Sample_Library.Global_Sample_Array;
   end record;

end ROM_Builder.Sample_Library;
