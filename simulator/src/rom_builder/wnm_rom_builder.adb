with ROM_Builder.From_TOML;

procedure Wnm_Rom_Builder is
   Img : ROM_Builder.From_TOML.RAM_Image_Acc;
begin
   Img := ROM_Builder.From_TOML.Build_From_TOML
     (Path_To_TOML   => "wnm_rom_descriptor.toml");

   ROM_Builder.From_TOML.Write_To_File (Img.all,
                                        Path_To_Output => "wnm_rom.bin");
end Wnm_Rom_Builder;
