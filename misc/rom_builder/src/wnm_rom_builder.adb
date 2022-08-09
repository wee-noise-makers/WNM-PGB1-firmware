with ROM_Builder.From_TOML;

procedure Wnm_Rom_Builder is
begin
   ROM_Builder.From_TOML.Build_From_TOML
     (Path_To_TOML   => "wnm_rom_descriptor.toml",
      Path_To_Output => "wnm_rom.bin");
end Wnm_Rom_Builder;
