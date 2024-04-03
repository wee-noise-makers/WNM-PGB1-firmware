with GNAT.OS_Lib;
with Ada.Real_Time;
with GNAT.Command_Line; use GNAT.Command_Line;
with ASFML_Sim;
with ASFML_SIM_Storage;

procedure WNM_PGB1_Simulator is
begin

   declare
      Config : GNAT.Command_Line.Command_Line_Configuration;
   begin
      Define_Switch
        (Config,
         ASFML_SIM_Storage.Switch_Storage_ROM'Access,
         "-r:",
         Long_Switch => "--rom=",
         Help => "Use the provided file as ROM image");

      Define_Switch
        (Config,
         ASFML_SIM_Storage.Switch_Storage_TOML'Access,
         "-t:",
         Long_Switch => "--rom-desc=",
         Help => "Make ROM image from the provided TOML description");

      Define_Switch
        (Config,
         ASFML_SIM_Storage.Switch_Reset_ROM'Access,
         "",
         Long_Switch => "--reset-rom",
         Help => "Reset the ROM image to factory default");

      Set_Usage
        (Config,
         "",
         "Wee-Noise-Maker Simulator");

      Getopt (Config);
   exception
      when GNAT.Command_Line.Invalid_Switch =>
         GNAT.OS_Lib.OS_Exit (1);
      when GNAT.Command_Line.Exit_From_Command_Line =>
         GNAT.OS_Lib.OS_Exit (0);
   end;

   declare
      Result : constant String :=
        ASFML_SIM_Storage.Load_ROM (ASFML_SIM_Storage.ROM_Path,
                                   Samples_From_TOML => True);
   begin
      if Result /= "" or else ASFML_SIM_Storage.Switch_Reset_ROM then
         declare
            Result : constant String := ASFML_SIM_Storage.Create_ROM;
         begin
            if Result /= "" then
               raise Program_Error with Result;
            end if;
         end;
      end if;
   end;

   ASFML_Sim.Start;
   ASFML_Sim.Sim_Clock.Release;
   delay until Ada.Real_Time.Time_Last;

end WNM_PGB1_Simulator;
