with Tresses.Resources;
with HAL_Configuration;

package WNM_Configuration is

   Screen_Width  : constant := 128;
   Screen_Height : constant := 64;

   Coproc_Data_Size : constant := 32;
   Coproc_Queue_Capacity : constant := 512;

   Individual_Synth_Perf_Enabled : constant Boolean := False;

   type Button is (B1, B2, B3, B4, B5, B6, B7, B8,
                   B9, B10, B11, B12, B13, B14, B15, B16,
                   Rec, Play,
                   Menu, Func, Step_Button, Track_Button, Pattern_Button,
                   Song_Button,
                   PAD_Up, PAD_Down, PAD_Left, PAD_Right, PAD_A, PAD_B);

   subtype LED is Button range B1 .. Song_Button;

   LED_Position : constant array (LED) of Positive :=
     (B1             => 6,
      B2             => 7,
      B3             => 8,
      B4             => 9,
      B5             => 10,
      B6             => 11,
      B7             => 12,
      B8             => 13,
      B9             => 16,
      B10            => 17,
      B11            => 18,
      B12            => 19,
      B13            => 20,
      B14            => 21,
      B15            => 22,
      B16            => 23,
      Rec            => 24,
      Play           => 14,
      Menu           => 1,
      Func           => 4,
      Step_Button    => 15,
      Track_Button   => 5,
      Pattern_Button => 3,
      Song_Button    => 2);

   package Audio is
      Sample_Frequency   : constant := Tresses.Resources.SAMPLE_RATE;
      Samples_Per_Buffer : constant := 256;
      Mixer_Buffer_Count : constant := HAL_Configuration.Mixer_Buffer_Count;
      Input_Buffer_Count : constant := HAL_Configuration.Input_Buffer_Count;

      Mono_Buffer_Size_In_Bytes   : constant := Samples_Per_Buffer * 2;
      Stereo_Buffer_Size_In_Bytes : constant := Samples_Per_Buffer * 4;
   end Audio;

   package Storage is
      pragma Style_Checks ("M120");

      Flash_Base : constant := 16#10000000#;

      Sector_Byte_Size : constant := 4096;

      Total_Storage_Byte_Size : constant := 16 * 1024 * 1024;

      Total_Sectors : constant := Total_Storage_Byte_Size / Sector_Byte_Size;
      --  That's 3072 sectors

      Code_Sectors : constant := 256;
      Code_Byte_Size : constant := Sector_Byte_Size * Code_Sectors;

      --  A worst case project is about 103662 bytes, that's 25.3 sectors.
      --  Let's round it to 30 sectors for good measure. With 20 project
      --  slots we can get up to 600 sectors for projects.
      FS_Sectors : constant := 1792;
      FS_Byte_Size : constant := Sector_Byte_Size * FS_Sectors;

      Nbr_Samples              : constant := 64;
      Sample_Library_Sectors   : constant := 32 * Nbr_Samples;
      Sample_Library_Byte_Size : constant := Sector_Byte_Size * Sample_Library_Sectors;

      Code_Offset           : constant := 0;
      FS_Offset             : constant := Code_Offset + Code_Byte_Size;
      Sample_Library_Offset : constant := FS_Offset + FS_Byte_Size;

      Code_Base_Addr           : constant := Flash_Base + Code_Offset;
      FS_Base_Addr             : constant := Flash_Base + FS_Offset;
      Sample_Library_Base_Addr : constant := Flash_Base + Sample_Library_Offset;

      pragma Compile_Time_Error
        ((Code_Sectors + Sample_Library_Sectors + FS_Sectors) /= Total_Sectors,
         "Invalid number of used sectors");

      Sectors_Per_Sample : constant := Sample_Library_Sectors / Nbr_Samples;
      Sample_Name_Length : constant := 14;

   end Storage;

   package Samples is
      Sample_Count : constant :=
        WNM_Configuration.Storage.Nbr_Samples;

      Single_Sample_Data_Byte_Size : constant :=
        WNM_Configuration.Storage.Sample_Library_Byte_Size / Sample_Count;

      Sample_Storage_Len_Size : constant := 32;

      Sample_Metadata_Byte_Size : constant :=
        WNM_Configuration.Storage.Sample_Name_Length
          + (Sample_Storage_Len_Size / 8);

      Sample_Audio_Byte_Size : constant :=
        Single_Sample_Data_Byte_Size - Sample_Metadata_Byte_Size;

      Points_Per_Sample : constant :=
        Sample_Audio_Byte_Size / 2;
   end Samples;
end WNM_Configuration;
