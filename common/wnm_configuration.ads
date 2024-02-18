with Tresses.Resources;

package WNM_Configuration is

   Screen_Width  : constant := 128;
   Screen_Height : constant := 64;

   Coproc_Data_Size : constant := 32;
   Coproc_Queue_Capacity : constant := 512;

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
      Song_Button   => 2);

   package Audio is
      Sample_Frequency            : constant := Tresses.Resources.SAMPLE_RATE;
      Samples_Per_Buffer          : constant := 128;
      Mixer_Buffer_Count          : constant := 3;

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

      Code_Sectors : constant := 1204;
      Code_Byte_Size : constant := Sector_Byte_Size * Code_Sectors;

      FS_Sectors : constant := 1484;
      FS_Byte_Size : constant := Sector_Byte_Size * FS_Sectors;

      Nbr_Samples              : constant := 32;
      Sample_Library_Sectors   : constant := 44 * Nbr_Samples;
      Sample_Library_Byte_Size : constant := Sector_Byte_Size * Sample_Library_Sectors;
      --  Comes from the required sectors to store ~2 seconds of audio in QOA
      --  format.

      Code_Offset           : constant := 0;
      FS_Offset             : constant := Code_Offset + Code_Byte_Size;
      Sample_Library_Offset : constant := FS_Offset + FS_Byte_Size;

      Code_Base_Addr           : constant := Flash_Base + Code_Offset;
      FS_Base_Addr             : constant := Flash_Base + FS_Offset;
      Sample_Library_Base_Addr : constant := Flash_Base + Sample_Library_Offset;

      pragma Compile_Time_Error
        ((Code_Sectors + Sample_Library_Sectors + FS_Sectors) /= Total_Sectors,
         "Invalid number of used sectors");

      Sectors_Per_Sample       : constant := Sample_Library_Sectors / Nbr_Samples;
      Single_Sample_Byte_Size  : constant := Sample_Library_Byte_Size / Nbr_Samples;
      Single_Sample_Point_Cnt  : constant := Single_Sample_Byte_Size / 2;
      Sample_Name_Lenght       : constant := 14;

   end Storage;
end WNM_Configuration;
