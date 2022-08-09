with Littlefs; use Littlefs;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;

with Ada.Streams.Stream_IO;

with Ada.Streams; use Ada.Streams;

with VirtAPU;

with WNM.Audio;

package body WNM.Factory_Reset is

   procedure Create_Sample_From_VirtAPU (FS       : aliased in out Littlefs.LFS_T;
                                         Filename : String;
                                         Note     : VirtAPU.Frequency;
                                         Size     : Natural);

   procedure Copy_From_Host (FS : aliased in out Littlefs.LFS_T;
                             Hostpath, Localpath : String);
   pragma Unreferenced (Copy_From_Host);

   --------------------------------
   -- Create_Sample_From_VirtAPU --
   --------------------------------

   procedure Create_Sample_From_VirtAPU (FS       : aliased in out Littlefs.LFS_T;
                                         Filename : String;
                                         Note     : VirtAPU.Frequency;
                                         Size     : Natural)
   is
      use WNM.Audio;

      APU : VirtAPU.Instance (1, WNM.Sample_Frequency);

      type Buffer_Type is array (Natural range <>) of Mono_Point;
      procedure Next_Samples is new VirtAPU.Next_Samples_Int
        (Mono_Point,
         Buffer_Type);

      Buf : Buffer_Type (1 .. WNM.Samples_Per_Buffer);
      Count : Natural := 0;

      Local_FD : aliased LFS_File;
      Len : Littlefs.LFS_Signed_Size;
      Error : int;
   begin
      APU.Set_Rhythm (120, WNM.Sample_Frequency / Buf'Length);
      APU.Set_Mode (1, VirtAPU.Pulse);
      APU.Set_Volume (1, 10);
      APU.Set_Sweep (Chan_Id     => 1,
                     Kind        => VirtAPU.Down,
                     Sweep_Len   => 2,
                     Sweep_Ticks => 10);
      APU.Note_On (1, Note);
      Error := Littlefs.Open (FS, Local_FD, Filename, LFS_O_WRONLY + LFS_O_CREAT);
      if Error /= 0 then
         raise Program_Error with "Create file error:" & Error'Img;
      end if;

      loop

         APU.Set_Width
           (1, VirtAPU.Pulse_Width (10 + Natural'Min (80, Count / Buf'Length)));

         Next_Samples (APU, Buf);
         APU.Tick;

         Len :=  Write (FS, Local_FD,
                        Buf'Address,
                        Buf'Length * 2);

         if Len /= Buf'Length * 2 then
            raise Program_Error with "Create_Sample_From_VirtAPU error:" & Len'Img;
         end if;
         Count := Count + Buf'Length * 2;
         exit when Count >= Size;

      end loop;
      if Close (FS, Local_FD) /= 0 then
         raise Program_Error with "Close error...";
      end if;
   end Create_Sample_From_VirtAPU;


   --------------------
   -- Copy_From_Host --
   --------------------

   procedure Copy_From_Host (FS : aliased in out Littlefs.LFS_T;
                             Hostpath, Localpath : String) is
      Local_FD : aliased LFS_File;

      Host_File : Ada.Streams.Stream_IO.File_Type;
      Host_Stream : Ada.Streams.Stream_IO.Stream_Access;

      Buffer : Stream_Element_Array (1 .. 512);
      Last   : Stream_Element_Offset;
      Len : Littlefs.LFS_Signed_Size;
   begin
      if Littlefs.Open (FS, Local_FD, Localpath, LFS_O_WRONLY + LFS_O_CREAT) /= 0 then
         raise Program_Error with "Create file error...";
      end if;

      Ada.Streams.Stream_IO.Open (Host_File,
                                  Ada.Streams.Stream_IO.In_File,
                                  Hostpath);

      Host_Stream := Ada.Streams.Stream_IO.Stream (Host_File);

      loop

         Host_Stream.Read (Buffer, Last);

         exit when Last < Buffer'Last;

         Len :=  Write (FS, Local_FD,
                        Buffer'Address,
                        Buffer'Length);

         if Len /= Buffer'Length then
            raise Program_Error with "Copy_From_Host error:" & Len'Img;
         end if;

      end loop;

      if Close (FS, Local_FD) /= 0 then
         raise Program_Error with "Close error...";
      end if;

      Ada.Streams.Stream_IO.Close (Host_File);

   end Copy_From_Host;

   -----------
   -- Reset --
   -----------

   procedure Reset (FS : aliased in out Littlefs.LFS_T) is
   begin
      if Littlefs.Mkdir (FS, "samples") /= 0 then
         raise Program_Error with "Mkdir error...";
      end if;

      if Littlefs.Mkdir (FS, "projects") /= 0 then
         raise Program_Error with "Mkdir error...";
      end if;

      --  Copy_From_Host (FS, "/tmp/wnm-samples/BT0A0A7.WAV.raw", "/samples/kick");
      --  Copy_From_Host (FS, "/tmp/wnm-samples/ST0T0S0.WAV.raw", "/samples/snare");
      --  Copy_From_Host (FS, "/tmp/wnm-samples/HANDCLP1.WAV.raw", "/samples/clap");
      --  Copy_From_Host (FS, "/tmp/wnm-samples/RIM63.WAV.raw", "/samples/rim");
      --  Copy_From_Host (FS, "/tmp/wnm-samples/HHOD6.WAV.raw", "/samples/hho");
      --  Copy_From_Host (FS, "/tmp/wnm-samples/HHCD6.WAV.raw", "/samples/hhc");
      --  Copy_From_Host (FS, "/tmp/wnm-samples/CSHD4.WAV.raw", "/samples/crash");
      --  Copy_From_Host (FS, "/tmp/wnm-samples/RIDED6.WAV.raw", "/samples/ride");

      Create_Sample_From_VirtAPU (FS, "/samples/A4", VirtAPU.A4.Freq, WNM.Sample_Frequency);
      Create_Sample_From_VirtAPU (FS, "/samples/B4", VirtAPU.B4.Freq, WNM.Sample_Frequency);
      Create_Sample_From_VirtAPU (FS, "/samples/C5", VirtAPU.C5.Freq, WNM.Sample_Frequency);
      Create_Sample_From_VirtAPU (FS, "/samples/D5", VirtAPU.D5.Freq, WNM.Sample_Frequency);
      Create_Sample_From_VirtAPU (FS, "/samples/E5", VirtAPU.E5.Freq, WNM.Sample_Frequency);
      Create_Sample_From_VirtAPU (FS, "/samples/F5", VirtAPU.F5.Freq, WNM.Sample_Frequency);
      Create_Sample_From_VirtAPU (FS, "/samples/G5", VirtAPU.G5.Freq, WNM.Sample_Frequency);
      Create_Sample_From_VirtAPU (FS, "/samples/A5", VirtAPU.A5.Freq, WNM.Sample_Frequency);
   end Reset;

end WNM.Factory_Reset;
