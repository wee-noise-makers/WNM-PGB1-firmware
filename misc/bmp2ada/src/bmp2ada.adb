with GNAT.Command_Line; use GNAT.Command_Line;
with Ada; use Ada;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO;
use Ada.Streams;
with BMP; use BMP;
with Interfaces; use Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;
with GNAT.OS_Lib;

procedure BMP2Ada is
   File_In  : Stream_IO.File_Type;
   File_Out : Ada.Text_IO.File_Type;
   Input    : Stream_IO.Stream_Access;
   Header   : BMP.Header;
   Info     : BMP.Info;
   Row_Size, Row_Padding : Integer;

   Output_Dir : Unbounded_String := Null_Unbounded_String;
   Output_Filename : Unbounded_String := Null_Unbounded_String;

   function Get_Obj_Dir (Filename : String) return Boolean;
   procedure Print_Help;
   -----------------
   -- Get_Obj_Dir --
   -----------------

   function Get_Obj_Dir (Filename : String) return Boolean is
      File       : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Filename);
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
      begin
         Ada.Text_IO.Close (File);

         if Line'Length /= 0 then
            Text_IO.Put_Line ("Output directory: '" & Line & "'");
            Output_Dir := To_Unbounded_String (Line);
            return True;
         else
            return False;
         end if;
      end;
   end Get_Obj_Dir;

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
   begin
      Put_Line ("bmp2ada -o <output_filename> <input_bitmap_file>");
      Put_Line (" --dma2d to enable STM32 DMAD2 format");
   end Print_Help;
begin
   loop
      case Getopt ("o: d: v h -help -dma2d") is
      when 'o' =>
         Output_Filename := To_Unbounded_String (Parameter);
         Put_Line ("Output filename:" & Parameter);
      when 'd' =>
         if not Get_Obj_Dir (Parameter) then
            return;
         end if;
      when 'v' =>
         BMP.Verbose := True;
      when 'h' =>
         Print_Help;
         return;
      when '-' =>
         if Full_Switch = "-help" then
            Print_Help;
            return;
         elsif Full_Switch = "-dma2d" then
            DMA2D_Format := True;
         end if;
      when others =>
         exit;
      end case;
   end loop;

   declare
      Input_Filename  : constant String := Get_Argument (Do_Expansion => True);
      Output_Path : constant String :=
        To_String (Output_Dir & Output_Filename);
      Package_Name    : constant String :=
        Ada.Directories.Base_Name (Input_Filename);
   begin
      if Input_Filename'Length = 0 then
         Put_Line ("Input filename missing...");
         GNAT.OS_Lib.OS_Exit (-1);
      end if;

      Put_Line ("Openning input " & Input_Filename);
      Stream_IO.Open (File_In, Stream_IO.In_File, Input_Filename);
      Input := Stream_IO.Stream (File_In);

      Put_Line ("Creating output " & Output_Path);
      Text_IO.Create (File_Out, Text_IO.Out_File, Output_Path);

      Put_Line ("Creating package: " & Package_Name);

      BMP.Header'Read (Input, Header);
      Text_IO.Put_Line ("Signature " & Header.Signature'Img);
      Text_IO.Put_Line ("Size " & Header.Size'Img);
      Text_IO.Put_Line ("Reserved1 " & Header.Reserved1'Img);
      Text_IO.Put_Line ("Reserved2 " & Header.Reserved2'Img);
      Text_IO.Put_Line ("Offset " & Header.Offset'Img);

      BMP.Info'Read (Input, Info);
      Text_IO.Put_Line ("Struct_Size " & Info.Struct_Size'Img);
      Text_IO.Put_Line ("Width " & Info.Width'Img);
      Text_IO.Put_Line ("Height " & Info.Height'Img);
      Text_IO.Put_Line ("Planes " & Info.Planes'Img);
      Text_IO.Put_Line ("Pixel_Size " & Info.Pixel_Size'Img);
      Text_IO.Put_Line ("Compression " & Info.Compression'Img);
      Text_IO.Put_Line ("Image_Size " & Info.Image_Size'Img);
      Text_IO.Put_Line ("PPMX " & Info.PPMX'Img);
      Text_IO.Put_Line ("PPMY " & Info.PPMY'Img);
      Text_IO.Put_Line ("Palette_Size " & Info.Palette_Size'Img);
      Text_IO.Put_Line ("Important " & Info.Important'Img);

      Row_Size := Integer (Info.Width) * Integer (Info.Pixel_Size);
      Row_Padding :=  (32 - (Row_Size mod 32)) mod 32;
      Row_Size := (Row_Size + Row_Padding) / 8;
      Row_Padding := Row_Padding / 8;

      Text_IO.Put_Line ("Row_Size " & Row_Size'Img);
      Text_IO.Put_Line ("Row_Padding " & Row_Padding'Img);

      if Info.Compression /= 0 then
         Put_Line ("Compression not supported.");
         GNAT.OS_Lib.OS_Exit (-1);
      end if;

      if DMA2D_Format then
         case Info.Pixel_Size is
            when 1 | 2 | 4 | 8 | 24 =>
               null;
            when others =>
               Put_Line ("Only 2, 16 and 256 palette size support supported" &
                           "for DMA2D output.");
               GNAT.OS_Lib.OS_Exit (-1);
         end case;
      end if;

      if Info.Palette_Size /= 0 then
         BMP.Palettized (File_In,
                         Input,
                         File_Out,
                         Package_Name,
                         Header,
                         Info,
                         Row_Size);
      else
         BMP.Standard (File_In,
                       Input,
                       File_Out,
                       Package_Name,
                       Header,
                       Info,
                       Row_Size);
      end if;
      Ada.Text_IO.Close (File_Out);
      Stream_IO.Close (File_In);
   end;
end BMP2Ada;
