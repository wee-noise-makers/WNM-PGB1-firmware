with Interfaces; use Interfaces;
with Ada.Streams.Stream_IO; use Ada.Streams;
with Ada.Text_IO;

package BMP is
   type Header is record
      Signature : Integer_16;
      Size      : Integer_32;
      Reserved1 : Integer_16;
      Reserved2 : Integer_16;
      Offset    : Integer_32;
   end record;

   type Info is record
      Struct_Size   : Integer_32;
      Width         : Integer_32; -- Image width in pixels
      Height        : Integer_32; -- Image hieght in pixels
      Planes        : Integer_16;
      Pixel_Size    : Integer_16; -- Bits per pixel
      Compression   : Integer_32; -- Zero means no compression
      Image_Size    : Integer_32; -- Size of the image data in bytes
      PPMX          : Integer_32; -- Pixels per meter in x led
      PPMY          : Integer_32; -- Pixels per meter in y led
      Palette_Size  : Integer_32; -- Number of colors
      Important     : Integer_32;
   end record;

   type Pixel_RGB24 is record
     B, G, R : Unsigned_8;
   end record with Pack, Size => 24;

   type Image_RGB24 is array (Integer range <>) of Pixel_RGB24 with Pack;

   type Unsigned_1 is mod 2**1 with Size => 1;
   type Unsigned_2 is mod 2**2 with Size => 2;
   type Unsigned_4 is mod 2**4 with Size => 4;

   type P1_Array is array (Integer range <>) of Unsigned_1 with Pack;
   type P2_Array is array (Integer range <>) of Unsigned_2 with Pack;
   type P4_Array is array (Integer range <>) of Unsigned_4 with Pack;
   type P8_Array is array (Integer range <>) of Unsigned_8 with Pack;

   type Pix_Index_Size is (Pix_1, Pix_2, Pix_4, Pix_8);

   type Pix_Index_Rec (Size : Pix_Index_Size := Pix_8) is record
      case Size is
         when Pix_1 => P1 : P1_Array (0 .. 7);
         when Pix_2 => P2 : P2_Array (0 .. 3);
         when Pix_4 => P4 : P4_Array (0 .. 1);
         when Pix_8 => P8 : P8_Array (0 .. 0);
      end case;
   end record with Pack, Size => 8, Unchecked_Union;

   type Pix_Index_Array is array (Integer range <>) of Pix_Index_Rec;

   type Color_Definition is record
      B, G, R, A : Unsigned_8;
   end record with Pack, Size => 4 * 8;

   type Palette is array (Unsigned_8 range <>) of Color_Definition with Pack;

   procedure Standard
     (File_In : Stream_IO.File_Type;
      Input    : Stream_IO.Stream_Access;
      File_Out : Ada.Text_IO.File_Type;
      Package_Name : String;
      Header   : BMP.Header;
      Info     : BMP.Info;
      Row_Size : Integer);

   procedure Palettized
     (File_In : Stream_IO.File_Type;
      Input    : Stream_IO.Stream_Access;
      File_Out : Ada.Text_IO.File_Type;
      Package_Name : String;
      Header   : BMP.Header;
      Info     : BMP.Info;
      Row_Size : Integer);

   DMA2D_Format : Boolean := False;
   Verbose : Boolean := False;
end BMP;
