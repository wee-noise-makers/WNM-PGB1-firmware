-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2021 Fabien Chouteau                  --
--                                                                           --
--    Wee Noise Maker is free software: you can redistribute it and/or       --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Maker is distributed in the hope that it will be useful,     --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with We Noise Maker. If not, see <http://www.gnu.org/licenses/>. --
--                                                                           --
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with WNM_Sim;

with sf.Graphics.Color;

package body WNM.Screen is

   --  SDL_Width : constant := 800;
   --  SDL_Height : constant := 600;
   --
   --  Button_X_Offset : constant := 10;
   --  Button_Y_Offset : constant := 120;
   --  Button_Size : constant := 50;
   --  Button_X_Margin : constant := 5;
   --  Button_Y_Margin : constant := 20;
   --  LED_Size : constant := 10;
   --
   --  Screen_Pixel_Size : constant := 4;
   --
   --  Button_Col : constant array (Button) of Natural :=
   --    (Pattern | Track_Button | Encoder_L => 0,
   --     B1   | B9 | Encoder_R              => 1,
   --     B2   | B10 | Menu                  => 2,
   --     B3   | B11                         => 3,
   --     B4   | B12                         => 4,
   --     B5   | B13                         => 5,
   --     B6   | B14                         => 6,
   --     B7   | B15                         => 7,
   --     B8   | B16                         => 8,
   --     Play | Rec                         => 9,
   --     others                             => 10);
   --
   --  Button_Row : constant array (Button) of Natural :=
   --    (Encoder_L | Encoder_R | Menu   => 0,
   --     B1 .. B8 | Play | Track_Button => 1,
   --     B9 .. B16 | Rec | Pattern      => 2,
   --     others                         => 3);
   --

   Screen_Pixels : array
     (0 .. WNM.Screen.Width - 1, 0 .. WNM.Screen.Height - 1)
     of Boolean := (others => (others => False));

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   ------------
   -- Update --
   ------------

   procedure Update is
      use sf.Graphics.Color;

      FB : array (0 .. (Width * Height) - 1) of sfColor
        with Address => WNM_Sim.Frame_Buffer'Address;
      Pix_Color : constant sfColor := fromRGB (0, 153, 255);
      BG_Color  : constant sfColor := fromRGB (0, 0, 0);
   begin
      FB := (others => BG_Color);

      for X in Screen_Pixels'Range (1) loop
         for Y in Screen_Pixels'Range (2) loop
            if Screen_Pixels (X, Y) then
               FB (X + Y * Width) := Pix_Color;
            end if;
         end loop;
      end loop;
   end Update;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Screen_Pixels := (others => (others => False));
   end Clear;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (Pt : Point; On : Boolean := True) is
   begin
      Screen_Pixels (Pt.X, Pt.Y) := On;
   end Set_Pixel;

   ---------------
   -- Fill_Rect --
   ---------------

   procedure Fill_Rect (R : Rect; On : Boolean := True) is
   begin
      for X in R.Position.X .. R.Position.X + R.Width - 1 loop
         for Y in R.Position.Y .. R.Position.Y + R.Height - 1 loop
            Screen_Pixels (X, Y) := On;
         end loop;
      end loop;
   end Fill_Rect;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (Start, Stop : Point; On : Boolean := True) is
      DX     : constant Float := abs Float (Stop.X - Start.X);
      DY     : constant Float := abs Float (Stop.Y - Start.Y);
      Err    : Float;
      X      : Natural := Start.X;
      Y      : Natural := Start.Y;
      Step_X : Integer := 1;
      Step_Y : Integer := 1;

   begin
      if Start.X > Stop.X then
         Step_X := -1;
      end if;

      if Start.Y > Stop.Y then
         Step_Y := -1;
      end if;

      if DX > DY then
         Err := DX / 2.0;
         while X /= Stop.X loop
            Set_Pixel ((X, Y), On);
            Err := Err - DY;
            if Err < 0.0 then
               Y := Y + Step_Y;
               Err := Err + DX;
            end if;
            X := X + Step_X;
         end loop;
      else
         Err := DY / 2.0;
         while Y /= Stop.Y loop
            Set_Pixel ((X, Y), On);
            Err := Err - DX;
            if Err < 0.0 then
               X := X + Step_X;
               Err := Err + DY;
            end if;
            Y := Y + Step_Y;
         end loop;
      end if;

      Set_Pixel ((X, Y), On);
   end Draw_Line;

   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle (Center : Point; Radius : Natural) is
      F     : Integer := 1 - Radius;
      ddF_X : Integer := 0;
      ddF_Y : Integer := (-2) * Radius;
      X     : Integer := 0;
      Y     : Integer := Radius;
   begin
      Set_Pixel ((Center.X, Center.Y + Radius));
      Set_Pixel ((Center.X, Center.Y - Radius));
      Set_Pixel ((Center.X + Radius, Center.Y));
      Set_Pixel ((Center.X - Radius, Center.Y));

      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;
         Set_Pixel ((Center.X + X, Center.Y + Y));
         Set_Pixel ((Center.X - X, Center.Y + Y));
         Set_Pixel ((Center.X + X, Center.Y - Y));
         Set_Pixel ((Center.X - X, Center.Y - Y));
         Set_Pixel ((Center.X + Y, Center.Y + X));
         Set_Pixel ((Center.X - Y, Center.Y + X));
         Set_Pixel ((Center.X + Y, Center.Y - X));
         Set_Pixel ((Center.X - Y, Center.Y - X));
      end loop;
   end Draw_Circle;

   -----------------
   -- Draw_H_Line --
   -----------------

   procedure Draw_H_Line (Y : Natural; On : Boolean := True) is
   begin
      for X in 0 .. Width - 1 loop
         Screen_Pixels (X, Y) := On;
      end loop;
   end Draw_H_Line;

   ---------------------
   -- Draw_Dot_H_Line --
   ---------------------

   procedure Draw_Dot_H_Line (Y : Natural; On : Boolean := True) is
   begin
      for X in 0 .. Width - 1 loop
         Screen_Pixels (X, Y) := (if (X mod 2) = 0 then On else not On);
      end loop;
   end Draw_Dot_H_Line;


   -----------------
   -- Copy_Bitmap --
   -----------------

   procedure Copy_Bitmap (Bmp          : Bitmap;
                          X, Y         : Integer;
                          Invert_Color : Boolean := False)
   is
      type Bit_Array is array (Positive range <>) of Boolean
        with Pack;

      Data : Bit_Array (1 .. Bmp.W * Bmp.H)
        with Address => Bmp.Data'Address;

   begin
      for A in 0 .. Bmp.W - 1 loop
         for B in 0 .. Bmp.H - 1 loop
            Screen_Pixels (X + A, Y + B) :=
              (Data (1 + A + B * Bmp.W) = Invert_Color);
         end loop;
      end loop;
   end Copy_Bitmap;

   -----------
   -- Sleep --
   -----------

   procedure Sleep is
   begin
      null;
   end Sleep;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup is
   begin
      null;
   end Wakeup;

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down is
   begin
      null;
   end Power_Down;

begin
   Initialize;
end WNM.Screen;
