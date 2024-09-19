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

package body WNM.Screen is

   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      Update_Screen;
   end Update;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Clear_Pixels;
   end Clear;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (Pt : Point; On : Boolean := True) is
   begin
      pragma Warnings (Off, "lower bound check");
      if Pt.X in Natural (Pix_X'First) .. Natural (Pix_X'Last)
        and then
         Pt.Y in Natural (Pix_Y'First) .. Natural (Pix_Y'Last)
      then
         WNM_HAL.Set_Pixel (Pix_X (Pt.X), Pix_Y (Pt.Y), On);
      else
         raise Program_Error with "Pixel out of bounds: (" & Pt.X'Img &
           ", " & Pt.Y'Img & ")";
      end if;
      pragma Warnings (On, "lower bound check");
   end Set_Pixel;

   ---------------
   -- Fill_Rect --
   ---------------

   procedure Fill_Rect (R : Rect; On : Boolean := True) is
   begin
      for X in R.Position.X .. R.Position.X + R.Width - 1 loop
         for Y in R.Position.Y .. R.Position.Y + R.Height - 1 loop
            Set_Pixel ((X, Y), On);
         end loop;
      end loop;
   end Fill_Rect;

   ---------------
   -- Draw_Rect --
   ---------------

   procedure Draw_Rect (R : Rect; On : Boolean := True) is
      TL : constant Point :=
        (R.Position.X, R.Position.Y);
      TR : constant Point :=
        (R.Position.X + R.Width - 1, R.Position.Y);
      BL : constant Point :=
        (R.Position.X, R.Position.Y + R.Height - 1);
      BR : constant Point :=
        (R.Position.X + R.Width - 1, R.Position.Y + R.Height - 1);
   begin
      Draw_Line (TL, TR, On => On);
      Draw_Line (TR, BR, On => On);
      Draw_Line (BL, BR, On => On);
      Draw_Line (TL, BL, On => On);
   end Draw_Rect;

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
         Set_Pixel ((X, Y), On);
      end loop;
   end Draw_H_Line;

   ---------------------
   -- Draw_Dot_H_Line --
   ---------------------

   procedure Draw_Dot_H_Line (Y : Natural; On : Boolean := True) is
   begin
      for X in 0 .. Width - 1 loop
         Set_Pixel ((X, Y), (if (X mod 2) = 0 then On else not On));
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
            Set_Pixel ((X + A, Y + B),
                       Data (1 + A + B * Bmp.W) /= Invert_Color);
         end loop;
      end loop;
   end Copy_Bitmap;

   ---------------------
   -- Copy_Bitmap_Sub --
   ---------------------

   procedure Copy_Bitmap_Sub (Bmp          : Bitmap;
                              X, Y         : Integer;
                              Sub          : Rect;
                              Invert_Color : Boolean := False)
   is
      type Bit_Array is array (Positive range <>) of Boolean
        with Pack;

      Data : Bit_Array (1 .. Bmp.W * Bmp.H)
        with Address => Bmp.Data'Address;

   begin
      for A in 0 .. Sub.Width - 1 loop
         for B in 0 .. Sub.Height - 1 loop
            Set_Pixel ((X + A, Y + B),
                       Data (1 + Sub.Position.X + A +
                         (Sub.Position.Y + B) * Bmp.W) /= Invert_Color);
         end loop;
      end loop;
   end Copy_Bitmap_Sub;

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
end WNM.Screen;
