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

with HAL; use HAL;

with WNM.Samd51;

with SSD1306.Standard_Resolutions;

package body WNM.Screen is

   --  I2C addr is 16#3C#

   Screen : SSD1306.Standard_Resolutions.SSD1306_128x32_Screen
     (WNM.Samd51.I2C_Port'Access, null, null);

   --   Data : UInt8_Array (0 .. 128 * (32 / 8) - 1);
   Data : UInt8_Array (0 .. 128 * (16 / 8) - 1);

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Screen.Initialize (External_VCC => False);
      Screen.Turn_On;
   end Initialize;

   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      Screen.Write_Raw_Pixels (Data);
   end Update;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Data := (others => 0);
   end Clear;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (Pt : Point; On : Boolean := True) is

      X     : constant Natural := Pt.X;
      Y     : constant Natural := Pt.Y;
      Index : constant Natural := X + (Y / 8) * Screen.Width;
      Byte  : UInt8 renames Data (Data'First + Index);
   begin
      if On then
         Byte := Byte or Shift_Left (1, Y mod 8);
      else
         Byte := Byte and not (Shift_Left (1, Y mod 8));
      end if;
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
            null;
            Set_Pixel ((X + A, Y + B),
                       Data (1 + A + B * Bmp.W) = Invert_Color);
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
