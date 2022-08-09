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

with Interfaces;

package WNM.Screen is

   Width  : constant := 128;
   Height : constant := 64;

   procedure Update;

   procedure Clear;
   type Point is record
      X, Y : Natural;
   end record;
   type Rect is record
      Position : Point;
      Width, Height : Natural;
   end record;

   procedure Set_Pixel (Pt : Point; On : Boolean := True);
   procedure Fill_Rect (R : Rect; On : Boolean := True);
   procedure Draw_Line (Start, Stop : Point; On : Boolean := True);
   procedure Draw_Circle (Center : Point; Radius : Natural);
   procedure Draw_H_Line (Y : Natural; On : Boolean := True);
   procedure Draw_Dot_H_Line (Y : Natural; On : Boolean := True);

   type Unsigned_8_Array is array (Positive range <>) of Interfaces.Unsigned_8;

   type Bitmap (Length_Byte : Positive) is record
      Data : Unsigned_8_Array (1 .. Length_Byte);
      W, H : Positive;
   end record;

   procedure Copy_Bitmap (Bmp          : Bitmap;
                          X, Y         : Integer;
                          Invert_Color : Boolean := False);

   procedure Sleep;
   procedure Wakeup;

   procedure Power_Down;

private

   pragma Inline_Always (Set_Pixel);
   pragma Inline_Always (Draw_H_Line);
   pragma Inline_Always (Draw_Dot_H_Line);

   type Screen_Mode is (Text, Parameter);

end WNM.Screen;
