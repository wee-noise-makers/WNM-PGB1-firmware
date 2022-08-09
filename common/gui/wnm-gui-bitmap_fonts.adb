-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2017 Fabien Chouteau                  --
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

with font_5x7;

with WNM; use WNM;

package body WNM.GUI.Bitmap_Fonts is

   -----------
   -- Print --
   -----------

   procedure Print
     (X_Offset    : in out Integer;
      Y_Offset    : Integer;
      C           : Character;
      Invert_From : Integer := WNM.Screen.Width;
      Invert_To   : Integer := WNM.Screen.Width)
   is
      Index : constant Integer := Character'Pos (C) - Character'Pos ('!');
      Bitmap_Offset : constant Integer := Index * 5;

      function Color (X, Y : Integer) return Boolean;
      function Invert (X : Integer) return Boolean;

      -----------
      -- Color --
      -----------

      function Color (X, Y : Integer) return Boolean is
         type Bit_Array is array (Positive range <>) of Boolean
           with Pack;

         Data : Bit_Array (1 .. font_5x7.BMP.W * font_5x7.BMP.H)
           with Address => font_5x7.BMP.Data'Address;

      begin
         if Index in 0 .. 93 and then X in 0 .. 4 and then Y in 0 .. 6 then
            return not Data (1 + X + Bitmap_Offset + Y * font_5x7.BMP.W);
         else
            return False;
         end if;
      end Color;

      ------------
      -- Invert --
      ------------

      function Invert (X : Integer) return Boolean
      is (X in Invert_From .. Invert_To);

   begin
      Draw_Loop : for X in 0 .. 5 loop
         for Y in 0 .. 6 loop

            if Y + Y_Offset in 0 .. Screen.Height - 1 then
               if X + X_Offset > Screen.Width - 1 then
                  exit Draw_Loop;
               elsif X + X_Offset >= 0
                 and then
                   Y + Y_Offset in 0 .. Screen.Height - 1
               then
                  Screen.Set_Pixel ((X + X_Offset, Y + Y_Offset),
                                    Color (X, Y) xor Invert (X + X_Offset));
               end if;
            end if;
         end loop;
      end loop Draw_Loop;

      X_Offset := X_Offset + Width;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (X_Offset    : in out Integer;
      Y_Offset    : Integer;
      Str         : String;
      Invert_From : Integer := WNM.Screen.Width;
      Invert_To   : Integer := WNM.Screen.Width)
   is
      Stop  : Integer;
      Start : Integer;
   begin
      if Invert_From < X_Offset then
         Stop := Integer'Min (X_Offset, Invert_To);

         Screen.Fill_Rect ((Position => (Invert_From, Y_Offset),
                            Width    => Stop - Invert_From,
                            Height   => Height));
      end if;

      for C of Str loop
         if X_Offset > Screen.Width then
            return;
         end if;
         Print (X_Offset,
                Y_Offset,
                C,
                Invert_From,
                Invert_To);
      end loop;

      if Invert_From < 96 and then X_Offset < Invert_To then
         Start :=  Integer'Max (X_Offset, Invert_From);

         Screen.Fill_Rect ((Position => (Start, Y_Offset),
                            Width    => Invert_To - Start,
                            Height   => Height));
      end if;
   end Print;

end WNM.GUI.Bitmap_Fonts;
