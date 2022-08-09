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

with STM32.Device;                 use STM32.Device;
with STM32.GPIO;                   use STM32.GPIO;
with HAL;                          use HAL;
with HAL.GPIO;                     use HAL.GPIO;
with SSD1306.Standard_Resolutions; use SSD1306.Standard_Resolutions;
with Ravenscar_Time;
with HAL.Bitmap;                   use HAL.Bitmap;

with WNM.I2C;

with Giza.Colors;

package body WNM.Screen is

   Screen_Reset : STM32.GPIO.GPIO_Point renames PA4;

   Screen : SSD1306_96x16_Screen (WNM.I2C.Port,
                                  Screen_Reset'Access,
                                  Ravenscar_Time.Delays);

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Unref : Boolean with Unreferenced;

   begin
      Enable_Clock (Screen_Reset);
      Unref := Screen_Reset.Set_Mode (Output);

      Screen.Initialize (External_VCC => False);
      Screen.Initialize_Layer (1, M_1);

      Wakeup;
   end Initialize;

   -----------------
   -- Copy_Bitmap --
   -----------------

   procedure Copy_Bitmap (Bmp          : Giza.Bitmaps.Indexed_1bit.Bitmap_Indexed;
                          X, Y         : Integer;
                          Invert_Color : Boolean := False)
   is
      use type Giza.Colors.RGB_Component;

      Giza_Color : Giza.Colors.Color;
      On         : Boolean;
   begin
      for W in 0 .. Bmp.W - 1 loop
         for H in 0 .. Bmp.H - 1 loop
            Giza_Color := Giza.Bitmaps.Indexed_1bit.Get_Pixel (Bmp, (W, H));
            On := Giza_Color.R /= 0;

            if Invert_Color then
               On := not On;
            end if;

            Screen.Hidden_Buffer (1).Set_Pixel ((X + W, Y + H),
                                                (if On then White else Black));
         end loop;
      end loop;

   end Copy_Bitmap;

   -----------
   -- Sleep --
   -----------

   procedure Sleep is
   begin
      Screen.Turn_Off;
   end Sleep;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup is
   begin
      Screen.Turn_On;
   end Wakeup;

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down is
   begin
      Sleep;
      Screen_Reset.Clear;
   end Power_Down;


   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      Screen.Update_Layers;
   end Update;

   ------------
   -- Buffer --
   ------------

   function Buffer return not null HAL.Bitmap.Any_Bitmap_Buffer
   is (Screen.Hidden_Buffer (1));

begin
   Initialize;
end WNM.Screen;
