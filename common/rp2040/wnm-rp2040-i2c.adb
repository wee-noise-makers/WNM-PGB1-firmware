-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with RP.Device;
with RP.I2C_Master;
with RP.GPIO;

with WNM.Screen;
with WNM.GUI.Bitmap_Fonts;

package body WNM.RP2040.I2C is

   I2C_Port : RP.I2C_Master.I2C_Master_Port renames RP.Device.I2C_1;
   I2C_SDA  : RP.GPIO.GPIO_Point := (Pin => 18);
   I2C_SCL  : RP.GPIO.GPIO_Point := (Pin => 19);

   ----------
   -- Port --
   ----------

   function Port return not null HAL.I2C.Any_I2C_Port is
   begin
      return I2C_Port'Access;
   end Port;

   -------------------------
   -- Display_Device_Scan --
   -------------------------

   procedure Display_Device_Scan is
      use HAL.I2C;

      X, Y : Integer := 0;
      Status : HAL.I2C.I2C_Status;
   begin
      Screen.Clear;
      GUI.Bitmap_Fonts.Print (X_Offset    => X,
                              Y_Offset    => 0,
                              Str         => "I2C devices:");

      X := 0;
      Y := 9;
      for Addr in HAL.I2C.I2C_Address loop

         I2C_Port.Master_Transmit (Addr    => Addr,
                                   Data    => (0 => 0),
                                   Status  => Status);

         if Status = Ok then
            GUI.Bitmap_Fonts.Print (X_Offset    => X,
                                    Y_Offset    => Y,
                                    Str         => Addr'Img & ",");

            if X >= (Screen.Width - 24) then
               X := 0;
               Y := Y + 9;
            end if;
         end if;
      end loop;
      Screen.Update;
   end Display_Device_Scan;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use RP.GPIO;
   begin
      I2C_Port.Enable (400_000);

      I2C_SDA.Configure (Output, Pull_Up, RP.GPIO.I2C);
      I2C_SCL.Configure (Output, Pull_Up, RP.GPIO.I2C);

   end Initialize;

begin
   Initialize;
end WNM.RP2040.I2C;
