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

with RP.PIO; use RP.PIO;
with RP.Device;

with WNM.RP2040.WS2812_PIO_ASM; use WNM.RP2040.WS2812_PIO_ASM;
with WNM.RP2040.PIO_Rotary_Encoder_ASM; use WNM.RP2040.PIO_Rotary_Encoder_ASM;

package WNM.RP2040.PIO is
   pragma Elaborate_Body;

   -- PIO 0 --

   Encoder_PIO    : PIO_Device renames RP.Device.PIO_0;
   Encoder_L_SM   : constant PIO_SM := 0;
   Encoder_R_SM   : constant PIO_SM := 1;
   Encoder_Offset : constant PIO_Address := 0;
   Encoder_Last   : constant PIO_Address :=
     Encoder_Offset + Pio_Rotary_Encoder_Program_Instructions'Length - 1;

   WS2812_PIO    : PIO_Device renames RP.Device.PIO_0;
   WS2812_SM     : constant PIO_SM := 2;
   WS2812_Offset : constant PIO_Address :=
     PIO_Address'Last - Ws2812_Program_Instructions'Length + 1;
   WS2812_DMA_Trigger : constant RP.DMA.DMA_Request_Trigger := RP.DMA.PIO0_TX2;

   pragma Compile_Time_Error (Encoder_Last >= WS2812_Offset,
                              "PIO programs do not fit in memory");

   -- PIO 1 --

   I2S_PIO    : PIO_Device renames RP.Device.PIO_1;
   I2S_SM     : constant PIO_SM := 0;
   I2S_Offset : constant PIO_Address := 0;
   I2S_OUT_DMA_Trigger : constant RP.DMA.DMA_Request_Trigger := RP.DMA.PIO1_TX0;
   I2S_IN_DMA_Trigger  : constant RP.DMA.DMA_Request_Trigger := RP.DMA.PIO1_RX0;

end WNM.RP2040.PIO;
