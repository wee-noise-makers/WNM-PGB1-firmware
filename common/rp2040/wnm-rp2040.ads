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

with RP.Clock;
with RP.DMA;
with System;

package WNM.RP2040 is

   I2S_OUT_DMA      : constant RP.DMA.DMA_Channel_Id := 0;
   I2S_IN_DMA       : constant RP.DMA.DMA_Channel_Id := 1;
   Screen_SPI_DMA   : constant RP.DMA.DMA_Channel_Id := 2;
   LED_PIO_DMA      : constant RP.DMA.DMA_Channel_Id := 3;
   MIDI_UART_TX_DMA : constant RP.DMA.DMA_Channel_Id := 4;

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");

   XOSC_Frequency : RP.Clock.XOSC_Hertz := 12_000_000;
end WNM.RP2040;
