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
with RP.PIO; use RP.PIO;
with RP.GPIO; use RP.GPIO;
with RP.DMA;

with WNM.RP2040.WS2812_PIO_ASM;
with WNM.RP2040.PIO; use WNM.RP2040.PIO;

package body WNM.RP2040.WS2812 is

   Out_Pin : RP.GPIO.GPIO_Point := (Pin => 3);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Config         : PIO_SM_Config := Default_SM_Config;

      Freq           : constant := 80_0000;
      Cycles_Per_Bit : constant := WS2812_PIO_ASM.T1 +
        WS2812_PIO_ASM.T2 + WS2812_PIO_ASM.T3;

      Bit_Per_LED : constant := 24;

   begin
      Out_Pin.Configure (Output, Pull_Up, WS2812_PIO.GPIO_Function);

      WS2812_PIO.Load (WS2812_PIO_ASM.Ws2812_Program_Instructions,
                       Offset => WS2812_Offset);

      WS2812_PIO.Set_Pin_Direction (WS2812_SM, Out_Pin.Pin, Output);

      Set_Sideset (Config,
                   Bit_Count => 1,
                   Optional  => False,
                   Pindirs   => False);
      Set_Sideset_Pins (Config, Sideset_Base => Out_Pin.Pin);

      Set_Out_Shift (Config,
                     Shift_Right    => True,
                     Autopull       => True,
                     Pull_Threshold => Bit_Per_LED);
      Set_FIFO_Join (Config,
                     Join_TX => True,
                     Join_RX => False);

      Set_Wrap (Config,
                WS2812_Offset + WS2812_PIO_ASM.Ws2812_Wrap_Target,
                WS2812_Offset + WS2812_PIO_ASM.Ws2812_Wrap);
      Set_Clock_Frequency (Config, Freq * Cycles_Per_Bit);

      WS2812_PIO.SM_Initialize (WS2812_SM,
                                WS2812_Offset,
                                Config);
      WS2812_PIO.Set_Enabled (WS2812_SM, True);

      -- DMA --
      declare
         use RP.DMA;
         Config : DMA_Configuration;
      begin
         Config.Trigger := WS2812_DMA_Trigger;
         Config.Data_Size := Transfer_32;
         Config.Increment_Read := True;
         Config.Increment_Write := False;

         RP.DMA.Configure (RP2040.LED_PIO_DMA, Config);
      end;

   end Initialize;

   -------------------
   -- Push_Data_DMA --
   -------------------

   procedure Push_Data_DMA (Data : not null LED_Data_Access) is
   begin
      if RP.DMA.Busy (RP2040.LED_PIO_DMA) then
         --  Previous DMA transfer still in progress
         return;
      end if;

      RP.DMA.Start (Channel => RP2040.LED_PIO_DMA,
                    From    => Data.all'Address,
                    To      => WS2812_PIO.TX_FIFO_Address (WS2812_SM),
                    Count   => Data.all'Length);
   end Push_Data_DMA;

begin
   Initialize;
end WNM.RP2040.WS2812;
