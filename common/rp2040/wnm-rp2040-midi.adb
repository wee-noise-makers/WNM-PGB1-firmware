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

with HAL; use HAL;

with WNM.MIDI.Queues;

with RP.Device;
with RP.UART;
with RP.GPIO; use RP.GPIO;

with BBqueue;         use BBqueue;
with BBqueue.Buffers; use BBqueue.Buffers;
with HAL.UART; use HAL.UART;

package body WNM.RP2040.MIDI is

   UART           : RP.UART.UART_Port renames RP.Device.UART_0;
   DMA_TX_Trigger : RP.DMA.DMA_Request_Trigger := RP.DMA.UART0_TX;
   UART_TX        : RP.GPIO.GPIO_Point := (Pin => 16);
   UART_RX        : RP.GPIO.GPIO_Point := (Pin => 17);

   Out_Grant : BBqueue.Buffers.Read_Grant;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      UART_TX.Configure (Output, Pull_Up, RP.GPIO.UART);
      UART_RX.Configure (Input, Floating, RP.GPIO.UART);
      UART.Configure
        (Config =>
           (Baud      => 31_250,
            Word_Size => 8,
            Parity    => False,
            Stop_Bits => 1,
            others    => <>));

      -- DMA --
      declare
         use RP.DMA;
         Config : DMA_Configuration;
      begin
         Config.Trigger := DMA_TX_Trigger;
         Config.High_Priority := True;
         Config.Data_Size := Transfer_8;
         Config.Increment_Read := True;
         Config.Increment_Write := False;

         RP.DMA.Configure (RP2040.MIDI_UART_TX_DMA, Config);
      end;

   end Initialize;

   ------------
   -- Update --
   ------------

   procedure Update is
      use BBqueue.Buffers;
   begin
      if RP.DMA.Busy (RP2040.MIDI_UART_TX_DMA) then
         --  Previous DMA transfer still in progress
         return;
      end if;

      if State (Out_Grant) = Valid then
         --  Release the previous grant
         WNM.MIDI.Queues.MIDI_Out_Release (Out_Grant);
      end if;

      --  Try to get a new grant
      WNM.MIDI.Queues.MIDI_Out_Read (Out_Grant);

      if State (Out_Grant) = Valid then

         --  If we have a new grant, start DMA transfer

         RP.DMA.Start (Channel => RP2040.MIDI_UART_TX_DMA,
                       From    => Slice (Out_Grant).Addr,
                       To      => UART.FIFO_Address,
                       Count   => UInt32 (Slice (Out_Grant).Length));
      end if;

   end Update;

begin
   Initialize;
end WNM.RP2040.MIDI;
