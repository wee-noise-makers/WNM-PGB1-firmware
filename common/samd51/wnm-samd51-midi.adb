with HAL;      use HAL;
with HAL.GPIO;

with SAM.SERCOM.USART;
with SAM.Port;
with SAM.Clock_Generator.IDs;
with SAM.Functions;
with SAM.Main_Clock;
with SAM.DMAC; use SAM.DMAC;
with SAM.DMAC.Sources;

with BBqueue.Buffers;

with WNM.MIDI;
with WNM.MIDI.Queues;

package body WNM.Samd51.MIDI is

   UART      : SAM.SERCOM.USART.USART_Device renames SAM.Device.USART5;
   RX        : SAM.Port.GPIO_Point renames SAM.Device.PB17;
   TX        : SAM.Port.GPIO_Point renames SAM.Device.PB16;
   Out_Grant : BBqueue.Buffers.Read_Grant;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin

      -- IOs --

      RX.Clear;
      RX.Set_Mode (HAL.GPIO.Output);
      RX.Set_Pull_Resistor (HAL.GPIO.Floating);
      RX.Set_Function (SAM.Functions.PB17_SERCOM5_PAD1);

      TX.Clear;
      TX.Set_Mode (HAL.GPIO.Output);
      TX.Set_Pull_Resistor (HAL.GPIO.Floating);
      TX.Set_Function (SAM.Functions.PB16_SERCOM5_PAD0);

      -- Clocks --
      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.SERCOM5_CORE,
         SAM.Clock_Setup_120Mhz.Clk_48Mhz);

      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.SERCOM5_SLOW,
         SAM.Clock_Setup_120Mhz.Clk_32Khz);

      -- USART --

      SAM.Main_Clock.SERCOM5_On;

      UART.Configure (Baud            => 64_855, -- MIDI should be 31,250 bits/s
                      MSB_First       => False,
                      TX_Fall_RX_Rise => False,
                      Parity          => False,
                      Synchronous_Com => False,
                      RXPO            => 1,
                      TXPO            => 2,
                      Run_In_Standby  => False);

      UART.Debug_Stop_Mode (Enabled => True);

      UART.Enable;
      UART.Enable_Transmitter;

      -- DMA --

      Configure (DMA_MIDI_OUT,
                 Trig_Src       => SAM.DMAC.Sources.SERCOM5_TX,
                 Trig_Action    => Burst,
                 Priority       => 0,
                 Burst_Len      => 1,
                 Threshold      => BEAT_1,
                 Run_In_Standby => False);

      Configure_Descriptor (DMA_Descs (DMA_MIDI_OUT),
                            Valid           => True,
                            Event_Output    => Disable,
                            Block_Action    => Interrupt,
                            Beat_Size       => B_8bit,
                            Src_Addr_Inc    => True,
                            Dst_Addr_Inc    => False,
                            Step_Selection  => Source,
                            Step_Size       => X1);
   end Initialize;

   ------------
   -- Update --
   ------------

   function Update return Time.Time_Ms is
      use BBqueue;
      use BBqueue.Buffers;
   begin

      if not Pending (DMA_MIDI_OUT) then

         if State (Out_Grant) = BBqueue.Valid then
            --  Release the previous slice, if any
            WNM.MIDI.Queues.MIDI_Out_Release (Out_Grant);
         end if;

         --  Try to get a new slice
         WNM.MIDI.Queues.MIDI_Out_Read (Out_Grant);

         if State (Out_Grant) = BBqueue.Valid then

            --  Start transfer with the new slice, if any

            Set_Data_Transfer
              (DMA_Descs (DMA_MIDI_OUT),
               Block_Transfer_Count => UInt16 (Slice (Out_Grant).Length),
               Src_Addr             => Slice (Out_Grant).Addr,
               Dst_Addr             => UART.Data_Address);

            Enable (DMA_MIDI_OUT);
         end if;
      end if;

      return Time.Time_Ms'First;
   end Update;

begin
   Initialize;
end WNM.Samd51.MIDI;
