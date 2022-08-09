with SAM.DMAC;
with SAM.Clock_Generator; use SAM.Clock_Generator;
with SAM.Clock_Setup_120Mhz;
with SAM.Device;
with SAM.Port;
with SAM.SERCOM.I2C;

package WNM.Samd51 is
   pragma Elaborate_Body;

   Clk_CPU    : constant Generator_Id := SAM.Clock_Setup_120Mhz.Clk_CPU;
   Clk_120Mhz : constant Generator_Id := SAM.Clock_Setup_120Mhz.Clk_120Mhz;
   Clk_48Mhz  : constant Generator_Id := SAM.Clock_Setup_120Mhz.Clk_48Mhz;
   Clk_32Khz  : constant Generator_Id := SAM.Clock_Setup_120Mhz.Clk_32Khz;
   Clk_2Mhz   : constant Generator_Id := SAM.Clock_Setup_120Mhz.Clk_2Mhz;

   DMA_Descs : aliased SAM.DMAC.Descriptor_Section;
   DMA_WB : aliased SAM.DMAC.Descriptor_Section;

   DMA_DAC_0   : constant SAM.DMAC.Channel_Id := 0;
   DMA_DAC_1   : constant SAM.DMAC.Channel_Id := 2;
   DMA_ADC_0   : constant SAM.DMAC.Channel_Id := 3;
   DMA_ADC_1   : constant SAM.DMAC.Channel_Id := 4;

   --  We use 1 because there are only 5 interrupt vectors for the DMAC and we
   --  want a dedicated handler for the LED SPI.
   DMA_LED_SPI : constant SAM.DMAC.Channel_Id := 1;

   DMA_MIDI_OUT : constant SAM.DMAC.Channel_Id := 5;

   I2C_Port : SAM.SERCOM.I2C.I2C_Device renames SAM.Device.I2C2;
   SCL   : SAM.Port.GPIO_Point renames SAM.Device.PA13;
   SDA   : SAM.Port.GPIO_Point renames SAM.Device.PA12;

end WNM.Samd51;
