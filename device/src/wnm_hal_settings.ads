with RP.PIO; use RP.PIO;
with RP.Device;
with RP.Clock;
with RP.DMA; use RP.DMA;

with WNM_HAL_PIO_Encoders_ASM; use WNM_HAL_PIO_Encoders_ASM;

with Noise_Nugget_SDK;
pragma Unreferenced (Noise_Nugget_SDK);

package WNM_HAL_Settings
with Elaborate_Body
is
   XOSC_Frequency : constant RP.Clock.XOSC_Hertz := 12_000_000;

   -- Encoders --
   Encoder_PIO    : PIO_Device renames RP.Device.PIO_0;
   Encoder_L_SM   : constant PIO_SM := 0;
   Encoder_R_SM   : constant PIO_SM := 1;
   Encoder_L_IRQ  : constant PIO_IRQ_ID := 0;
   Encoder_R_IRQ  : constant PIO_IRQ_ID := 1;
   Encoder_Offset : constant PIO_Address := 0;
   Encoder_Last   : constant PIO_Address :=
     Encoder_Offset + Pio_Rotary_Encoder_Program_Instructions'Length - 1;

   -- LEDs --
   WS2812_PIO    : PIO_Device renames RP.Device.PIO_0;
   WS2812_SM     : constant PIO_SM := 2;
   WS2812_Offset : constant PIO_Address := Encoder_Last + 1;

   -- OLED --
   Screen_SPI_DMA : constant DMA_Channel_Id := 4;

end WNM_HAL_Settings;
