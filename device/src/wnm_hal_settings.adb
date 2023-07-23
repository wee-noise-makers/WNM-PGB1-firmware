package body WNM_HAL_Settings is
begin

   --  RP.Clock.Initialize (XOSC_Frequency);
   --  RP.Clock.Enable (RP.Clock.PERI);
   RP.Device.Timer.Enable;
   RP.Device.PIO_0.Enable;
   --  RP.Device.PIO_1.Enable;
   --  RP.GPIO.Enable;
   --  RP.DMA.Enable;
end WNM_HAL_Settings;
