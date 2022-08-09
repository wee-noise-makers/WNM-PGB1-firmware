with RP.PIO;  use RP.PIO;
with RP.GPIO; use RP.GPIO;
with RP.DMA;  use RP.DMA;
with RP.PWM;
with RP.Device;

with WNM.RP2040.PIO; use WNM.RP2040.PIO;
with WNM.RP2040.PIO_I2S_ASM; use WNM.RP2040.PIO_I2S_ASM;
with RP; use RP;
with WNM.RP2040;
with WNM.RP2040.I2C;

with SGTL5000; use SGTL5000;

package body WNM.Audio is

   Flip : Boolean := False
     with Atomic, Volatile;

   Out_Buffer :  Stereo_Buffer := (others => (0, 0));
   In_Buffer  :  Stereo_Buffer := (others => (0, 0));

   Data_In     : RP.GPIO.GPIO_Point := (Pin => 21);
   Data_Out    : RP.GPIO.GPIO_Point := (Pin => 22);
   BCLK        : RP.GPIO.GPIO_Point := (Pin => 23);
   LRCLK       : RP.GPIO.GPIO_Point := (Pin => 24);
   MCLK        : RP.GPIO.GPIO_Point := (Pin => 25);

   DMA_IRQ     : constant RP.DMA.DMA_IRQ_Id := 0;

   MCLK_Requested_Frequency : constant := 256 * WNM.Sample_Frequency;
   MCLK_PWM : constant RP.PWM.PWM_Slice := RP.PWM.To_PWM (MCLK).Slice;

   DAC_Dev : SGTL5000.SGTL5000_DAC (WNM.RP2040.I2C.Port,
                                    RP.Device.Timer'Access);

   procedure DMA_Out_Handler
     with Export        => True,
          Convention    => C,
          External_Name => "isr_irq11";

   ---------------------
   -- DMA_Out_Handler --
   ---------------------

   procedure DMA_Out_Handler is
   begin

      RP.DMA.Ack_IRQ (WNM.RP2040.I2S_OUT_DMA, DMA_IRQ);

      --  Copy last in into next out
      Out_Buffer := In_Buffer;

      Out_Buffer (Out_Buffer'First) := (30000, 30000);
      for X in 1 .. 40 loop
         Out_Buffer (Out_Buffer'First + X) := (2#1010101010#,
                                               2#1010101010#);
      end loop;

      RP.DMA.Start
         (Channel => WNM.RP2040.I2S_OUT_DMA,
          From    => Out_Buffer'Address,
          To      => TX_FIFO_Address (I2S_PIO, I2S_SM),
          Count   => Out_Buffer'Length);

      RP.DMA.Start
         (Channel => WNM.RP2040.I2S_IN_DMA,
          From    => RX_FIFO_Address (I2S_PIO, I2S_SM),
          To      => In_Buffer'Address,
          Count   => In_Buffer'Length);
   end DMA_Out_Handler;

   -------------------
   -- Init_SGTL5000 --
   -------------------

   procedure Init_SGTL5000 is
   begin
      if not DAC_Dev.Valid_Id then
         raise Program_Error with "Cannot get DAC chip ID";
      end if;

      DAC_Dev.Set_Analog_Power (Reftop_PowerUp => True,
                                DAC_Mono       => True, -- Can be removed?
                                ADC_Mono       => True); -- Can be removed?

      DAC_Dev.Set_Linereg_Control (Charge_Pump_Src_Override => True,
                                   Charge_Pump_Src          => VDDIO,
                                   Linereg_Out_Voltage      => 16#C#);

      DAC_Dev.Set_Reference_Control (VAG           => 16#1F#,
                                     Bias          => 16#1#,
                                     Slow_VAG_Ramp => False);

      --  DAC_Dev.Set_Lineout_Control (Out_Current            => C_0_54ma,
      --                               Amp_Analog_GND_Voltage => 16#22#);

      DAC_Dev.Set_Short_Detectors (Right_HP  => 4,
                                   Left_HP   => 4,
                                   Center_HP => 4,
                                   Mode_LR   => 1,
                                   Mode_CM   => 2);

      DAC_Dev.Mute_Headphones (True);
      DAC_Dev.Mute_DAC (True);
      DAC_Dev.Mute_ADC (True);

      DAC_Dev.Set_Analog_Power (VAG_PowerUp               => True,
                                Reftop_PowerUp            => True,
                                Headphone_PowerUp         => True,
                                DAC_PowerUp               => True,
                                Capless_Headphone_PowerUp => True,
                                Linout_PowerUp            => True,
                                ADC_PowerUp               => True,
                                ADC_Mono                  => True,
                                DAC_Mono                  => True,
                                PLL_PowerUp               => False,
                                VCOAmp_PowerUp            => False);

      DAC_Dev.Set_Power_Control (ADC     => On,
                                 DAC     => On,
                                 DAP     => Off,
                                 I2S_Out => On,
                                 I2S_In  => On);

      DAC_Dev.Time.Delay_Microseconds (400);

      DAC_Dev.Set_Clock_Control (Rate => SYS_FS,
                                 FS   => SYS_FS_44kHz,
                                 MCLK => MCLK_256FS);

      DAC_Dev.Set_I2S_Control (SCLKFREQ    => SCLKFREQ_64FS,
                               Invert_SCLK => False,
                               Master_Mode => False,
                               Data_Len    => Data_16b,
                               I2S         => I2S_Left_Justified,
                               LR_Align    => False,
                               LR_Polarity => False);

      --  DAC Routing
      DAC_Dev.Select_DAC_Source (I2S_In);
      DAC_Dev.Select_HP_Source (DAC, True);

      --  ADC Routing
      DAC_Dev.Select_ADC_Source (Line_In, True);
      DAC_Dev.Select_I2S_Out_Source (ADC);

      --  +0db
      DAC_Dev.Set_DAC_Volume (16#3C#, 16#3C#);

      --  +0db
      DAC_Dev.Set_ADC_Volume (16#F#, 16#F#, Minus_6db => False);

      DAC_Dev.Set_Headphones_Volume (16#0#, 16#0#);

      --  Unmute
      DAC_Dev.Mute_DAC (False);
      DAC_Dev.Mute_ADC (False);
      DAC_Dev.Mute_Headphones (False);
      DAC_Dev.Mute_Line_Out (False);
   end Init_SGTL5000;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Config : RP.PIO.PIO_SM_Config := Default_SM_Config;

      Sample_Rate       : constant Hertz := Hertz (WNM.Sample_Frequency);
      Sample_Bits       : constant := 16;
      Cycles_Per_Sample : constant := 4;
      Channels          : constant := 2;

      DMA_Config : DMA_Configuration;
   begin
      -- GPIO --
      Data_Out.Configure (Output, Pull_Both, I2S_PIO.GPIO_Function);
      Data_In.Configure (Output, Pull_Both, I2S_PIO.GPIO_Function);
      BCLK.Configure (Output, Pull_Both, I2S_PIO.GPIO_Function);
      LRCLK.Configure (Output, Pull_Both, I2S_PIO.GPIO_Function);

      --  Square wave with PWM for the SGTL5000 MCLK
      RP.PWM.Initialize;
      MCLK.Configure (RP.GPIO.Output, RP.GPIO.Floating, RP.GPIO.PWM);
      RP.PWM.Set_Frequency (MCLK_PWM,
                            Frequency => MCLK_Requested_Frequency * 2);
      RP.PWM.Set_Interval (MCLK_PWM, Clocks => 1);
      RP.PWM.Set_Duty_Cycle (MCLK_PWM, 1, 1);
      RP.PWM.Enable (MCLK_PWM);

      -- SGTL5000 --

      Init_SGTL5000;

      -- I2S PIO --

      Enable (I2S_PIO);
      Load (I2S_PIO,
            Prog   => Audio_I2s_Program_Instructions,
            Offset => I2S_Offset);

      Set_Out_Pins (Config, Data_Out.Pin, 1);
      Set_In_Pins (Config, Data_In.Pin);
      Set_Sideset_Pins (Config, BCLK.Pin);
      Set_Sideset (Config, 2, False, False);
      Set_Out_Shift (Config, False, True, 32);
      Set_In_Shift (Config, False, True, 32);

      Set_Wrap (Config,
          Wrap        => I2S_Offset + Audio_I2s_Wrap,
          Wrap_Target => I2S_Offset + Audio_I2s_Wrap_Target);

      Set_Config (I2S_PIO, I2S_SM, Config);
      SM_Initialize (I2S_PIO, I2S_SM, I2S_Offset, Config);

      Set_Pin_Direction (I2S_PIO, I2S_SM, Data_Out.Pin, Output);
      Set_Pin_Direction (I2S_PIO, I2S_SM, Data_In.Pin, Input);
      Set_Pin_Direction (I2S_PIO, I2S_SM, BCLK.Pin, Output);
      Set_Pin_Direction (I2S_PIO, I2S_SM, LRCLK.Pin, Output);

      Execute (I2S_PIO, I2S_SM,
               PIO_Instruction (I2S_Offset + Offset_entry_point));

      Set_Clock_Frequency
        (Config,
         Sample_Rate * Sample_Bits * Channels * Cycles_Per_Sample);
      Set_Config (I2S_PIO, I2S_SM, Config);

      Set_Enabled (I2S_PIO, I2S_SM, True);

      -- I2S DMA --
      DMA_Config.Trigger := I2S_OUT_DMA_Trigger;
      DMA_Config.High_Priority := True;
      DMA_Config.Increment_Read := True;
      DMA_Config.Increment_Write := False;
      DMA_Config.Data_Size := Transfer_32;
      DMA_Config.Quiet := False; -- Enable interrupt
      RP.DMA.Configure (WNM.RP2040.I2S_OUT_DMA, DMA_Config);

      RP.DMA.Enable_IRQ (WNM.RP2040.I2S_OUT_DMA, DMA_IRQ);

      DMA_Config.Trigger := I2S_IN_DMA_Trigger;
      DMA_Config.High_Priority := True;
      DMA_Config.Increment_Read := False;
      DMA_Config.Increment_Write := True;
      DMA_Config.Data_Size := Transfer_32;
      RP.DMA.Configure (WNM.RP2040.I2S_IN_DMA, DMA_Config);

      --  Start a first transfer
      DMA_Out_Handler;
   end Initialize;

   ------------------
   -- Select_Input --
   ------------------

   procedure Select_Input (Kind : Input_Kind) is
   begin
      null;
   end Select_Input;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume (Volume : DAC_Volume) is
   begin
      null;
   end Set_Volume;

begin
   Initialize;
end WNM.Audio;
