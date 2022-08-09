-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2017 Fabien Chouteau                  --
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

with STM32.Device;                 use STM32.Device;
with STM32.GPIO;                   use STM32.GPIO;
with HAL;                          use HAL;
with STM32.I2S;                    use STM32.I2S;
with STM32.Timers;                 use STM32.Timers;
with STM32.PWM;                    use STM32.PWM;
with SGTL5000;                     use SGTL5000;
with Ravenscar_Time;               use Ravenscar_Time;
with HAL.Audio;                    use HAL.Audio;
with STM32.DMA;                    use STM32.DMA;
with Double_Buffers_Interrupts;    use Double_Buffers_Interrupts;

with System;                       use System;

with Ada.Synchronous_Task_Control;
with Ada.Interrupts.Names;
with Quick_Synth;

with WNM.I2C;

package body WNM.Audio_DAC is

   type Stereo_Buffer_Access is access all Quick_Synth.Stereo_Buffer;

   Task_Start : Ada.Synchronous_Task_Control.Suspension_Object;

   DAC_Dev : SGTL5000.SGTL5000_DAC (Port => WNM.I2C.Port,
                                    Time => Ravenscar_Time.Delays);

   -----------
   -- Audio --
   -----------

   Audio_I2S_Points : constant GPIO_Points (1 .. 4) := (PA15, PB3, PB5, PC7);
   Audio_I2S_Points_Ext : GPIO_Point renames PB4;
   Audio_I2S_TX : I2S_Port renames I2S_3;
   Audio_I2S_RX : I2S_Port renames I2S_3_Ext;

   ---------------
   -- Audio DMA --
   ---------------

   Audio_TX_DMA        : STM32.DMA.DMA_Controller renames DMA_1;
   Audio_TX_DMA_Chan   : STM32.DMA.DMA_Channel_Selector renames STM32.DMA.Channel_0;
   Audio_TX_DMA_Stream : STM32.DMA.DMA_Stream_Selector renames STM32.DMA.Stream_5;
   Audio_TX_DMA_Int    : DMA_Interrupt_Controller (Audio_TX_DMA'Access,
                                                   Audio_TX_DMA_Stream,
                                                   Ada.Interrupts.Names.DMA1_Stream5_Interrupt);

   Audio_RX_DMA        : STM32.DMA.DMA_Controller renames DMA_1;
   Audio_RX_DMA_Chan   : STM32.DMA.DMA_Channel_Selector renames STM32.DMA.Channel_2;
   Audio_RX_DMA_Stream : STM32.DMA.DMA_Stream_Selector renames STM32.DMA.Stream_2;
   Audio_RX_DMA_Int    : DMA_Interrupt_Controller (Audio_RX_DMA'Access,
                                                   Audio_RX_DMA_Stream,
                                                   Ada.Interrupts.Names.DMA1_Stream2_Interrupt);

   -- Buffers --

   TX0 : constant Stereo_Buffer_Access := new Quick_Synth.Stereo_Buffer;
   TX1 : constant Stereo_Buffer_Access := new Quick_Synth.Stereo_Buffer;
   RX0 : constant Stereo_Buffer_Access := new Quick_Synth.Stereo_Buffer;
   RX1 : constant Stereo_Buffer_Access := new Quick_Synth.Stereo_Buffer;

   ----------
   -- MCLK --
   ----------

   MCLK_Timer : STM32.Timers.Timer renames Timer_8;

   MCLK_Timer_AF : constant STM32.GPIO_Alternate_Function := GPIO_AF_TIM8_3;
   --  Note that this value MUST match the corresponding timer selected!

   Output_Channel : constant Timer_Channel := Channel_2; -- arbitrary

   MCLK_Requested_Frequency : constant Hertz := 12_000_000;  -- arbitrary

   MCLK_Control : PWM_Modulator;

   MCLK_Point : GPIO_Point renames PC7;

   procedure Initialize;
   procedure Initialize_DMA;
   procedure Initialize_I2S;
   procedure Initialize_MCLK with Unreferenced;

   --------------------
   -- Initialize_I2S --
   --------------------

   procedure Initialize_I2S is

      procedure Initialize_GPIO;

      ---------------------
      -- Initialize_GPIO --
      ---------------------

      procedure Initialize_GPIO is
      begin

         -- I2S --

         Enable_Clock (Audio_I2S_Points);

         Configure_IO (Audio_I2S_Points,
                       (Speed       => Speed_High,
                        Mode        => Mode_AF,
                        Output_Type => Push_Pull,
                        Resistors   => Floating));

         Configure_Alternate_Function (Audio_I2S_Points,
                                       STM32.Device.GPIO_AF_SPI3_6);

         Enable_Clock (Audio_I2S_Points_Ext);

         Configure_IO (Audio_I2S_Points_Ext,
                       (Speed       => Speed_High,
                        Mode        => Mode_AF,
                        Output_Type => Push_Pull,
                        Resistors   => Floating));

         Configure_Alternate_Function (Audio_I2S_Points_Ext,
                                       STM32.Device.GPIO_AF_I2S3ext_7);

      end Initialize_GPIO;

      Conf : I2S_Configuration;

   begin
      Initialize_GPIO;

      --  | Sample Freq | PLL N | PLL R |
      --  |     8000    |  256  |   5   |
      --  |    16000    |  213  |   2   |
      --  |    32000    |  213  |   2   |
      --  |    48000    |  258  |   3   |
      --  |    96000    |  344  |   2   |
      --  |    22050    |  429  |   4   |
      --  |    44100    |  271  |   2   |
      Set_PLLI2S_Factors (Pll_N => 213,
                          Pll_R => 2);
      Enable_PLLI2S;

      -- I2S TX --
      Enable_Clock (Audio_I2S_TX);

      Conf.Mode                     := Master_Transmit;
      Conf.Standard                 := I2S_Philips_Standard;
      Conf.Clock_Polarity           := Steady_State_Low;
      Conf.Data_Length              := Data_16bits;
      Conf.Chan_Length              := Channel_16bits;
      Conf.Master_Clock_Out_Enabled := True;
      Conf.Transmit_DMA_Enabled     := True;
      Conf.Receive_DMA_Enabled      := False;

      Audio_I2S_TX.Configure (Conf);
      Audio_I2S_TX.Set_Frequency (Audio_Freq_32kHz);

      -- I2S RX --
      Enable_Clock (Audio_I2S_RX);

      Conf.Mode                     := Slave_Receive;
      Conf.Master_Clock_Out_Enabled := False;
      Conf.Transmit_DMA_Enabled     := False;
      Conf.Receive_DMA_Enabled      := True;

      Audio_I2S_RX.Configure (Conf);
      Audio_I2S_RX.Set_Frequency (Audio_Freq_32kHz);

      Audio_I2S_TX.Enable;
      Audio_I2S_RX.Enable;

   end Initialize_I2S;

   --------------------
   -- Initialize_DMA --
   --------------------

   procedure Initialize_DMA is
      Config : DMA_Stream_Configuration;
   begin
      -- TX DMA --

      Enable_Clock (Audio_TX_DMA);

      Config.Channel := Audio_TX_DMA_Chan;
      Config.Direction := Memory_To_Peripheral;
      Config.Increment_Peripheral_Address := False;
      Config.Increment_Memory_Address := True;
      Config.Peripheral_Data_Format := HalfWords;
      Config.Memory_Data_Format := HalfWords;
      Config.Operation_Mode := Circular_Mode;
      Config.Priority := Priority_Very_High;
      Config.FIFO_Enabled := False;
      Config.FIFO_Threshold := FIFO_Threshold_Full_Configuration;
      Config.Memory_Burst_Size := Memory_Burst_Single;
      Config.Peripheral_Burst_Size := Peripheral_Burst_Single;
      Configure (Audio_TX_DMA, Audio_TX_DMA_Stream, Config);

      -- RX DMA --

      Enable_Clock (Audio_RX_DMA);

      Config.Channel   := Audio_RX_DMA_Chan;
      Config.Direction := Peripheral_To_Memory;
      Configure (Audio_RX_DMA, Audio_RX_DMA_Stream, Config);
   end Initialize_DMA;

   ---------------------
   -- Initialize_MCLK --
   ---------------------

   procedure Initialize_MCLK is
   begin
      --  The STM32F4 is capable of generating the MCLK for the SGTL5000 DAC.
      --  However, we need the MCLK to be always enabled to be able to
      --  communicate with the DAC even when we are not sending audio data so
      --  we use a timer to generate the MCLK signal.

      Configure_PWM_Timer (MCLK_Timer'Access, MCLK_Requested_Frequency);

      MCLK_Control.Attach_PWM_Channel
        (MCLK_Timer'Access,
         Output_Channel,
         MCLK_Point,
         MCLK_Timer_AF);

      MCLK_Control.Set_Duty_Cycle (50);
      MCLK_Control.Enable_Output;
   end Initialize_MCLK;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialize_DMA;
      --  Initialize_MCLK;
      Initialize_I2S;

      Ada.Synchronous_Task_Control.Set_True (Task_Start);

      delay until Clock + Milliseconds (500);

      if not DAC_Dev.Valid_Id then
         raise Program_Error with "Cannot get DAC chip ID";
      end if;

      DAC_Dev.Set_Analog_Power (Reftop_PowerUp => True);

      DAC_Dev.Set_Linereg_Control (Charge_Pump_Src_Override => True,
                                   Charge_Pump_Src          => VDDIO,
                                   Linereg_Out_Voltage      => 16#C#);

      DAC_Dev.Set_Reference_Control (VAG           => 16#1F#,
                                     Bias          => 16#1#,
                                     Slow_VAG_Ramp => False);

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
                                ADC_PowerUp               => True);

      DAC_Dev.Set_Power_Control (ADC     => On,
                                 DAC     => On,
                                 DAP     => Off,
                                 I2S_Out => On,
                                 I2S_In  => On);

      DAC_Dev.Time.Delay_Microseconds (400);

      DAC_Dev.Set_Clock_Control (Rate => SYS_FS,
                                 FS   => SYS_FS_32kHz,
                                 MCLK => MCLK_256FS);

      DAC_Dev.Set_I2S_Control (SCLKFREQ    => SCLKFREQ_32FS,
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
      DAC_Dev.Set_ADC_Volume (16#0#, 16#0#, Minus_6db => False);

      DAC_Dev.Set_Headphones_Volume (16#70#, 16#70#);

      --  Unmute
      DAC_Dev.Mute_DAC (False);
      DAC_Dev.Mute_ADC (False);
      DAC_Dev.Mute_Headphones (False);

      --  Mute
      DAC_Dev.Mute_Line_Out (True);
   end Initialize;

   ----------------
   -- I2S_Stream --
   ----------------

   task I2S_Stream is
      pragma Priority (DAC_Task_Priority);
   end I2S_Stream;

   task body I2S_Stream is
      Unused_TX : Stereo_Buffer_Access;
      Unused_RX : Stereo_Buffer_Access;
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True (Task_Start);

      Audio_TX_DMA_Int.Start_Mem_To_Periph (Audio_I2S_TX.Data_Register_Address,
                                            TX0.all'Address,
                                            TX1.all'Address,
                                            Stereo_Buffer_Size_In_Bytes / 2);

      Audio_RX_DMA_Int.Start_Periph_To_Mem (Audio_I2S_RX.Data_Register_Address,
                                            RX0.all'Address,
                                            RX1.all'Address,
                                            Stereo_Buffer_Size_In_Bytes / 2);

      loop
         Audio_TX_DMA_Int.Wait_For_Interrupt;

         if Audio_TX_DMA_Int.Not_In_Transfer = TX0.all'Address then
            Unused_TX := TX0;
         else
            Unused_TX := TX1;
         end if;

         if Audio_RX_DMA_Int.Not_In_Transfer = RX0.all'Address then
            Unused_RX := RX0;
         else
            Unused_RX := RX1;
         end if;

         Quick_Synth.Fill (Unused_RX.all, Unused_TX.all);
      end loop;
   end I2S_Stream;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume (Volume : DAC_Volume) is
      HP_Vol : HP_Volume;

   begin
      HP_Vol := HP_Volume'Last - UInt7 (Volume);
      DAC_Dev.Set_Headphones_Volume (HP_Vol, HP_Vol);
   end Set_Volume;

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down is
   begin
      DAC_Dev.Set_Analog_Power (VAG_PowerUp               => False,
                                Reftop_PowerUp            => False,
                                Headphone_PowerUp         => False,
                                DAC_PowerUp               => False,
                                Capless_Headphone_PowerUp => False,
                                ADC_PowerUp               => False);

      DAC_Dev.Set_Power_Control (ADC     => Off,
                                 DAC     => Off,
                                 DAP     => Off,
                                 I2S_Out => Off,
                                 I2S_In  => Off);
   end Power_Down;

begin
   Initialize;
end WNM.Audio_DAC;
