with System;

with HAL.GPIO;

with WNM.Samd51; use WNM.Samd51;
pragma Elaborate (WNM.Samd51);

with SAM.Clock_Generator.IDs;
with SAM.Main_Clock;
with SAM.DAC; use SAM.DAC;
with SAM.Device;
with SAM.DMAC; use SAM.DMAC;
with SAM.DMAC.Sources;
with SAM.TC; use SAM.TC;
with SAM.Interrupt_Names;
with SAM.ADC;

with Cortex_M.NVIC;

with HAL; use HAL;

with BBqueue;

with System.Storage_Elements; use System.Storage_Elements;

package body WNM.Audio is

   Buffer_Count : constant BBqueue.Buffer_Offset := Audio_Queue_Size;

   Out_L : array (1 .. Buffer_Count) of Mono_Buffer;
   Out_R : array (1 .. Buffer_Count) of Mono_Buffer;
   In_L : array (1 .. Buffer_Count) of Mono_Buffer;
   In_R : array (1 .. Buffer_Count) of Mono_Buffer;

   Zero_Buffer : constant Mono_Buffer := (others => 0);

   Out_Queue : BBqueue.Offsets_Only (Buffer_Count);
   In_Queue : BBqueue.Offsets_Only (Buffer_Count);

   ADC_0 : SAM.ADC.ADC_Device renames SAM.Device.ADC0;
   ADC_1 : SAM.ADC.ADC_Device renames SAM.Device.ADC1;

   Out_Read : BBQueue.Read_Grant := BBqueue.Empty;
   In_Write : BBQueue.Write_Grant := BBqueue.Empty;

   Starving : Natural := 0 with Volatile;
   pragma Export (C, Starving, "__wnm_audio_starving");

   procedure DAC_DMA_Int_Handler;
   pragma Export (C, DAC_DMA_Int_Handler, "__dmac_tcmpl_0_handler");

   -------------------------
   -- DAC_DMA_Int_Handler --
   -------------------------

   procedure DAC_DMA_Int_Handler is
      use BBqueue;

      Out_L_Addr, Out_R_Addr, In_L_Addr, In_R_Addr : System.Address;
   begin

      Cortex_M.NVIC.Clear_Pending (SAM.Interrupt_Names.dmac_0_interrupt);

      -- Output --

      --  Release the buffer that was just sent, if any
      if State (Out_Read) = Valid then
         Release (Out_Queue, Out_Read, 1);
      end if;

      Read (Out_Queue, Out_Read, 1);

      if State (Out_Read) /= Valid then
         --  No data to send, use a buffer of zeroes
         Out_L_Addr := Zero_Buffer'Address;
         Out_R_Addr := Zero_Buffer'Address;
         Starving := Starving + 1;
      else
         declare
            Out_Index : constant Buffer_Offset :=
              Out_L'First + Slice (Out_Read).From;
         begin
            Out_L_Addr := Out_L (Out_Index)'Address;
            Out_R_Addr := Out_R (Out_Index)'Address;
         end;
      end if;

      -- DAC --

      Clear (DMA_DAC_0, Transfer_Complete);
      Clear (DMA_DAC_1, Transfer_Complete);

      Set_Data_Transfer (DMA_Descs (DMA_DAC_0),
                         Block_Transfer_Count => Mono_Buffer'Length,
                         Src_Addr             => Out_L_Addr,
                         Dst_Addr             => SAM.DAC.Data_Address (0));
      Set_Data_Transfer (DMA_Descs (DMA_DAC_1),
                         Block_Transfer_Count => Mono_Buffer'Length,
                         Src_Addr             => Out_R_Addr,
                         Dst_Addr             => SAM.DAC.Data_Address (1));

      Enable (DMA_DAC_0);
      Enable (DMA_DAC_1);

      -- Input --

      --  Commit the buffer that was just received, if any
      if State (In_Write) = Valid then
         Commit (In_Queue, In_Write, 1);
      end if;

      Grant (In_Queue, In_Write, 1);

      if State (In_Write) /= Valid then
         --  No space for incoming data, it will be lost...
         return;
      end if;

      declare
         In_Index : constant Buffer_Offset :=
           In_L'First + Slice (In_Write).From;
      begin
         In_L_Addr := In_L (In_Index)'Address;
         In_R_Addr := In_R (In_Index)'Address;
      end;

      -- ADC --

      Clear (DMA_ADC_0, Transfer_Complete);
      Clear (DMA_ADC_1, Transfer_Complete);

      Set_Data_Transfer (DMA_Descs (DMA_ADC_0),
                         Block_Transfer_Count => Mono_Buffer'Length,
                         Src_Addr             => ADC_0.Result_Address,
                         Dst_Addr             => In_L_Addr);
      Set_Data_Transfer (DMA_Descs (DMA_ADC_1),
                         Block_Transfer_Count => Mono_Buffer'Length,
                         Src_Addr             => ADC_1.Result_Address,
                         Dst_Addr             => In_R_Addr);

      Enable (DMA_ADC_0);
      Enable (DMA_ADC_1);

   end DAC_DMA_Int_Handler;

   --------------------
   -- Generate_Audio --
   --------------------

   procedure Generate_Audio is
      use BBqueue;

      In_Read : Read_Grant;
      Out_Write : Write_Grant;
   begin
      Grant (Out_Queue, Out_Write, 1);

      if State (Out_Write) /= Valid then
         return;
      end if;

      BBqueue.Read (In_Queue, In_Read, 1);

      if State (In_Read) /= Valid then
         Commit (Out_Queue, Out_Write, 0);
         return;
      end if;

      declare
         Out_Index : constant Buffer_Offset :=
           Out_L'First + Slice (Out_Write).From;

         In_Index : constant Buffer_Offset :=
           In_L'First +  Slice (Out_Write).From;
      begin
         Process (Out_L (Out_Index),
                  Out_R (Out_Index),
                  In_L (In_Index),
                  In_R (In_Index));

         Commit (Out_Queue, Out_Write, 1);
         Release (In_Queue, In_Read, 1);
      end;
   end Generate_Audio;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin

      -- DAC --

      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.DAC, Clk_48Mhz);

      SAM.Main_Clock.DAC_On;

      SAM.DAC.Configure (Single_Mode, VREFAB);

      Debug_Stop_Mode (False);

      Configure_Channel (Chan                           => 0,
                         Oversampling                   => OSR_16,
                         Refresh                        => 0,
                         Enable_Dithering               => True,
                         Run_In_Standby                 => True,
                         Standalone_Filter              => False,
                         Current                        => CC1M,
                         Adjustement                    => Right_Adjusted,
                         Enable_Filter_Result_Ready_Evt => False,
                         Enable_Data_Buffer_Empty_Evt   => False,
                         Enable_Convert_On_Input_Evt    => False,
                         Invert_Input_Evt               => False,
                         Enable_Overrun_Int             => False,
                         Enable_Underrun_Int            => False,
                         Enable_Result_Ready_Int        => False,
                         Enable_Buffer_Empty_Int        => False);

      Configure_Channel (Chan                           => 1,
                         Oversampling                   => OSR_16,
                         Refresh                        => 0,
                         Enable_Dithering               => True,
                         Run_In_Standby                 => True,
                         Standalone_Filter              => False,
                         Current                        => CC1M,
                         Adjustement                    => Right_Adjusted,
                         Enable_Filter_Result_Ready_Evt => False,
                         Enable_Data_Buffer_Empty_Evt   => False,
                         Enable_Convert_On_Input_Evt    => False,
                         Invert_Input_Evt               => False,
                         Enable_Overrun_Int             => False,
                         Enable_Underrun_Int            => False,
                         Enable_Result_Ready_Int        => False,
                         Enable_Buffer_Empty_Int        => False);

      Enable (Chan_0 => True,
              Chan_1 => True);

      -- Enable speaker --

      SAM.Device.PA27.Set_Mode (HAL.GPIO.Output);
      SAM.Device.PA27.Set;

      -- DMA DAC --

      Configure (DMA_DAC_0,
                 Trig_Src       => SAM.DMAC.Sources.TC0_OVF,
                 Trig_Action    => Burst,
                 Priority       => 1,
                 Burst_Len      => 1,
                 Threshold      => BEAT_1,
                 Run_In_Standby => False);

      --  Only enable the channel 0 interrupt
      Enable (DMA_DAC_0, Transfer_Complete);
      Cortex_M.NVIC.Enable_Interrupt (SAM.Interrupt_Names.dmac_0_interrupt);

      Configure (DMA_DAC_1,
                 Trig_Src       => SAM.DMAC.Sources.TC0_OVF,
                 Trig_Action    => Burst,
                 Priority       => 1,
                 Burst_Len      => 1,
                 Threshold      => BEAT_1,
                 Run_In_Standby => False);

      Configure_Descriptor (DMA_Descs (DMA_DAC_0),
                            Valid           => True,
                            Event_Output    => Disable,
                            Block_Action    => Interrupt,
                            Beat_Size       => B_16bit,
                            Src_Addr_Inc    => True,
                            Dst_Addr_Inc    => False,
                            Step_Selection  => Source,
                            Step_Size       => X1);

      Configure_Descriptor (DMA_Descs (DMA_DAC_1),
                            Valid           => True,
                            Event_Output    => Disable,
                            Block_Action    => Interrupt,
                            Beat_Size       => B_16bit,
                            Src_Addr_Inc    => True,
                            Dst_Addr_Inc    => False,
                            Step_Selection  => Source,
                            Step_Size       => X1);

      -- ADC --

      --  TODO: Use ADC Offset Correction to remove Tuner DC offset?

      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.ADC0, Clk_48Mhz);

      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.ADC1, Clk_48Mhz);

      SAM.Main_Clock.ADC0_On;
      SAM.Main_Clock.ADC1_On;

      ADC_0.Configure (Resolution                   => SAM.ADC.Res_16bit,
                       Reference                    => SAM.ADC.VDDANA,
                       Prescaler                    => SAM.ADC.Pre_4,
                       Free_Running                 => True,
                       Differential_Mode            => False);

      ADC_1.Configure (Resolution                   => SAM.ADC.Res_16bit,
                       Reference                    => SAM.ADC.VDDANA,
                       Prescaler                    => SAM.ADC.Pre_4,
                       Free_Running                 => True,
                       Differential_Mode            => False);

      ADC_0.Enable;
      ADC_1.Enable;

      ADC_0.Set_Inputs (SAM.ADC.GND, SAM.ADC.AIN3);
      ADC_1.Set_Inputs (SAM.ADC.GND, SAM.ADC.AIN2);

      ADC_0.Software_Start;
      ADC_1.Software_Start;

      -- DMA ADC --

      Configure (DMA_ADC_0,
                 Trig_Src       => SAM.DMAC.Sources.TC0_OVF,
                 Trig_Action    => Burst,
                 Priority       => 1,
                 Burst_Len      => 1,
                 Threshold      => BEAT_1,
                 Run_In_Standby => False);

      Configure (DMA_ADC_1,
                 Trig_Src       => SAM.DMAC.Sources.TC0_OVF,
                 Trig_Action    => Burst,
                 Priority       => 1,
                 Burst_Len      => 1,
                 Threshold      => BEAT_1,
                 Run_In_Standby => False);

      Configure_Descriptor (DMA_Descs (DMA_ADC_0),
                            Valid           => True,
                            Event_Output    => Disable,
                            Block_Action    => No_Action,
                            Beat_Size       => B_16bit,
                            Src_Addr_Inc    => False,
                            Dst_Addr_Inc    => True,
                            Step_Selection  => Destination,
                            Step_Size       => X1);

      Configure_Descriptor (DMA_Descs (DMA_ADC_1),
                            Valid           => True,
                            Event_Output    => Disable,
                            Block_Action    => No_Action,
                            Beat_Size       => B_16bit,
                            Src_Addr_Inc    => False,
                            Dst_Addr_Inc    => True,
                            Step_Selection  => Destination,
                            Step_Size       => X1);

      -- Timer --

      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.TC0, Clk_48Mhz);

      SAM.Main_Clock.TC0_On;

      SAM.Device.TC0.Configure (Mode             => TC_8bit,
                                Prescaler        => DIV64,
                                Run_In_Standby   => True,
                                Clock_On_Demand  => False,
                                Auto_Lock        => False,
                                Capture_0_Enable => False,
                                Capture_1_Enable => False,
                                Capture_0_On_Pin => False,
                                Capture_1_On_Pin => False,
                                Capture_0_Mode   => Default,
                                Capture_1_Mode   => Default);

      --  Start the time with the longest period to have a reduced number of
      --  interrupt until the audio is actually used.
      SAM.Device.TC0.Set_Period (255);

      SAM.Device.TC0.Enable;

      --  Start the first DMA transfer
      DAC_DMA_Int_Handler;

      --  Set the timer period coresponding to the requested sample rate
      SAM.Device.TC0.Set_Period
        (UInt8 ((UInt32 (48000000) / 64) / WNM.Sample_Frequency));

   end Initialize;

begin
   Initialize;
end WNM.Audio;
