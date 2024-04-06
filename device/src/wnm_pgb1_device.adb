with System;
with HAL;
with WNM.Tasks;
with WNM_HAL.Synth_Core;
with RP.Multicore;
with Noise_Nugget_SDK.Audio;
with WNM_Configuration;
with RP_Interrupts;
with RP2040_SVD.Interrupts;
with RP2040_SVD.SIO;
with RP.Multicore.FIFO;
with RP.Multicore.Spinlocks;
with Cortex_M.NVIC;

procedure WNM_PGB1_Device is
begin
   --  Make sure we don't have data left in the FIFO after reset
   RP.Multicore.FIFO.Drain;

   --  Clear FIFO Status
   RP2040_SVD.SIO.SIO_Periph.FIFO_ST := (others => <>);

   --  Make sure we don't have spinlocks locked after reset
   for Id in RP.Multicore.Spinlocks.Lock_Id loop
      RP.Multicore.Spinlocks.Release (Id);
   end loop;

   RP_Interrupts.Attach_Handler
     (WNM.Tasks.Sequencer_Coproc_Receive'Access,
      RP2040_SVD.Interrupts.SIO_IRQ_PROC0_Interrupt,
      RP_Interrupts.Interrupt_Priority'Last);

   --  Coproc FIFO interrupt should be lower priority than DAC interrupt.
   --  The audio mixing is done during this interrupt.
   Cortex_M.NVIC.Set_Priority
     (RP2040_SVD.Interrupts.SIO_IRQ_PROC0_Interrupt,
      1);

   --  SysTick intrrrupt should be lower than both copro and DAC interrupt to
   --  avoid interrupting the DAC callback and mixing.
   declare
      SHPR3 : HAL.UInt32
        with Volatile_Full_Access,
        Address => System'To_Address (16#E000ED20#);
   begin
      --  Set Systick priority
      SHPR3 := 2#11_000000_00_0000000000000000000000#;
      --         ^^ Systick
      --                   ^^ PendSV
   end;

   --  Start the second CPU core that will run the synth
   RP.Multicore.Launch_Core1
     (Trap_Vector   => WNM_HAL.Synth_Core.Trap_Vector,
      Stack_Pointer => WNM_HAL.Synth_Core.Stack_Pointer,
      Entry_Point   => WNM_HAL.Synth_Core.Entry_Point);

   --  DAC interrupts will run on the first core
   if not Noise_Nugget_SDK.Audio.Start
     (WNM_Configuration.Audio.Sample_Frequency,
      Output_Callback => WNM.Tasks.Next_Output_Buffer'Access,
      Input_Callback  => WNM.Tasks.Next_Input_Buffer'Access)
   then
      raise Program_Error with "MDM";
   end if;

   Noise_Nugget_SDK.Audio.Enable_Mic (True, True);
   Noise_Nugget_SDK.Audio.Mixer_To_Output (False, False);
   Noise_Nugget_SDK.Audio.Set_ADC_Volume (1.0, 1.0);

   WNM_HAL.Set_Line_In_Volume (WNM_HAL.Init_Volume);
   WNM_HAL.Set_Mic_Volumes (WNM_HAL.Init_Input_Volume,
                            WNM_HAL.Init_Input_Volume);
   WNM_HAL.Set_Main_Volume (WNM_HAL.Init_Input_Volume);

   WNM.Tasks.Sequencer_Core;
end WNM_PGB1_Device;
