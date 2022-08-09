with HAL.GPIO;

with SAM.Port;
with SAM.Device;
with SAM.Functions;
with SAM.Interrupt_Names;
with SAM.EIC;

with Cortex_M.NVIC;
with HAL; use HAL;
with Atomic.Generic_Signed32;

package body WNM.SAMD51.Encoders is

   package Atomic_Int is new Atomic.Generic_Signed32 (Integer);

   LA : SAM.Port.GPIO_Point renames SAM.Device.PB22;
   LB : SAM.Port.GPIO_Point renames SAM.Device.PA14;
   RA : SAM.Port.GPIO_Point renames SAM.Device.PA18;
   RB : SAM.Port.GPIO_Point renames SAM.Device.PA19;

   Val_L : aliased Atomic_Int.Instance;
   Val_R : aliased Atomic_Int.Instance;

   Seq_LA : UInt8 := 0;
   Seq_LB : UInt8 := 0;
   Seq_RA : UInt8 := 0;
   Seq_RB : UInt8 := 0;

   procedure Extint14_Handler;
   pragma Export (C, Extint14_Handler, "__eic_extint_14_interrupt_handler");

   procedure Extint2_Handler;
   pragma Export (C, Extint2_Handler, "__eic_extint_2_interrupt_handler");

   procedure Extint3_Handler;
   pragma Export (C, Extint3_Handler, "__eic_extint_3_interrupt_handler");

   procedure Extint6_Handler;
   pragma Export (C, Extint6_Handler, "__eic_extint_6_interrupt_handler");

   -------------------
   -- Left_Handling --
   -------------------

   procedure Left_Handling is
      A, B : UInt8 := 0;
   begin
      if LA.Set then
         A := 1;
      end if;
      if LB.Set then
         B := 1;
      end if;

      Seq_LA := Shift_Left (Seq_LA, 1);
      Seq_LA := (Seq_LA or A) and 2#1111#;
      Seq_LB := Shift_Left (Seq_LB, 1);
      Seq_LB := (Seq_LB or B) and 2#1111#;

      if Seq_LA = 2#1001# and then Seq_LB = 2#0011# then
         Atomic_Int.Add (Val_L, 1);
      end if;
      if Seq_LA = 2#0011# and then Seq_LB = 2#1001# then
         Atomic_Int.Sub (Val_L, 1);
      end if;
   end Left_Handling;

   --------------------
   -- Right_Handling --
   --------------------

   procedure Right_Handling is
      A, B : UInt8 := 0;
   begin
      if RA.Set then
         A := 1;
      end if;
      if RB.Set then
         B := 1;
      end if;

      Seq_RA := Shift_Left (Seq_RA, 1);
      Seq_RA := (Seq_RA or A) and 2#1111#;
      Seq_RB := Shift_Left (Seq_RB, 1);
      Seq_RB := (Seq_RB or B) and 2#1111#;

      if Seq_RA = 2#1001# and then Seq_RB = 2#0011# then
         Atomic_Int.Add (Val_R, 1);
      end if;
      if Seq_RA = 2#0011# and then Seq_RB = 2#1001# then
         Atomic_Int.Sub (Val_R, 1);
      end if;
   end Right_Handling;

   ---------------------
   -- Extint1_Handler --
   ---------------------

   procedure Extint14_Handler is
   begin
      SAM.EIC.Clear_Interrupt (14);
      Cortex_M.NVIC.Clear_Pending
        (SAM.Interrupt_Names.eic_extint_14_interrupt);

      Left_Handling;
   end Extint14_Handler;

   ---------------------
   -- Extint2_Handler --
   ---------------------

   procedure Extint2_Handler is
   begin
      SAM.EIC.Clear_Interrupt (2);
      Cortex_M.NVIC.Clear_Pending
        (SAM.Interrupt_Names.eic_extint_2_interrupt);

      Right_Handling;
   end Extint2_Handler;

   ---------------------
   -- Extint3_Handler --
   ---------------------

   procedure Extint3_Handler is
   begin
      SAM.EIC.Clear_Interrupt (3);
      Cortex_M.NVIC.Clear_Pending
        (SAM.Interrupt_Names.eic_extint_3_interrupt);

      Right_Handling;
   end Extint3_Handler;

   ---------------------
   -- Extint6_Handler --
   ---------------------

   procedure Extint6_Handler is
   begin
      SAM.EIC.Clear_Interrupt (6);
      Cortex_M.NVIC.Clear_Pending
        (SAM.Interrupt_Names.eic_extint_6_interrupt);

      Left_Handling;
   end Extint6_Handler;

   ----------
   -- Left --
   ----------

   function Left return Integer is
      Res : Integer;
   begin
      --  Load and reset
      Atomic_Int.Exchange (Val_L, 0, Res);
      return Res;
   end Left;

   -----------
   -- Right --
   -----------

   function Right return Integer is
      Res : Integer;
   begin
      --  Load and reset
      Atomic_Int.Exchange (Val_R, 0, Res);
      return Res;
   end Right;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      LA.Set_Mode (HAL.GPIO.Input);
      LA.Set_Pull_Resistor (HAL.GPIO.Pull_Up);
      LA.Set_Function (SAM.Functions.PB22_EIC_EXTINT6);

      LB.Set_Mode (HAL.GPIO.Input);
      LB.Set_Pull_Resistor (HAL.GPIO.Pull_Up);
      LB.Set_Function (SAM.Functions.PA14_EIC_EXTINT14);

      RA.Set_Mode (HAL.GPIO.Input);
      RA.Set_Pull_Resistor (HAL.GPIO.Pull_Up);
      RA.Set_Function (SAM.Functions.PA18_EIC_EXTINT2);

      RB.Set_Mode (HAL.GPIO.Input);
      RB.Set_Pull_Resistor (HAL.GPIO.Pull_Up);
      RB.Set_Function (SAM.Functions.PA19_EIC_EXTINT3);

      Cortex_M.NVIC.Enable_Interrupt
        (SAM.Interrupt_Names.eic_extint_2_interrupt);
      Cortex_M.NVIC.Enable_Interrupt
        (SAM.Interrupt_Names.eic_extint_3_interrupt);
      Cortex_M.NVIC.Enable_Interrupt
        (SAM.Interrupt_Names.eic_extint_6_interrupt);
      Cortex_M.NVIC.Enable_Interrupt
        (SAM.Interrupt_Names.eic_extint_14_interrupt);
   end Initialize;

begin
   Initialize;
end WNM.SAMD51.Encoders;
