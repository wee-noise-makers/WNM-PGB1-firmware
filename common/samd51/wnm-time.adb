with Cortex_M.Systick;

with System.Machine_Code; use System.Machine_Code;

package body WNM.Time is

   package Systick renames Cortex_M.Systick;

   Clock_Ms  : Time_Ms := 0 with Volatile;
   Period_Ms : constant Time_Ms := 1;

   procedure Initialize;
   procedure SysTick_Handler;
   pragma Export (C, SysTick_Handler, "__gnat_sys_tick_trap");
   --  pragma Export (C, SysTick_Handler, "SysTick_Handler");

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Reload : constant := 120_000_000 / 1_000;
   begin

      --  Configure for 1kH tick
      Systick.Configure (Source             => Systick.CPU_Clock,
                         Generate_Interrupt => True,
                         Reload_Value       => Reload);

      Systick.Enable;
   end Initialize;

   ---------------------
   -- SysTick_Handler --
   ---------------------

   procedure SysTick_Handler is
   begin
      Clock_Ms := Clock_Ms + Period_Ms;
   end SysTick_Handler;

   -----------
   -- Clock --
   -----------

   function Clock return Time_Ms
   is (Clock_Ms);

   --------------
   -- Delay_Ms --
   --------------

   procedure Delay_Ms (Milliseconds : UInt64) is
   begin
      Delay_Until (Clock + Milliseconds);
   end Delay_Ms;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Wakeup_Time : Time_Ms) is
   begin
      while Wakeup_Time > Clock loop
         Asm (Template => "wfi", -- Wait for interrupt
              Volatile => True);
      end loop;
   end Delay_Until;

begin
   Initialize;
end WNM.Time;
