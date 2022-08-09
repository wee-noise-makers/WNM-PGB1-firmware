with HAL; use HAL;
with HAL.GPIO;

with SAM.Port;
with SAM.Functions;
with SAM.Device;
with SAM.EIC;
with SAM.Interrupt_Names;
with HAL.I2C; use HAL.I2C;
with Cortex_M.NVIC;
with WNM.Time;
with WNM.Samd51;

with Atomic;

package body WNM.FM_Tuner is

   --  Reset : SAM.Port.GPIO_Point renames SAM.Device.PA18;

   Scan_Int : SAM.Port.GPIO_Point renames SAM.Device.PA16;

   subtype SI470X_Registers is I2C_Data (0 .. 31);

   X0A_STATUSRSSI : constant Natural := 0;
   X0B_READCHAN   : constant Natural := 2;
   --  X0C_RDSA       : constant Natural := 4;
   --  X0D_RDSB       : constant Natural := 6;
   --  X0E_RDSC       : constant Natural := 8;
   --  X0F_RDSD       : constant Natural := 10;
   --  X00_DEVICEID   : constant Natural := 12;
   --  X01_CHIPID     : constant Natural := 14;
   X02_POWERCFG   : constant Natural := 16;
   X03_CHANNEL    : constant Natural := 18;
   X04_SYSCONFIG1 : constant Natural := 20;
   X05_SYSCONFIG2 : constant Natural := 22;
   --  X06_SYSCONFIG3 : constant Natural := 24;
   X07_TEST1      : constant Natural := 26;
   --  X08_TEST2      : constant Natural := 28;
   --  X09_BOOTCONFIG : constant Natural := 30;

   Op_In_Progress : aliased Atomic.Flag := Atomic.Init (False);
   Internal_State : State_Kind := Tuned with Volatile;
   Chan_Save : HAL.UInt10 := 0;
   Strength_Save : HAL.UInt8 := 0;

   procedure Read_Registers (Reg : out SI470X_Registers);
   procedure Write_Registers (Reg : SI470X_Registers);
   procedure Init_Tuner;
   pragma Unreferenced (Init_Tuner);
   procedure Extint0_Handler;
   pragma Export (C, Extint0_Handler, "__eic_extint_0_interrupt_handler");

   -----------
   -- State --
   -----------

   function State return State_Kind
   is (Internal_State);

   -------------
   -- Channel --
   -------------

   function Channel return HAL.UInt10
   is (Chan_Save);

   --------------
   -- Strength --
   --------------

   function Strength return HAL.UInt8
   is (Strength_Save);

   --------------------
   -- Read_Registers --
   --------------------

   procedure Read_Registers (Reg : out SI470X_Registers) is
      Status : I2C_Status;
   begin
      WNM.Samd51.I2C_Port.Master_Receive (Addr    => 16#10#,
                                          Data    => Reg,
                                          Status  => Status);
      if Status /= HAL.I2C.Ok then
         raise Program_Error;
      end if;
   end Read_Registers;

   ---------------------
   -- Write_Registers --
   ---------------------

   procedure Write_Registers (Reg : SI470X_Registers) is
      Status : I2C_Status;
   begin
      WNM.Samd51.I2C_Port.Master_Transmit
        (Addr => 16#10#,
         Data => Reg (X02_POWERCFG .. X07_TEST1 + 1),
         Status => Status);
      if Status /= HAL.I2C.Ok then
         raise Program_Error;
      end if;
   end Write_Registers;

   ---------------------
   -- Extint0_Handler --
   ---------------------

   procedure Extint0_Handler is
      Regs : SI470X_Registers;
      In_Progress : Boolean;
   begin
      SAM.EIC.Clear_Interrupt (6);
      Cortex_M.NVIC.Clear_Pending
        (SAM.Interrupt_Names.eic_extint_6_interrupt);

      Atomic.Test_And_Set (Op_In_Progress, In_Progress);
      if In_Progress then
         return;
      end if;

      Read_Registers (Regs);

      --  clear TUNE
      Regs (X03_CHANNEL) := Regs (X03_CHANNEL) and (not 16#80#);

      --  Clear the seek bit
      Regs (X02_POWERCFG) := Regs (X02_POWERCFG) and (not 16#01#);

      --  Get the current channel
      Chan_Save := UInt10 (Shift_Left (UInt16 (Regs (X0B_READCHAN)), 8)
                           or UInt16 (Regs (X0B_READCHAN + 1)))
        and 2#1111111111#;

      --  Get the current strength
      Strength_Save := Regs (X0A_STATUSRSSI + 1);

      Write_Registers (Regs);

      Internal_State := Tuned;

      Atomic.Clear (Op_In_Progress);
   end Extint0_Handler;

   ----------
   -- Seek --
   ----------

   procedure Seek (Dir : Seek_Direction) is
      Reg : SI470X_Registers;
      In_Progress : Boolean;
   begin

      Atomic.Test_And_Set (Op_In_Progress, In_Progress);
      if In_Progress then
         return;
      end if;

      Read_Registers (Reg);

      --  SKMODE
      Reg (X02_POWERCFG) := Reg (X02_POWERCFG) or 16#04#;

      --  Seek direction
      if Dir = Up then
         Reg (X02_POWERCFG) := Reg (X02_POWERCFG) or 16#02#;
         Internal_State := Seeking_Up;
      else
         Reg (X02_POWERCFG) := Reg (X02_POWERCFG) and (not 16#02#);
         Internal_State := Seeking_Down;
      end if;

      --  Start seek
      Reg (X02_POWERCFG) := Reg (X02_POWERCFG) or 16#01#;

      Write_Registers (Reg);

      Atomic.Clear (Op_In_Progress);
   end Seek;

   ----------
   -- Tune --
   ----------

   procedure Tune (Freq : HAL.UInt10) is
      Regs : SI470X_Registers;
      In_Progress : Boolean;
   begin

      Atomic.Test_And_Set (Op_In_Progress, In_Progress);
      if In_Progress then
         return;
      end if;

      Read_Registers (Regs);

      --  Clear channel
      Regs (X03_CHANNEL) := Regs (X03_CHANNEL) and 16#FE#;
      Regs (X03_CHANNEL + 1) := Regs (X03_CHANNEL + 1) and 16#00#;

      --  Set new channel ((Target_Freq - 87.5) * 10)
      Regs (X03_CHANNEL) := UInt8 (Shift_Right (UInt16 (Freq), 8) and 16#03#);
      Regs (X03_CHANNEL + 1) := UInt8 (Freq and 16#FF#);

      --  Start tune operation
      Regs (X03_CHANNEL) := Regs (X03_CHANNEL) or 16#80#; -- Set TUNE

      Write_Registers (Regs);

      Internal_State := Tunning;

      Atomic.Clear (Op_In_Progress);
   end Tune;

   ----------------
   -- Init_Tuner --
   ----------------

   procedure Init_Tuner is
      Regs : SI470X_Registers;

   begin

      -- EIC --

      Cortex_M.NVIC.Enable_Interrupt
        (SAM.Interrupt_Names.eic_extint_0_interrupt);

      -- IO Pin --

      Scan_Int.Clear;
      Scan_Int.Set_Mode (HAL.GPIO.Input);
      Scan_Int.Set_Pull_Resistor (HAL.GPIO.Floating);
      Scan_Int.Set_Function (SAM.Functions.PA16_EIC_EXTINT0);

      -- Tuner --

      Read_Registers (Regs);

      --  Enable the oscillator, from AN230 page 9, rev 0.5 (DOES NOT WORK, wtf
      --  Silicon Labs datasheet?)
      --  si4703_registers[0x07] = 0xBC04;

      --  Enable the oscillator, from AN230 page 9, rev 0.61 (works)
      Regs (X07_TEST1) := 16#81#;
      Regs (X07_TEST1 + 1) := 16#00#;

      --  Set bit 14 to high to enable STC Interrupt
      Regs (X04_SYSCONFIG1) := Regs (X04_SYSCONFIG1) or 16#40#;

      --  Enabled interrupts on GPIO2
      Regs (X04_SYSCONFIG1 + 1) :=
        Regs (X04_SYSCONFIG1 + 1) or 16#04#;

      Write_Registers (Regs);

      Time.Delay_Ms (500);

      Read_Registers (Regs);

      --  Set DMUTE
      Regs (X02_POWERCFG) := 16#40#;

      --  Enable the IC
      Regs (X02_POWERCFG + 1) := 16#01#;

      --  Enable RDS
      Regs (X04_SYSCONFIG1) := Regs (X04_SYSCONFIG1) or 16#10#;

      --  RE - 50kHz Europe setup
      Regs (X04_SYSCONFIG1) := Regs (X04_SYSCONFIG1) or 16#08#;

      --  SPACE = 01 -> 100kHz channel spacing for Europe
      --  BAND = 00 -> 87.5-108 MHz (USA, Europe)
      Regs (X05_SYSCONFIG2 + 1) := Regs (X05_SYSCONFIG2 + 1) or 16#10#;

      --  Set volume to 0
      Regs (X05_SYSCONFIG2 + 1) := Regs (X05_SYSCONFIG2 + 1) and 16#F0#;
      --  Set volume to X
      Regs (X05_SYSCONFIG2 + 1) := Regs (X05_SYSCONFIG2 + 1) or 16#0F#;

      Write_Registers (Regs);
   end Init_Tuner;

--  begin
   --  Init_Tuner;
end WNM.FM_Tuner;
