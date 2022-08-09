with System;
with System.Storage_Elements;

with SAM.Main_Clock;

with HAL;
with HAL.GPIO;

with SAM.Clock_Generator.IDs;
with SAM.Functions;
with SAM.EIC;
with SAM.CMCC;

package body WNM.Samd51 is

   System_Vectors : constant HAL.UInt32;
   pragma Import (Asm, System_Vectors, "__vectors");

   VTOR : System.Address
     with Volatile,
     Address => System.Storage_Elements.To_Address (16#E000_ED08#);

   procedure Unknown_Interrupt;
   pragma Export (C, Unknown_Interrupt, "__common_int_handler");

   -----------------------
   -- Unknown_Interrupt --
   -----------------------

   procedure Unknown_Interrupt is
   begin
      raise Program_Error;
   end Unknown_Interrupt;

   --------------
   -- Init_I2C --
   --------------

   --  Ok_Addr : I2C_Address := 0
   --    with Volatile;

   procedure Init_I2C is
   begin
      -- IOs --

      SCL.Clear;
      SCL.Set_Mode (HAL.GPIO.Output);
      SCL.Set_Pull_Resistor (HAL.GPIO.Floating);

      SDA.Clear;
      SDA.Set_Mode (HAL.GPIO.Output);
      SDA.Set_Pull_Resistor (HAL.GPIO.Floating);

      --  Reset.Clear;
      --  Reset.Set_Mode (HAL.GPIO.Output);
      --  Reset.Set_Pull_Resistor (HAL.GPIO.Floating);

      --  Set the Tuner is I2C mode
      --  SDA.Clear;
      --  Reset.Clear;
      --  Time.Delay_Ms (5);
      --  Reset.Set;
      --  Time.Delay_Ms (5);

      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.SERCOM2_CORE,
         SAM.Clock_Setup_120Mhz.Clk_48Mhz);

      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.SERCOM2_SLOW,
         SAM.Clock_Setup_120Mhz.Clk_32Khz);

      SAM.Main_Clock.SERCOM2_On;

      -- I2C --

      I2C_Port.Configure (Baud => 15); --  150 work for the FM tunner

      I2C_Port.Debug_Stop_Mode (Enabled => True);

      I2C_Port.Enable;

      -- IOs --

      SCL.Set_Function (SAM.Functions.PA13_SERCOM2_PAD1);
      SDA.Set_Function (SAM.Functions.PA12_SERCOM2_PAD0);


      --  for Addr in I2C_Address loop
      --     declare
      --        Status : I2C_Status;
      --     begin
      --        I2C_Port.Master_Transmit (Addr, (1 => 42), Status);
      --        if Status = Ok then
      --           Ok_Addr := Addr;
      --           raise Program_Error with Addr'Img;
      --        end if;
      --     end;
      --  end loop;

   end Init_I2C;

   --------------
   -- Init_EIC --
   --------------

   procedure Init_EIC is
   begin
      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.EIC,
         SAM.Clock_Setup_120Mhz.Clk_48Mhz);

      SAM.Main_Clock.EIC_On;

      --  FM Tunner
      SAM.EIC.Configure (0,
                         SAM.EIC.Fall,
                         Enable_Interrupt => True,
                         Debouncer => False);

      --  Encoders
      SAM.EIC.Configure (6,
                         SAM.EIC.Both,
                         Enable_Interrupt => True,
                         Debouncer => False);
      SAM.EIC.Configure (14,
                         SAM.EIC.Both,
                         Enable_Interrupt => True,
                         Debouncer => False);
      SAM.EIC.Configure (3,
                         SAM.EIC.Both,
                         Enable_Interrupt => True,
                         Debouncer => False);
      SAM.EIC.Configure (2,
                         SAM.EIC.Both,
                         Enable_Interrupt => True,
                         Debouncer => False);

      SAM.EIC.Enable_EIC (SAM.EIC.CLK_ULP32K);

   end Init_EIC;
begin

   --  Set the vector table address
   VTOR := System_Vectors'Address;

   --  Setup the clock system
   SAM.Clock_Setup_120Mhz.Initialize_Clocks;

   --  Turn on and enable DMAC
   SAM.Main_Clock.DMAC_On;
   SAM.DMAC.Enable (DMA_Descs'Access, DMA_WB'Access);

   Init_I2C;
   Init_EIC;

   SAM.CMCC.Enable_Cache;

end WNM.Samd51;
