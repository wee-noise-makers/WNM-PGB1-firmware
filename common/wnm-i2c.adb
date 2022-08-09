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

with STM32.Device;         use STM32.Device;
with STM32.GPIO;           use STM32.GPIO;
with STM32.I2C;            use STM32.I2C;
with STM32.I2C.DMA;        use STM32.I2C.DMA;
with STM32.DMA;            use STM32.DMA;
with STM32.DMA.Interrupts;

with Ravenscar_Time;
with HAL; use HAL;
with HAL.I2C; use HAL.I2C;
with STM32.I2S; use STM32.I2S;

package body WNM.I2C is

   I2C_Port : STM32.I2C.DMA.I2C_Port_DMA renames I2C_1_DMA;

   I2C_DMA        : STM32.DMA.DMA_Controller renames DMA_1;
   I2C_DMA_Chan   : STM32.DMA.DMA_Channel_Selector renames
     STM32.DMA.Channel_1;
   I2C_DMA_Stream : STM32.DMA.DMA_Stream_Selector renames
     STM32.DMA.Stream_6;
   I2C_DMA_Int    : STM32.DMA.Interrupts.DMA_Interrupt_Controller renames
     DMA1_Stream6;

   procedure Initialize;
   procedure Initialize_I2C_GPIO;
   procedure Initialize_I2C;
   procedure Initialize_DMA;

   -------------------------
   -- Initialize_I2C_GPIO --
   -------------------------

   procedure Initialize_I2C_GPIO is
      Points : GPIO_Points (1 .. 2);
   begin
      Points := (PB6, PB7);

      Enable_Clock (Points);

      Configure_Alternate_Function (Points, GPIO_AF_I2C1_4);

      Configure_IO (Points,
                    (Speed       => Speed_High,
                     Mode        => Mode_AF,
                     Output_Type => Open_Drain,
                     Resistors   => Floating));
      --  Lock (Points);
   end Initialize_I2C_GPIO;

   --------------------
   -- Initialize_I2C --
   --------------------

   procedure Initialize_I2C
   is
      I2C_Conf : I2C_Configuration;
   begin

      Enable_Clock (I2C_Port);
      Ravenscar_Time.Delays.Delay_Milliseconds (200);
      Reset (I2C_Port);

      I2C_Conf.Own_Address := 16#00#;
      I2C_Conf.Addressing_Mode := Addressing_Mode_7bit;
      I2C_Conf.General_Call_Enabled := False;
      I2C_Conf.Clock_Stretching_Enabled := True;

      I2C_Conf.Clock_Speed := 400_000;
      I2C_Conf.Enable_DMA := True;

      I2C_Port.Configure (I2C_Conf);
   end Initialize_I2C;

   --------------------
   -- Initialize_DMA --
   --------------------

   procedure Initialize_DMA is
      Config : DMA_Stream_Configuration;
   begin
      Enable_Clock (I2C_DMA);

      Config.Increment_Peripheral_Address := False;
      Config.Increment_Memory_Address := True;
      Config.Peripheral_Data_Format := Bytes;
      Config.Memory_Data_Format := Bytes;
      Config.Operation_Mode := Normal_Mode;
      Config.Priority := Priority_High;
      Config.FIFO_Enabled := False;
      Config.FIFO_Threshold := FIFO_Threshold_Full_Configuration;
      Config.Memory_Burst_Size := Memory_Burst_Inc4;
      Config.Peripheral_Burst_Size := Peripheral_Burst_Single;

      -- TX DMA --

      Config.Channel   := I2C_DMA_Chan;
      Config.Direction := Memory_To_Peripheral;

      Configure (I2C_DMA, I2C_DMA_Stream, Config);

      I2C_Port.Set_TX_DMA_Handler (I2C_DMA_Int'Access);
--        I2C_Port.Set_Polling_Threshold (1000);
   end Initialize_DMA;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
   begin
      Initialize_I2C_GPIO;
      Initialize_DMA;
      Initialize_I2C;
   end Initialize;

   ----------
   -- Port --
   ----------

   function Port
      return not null HAL.I2C.Any_I2C_Port
   is
   begin
      return I2C_Port'Access;
   end Port;

begin
   Initialize;
end WNM.I2C;
