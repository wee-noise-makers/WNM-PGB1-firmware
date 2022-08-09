-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2021 Fabien Chouteau                  --
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

with HAL.GPIO;

with SAM.Device;
with SAM.SERCOM.SPI;
with SAM.Port;
with SAM.Clock_Generator;
with SAM.Clock_Generator.IDs;
with SAM.Main_Clock;
with SAM.Functions;
with SAM.DMAC; use SAM.DMAC;
with SAM.DMAC.Sources;
with SAM.Interrupt_Names;

with WNM.Samd51; use WNM.Samd51;

with Cortex_M.NVIC;

package body WNM.LED is

   SPI : SAM.SERCOM.SPI.SPI_Device renames SAM.Device.SPI1;
   MOSI : SAM.Port.GPIO_Point renames SAM.Device.PB23;

   LED_Brightness : constant array (LEDs) of UInt8 := (others => 0);

   LED_Data : UInt8_Array (1 .. 18 * 24 + 50) := (others => 0);
   --  As a first quick implementation, we use one byte for each bit of the
   --  ws2812 data. +50 low bits for reset.

   Update_In_Progress : Boolean := False
     with Volatile;
   Next_Start : WNM.Time.Time_Ms := WNM.Time.Clock + LED_Task_Period_Ms;

   procedure DMA_Int_Handler;
   pragma Export (C, DMA_Int_Handler, "__dmac_tcmpl_1_handler");

   L0 : constant UInt8 := 2#11100000#;
   L1 : constant UInt8 := 2#11111000#;

   LED_Red : constant UInt8_Array (1 .. 24) :=
     (
      L0, L0, L0, L0, L0, L0, L0, L0, -- Green
      L0, L0, L0, L0, L0, L1, L1, L1, -- Red
      L0, L0, L0, L0, L0, L0, L0, L0  -- Blue
     );

   LED_Green : constant UInt8_Array (1 .. 24) :=
     (
      L0, L0, L0, L0, L0, L1, L1, L1, -- Green
      L0, L0, L0, L0, L0, L0, L0, L0, -- Red
      L0, L0, L0, L0, L0, L0, L0, L0  -- Blue
     );

   LED_Blue : constant UInt8_Array (1 .. 24) :=
     (
      L0, L0, L0, L0, L0, L0, L0, L0, -- Green
      L0, L0, L0, L0, L0, L0, L0, L0, -- Red
      L0, L0, L0, L0, L0, L1, L1, L1  -- Blue
     );

   function To_LED_Index (B : LEDs) return Natural
   is (case B is
          when B1 => LED_Data'First + 0 * 24,
          when B2 => LED_Data'First + 1 * 24,
          when B3 => LED_Data'First + 2 * 24,
          when B4 => LED_Data'First + 3 * 24,
          when B5 => LED_Data'First + 4 * 24,
          when B6 => LED_Data'First + 5 * 24,
          when B7 => LED_Data'First + 6 * 24,
          when B8 => LED_Data'First + 7 * 24,
          when B9 => LED_Data'First + 9 * 24,
          when B10 => LED_Data'First + 10 * 24,
          when B11 => LED_Data'First + 11 * 24,
          when B12 => LED_Data'First + 12 * 24,
          when B13 => LED_Data'First + 13 * 24,
          when B14 => LED_Data'First + 14 * 24,
          when B15 => LED_Data'First + 15 * 24,
          when B16 => LED_Data'First + 16 * 24,
          when Play => LED_Data'First + 8 * 24,
          when Rec  => LED_Data'First + 17 * 24);

   ---------------------
   -- DMA_Int_Handler --
   ---------------------

   procedure DMA_Int_Handler is
   begin
      Clear (DMA_LED_SPI, Transfer_Complete);

      Update_In_Progress := False;
   end DMA_Int_Handler;

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (B : LEDs) is
      First : constant Natural := To_LED_Index (B);
   begin
      if B = Rec then
         LED_Data (First .. First + 23) := LED_Red;
      elsif B = Play then
         LED_Data (First .. First + 23) := LED_Green;
      else
         LED_Data (First .. First + 23) := LED_Blue;
      end if;
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (B : LEDs) is
      First : constant Natural := To_LED_Index (B);
   begin
      LED_Data (First .. First + 23) := (others => 0);
   end Turn_Off;

   ------------------
   -- Turn_Off_All --
   ------------------

   procedure Turn_Off_All is
   begin
      LED_Data (LED_Data'First .. LED_Data'First + 18 * 24) :=
        (others => L0);
   end Turn_Off_All;

   ----------------
   -- Brightness --
   ----------------

   function Brightness (B : LEDS) return UInt8
   is (LED_Brightness (B));

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      -- Clocks --

      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.SERCOM1_CORE, Clk_48Mhz);

      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.SERCOM1_SLOW, Clk_32Khz);

      SAM.Main_Clock.SERCOM1_On;

      -- SPI --

      SPI.Configure
        (Baud                => 2,
         Data_Order          => SAM.SERCOM.SPI.Most_Significant_First,
         Phase               => SAM.SERCOM.SPI.Sample_Leading_Edge,
         Polarity            => SAM.SERCOM.SPI.Active_High,
         DIPO                => 0,
         DOPO                => 2,
         Slave_Select_Enable => False);

      SPI.Debug_Stop_Mode (Enabled => True);

      SPI.Enable;

      -- IOs --

      MOSI.Clear;
      MOSI.Set_Mode (HAL.GPIO.Output);
      MOSI.Set_Pull_Resistor (HAL.GPIO.Pull_Down);
      MOSI.Set_Function (SAM.Functions.PB23_SERCOM1_PAD3);

      -- DMA --

      Configure (DMA_LED_SPI,
                 Trig_Src       => SAM.DMAC.Sources.SERCOM1_TX,
                 Trig_Action    => Burst,
                 Priority       => 0,
                 Burst_Len      => 1,
                 Threshold      => BEAT_1,
                 Run_In_Standby => False);

      Enable (DMA_LED_SPI, Transfer_Complete);

      Cortex_M.NVIC.Enable_Interrupt (SAM.Interrupt_Names.dmac_1_interrupt);

      Configure_Descriptor (DMA_Descs (DMA_LED_SPI),
                            Valid           => True,
                            Event_Output    => Disable,
                            Block_Action    => Interrupt,
                            Beat_Size       => B_8bit,
                            Src_Addr_Inc    => True,
                            Dst_Addr_Inc    => False,
                            Step_Selection  => Source,
                            Step_Size       => X1);

      --  Start the first DMA transfer

      Update_In_Progress := True;

      Clear (DMA_LED_SPI, Transfer_Complete);

      Set_Data_Transfer (DMA_Descs (DMA_LED_SPI),
                         Block_Transfer_Count => LED_Data'Length,
                         Src_Addr             => LED_Data'Address,
                         Dst_Addr             => SPI.Data_Address);

      Enable (DMA_LED_SPI);

   end Initialize;

   ------------
   -- Update --
   ------------

   function Update return WNM.Time.Time_Ms is
   begin

      --  I didn't find a way to get the MOSI line low when the SPI is idle (not
      --  transmitting). The problem with this is that when two transmission are
      --  too close to each others the LEDs will take the high-when-idle state
      --  of the MOSI line as input data. The solution used here is to make sure
      --  we don't update the LED too quickly.

      if not Update_In_Progress
        and then
          WNM.Time.Clock >= Next_Start
      then

         Next_Start := Next_Start + LED_Task_Period_Ms;
         Update_In_Progress := True;

         Set_Data_Transfer (DMA_Descs (DMA_LED_SPI),
                            Block_Transfer_Count => LED_Data'Length,
                            Src_Addr             => LED_Data'Address,
                            Dst_Addr             => SPI.Data_Address);

         Enable (DMA_LED_SPI);
      end if;

      return Next_Start;
   end Update;

begin
   Initialize;
end WNM.LED;
