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

with Ada.Interrupts.Names;

with HAL;                  use HAL;
with STM32.Device;         use STM32.Device;
with STM32.ADC;            use STM32.ADC;

package body WNM.Battery is

   All_Regular_Conversions : constant Regular_Channel_Conversions :=
     (1 => (Channel => VBat.Channel, Sample_Time => Sample_15_Cycles));

   Supply_Voltage : constant := 3300;  -- millivolts

   type Mes_Index is mod 20;
   type Mes_Array is array (Mes_Index) of Natural;

   procedure Initialize;

   protected Handler is
      pragma Interrupt_Priority;

      function Millivolts return Natural;

   private

      Values : Mes_Array := (others => 0);
      Index  : Mes_Index := Mes_Index'First;

      Last_Millivolts : Natural;
      Last_Read : Time := Clock;

      procedure IRQ_Handler with
        Attach_Handler => Ada.Interrupts.Names.ADC_Interrupt;

   end Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Enable_Clock (VBat.ADC.all);

      Configure_Common_Properties
        (Mode           => Independent,
         Prescalar      => PCLK2_Div_2,
         DMA_Mode       => Disabled,
         Sampling_Delay => Sampling_Delay_5_Cycles);

      Configure_Unit
        (VBat.ADC.all,
         Resolution => ADC_Resolution_12_Bits,
         Alignment  => Right_Aligned);

      Configure_Regular_Conversions
        (VBat.ADC.all,
         Continuous  => False,
         Trigger     => Software_Triggered,
         Enable_EOC  => True,
         Conversions => All_Regular_Conversions);

      Enable_Interrupts (VBat.ADC.all, Regular_Channel_Conversion_Complete);

      Enable (VBat.ADC.all);

      --  Do a first conversion
      Start_Conversion (VBat.ADC.all);

   end Initialize;

   -------------
   -- Handler --
   -------------

   protected body Handler is

      ----------------
      -- Millivolts --
      ----------------

      function Millivolts return Natural is
         Acc : Natural := 0;
      begin
         if Clock - Last_Read > Milliseconds (100) then
            Start_Conversion (VBat.ADC.all);
         end if;

         for Val of Values loop
            Acc := Acc + Val;
         end loop;

         return Acc / Natural (Values'Length);
      end Millivolts;

      -----------------
      -- IRQ_Handler --
      -----------------

      procedure IRQ_Handler is
         Counts  : Natural;
      begin
         if Status (VBat.ADC.all, Regular_Channel_Conversion_Complete) then
            if Interrupt_Enabled (VBat.ADC.all, Regular_Channel_Conversion_Complete) then
               Clear_Interrupt_Pending (VBat.ADC.all, Regular_Channel_Conversion_Complete);

               Counts := Natural (Conversion_Value (VBat.ADC.all));

               Last_Millivolts :=
                 ((Counts * VBat_Bridge_Divisor) * Supply_Voltage);

               --  There's another voltage divider on the board
               Last_Millivolts := Last_Millivolts * 2;

               --  16#FFF# because we are using 12-bit conversion resolution
               Last_Millivolts := Last_Millivolts / 16#FFF#;

               Values (Index) := Last_Millivolts;
               Index := Index + 1;
            end if;
         end if;
      end IRQ_Handler;

   end Handler;

   ----------------
   -- Millivolts --
   ----------------

   function Millivolts return Natural
   is (Handler.Millivolts);

begin
   Initialize;
end WNM.Battery;
