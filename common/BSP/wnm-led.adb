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

with STM32.Timers; use STM32.Timers;
with STM32.GPIO;   use STM32.GPIO;
with STM32.Device; use STM32.Device;
with HAL;          use HAL;

package body WNM.LED is

   type Row_Index is range 1 .. 2;
   type Col_Index is range 1 .. 9;

   Row_To_Point : array (Row_Index) of GPIO_Point :=
     (1 => PD13,
      2 => PD12);

   Col_To_Point : array (Col_Index) of GPIO_Point :=
     (1 => PC5,
      2 => PB1,
      3 => PE8,
      4 => PE10,
      5 => PE12,
      6 => PE14,
      7 => PB10,
      8 => PB13,
      9 => PD11);

   type LED_Address is record
      Row : Row_Index;
      Col : Col_Index;
   end record;

   LED_To_Address : constant array (LEDs) of LED_Address :=
     (B1      => (Row => 2, Col => 1),
      B2      => (Row => 2, Col => 2),
      B3      => (Row => 2, Col => 3),
      B4      => (Row => 2, Col => 4),
      B5      => (Row => 2, Col => 5),
      B6      => (Row => 2, Col => 6),
      B7      => (Row => 2, Col => 7),
      B8      => (Row => 2, Col => 8),
      B9      => (Row => 1, Col => 1),
      B10     => (Row => 1, Col => 2),
      B11     => (Row => 1, Col => 3),
      B12     => (Row => 1, Col => 4),
      B13     => (Row => 1, Col => 5),
      B14     => (Row => 1, Col => 6),
      B15     => (Row => 1, Col => 7),
      B16     => (Row => 1, Col => 8),
      Rec     => (Row => 1, Col => 9),
      Play    => (Row => 2, Col => 9));

   LED_State : array (LEDs) of UInt8 := (others => 0);

   LED_Timer : STM32.Timers.Timer renames Timer_7;

   protected LED_Timer_Handler is
      pragma Interrupt_Priority;

   private

      Current_LED : LEDs := LEDs'First;

      procedure IRQ_Handler;
      pragma Attach_Handler (IRQ_Handler, Ada.Interrupts.Names.TIM7_Interrupt);

   end LED_Timer_Handler;

   --  LED_Timer_Handler is only referenced by the interrupt handler
   pragma Unreferenced (LED_Timer_Handler);

   procedure Turn_On (LED_Addr : LED_Address) with Inline_Always;
   procedure Turn_Off (LED_Addr : LED_Address) with Inline_Always;

   -----------
   -- Start --
   -----------

   procedure Start is
      Config : GPIO_Port_Configuration;
   begin
      -- LEDs --

      Config.Mode := Mode_Out;
      Config.Output_Type := Push_Pull;
      Config.Resistors := Floating;

      for Pt of Row_To_Point loop
         Enable_Clock (Pt);
         Pt.Configure_IO (Config);
         Pt.Set;
      end loop;

      for Pt of Col_To_Point loop
         Enable_Clock (Pt);
         Pt.Configure_IO (Config);
         Pt.Clear;
      end loop;

      for B in LEDs loop
         Turn_Off (B);
      end loop;

      --  LED Timer --

      Enable_Clock (LED_Timer);

      Reset (LED_Timer);

      Configure (LED_Timer, Prescaler => 100, Period => 100);

      Enable_Interrupt (LED_Timer, Timer_Update_Interrupt);

      Enable (LED_Timer);
   end Start;

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (B : LEDs) is
   begin
      LED_State (B) := LED_State (B) + 1;
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (B : LEDs) is
   begin
      LED_State (B) := 0;
   end Turn_Off;

   ------------------
   -- Turn_Off_All --
   ------------------

   procedure Turn_Off_All is
   begin
      LED_State := (others => 0);
   end Turn_Off_All;
   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (LED_Addr : LED_Address)
   is
   begin
      Row_To_Point (LED_Addr.Row).Set;
      Col_To_Point (LED_Addr.Col).Clear;
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (LED_Addr : LED_Address)
   is
   begin
      Row_To_Point (LED_Addr.Row).Clear;
      Col_To_Point (LED_Addr.Col).Set;
   end Turn_Off;

   -----------------------
   -- LED_Timer_Handler --
   -----------------------

   protected body LED_Timer_Handler is

      -----------------
      -- IRQ_Handler --
      -----------------

      procedure IRQ_Handler is
      begin
         if Status (LED_Timer, Timer_Update_Indicated) then
            if Interrupt_Enabled (LED_Timer, Timer_Update_Interrupt) then
               Clear_Pending_Interrupt (LED_Timer, Timer_Update_Interrupt);
            end if;

            Turn_Off (LED_To_Address (Current_LED));

            if Current_LED = LEDs'Last then
               Current_LED := LEDs'First;
            else
               Current_LED := LEDs'Succ (Current_LED);
            end if;

            if LED_State (Current_LED) > 0 then
               Turn_On (LED_To_Address (Current_LED));
               Configure (LED_Timer,
                          Prescaler => 50,
                          Period => (case LED_State (Current_LED) is
                                        when 1      => 700,
                                        when others => 3000));
            else
               Configure (LED_Timer,
                          Prescaler => 50,
                          Period => 500);
            end if;

         end if;
      end IRQ_Handler;
   end LED_Timer_Handler;

end WNM.LED;
