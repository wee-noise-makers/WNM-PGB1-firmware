-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2023 Fabien Chouteau                    --
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

with WNM_HAL_Settings; use WNM_HAL_Settings;

with System;

with RP.GPIO; use RP.GPIO;

with Atomic.Signed;

with WNM_HAL_PIO_Encoders_ASM; use WNM_HAL_PIO_Encoders_ASM;
with RP.PIO; use RP.PIO;
with RP_Interrupts;
with RP2040_SVD.Interrupts;

package body WNM_HAL.Encoders is

   procedure PIO0_IRQ0_Handler;
   procedure PIO0_IRQ1_Handler;

   package Atomic_Int is new Atomic.Signed (Integer);

   LA : RP.GPIO.GPIO_Point := (Pin => 28);
   LB : RP.GPIO.GPIO_Point := (Pin => 29);
   RA : RP.GPIO.GPIO_Point := (Pin => 26);
   RB : RP.GPIO.GPIO_Point := (Pin => 27);

   --  Side set pins for debugging the PIO program
   Enable_Sideset_Debug : constant Boolean := False;
   Side1 : RP.GPIO.GPIO_Point := (Pin => 9);
   Side2 : RP.GPIO.GPIO_Point := (Pin => 10);

   Val_L : aliased Atomic_Int.Instance;
   Val_R : aliased Atomic_Int.Instance;

   -----------------------
   -- PIO0_IRQ0_Handler --
   -----------------------

   procedure PIO0_IRQ0_Handler is
   begin
      if Encoder_PIO.SM_IRQ_Status (0) then
         Atomic_Int.Sub (Val_L, 1);
         Encoder_PIO.Ack_SM_IRQ (0);
      elsif Encoder_PIO.SM_IRQ_Status (2) then
         Atomic_Int.Add (Val_L, 1);
         Encoder_PIO.Ack_SM_IRQ (2);
      end if;
   end PIO0_IRQ0_Handler;

   -----------------------
   -- PIO0_IRQ1_Handler --
   -----------------------

   procedure PIO0_IRQ1_Handler is
   begin
      if Encoder_PIO.SM_IRQ_Status (1) then
         Atomic_Int.Sub (Val_R, 1);
         Encoder_PIO.Ack_SM_IRQ (1);
      elsif Encoder_PIO.SM_IRQ_Status (3) then
         Atomic_Int.Add (Val_R, 1);
         Encoder_PIO.Ack_SM_IRQ (3);
      end if;
   end PIO0_IRQ1_Handler;

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
      Config : PIO_SM_Config := Default_SM_Config;
   begin
      LA.Configure (Input, Pull_Up, Encoder_PIO.GPIO_Function);
      LB.Configure (Input, Pull_Up, Encoder_PIO.GPIO_Function);

      RA.Configure (Input, Pull_Up, Encoder_PIO.GPIO_Function);
      RB.Configure (Input, Pull_Up, Encoder_PIO.GPIO_Function);

      if Enable_Sideset_Debug then
         --  Side set pins for debugging the PIO program
         Set_Sideset_Pins (Config, Side1.Pin);
         Set_Sideset (Config,
                      Bit_Count => 2,
                      Optional  => False,
                      Pindirs   => False);

         Side1.Configure (Output, Floating, Encoder_PIO.GPIO_Function);
         Side2.Configure (Output, Floating, Encoder_PIO.GPIO_Function);
         Set_Pin_Direction (Encoder_PIO,
                            SM        => Encoder_L_SM,
                            Pin       => Side1.Pin,
                            Direction => Output);
         Set_Pin_Direction (Encoder_PIO,
                            SM        => Encoder_L_SM,
                            Pin       => Side2.Pin,
                            Direction => Output);
      end if;

      Encoder_PIO.Load (Pio_Rotary_Encoder_Program_Instructions,
                        Offset => Encoder_Offset);

      Set_In_Shift (Config,
                    Shift_Right    => False,
                    Autopush       => False,
                    Push_Threshold => 32);

      Set_Wrap (Config,
                Encoder_Offset + Pio_Rotary_Encoder_Wrap_Target,
                Encoder_Offset + Pio_Rotary_Encoder_Wrap);

      Set_Clock_Frequency (Config, 5_000);

      Set_In_Pins (Config, LA.Pin);
      Encoder_PIO.SM_Initialize (Encoder_L_SM,
                                 Encoder_Offset + 16,
                                 Config);

      RP_Interrupts.Attach_Handler
        (PIO0_IRQ0_Handler'Access,
         RP2040_SVD.Interrupts.PIO0_IRQ_0_Interrupt,
         System.Interrupt_Priority'Last);

      RP_Interrupts.Attach_Handler
        (PIO0_IRQ1_Handler'Access,
         RP2040_SVD.Interrupts.PIO0_IRQ_1_Interrupt,
         System.Interrupt_Priority'Last);

      --  Enable SM IRQ flags 0 and 2 for the SM 0 on PIO IRQ line 0 (see
      --  pio_rotary_encoder.pio).
      pragma Compile_Time_Error (Encoder_L_SM /= 0,
                                 "Left encoder must use SM0");
      Encoder_PIO.Enable_IRQ_Flag (Encoder_L_IRQ, SM_IRQ0);
      Encoder_PIO.Enable_IRQ_Flag (Encoder_L_IRQ, SM_IRQ2);
      Encoder_PIO.Enable_IRQ (Encoder_L_IRQ);

      Set_In_Pins (Config, RA.Pin);
      Encoder_PIO.SM_Initialize (Encoder_R_SM,
                                 Encoder_Offset + 16,
                                 Config);

      --  Enable SM IRQ flags 1 and 3 for the SM 1 on PIO IRQ line 1 (see
      --  pio_rotary_encoder.pio).
      pragma Compile_Time_Error (Encoder_R_SM /= 1,
                                 "Right encoder must use SM1");
      Encoder_PIO.Enable_IRQ_Flag (Encoder_R_IRQ, SM_IRQ1);
      Encoder_PIO.Enable_IRQ_Flag (Encoder_R_IRQ, SM_IRQ3);
      Encoder_PIO.Enable_IRQ (Encoder_R_IRQ);

      Encoder_PIO.Set_Enabled (Encoder_L_SM, True);
      Encoder_PIO.Set_Enabled (Encoder_R_SM, True);
   end Initialize;

begin
   Initialize;
end WNM_HAL.Encoders;
