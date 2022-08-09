-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with RP.GPIO; use RP.GPIO;

with Cortex_M.NVIC;
with HAL; use HAL;
with Atomic.Signed;
with Atomic.Unsigned;

with WNM.RP2040.PIO_Rotary_Encoder_ASM; use WNM.RP2040.PIO_Rotary_Encoder_ASM;
with WNM.RP2040.PIO; use WNM.RP2040.PIO;
with RP.PIO; use RP.PIO;

package body WNM.RP2040.Encoders is

   procedure PIO0_IRQ0_Handler
     with Export => True,
     Convention => C,
     External_Name => "isr_irq7";

   procedure PIO0_IRQ1_Handler
     with Export => True,
     Convention => C,
     External_Name => "isr_irq8";

   package Atomic_Int is new Atomic.Signed (Integer);
   package Atomic_Uint is new Atomic.Unsigned (UInt32);

   LA : RP.GPIO.GPIO_Point := (Pin => 0);
   LB : RP.GPIO.GPIO_Point := (Pin => 1);
   RA : RP.GPIO.GPIO_Point := (Pin => 5);
   RB : RP.GPIO.GPIO_Point := (Pin => 6);

   --  Side set pins for debugging the PIO program
   Enable_Sideset_Debug : constant Boolean := False;
   Side1 : RP.GPIO.GPIO_Point := (Pin => 9);
   Side2 : RP.GPIO.GPIO_Point := (Pin => 10);

   Val_L : aliased Atomic_Int.Instance;
   Val_R : aliased Atomic_Int.Instance;

   Test : aliased Atomic_Uint.Instance;

   -----------------------
   -- PIO0_IRQ0_Handler --
   -----------------------

   procedure PIO0_IRQ0_Handler is
   begin
      Atomic_Uint.Add (Test, 1);
      if Encoder_PIO.SM_IRQ_Status (0) then
         Atomic_Int.Add (Val_L, 1);
         Encoder_PIO.Ack_SM_IRQ (0);
      elsif Encoder_PIO.SM_IRQ_Status (2) then
         Atomic_Int.Sub (Val_L, 1);
         Encoder_PIO.Ack_SM_IRQ (2);
      end if;
   end PIO0_IRQ0_Handler;

   -----------------------
   -- PIO0_IRQ1_Handler --
   -----------------------

   procedure PIO0_IRQ1_Handler is
   begin
      if Encoder_PIO.SM_IRQ_Status (1) then
         Atomic_Int.Add (Val_R, 1);
         Encoder_PIO.Ack_SM_IRQ (1);
      elsif Encoder_PIO.SM_IRQ_Status (3) then
         Atomic_Int.Sub (Val_R, 1);
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

      --  Enable IRQ0 for the SM 0 (see pio_rotary_encoder.pio)
      pragma Compile_Time_Error (Encoder_L_SM /= 0, "Left encoder must use SM0");
      Encoder_PIO.Enable_IRQ_Flag (0, SM_IRQ0);
      Encoder_PIO.Enable_IRQ_Flag (0, SM_IRQ2);
      Encoder_PIO.Enable_IRQ (0);

      Set_In_Pins (Config, RA.Pin);
      Encoder_PIO.SM_Initialize (Encoder_R_SM,
                                 Encoder_Offset + 16,
                                 Config);

      --  Enable IRQ1 for the SM 1 (see pio_rotary_encoder.pio)
      pragma Compile_Time_Error (Encoder_R_SM /= 1, "Right encoder must use SM1");
      Encoder_PIO.Enable_IRQ_Flag (1, SM_IRQ1);
      Encoder_PIO.Enable_IRQ_Flag (1, SM_IRQ3);
      Encoder_PIO.Enable_IRQ (1);

      Encoder_PIO.Set_Enabled (Encoder_L_SM, True);
      Encoder_PIO.Set_Enabled (Encoder_R_SM, True);
   end Initialize;

begin
   Initialize;
end WNM.RP2040.Encoders;
