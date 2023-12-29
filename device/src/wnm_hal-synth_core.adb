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

with WNM.Tasks;
with RP.Multicore.FIFO;
with RP.Multicore.Spinlocks;
with RP_Interrupts;
with RP2040_SVD.Interrupts;
with RP2040_SVD.SIO;
with Cortex_M.NVIC;

package body WNM_HAL.Synth_Core is

   Scartch_X_Size  : constant := 4 * 1024;
   Scartch_X_Start : constant := 16#20040000#;
   Scartch_X_End   : constant := Scartch_X_Start + Scartch_X_Size;

   procedure Main;

   Vector : Integer;
   pragma Import (C, Vector, "__vectors");

   -----------------
   -- Trap_Vector --
   -----------------

   function Trap_Vector return HAL.UInt32
   is (HAL.UInt32 (System.Storage_Elements.To_Integer (Vector'Address)));

   -------------------
   -- Stack_Pointer --
   -------------------

   function Stack_Pointer return HAL.UInt32
   is (Scartch_X_End);

   -----------------
   -- Entry_Point --
   -----------------

   function Entry_Point return HAL.UInt32
   is (HAL.UInt32 (System.Storage_Elements.To_Integer (Main'Address)));

   ----------
   -- Main --
   ----------

   procedure Main is
   begin
      --  Make sure we don't have data left in the FIFO after reset
      RP.Multicore.FIFO.Drain;

      --  Clear FIFO Status
      RP2040_SVD.SIO.SIO_Periph.FIFO_ST := (others => <>);

      --  Make sure we don't have spinlocks locked after reset
      for Id in RP.Multicore.Spinlocks.Lock_Id loop
         RP.Multicore.Spinlocks.Release (Id);
      end loop;

      --  Attach copproc interrupt handler
      RP_Interrupts.Attach_Handler
        (WNM.Tasks.Synth_Coproc_Receive'Access,
         RP2040_SVD.Interrupts.SIO_IRQ_PROC1_Interrupt,
         RP_Interrupts.Interrupt_Priority'Last);

      Cortex_M.NVIC.Set_Priority
        (RP2040_SVD.Interrupts.SIO_IRQ_PROC1_Interrupt,
         1);

      WNM.Tasks.Synth_Core;
   end Main;

end WNM_HAL.Synth_Core;
