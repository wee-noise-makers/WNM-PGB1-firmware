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

with Ada.Interrupts;
with STM32.DMA;      use STM32.DMA;
with System;         use System;
with HAL;            use HAL;

package Double_Buffers_Interrupts is

   protected type DMA_Interrupt_Controller
     (Controller : not null access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      ID         : Ada.Interrupts.Interrupt_ID)
   is
      pragma Interrupt_Priority (Any_Priority'Last);

      procedure Start_Mem_To_Periph
        (Destination : Address;
         Source_0    : Address;
         Source_1    : Address;
         Data_Count  : UInt16);

      procedure Start_Periph_To_Mem
        (Source        : Address;
         Destination_0 : Address;
         Destination_1 : Address;
         Data_Count    : UInt16);

      entry Wait_For_Interrupt;

      function Not_In_Transfer return Address;
   private

      procedure Interrupt_Handler;
      pragma Attach_Handler (Interrupt_Handler, ID);

      Interrupt_Triggered : Boolean := False;
      Buffer_0 : Address := Null_Address;
      Buffer_1 : Address := Null_Address;
   end DMA_Interrupt_Controller;

   type DMA_Interrupt_Controller_Access is access all DMA_Interrupt_Controller;

end Double_Buffers_Interrupts;
