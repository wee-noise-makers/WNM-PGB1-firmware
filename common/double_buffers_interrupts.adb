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

package body Double_Buffers_Interrupts is

   ------------------------------
   -- DMA_Interrupt_Controller --
   ------------------------------

   protected body DMA_Interrupt_Controller is

      -------------------------
      -- Start_Mem_To_Periph --
      -------------------------

      procedure Start_Mem_To_Periph
        (Destination : Address;
         Source_0    : Address;
         Source_1    : Address;
         Data_Count  : UInt16)
      is
      begin

         Configure_Data_Flow
           (Controller.all,
            Stream,
            Source      => Source_0,
            Destination => Destination,
            Data_Count  => Data_Count);

         for Selected_Interrupt in DMA_Interrupt loop
            Enable_Interrupt (Controller.all, Stream, Selected_Interrupt);
         end loop;


         Configure_Double_Buffered_Mode (This              => Controller.all,
                                         Stream            => Stream,
                                         Buffer_0_Value    => Source_0,
                                         Buffer_1_Value    => Source_1,
                                         First_Buffer_Used => Memory_Buffer_0);

         Buffer_0 := Source_0;
         Buffer_1 := Source_1;

         Enable_Double_Buffered_Mode (Controller.all, Stream);

         Clear_All_Status (Controller.all, Stream);

         Enable (Controller.all, Stream);
      end Start_Mem_To_Periph;

      -------------------------
      -- Start_Periph_To_Mem --
      -------------------------

      procedure Start_Periph_To_Mem
        (Source        : Address;
         Destination_0 : Address;
         Destination_1 : Address;
         Data_Count    : UInt16)
      is
      begin

         Configure_Data_Flow
           (Controller.all,
            Stream,
            Source      => Source,
            Destination => Destination_0,
            Data_Count  => Data_Count);

         for Selected_Interrupt in DMA_Interrupt loop
            Enable_Interrupt (Controller.all, Stream, Selected_Interrupt);
         end loop;


         Configure_Double_Buffered_Mode (This              => Controller.all,
                                         Stream            => Stream,
                                         Buffer_0_Value    => Destination_0,
                                         Buffer_1_Value    => Destination_1,
                                         First_Buffer_Used => Memory_Buffer_0);

         Buffer_0 := Destination_0;
         Buffer_1 := Destination_1;

         Clear_All_Status (Controller.all, Stream);

         Enable_Double_Buffered_Mode (Controller.all, Stream);

         Enable (Controller.all, Stream);
      end Start_Periph_To_Mem;

      ------------------------
      -- Wait_For_Interrupt --
      ------------------------

      entry Wait_For_Interrupt when Interrupt_Triggered is
      begin
         Interrupt_Triggered := False;
      end Wait_For_Interrupt;

      ---------------------
      -- Not_In_Transfer --
      ---------------------

      function Not_In_Transfer return Address is
      begin
         case Current_Memory_Buffer (Controller.all, Stream) is
            when Memory_Buffer_0 =>
               return Buffer_1;
            when Memory_Buffer_1 =>
               return Buffer_0;
         end case;
      end Not_In_Transfer;

      -----------------------
      -- Interrupt_Handler --
      -----------------------

      procedure Interrupt_Handler is
      begin
         for Flag in DMA_Status_Flag loop
            if Status (Controller.all, Stream, Flag) then
               case Flag is
                  when FIFO_Error_Indicated =>
                     --                       raise Program_Error with "FIFO_Error_Indicated";
                     null;
                  when Direct_Mode_Error_Indicated =>
                     raise Program_Error with "Direct_Mode_Error";
                  when Transfer_Error_Indicated =>
                     raise Program_Error with "Transfer_Error";
                  when Half_Transfer_Complete_Indicated =>
                     null;
                  when Transfer_Complete_Indicated =>
                     Interrupt_Triggered := True;
               end case;
               Clear_Status (Controller.all, Stream, Flag);
            end if;
         end loop;

      end Interrupt_Handler;

   end DMA_Interrupt_Controller;

end Double_Buffers_Interrupts;
