------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of STMicroelectronics nor the names of its       --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
--  This file is based on:                                                  --
--   @file    stm32f769i_discovery_sd.h                                     --
--   @author  MCD Application Team                                          --
------------------------------------------------------------------------------

with HAL.SDMMC;
with STM32.SDMMC;

with HAL;                   use HAL;
with HAL.Block_Drivers;     use HAL.Block_Drivers;
with STM32.GPIO;            use STM32.GPIO;
with STM32.Device;          use STM32.Device;
with STM32;                 use STM32;
with STM32.DMA;             use STM32.DMA;
with STM32.DMA.Interrupts;  use STM32.DMA.Interrupts;
with Ada.Interrupts;
with Ada.Interrupts.Names;
with Monitor.Block_Drivers;
with Semihosting;

package WNM.SDCard is

   procedure Initialize;

   function Mount_Dir return String;

   procedure Mon_Enable;
   procedure Mon_Disable;

private

   type SDCard_Controller
     (Device : not null access STM32.SDMMC.SDMMC_Controller) is
   limited new HAL.Block_Drivers.Block_Driver with record
      Card_Detected : Boolean := False;
   end record;

   Device_Error : exception;

   procedure Initialize
     (This : in out SDCard_Controller);
   --  Initilizes the Controller's pins

   function Get_Card_Information
     (This : in out SDCard_Controller)
      return HAL.SDMMC.Card_Information;
   --  Retrieves the card informations

   function Block_Size
     (This : in out SDCard_Controller)
     return UInt32;
   --  The insterted card block size. 512 Bytes for sd-cards

   overriding function Read
     (This         : in out SDCard_Controller;
      Block_Number : UInt64;
      Data         : out Block) return Boolean
     with Pre => Data'Length <= 16#10000#;
   --  Reads Data.
   --  Data size needs to be a multiple of the card's block size and maximum
   --  length is 2**16

   overriding function Write
     (This         : in out SDCard_Controller;
      Block_Number : UInt64;
      Data         : Block) return Boolean
     with Pre => Data'Length <= 16#10000#;
   --  Writes Data.
   --  Data size needs to be a multiple of the card's block size and maximum
   --  length is 2**16

   SD_Pins    : constant GPIO_Points := (PC8, PC9, PC10, PC11, PC12, PD2);
   SD_Pins_AF : constant GPIO_Alternate_Function := GPIO_AF_SDIO_12;

   SD_DMA            : DMA_Controller       renames DMA_2;
   SD_DMA_RX_Stream  : DMA_Stream_Selector  renames Stream_3;
   SD_DMA_RX_Channel : DMA_Channel_Selector renames Channel_4;
   SD_DMA_TX_Stream  : DMA_Stream_Selector  renames Stream_6;
   SD_DMA_TX_Channel : DMA_Channel_Selector renames Channel_4;

   SD_Interrupt : Ada.Interrupts.Interrupt_ID renames
     Ada.Interrupts.Names.SDIO_Interrupt;

   SD_Rx_DMA_Int     : DMA_Interrupt_Controller renames DMA2_Stream3;
   SD_Tx_DMA_Int     : DMA_Interrupt_Controller renames DMA2_Stream6;

   SDCard_Device : aliased SDCard.SDCard_Controller (SDIO'Access);
   BD_Monitor    : aliased Monitor.Block_Drivers.Block_Driver_Monitor
     (Driver_Under_Monitoring => SDCard_Device'Access,
      Put_Line                => Semihosting.Log_Line'Access);
end WNM.SDCard;

