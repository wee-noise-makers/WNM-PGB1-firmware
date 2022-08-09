-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2017 Fabien Chouteau                    --
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

with HAL;             use HAL;
with Managed_Buffers; use Managed_Buffers;

package WNM.Buffer_Allocation is

   type Memory_Kind is (RAM, CCM, Any);

   function Allocate (Kind : Memory_Kind;
                      Size : Natural)
                      return Any_Managed_Buffer;

   procedure Release_Buffer (Buf : Any_Managed_Buffer);

private

   type Data_Array is array (Natural range <>) of UInt8
     with Alignment => 2;

   type RAM_Managed_Buffer (Capacity : Natural) is new Managed_Buffer with record
      Data      : Data_Array (1 .. Capacity);
   end record;

   type RAM_Managed_Buffer_Access is access all RAM_Managed_Buffer;

   overriding
   function Buffer_Address (This : RAM_Managed_Buffer)
                            return System.Address
   is (This.Data'Address);

   overriding
   function Buffer_Length (This : RAM_Managed_Buffer)
                           return UInt64
   is (This.Data'Length);

   type CCM_Managed_Buffer (Capacity : Natural) is new Managed_Buffer with record
      Data      : Data_Array (1 .. Capacity);
   end record;

   type CCM_Managed_Buffer_Access is access all CCM_Managed_Buffer;

   overriding
   function Buffer_Address (This : CCM_Managed_Buffer)
                            return System.Address
   is (This.Data'Address);

   overriding
   function Buffer_Length (This : CCM_Managed_Buffer)
                           return UInt64
   is (This.Data'Length);

end WNM.Buffer_Allocation;
