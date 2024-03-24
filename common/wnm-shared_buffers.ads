-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2024 Fabien Chouteau                    --
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

with HAL;

package WNM.Shared_Buffers is

   Shared_Buffer_Byte_Size : constant := 45_134;
   --  As small as possbile, but big enough to fit either the QOA sample or
   --  the reverb buffer.

   Shared_Butffer_Bit_Size : constant := Shared_Buffer_Byte_Size * 8;
   Shared_Buffer : HAL.UInt8_Array (1 .. Shared_Buffer_Byte_Size)
     with Alignment => 8;

end WNM.Shared_Buffers;
