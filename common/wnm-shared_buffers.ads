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

   Shared_Buffer_Byte_Size : constant := 131_100;
   --  As small as possbile, but big enough to fit either the QOA sample or
   --  the reverb buffer.

   Shared_Butffer_Bit_Size : constant := Shared_Buffer_Byte_Size * 8;
   Shared_Buffer : HAL.UInt8_Array (1 .. Shared_Buffer_Byte_Size)
     with Alignment => 8;

   Lead_Synth_Offset : constant := 1;
   Lead_Synth_Byte_Size : constant := 8_400;

   Bass_Synth_Offset : constant := Lead_Synth_Offset + Lead_Synth_Byte_Size;
   Bass_Synth_Byte_Size : constant := 8_400;

   Reverb_Offset : constant := Bass_Synth_Offset + Bass_Synth_Byte_Size;
   Reverb_Byte_Size : constant := 33_000;

   Short_Term_Offset : constant := Reverb_Offset + Reverb_Byte_Size;
   Short_Term_Byte_Size : constant := 20_500;

   Short_Term_Alloc_Offset : constant :=
     Short_Term_Offset + Short_Term_Byte_Size;
   Short_Term_Alloc_Byte_Size : constant := 2_600;

   Top : constant := Short_Term_Alloc_Offset + Short_Term_Alloc_Byte_Size;

   pragma Compile_Time_Error (Top >= Shared_Buffer_Byte_Size,
                              "Invalid layout");

   procedure Clear_Synth_Buffers;

end WNM.Shared_Buffers;
