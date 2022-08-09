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

with WNM.Buffer_FIFO; use WNM.Buffer_FIFO;

package body WNM.Buffer_Allocation is

   Buffer_Size : constant := Mono_Buffer_Size_In_Bytes;
   Number_Of_RAM_Buffers : constant := 60;
   Number_Of_CCM_Buffers : constant := 1;

   RAM_Buffers : array (1 .. Number_Of_RAM_Buffers) of
     aliased RAM_Managed_Buffer (Buffer_Size);
   CCM_Buffers : array (1 .. Number_Of_CCM_Buffers) of
     aliased RAM_Managed_Buffer (Buffer_Size)
   with Linker_Section => ".ccmdata";

   Free_RAM_Buffer_FIFO : FIFO (Number_Of_RAM_Buffers);

   Free_CCM_Buffer_FIFO : FIFO (Number_Of_CCM_Buffers);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Kind : Memory_Kind;
      Size : Natural)
      return Any_Managed_Buffer
   is
      Buffer : Any_Managed_Buffer;
   begin
      if Size /= Buffer_Size then
         raise Program_Error with "Not implemented...";
      end if;

      case Kind is
         when RAM =>
            Pop (Free_RAM_Buffer_FIFO, Buffer);
         when CCM | Any =>
            Pop (Free_CCM_Buffer_FIFO, Buffer);
      end case;
      return Buffer;
   end Allocate;

   --------------------
   -- Release_Buffer --
   --------------------

   procedure Release_Buffer (Buf : Any_Managed_Buffer) is
   begin

      if Buf.Count = 1 then
         if Buf.all in RAM_Managed_Buffer then
            Push (Free_RAM_Buffer_FIFO, Buf);
         elsif Buf.all in CCM_Managed_Buffer then
            Push (Free_CCM_Buffer_FIFO, Buf);
         else
            raise Program_Error with "WTF?!?";
         end if;
      else
         Buf.Release;
      end if;
   end Release_Buffer;

begin
   for Index in RAM_Buffers'Range loop
      Push (Free_RAM_Buffer_FIFO, RAM_Buffers (Index)'Access);
   end loop;
   for Index in CCM_Buffers'Range loop
      Push (Free_CCM_Buffer_FIFO, CCM_Buffers (Index)'Access);
   end loop;
end WNM.Buffer_Allocation;
