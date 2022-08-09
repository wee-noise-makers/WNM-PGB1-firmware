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

with System;
with HAL; use HAL;

package Managed_Buffers is

   type Managed_Buffer is abstract tagged limited private;
   type Any_Managed_Buffer is access all Managed_Buffer'Class;

   procedure Take (This : in out Managed_Buffer);
   procedure Release (This : in out Managed_Buffer);
   function Count (This : in out Managed_Buffer) return Natural;
   function Buffer_Address (This : Managed_Buffer) return System.Address is abstract;
   function Buffer_Length (This : Managed_Buffer) return UInt64 is abstract;

private
   type Managed_Buffer is abstract tagged limited record
      Ref_Count : Natural := 1;
   end record;
end Managed_Buffers;
