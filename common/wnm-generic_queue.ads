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

with BBqueue;

generic
   type Element is private;
   Name : String;
package WNM.Generic_Queue is

   type Instance (Capacity : BBqueue.Buffer_Size)
   is limited
   private;

   type Element_Array is array (BBqueue.Buffer_Offset range <>) of Element;

   procedure Push (Q : in out Instance;
                   E : Element);

   procedure Pop (Q       : in out Instance;
                  E       : out Element;
                  Success : out Boolean);

   generic
      with procedure Process (E : Element);
   procedure Pop_CB (Q : in out Instance);

private
   type Instance (Capacity : BBqueue.Buffer_Size)
   is limited
           record
              Queue : BBqueue.Offsets_Only (Capacity);
              Data  : Element_Array (1 .. Capacity);
           end record;

end WNM.Generic_Queue;
