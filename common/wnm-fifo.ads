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

generic
   type Element is private;
   Empty_Value : Element;
package WNM.FIFO is

   type FIFO (Capacity : Positive) is private;

   procedure Push (This   : in out FIFO;
                   Buffer :        Element)
     with Pre => not Full (This);

   procedure Pop (This   : in out FIFO;
                  Buffer :    out Element);

   function Full (This : FIFO) return Boolean;

   function Empty (This : FIFO) return Boolean;
private

   type Buffer_Array is array (Positive range <>) of Element;

   type FIFO (Capacity : Positive) is record
      Data     : Buffer_Array (1 .. Capacity) := (others => Empty_Value);
      Next_In  : Positive := 1;
      Next_Out : Positive := 1;
      Cnt      : Natural  := 0;
   end record;

end WNM.FIFO;
