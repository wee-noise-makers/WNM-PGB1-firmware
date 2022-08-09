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

package body WNM.FIFO is

   ----------
   -- Push --
   ----------

   procedure Push
     (This   : in out FIFO;
      Buffer :        Element)
   is
   begin
      if Full (This) then
         return;
      end if;
      This.Data (This.Next_In) := Buffer;
      This.Next_In := (This.Next_In mod This.Capacity) + 1;
      This.Cnt := This.Cnt + 1;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop
     (This   : in out FIFO;
      Buffer :    out Element)
   is
   begin
      if Empty (This) then
         Buffer := Empty_Value;
      else
         Buffer := This.Data (This.Next_Out);
         This.Data (This.Next_Out) := Empty_Value;
         This.Next_Out := (This.Next_Out mod This.Capacity) + 1;
         This.Cnt := This.Cnt - 1;
      end if;
   end Pop;

   ----------
   -- Full --
   ----------

   function Full (This : FIFO) return Boolean
   is (This.Cnt = This.Capacity);

   -----------
   -- Empty --
   -----------

   function Empty (This : FIFO) return Boolean
   is (This.Cnt = 0);

end WNM.FIFO;
