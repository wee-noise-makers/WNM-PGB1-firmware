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

package body Enum_Next is

   ----------
   -- Next --
   ----------

   function Next (Elt : T) return T is
   begin
      if Elt = T'Last then
         if Wrap then
            return T'First;
         else
            return Elt;
         end if;
      else
         return T'Succ (Elt);
      end if;
   end Next;

   ----------
   -- Prev --
   ----------

   function Prev (Elt : T) return T is
   begin
      if Elt = T'First then
         if Wrap then
            return T'Last;
         else
            return Elt;
         end if;
      else
         return T'Pred (Elt);
      end if;
   end Prev;

   ----------
   -- Next --
   ----------

   procedure Next (Elt : in out T) is
   begin
      Elt := Next (Elt);
   end Next;

   ----------
   -- Prev --
   ----------

   procedure Prev (Elt : in out T) is
   begin
      Elt := Prev (Elt);
   end Prev;

   ---------------
   -- Next_Fast --
   ---------------

   function Next_Fast (Elt : T) return T is
      Result : T := Elt;
   begin
      --  Naive implementation using a loop, not sure if we can do better
      for Cnt in 1 .. 10 loop
         Next (Result);
      end loop;
      return Result;
   end Next_Fast;

   ---------------
   -- Prev_Fast --
   ---------------

   function Prev_Fast (Elt : T) return T is
      Result : T := Elt;
   begin
      --  Naive implementation using a loop, not sure if we can do better
      for Cnt in 1 .. 10 loop
         Prev (Result);
      end loop;
      return Result;
   end Prev_Fast;

   ---------------
   -- Next_Fast --
   ---------------

   procedure Next_Fast (Elt : in out T) is
   begin
      Elt := Next_Fast (Elt);
   end Next_Fast;

   ---------------
   -- Prev_Fast --
   ---------------

   procedure Prev_Fast (Elt : in out T) is
   begin
      Elt := Prev_Fast (Elt);
   end Prev_Fast;
end Enum_Next;
