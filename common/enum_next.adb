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

   -----------
   -- Clamp --
   -----------

   function Clamp (Elt : T; Lim : Limits) return T is
   begin
      return T'Min (T'Max (Elt, Lim.Min), Lim.Max);
   end Clamp;

   ----------
   -- Next --
   ----------

   function Next (Elt : T; Lim : Limits := Default_Limits) return T is
      Res : T;
   begin
      if Elt = T'Last then
         if Wrap then
            Res := T'First;
         else
            Res := Elt;
         end if;
      else
         Res := T'Succ (Elt);
      end if;

      return Clamp (Res, Lim);
   end Next;

   ----------
   -- Prev --
   ----------

   function Prev (Elt : T; Lim : Limits := Default_Limits) return T is
      Res : T;
   begin
      if Elt = T'First then
         if Wrap then
            Res := T'Last;
         else
            Res := Elt;
         end if;
      else
         Res := T'Pred (Elt);
      end if;

      return Clamp (Res, Lim);
   end Prev;

   ----------
   -- Next --
   ----------

   procedure Next (Elt : in out T; Lim : Limits := Default_Limits) is
   begin
      Elt := Next (Elt, Lim);
   end Next;

   ----------
   -- Prev --
   ----------

   procedure Prev (Elt : in out T; Lim : Limits := Default_Limits) is
   begin
      Elt := Prev (Elt, Lim);
   end Prev;

   ---------------
   -- Next_Fast --
   ---------------

   function Next_Fast (Elt : T; Lim : Limits := Default_Limits) return T is
      Result : T := Elt;
   begin
      --  Naive implementation using a loop, not sure if we can do better
      for Cnt in 1 .. 10 loop
         Next (Result, Lim);
      end loop;
      return Result;
   end Next_Fast;

   ---------------
   -- Prev_Fast --
   ---------------

   function Prev_Fast (Elt : T; Lim : Limits := Default_Limits) return T is
      Result : T := Elt;
   begin
      --  Naive implementation using a loop, not sure if we can do better
      for Cnt in 1 .. 10 loop
         Prev (Result, Lim);
      end loop;
      return Result;
   end Prev_Fast;

   ---------------
   -- Next_Fast --
   ---------------

   procedure Next_Fast (Elt : in out T; Lim : Limits := Default_Limits) is
   begin
      Elt := Next_Fast (Elt, Lim);
   end Next_Fast;

   ---------------
   -- Prev_Fast --
   ---------------

   procedure Prev_Fast (Elt : in out T; Lim : Limits := Default_Limits) is
   begin
      Elt := Prev_Fast (Elt, Lim);
   end Prev_Fast;
end Enum_Next;
