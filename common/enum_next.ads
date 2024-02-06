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

with WNM_HAL;

generic
   type T is (<>);
   Wrap : Boolean := True;
package Enum_Next is

   type Limits is record
      Min : T := T'First;
      Max : T := T'Last;
   end record;

   Default_Limits : constant Limits := (others => <>);

   function Next (Elt : T; Lim : Limits := Default_Limits) return T
     with Inline_Always;
   --  Return the next value of type T within limits, wraps to the Min value
   --  if Wrap is True.

   function Prev (Elt : T; Lim : Limits := Default_Limits) return T
     with Inline_Always;
   --  Return the previous value of type T within limits, wraps to the Max
   --  value within limits type if Wrap is True.

   procedure Next (Elt : in out T; Lim : Limits := Default_Limits)
     with Inline_Always;
   procedure Prev (Elt : in out T; Lim : Limits := Default_Limits)
     with Inline_Always;

   function Next_Fast (Elt : T; Lim : Limits := Default_Limits) return T
     with Inline_Always;
   --  Same as Next but jumping over 10 values

   function Prev_Fast (Elt : T; Lim : Limits := Default_Limits) return T
     with Inline_Always;
   --  Same as Prev but jumping over 10 values

   procedure Next_Fast (Elt : in out T; Lim : Limits := Default_Limits)
     with Inline_Always;
   procedure Prev_Fast (Elt : in out T; Lim : Limits := Default_Limits)
     with Inline_Always;

   procedure Set (Elt : in out T; V : T; Lim : Limits := Default_Limits);
   procedure Set (Elt : in out T;
                  V   : WNM_HAL.Touch_Value;
                  Lim : Limits := Default_Limits);
end Enum_Next;
