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

package body WNM.Generic_Event_Broadcast is

   -----------------------
   -- Register_Listener --
   -----------------------

   procedure Register_Listener (Acc : not null Listener_Access)
   is
      Ptr : Listener_Access := Head;
   begin

      --  Check if node is already in the list
      while Ptr /= null loop
         if Ptr = Acc then
            raise Program_Error;
         end if;

         Ptr := Ptr.Next;
      end loop;

      if Head = null or else Head.Priority < Acc.Priority then
         --  Head insert
         Acc.Next := Head;
         Head := Acc;
      else
         Ptr := Head;
         loop
            if Ptr.Next = null or else Ptr.Next.Priority < Acc.Priority then
               Acc.Next := Ptr.Next;
               Ptr.Next := Acc;
               return;
            end if;

            Ptr := Ptr.Next;
         end loop;
      end if;
   end Register_Listener;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast is
      Ptr : Listener_Access := Head;
   begin
      while Ptr /= null loop
         Ptr.CB.all;
         Ptr := Ptr.Next;
      end loop;
   end Broadcast;

   --------------
   -- Register --
   --------------

   package body Register is
      Lis : aliased Listener (CB, Priority);
   begin
      Register_Listener (Lis'Access);
   end Register;

end WNM.Generic_Event_Broadcast;
