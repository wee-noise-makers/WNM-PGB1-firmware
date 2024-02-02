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

package body WNM.Step_Event_Broadcast is

   --------------
   -- Register --
   --------------

   procedure Register (Acc : not null Listener_Acess) is
      Ptr : Listener_Acess := Head;
   begin

      --  Check if node is already in the list
      while Ptr /= null loop
         if Ptr = Acc then
            raise Program_Error;
         end if;

         Ptr := Ptr.Next;
      end loop;

      Acc.Next := Head;
      Head := Acc;
   end Register;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast is
      Ptr : Listener_Acess := Head;
   begin

      --  Check if node is already in the list
      while Ptr /= null loop
         Ptr.CB.all;
         Ptr := Ptr.Next;
      end loop;
   end Broadcast;

end WNM.Step_Event_Broadcast;
