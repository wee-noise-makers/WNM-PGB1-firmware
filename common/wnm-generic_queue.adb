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

with BBqueue; use BBqueue;
with System.Storage_Elements; use System.Storage_Elements;

package body WNM.Generic_Queue is

   ----------
   -- Push --
   ----------

   procedure Push (Q : in out Instance;
                   E : Element)
   is
      WG : Write_Grant;
   begin
      Grant (Q.Queue, WG, 1);

      if State (WG) = Valid then
         Q.Data (Q.Data'First + Slice (WG).From) := E;

         Commit (Q.Queue, WG, 1);
      else
         raise Program_Error
           with Name & " queue error: write grant failed ( " &
           State (WG)'Img & ")";
      end if;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Q       : in out Instance;
                  E       : out Element;
                  Success : out Boolean)
   is
      RG : Read_Grant;
   begin
      Read (Q.Queue, RG, 1);

      if State (RG) = Valid then
         E := Q.Data (Q.Data'First + Slice (RG).From);
         Release (Q.Queue, RG, 1);
         Success := True;
      else
         Success := False;
      end if;
   end Pop;

   ------------
   -- Pop_CB --
   ------------

   procedure Pop_CB (Q : in out Instance) is
      RG : Read_Grant;
   begin
      Read (Q.Queue, RG);

      if State (RG) = Valid then
         declare
            From : constant Buffer_Offset :=
              Q.Data'First + Slice (RG).From;
            To : constant Buffer_Offset :=
              Q.Data'First + Slice (RG).To;
         begin

            for Elt of Q.Data (From .. To) loop
               Process (Elt);
            end loop;
         end;
      end if;
   end Pop_CB;

end WNM.Generic_Queue;
