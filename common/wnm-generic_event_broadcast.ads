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

generic
package WNM.Generic_Event_Broadcast is

   type Callback is access procedure;

   generic
      CB : not null Callback;
      Priority : Natural := 0; -- Higher priority will be called first
   package Register is
   end Register;

   procedure Broadcast;

private

   type Listener (CB : not null Callback; Priority : Natural);
   type Listener_Access is access all Listener;

   procedure Register_Listener (Acc : not null Listener_Access);

   type Listener (CB : not null Callback; Priority : Natural) is record
      Next : Listener_Access := null;
   end record;

   Head : Listener_Access := null;

end WNM.Generic_Event_Broadcast;
