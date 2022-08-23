-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2022 Fabien Chouteau                    --
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

package body WNM.Coproc is

   ----------
   -- Push --
   ----------

   procedure Push (Msg : Message) is
   begin
      WNM_HAL.Push (To_Coproc_Data (Msg));
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Msg : out Message; Success : out Boolean) is
      D : WNM_HAL.Coproc_Data;
   begin
      WNM_HAL.Pop (D, Success);

      if Success then
         Msg := From_Coproc_Data (D);
      end if;
   end Pop;

end WNM.Coproc;
