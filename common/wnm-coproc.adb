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

   -------------------
   -- Push_To_Synth --
   -------------------

   procedure Push_To_Synth (Msg : Message) is
   begin
      WNM_HAL.Push (Synth_CPU, To_Coproc_Data (Msg));
   end Push_To_Synth;

   -------------------
   -- Pop_For_Synth --
   -------------------

   procedure Pop_For_Synth (Msg : out Message; Success : out Boolean) is
      D : WNM_HAL.Coproc_Data;
   begin
      WNM_HAL.Pop (Synth_CPU, D, Success);

      if Success then
         Msg := From_Coproc_Data (D);
      end if;
   end Pop_For_Synth;

   ------------------
   -- Push_To_Main --
   ------------------

   procedure Push_To_Main (Msg : Message) is
   begin
      WNM_HAL.Push (Main_CPU, To_Coproc_Data (Msg));
   end Push_To_Main;

   ------------------
   -- Pop_For_Main --
   ------------------

   procedure Pop_For_Main (Msg : out Message; Success : out Boolean) is
      D : WNM_HAL.Coproc_Data;
   begin
      WNM_HAL.Pop (Main_CPU, D, Success);

      if Success then
         Msg := From_Coproc_Data (D);
      end if;
   end Pop_For_Main;

end WNM.Coproc;
