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

with STM32.Power_Control;
with WNM.Screen;
with WNM.Audio_DAC;

package body WNM.Power_Control is

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down is
   begin
      WNM.Screen.Power_Down;
      WNM.Audio_DAC.Power_Down;

      STM32.Power_Control.Enable_Wakeup_Pin;
      STM32.Power_Control.Enter_Standby_Mode;
   end Power_Down;

end WNM.Power_Control;
