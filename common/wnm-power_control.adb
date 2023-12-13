-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2022 Fabien Chouteau                  --
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

with WNM.Persistent;
with WNM_HAL;
with WNM.Project.Library;

package body WNM.Power_Control is

   --------------------------
   -- Save_Before_Shutdown --
   --------------------------

   procedure Save_Before_Shutdown is
   begin
      if WNM_HAL.Get_LFS_Config /= null then
         Persistent.Save;
         WNM.Project.Library.Try_Save_For_Shutdown;
      end if;
   end Save_Before_Shutdown;

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down is
   begin
      Save_Before_Shutdown;
      WNM_HAL.Power_Down;
   end Power_Down;

   --------------------
   -- Enter_DFU_Mode --
   --------------------

   procedure Enter_DFU_Mode is
   begin
      Save_Before_Shutdown;
      WNM_HAL.Enter_DFU_Mode;
   end Enter_DFU_Mode;

end WNM.Power_Control;
