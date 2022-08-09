-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with RP.Device;
with RP.Timer;

with WNM.RP2040;
pragma Elaborate (WNM.RP2040);

package body WNM.Time is


   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      RP.Device.Timer.Enable;
   end Initialize;

   -----------
   -- Clock --
   -----------

   function Clock return Time_Microseconds
   is (Time_Microseconds (RP.Timer.Clock));


   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   procedure Delay_Milliseconds (Milliseconds : UInt64) is
   begin
      RP.Device.Timer.Delay_Milliseconds (Integer (Milliseconds));
   end Delay_Milliseconds;

   ------------------------
   -- Delay_MicroSeconds --
   ------------------------

   procedure Delay_Microseconds (Microseconds : Time_Microseconds) is
   begin
      RP.Device.Timer.Delay_Microseconds (Integer (Microseconds));
   end Delay_Microseconds;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Wakeup_Time : Time_Microseconds) is
   begin
      RP.Device.Timer.Delay_Until (RP.Timer.Time (Wakeup_Time));
   end Delay_Until;

begin
   Initialize;
end WNM.Time;
