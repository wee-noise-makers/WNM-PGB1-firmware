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

--  Volume update requires I2C communication with the DAC. Doing this from the
--  UI would introduce too much lag in a task that must be fast.

package WNM.Master_Volume is

   subtype Volume_Value is Natural range 0 .. 100;

   procedure Update;
   --  Send the current volume value to the DAC. This is meant to be called in
   --  a low priority task,

   procedure Set (Vol : Volume_Value);
   --  Set the desired volume value. The value will not be update until the
   --  Update sub-program is executed.
   --  Set_Volume is not blocking and can be called in an high priority task.

   procedure Change (Delta_Vol : Integer);
   --  Change the desired volume value. The value will not be update until the
   --  Update sub-program is executed.
   --  Change_Volume is not blocking and can be called in an high priority
   --  task.

   function Value return Volume_Value;

end WNM.Master_Volume;
