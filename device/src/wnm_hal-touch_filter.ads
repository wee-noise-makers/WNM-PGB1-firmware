-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2024 Fabien Chouteau                    --
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

with RP.GPIO;
with RP.PIO;
with RP.PIO.Touch_Sense;

package WNM_HAL.Touch_Filter is

   Data_Count : constant := 5;
   type Data_Index is mod Data_Count;
   type Data_Array is array (Data_Index) of HAL.UInt32;

   type Filtered_Touch_Sensor
     (Pin   : not null access RP.GPIO.GPIO_Point;
      PIO   : not null access RP.PIO.PIO_Device;
      SM    : RP.PIO.PIO_SM)
   is new RP.PIO.Touch_Sense.Touch_Sensor (Pin, PIO, SM)
   with record
      Values  : Data_Array := (others => 0);
      Next_In : Data_Index := Data_Index'First;
   end record;

   procedure Trigger_Measures (This : in out Filtered_Touch_Sensor);
   procedure Process_Measures (This : in out Filtered_Touch_Sensor);

   function Read (This : in out Filtered_Touch_Sensor) return HAL.UInt32;

   type Scale is record
      Min : Integer := Integer'Last;
      Max : Integer := Integer'First;

      Min_F : Float := Float'Last;
      Max_F : Float := Float'First;
   end record;

   function Process (This   : in out Scale;
                     P1, P2 :        Integer)
                     return Touch_Value;
end WNM_HAL.Touch_Filter;
