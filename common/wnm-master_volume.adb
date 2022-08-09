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

with WNM.Audio_DAC; use WNM.Audio_DAC;

package body WNM.Master_Volume is

   Desired_Volume : Volume_Value := 40;

   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      WNM.Audio_DAC.Set_Volume (DAC_Volume (Desired_Volume));
   end Update;

   ---------
   -- Set --
   ---------

   procedure Set (Vol : Volume_Value) is
   begin
      Desired_Volume := Vol;
   end Set;

   ------------
   -- Change --
   ------------

   procedure Change (Delta_Vol : Integer) is
      Tmp : Integer;
   begin

      Tmp := Integer (Desired_Volume) + Delta_Vol;

      if Tmp in Volume_Value then
         Desired_Volume := Tmp;
      end if;
   end Change;

   -----------
   -- Value --
   -----------

   function Value return Volume_Value
   is (Desired_Volume);

end WNM.Master_Volume;
