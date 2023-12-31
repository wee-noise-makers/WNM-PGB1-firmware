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

package body WNM.Utils is

   --------------
   -- Copy_Str --
   --------------

   procedure Copy_Str (From :     String;
                       To   : out String;
                       Fill :     Character := ' ')
   is
   begin
      if From'Length = To'Length then
         To := From;
      elsif From'Length > To'Length then
         To := From (From'First .. From'First + To'Length - 1);
      else
         To (To'First .. To'First + From'Length - 1) := From;
         To (To'First + From'Length .. To'Last) := (others => Fill);
      end if;
   end Copy_Str;

   ----------
   -- Trim --
   ----------

   function Trim (Str : String) return String is
      First : Natural := Str'First;
      Last : Natural := Str'Last;
   begin
      while First in Str'Range and then Str (First) = ' ' loop
         First := First + 1;
      end loop;

      while Last in Str'Range and then Str (Last) = ' ' loop
         Last := Last - 1;
      end loop;

      return Str (First .. Last);
   end Trim;
end WNM.Utils;
