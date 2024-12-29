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

package WNM.Utils is

   procedure Copy_Str (From :     String;
                       To   : out String;
                       Fill :     Character := ' ');
   --  Copy string From in To. If the length doesn't match, either trucate
   --  From or fill the remaining characters of To with Fill.

   function Starts_With (Full, Prefix : String) return Boolean is
     (Full'Length >= Prefix'Length
      and then Full (Full'First .. Full'First + Prefix'Length - 1) = Prefix);

   function Ends_With (Full, Suffix : String) return Boolean is
     (Full'Length >= Suffix'Length
      and then Full (Full'Last - Suffix'Length + 1 .. Full'Last) = Suffix);

   function Trim (Str : String) return String;

end WNM.Utils;
