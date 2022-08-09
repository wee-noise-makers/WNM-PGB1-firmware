-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2017 Fabien Chouteau                    --
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

package body Managed_Buffers is

   ----------
   -- Take --
   ----------

   procedure Take (This : in out Managed_Buffer) is
   begin
      This.Ref_Count := This.Ref_Count + 1;
   end Take;

   -------------
   -- Release --
   -------------

   procedure Release (This : in out Managed_Buffer) is
   begin
      This.Ref_Count := This.Ref_Count - 1;
   end Release;

   -----------
   -- Count --
   -----------

   function Count (This : in out Managed_Buffer) return Natural is
   begin
      return This.Ref_Count;
   end Count;

end Managed_Buffers;
