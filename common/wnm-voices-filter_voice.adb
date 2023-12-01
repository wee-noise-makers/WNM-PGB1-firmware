-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2023 Fabien Chouteau                    --
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

with Tresses.Filters.SVF; use Tresses.Filters.SVF;

package body WNM.Voices.Filter_Voice is

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Instance;
                     Left   : in out Tresses.Mono_Buffer;
                     Right  : in out Tresses.Mono_Buffer)
   is
   begin
      Set_Frequency (This.Left, This.Params (P_Cutoff));
      Set_Frequency (This.Right, This.Params (P_Cutoff));

      Set_Resonance (This.Left, This.Params (P_Resonance));
      Set_Resonance (This.Right, This.Params (P_Resonance));

      declare
         Third : constant Tresses.Param_Range := Tresses.Param_Range'Last / 3;
         Filter_Mode : constant Mode_Kind :=
           (case This.Params (P_Mode) is
               when 0 .. Third => Low_Pass,
               when Third + 1 .. 2 * Third => Band_Pass,
               when others => High_Pass);
      begin
         Tresses.Filters.SVF.Set_Mode (This.Left, Filter_Mode);
         Tresses.Filters.SVF.Set_Mode (This.Right, Filter_Mode);
      end;

      for Elt of Left loop
         Elt := S16 (Tresses.Filters.SVF.Process (This.Left, S32 (Elt)));
      end loop;

      for Elt of Right loop
         Elt := S16 (Tresses.Filters.SVF.Process (This.Right, S32 (Elt)));
      end loop;

   end Render;

end WNM.Voices.Filter_Voice;
