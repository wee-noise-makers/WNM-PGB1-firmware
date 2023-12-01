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

with Tresses.FX.Bitcrusher; use Tresses.FX.Bitcrusher;

package body WNM.Voices.Bitcrusher_Voice is

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Instance;
                     Left   : in out Tresses.Mono_Buffer;
                     Right  : in out Tresses.Mono_Buffer)
   is

   begin
      Process (This.BTL, Left,
               Param_To_Depth (This.Params (P_Depth)),
               Param_To_Downsampling (This.Params (P_Down)),
               Amount => This.Params (P_Mix),
               Cutoff => This.Params (P_Cutoff));

      Process (This.BTR, Right,
               Param_To_Depth (This.Params (P_Depth)),
               Param_To_Downsampling (This.Params (P_Down)),
               Amount => This.Params (P_Mix),
               Cutoff => This.Params (P_Cutoff));
   end Render;
end WNM.Voices.Bitcrusher_Voice;
