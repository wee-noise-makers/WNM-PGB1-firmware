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

package body WNM.Voices.Reverb_Voice is

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Instance;
                     Left   : in out Tresses.Mono_Buffer;
                     Right  : in out Tresses.Mono_Buffer)
   is
   begin
      Reverb_Pck.Set_Amount (This.Rev, This.Params (P_Amount));
      Reverb_Pck.Set_Time (This.Rev, This.Params (P_Time));
      Reverb_Pck.Set_Diffusion (This.Rev, This.Params (P_Diffusion));
      Reverb_Pck.Set_Low_Pass (This.Rev, This.Params (P_Cutoff));
      Reverb_Pck.Process (This.Rev, Left, Right);
   end Render;

end WNM.Voices.Reverb_Voice;
