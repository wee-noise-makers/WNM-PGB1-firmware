-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2024-2025 Fabien Chouteau                  --
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

package body WNM.Shared_Buffers is

   -------------------------
   -- Clear_Synth_Buffers --
   -------------------------

   procedure Clear_Synth_Buffers is
      subtype Lead_Range
        is Natural range
          Lead_Synth_Offset .. Lead_Synth_Offset + Lead_Synth_Byte_Size - 1;

      subtype Bass_Range
        is Natural range
          Bass_Synth_Offset .. Bass_Synth_Offset + Bass_Synth_Byte_Size - 1;

      subtype Reverb_Range
        is Natural range
          Reverb_Offset .. Reverb_Offset + Reverb_Byte_Size - 1;
   begin
      Shared_Buffer (Lead_Range) := (others => 0);
      Shared_Buffer (Bass_Range) := (others => 0);
      Shared_Buffer (Reverb_Range) := (others => 0);
   end Clear_Synth_Buffers;

end WNM.Shared_Buffers;
