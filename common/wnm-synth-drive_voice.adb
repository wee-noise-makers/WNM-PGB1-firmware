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

with Interfaces; use Interfaces;

with Tresses.FX.Overdrive;
with Tresses.DSP;

package body WNM.Synth.Drive_Voice is

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Instance;
                     Left   : in out Tresses.Mono_Buffer;
                     Right  : in out Tresses.Mono_Buffer)
   is
      Gain  : constant Param_Range := This.Params (P_Gain);
      Drive : constant Param_Range := This.Params (P_Drive);
      Pan   : constant Param_Range := This.Params (P_Pan);

      --  Drive ratio depending on the pan setting

      L_Ratio : constant Param_Range :=
        (if Pan < (Param_Range'Last / 2)
         then Pan * 2
         else Param_Range'Last);

      R_Ratio : constant Param_Range :=
        (if Pan > (Param_Range'Last / 2)
         then (Param_Range'Last - Pan) * 2
         else Param_Range'Last);

      L_Drive : constant Param_Range :=
        Param_Range ((S32 (Drive) * S32 (L_Ratio)) / 2**15);

      R_Drive : constant Param_Range :=
        Param_Range ((S32 (Drive) * S32 (R_Ratio)) / 2**15);

      Level   : constant Param_Range := This.Params (4);

      --  Level is inversely proportional to drive, this is to compensate the
      --  gain introduced by the overdrive.

      L_Level : constant Param_Range :=
        Param_Range
          (DSP.Clip_S16 ((S32 (Level) + S32 (Param_Range'Last - L_Ratio))));

      R_Level : constant Param_Range :=
        Param_Range
          (DSP.Clip_S16 ((S32 (Level) + S32 (Param_Range'Last - R_Ratio))));

   begin
      Tresses.FX.Overdrive.Process (Left, Gain, L_Drive, L_Level);
      Tresses.FX.Overdrive.Process (Right, Gain, R_Drive, R_Level);
   end Render;
end WNM.Synth.Drive_Voice;
