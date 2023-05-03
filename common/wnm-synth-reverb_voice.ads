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

with Tresses;            use Tresses;
with Tresses.Interfaces; use Tresses.Interfaces;

private with Tresses.FX.Reverb;

private package WNM.Synth.Reverb_Voice is

   type Instance
   is new Four_Params_Voice
   with private;

   procedure Render (This   : in out Instance;
                     Left   : in out Tresses.Mono_Buffer;
                     Right  : in out Tresses.Mono_Buffer);

   P_Amount    : constant Tresses.Param_Id := 1;
   P_Time      : constant Tresses.Param_Id := 2;
   P_Diffusion : constant Tresses.Param_Id := 3;
   P_Cutoff    : constant Tresses.Param_Id := 4;

   --  Interfaces --

   overriding
   function Param_Label (This : Instance; Id : Param_Id) return String
   is (case Id is
          when P_Amount    => "Amount",
          when P_Time      => "Time",
          when P_Diffusion => "Diffusion",
          when P_Cutoff    => "LP Cutoff");

   overriding
   function Param_Short_Label (This : Instance; Id : Param_Id)
                               return Short_Label
   is (case Id is
          when P_Amount    => "AMT",
          when P_Time      => "TIM",
          when P_Diffusion => "DIF",
          when P_Cutoff    => "CTF");

private

   package Reverb_Pck is new Tresses.FX.Reverb;

   type Instance
   is new Four_Params_Voice
   with record
      Rev : Reverb_Pck.Instance;
   end record;

end WNM.Synth.Reverb_Voice;
