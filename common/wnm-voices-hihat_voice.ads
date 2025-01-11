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
with Tresses.Random;

private with Tresses.Filters.SVF;
private with Tresses.Envelopes.AR;
private with Tresses.Drums.Cymbal;

package WNM.Voices.Hihat_Voice is

   type Instance
   is new Four_Params_Voice
   with private;

   type HH_Engine is (Cymbal, HH909, HH707);

   function Engine (This : Instance) return HH_Engine;
   procedure Set_Engine (This : in out Instance; E : HH_Engine);

   function Img (E : HH_Engine) return String;

   procedure Init (This : in out Instance);

   procedure Render (This   : in out Instance;
                     Buffer :    out Tresses.Mono_Buffer);

   --  Interfaces --

   overriding
   function Param_Label (This : Instance; Id : Param_Id) return String;

   overriding
   function Param_Short_Label (This : Instance; Id : Param_Id)
                               return Short_Label;

private

   type Instance
   is new Four_Params_Voice
   with record

      Engine : HH_Engine := HH_Engine'First;

      Phase : U32 := 0;

      Filter0, Filter1 : Filters.SVF.Instance;
      Env0 : Tresses.Envelopes.AR.Instance;
      Rng : Tresses.Random.Instance;
      Cym_State : Tresses.Drums.Cymbal.Cymbal_State;
      Do_Init : Boolean := True;
   end record;

end WNM.Voices.Hihat_Voice;
