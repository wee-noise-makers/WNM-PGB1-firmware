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

private with Tresses.Envelopes.AR;

package WNM.Voices.Kick_Voice is

   type Instance
   is new Four_Params_Voice
   with private;

   type Kick_Engine is (Sine_Kick, Triangle_Kick, Chip_Kick);

   function Engine (This : Instance) return Kick_Engine;
   procedure Set_Engine (This : in out Instance; E : Kick_Engine);

   function Img (E : Kick_Engine) return String;

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

      Engine : Kick_Engine := Kick_Engine'First;

      Phase, Phase_Increment, Target_Phase_Increment : U32 := 0;
      Env0, Env1  : Tresses.Envelopes.AR.Instance;
      Do_Init : Boolean := True;
   end record;

end WNM.Voices.Kick_Voice;
