-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2024 Fabien Chouteau                    --
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

private with Tresses.Envelopes.AR;

package WNM.Voices.Stutter_FX is

   type Mode_Kind is (Off, On_Short, On_Trip);

   type Instance is private;

   procedure Set_Mode (This   : in out Instance;
                       Mode :        Mode_Kind);

   function Mode (This : Instance) return Mode_Kind;

   procedure Render (This   : in out Instance;
                     Buffer : in out WNM_HAL.Stereo_Buffer);

private

   type Instance is record
      Do_Init : Boolean := True;

      Mute : Boolean := True;
      Mode : Mode_Kind := Off;

      Env : Tresses.Envelopes.AR.Instance;
   end record;

end WNM.Voices.Stutter_FX;
