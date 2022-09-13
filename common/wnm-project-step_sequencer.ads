-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2022 Fabien Chouteau                    --
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

with WNM.UI;

package WNM.Project.Step_Sequencer is

   function Playing_Step return Sequencer_Steps;

   function Playing_Pattern return Patterns;

   procedure Play_Pause;
   --  Use it to signal a play/pause event

   procedure On_Press (Button : Keyboard_Button;
                       Mode : WNM.UI.Main_Modes);

   procedure On_Release (Button : Keyboard_Button;
                         Mode : WNM.UI.Main_Modes);

   procedure Execute_Step;

   function Update return Time.Time_Microseconds;

private

   Current_Playing_Step : Sequencer_Steps := Sequencer_Steps'First with Atomic;

   --  Current_Seq_State : Sequencer_State := Pause with Atomic;
   Current_Track     : Tracks := Tracks'First with Atomic;

end WNM.Project.Step_Sequencer;
