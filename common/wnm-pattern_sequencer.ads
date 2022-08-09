-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2017 Fabien Chouteau                  --
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

with WNM.Time;
with WNM.UI;

package WNM.Pattern_Sequencer is

   procedure Start_Recording;
   procedure End_Recording;

   procedure Play_Pause;
   function Playing return Boolean;

   procedure On_Press (Button : Keyboard_Button;
                       Mode : WNM.UI.Main_Modes);

   procedure On_Release (Button : Keyboard_Button;
                         Mode : WNM.UI.Main_Modes);

   procedure Single_Play (P : Patterns);

   procedure Add_To_Sequence (Pattern : Patterns);

   function Playing_Pattern return Patterns;
   function Is_In_Pattern_Sequence (Pattern : Patterns) return Boolean;

   procedure Signal_End_Of_Pattern;
   procedure Signal_Mid_Pattern;

end WNM.Pattern_Sequencer;
