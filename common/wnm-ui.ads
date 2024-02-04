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

with WNM.Sequence_Copy;

package WNM.UI is

   type Button_Event is (On_Press,
                         On_Long_Press,
                         On_Release,
                         Waiting_For_Long_Press);

   procedure Update;

   procedure Update_LEDs;

   type Input_Mode_Type is (Pattern_Mode,
                            Track_Mode,
                            Step_Mode,
                            Chord_Mode,
                            FX_Mode,

                            Pattern_Select,
                            Track_Select,
                            Step_Select,
                            Chord_Select,

                            Volume_BPM_Mute,
                            Volume_BPM_Solo,
                            FX_Alt,
                            Copy
                           );

   subtype Main_Modes is Input_Mode_Type range Pattern_Mode .. FX_Mode;
   function Input_Mode return Input_Mode_Type;
   function Input_GUI_Mode return Input_Mode_Type;

   function Recording return Boolean;

   function FX_On (B : Keyboard_Button) return Boolean;

   function Muted (Track : WNM.Tracks) return Boolean;

   Copy_T : WNM.Sequence_Copy.Copy_Transaction;

private

   function Has_Long_Press (B : Button) return Boolean;
   --  Can this button trigger a On_Long_Press event?

   function Has_Repeat_Press (B : Button) return Boolean;
   --  Can this button re-trigger a On_Press event when held down?

end WNM.UI;
