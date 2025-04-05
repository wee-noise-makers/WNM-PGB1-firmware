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

package WNM.UI is

   type Button_Event is (On_Press,
                         On_Long_Press,
                         On_Release,
                         Waiting_For_Long_Press);

   procedure Update;

   procedure Update_LEDs;

   type Input_Mode_Type is (Track_Settings_Mode,
                            Chord_Mode,
                            Lead_Mode,

                            Track_Select,

                            Volume_BPM_Mute,
                            FX_Alt
                           );

   subtype Main_Modes
     is Input_Mode_Type range Track_Settings_Mode .. Lead_Mode;

   function Input_Mode return Input_Mode_Type;
   function Input_GUI_Mode return Input_Mode_Type;

   function Muted (Track : WNM.Tracks) return Boolean;

   function Some_FX_On return Boolean;

private

   function Has_Long_Press (B : Button) return Boolean;
   --  Can this button trigger a On_Long_Press event?

   function Has_Repeat_Press (B : Button) return Boolean;
   --  Can this button re-trigger a On_Press event when held down?

end WNM.UI;
