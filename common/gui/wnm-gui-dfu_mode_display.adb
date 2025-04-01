-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2024 Fabien Chouteau                  --
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

--  with WNM.Screen;
with WNM.LEDs;
--  with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Time;

package body WNM.GUI.DFU_Mode_Display is

   -----------
   -- Setup --
   -----------

   procedure Setup is
   begin

      --  TODO: The DFU mode will reset all GPIO and therefore turn-off the
      --  screen N-reset pin. Cannot find a way to keep the screen up at this
      --  time...

      --  --  Wait in case there is a running screen DMA transfer
      --  WNM.Time.Delay_Milliseconds (30);
      --
      --  Screen.Clear;
      --
      --  Draw_Str_Center (0,  "Device in Update Mode");
      --
      --  Draw_Str_Center (12, "Go to");
      --  Draw_Str_Center (20, "weenoisemakers.com");
      --  Draw_Str_Center (28, "for new firmware and");
      --  Draw_Str_Center (36, "sample transfer info");
      --
      --  Draw_Str_Center (49, "Turn off the device");
      --  Draw_Str_Center (57, "to leave Update Mode");
      --
      --  Screen.Update;

      LEDs.Turn_Off_All;
      LEDs.Set_Hue (LEDs.Green);

      --  U...
      LEDs.Turn_On (C2);
      LEDs.Turn_On (C6);
      LEDs.Turn_On (C7);
      LEDs.Turn_On (C8);
      LEDs.Turn_On (C4);

      --  P...
      LEDs.Turn_On (WNM_Configuration.Menu);
      LEDs.Turn_On (Drums_Play);
      LEDs.Turn_On (Drum_Edit);
      LEDs.Turn_On (L2);
      LEDs.Turn_On (L4);
      LEDs.Turn_On (L3);
      LEDs.Turn_On (L6);

      LEDs.Update;

      --  Wait for LEDs and screen DMA to complete
      WNM.Time.Delay_Milliseconds (30);
   end Setup;

end WNM.GUI.DFU_Mode_Display;
