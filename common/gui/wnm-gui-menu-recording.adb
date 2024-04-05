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

with WNM.Screen;               use WNM.Screen;
with WNM.GUI.Bitmap_Fonts;     use WNM.GUI.Bitmap_Fonts;

with tape_1;
with tape_2;
with tape_3;
with tape_4;
with HAL; use HAL;

with WNM.GUI.Menu.Drawing;

package body WNM.GUI.Menu.Recording is

   Recording_Singleton : aliased Recording_Window;
   Animation_Step : UInt32 := 0;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Recording_Singleton'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (This   : in out Recording_Window)
   is
      pragma Unreferenced (This);
      X             : Integer := 35;
      Wave_Top      : constant Integer := 15;
      Recording_Top : constant Integer := Screen_Height - tape_1.Data.H - 1;

   begin
      Print (X_Offset    => X,
             Y_Offset    => Recording_Top + 5,
             Str         => "Recording");

      Copy_Bitmap ((case Animation_Step mod 4 is
                      when 0      => tape_1.Data,
                      when 1      => tape_2.Data,
                      when 2      => tape_3.Data,
                      when others => tape_4.Data),
                   X            => 0,
                   Y            => Recording_Top,
                   Invert_Color => True);
      Animation_Step := Animation_Step + 1;

      Drawing.Draw_Waveform (Wave_Top, Show_Cut => True);
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Recording_Window;
      Event : Menu_Event)
   is
      pragma Unreferenced (This);
   begin
      case Event.Kind is
         when A_Press =>
            Menu.Pop (Exit_Value => Success);
         when B_Press =>
            Menu.Pop (Exit_Value => Failure);
         when others =>
            null;
      end case;
   end On_Event;

end WNM.GUI.Menu.Recording;
