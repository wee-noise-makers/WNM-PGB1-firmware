-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with WNM.FM_Tuner;
with WNM.GUI.Bitmap_Fonts;     use WNM.GUI.Bitmap_Fonts;
with HAL; use HAL;

package body WNM.GUI.Menu.FM_Settings is

   Singleton : aliased FM_Settings_Menu;
   Animation_Step : UInt32 := 0;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Singleton'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out FM_Settings_Menu) is
      X : Integer := 35;
   begin
      case WNM.FM_Tuner.State is
         when WNM.FM_Tuner.Seeking_Up =>
            X := X + Integer (Animation_Step mod 10);
            Print (X_Offset    => X,
                   Y_Offset    => 5 + 8,
                   Str         => ">>>");
         when WNM.FM_Tuner.Seeking_Down =>
            X := X - Integer (Animation_Step mod 10);
            Print (X_Offset    => X,
                   Y_Offset    => 5 + 8,
                   Str         => "<<<");
         when WNM.FM_Tuner.Tunning =>
            Print (X_Offset    => X,
                   Y_Offset    => 5 + 8,
                   Str         => "...");
         when WNM.FM_Tuner.Tuned =>
            Print (X_Offset    => X,
                   Y_Offset    => 5 + 8,
                   Str         => WNM.FM_Tuner.Channel'Img);
      end case;
      Animation_Step := Animation_Step + 1;
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out FM_Settings_Menu;
                       Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            Menu.Pop (Success);
         when Right_Press =>
            Menu.Pop (Failure);
         when Encoder_Right =>
            if Event.Value > 0 then
               WNM.FM_Tuner.Seek (WNM.FM_Tuner.Up);
            else
               WNM.FM_Tuner.Seek (WNM.FM_Tuner.Down);
            end if;
         when Encoder_Left =>
            null;
      end case;

   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed (This  : in out FM_Settings_Menu) is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus (This       : in out FM_Settings_Menu;
                       Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;


end WNM.GUI.Menu.FM_Settings;
