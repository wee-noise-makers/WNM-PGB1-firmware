-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2020 Fabien Chouteau                    --
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

with WNM.Synth;
with WNM.GUI.Bitmap_Fonts; use WNM.GUI.Bitmap_Fonts;
with HAL.Bitmap;           use HAL.Bitmap;
with Enum_Next;

package body WNM.GUI.Menu.Passthrough is

   package Src_Enum_Next is new Enum_Next (Audio.Input_Kind);
   use Src_Enum_Next;

   Win  : aliased Passthrough_Window;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Win'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Passthrough_Window)
   is
      X : Integer := 5;
   begin
      Print (X_Offset    => X,
             Y_Offset    => 0 + 8,
             Str         => "Source:");

      X := 5;
      Print (X_Offset    => X,
             Y_Offset    => 9 + 8,
             Str         => This.Src'Img);
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Passthrough_Window;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            Menu.Exit_Menu;
         when Right_Press =>
            Synth.Set_Passthrough (This.Before);
            Menu.Pop (Exit_Value => Failure);
         when Encoder_Right =>
            null;
         when Encoder_Left =>
            if Event.Value > 0 then
               This.Src := Next (This.Src);
               Synth.Set_Passthrough (This.Src);
            elsif Event.Value < 0 then
               This.Src := Prev (This.Src);
               Synth.Set_Passthrough (This.Src);
            end if;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding procedure On_Pushed
     (This  : in out Passthrough_Window)
   is
   begin
      This.Src := Synth.Get_Passthrough;
      This.Before := This.Src;
   end On_Pushed;

end WNM.GUI.Menu.Passthrough;
