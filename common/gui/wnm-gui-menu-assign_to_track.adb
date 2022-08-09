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

with WNM.GUI.Bitmap_Fonts; use WNM.GUI.Bitmap_Fonts;
with WNM.Sequencer;
with Enum_Next;

package body WNM.GUI.Menu.Assign_To_Track is

   package Tracks_Enum_Next is new Enum_Next (Tracks);
   use Tracks_Enum_Next;

   Assign_To_Track_Singleton : aliased Assign_To_Track_Window;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Assign_To_Track_Singleton'Access);
   end Push_Window;

   --------------------
   -- Selected_Track --
   --------------------

   function Selected_Track return Tracks
   is (Assign_To_Track_Singleton.Track);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This   : in out Assign_To_Track_Window)
   is
      X : Integer;
   begin
      X := 5;
      Print (X_Offset    => X,
             Y_Offset    => 0 + 8,
             Str         => "Assign to track:");

      X := 5;
      Print (X_Offset    => X,
             Y_Offset    => 9 + 8,
             Str         => This.Track'Img);
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Assign_To_Track_Window;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            Pop (Exit_Value => Success);
         when Right_Press =>
            Pop (Exit_Value => Failure);
         when Encoder_Right =>
            null;
         when Encoder_Left =>
            if Event.Value > 0 then
               This.Track := Next (This.Track);
            elsif Event.Value < 0 then
               This.Track := Prev (This.Track);
            end if;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed (This  : in out Assign_To_Track_Window)
   is
   begin
      This.Track := Sequencer.Editing_Track;
   end On_Pushed;


end WNM.GUI.Menu.Assign_To_Track;
