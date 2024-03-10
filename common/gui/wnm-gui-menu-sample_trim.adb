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

with WNM.Shared_Buffers;
with WNM.Sample_Library;
with WNM.Sample_Edit;
with WNM.GUI.Menu.Drawing;
with WNM.GUI.Bitmap_Fonts;     use WNM.GUI.Bitmap_Fonts;

with Tresses.Resources;

package body WNM.GUI.Menu.Sample_Trim is

   Sample_Trim_Singleton : aliased Trim_Window;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Sample_Trim_Singleton'Access);
   end Push_Window;

   --------------------
   -- Preview_Sample --
   --------------------

   procedure Preview_Sample (This : Trim_Window) is
   begin
      null;
   end Preview_Sample;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Trim_Window)
   is
      use Sample_Library;
      use Shared_Buffers;
      X : Integer;

      Length : constant Sample_Point_Count := End_Point - Start_Point;

      Text_Top : constant Integer := Screen_Height - Bitmap_Fonts.Height - 2;

      Start_Img : constant String := To_Seconds (Start_Point)'Img;
      End_Img : constant String := To_Seconds (End_Point)'Img;
      Len_Img : constant String := To_Seconds (Length)'Img;
   begin
      WNM.GUI.Menu.Drawing.Draw_Waveform (15,
                                          Show_Cut => True,
                                          Show_Playhead => True);
      X := 0;
      Print (X_Offset => X,
             Y_Offset => Text_Top,
             Str      => Start_Img & " <-" & Len_Img & " ->" & End_Img);
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Trim_Window;
      Event : Menu_Event)
   is
      Move_Step : constant := Tresses.Resources.SAMPLE_RATE / 100;
   begin
      case Event.Kind is
         when A_Press =>
            Pop (Exit_Value => Success);
         when B_Press =>
            Pop (Exit_Value => Failure);
         when Up_Press =>
            Shared_Buffers.Move_End_Point (Move_Step);
         when Down_Press =>
            Shared_Buffers.Move_End_Point (-Move_Step);
         when Right_Press =>
            Shared_Buffers.Move_Start_Point (Move_Step);
         when Left_Press =>
            Shared_Buffers.Move_Start_Point (-Move_Step);
         when others =>
            null;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed (This : in out Trim_Window) is
   begin
      WNM.Sample_Edit.Update_Waveform;
   end On_Pushed;

end WNM.GUI.Menu.Sample_Trim;
