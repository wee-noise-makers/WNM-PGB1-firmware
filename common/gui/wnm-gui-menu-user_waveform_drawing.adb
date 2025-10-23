-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2023 Fabien Chouteau                  --
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
with Tresses; use Tresses;
with Tresses.DSP;
with Interfaces; use Interfaces;

package body WNM.GUI.Menu.User_Waveform_Drawing is

   Wave_Height : constant := Screen_Height - 15;
   H_Step : constant := 65_535 / Wave_Height;
   Singleton : aliased Instance;

   package S16_Next is new Enum_Next (S16);

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Singleton'Access);
   end Push_Window;

   --------------------------
   -- Cursor_To_Wave_Index --
   --------------------------

   function Cursor_To_Wave_Index (X : Pix_X) return U16
   is (U16 (X) * 2);

   ---------------------
   -- Frame_To_Height --
   ---------------------

   function Frame_To_Height (F : S16) return Natural is
      Ret : constant Integer := Integer (F) - Integer (S16'First);
   begin
      return Ret / H_Step;
   end Frame_To_Height;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This   : in out Instance) is

      ---------------------
      -- Set_Pixel_Check --
      ---------------------

      procedure Set_Pixel_Check (X, Y : Integer) is
      begin
         if X in Integer (Pix_X'First) .. Integer (Pix_X'Last)
           and then
             Y in Integer (Pix_Y'First) .. Integer (Pix_Y'Last)
         then
            Set_Pixel (Pix_X (X), Pix_Y (Y));
         end if;
      end Set_Pixel_Check;

      Y_Base : constant := Screen_Height - 1;
   begin
      --  Draw the cursor
      declare
         Point : constant S16 := This.Height;
         X : constant Integer := Integer (This.Cursor);
         Y : constant Integer :=
           Integer (Pix_Y (Y_Base - Frame_To_Height (Point)));
      begin
         if This.Writing then
            Set_Pixel_Check (X - 2, Y - 2);
            Set_Pixel_Check (X - 3, Y - 3);

            Set_Pixel_Check (X + 2, Y - 2);
            Set_Pixel_Check (X + 3, Y - 3);

            Set_Pixel_Check (X - 2, Y + 2);
            Set_Pixel_Check (X - 3, Y + 3);

            Set_Pixel_Check (X + 2, Y + 2);
            Set_Pixel_Check (X + 3, Y + 3);
         else
            Set_Pixel_Check (X - 2, Y - 1);
            Set_Pixel_Check (X - 1, Y - 2);

            Set_Pixel_Check (X + 2, Y - 1);
            Set_Pixel_Check (X + 1, Y - 2);

            Set_Pixel_Check (X - 2, Y + 1);
            Set_Pixel_Check (X - 1, Y + 2);

            Set_Pixel_Check (X + 2, Y + 1);
            Set_Pixel_Check (X + 1, Y + 2);
         end if;
      end;

      --  Draw the waveform
      for X in Pix_X loop
         declare
            Wave_Idx : constant U16 := Cursor_To_Wave_Index (X);
            Point : constant S16 := Synth.User_Waveform (Wave_Idx);
            Y : constant Pix_Y :=
              Pix_Y (Y_Base - Frame_To_Height (Point));
         begin
            Set_Pixel (X, Y);
            Set_Pixel (X, Y - 1, On => False);
            if Y /= Pix_Y'Last then
               Set_Pixel (X, Y + 1, On => False);
            end if;
         end;
      end loop;

      --  --  Zoom on half of the waveform for debugging
      --  for X in Pix_X loop
      --     declare
      --        Wave_Idx : constant U16 := U16 (X) + 129;
      --        Point : constant S16 := Synth.User_Waveform (Wave_Idx);
      --        Y : constant Pix_Y :=
      --          Pix_Y (Y_Base - Frame_To_Height (Point));
      --     begin
      --        Set_Pixel (X, Y);
      --     end;
      --  end loop;
      --  Draw_Str_Center (20, Cursor_To_Wave_Index (This.Cursor)'Img);
   end Draw;

   ---------------------------
   -- Update_Waveform_Point --
   ---------------------------

   procedure Update_Waveform_Point (This : Instance) is
      Wave_Idx : constant U16 := Cursor_To_Wave_Index (This.Cursor);
   begin
      Synth.User_Waveform (Wave_Idx) := This.Height;

      --  Corner case: last waveform point. The waveform index goes from 0 to
      --  256, but given the screen resolution we can only edit up to 254 (127
      --  * 2). We set it to the mean between the first and last
      --  editable points.
      if This.Cursor in Pix_X'First | Pix_X'Last then
         declare
            First : constant S16 :=
              Synth.User_Waveform (Cursor_To_Wave_Index (Pix_X'First));
            Last : constant S16 :=
              Synth.User_Waveform (Cursor_To_Wave_Index (Pix_X'Last));
            Mean : constant S16 :=
              Tresses.DSP.Mix (Last, First, Param_Range'Last / 2);
         begin
            Synth.User_Waveform (Synth.User_Waveform'Last) := Mean;
         end;
      end if;

      --  The screen horizontal resolution is half the number of points in the
      --  waveform. So we can only draw and control every other point of the
      --  waveform. When one of these point changes, we also modify the one
      --  before and the one after (that are not controlled directly) to be
      --  the average between controlled points.

      --  Average the point after
      declare
         Next : constant S16 := Synth.User_Waveform (Wave_Idx + 2);
         Mean : constant S16 :=
           Tresses.DSP.Mix (This.Height, Next, Param_Range'Last / 2);
      begin
         Synth.User_Waveform (Wave_Idx + 1) := Mean;
      end;

      --  Average the point before
      if Wave_Idx > Synth.User_Waveform'First + 1 then
         declare
            Prev : constant S16 := Synth.User_Waveform (Wave_Idx - 2);
            Mean : constant S16 :=
              Tresses.DSP.Mix (Prev, This.Height, Param_Range'Last / 2);
         begin
            Synth.User_Waveform (Wave_Idx - 1) := Mean;
         end;
      end if;

   end Update_Waveform_Point;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Instance;
                       Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            if This.Cursor /= Pix_X'First then
               This.Cursor := @ - 1;
               if This.Writing then
                  Update_Waveform_Point (This);
               end if;
            end if;
         when Right_Press =>
            if This.Cursor /= Pix_X'Last then
               This.Cursor := @ + 1;
               if This.Writing then
                  Update_Waveform_Point (This);
               end if;
            end if;
         when Up_Press =>
            if This.Height <= S16'Last - H_Step then
               This.Height := @ + H_Step;
            else
               This.Height := S16'Last;
            end if;

            if This.Writing then
               Update_Waveform_Point (This);
            end if;
         when Down_Press =>
            if This.Height >= S16'First + H_Step then
               This.Height := @ - H_Step;
            else
               This.Height := S16'First;
            end if;

            if This.Writing then
               Update_Waveform_Point (This);
            end if;

         when B_Press =>
            Menu.Pop (Success);

         when A_Press =>
            This.Writing := not This.Writing;

         when Slider_Touch =>
            S16_Next.Set (This.Height, Event.Slider_Value);
            if This.Writing then
               Update_Waveform_Point (This);
            end if;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed (This  : in out Instance) is
   begin
      This.Cursor := 0;
      This.Height := Synth.User_Waveform (Synth.User_Waveform'First);
   end On_Pushed;

end WNM.GUI.Menu.User_Waveform_Drawing;
