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

with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Screen; use WNM.Screen;
with WNM.Utils; use WNM.Utils;

package body WNM.GUI.Menu.Drum_Settings is

   Track_Settings_Singleton : aliased Instance;

   Step_Width : constant := Font_Width - 1;
   Step_Height : constant := Font_Height + 1;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Track_Settings_Singleton'Access);
   end Push_Window;

   ---------------------
   -- Draw_Select_Box --
   ---------------------

   procedure Draw_Select_Box (X, Y : Natural) is
   begin
      Draw_Rect (((X, Y + 1), Step_Width, Step_Height - 3));
   end Draw_Select_Box;

   ------------------
   -- Draw_Trigger --
   ------------------

   procedure Draw_Trigger (X, Y : Natural; Trig : Project.Trigger_Kind) is
      use WNM.Project;
      DX : constant Natural := X + Font_Width / 2 - 1;
      DY : constant Natural := Y + Font_Height / 2 + 1;
   begin
      case Trig is
         when None => null;
         when Ghost =>
            Draw_Line ((DX - 1, DY), (DX + 1, DY));
         when Hit =>
            Draw_Line ((DX - 1, DY), (DX + 1, DY));
            Draw_Line ((DX - 1, DY - 1), (DX + 1, DY - 1));
         when Accent =>
            Draw_Line ((DX - 1, DY), (DX + 1, DY));
            Draw_Line ((DX - 1, DY - 1), (DX + 1, DY - 1));
            Draw_Line ((DX - 1, DY - 2), (DX + 1, DY - 2));
      end case;
   end Draw_Trigger;

   ----------------
   -- Small_Tick --
   ----------------

   procedure Small_Tick (X, Y : Integer) is
      DX : constant Integer := X + Font_Width / 2 - 1;
   begin
      Set_Pixel ((DX, Y));
   end Small_Tick;

   --------------
   -- Big_Tick --
   --------------

   procedure Big_Tick (X, Y : Integer) is
      DX : constant Integer := X + Font_Width / 2 - 1;
   begin
      Screen.Set_Pixel ((DX, Y));
      Screen.Set_Pixel ((DX, Y - 1));
   end Big_Tick;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Instance) is
      use WNM.Project;

      Disp_Step_Cnt : constant := 20;
      Step_Cnt : constant Pattern_Length :=
        Pattern_Length'Min (Disp_Step_Cnt, Project.Pattern_Length);

      First_Step : constant WNM.Pattern_Length :=
        Pattern_Length
          (Integer'Min
             (Integer'Max (1,
                           Integer (This.Selected_Step) - Disp_Step_Cnt / 2),
              Integer (Project.Pattern_Length) -
                  Integer (Step_Cnt) + 1));

      Last_Step  : constant WNM.Pattern_Length :=
        Pattern_Length'Min (Project.Pattern_Length,
                                First_Step + Step_Cnt - 1);

      function SX (Step : WNM.Pattern_Length)
                   return Natural
      is (7 + Integer (Step - First_Step + 1) * Step_Width);

      function SY (DT : Project.Drum_Tracks)
                   return Natural
      is (Box_Top - 2 + Project.Drum_Tracks'Pos (DT) * Step_Height);

      Tick_Line_Y : constant := Box_Top - 2;
      Tick_Text_Y : constant := Tick_Line_Y - 10;

      Len_Box_Left : constant Natural :=
        SX (Last_Step) + Step_Width + 1;
   begin

      for S in WNM.Pattern_Length range First_Step .. Last_Step loop

         if (Integer (S) - 1) mod 4 = 0 then
            Big_Tick (SX (S), Tick_Line_Y);
         else
            Small_Tick (SX (S), Tick_Line_Y);
         end if;

         if (Integer (S) - 1) mod 4 = 0 then
            Draw_Str (SX (S), Tick_Text_Y,
                      Trim (Pattern_Length'Image ((S / 4) + 1)));
         end if;
      end loop;

      if Last_Step /= Project.Pattern_Length then
         Draw_Str (SX (Last_Step + 1), Tick_Text_Y, ">");
      end if;

      if First_Step /= Pattern_Length'First then
         Draw_Str (SX (First_Step - 1) - 1, Tick_Text_Y, "<");
      end if;

      for T in Project.Drum_Tracks loop

         Draw_Str (0, SY (T), (case T is
                      when Kick => "BD",
                      when Snare => "SD",
                      when Hihat_Closed => "HC",
                      when Hihat_Open => "HO",
                      when Sample => "SP"));

         for S in WNM.Pattern_Length range First_Step .. Last_Step loop
            declare
               Trig : constant Project.Trigger_Kind :=
                 Project.Trigger (T, S);
            begin
               if Trig /= None then
                  Draw_Trigger (SX (S), SY (T), Trig);
               end if;
            end;
         end loop;
      end loop;

      if This.Mode = Step_Edit then
         Draw_Select_Box (SX (This.Selected_Step),
                          SY (This.Selected_Track));
      else
         Draw_Rect (((Len_Box_Left, SY (Kick)),
                    Font_Width + 3,
                    Box_Height));
      end if;

      Draw_Str (Len_Box_Left + 2, SY (Snare), "+");
      Draw_Str (Len_Box_Left + 2, SY (Hihat_Open), "-");
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Instance;
      Event : Menu_Event)
   is
   begin
      case This.Mode is
         when Step_Edit =>
            case Event.Kind is
            when Left_Press =>
               if This.Selected_Step /= WNM.Pattern_Length'First then
                  This.Selected_Step := This.Selected_Step - 1;
               end if;

            when Right_Press =>
               if This.Selected_Step < Project.Pattern_Length then
                  This.Selected_Step := This.Selected_Step + 1;
               else
                  This.Mode := Len_Edit;
               end if;

            when Up_Press =>
               Prev (This.Selected_Track);

            when Down_Press =>
               Next (This.Selected_Track);

            when A_Press =>
               Project.Trigger_Next (This.Selected_Track, This.Selected_Step);
            when B_Press =>
               Project.Trigger_Prev (This.Selected_Track, This.Selected_Step);

            when Slider_Touch =>
               null;
            end case;

         when Len_Edit =>
            case Event.Kind is
            when Left_Press =>
                  This.Mode := Step_Edit;
            when Right_Press =>
               null;

            when Up_Press =>
               null;
            when Down_Press =>
               null;
            when A_Press =>
               Project.Incr_Pattern_Length;
               This.Selected_Step := Project.Pattern_Length;
            when B_Press =>
               Project.Decr_Pattern_Length;
               This.Selected_Step := Project.Pattern_Length;
            when Slider_Touch =>
               null;
            end case;
      end case;

   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Instance)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus
     (This       : in out Instance;
      Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Drum_Settings;
