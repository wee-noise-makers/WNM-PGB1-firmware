-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2021 Fabien Chouteau                  --
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

with WNM.Utils;
with WNM.Sample_Recording;

with lfo_sine;
with lfo_ramp_up;
with lfo_ramp_down;
with lfo_exp_up;
with lfo_exp_down;
with lfo_triangle;
with lfo_sync;
with lfo_loop;
with filter_bp;
with filter_lp;
with filter_hp;
with fx_bypass;
with fx_bitcrusher;
with fx_filter;
with fx_overdrive;
with fx_reverb;
with cut_2;
with magic_hat_1;
with magic_hat_2;
with magic_hat_3;
with battery_100;
with battery_90;
with battery_80;
with battery_70;
with battery_60;
with battery_50;
with battery_40;
with battery_30;
with battery_20;
with battery_10;
with battery_0;

package body WNM.GUI.Menu.Drawing is

   Scroll_Bar_Y_Offset : constant := 19;

   Value_Text_Y : constant := Box_Bottom - 11;
   Select_Line_Y : constant := Box_Bottom - 3;
   Title_Text_Y : constant := Box_Top + 4;

   Arrow_Y_Offset : constant := Box_Top - Bitmap_Fonts.Height - 2;

   --------------
   -- Draw_Str --
   --------------

   procedure Draw_Str (X, Y      : Integer;
                       Str       : String;
                       Underline : Boolean := False)
   is
      VX : Integer := X;
   begin
      Bitmap_Fonts.Print (VX, Y, Str);

      if Underline then
         Screen.Draw_Line ((X, Y + Font_Height), (VX, Y + Font_Height));
      end if;
   end Draw_Str;

   -------------------
   -- Draw_Menu_Box --
   -------------------

   procedure Draw_Menu_Box
     (Title       : String;
      Count       : Natural;
      Index       : Natural)
   is
      X : Integer;
   begin
      X := (Screen.Width - Title'Length * Bitmap_Fonts.Width) / 2;
      Print (X_Offset    => X,
             Y_Offset    => Title_Y_Offset,
             Str         => Title);

      if Count > 0 then
         declare
            Item_width   : constant := 4;
            Item_Spacing : constant := Item_width + 2;
            X_Offset : constant Natural :=
              (Screen.Width - Count * Item_Spacing) / 2;
         begin
            for Item in 0 .. Count - 1 loop
               Screen.Draw_Line ((X_Offset + Item * Item_Spacing,
                                 Scroll_Bar_Y_Offset),
                                 (X_Offset + Item * Item_Spacing + Item_width,
                                  Scroll_Bar_Y_Offset));
            end loop;

            Screen.Draw_Line ((X_Offset + Index * Item_Spacing,
                              Scroll_Bar_Y_Offset + 1),
                              (X_Offset + Index * Item_Spacing + Item_width,
                               Scroll_Bar_Y_Offset + 1));
         end;

         if Index < Count - 1 then
            X := Box_Right - Bitmap_Fonts.Width;
            Print (X_Offset => X,
                   Y_Offset => Arrow_Y_Offset,
                   C        => '>');
         end if;
      end if;

      --  Top line
      Screen.Draw_Line (Start => (Box_Left + 2, Box_Top),
                        Stop  => (Box_Right - 2, Box_Top));
      --  Bottom line
      Screen.Draw_Line (Start => (Box_Left + 2, Box_Bottom),
                        Stop  => (Box_Right - 2, Box_Bottom));

      --  Side left
      Screen.Draw_Line (Start => (Box_Left, Box_Top + 2),
                        Stop  => (Box_Left, Box_Bottom - 2));
      --  Side right
      Screen.Draw_Line (Start => (Box_Right, Box_Top + 2),
                        Stop  => (Box_Right, Box_Bottom - 2));

      Screen.Set_Pixel ((Box_Left + 1, Box_Top + 1));
      Screen.Set_Pixel ((Box_Left + 1, Box_Bottom - 1));
      Screen.Set_Pixel ((Box_Right - 1, Box_Top + 1));
      Screen.Set_Pixel ((Box_Right - 1, Box_Bottom - 1));

      if Count > 0 and then Index /= 0 then
         X := 0;
         Print (X_Offset => X,
                Y_Offset => Arrow_Y_Offset,
                C        => '<');
      end if;
   end Draw_Menu_Box;

   -----------------
   -- Draw_Volume --
   -----------------

   procedure Draw_Volume (Title : String;
                          Val   : WNM_HAL.Audio_Volume)
   is
      X : Integer := Box_Left + 4;
   begin
      Print (X_Offset    => X,
             Y_Offset    => Title_Text_Y,
             Str         => Title);

      Screen.Draw_Line ((Box_Center.X - 50, Box_Center.Y),
                        (Box_Center.X + 50, Box_Center.Y));

      Screen.Draw_Line ((Box_Center.X - 50 + Integer (Val), Box_Center.Y - 2),
                        (Box_Center.X - 50 + Integer (Val), Box_Center.Y + 2));

      X := Box_Center.X - 11;
      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Val'Img & "%");

   end Draw_Volume;

   --------------
   -- Draw_Pan --
   --------------

   procedure Draw_Pan (Title : String;
                       Val   : WNM_HAL.Audio_Pan)
   is
      X : Integer := Box_Left + 4;
   begin
      Print (X_Offset    => X,
             Y_Offset    => Title_Text_Y,
             Str         => Title);

      Screen.Draw_Line ((Box_Center.X - 50, Box_Center.Y),
                        (Box_Center.X + 50, Box_Center.Y));

      Screen.Draw_Line ((Box_Center.X - 50 + Integer (Val), Box_Center.Y - 2),
                        (Box_Center.X - 50 + Integer (Val), Box_Center.Y + 2));

      X := Box_Center.X - 8;
      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Val'Img);
   end Draw_Pan;

   -------------------
   -- Draw_MIDI_Val --
   -------------------

   procedure Draw_MIDI_Val (Val      : MIDI.MIDI_Data;
                            Selected : Boolean)
   is
      X : Integer := Box_Center.X + 20;
   begin
      if Selected then
         Screen.Draw_Line ((X + 5, Select_Line_Y),
                           (X + 4 * 6, Select_Line_Y));
      end if;

      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Val'Img);
   end Draw_MIDI_Val;

   --------------------
   -- Draw_MIDI_Note --
   --------------------

   procedure Draw_MIDI_Note (Key      : MIDI.MIDI_Key;
                             Selected : Boolean)
   is
      X : Integer := Box_Left + 5;
      Str : constant String := Utils.Trim (Key_Img (Key));
   begin
      if Selected then
         Screen.Draw_Line ((X - 1, Select_Line_Y),
                           (X + Str'Length * Bitmap_Fonts.Width,
                            Select_Line_Y));
      end if;

      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Str);

   end Draw_MIDI_Note;

   -------------------
   -- Draw_Duration --
   -------------------

   procedure Draw_Duration (D        : Project.Note_Duration;
                            Selected : Boolean)
   is
      use WNM.Project;

      Pos_X : constant Natural := Box_Center.X + 14;
      DX : constant Natural := Pos_X - 3;
      DY : constant := Box_Top + 15;

      Str : constant String := Img (D);
      Str_Pix_Len : constant Natural := Str'Length * Bitmap_Fonts.Width;
      Str_X : constant Natural := Pos_X - (Str_Pix_Len / 2);

      X : Integer := Str_X;
   begin

      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Str);

      if Selected then
         Screen.Draw_Line
           ((Str_X, Select_Line_Y),

            (Str_X + Str_Pix_Len, Select_Line_Y));
      end if;

      if D = Double then
         for Cnt in 0 .. 4 loop
            --  X     X
            --  XXXXXXX
            --  X     X
            --  XXXXXXX
            --  X     X
            Screen.Set_Pixel ((DX, DY + 5 + Cnt));
            Screen.Set_Pixel ((DX + 5, DY + 5 + Cnt));

            Screen.Set_Pixel ((DX + Cnt, DY + 5 +  1));
            Screen.Set_Pixel ((DX + Cnt, DY + 5 + 3));
         end loop;
      else
         --   XXX
         --  X   X
         --  X   X
         --   XXX
         Screen.Set_Pixel ((DX + 0, DY + 7));
         Screen.Set_Pixel ((DX + 0, DY + 8));
         Screen.Set_Pixel ((DX + 4, DY + 7));
         Screen.Set_Pixel ((DX + 4, DY + 8));
         Screen.Set_Pixel ((DX + 1, DY + 6));
         Screen.Set_Pixel ((DX + 2, DY + 6));
         Screen.Set_Pixel ((DX + 3, DY + 6));
         Screen.Set_Pixel ((DX + 1, DY + 9));
         Screen.Set_Pixel ((DX + 2, DY + 9));
         Screen.Set_Pixel ((DX + 3, DY + 9));
         if D = Whole then
            return;
         end if;

         --      X
         --      X
         --      X
         --      X
         --      X
         --      X
         --   XXXX
         --  X   X
         --  X   X
         --   XXX
         for Cnt in 0 .. 6 loop
            Screen.Set_Pixel ((DX + 4, DY + Cnt));
         end loop;

         if D = Half then
            return;
         end if;

         --      X
         --      X
         --      X
         --      X
         --      X
         --      X
         --   XXXX
         --  XXXXX
         --  XXXXX
         --   XXX
         Screen.Set_Pixel ((DX + 1, DY + 7));
         Screen.Set_Pixel ((DX + 2, DY + 7));
         Screen.Set_Pixel ((DX + 3, DY + 7));
         Screen.Set_Pixel ((DX + 1, DY + 8));
         Screen.Set_Pixel ((DX + 2, DY + 8));
         Screen.Set_Pixel ((DX + 3, DY + 8));

         if D = Quarter then
            return;
         end if;

         --      XX
         --      X X
         --      X  X
         --      X
         --      X
         --      X
         --   XXXX
         --  XXXXX
         --  XXXXX
         --   XXX
         Screen.Set_Pixel ((DX + 5, DY + 0));
         Screen.Set_Pixel ((DX + 6, DY + 1));
         Screen.Set_Pixel ((DX + 7, DY + 2));
         if D = N_8th then
            return;
         end if;

         --      XX
         --      X X
         --      XX X
         --      X X X
         --      X
         --      X
         --   XXXX
         --  XXXXX
         --  XXXXX
         --   XXX
         Screen.Set_Pixel ((DX + 8, DY + 3));
         Screen.Set_Pixel ((DX + 5, DY + 2));
         Screen.Set_Pixel ((DX + 6, DY + 3));
         if D = N_16th then
            return;
         end if;

         --      XX
         --      X X
         --      XX X
         --      X X X
         --      XX X
         --      X X
         --   XXXX
         --  XXXXX
         --  XXXXX
         --   XXX
         Screen.Set_Pixel ((DX + 7, DY + 4));
         Screen.Set_Pixel ((DX + 5, DY + 4));
         Screen.Set_Pixel ((DX + 6, DY + 5));
      end if;
   end Draw_Duration;

   ---------------------
   -- Draw_Chord_Kind --
   ---------------------

   procedure Draw_Chord_Kind (Str      : String;
                              Selected : Boolean)
   is
      X : Integer := Box_Left + 8;
   begin
      if Selected then
         Screen.Draw_Line ((X - 1, Select_Line_Y),
                           (X + 3 * 6, Select_Line_Y));
      end if;

      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Str);
   end Draw_Chord_Kind;

   ----------------
   -- Draw_Title --
   ----------------

   procedure Draw_Title (Title : String;
                        Val   : String)
   is
      X : Integer := Box_Left + 4;
   begin
      Print (X_Offset    => X,
             Y_Offset    => Title_Text_Y,
             Str         => Title);

      X := Box_Left + 4;
      Print (X_Offset    => X,
             Y_Offset    => Title_Text_Y + 8,
             Str         => Val);
   end Draw_Title;

   ----------------
   -- Draw_Value --
   ----------------

   procedure Draw_Value (Val      : String;
                         Selected : Boolean := False)
   is
      X : Integer := Box_Left + 8;
   begin

      if Selected then
         Screen.Draw_Line ((X - 1, Select_Line_Y),
                           (X + Val'Length * 6, Select_Line_Y));
      end if;

      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Val);
   end Draw_Value;

   ---------------------
   -- Draw_Value_Left --
   ---------------------

   procedure Draw_Value_Left (Val      : String;
                              Selected : Boolean := False)
   is
      X : Integer := Box_Center.X + 4;
   begin
      if Selected then
         Screen.Draw_Line ((X - 1, Select_Line_Y),
                           (X + Val'Length * 6, Select_Line_Y));
      end if;

      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Val);
   end Draw_Value_Left;

   --------------------
   -- Draw_Value_Pos --
   --------------------

   procedure Draw_Value_Pos (Val      : String;
                             Pos      : Natural;
                             Selected : Boolean := False)
   is
      X : Integer := Pos;
   begin
      if Selected then
         Screen.Draw_Line ((X - 1, Select_Line_Y),
                           (X + Val'Length * 6, Select_Line_Y));
      end if;

      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Val);
   end Draw_Value_Pos;

   -------------------
   -- Draw_CC_Value --
   -------------------

   procedure Draw_CC_Value (Id       : WNM.Project.CC_Id;
                            Value    : MIDI.MIDI_Data;
                            Label    : String;
                            Selected : Boolean;
                            Enabled  : Boolean := True;
                            Style    : CC_Draw_Style := Positive)
   is
      Val : Natural := Natural (Value) + 1;
      Last_Width : Natural;
      X, Y : Natural;

      Spacing : constant := 32;
      Left : constant Natural := Box_Left + 6 +
        (case Id is
            when WNM.Project.A => 0 * Spacing,
            when WNM.Project.B => 1 * Spacing,
            when WNM.Project.C => 2 * Spacing,
            when WNM.Project.D => 3 * Spacing);

      Sub_Label_Width : constant := (Bitmap_Fonts.Width * 3) - 1;
      Bar_Width : constant := 8;
      Bar_Height : constant := 128 / Bar_Width - 1;
      Bar_Left : constant Natural :=
        Left + (Sub_Label_Width - Bar_Width) / 2;
      Bar_Bottom : constant := Value_Text_Y + 1;
      Bar_Top    : constant := Bar_Bottom - Bar_Height;
      Bar_Center : constant Screen.Point :=
        (Bar_Left + Bar_Width / 2,
         Bar_Bottom - Bar_Height / 2);

   begin
      Y := Bar_Bottom;

      if Selected then
         Screen.Draw_Line ((Bar_Left - 2, Y), (Bar_Left - 2, Y - Bar_Height));
         Screen.Draw_Line ((Bar_Left + Bar_Width + 1, Y),
                           (Bar_Left + Bar_Width + 1, Y - Bar_Height));
      end if;

      --  Short label
      X := Left;
      Print (X_Offset => X,
             Y_Offset => Value_Text_Y + 3,
             Str      => Label);

      if Enabled then

         case Style is
            when Positive =>

               Y := Bar_Bottom;

               while Val >= Bar_Width loop
                  Val := Val - Bar_Width;
                  Screen.Draw_Line ((Bar_Left, Y),
                                    (Bar_Left + Bar_Width - 1, Y));
                  Y := Y - 1;
               end loop;

               if Val > 0 then
                  Last_Width := Val - 1;
                  Screen.Draw_Line ((Bar_Left, Y),
                                    (Bar_Left + Last_Width, Y));
               end if;

            when Center | Negative =>

               if Style = Center then
                  Y := Bar_Top + (Bar_Height / 2 - (Val / Bar_Width) / 2);
               else
                  Y := Bar_Top;
               end if;

               while Val >= Bar_Width loop
                  Val := Val - Bar_Width;
                  Screen.Draw_Line ((Bar_Left, Y),
                                    (Bar_Left + Bar_Width - 1, Y));
                  Y := Y + 1;
               end loop;

               if Val > 0 then
                  Last_Width := Val - 1;
                  Screen.Draw_Line ((Bar_Left, Y),
                                    (Bar_Left + Last_Width, Y));
               end if;

         end case;
      else
         Screen.Draw_Line
           ((Bar_Center.X - Bar_Width / 2, Bar_Center.Y + Bar_Width / 2),
            (Bar_Center.X + Bar_Width / 2, Bar_Center.Y - Bar_Width / 2));
         Screen.Draw_Line
           ((Bar_Center.X + Bar_Width / 2, Bar_Center.Y + Bar_Width / 2),
            (Bar_Center.X - Bar_Width / 2, Bar_Center.Y - Bar_Width / 2));
      end if;

   end Draw_CC_Value;

   --------------------
   -- Draw_LFO_Shape --
   --------------------

   procedure Draw_LFO_Shape (Id       : WNM.Project.CC_Id;
                             Label    : String;
                             Selected : Boolean;
                             Shape    : WNM.Project.LFO_Shape_Kind;
                             Sync     : WNM.Project.LFO_Sync_Kind;
                             Loo      : WNM.Project.LFO_Loop_Kind)
   is
      use WNM.Project;

      X, Y : Natural;

      Spacing : constant := 32;
      Left : constant Natural := Box_Left + 6 +
        (case Id is
            when WNM.Project.A => 0 * Spacing,
            when WNM.Project.B => 1 * Spacing,
            when WNM.Project.C => 2 * Spacing,
            when WNM.Project.D => 3 * Spacing);

      Sub_Label_Width : constant := (Bitmap_Fonts.Width * 3) - 1;
      Bar_Width : constant := 8;
      Bar_Height : constant := 128 / Bar_Width - 1;
      Bar_Left : constant Natural :=
        Left + (Sub_Label_Width - Bar_Width) / 2;
      Bar_Bottom : constant := Value_Text_Y + 1;
      Bar_Top    : constant := Bar_Bottom - Bar_Height;
      --  Bar_Center : constant Screen.Point :=
      --    (Bar_Left + Bar_Width / 2,
      --     Bar_Bottom - Bar_Height / 2);
   begin

      Y := Bar_Bottom;

      if Selected then
         Screen.Draw_Line ((Bar_Left - 3, Y), (Bar_Left - 3, Y - Bar_Height));
         Screen.Draw_Line ((Bar_Left + Bar_Width + 2, Y),
                           (Bar_Left + Bar_Width + 2, Y - Bar_Height));
      end if;

      --  Short label
      X := Left;
      Print (X_Offset => X,
             Y_Offset => Value_Text_Y + 4,
             Str      => Label);

      if Sync = On then
         Screen.Copy_Bitmap
           (lfo_sync.Data,
            Bar_Left,
            Value_Text_Y - 2);
      end if;

      if Loo = On then
         Screen.Copy_Bitmap
           (lfo_loop.Data,
            Bar_Left + 5,
            Value_Text_Y - 2);
      end if;

      X := Bar_Left - 1;
      Y := Bar_Top;
      case Shape is
         when Sine =>
            Screen.Copy_Bitmap
              (lfo_sine.Data, X, Y);
         when Ramp_Up =>
            Screen.Copy_Bitmap
              (lfo_ramp_up.Data, X, Y);
         when Ramp_Down =>
            Screen.Copy_Bitmap
              (lfo_ramp_down.Data, X, Y);
         when Exp_Up =>
            Screen.Copy_Bitmap
              (lfo_exp_up.Data, X, Y);
         when Exp_Down =>
            Screen.Copy_Bitmap
              (lfo_exp_down.Data, X, Y);
         when Triangle =>
            Screen.Copy_Bitmap
              (lfo_triangle.Data, X, Y);
      end case;

   end Draw_LFO_Shape;

   -----------------
   -- Draw_Volume --
   -----------------

   procedure Draw_Volume (Id       : WNM.Project.CC_Id;
                          Value    : WNM_HAL.Audio_Volume;
                          Label    : String;
                          Selected : Boolean)
   is
      Val : Natural := Natural (Value);
      Last_Width : Natural;
      X, Y : Natural;

      Spacing : constant := 32;
      Left : constant Natural := Box_Left + 6 +
        (case Id is
            when WNM.Project.A => 0 * Spacing,
            when WNM.Project.B => 1 * Spacing,
            when WNM.Project.C => 2 * Spacing,
            when WNM.Project.D => 3 * Spacing);

      Sub_Label_Width : constant := (Bitmap_Fonts.Width * 3) - 1;
      Bar_Width : constant := 10;
      Bar_Height : constant := 100 / Bar_Width - 1;
      Bar_Left : constant Natural :=
        Left + (Sub_Label_Width - Bar_Width) / 2;
      Bar_Bottom : constant := Value_Text_Y - 3;
      Bar_Center : constant Screen.Point :=
        (Bar_Left + Bar_Width / 2,
         Bar_Bottom - Bar_Height / 2);

      Enabled : constant Boolean := Value /= 0;
   begin
      Y := Bar_Bottom;

      if Selected then
         Screen.Draw_Line ((Bar_Left - 2, Y), (Bar_Left - 2, Y - Bar_Height));
         Screen.Draw_Line ((Bar_Left + Bar_Width + 1, Y),
                           (Bar_Left + Bar_Width + 1, Y - Bar_Height));
      end if;

      --  Short label
      X := Left;
      Print (X_Offset => X,
             Y_Offset => Value_Text_Y + 4,
             Str      => Label);

      if Enabled then

         Y := Bar_Bottom;

         while Val >= Bar_Width loop
            Val := Val - Bar_Width;
            Screen.Draw_Line ((Bar_Left, Y),
                              (Bar_Left + Bar_Width - 1, Y));
            Y := Y - 1;
         end loop;

         if Val > 0 then
            Last_Width := Val - 1;
            Screen.Draw_Line ((Bar_Left, Y),
                              (Bar_Left + Last_Width, Y));
         end if;

      else
         Screen.Draw_Line
           ((Bar_Center.X - Bar_Width / 2, Bar_Center.Y + Bar_Width / 2),
            (Bar_Center.X + Bar_Width / 2, Bar_Center.Y - Bar_Width / 2));
         Screen.Draw_Line
           ((Bar_Center.X + Bar_Width / 2, Bar_Center.Y + Bar_Width / 2),
            (Bar_Center.X - Bar_Width / 2, Bar_Center.Y - Bar_Width / 2));
      end if;

   end Draw_Volume;

   -------------
   -- Draw_FX --
   -------------

   procedure Draw_FX (Id       : WNM.Project.CC_Id;
                      Value    : FX_Kind;
                      Selected : Boolean)
   is
      use WNM.Project;

      X, Y : Natural;

      Spacing : constant := 32;
      Left : constant Natural := Box_Left + 6 +
        (case Id is
            when WNM.Project.A => 0 * Spacing,
            when WNM.Project.B => 1 * Spacing,
            when WNM.Project.C => 2 * Spacing,
            when WNM.Project.D => 3 * Spacing);

      Sub_Label_Width : constant := (Bitmap_Fonts.Width * 3) - 1;
      Bar_Width : constant := 8;
      Bar_Height : constant := 128 / Bar_Width - 1;
      Bar_Left : constant Natural :=
        Left + (Sub_Label_Width - Bar_Width) / 2;
      Bar_Bottom : constant := Value_Text_Y + 1;
      Bar_Top    : constant := Bar_Bottom - Bar_Height;
      --  Bar_Center : constant Screen.Point :=
      --    (Bar_Left + Bar_Width / 2,
      --     Bar_Bottom - Bar_Height / 2);

      Label : constant String := "FX";
   begin

      Y := Bar_Bottom;

      if Selected then
         Screen.Draw_Line ((Bar_Left - 3, Y), (Bar_Left - 3, Y - Bar_Height));
         Screen.Draw_Line ((Bar_Left + Bar_Width + 2, Y),
                           (Bar_Left + Bar_Width + 2, Y - Bar_Height));
      end if;

      --  Short label
      X := Left;
      Print (X_Offset => X,
             Y_Offset => Value_Text_Y + 4,
             Str      => Label);

      X := Bar_Left - 1;
      Y := Bar_Top;
      case Value is
         when Bypass =>
            Screen.Copy_Bitmap (fx_bypass.Data, X, Y);
         when Overdrive =>
            Screen.Copy_Bitmap (fx_overdrive.Data, X, Y);
         when Filter =>
            Screen.Copy_Bitmap (fx_filter.Data, X, Y);
         when Bitcrusher =>
            Screen.Copy_Bitmap (fx_bitcrusher.Data, X, Y);
         when Reverb =>
            Screen.Copy_Bitmap (fx_reverb.Data, X, Y);
      end case;

   end Draw_FX;

   --------------------------
   -- Draw_CC_Control_Page --
   --------------------------

   procedure Draw_CC_Control_Page
     (Mode        : WNM.Project.Track_Mode_Kind;
      Selected    : WNM.Project.CC_Id;
      Val_A, Val_B, Val_C, Val_D : MIDI.MIDI_Data;
      Ena_A, Ena_B, Ena_C, Ena_D : Boolean := True)
   is
      use WNM.Project;

      First : Project.CC_Id;
   begin

      if Mode in Sample1_Mode | Sample2_Mode
        and then Selected = A
        and then Ena_A
      then
         First := B;

         declare
            use MIDI;
            use Sample_Library;
            Id : constant Natural :=
              (if Val_A > MIDI_Data (Valid_Sample_Index'Last - 1)
               then Valid_Sample_Index'Last
               else Natural (Val_A) + 1);
         begin
            Draw_Sample_Select
              (Sample_Library.Valid_Sample_Index (Id));
         end;

      elsif Mode = Speech_Mode
        and then Selected = A
        and then Ena_A
      then
         First := B;

         declare
            Id : constant Natural := Natural (Val_A);
         begin
            Draw_Word_Select (Speech.Word (Id));
         end;

      else

         for Id in CC_Id range First .. CC_Id'Last loop
            Draw_CC_Value
              (Id, (case Id is
                      when A => Val_A,
                      when B => Val_B,
                      when C => Val_C,
                      when D => Val_D),
               Project.CC_Controller_Short_Label (Editing_Track, Id),
               Id = Selected,
               Enabled => (case Id is
                             when A => Ena_A,
                             when B => Ena_B,
                             when C => Ena_C,
                             when D => Ena_D));
         end loop;

         Draw_Title
           (Project.CC_Controller_Label (Editing_Track, Selected),
            "");
      end if;
   end Draw_CC_Control_Page;

   ----------------------
   -- Draw_Filter_Mode --
   ----------------------

   procedure Draw_Filter_Mode (Id       : WNM.Project.CC_Id;
                               Mode     : Project.Filter_Mode_Kind;
                               Label    : String;
                               Selected : Boolean)
   is
      use WNM.Project;

      X, Y : Natural;

      Spacing : constant := 30;
      Left : constant Natural := Box_Left + 5 +
        (case Id is
            when WNM.Project.A => 0 * Spacing,
            when WNM.Project.B => 1 * Spacing,
            when WNM.Project.C => 2 * Spacing,
            when WNM.Project.D => 3 * Spacing);

      Sub_Label_Width : constant := (Bitmap_Fonts.Width * 3) - 1;
      Bar_Width : constant := 8;
      Bar_Height : constant := 128 / Bar_Width - 1;
      Bar_Left : constant Natural :=
        Left + (Sub_Label_Width - Bar_Width) / 2;
      Bar_Bottom : constant := Value_Text_Y + 2;
      Bar_Top    : constant := Bar_Bottom - Bar_Height;
      --  Bar_Center : constant Screen.Point :=
      --    (Bar_Left + Bar_Width / 2,
      --     Bar_Bottom - Bar_Height / 2);
   begin

      Y := Bar_Bottom;

      if Selected then
         Screen.Draw_Line ((Bar_Left - 3, Y), (Bar_Left - 3, Y - Bar_Height));
         Screen.Draw_Line ((Bar_Left + Bar_Width + 2, Y),
                           (Bar_Left + Bar_Width + 2, Y - Bar_Height));
      end if;

      --  Short label
      X := Left;
      Print (X_Offset => X,
             Y_Offset => Value_Text_Y + 4,
             Str      => Label);

      X := Bar_Left - 1;
      Y := Bar_Top;
      case Mode is
         when Project.Low_Pass =>
            Screen.Copy_Bitmap (filter_lp.Data, X, Y);
         when Project.High_Pass =>
            Screen.Copy_Bitmap (filter_hp.Data, X, Y);
         when Project.Band_Pass =>
            Screen.Copy_Bitmap (filter_bp.Data, X, Y);
      end case;

   end Draw_Filter_Mode;

   ---------------
   -- Draw_Knob --
   ---------------

   procedure Draw_Knob (Title : String;
                        Value : Natural)
   is
      pragma Unreferenced (Title, Value);
   begin
      Screen.Draw_Line ((Box_Left, Box_Center.Y),
                        (Box_Left + 100, Box_Center.Y));
      Screen.Draw_Circle (Box_Center, 15);
   end Draw_Knob;

   ------------------------
   -- Draw_Sample_Select --
   ------------------------

   procedure Draw_Sample_Select (Val : Sample_Library.Valid_Sample_Index) is
      use Sample_Library;

      X : Integer;

      function Display_Text (Val : Valid_Sample_Index) return String is
         Num : constant String := (if Val < 10 then " " else "") & Val'Img;
      begin
         return Num (Num'First + 1 .. Num'Last) & " " & Entry_Name (Val);
      end Display_Text;

   begin

      if Val /= Valid_Sample_Index'Last then
         X := Box_Left + 2;
         Print (X_Offset => X,
                Y_Offset => Value_Text_Y - 23,
                Str      => Display_Text (Val + 1));
      end if;

      X := Box_Left + 2;
      Print (X_Offset => X,
             Y_Offset => Value_Text_Y - 11,
             Str      => Display_Text (Val),
             Invert_From => Box_Left,
             Invert_To   => Box_Right);

      if Val /= Valid_Sample_Index'First then
         X := Box_Left + 2;
         Print (X_Offset => X,
                Y_Offset => Value_Text_Y + 1,
                Str      => Display_Text (Val - 1));
      end if;
   end Draw_Sample_Select;

   -------------------------
   -- Draw_Project_Select --
   -------------------------

   procedure Draw_Project_Select (Val : Project.Library.Valid_Prj_Index) is
      use Project.Library;

      X : Integer;

      function Display_Text (Val : Valid_Prj_Index) return String is
         Num : constant String := (if Val < 10 then " " else "") & Val'Img;
      begin
         return Num (Num'First + 1 .. Num'Last) & " " & Entry_Name (Val);
      end Display_Text;

   begin

      if Val /= Valid_Prj_Index'First then
         X := Box_Left + 2;
         Print (X_Offset => X,
                Y_Offset => Value_Text_Y - 23,
                Str      => Display_Text (Val - 1));
      end if;

      X := Box_Left + 2;
      Print (X_Offset => X,
             Y_Offset => Value_Text_Y - 11,
             Str      => Display_Text (Val),
             Invert_From => Box_Left,
             Invert_To   => Box_Right);

      if Val /= Valid_Prj_Index'Last then
         X := Box_Left + 2;
         Print (X_Offset => X,
                Y_Offset => Value_Text_Y + 1,
                Str      => Display_Text (Val + 1));
      end if;
   end Draw_Project_Select;

   ----------------------
   -- Draw_Word_Select --
   ----------------------

   procedure Draw_Word_Select (Word : Speech.Word) is
      use Speech;

      X : Integer;

      function Display_Text (Val : Speech.Word) return String is
      begin
         return Speech.Img (Val);
      end Display_Text;

      Left_Pos : constant Integer := Box_Left + 5;
   begin

      if Word /= Speech.Word'First then
         X := Left_Pos;
         Print (X_Offset => X,
                Y_Offset => Value_Text_Y - 23,
                Str      => Display_Text (Word - 1));
      end if;

      X := Left_Pos;
      Print (X_Offset => X,
             Y_Offset => Value_Text_Y - 11,
             Str      => Display_Text (Word),
             Invert_From => Box_Left,
             Invert_To   => Box_Right);

      if Word /= Speech.Word'Last then
         X := Left_Pos;
         Print (X_Offset => X,
                Y_Offset => Value_Text_Y + 1,
                Str      => Display_Text (Word + 1));
      end if;
   end Draw_Word_Select;

   ------------------------
   -- Draw_Step_Duration --
   ------------------------

   procedure Draw_Step_Duration (Pos      : Natural;
                                 D        : Duration_In_Steps;
                                 Selected : Boolean)
   is
      use WNM.Utils;

      Pt    : constant WNM.Screen.Point := (Pos, Value_Text_Y);
      Bar   : constant Natural := Natural (D) / 16;
      Beat  : constant Natural := (Natural (D) mod 16) / 4;
      Step  : constant Natural := Natural (D) mod 4;
      X_Str : Natural;

   begin
      --  BAR:BEAT:STEP
      X_Str := Pt.X;
      Print (X_Str, Pt.Y, Trim (Bar'Img));

      Set_Pixel (Pix_X (X_Str), Pix_Y (Pt.Y + 1));
      Set_Pixel (Pix_X (X_Str), Pix_Y (Pt.Y + 5));

      X_Str := @ + 2;
      Print (X_Str, Pt.Y, Trim (Beat'Img));

      Set_Pixel (Pix_X (X_Str), Pix_Y (Pt.Y + 1));
      Set_Pixel (Pix_X (X_Str), Pix_Y (Pt.Y + 5));

      X_Str := @ + 2;
      Print (X_Str, Pt.Y, Trim (Step'Img));

      if Selected then
         declare
            Len : constant Natural := (if Bar < 10
                                       then 20
                                       else 25);
         begin
            Screen.Draw_Line ((Pos, Select_Line_Y),
                              (Pos + Len, Select_Line_Y));
         end;
      end if;

      --  Commented below are alternative step duration drawings

      --  --  BAR and steps as dots in a 4x4 grid
      --  declare
      --     Cnt : Natural := Natural (D) mod 16;
      --  begin
      --     X_Str := Pt.X;
      --     Print (X_Str, Pt.Y, Trim (Natural'Image (D / 16)));
      --     for Y in 0 .. 3 loop
      --        for X in 0 .. 3 loop
      --           exit when Cnt = 0;
      --           Set_Pixel (Pix_X (X_Str + X * 2),
      --                      Pix_Y (Pt.Y + Y * 2));
      --           Cnt := @ - 1;
      --        end loop;
      --     end loop;
      --  end;
      --
      --  --  BAR and steps as dots in a 8x2 grid
      --  declare
      --     Cnt : Natural := Natural (D) mod 16;
      --  begin
      --     X_Str := Pt.X;
      --     Print (X_Str, Pt.Y, Trim (Natural'Image (D / 16)));
      --     Cnt := D mod 16;
      --     for Y in 0 .. 1 loop
      --        for X in 0 .. 7 loop
      --           exit when Cnt = 0;
      --           for H in 0 .. 2 loop
      --              Set_Pixel (Pix_X (X_Str + X * 2),
      --                         Pix_Y (Pt.Y + Y * 4 + H));
      --           end loop;
      --           Cnt := @ - 1;
      --        end loop;
      --     end loop;
      --  end;
   end Draw_Step_Duration;

   -------------------------
   -- Draw_Pattern_Length --
   -------------------------

   procedure Draw_Pattern_Length (Pos      : Natural;
                                  D        : Pattern_Length;
                                  Selected : Boolean)
   is
   begin
      Draw_Step_Duration (Pos, Duration_In_Steps (D), Selected);
   end Draw_Pattern_Length;

   -------------------
   -- Draw_Waveform --
   -------------------

   procedure Draw_Waveform (Top           : Integer;
                            Show_Cut      : Boolean := False;
                            Show_Playhead : Boolean := False)
   is
      use WNM.Sample_Recording;
      use WNM.Screen;

      Wave_Height : constant Integer := Integer (Waveform_Point'Last);
      Cut_Top : constant Integer := Top + Wave_Height;
      Wave_Left : constant Integer := cut_2.Data.W / 2;
      Wave_Mid  : constant Integer :=  Top + Wave_Height / 2;
      Start_Index : constant Waveform_Index := Start_Point_Index;
      End_Index : constant Waveform_Index := End_Point_Index;
      Play_Index : constant Waveform_Index := Last_Played_Point_Index;

      procedure Draw_Cut (X, Y : Integer) is
      begin
         --  Copy_Bitmap ((case Animation_Step mod 12 is
         --                  when 0 .. 6 => cut_1.Data,
         --                  when others => cut_2.Data),
         --               X            => X - cut_1.Data.W / 2,
         --               Y            => Y,
         --               Invert_Color => False);

         Copy_Bitmap (cut_2.Data,
                      X            => X - cut_2.Data.W / 2,
                      Y            => Y,
                      Invert_Color => True);

         --  Dot vertical line
         for V in Y - Integer (Waveform_Point'Last) .. Y - 1 loop
            Screen.Set_Pixel ((X, V), On => (V mod 2) = 0);
         end loop;
      end Draw_Cut;

      Wave : constant Waveform := Waveform_Data;
      X : Integer := Wave_Left;
   begin
      for Idx in Wave'Range loop
         Screen.Draw_Line ((X, Wave_Mid + Integer (Wave (Idx) / 2)),
                           (X, Wave_Mid - Integer (Wave (Idx) / 2)));
         X := X + 1;
      end loop;

      if Show_Cut then
         Draw_Cut (Wave_Left + Integer (Start_Index) - 1, Cut_Top);
         Draw_Cut (Wave_Left + Integer (End_Index) - 1, Cut_Top);
      end if;

      if Show_Playhead and then Play_Index /= Waveform_Index'First then
         Screen.Draw_Line
           ((Wave_Left + Integer (Play_Index) - 1, Cut_Top),
            (Wave_Left + Integer (Play_Index) - 1, Cut_Top - Wave_Height));
      end if;
   end Draw_Waveform;

   --------------------
   -- Draw_Magic_Hat --
   --------------------

   procedure Draw_Magic_Hat (X, Y : Integer;
                             Animate : Boolean;
                             Step    : HAL.UInt32)
   is
      use WNM.Screen;
      use type HAL.UInt32;

      Step_Per_Frames : constant := 3;
      Frame_Count : constant HAL.UInt32 := Step_Per_Frames * 3;
      Still_Frame : constant HAL.UInt32 := 0;
      Frame : constant HAL.UInt32 := (if Animate
                                      then Step mod Frame_Count
                                      else Still_Frame);

      BX : constant Natural := X + (Natural (Step) mod 2);
      BY : constant Natural := Y + (Natural (Step) mod 2);

      F1_S : constant := 0;
      F1_E : constant := F1_S + Step_Per_Frames;
      F2_S : constant := F1_E + 1;
      F2_E : constant := F2_S + Step_Per_Frames;
      F3_S : constant := F2_E + 1;
      F3_E : constant := F3_S + Step_Per_Frames;

   begin
      case Frame is
         when F1_S .. F1_E =>
            Copy_Bitmap (magic_hat_1.Data, BX, BY);
         when F2_S .. F2_E =>
            Copy_Bitmap (magic_hat_2.Data, BX, BY);
         when F3_S .. F3_E =>
            Copy_Bitmap (magic_hat_3.Data, BX, BY);
         when others => null;
      end case;

      --  Random blinking stars
      if Animate then
         for Stars in 1 .. Random mod 4 loop
            declare
               SX : constant Natural := X + Natural (Random mod 25);
               SY : constant Natural := Y + Natural (Random mod 25);
            begin
               if (Random mod 2) = 0 then
                  Screen.Set_Pixel ((SX, SY));
               else
                  Screen.Set_Pixel ((SX + 0, SY + 0));
                  Screen.Set_Pixel ((SX + 1, SY + 0));
                  Screen.Set_Pixel ((SX - 1, SY + 0));
                  Screen.Set_Pixel ((SX + 0, SY + 1));
                  Screen.Set_Pixel ((SX + 0, SY - 1));
               end if;
            end;
         end loop;
      end if;
   end Draw_Magic_Hat;

   ------------------
   -- Draw_Battery --
   ------------------

   procedure Draw_Battery (Step : HAL.UInt32) is
      use HAL;

      X : constant Integer := Screen_Width - battery_100.Data.W - 1;
      Y : constant Integer := 0;

      Blink_Rate_0 : constant := 10;
      Blink_Rate_10 : constant := 15;
      Blink_Rate_20 : constant := 30;

      Millivolts : constant Natural := WNM_HAL.Battery_Millivolts;
   begin
      if Millivolts <= 2500 then

         if Step mod Blink_Rate_0 < Blink_Rate_0 / 2 then -- Blink
            Screen.Copy_Bitmap (battery_0.Data, X, Y);
         end if;

      elsif Millivolts <= 3000 then
         if Step mod Blink_Rate_10 < Blink_Rate_10 / 2 then -- Blink
            Screen.Copy_Bitmap (battery_10.Data, X, Y);
         end if;

      elsif Millivolts <= 3200 then
         if Step mod Blink_Rate_20 < Blink_Rate_20 / 2 then -- Blink
            Screen.Copy_Bitmap (battery_20.Data, X, Y);
         end if;

      elsif Millivolts <= 3220 then
         Screen.Copy_Bitmap (battery_30.Data, X, Y);
      elsif Millivolts <= 3250 then
         Screen.Copy_Bitmap (battery_40.Data, X, Y);
      elsif Millivolts <= 3300 then
         Screen.Copy_Bitmap (battery_50.Data, X, Y);
      elsif Millivolts <= 3400 then
         Screen.Copy_Bitmap (battery_60.Data, X, Y);
      elsif Millivolts <= 3500 then
         Screen.Copy_Bitmap (battery_70.Data, X, Y);
      elsif Millivolts <= 3600 then
         Screen.Copy_Bitmap (battery_80.Data, X, Y);
      elsif Millivolts <= 3700 then
         Screen.Copy_Bitmap (battery_90.Data, X, Y);
      else
         Screen.Copy_Bitmap (battery_100.Data, X, Y);
      end if;
   end Draw_Battery;

end WNM.GUI.Menu.Drawing;
