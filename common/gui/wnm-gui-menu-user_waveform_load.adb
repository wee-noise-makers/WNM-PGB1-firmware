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

with Interfaces; use Interfaces;

with WNM.Synth;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.GUI.Bitmap_Fonts;
with WNM.GUI.Popup;

with Tresses; use Tresses;
with Tresses.Resources; use Tresses.Resources;

package body WNM.GUI.Menu.User_Waveform_Load is

   type Wave_Ref is not null access constant Tresses.Resources.Table_257_S16;

   Singleton : aliased Instance;

   function Menu_Item_Text (Item : Menu_Items) return String
   is (case Item is
          when WAV_Sine                     => "Sine",
          when WAV_Sine_Warp1               => "Sine Warp 1",
          when WAV_Sine_Warp2               => "Sine Warp 2",
          when WAV_Sine_Warp3               => "Sine Warp 3",
          when WAV_Sine2_Warp1              => "2 Sine Warp 1",
          when WAV_Sine2_Warp2              => "2 Sine Warp 2",
          when WAV_Sine2_Warp3              => "2 Sine Warp 3",
          when WAV_Sawtooth                 => "Sawtooth",
          when WAV_Screech                  => "Screech",
          when WAV_Triangle                 => "Triangle",
          when WAV_Chip_Triangle            => "Chiptune Triangle",
          when WAV_Chip_Pulse_50            => "Chiptune Square 50%",
          when WAV_Chip_Pulse_25            => "Chiptune Square 25%",
          when WAV_Combined_Sin_Saw         => "Sine + Saw",
          when WAV_Combined_Saw_Sin         => "Saw + Half Sine",
          when WAV_Combined_Trig_Sin        => "Triangle + Half Sine",
          when WAV_Combined_Sin_Square      => "Half Sine + Square",
          when WAV_Combined_Square_Sin      => "Square + Half Sine",
          when WAV_Combined_Square_Full_Sin => "Square + Full Sine",
          when WAV_Bandlimited_Comb         => "Comb");

   pragma Style_Checks ("M100");
   function Wave (Item : Menu_Items) return Wave_Ref
   is (case Item is
          when WAV_Sine                     => Resources.WAV_Sine'Access,
          when WAV_Sine_Warp1               => Resources.WAV_Sine_Warp1'Access,
          when WAV_Sine_Warp2               => Resources.WAV_Sine_Warp2'Access,
          when WAV_Sine_Warp3               => Resources.WAV_Sine_Warp3'Access,
          when WAV_Sine2_Warp1              => Resources.WAV_Sine2_Warp1'Access,
          when WAV_Sine2_Warp2              => Resources.WAV_Sine2_Warp2'Access,
          when WAV_Sine2_Warp3              => Resources.WAV_Sine2_Warp3'Access,
          when WAV_Sawtooth                 => Resources.WAV_Sawtooth'Access,
          when WAV_Screech                  => Resources.WAV_Screech'Access,
          when WAV_Triangle                 => Resources.WAV_Triangle'Access,
          when WAV_Chip_Triangle            => Resources.WAV_Chip_Triangle'Access,
          when WAV_Chip_Pulse_50            => Resources.WAV_Chip_Pulse_50'Access,
          when WAV_Chip_Pulse_25            => Resources.WAV_Chip_Pulse_25'Access,
          when WAV_Combined_Sin_Saw         => Resources.WAV_Combined_Sin_Saw'Access,
          when WAV_Combined_Saw_Sin         => Resources.WAV_Combined_Saw_Sin'Access,
          when WAV_Combined_Trig_Sin        => Resources.WAV_Combined_Trig_Sin'Access,
          when WAV_Combined_Sin_Square      => Resources.WAV_Combined_Sin_Square'Access,
          when WAV_Combined_Square_Sin      => Resources.WAV_Combined_Square_Sin'Access,
          when WAV_Combined_Square_Full_Sin => Resources.WAV_Combined_Square_Full_Sin'Access,
          when WAV_Bandlimited_Comb         => Resources.WAV_Bandlimited_Comb_10'Access);

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Singleton'Access);
   end Push_Window;

   -----------------------
   -- Draw_Wave_Preview --
   -----------------------

   procedure Draw_Wave_Preview (Wave : Wave_Ref) is
      Wave_Height : constant := Box_Height - Font_Height - 8;
      H_Step : constant := 65_535 / Wave_Height;
      Y_Base : constant := Box_Top + Wave_Height + 3;

      function Cursor_To_Wave_Index (X : Pix_X) return U16
      is (U16 (X) * 4);

      function Frame_To_Height (F : S16) return Natural is
         Ret : constant Integer := Integer (F) - Integer (S16'First);
      begin
         return Ret / H_Step;
      end Frame_To_Height;

      Width : constant := Tresses.Resources.Table_257_S16'Length / 4;
      X_Base : constant := Box_Left + (Box_Width - Width) / 2;
   begin
      for X in Pix_X'First .. Pix_X'Last / 2 loop
         declare
            Wave_Idx : constant U16 := Cursor_To_Wave_Index (X);
            Point : constant S16 := Wave (Wave_Idx);
            Y : constant Pix_Y :=
              Pix_Y (Y_Base - Frame_To_Height (Point));
         begin
            Set_Pixel (X_Base + X, Y);
         end;
      end loop;

   end Draw_Wave_Preview;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This   : in out Instance) is

   begin
      Draw_Menu_Box ("Menu",
                     Count => Menu_Items_Count,
                     Index => Menu_Items'Pos (This.Item));

      Draw_Wave_Preview (Wave (This.Item));

      Draw_Str_Center (Box_Bottom - Bitmap_Fonts.Height - 1,
                       Menu_Item_Text (This.Item));
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Instance;
                       Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Right_Press =>
            if This.Item /= Menu_Items'Last then
               This.Item := Menu_Items'Succ (This.Item);
            end if;

         when Left_Press =>
            if This.Item /= Menu_Items'First then
               This.Item := Menu_Items'Pred (This.Item);
            end if;

         when A_Press =>
            Synth.User_Waveform := Wave (This.Item).all;
            WNM.GUI.Popup.Display ("     Loaded    ", 500_000);
         when B_Press =>
            Menu.Pop (Success);
         when others =>
            null;
      end case;
   end On_Event;

end WNM.GUI.Menu.User_Waveform_Load;
