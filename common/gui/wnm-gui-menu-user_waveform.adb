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

with HAL; use HAL;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Screen;
with WNM.GUI.Bitmap_Fonts;
with WNM.GUI.Menu.User_Waveform_Drawing;
with WNM.GUI.Menu.User_Waveform_Load;

with edit_wave_icon;
with edit_wave_icon_2;
with edit_wave_icon_3;
with load_wave_icon;
with load_wave_icon_2;
with load_wave_icon_3;

package body WNM.GUI.Menu.User_Waveform is

   Singleton : aliased Instance;

   function Menu_Item_Text (Item : Menu_Items) return String
   is (case Item is
          when Load_Wave => "Load",
          when Edit_Wave => "Edit / Draw");

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

   Anim_Step : HAL.UInt32 := 0;

   overriding
   procedure Draw (This   : in out Instance) is
      Step : constant HAL.UInt32 := Anim_Step / 6;

      Icon_W : constant Natural := load_wave_icon.Data.W;
      Icon_H : constant Natural := load_wave_icon.Data.H;
      Icon_Left : constant Natural :=
        Box_Center.X - (Icon_W / 2);

      Icon_Top : constant Natural :=
        Box_Top + 2;
   begin
      Draw_Menu_Box ("Menu",
                     Count => Menu_Items_Count,
                     Index => Menu_Items'Pos (This.Item));

      WNM.Screen.Copy_Bitmap
        ((case This.Item is
            when Load_Wave =>
           (case Step  mod 3 is
               when 0      => load_wave_icon.Data,
               when 1      => load_wave_icon_2.Data,
               when others => load_wave_icon_3.Data),

            when Edit_Wave =>
           (case Step mod 3 is
               when 0      => edit_wave_icon.Data,
               when 1      => edit_wave_icon_2.Data,
               when others => edit_wave_icon_3.Data)),
         Icon_Left,
         Icon_Top);

      Anim_Step := Anim_Step + 1;

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
            case This.Item is
               when Edit_Wave =>
                  User_Waveform_Drawing.Push_Window;

               when Load_Wave =>
                  User_Waveform_Load.Push_Window;
            end case;

         when B_Press =>
            Menu.Pop (Success);
         when others =>
            null;
      end case;
   end On_Event;

end WNM.GUI.Menu.User_Waveform;
