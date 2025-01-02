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

with WNM.Utils;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.GUI.Bitmap_Fonts;
with WNM.Screen;

with dialog_yes;
with dialog_no;

package body WNM.GUI.Menu.Yes_No_Dialog is

   Yes_No_Dialog  : aliased Yes_No_Dialog_Window;
   Dialog_Title : String (1 .. Title_Max_Len) := (others => ' ');

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Yes_No_Dialog'Access);
   end Push_Window;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Title : String) is
   begin
      WNM.Utils.Copy_Str (Title, Dialog_Title);
   end Set_Title;

   ----------------
   -- Trim_Title --
   ----------------

   function Trim_Title return String is
      Count : Natural := 0;
   begin
      for Elt of reverse Dialog_Title loop
         exit when Elt /= ' ';
         Count := Count + 1;
      end loop;
      return Dialog_Title (Dialog_Title'First .. Dialog_Title'Last - Count);
   end Trim_Title;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Yes_No_Dialog_Window)
   is
      Space : constant := 6;
      Yes_X  : constant Natural := Box_Center.X + Space;
      No_X : constant Natural :=
        Box_Center.X - Space
          - dialog_yes.Data.W
          - Bitmap_Fonts.Width * 3;

      Y : constant Natural := Box_Bottom - 19;

      X : Natural;
   begin
      Drawing.Draw_Menu_Box ("Confirm", 0, 0);

      Draw_Str_Center (Box_Top + 4, Trim_Title);

      Screen.Copy_Bitmap (dialog_yes.Data, Yes_X, Y,
                          Invert_Color => This.Yes);
      if This.Yes then
         X := Yes_X + dialog_yes.Data.W + 3;
         Bitmap_Fonts.Print
           (X,
            Y + dialog_yes.Data.W / 2 - Bitmap_Fonts.Height / 2,
            "YES");

         Screen.Draw_Rect (((Yes_X, Y),
                           dialog_yes.Data.W + Bitmap_Fonts.Width * 4,
                           dialog_yes.Data.H));

      end if;

      Screen.Copy_Bitmap (dialog_no.Data, No_X, Y,
                         Invert_Color => not This.Yes);
      if not This.Yes then
         X := No_X + dialog_no.Data.W + 3 + Bitmap_Fonts.Width / 2;
         Bitmap_Fonts.Print
           (X,
            Y + dialog_no.Data.W / 2 - Bitmap_Fonts.Height / 2,
            "NO");

         Screen.Draw_Rect (((No_X, Y),
                           dialog_no.Data.W + Bitmap_Fonts.Width * 4,
                           dialog_no.Data.H));

      end if;

   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Yes_No_Dialog_Window;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when A_Press =>
            if This.Yes then
               Menu.Pop (Exit_Value => Success);
            else
               Menu.Pop (Exit_Value => Failure);
            end if;
         when B_Press =>
            Menu.Pop (Exit_Value => Failure);
         when Right_Press | Left_Press =>
            This.Yes := not This.Yes;
         when others =>
            null;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Yes_No_Dialog_Window)
   is
   begin
      This.Yes := False;
   end On_Pushed;

end WNM.GUI.Menu.Yes_No_Dialog;
