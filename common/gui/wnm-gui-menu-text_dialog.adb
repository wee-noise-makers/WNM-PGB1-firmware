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
with WNM.GUI.Menu.Drawing;
with WNM.Screen;

package body WNM.GUI.Menu.Text_Dialog is

   Singleton  : aliased Text_Dialog_Window;

   procedure Set_Value (Str : String);

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Singleton.Reset_On_Push := True;
      Push (Singleton'Access);
   end Push_Window;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window (Str : String) is
   begin
      Set_Value (Str);
      Singleton.Reset_On_Push := False;
      Push (Singleton'Access);
   end Push_Window;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Title : String) is
      Dialog_Title : String renames Singleton.Dialog_Title;
   begin
      if Title'Length <= Title_Max_Len then

         Dialog_Title
           (Dialog_Title'First .. Dialog_Title'First + Title'Length - 1) :=
           Title;

         Dialog_Title
           (Dialog_Title'First + Title'Length .. Dialog_Title'Last) :=
           (others => ' ');
      end if;
   end Set_Title;

   -----------
   -- Value --
   -----------

   function Value return String
   is (Singleton.To_String);

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Str : String) is
      Len : constant Natural :=
        Natural'Min (Str'Length, Singleton.Text'Length);
   begin
      Singleton.Len := Len;

      for X in 0 .. Len - 1 loop
         Singleton.Text (Singleton.Text'First + X)
           := To_Valid_Char (Str (Str'First + X));
      end loop;
      Singleton.Cursor := Len;
   end Set_Value;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Text_Dialog_Window)
   is
      package Font renames Bitmap_Fonts;

      X : Integer := Drawing.Box_Left + 2 * Font.Width;

      Input_Top : constant Integer :=
        Drawing.Box_Bottom - 3 * Font.Height - 4;

      Input_Botton : constant Integer := Input_Top + Font.Height;

      Input_Left : constant Integer :=
        X + (This.Cursor - This.Text'First) * Font.Width;

      Info_Top : constant Integer := Input_Top + 3 * Font.Height - 5;
   begin
      Drawing.Draw_Menu_Box (This.Dialog_Title, 0, 0);

      Print (X_Offset    => X,
             Y_Offset    => Input_Top,
             Str         => This.To_String);

      --  Bottom chevron
      Screen.Set_Pixel ((Input_Left + 0, Input_Botton + 2));
      Screen.Set_Pixel ((Input_Left + 1, Input_Botton + 1));
      Screen.Set_Pixel ((Input_Left + 2, Input_Botton + 0));
      Screen.Set_Pixel ((Input_Left + 3, Input_Botton + 1));
      Screen.Set_Pixel ((Input_Left + 4, Input_Botton + 2));

      --  Top chevron
      Screen.Set_Pixel ((Input_Left + 0, Input_Top - 4));
      Screen.Set_Pixel ((Input_Left + 1, Input_Top - 3));
      Screen.Set_Pixel ((Input_Left + 2, Input_Top - 2));
      Screen.Set_Pixel ((Input_Left + 3, Input_Top - 3));
      Screen.Set_Pixel ((Input_Left + 4, Input_Top - 4));

      --  Info text
      X := Drawing.Box_Left + 3;
      Print (X_Offset => X,
             Y_Offset => Info_Top,
             Str      => "B:Delete   A:Enter");
   end Draw;

   -----------------
   -- Delete_Char --
   -----------------

   procedure Delete_Char (This  : in out Text_Dialog_Window) is
      Last : constant Integer := This.Text'First + This.Len - 1;
   begin
      if This.Len > 1 then
         if This.Cursor = Last then
            This.Cursor := This.Cursor - 1;
         else
            for X in This.Cursor .. Last - 1 loop
               This.Text (X) := This.Text (X + 1);
            end loop;
         end if;

         This.Len := This.Len - 1;
      else

         --  Trying to delete the last char means exit
         Menu.Pop (Exit_Value => Failure);
      end if;
   end Delete_Char;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Text_Dialog_Window;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when A_Press =>
            Menu.Pop (Exit_Value => Success);
         when B_Press =>
            Delete_Char (This);

         when Right_Press =>
            --  Move cursor and increase length if necessary
            if This.Cursor < This.Text'First + This.Len - 1 then
               This.Cursor := This.Cursor + 1;
            elsif This.Cursor /= This.Text'Last then
               This.Cursor := This.Cursor + 1;
               This.Len := This.Len + 1;
               This.Text (This.Cursor) := This.Text (This.Cursor - 1);
            end if;

         when Left_Press =>

            if This.Cursor > This.Text'First then
               This.Cursor := This.Cursor - 1;
            end if;

         when Up_Press =>
            declare
               C : Valid_Character renames This.Text (This.Cursor);
            begin
               if C > Valid_Character'First then
                  C := Valid_Character'Pred (C);
               else
                  C := Valid_Character'Last;
               end if;
            end;

         when Down_Press =>
            declare
               C : Valid_Character renames This.Text (This.Cursor);
            begin
               if C < Valid_Character'Last then
                  C := Valid_Character'Succ (C);
               else
                  C := Valid_Character'First;
               end if;
            end;
         when Slider_Touch =>
            null;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Text_Dialog_Window)
   is
   begin
      if This.Reset_On_Push then
         This.Len := 1;
         This.Cursor := This.Text'First;
         This.Text (This.Text'First) := 'A';
      end if;
   end On_Pushed;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Text_Dialog_Window) return String is
      First : constant Integer := Singleton.Text'First;
      Last  : constant Integer := First + Singleton.Len - 1;
      Result : String (First .. Last);
   begin
      for X in Result'Range loop
         Result (X) := To_Char (Singleton.Text (X));
      end loop;
      return Result;
   end To_String;

end WNM.GUI.Menu.Text_Dialog;
