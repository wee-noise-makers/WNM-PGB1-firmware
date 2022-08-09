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
with HAL.Bitmap;           use HAL.Bitmap;

package body WNM.GUI.Menu.Text_Dialog is

   Text_Dialog  : aliased Text_Dialog_Window;
   Dialog_Title : String (1 .. Title_Max_Len) := (others => ' ');

   procedure Set_Value (Str : String);

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Text_Dialog.Reset_On_Push := True;
      Push (Text_Dialog'Access);
   end Push_Window;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window (Str : String) is
   begin
      Set_Value (Str);
      Text_Dialog.Reset_On_Push := False;
      Push (Text_Dialog'Access);
   end Push_Window;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Title : String) is
   begin
      if Title'Length <= Title_Max_Len then
         Dialog_Title (Dialog_Title'First .. Dialog_Title'First + Title'Length - 1) := Title;
         Dialog_Title (Dialog_Title'First + Title'Length .. Dialog_Title'Last) := (others => ' ');
      end if;
   end Set_Title;


   -----------
   -- Value --
   -----------

   function Value return String
   is (Text_Dialog.Text (Text_Dialog.Text'First .. Text_Dialog.Text'First + Text_Dialog.Len - 1));

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Str : String) is
      Len : Natural := Natural'Min (Str'Length, Text_Dialog.Text'Length);
   begin
      Text_Dialog.Len := Len;
      Text_Dialog.Text (1 .. Len - 1) := Str (Str'First .. Str'First + Len - 1);
      Text_Dialog.Index := Len;
   end Set_Value;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Text_Dialog_Window)
   is
      X        : Integer := 1;
      Select_X : constant Integer := X + (This.Index - This.Text'First) * 6;
   begin
      Print (X_Offset    => X,
             Y_Offset    => 8,
             Str         => Dialog_Title);

      X := 1;
      Print (X_Offset    => X,
             Y_Offset    => 9 + 8,
             Str         => This.Text (This.Text'First .. This.Text'First + This.Len - 1),
             Invert_From => (Select_X - 1),
             Invert_To   => (Select_X + 5));
   end Draw;

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
         when Left_Press =>
            Menu.Pop (Exit_Value => Success);
         when Right_Press =>

            if This.Len > 1 then
               if This.Index = This.Text'First + This.Len - 1 then
                  This.Index := This.Index - 1;
               end if;
               This.Len := This.Len - 1;
            else

               --  Trying to delete the last char means exit
               Menu.Pop (Exit_Value => Failure);
            end if;

         when Encoder_Right =>

            --  Move cursor and increase length if necessary

            if Event.Value > 0 then
               if This.Index < This.Text'First + This.Len - 1 then
                  This.Index := This.Index + 1;
               elsif This.Index /= This.Text'Last then
                  This.Index := This.Index + 1;
                  This.Len   := This.Len + 1;
                  This.Text (This.Index) := 'A';
               end if;
            elsif Event.Value < 0 then
               if This.Index > This.Text'First then
                  This.Index := This.Index - 1;
               end if;
            end if;
         when Encoder_Left =>
            if Event.Value > 0 then
               This.Text (This.Index) := Character'Succ (This.Text (This.Index));
            elsif Event.Value < 0 then
               This.Text (This.Index) := Character'Pred (This.Text (This.Index));
            end if;
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
         This.Len   := 1;
         This.Index := This.Text'First;
         This.Text (This.Text'First) := 'A';
      end if;
   end On_Pushed;

end WNM.GUI.Menu.Text_Dialog;
