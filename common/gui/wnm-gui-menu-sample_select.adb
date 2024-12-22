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

with WNM.Utils;

with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.GUI.Bitmap_Fonts; use WNM.GUI.Bitmap_Fonts;

--------------------------------
-- WNM.GUI.Menu.Sample_Select --
--------------------------------

package body WNM.GUI.Menu.Sample_Select is

   Sample_Select : aliased Sample_Select_Window;
   Dialog_Title : String (1 .. Title_Max_Len) := (others => ' ');

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window (Title : String) is
   begin
      WNM.Utils.Copy_Str (Title, Dialog_Title);
      Push (Sample_Select'Access);
   end Push_Window;

   --------------
   -- Selected --
   --------------

   function Selected return Sample_Index
   is (Sample_Select.Index);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Sample_Select_Window)
   is
      X : Integer := 5;
   begin
      Print (X_Offset    => X,
             Y_Offset    => Drawing.Box_Top - 5,
             Str         => Dialog_Title);

      Draw_Sample_Select ((Sample => This.Index, Slice => 0));
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Sample_Select_Window;
      Event :        Menu_Event)
   is
   begin
      case Event.Kind is
         when A_Press =>
            Menu.Pop (Exit_Value => Success);
         when B_Press =>
            Menu.Pop (Exit_Value => Failure);
         when Up_Press =>
            if This.Index /= Sample_Index'Last then
               This.Index := This.Index + 1;
            end if;
         when Down_Press =>
            if This.Index /= Sample_Index'First then
               This.Index := This.Index - 1;
            end if;
         when others => null;
      end case;
   end On_Event;

end WNM.GUI.Menu.Sample_Select;
