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

with HAL.Bitmap;           use HAL.Bitmap;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Synth;
with WNM.Sequencer;

package body WNM.GUI.Menu.Sample_Select is

   Sample_Select : aliased Sample_Select_Window;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
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
   begin
      Draw_Sample_Select (This.Index);
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Sample_Select_Window;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            Menu.Pop (Exit_Value => Success);
         when Right_Press =>
            Menu.Pop (Exit_Value => Failure);
         when Encoder_Right =>
            if Event.Value > 0 then
               if This.Index /= Valid_Sample_Index'Last then
                  This.Index := This.Index + 1;
               end if;
            elsif Event.Value < 0 then
               if This.Index /= Valid_Sample_Index'First then
                  This.Index := This.Index - 1;
               end if;
            end if;
         when Encoder_Left =>
            null;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding procedure On_Pushed
     (This  : in out Sample_Select_Window)
   is
   begin
      This.Index := Valid_Sample_Index'First;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding procedure On_Focus
     (This       : in out Sample_Select_Window;
      Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Sample_Select;
