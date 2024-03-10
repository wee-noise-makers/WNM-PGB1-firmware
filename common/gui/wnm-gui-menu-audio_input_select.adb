-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2024 Fabien Chouteau                  --
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

with WNM.Mixer;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;

package body WNM.GUI.Menu.Audio_Input_Select is

   package Sample_Rec_Inputs_Next
   is new Enum_Next (WNM.Mixer.Sample_Rec_Inputs);
   use Sample_Rec_Inputs_Next;

   Input_Select : aliased Input_Select_Window;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Input_Select'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Input_Select_Window)
   is
   begin
      Draw_Title ("Input Select", WNM.Mixer.Sample_Rec_Input'Img);
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Input_Select_Window;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when A_Press =>
            Menu.Pop (Exit_Value => Success);
         when B_Press =>
            Menu.Pop (Exit_Value => Failure);
         when Up_Press =>
            Next (WNM.Mixer.Sample_Rec_Input);
         when Down_Press =>
            Prev (WNM.Mixer.Sample_Rec_Input);
         when others =>
            null;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Input_Select_Window)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding procedure On_Focus
     (This       : in out Input_Select_Window;
      Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Audio_Input_Select;
