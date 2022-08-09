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

with WNM.GUI.Bitmap_Fonts;     use WNM.GUI.Bitmap_Fonts;
with WNM.Sample_Stream;
with WNM.Screen;
with WNM.Sample_Library; use WNM.Sample_Library;
with WNM.Synth;

with WNM.Sample_Edit;
with WNM.GUI.Menu.Drawing;

package body WNM.GUI.Menu.Sample_Trim is

   Sample_Trim_Singleton : aliased Trim_Window;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Sample_Trim_Singleton'Access);
   end Push_Window;

   --------------------
   -- Preview_Sample --
   --------------------

   procedure Preview_Sample (This : Trim_Window) is
   begin
      null;
   end Preview_Sample;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Trim_Window)
   is
   begin
      WNM.GUI.Menu.Drawing.Draw_Waveform;
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Trim_Window;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            Pop (Exit_Value => Success);
         when Right_Press =>
            This.Preview_Sample;
         when Encoder_Right =>
            if Event.Value > 0 then
               WNM.Sample_Edit.Inc_Stop;
            elsif Event.Value < 0 then
               WNM.Sample_Edit.Dec_Stop;
            end if;

            WNM.Sample_Edit.Update_Waveform;
            This.Preview_Sample;

         when Encoder_Left =>

            if Event.Value > 0 then
               WNM.Sample_Edit.Inc_Start;
            elsif Event.Value < 0 then
               WNM.Sample_Edit.Dec_Start;
            end if;

            WNM.Sample_Edit.Update_Waveform;
            This.Preview_Sample;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed (This : in out Trim_Window) is
   begin
      WNM.Sample_Edit.Update_Waveform;
   end On_Pushed;

end WNM.GUI.Menu.Sample_Trim;
