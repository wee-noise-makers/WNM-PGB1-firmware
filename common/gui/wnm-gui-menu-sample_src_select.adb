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

with WNM.Synth;            use WNM.Synth;
with WNM.Audio;
with WNM.GUI.Bitmap_Fonts;   use WNM.GUI.Bitmap_Fonts;
with Enum_Next;

package body WNM.GUI.Menu.Sample_Src_Select is

   package Rec_Src_Enum_Next is new Enum_Next (Rec_Src);
   use Rec_Src_Enum_Next;

   Src_Select_Window_Singleton : aliased Src_Select_Menu;

   procedure Set_Passthrough (Src : Rec_Src) is
   begin
      case Src is
         when Line_In =>
            WNM.Synth.Set_Passthrough (Audio.Line_In);
         --  when FM =>
         --     WNM.Synth.Set_Passthrough (Audio.FM);
         when Master_Output =>
            WNM.Synth.Set_Passthrough (Audio.None);
      end case;
   end Set_Passthrough;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Src_Select_Window_Singleton'Access);
   end Push_Window;

   ---------
   -- Src --
   ---------

   function Src return WNM.Synth.Rec_Source
   is (Src_Select_Window_Singleton.Src);

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (This : in out Src_Select_Menu)
   is
      X : Integer := 5;
   begin
      Print (X_Offset    => X,
             Y_Offset    => 0 + 8,
             Str         => "Source:");

      X := 5;
      Print (X_Offset    => X,
             Y_Offset    => 9 + 8,
             Str         => This.Src'Img);

      --  Print_Percentage (Down, "Volume", This.Volume);
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (This  : in out Src_Select_Menu;
      Event : Menu_Event)
   is
      Tmp : Integer;
   begin
      case Event.Kind is
         when Left_Press =>
            Menu.Pop (Exit_Value => Success);
         when Right_Press =>
            Menu.Pop (Exit_Value => Failure);
         when Encoder_Right =>
            Tmp := This.Volume + Event.Value;
            if Tmp < 0 then
               This.Volume := 0;
            elsif Tmp > 100 then
               This.Volume := 100;
            else
               This.Volume := Natural (Tmp);
            end if;
         when Encoder_Left =>
            if Event.Value > 0 then
               This.Src := Next (This.Src);
               Set_Passthrough (This.Src);
            elsif Event.Value < 0 then
               This.Src := Prev (This.Src);
               Set_Passthrough (This.Src);
            end if;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding procedure On_Pushed
     (This  : in out Src_Select_Menu)
   is
   begin
      Set_Passthrough (This.Src);
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding procedure On_Focus
     (This       : in out Src_Select_Menu;
      Exit_Value : Window_Exit_Value)
   is
      pragma Unreferenced (This);
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Sample_Src_Select;
