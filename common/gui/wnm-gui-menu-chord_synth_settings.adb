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

with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;

package body WNM.GUI.Menu.Chord_Synth_Settings is

   T   : constant Tracks := 6;

   package Sub_Settings_Next is new Enum_Next (Sub_Settings,
                                               Wrap => False);
   use Sub_Settings_Next;

   Track_Settings_Singleton : aliased Track_Settings_Menu;

   ------------
   -- To_Top --
   ------------

   function To_Top (S : Sub_Settings) return Top_Settings
   is (case S is
       when Project.Engine => Engine,
       when Project.Volume => Volume,
       when Project.Pan => Pan,
       when Project.Master_FX => Master_FX,
       when Project.Track_Octave_Offset => Octave_Offset,
       when Project.Shuffle => Shuffle,
       when Project.LFO_Rate => LFO,
       when Project.LFO_Amplitude => LFO,
       when Project.LFO_Shape => LFO,
       when Project.LFO_Target => LFO,
       when Project.Arp_Mode => Arp_Mode,
       when Project.Arp_Notes => Arp_Notes,
       when Project.CC_Default_A => CC_Default,
       when Project.CC_Default_B => CC_Default,
       when Project.CC_Default_C => CC_Default,
       when Project.CC_Default_D => CC_Default);

   --------------
   -- To_CC_Id --
   --------------

   function To_CC_Id (S : Sub_Settings) return Project.CC_Id
   is (case S is
          when CC_Default_A => A,
          when CC_Default_B => B,
          when CC_Default_C => C,
          when CC_Default_D => D,
          when others => raise Program_Error);

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Track_Settings_Singleton'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Track_Settings_Menu)
   is
      Sub : constant Sub_Settings := This.Current_Setting;
      Top : constant Top_Settings := To_Top (Sub);
   begin

      Draw_Menu_Box ("Chord synth",
                     Count => Top_Settings_Count,
                     Index => Top_Settings'Pos (Top));
      case Top is
         when Volume =>
            Draw_Volume ("Volume:", Project.Track_Volume (T));

         when Pan =>
            Draw_Pan ("Pan:", Project.Track_Pan (T));

         when Master_FX =>
            Draw_Title ("FX send:", "");
            Draw_Value (Img (Project.Master_FX (T)));

            Draw_FX (Id => WNM.Project.A,
                     Value => Project.Master_FX (T),
                     Selected => False,
                     Label => "");

         when Octave_Offset =>
            Draw_Title ("Octave offset:", "");
            Draw_Value (Project.Track_Offset (T)'Img);

         when Shuffle =>
            Draw_Title ("Shuffle:", "");
            declare
               Shuffle : constant Project.Shuffle_Value :=
                 Project.Track_Shuffle (T);

               Linn : constant Integer :=
                 50 + Integer (Shuffle) / 2;

               Str : constant String :=
                 Linn'Img & (if Shuffle mod 2 = 1 then ".5" else ".0");
            begin
               Draw_Value (Str);
            end;

         when Arp_Mode =>
            Draw_Title ("Arpeggiator mode:", "");
            Draw_Value (Project.Img (Project.Arp_Mode (T)));

         when Arp_Notes =>
            Draw_Title ("Arpeggiator notes:", "");
            Draw_Value (Project.Img (Project.Arp_Notes (T)));

         when Engine =>
            Draw_Title ("Synth Engine:", "");
            Draw_Value (Project.Selected_Engine_Img (T));

         when LFO =>

            case Sub is
               when LFO_Rate =>
                  Draw_Title ("LFO Rate:", "");
               when LFO_Amplitude =>
                  Draw_Title ("LFO Amplitude:", "");
               when LFO_Shape =>
                  Draw_Title ("LFO Shape:", "");
               when LFO_Target =>
                  Draw_Title ("LFO Target:", "");
               when others =>
                  null;
            end case;

            Draw_CC_Value
              (A,
               Project.LFO_Rate (T),
               "RAT",
               Sub = LFO_Rate);

            Draw_CC_Value
              (B,
               Project.LFO_Amp (T),
               "AMP",
               Sub = LFO_Amplitude,
               Style => (case LFO_Amp_Mode (T) is
                            when Project.Positive => Drawing.Positive,
                            when Project.Center   => Drawing.Center,
                            when Project.Negative => Drawing.Negative));

            Draw_LFO_Shape (C,
                            "SHP",
                            Sub = LFO_Shape,
                            Project.LFO_Shape (T),
                            Project.LFO_Sync (T),
                            Project.LFO_Loop (T));

            Draw_CC_Value
              (D,
               0,
               Project.LFO_Target (T)'Img,
               Sub = LFO_Target);

         when CC_Default =>
            Draw_CC_Control_Page
              (T => T,
               Mode => Project.Mode (T),
               Selected => To_CC_Id (Sub),
               Val_A => Project.CC_Default (T, A),
               Val_B => Project.CC_Default (T, B),
               Val_C => Project.CC_Default (T, C),
               Val_D => Project.CC_Default (T, D),
               Ena_A => True,
               Ena_B => True,
               Ena_C => True,
               Ena_D => True);

      end case;

   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Track_Settings_Menu;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            Prev (This.Current_Setting);
         when Right_Press =>
            Next (This.Current_Setting);

         when Up_Press =>
            Project.Next_Value (This.Current_Setting, T);

         when Down_Press =>
            Project.Prev_Value (This.Current_Setting, T);

         when A_Press =>
            null;
         when B_Press =>
            null;

         when Slider_Touch =>
            Project.Set (This.Current_Setting, Event.Slider_Value, T);
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Track_Settings_Menu)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus
     (This       : in out Track_Settings_Menu;
      Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Chord_Synth_Settings;
