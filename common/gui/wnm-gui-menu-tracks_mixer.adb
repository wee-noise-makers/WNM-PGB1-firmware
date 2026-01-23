-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2026 Fabien Chouteau                  --
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

with Interfaces;

with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Screen; use WNM.Screen;
with WNM.Project; use WNM.Project;
with WNM.Mixer;
with WNM.Synth;
with MIDI;

with fx_bypass;
with fx_bitcrusher;
with fx_overdrive;
with fx_reverb;

with Tresses;

package body WNM.GUI.Menu.Tracks_Mixer is

   Singleton : aliased Instance;

   subtype Tracks_Sub_Settings is Project.User_Track_Settings
     range Project.Volume .. Project.Master_FX;

   function Sub (Top : Top_Settings) return Tracks_Sub_Settings
   is (case Top is
          when Track_Volume => Project.Volume,
          when Track_Pan    => Project.Pan,
          when others       => Project.Master_FX);

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Singleton'Access);
   end Push_Window;

   Top_Text_Y : constant := Box_Top + 2;
   Bot_Text_Y : constant := Box_Bottom - 2 - Font_Height * 1;

   Col_Top : constant := Top_Text_Y + Font_Height + 10;
   Col_Bot : constant := Bot_Text_Y - 2;
   Col_Height : constant := Col_Bot - Col_Top;

   Col_Width : constant := Screen_Width / 8;

   ----------------
   -- Draw_Peaks --
   ----------------

   procedure Draw_Peaks (X_Col_Center   : Natural;
                         L_Peak, R_Peak : Tresses.S16;
                         L_Hist, R_Hist : Tresses.S16;
                         Selected       : Boolean)
   is
      use Interfaces;
      use Tresses;

      F_L_Level : constant Float :=
        Float (abs L_Peak) / Float (S16'Last);
      F_R_Level : constant Float :=
        Float (abs R_Peak) / Float (S16'Last);

      F_L_Hist : constant Float :=
        Float (abs L_Hist) / Float (S16'Last);
      F_R_Hist : constant Float :=
        Float (abs R_Hist) / Float (S16'Last);

      L_Level : constant Natural :=
        Natural (F_L_Level * Float (Col_Height));
      R_Level : constant Natural :=
        Natural (F_R_Level * Float (Col_Height));

      L_Level_Alt : constant Natural :=
        Natural (F_L_Hist * Float (Col_Height));
      R_Level_Alt : constant Natural :=
        Natural (F_R_Hist * Float (Col_Height));

      On : constant Boolean := not Selected;
   begin
      if not On then
         Fill_Rect (((X_Col_Center - 5, Col_Top - 6),
                    Width => 11,
                    Height => Col_Height + 7));
      end if;
      Fill_Rect (((X_Col_Center - 4, Col_Bot - L_Level),
                 Width => 4,
                 Height => L_Level),
                 On);

      Fill_Rect (((X_Col_Center + 1, Col_Bot - R_Level),
                 Width => 4,
                 Height => R_Level),
                 On);

      Draw_Line ((X_Col_Center - 4, Col_Bot - L_Level_Alt),
                 (X_Col_Center - 1, Col_Bot - L_Level_Alt),
                 On);
      Draw_Line ((X_Col_Center + 1, Col_Bot - R_Level_Alt),
                 (X_Col_Center + 4, Col_Bot - R_Level_Alt),
                 On);

      if L_Peak = S16'Last then
         Draw_Line ((X_Col_Center - 4, Col_Top - 5),
                    (X_Col_Center - 1, Col_Top - 2),
                    On);
         Draw_Line ((X_Col_Center - 1, Col_Top - 5),
                    (X_Col_Center - 4, Col_Top - 2),
                    On);
      end if;

      if R_Peak = S16'Last then
         Draw_Line ((X_Col_Center + 4, Col_Top - 5),
                    (X_Col_Center + 1, Col_Top - 2),
                    On);
         Draw_Line ((X_Col_Center + 1, Col_Top - 5),
                    (X_Col_Center + 4, Col_Top - 2),
                    On);
      end if;
   end Draw_Peaks;

   ----------------
   -- Draw_Value --
   ----------------

   procedure Draw_Value (X : Natural; Value : Natural) is
   begin
      case Value is
         when 0 .. 9 =>
            declare
               Str : String := Value'Img;
            begin
               Str (Str'First) := '0';
               Draw_Str (X - Font_Width + 1, Bot_Text_Y, Str);
            end;
         when 100 =>
            Draw_Str (X - 6, Bot_Text_Y, "1");
            Draw_Str (X + 2, Bot_Text_Y, "0");
            Draw_Str (X - 2, Bot_Text_Y, "0");
         when others =>
            declare
               Str : constant String := Value'Img;
            begin
               Draw_Str (X - Font_Width + 1, Bot_Text_Y,
                         Str (Str'First + 1 .. Str'Last));
            end;
      end case;
   end Draw_Value;

   -----------------
   -- Draw_Volume --
   -----------------

   procedure Draw_Volume (X : Natural; V : WNM_HAL.Audio_Volume)
   is
   begin
      Draw_Value (X, Natural (V));
   end Draw_Volume;

   -------------
   -- Draw_FX --
   -------------

   procedure Draw_FX (X, Y : Natural; FX : FX_Kind) is
   begin
      case FX is
         when Bypass =>
            Screen.Copy_Bitmap
              (fx_bypass.Data, X - fx_bypass.Data.W / 2, Y);
         when Overdrive =>
            Screen.Copy_Bitmap
              (fx_overdrive.Data, X - fx_overdrive.Data.W / 2, Y);
         when Bitcrusher =>
            Screen.Copy_Bitmap
              (fx_bitcrusher.Data, X - fx_bitcrusher.Data.W / 2, Y);
         when Reverb =>
            Screen.Copy_Bitmap
              (fx_reverb.Data, X - fx_reverb.Data.W / 2, Y);
      end case;
   end Draw_FX;

   -----------------
   -- Draw_Tracks --
   -----------------

   procedure Draw_Tracks (This : in out Instance; Top : Top_Settings) is

      procedure Draw_Pan (X : Natural;
                          P : WNM_HAL.Audio_Pan)
      is
      begin
         Draw_Value (X, Natural (P));
      end Draw_Pan;

   begin
      for Id in Track_Id loop
         declare
            X_Col_Center : constant Natural :=
              (Natural (Id) - 1) * Col_Width + Col_Width / 2;

            Chan : constant MIDI.MIDI_Channel :=
              (case Id is
                  when 1 => WNM.Synth.Kick_Channel,
                  when 2 => WNM.Synth.Snare_Channel,
                  when 3 => WNM.Synth.Hihat_Channel,
                  when 4 => WNM.Synth.Bass_Channel,
                  when 5 => WNM.Synth.Lead_Channel,
                  when 6 => WNM.Synth.Chord_Channel,
                  when 7 => WNM.Synth.Sample1_Channel,
                  when 8 => WNM.Synth.Sample2_Channel);

         begin

            Draw_Peaks (X_Col_Center,
                        WNM.Synth.L_Peak (Chan),
                        WNM.Synth.R_Peak (Chan),
                        WNM.Synth.L_Peak_History (Chan),
                        WNM.Synth.R_Peak_History (Chan),
                        This.Selected_Track = Id);

            Draw_Str (X_Col_Center - Font_Width + 1,
                      Top_Text_Y,
                      (case Id is
                          when 1 => "KD",
                          when 2 => "SD",
                          when 3 => "HH",
                          when 4 => "BA",
                          when 5 => "LD",
                          when 6 => "CH",
                          when 7 => "S1",
                          when 8 => "S2"));

            case Sub (Top) is
               when Volume =>
                  Draw_Volume (X_Col_Center, Project.Track_Volume (Id));
               when Pan =>
                  Draw_Pan (X_Col_Center, Project.Track_Pan (Id));
               when Master_FX =>
                  Draw_FX (X_Col_Center, Bot_Text_Y, Project.Master_FX (Id));
            end case;

         end;
      end loop;

   end Draw_Tracks;

   ----------------
   -- Draw_Gains --
   ----------------

   procedure Draw_Gains (This : in out Instance) is
   begin
      for S in Project.Project_Mixer_Settings loop
         declare
            Id : constant Natural :=
              (if S /= Project.Output_Gain
               then Project.Project_Mixer_Settings'Pos (S)
               else Project.Project_Mixer_Settings'Pos (S) + 2);
            X_Col_Center : constant Natural := Id * Col_Width + Col_Width / 2;
         begin
            if S in Project.Bypass_Gain .. Project.Crusher_Gain then
               Draw_FX (X_Col_Center, Top_Text_Y,
                        (case S is
                            when Project.Bypass_Gain => Bypass,
                            when Project.Reverb_Gain => Reverb,
                            when Project.Overdrive_Gain => Overdrive,
                            when others => Bitcrusher));
            else
               Draw_Str (X_Col_Center - 3 * Font_Width + 1, Top_Text_Y,
                         "Output");
            end if;

            Draw_Peaks (X_Col_Center,
                        WNM.Mixer.L_Mix_Peak (S),
                        WNM.Mixer.R_Mix_Peak (S),
                        WNM.Mixer.L_Mix_Peak_History (S),
                        WNM.Mixer.R_Mix_Peak_History (S),
                        This.Gain_Select = S);
            Draw_Volume (X_Col_Center, Project.Get (S));
         end;
      end loop;
   end Draw_Gains;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Instance) is
      Top : constant Top_Settings := This.Current_Setting;
   begin
      case This.Current_Setting is
         when Track_Volume | Track_Pan | Track_FX =>
            Draw_Tracks (This, Top);
         when others =>
            Draw_Gains (This);
      end case;

      Draw_Menu_Box (Img (Top),
                     Count => Top_Settings_Count,
                     Index => Top_Settings'Pos (Top) -
                       Top_Settings'Pos (Top_Settings'First));

   end Draw;

   ---------------------
   -- On_Tracks_Event --
   ---------------------

   procedure On_Tracks_Event (This  : in out Instance;
                              Event : Menu_Event)
   is
      Top : constant Top_Settings := This.Current_Setting;
   begin
      case Event.Kind is
         when Left_Press =>
            if This.Selected_Track = Track_Id'First
              and then
                This.Current_Setting /= Top_Settings'First
            then
               This.Selected_Track := Track_Id'Last;
               Prev (This.Current_Setting);
            else
               Prev (This.Selected_Track);
            end if;

         when Right_Press =>
            if This.Selected_Track = Track_Id'Last
              and then
                This.Current_Setting /= Top_Settings'Last
            then
               This.Selected_Track := Track_Id'First;
               Next (This.Current_Setting);
            else
               Next (This.Selected_Track);
            end if;

         when Up_Press =>
            Project.Next_Value (This.Selected_Track, Sub (Top));

         when Down_Press =>
            Project.Prev_Value (This.Selected_Track, Sub (Top));

         when A_Press =>
            Next (This.Current_Setting);

         when B_Press =>
            Menu.Pop (Exit_Value => Success);

         when Slider_Touch =>
            Project.Set (This.Selected_Track,
                         Sub (Top),
                         Event.Slider_Value);

      end case;
   end On_Tracks_Event;

   --------------------
   -- On_Gains_Event --
   --------------------

   procedure On_Gains_Event (This  : in out Instance;
                             Event : Menu_Event)
   is
      package Gains_Next is new Enum_Next (Project.Project_Mixer_Settings,
                                           Wrap => False);
      use Gains_Next;
   begin
      case Event.Kind is
         when Left_Press =>
            Prev (This.Gain_Select);

         when Right_Press =>
            if This.Gain_Select = Project.Project_Mixer_Settings'Last then
               Next (This.Current_Setting);
               This.Selected_Track := Track_Id'First;
            else
               Next (This.Gain_Select);
            end if;

         when Up_Press =>
            Project.Next_Value (This.Gain_Select);

         when Down_Press =>
            Project.Prev_Value (This.Gain_Select);

         when A_Press =>
            Next (This.Current_Setting);

         when B_Press =>
            Menu.Pop (Exit_Value => Success);

         when Slider_Touch =>
            Project.Set (This.Gain_Select, Event.Slider_Value);

      end case;
   end On_Gains_Event;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Instance;
                       Event : Menu_Event)
   is
      Top : constant Top_Settings := This.Current_Setting;
   begin
      case Top is
         when Track_Volume | Track_Pan | Track_FX =>
            On_Tracks_Event (This, Event);
         when others =>
            On_Gains_Event (This, Event);
      end case;

   end On_Event;

end WNM.GUI.Menu.Tracks_Mixer;
