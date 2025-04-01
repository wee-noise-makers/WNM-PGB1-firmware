-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2022 Fabien Chouteau                    --
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

with HAL; use HAL;

with WNM.Coproc;
with WNM.Utils;
with WNM.Project_Load_Broadcast;
with WNM.Sample_Library;

package body WNM.Project is

   procedure Project_Load_Callback;
   package Project_Load_Listener
   is new Project_Load_Broadcast.Register (Project_Load_Callback'Access);
   pragma Unreferenced (Project_Load_Listener);

   ---------------------
   -- Chord_Play_Mode --
   ---------------------

   function Chord_Play_Mode return Chord_Play_Mode_Kind
   is (G_Project.Chord_Play_Mode);

   --------------------------
   -- Chord_Play_Mode_Next --
   --------------------------

   procedure Chord_Play_Mode_Next is
   begin
      if G_Project.Chord_Play_Mode /= Chord_Play_Mode_Kind'Last then
         G_Project.Chord_Play_Mode :=
           Chord_Play_Mode_Kind'Succ (G_Project.Chord_Play_Mode);
      else
         G_Project.Chord_Play_Mode := Chord_Play_Mode_Kind'First;
      end if;
   end Chord_Play_Mode_Next;

   --------------------
   -- Lead_Play_Mode --
   --------------------

   function Lead_Play_Mode return Lead_Play_Mode_Kind
   is (G_Project.Lead_Play_Mode);

   -------------------------
   -- Lead_Play_Mode_Next --
   -------------------------

   procedure Lead_Play_Mode_Next is
   begin
      if G_Project.Lead_Play_Mode /= Lead_Play_Mode_Kind'Last then
         G_Project.Lead_Play_Mode :=
           Lead_Play_Mode_Kind'Succ (G_Project.Lead_Play_Mode);
      else
         G_Project.Lead_Play_Mode := Lead_Play_Mode_Kind'First;
      end if;
   end Lead_Play_Mode_Next;

   -------------
   -- Set_BPM --
   -------------

   procedure Set_BPM (BPM : Beat_Per_Minute) is
   begin
      G_Project.BPM := BPM;
   end Set_BPM;

   ----------------
   -- Change_BPM --
   ----------------

   procedure Change_BPM (BPM_Delta : Integer) is
      Res : Integer;
   begin
      Res := Integer (G_Project.BPM) + BPM_Delta;
      if Res
        in Integer (Beat_Per_Minute'First) .. Integer (Beat_Per_Minute'Last)
      then
         G_Project.BPM := Beat_Per_Minute (Res);
      end if;
   end Change_BPM;

   -------------
   -- Get_BPM --
   -------------

   function Get_BPM return Beat_Per_Minute is
   begin
      return G_Project.BPM;
   end Get_BPM;

   ----------------------
   -- Samples_Per_Beat --
   ----------------------

   function Samples_Per_Beat return Synth.Sample_Time is
      use Synth;

      Samples_Per_Minute : constant Sample_Time :=
        60 * WNM_Configuration.Audio.Sample_Frequency;
   begin
      return Samples_Per_Minute / Sample_Time (Get_BPM);
   end Samples_Per_Beat;

   ---------------------------
   -- Microseconds_Per_Beat --
   ---------------------------

   function Microseconds_Per_Beat return Time.Time_Microseconds is
   begin
      return (60 * 1_000 * 1_000) / Time.Time_Microseconds (Get_BPM);
   end Microseconds_Per_Beat;

   ----------------------
   -- Load_Progression --
   ----------------------

   procedure Load_Progression (Scale_Choice : Valid_Scale_Choice;
                               Key          : MIDI.MIDI_Key;
                               Id           : Natural)
   is
      use WNM.Chord_Settings;
      use type MIDI.MIDI_Key;

      Collection : access constant Progression_Collection := null;
      Scale : Scale_Name;
   begin
      case Scale_Choice is
         when Major =>
            Collection := Major_Progressions'Access;
            Scale := Major_Scale;
         when Minor =>
            Collection := Minor_Progressions'Access;
            Scale := Minor_Scale;
         when Modal =>
            Collection := Modal_Progressions'Access;
            Scale := Major_Scale;
      end case;

      --  Load Lead notes
      declare
         L_Id : Lead_Button := Lead_Button'First;
      begin

         for Interval of WNM.Chord_Settings.Scales (Scale) loop
            G_Project.Leads (L_Id).Key := Key + Interval;
            L_Id := Lead_Button'Succ (L_Id);
         end loop;
      end;

      --  Load chords
      declare
         Collection_Len : constant Natural := Collection.all'Length;
         Offset : constant Natural := Id mod Collection_Len;
         Index  : constant Integer := Collection.all'First + Offset;

         C_Id : Chord_Button := Chord_Button'First;
      begin
         for Chord of Collection.all (Index).all loop
            G_Project.Chords (C_Id).Root := Tonic (Chord, Key, Scale);
            G_Project.Chords (C_Id).Quality := Chord.Harmonic_Function;

            exit when C_Id = Chord_Button'Last;
            C_Id := Chord_Button'Succ (C_Id);
         end loop;

         if C_Id /= Chord_Button'Last then
            --  Find substitutions for the remaining buttons
            for Chord of Collection.all (Index).all loop
               G_Project.Chords (C_Id).Root := Tonic (Chord, Key, Scale);
               G_Project.Chords (C_Id).Quality :=
                 Substitution (Chord.Harmonic_Function);

               exit when C_Id = Chord_Button'Last;
               C_Id := Chord_Button'Succ (C_Id);
            end loop;
         end if;
      end;
   end Load_Progression;

   ---------------------------------
   -- Randomly_Pick_A_Progression --
   ---------------------------------

   procedure Randomly_Pick_A_Progression (Scale_C : Scale_Choice;
                                          Key_C   : Key_Choice)
   is
      use WNM.Chord_Settings;
      use type MIDI.MIDI_Key;

      pragma Compile_Time_Error
        (Major_Progressions'Length > Rand_Percent'Last,
         "Random values don't cover all progressions range");
      pragma Compile_Time_Error
        (Minor_Progressions'Length > Rand_Percent'Last,
         "Random values don't cover all progressions range");
      pragma Compile_Time_Error
        (Modal_Progressions'Length > Rand_Percent'Last,
         "Random values don't cover all progressions range");

      Key  : MIDI.MIDI_Key;
      Scale : Scale_Choice := Scale_C;
   begin
      if Scale = Random then
         Scale := (case (UInt32 (WNM.Random) mod 3) is
                      when 0      => Major,
                      when 1      => Minor,
                      when others => Modal);
      end if;

      case Key_C is
         when Random => Key := MIDI.C3 + MIDI.MIDI_Key (Random mod 12);
         when C      => Key := MIDI.C3;
         when Cs     => Key := MIDI.Cs3;
         when D      => Key := MIDI.D3;
         when Ds     => Key := MIDI.Ds3;
         when E      => Key := MIDI.E3;
         when F      => Key := MIDI.F3;
         when Fs     => Key := MIDI.Fs3;
         when G      => Key := MIDI.G3;
         when Gs     => Key := MIDI.Gs3;
         when A      => Key := MIDI.A3;
         when As     => Key := MIDI.As3;
         when B      => Key := MIDI.B4;
      end case;

      Load_Progression (Scale, Key, Natural (WNM.Random));

   end Randomly_Pick_A_Progression;

   --------------------
   -- Sample_Id_Next --
   --------------------

   procedure Sample_Id_Next (CC : in out MIDI.MIDI_Data) is
      use MIDI;
      use WNM.Sample_Library;

   begin
      if CC < MIDI_Data (Sample_Index'Last) then
         CC := CC + 1;
      else
         CC := MIDI_Data (Sample_Index'Last);
      end if;
   end Sample_Id_Next;

   --------------------
   -- Sample_Id_Prev --
   --------------------

   procedure Sample_Id_Prev (CC : in out MIDI.MIDI_Data) is
      use MIDI;
      use WNM.Sample_Library;
   begin
      if CC > MIDI_Data (Sample_Index'Last) then
         CC := MIDI_Data (Sample_Index'Last);
      elsif CC > MIDI_Data (Sample_Index'First) then
         CC := CC - 1;
      else
         CC := MIDI_Data (Sample_Index'First);
      end if;
   end Sample_Id_Prev;

   -------------
   -- CC_Next --
   -------------

   procedure CC_Next (T    :        Tracks;
                      Id   :        CC_Id;
                      CC   : in out MIDI.MIDI_Data;
                      Fast :        Boolean := False)
   is
   begin
      if Mode (T) in Sample1_Mode | Sample2_Mode
        and then
          Id = A
        and then
          not G_Project.Tracks (T).MIDI_Enabled
      then
         --  Special case for sample section
         Sample_Id_Next (CC);
      else
         if Fast then
            MIDI_Data_Next.Next_Fast (CC);
         else
            MIDI_Data_Next.Next (CC);
         end if;
      end if;
   end CC_Next;

   ------------
   -- CC_Set --
   ------------

   procedure CC_Set (T  :        Tracks;
                     Id :        CC_Id;
                     CC : in out MIDI.MIDI_Data;
                     V  :        WNM_HAL.Touch_Value)
   is
   begin
      if Mode (T) in Sample1_Mode | Sample2_Mode
        and then
          Id = A
        and then
          not G_Project.Tracks (T).MIDI_Enabled
      then
         null;
      else
         MIDI_Data_Next.Set (CC, V);
      end if;
   end CC_Set;

   -------------
   -- CC_Prev --
   -------------

   procedure CC_Prev (T    :        Tracks;
                      Id   :        CC_Id;
                      CC   : in out MIDI.MIDI_Data;
                      Fast :        Boolean := False)
   is
   begin
      if Mode (T) in Sample1_Mode | Sample2_Mode
        and then
          Id = A
        and then
          not G_Project.Tracks (T).MIDI_Enabled
      then
         --  Special case for sample section
         Sample_Id_Prev (CC);
      else

         if Fast then
            MIDI_Data_Next.Prev_Fast (CC);
         else
            MIDI_Data_Next.Prev (CC);
         end if;
      end if;
   end CC_Prev;

   ----------
   -- Mode --
   ----------

   function Mode (T : Tracks) return Track_Mode_Kind is
   begin
      if G_Project.Tracks (T).MIDI_Enabled then
         return MIDI_Mode;
      else
         return (case T is
                    when Kick_Track     => Kick_Mode,
                    when Snare_Track    => Snare_Mode,
                    when Cymbal_Track   => Hihat_Mode,
                    when Bass_Track     => Bass_Mode,
                    when Lead_Track     => Lead_Mode,
                    when Sample1_Track  => Sample1_Mode,
                    when Sample2_Track  => Sample2_Mode,
                    --  when Speech_Track   => Speech_Mode,
                    when Chord_Track    => Chord_Mode,
                    when Reverb_Track   => Reverb_Mode,
                    when Drive_Track    => Drive_Mode,
                    when Bitcrush_Track => Bitcrush_Mode);
      end if;
   end Mode;

   ---------------
   -- MIDI_Chan --
   ---------------

   function MIDI_Chan (T : Tracks) return MIDI.MIDI_Channel
   is (G_Project.Tracks (T).Chan);

   ----------------
   -- Track_Name --
   ----------------

   function Track_Name (T : Tracks) return String
   is (case Mode (T) is
          when MIDI_Mode => "MIDI" & MIDI_Chan (T)'Img,
          when others    => Img (Mode (T)));

   ------------------
   -- Track_Volume --
   ------------------

   function Track_Volume (T : Tracks) return Audio_Volume
   is (G_Project.Tracks (T).Volume);

   ---------------
   -- Track_Pan --
   ---------------

   function Track_Pan (T : Tracks) return Audio_Pan
   is (G_Project.Tracks (T).Pan);

   ------------------
   -- Track_Offset --
   ------------------

   function Track_Offset (T : Tracks) return Octave_Offset
   is (G_Project.Tracks (T).Offset);

   -------------------
   -- Track_Shuffle --
   -------------------

   function Track_Shuffle (T : Tracks) return Shuffle_Value
   is (G_Project.Tracks (T).Shuffle);

   -------------------
   -- CC_Controller --
   -------------------

   function CC_Controller (T : Tracks;
                           Id : CC_Id)
                           return MIDI.MIDI_Data
   is (G_Project.Tracks (T).CC (Id).Controller);

   ----------------
   -- CC_Default --
   ----------------

   function CC_Default (T : Tracks;
                        Id : CC_Id)
                        return MIDI.MIDI_Data
   is (G_Project.Tracks (T).CC (Id).Value);

   ---------------
   -- Master_FX --
   ---------------

   function Master_FX (T : Tracks) return FX_Kind
   is (G_Project.Tracks (T).FX);

   --------------
   -- LFO_Rate --
   --------------

   function LFO_Rate (T : Tracks) return MIDI.MIDI_Data
   is (G_Project.Tracks (T).LFO_Rate);

   -------------
   -- LFO_Amp --
   -------------

   function LFO_Amp (T : Tracks) return MIDI.MIDI_Data
   is (G_Project.Tracks (T).LFO_Amp);

   ----------------
   -- LFO_Target --
   ----------------

   function LFO_Target (T : Tracks) return LFO_Target_Kind
   is (G_Project.Tracks (T).LFO_Target);

   ---------------
   -- LFO_Shape --
   ---------------

   function LFO_Shape (T : Tracks) return LFO_Shape_Kind
   is (G_Project.Tracks (T).LFO_Shape);

   --------------
   -- LFO_Sync --
   --------------

   function LFO_Sync (T : Tracks) return LFO_Sync_Kind
   is (G_Project.Tracks (T).LFO_Sync);

   --------------
   -- LFO_Loop --
   --------------

   function LFO_Loop (T : Tracks) return LFO_Loop_Kind
   is (G_Project.Tracks (T).LFO_Loop);

   ------------------
   -- LFO_Amp_Mode --
   ------------------

   function LFO_Amp_Mode (T : Tracks) return LFO_Amp_Kind
   is (G_Project.Tracks (T).LFO_Amp_Mode);

   -------------------------
   -- CC_Controller_Label --
   -------------------------

   function CC_Controller_Label (T    : Tracks;
                                 Id   : CC_Id)
                                 return Controller_Label
   is
      Result : Controller_Label;

      Tresses_Id : constant Tresses.Param_Id := (case Id is
                                                    when A => 1,
                                                    when B => 2,
                                                    when C => 3,
                                                    when D => 4);
   begin
      case Mode (T) is
         when MIDI_Mode =>
            return G_Project.Tracks (T).CC (Id).Label;

         when Kick_Mode =>
            Utils.Copy_Str (Synth.Kick_Param_Label (Tresses_Id), Result);
            return Result;

         when Snare_Mode =>
            Utils.Copy_Str (Synth.Snare_Param_Label (Tresses_Id), Result);
            return Result;

         when Hihat_Mode =>
            Utils.Copy_Str (Synth.Hihat_Param_Label (Tresses_Id), Result);
            return Result;

         when Chord_Mode =>
            Utils.Copy_Str (Synth.Chord_Param_Label (Tresses_Id), Result);
            return Result;

         when Lead_Mode | Bass_Mode =>
            Utils.Copy_Str (Synth.Lead_Param_Label (Selected_Engine (T),
                                                    Tresses_Id),
                            Result);
            return Result;

         when Sample1_Mode =>
            Utils.Copy_Str (Synth.Sampler_Param_Label
                            (Synth.Sample1_Channel, Tresses_Id), Result);
            return Result;

         when Sample2_Mode =>
            Utils.Copy_Str (Synth.Sampler_Param_Label
                            (Synth.Sample2_Channel, Tresses_Id), Result);
            return Result;

         when Reverb_Mode =>
            Utils.Copy_Str (Synth.Reverb_Param_Label (Tresses_Id), Result);
            return Result;

         when Drive_Mode =>
            Utils.Copy_Str (Synth.Drive_Param_Label (Tresses_Id), Result);
            return Result;

         when Bitcrush_Mode =>
            Utils.Copy_Str (Synth.Bitcrush_Param_Label (Tresses_Id), Result);
            return Result;
      end case;
   end CC_Controller_Label;

   -------------------------------
   -- CC_Controller_Short_Label --
   -------------------------------

   function CC_Controller_Short_Label (T    : Tracks;
                                       Id   : CC_Id)
                                       return Tresses.Short_Label
   is
      Tresses_Id : constant Tresses.Param_Id := (case Id is
                                                    when A => 1,
                                                    when B => 2,
                                                    when C => 3,
                                                    when D => 4);
   begin
      case Mode (T) is
         when MIDI_Mode =>
            return G_Project.Tracks (T).CC (Id).Label (1 .. 3);

         when Kick_Mode =>
            return Synth.Kick_Param_Short_Label (Tresses_Id);

         when Snare_Mode =>
            return Synth.Snare_Param_Short_Label (Tresses_Id);

         when Hihat_Mode =>
            return Synth.Hihat_Param_Short_Label (Tresses_Id);

         when Chord_Mode =>
            return Synth.Chord_Param_Short_Label (Tresses_Id);

         when Lead_Mode | Bass_Mode =>
            return Synth.Lead_Param_Short_Label (Selected_Engine (T),
                                                 Tresses_Id);

         when Sample1_Mode =>
            return Synth.Sampler_Param_Short_Label
              (Synth.Sample1_Channel, Tresses_Id);

         when Sample2_Mode =>
            return Synth.Sampler_Param_Short_Label
              (Synth.Sample2_Channel, Tresses_Id);

         when Reverb_Mode =>
            return Synth.Reverb_Param_Short_Label (Tresses_Id);

         when Drive_Mode =>
            return Synth.Drive_Param_Short_Label (Tresses_Id);

         when Bitcrush_Mode =>
            return Synth.Bitcrush_Param_Short_Label (Tresses_Id);

      end case;
   end CC_Controller_Short_Label;

   ---------------------
   -- Selected_Engine --
   ---------------------

   function Selected_Engine (T : Tracks)
                             return MIDI.MIDI_Data
   is (G_Project.Tracks (T).Engine);

   -------------------------
   -- Selected_Engine_Img --
   -------------------------

   function Selected_Engine_Img (T : Tracks)
                                 return String
   is
   begin
      case Mode (T) is
         when Lead_Mode | Bass_Mode =>
            return Synth.Lead_Engine_Img (Selected_Engine (T));
         when Kick_Mode =>
            return Synth.Kick_Engine_Img (Selected_Engine (T));
         when Snare_Mode =>
            return Synth.Snare_Engine_Img (Selected_Engine (T));
         when Hihat_Mode =>
            return Synth.Hihat_Engine_Img (Selected_Engine (T));
         when Chord_Mode =>
            return Synth.Chord_Engine_Img (Selected_Engine (T));
         when Sample1_Mode | Sample2_Mode =>
            return Synth.Sampler_Engine_Img (Selected_Engine (T));
         when others =>
            return "No Engine";
      end case;
   end Selected_Engine_Img;

   --------------
   -- Arp_Mode --
   --------------

   function Arp_Mode (T : Tracks) return Arp_Mode_Kind
   is (G_Project.Tracks (T).Arp_Mode);

   ---------------
   -- Arp_Notes --
   ---------------

   function Arp_Notes (T : Tracks) return Arp_Notes_Kind
   is (G_Project.Tracks (T).Arp_Notes);

   ---------------------
   -- Notes_Per_Chord --
   ---------------------

   function Notes_Per_Chord (T : Tracks)
                             return Natural
   is (Natural (G_Project.Tracks (T).Notes_Per_Chord) + 1);

   ----------
   -- Next --
   ----------

   procedure Next (Shape : in out LFO_Shape_Kind;
                   Sync  : in out LFO_Sync_Kind;
                   Loo   : in out LFO_Loop_Kind)
   is
   begin
      --         Loop
      --  Sync + Loop
      --  Sync
      if Loo = On and then Sync = Off then
         Sync := On;
      elsif Loo = On and then Sync = On then
         Loo := Off;
      else
         Sync := Off;
         Loo := On;
         Next (Shape);
      end if;
   end Next;

   ----------
   -- Prev --
   ----------

   procedure Prev (Shape : in out LFO_Shape_Kind;
                   Sync  : in out LFO_Sync_Kind;
                   Loo   : in out LFO_Loop_Kind)
   is
   begin
      --  Sync
      --  Sync + Loop
      --         Loop
      if Loo = Off and then Sync = On then
         Loo := On;
      elsif Loo = On and then Sync = On then
         Sync := Off;
      else
         Sync := On;
         Loo := Off;
         Prev (Shape);
      end if;
   end Prev;

   ----------
   -- Next --
   ----------

   procedure Next (Amp    : in out MIDI.MIDI_Data;
                   Mode   : in out LFO_Amp_Kind;
                   Amount :        MIDI.MIDI_Data)
   is
      use MIDI;
   begin
      case Mode is
         when Positive =>
            if Amp <= MIDI_Data'Last - Amount then
               Amp := Amp + Amount;
            end if;
         when Center =>
            if Amp <= MIDI_Data'Last - Amount then
               Amp := Amp + Amount;
            else
               Mode := Positive;
               Amp := 0;
            end if;
         when Negative =>
            if Amp >= Amount then
               Amp := Amp - Amount;
            else
               Mode := Center;
               Amp := 0;
            end if;
      end case;
   end Next;

   ----------
   -- Prev --
   ----------

   procedure Prev (Amp    : in out MIDI.MIDI_Data;
                   Mode   : in out LFO_Amp_Kind;
                   Amount :        MIDI.MIDI_Data)
   is
      use MIDI;
   begin
      case Mode is
         when Positive =>
            if Amp >= Amount then
               Amp := Amp - Amount;
            else
               Mode := Center;
               Amp := MIDI_Data'Last;
            end if;
         when Center =>
            if Amp >= Amount then
               Amp := Amp - Amount;
            else
               Mode := Negative;
               Amp := 0;
            end if;
         when Negative =>
            if Amp <= MIDI_Data'Last - Amount then
               Amp := Amp + Amount;
            end if;
      end case;
   end Prev;

   --------------------------
   -- CC_For_Track_Setting --
   --------------------------

   function CC_For_Track_Setting (S : Track_Settings)
                                  return MIDI.MIDI_Data
   is (case S is
          when Engine          => Synth.Voice_Engine_CC,
          when Volume          => Synth.Voice_Volume_CC,
          when Pan             => Synth.Voice_Pan_CC,
          when Master_FX       => Synth.Voice_FX_CC,
          when LFO_Rate        => Synth.Voice_LFO_Rate_CC,
          when LFO_Amplitude   => Synth.Voice_LFO_Amp_CC,
          when LFO_Amp_Mode    => Synth.Voice_LFO_Amp_Mode_CC,
          when LFO_Shape       => Synth.Voice_LFO_Shape_CC,
          when LFO_Loop        => Synth.Voice_LFO_Loop_CC,
          when LFO_Sync        => Synth.Voice_LFO_Sync_CC,
          when LFO_Target      => Synth.Voice_LFO_Target_CC,
          when CC_Default_A    => Synth.Voice_Param_1_CC,
          when CC_Default_B    => Synth.Voice_Param_2_CC,
          when CC_Default_C    => Synth.Voice_Param_3_CC,
          when CC_Default_D    => Synth.Voice_Param_4_CC,
          when others          => MIDI.MIDI_Data'Last);

   -------------------------------
   -- Synchronize_Synth_Setting --
   -------------------------------

   procedure Synchronize_Synth_Setting (T : Tracks;
                                        S : Track_Settings)
   is
      use Coproc;
      use MIDI;

      Track : Track_Rec renames G_Project.Tracks (T);
      Chan : MIDI.MIDI_Channel;
      CC, Val : MIDI.MIDI_Data;
   begin

      if Mode (T) in Synth_Track_Mode_Kind then
         Chan := Voice_MIDI_Chan (Mode (T));
         CC := CC_For_Track_Setting (S);

         if CC = MIDI.MIDI_Data'Last then
            --  This setting doesn't control the synth
            return;
         end if;

         Val := (case S is
                    when Engine          => Selected_Engine (T),
                    when Volume          => MIDI.MIDI_Data (Track.Volume),
                    when Pan             => MIDI.MIDI_Data (Track.Pan),
                    when Master_FX       =>
                   (case Track.FX is
                       when Bypass     => Synth.FX_Select_Bypass,
                       when Overdrive  => Synth.FX_Select_Overdrive,
                       when Bitcrusher => Synth.FX_Select_Bitcrusher,
                       when Reverb     => Synth.FX_Select_Reverb),
                    when LFO_Rate        => Track.LFO_Rate,
                    when LFO_Amplitude   => Track.LFO_Amp,
                    when LFO_Amp_Mode    => Track.LFO_Amp_Mode'Enum_Rep,
                    when LFO_Shape       => Track.LFO_Shape'Enum_Rep,
                    when LFO_Target      => Track.LFO_Target'Enum_Rep,
                    when LFO_Loop        => Track.LFO_Loop'Enum_Rep,
                    when LFO_Sync        => Track.LFO_Sync'Enum_Rep,
                    when CC_Default_A    => Track.CC (A).Value,
                    when CC_Default_B    => Track.CC (B).Value,
                    when CC_Default_C    => Track.CC (C).Value,
                    when CC_Default_D    => Track.CC (D).Value,
                    when others          => MIDI.MIDI_Data'Last);

         Coproc.Push_To_Synth
           ((Kind     => MIDI_Event,
             MIDI_Evt =>
               (Kind => MIDI.Continous_Controller,
                Chan => Chan,
                Controller => CC,
                Controller_Value => Val)));
      end if;
   end Synchronize_Synth_Setting;

   -------------------
   -- Engine_Limits --
   -------------------

   function Engine_Limits (T : Tracks) return MIDI_Data_Next.Limits is
      use MIDI;
   begin
      case Mode (T) is
         when Lead_Mode =>
            return (MIDI_Data'First, Synth.Lead_Engine_Last);
         when Bass_Mode =>
            return (MIDI_Data'First, Synth.Lead_Engine_Last);
         when Kick_Mode =>
            return (MIDI_Data'First, Synth.Kick_Engine_Last);
         when Snare_Mode =>
            return (MIDI_Data'First, Synth.Snare_Engine_Last);
         when Hihat_Mode =>
            return (MIDI_Data'First, Synth.Hihat_Engine_Last);
         when Chord_Mode =>
            return (MIDI_Data'First, Synth.Chord_Engine_Last);
         when Sample1_Mode | Sample2_Mode  =>
            return (MIDI_Data'First, Synth.Sampler_Engine_Last);
         when others =>
            return (MIDI_Data'First, MIDI_Data'First);
      end case;
   end Engine_Limits;

   ---------
   -- Set --
   ---------

   procedure Set (S : User_Track_Settings;
                  V : WNM_HAL.Touch_Value;
                  T : Tracks)
   is
      Track : Track_Rec renames G_Project.Tracks (T);
   begin
      case S is
         when Engine          => Set (Track.Engine, V, Engine_Limits (T));
         when Volume          => Set (Track.Volume, V);
         when Pan             => Set (Track.Pan, V);
         when Master_FX       => Set (Track.FX, V);
         when Track_Octave_Offset => Set (Track.Offset, V);
         when Shuffle         => Set (Track.Shuffle, V);
         when LFO_Rate        => Set (Track.LFO_Rate, V);
         when LFO_Amplitude   => Set (Track.LFO_Amp, V);
         when LFO_Shape       => Set (Track.LFO_Shape, V);
         when LFO_Target      => Set (Track.LFO_Target, V);
         when Arp_Mode        => Set (Track.Arp_Mode, V);
         when Arp_Notes       => Set (Track.Arp_Notes, V);
         when CC_Default_A    => CC_Set (T, A, Track.CC (A).Value, V);
         when CC_Default_B    => CC_Set (T, B, Track.CC (B).Value, V);
         when CC_Default_C    => CC_Set (T, C, Track.CC (C).Value, V);
         when CC_Default_D    => CC_Set (T, D, Track.CC (D).Value, V);
      end case;

      Synchronize_Synth_Setting (T, S);

   end Set;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (S : User_Track_Settings;
                         T : Tracks)
   is
      Track : Track_Rec renames G_Project.Tracks (T);
   begin
      case S is
         when Engine          => Next (Track.Engine, Engine_Limits (T));
         when Volume          => Next (Track.Volume);
         when Pan             => Next (Track.Pan);
         when Master_FX       => Next (Track.FX);
         when Track_Octave_Offset => Next (Track.Offset);
         when Shuffle         => Next (Track.Shuffle);
         when LFO_Rate        => Next (Track.LFO_Rate);

         when LFO_Amplitude   =>
            Next (Track.LFO_Amp, Track.LFO_Amp_Mode, 1);
            Synchronize_Synth_Setting (T, LFO_Amp_Mode);

         when LFO_Shape       =>
            Next (Track.LFO_Shape, Track.LFO_Sync, Track.LFO_Loop);
            Synchronize_Synth_Setting (T, LFO_Sync);
            Synchronize_Synth_Setting (T, LFO_Loop);

         when LFO_Target      => Next (Track.LFO_Target);
         when Arp_Mode        => Next (Track.Arp_Mode);
         when Arp_Notes       => Next (Track.Arp_Notes);
         when CC_Default_A    => CC_Next (T, A, Track.CC (A).Value);
         when CC_Default_B    => CC_Next (T, B, Track.CC (B).Value);
         when CC_Default_C    => CC_Next (T, C, Track.CC (C).Value);
         when CC_Default_D    => CC_Next (T, D, Track.CC (D).Value);
      end case;

      Synchronize_Synth_Setting (T, S);

   end Next_Value;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value (S : User_Track_Settings;
                         T : Tracks)
   is
      Track : Track_Rec renames G_Project.Tracks (T);
   begin
      case S is
         when Engine          => Prev (Track.Engine, Engine_Limits (T));
         when Volume          => Prev (Track.Volume);
         when Pan             => Prev (Track.Pan);
         when Master_FX       => Prev (Track.FX);
         when Track_Octave_Offset => Prev (Track.Offset);
         when Shuffle         => Prev (Track.Shuffle);
         when LFO_Rate        => Prev (Track.LFO_Rate);
         when LFO_Amplitude   =>
            Prev (Track.LFO_Amp, Track.LFO_Amp_Mode, 1);
            Synchronize_Synth_Setting (T, LFO_Amp_Mode);

         when LFO_Shape       =>
            Prev (Track.LFO_Shape, Track.LFO_Sync, Track.LFO_Loop);
            Synchronize_Synth_Setting (T, LFO_Sync);
            Synchronize_Synth_Setting (T, LFO_Loop);

         when LFO_Target      => Prev (Track.LFO_Target);
         when Arp_Mode        => Prev (Track.Arp_Mode);
         when Arp_Notes       => Prev (Track.Arp_Notes);
         when CC_Default_A    => CC_Prev (T, A, Track.CC (A).Value);
         when CC_Default_B    => CC_Prev (T, B, Track.CC (B).Value);
         when CC_Default_C    => CC_Prev (T, C, Track.CC (C).Value);
         when CC_Default_D    => CC_Prev (T, D, Track.CC (D).Value);
      end case;

      Synchronize_Synth_Setting (T, S);
   end Prev_Value;

   ---------------------
   -- Next_Value_Fast --
   ---------------------

   procedure Next_Value_Fast (S : User_Track_Settings;
                              T : Tracks)
   is
      Track : Track_Rec renames G_Project.Tracks (T);
   begin
      case S is
         when Engine          => Next_Fast (Track.Engine, Engine_Limits (T));
         when Volume          => Next_Fast (Track.Volume);
         when Pan             => Next_Fast (Track.Pan);
         when Master_FX       => Next_Fast (Track.FX);
         when Track_Octave_Offset => Next_Fast (Track.Offset);
         when Shuffle         => Next_Fast (Track.Shuffle);
         when LFO_Rate        => Next_Fast (Track.LFO_Rate);

         when LFO_Amplitude   =>
            Next (Track.LFO_Amp, Track.LFO_Amp_Mode, 10);
            Synchronize_Synth_Setting (T, LFO_Amp_Mode);

         when LFO_Shape       =>
            Next (Track.LFO_Shape, Track.LFO_Sync, Track.LFO_Loop);
            Synchronize_Synth_Setting (T, LFO_Sync);
            Synchronize_Synth_Setting (T, LFO_Loop);

         when LFO_Target      => Next_Fast (Track.LFO_Target);
         when Arp_Mode        => Next_Fast (Track.Arp_Mode);
         when Arp_Notes       => Next_Fast (Track.Arp_Notes);
         when CC_Default_A    => CC_Next (T, A, Track.CC (A).Value, True);
         when CC_Default_B    => CC_Next (T, B, Track.CC (B).Value, True);
         when CC_Default_C    => CC_Next (T, C, Track.CC (C).Value, True);
         when CC_Default_D    => CC_Next (T, D, Track.CC (D).Value, True);
      end case;

      Synchronize_Synth_Setting (T, S);
   end Next_Value_Fast;

   ---------------------
   -- Prev_Value_Fast --
   ---------------------

   procedure Prev_Value_Fast (S : User_Track_Settings;
                              T : Tracks)
   is
      Track : Track_Rec renames G_Project.Tracks (T);
   begin
      case S is
         when Engine          => Prev_Fast (Track.Engine, Engine_Limits (T));
         when Volume          => Prev_Fast (Track.Volume);
         when Pan             => Prev_Fast (Track.Pan);
         when Master_FX       => Prev_Fast (Track.FX);
         when Track_Octave_Offset => Prev_Fast (Track.Offset);
         when Shuffle         => Prev_Fast (Track.Shuffle);
         when LFO_Rate        => Prev_Fast (Track.LFO_Rate);

         when LFO_Amplitude   =>
            Prev (Track.LFO_Amp, Track.LFO_Amp_Mode, 10);
            Synchronize_Synth_Setting (T, LFO_Amp_Mode);

         when LFO_Shape       =>
            Prev (Track.LFO_Shape, Track.LFO_Sync, Track.LFO_Loop);
            Synchronize_Synth_Setting (T, LFO_Sync);
            Synchronize_Synth_Setting (T, LFO_Loop);

         when LFO_Target      => Prev_Fast (Track.LFO_Target);
         when Arp_Mode        => Prev_Fast (Track.Arp_Mode);
         when Arp_Notes       => Prev_Fast (Track.Arp_Notes);
         when CC_Default_A    => CC_Prev (T, A, Track.CC (A).Value, True);
         when CC_Default_B    => CC_Prev (T, B, Track.CC (B).Value, True);
         when CC_Default_C    => CC_Prev (T, C, Track.CC (C).Value, True);
         when CC_Default_D    => CC_Prev (T, D, Track.CC (D).Value, True);
      end case;

      Synchronize_Synth_Setting (T, S);
   end Prev_Value_Fast;

   -----------------------
   -- Set_CC_Controller --
   -----------------------

   procedure Set_CC_Controller (T : Tracks; Id : CC_Id; C : MIDI.MIDI_Data) is
   begin
      G_Project.Tracks (T).CC (Id).Controller := C;
   end Set_CC_Controller;

   -----------------------------
   -- Set_CC_Controller_Label --
   -----------------------------

   procedure Set_CC_Controller_Label (T    : Tracks;
                                      Id   : CC_Id;
                                      Label : Controller_Label)
   is
   begin
      G_Project.Tracks (T).CC (Id).Label := Label;
   end Set_CC_Controller_Label;

   ---------------
   -- Root_Note --
   ---------------

   function Root_Note (C : Chord_Button) return MIDI.MIDI_Key is
   begin
      return G_Project.Chords (C).Root;
   end Root_Note;

   -------------
   -- Quality --
   -------------

   function Quality (C : Chord_Button) return WNM.Chord_Settings.Chord_Name is
   begin
      return G_Project.Chords (C).Quality;
   end Quality;

   ---------
   -- Set --
   ---------

   procedure Set (S : User_Chord_Settings;
                  V : WNM_HAL.Touch_Value;
                  C : Chord_Button)
   is
   begin
      case S is
         when Root => Set (G_Project.Chords (C).Root, V);
         when Quality => Set (G_Project.Chords (C).Quality, V);
      end case;
   end Set;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (S : User_Chord_Settings; C : Chord_Button) is
   begin
      case S is
         when Root => Next (G_Project.Chords (C).Root);
         when Quality => Next (G_Project.Chords (C).Quality);
      end case;
   end Next_Value;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value (S : User_Chord_Settings; C : Chord_Button) is
   begin
      case S is
         when Root => Prev (G_Project.Chords (C).Root);
         when Quality => Prev (G_Project.Chords (C).Quality);
      end case;
   end Prev_Value;

   ----------
   -- Note --
   ----------

   function Note (C : Lead_Button) return MIDI.MIDI_Key
   is (G_Project.Leads (C).Key);

   ---------
   -- Set --
   ---------

   procedure Set (S : User_Lead_Settings;
                  V : WNM_HAL.Touch_Value;
                  C : Lead_Button)
   is
   begin
      case S is
         when Note => Set (G_Project.Leads (C).Key, V);
      end case;
   end Set;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (S : User_Lead_Settings; C : Lead_Button)
        is
   begin
      case S is
         when Note => Next (G_Project.Leads (C).Key);
      end case;
   end Next_Value;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value (S : User_Lead_Settings; C : Lead_Button)
   is
   begin
      case S is
         when Note => Prev (G_Project.Leads (C).Key);
      end case;
   end Prev_Value;

   --------------------
   -- Pattern_Length --
   --------------------

   function Pattern_Length return WNM.Pattern_Length
   is (G_Project.Pattern_Len);

   -------------------------
   -- Incr_Pattern_Length --
   -------------------------

   procedure Incr_Pattern_Length is
   begin
      if G_Project.Pattern_Len /= WNM.Pattern_Length'Last then
         G_Project.Pattern_Len := @ + 1;
      end if;
   end Incr_Pattern_Length;

   -------------------------
   -- Decr_Pattern_Length --
   -------------------------

   procedure Decr_Pattern_Length is
   begin
      if G_Project.Pattern_Len /= WNM.Pattern_Length'First then
         G_Project.Pattern_Len := @ - 1;
      end if;
   end Decr_Pattern_Length;

   -------------
   -- Trigger --
   -------------

   function Trigger (DT : Drum_Tracks; Step : WNM.Pattern_Length)
                     return Trigger_Kind
   is (G_Project.Pattern (DT, Step));

   ------------------
   -- Trigger_Next --
   ------------------

   procedure Trigger_Next (DT : Drum_Tracks; Step : WNM.Pattern_Length) is
   begin
      Next (G_Project.Pattern (DT, Step));
   end Trigger_Next;

   ------------------
   -- Trigger_Prev --
   ------------------

   procedure Trigger_Prev (DT : Drum_Tracks; Step : WNM.Pattern_Length) is
   begin
      Prev (G_Project.Pattern (DT, Step));
   end Trigger_Prev;

   ---------
   -- Set --
   ---------

   procedure Set (S : User_Pattern_Settings; V : WNM_HAL.Touch_Value) is
      pragma Unreferenced (V);
      --  P : Pattern_Rec renames
      --    G_Project.Patterns (Editing_Track)(Editing_Pattern);
   begin
      case S is
         when Length =>
            --  Pattern_Length_Next.Set (P.Length, V);
            null;
      end case;
   end Set;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (S : User_Pattern_Settings) is
      --  P : Pattern_Rec renames
      --    G_Project.Patterns (Editing_Track)(Editing_Pattern);
   begin
      case S is
         when Length =>
            --  Next (P.Length);
            null;
      end case;
   end Next_Value;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value (S : User_Pattern_Settings) is
      --  P : Pattern_Rec renames
      --    G_Project.Patterns (Editing_Track)(Editing_Pattern);
   begin
      case S is
         when Length =>
            --  Prev (P.Length);
            null;
      end case;
   end Prev_Value;

   -----------------------
   -- Alt_Slider_Target --
   -----------------------

   function Alt_Slider_Target return Alt_Slider_Control
   is (G_Project.Alt_Slider_Target);

   ----------------------
   -- Alt_Slider_Value --
   ----------------------

   function Alt_Slider_Value return MIDI.MIDI_Data is
      use MIDI;
      T : constant Tracks := G_Project.Alt_Slider_Track;
   begin
      return
        (case Alt_Slider_Target is
            when Alt_Sld_CC_Default_A  => CC_Default (T, A),
            when Alt_Sld_CC_Default_B  => CC_Default (T, B),
            when Alt_Sld_CC_Default_C  => CC_Default (T, C),
            when Alt_Sld_CC_Default_D  => CC_Default (T, D),
            when Alt_Sld_LFO_Rate      => LFO_Rate (T),
            when Alt_Sld_LFO_Amplitude => LFO_Amp (T),
            when Alt_Sld_Volume        => MIDI.MIDI_Data (Track_Volume (T)),
            when Alt_Sld_Pan           => MIDI.MIDI_Data (Track_Pan (T)),
            when Alt_Sld_Shuffle       =>
              50 + MIDI.MIDI_Data (Track_Shuffle (T)) / 2
        );
   end Alt_Slider_Value;

   ---------------------------
   -- Alt_Slider_Target_Img --
   ---------------------------

   function Alt_Slider_Target_Label return String is
      T : constant Tracks := G_Project.Alt_Slider_Track;
   begin
      return Utils.Trim
        (
         case Alt_Slider_Target is
            when Alt_Sld_CC_Default_A  => CC_Controller_Label (T, A),
            when Alt_Sld_CC_Default_B  => CC_Controller_Label (T, B),
            when Alt_Sld_CC_Default_C  => CC_Controller_Label (T, C),
            when Alt_Sld_CC_Default_D  => CC_Controller_Label (T, D),
            when Alt_Sld_LFO_Rate      => "LFO Rate",
            when Alt_Sld_LFO_Amplitude => "LFO Amp",
            when Alt_Sld_Volume        => "Volume",
            when Alt_Sld_Pan           => "Pan",
            when Alt_Sld_Shuffle       => "Shuffle"
        );
   end Alt_Slider_Target_Label;

   ----------------------
   -- Alt_Slider_Track --
   ----------------------

   function Alt_Slider_Track return Tracks
   is (G_Project.Alt_Slider_Track);

   --------------------
   -- Alt_Slider_Set --
   --------------------

   procedure Alt_Slider_Set (Val : WNM_HAL.Touch_Value) is
      S : constant User_Track_Settings :=
        To_Track_Setting (G_Project.Alt_Slider_Target);
   begin
      Set (S, Val, G_Project.Alt_Slider_Track);
   end Alt_Slider_Set;

   ----------------------------
   -- Alt_Slider_Target_Next --
   ----------------------------

   procedure Alt_Slider_Target_Next is
   begin
      Next (G_Project.Alt_Slider_Target);
   end Alt_Slider_Target_Next;

   ----------------------------
   -- Alt_Slider_Target_Prev --
   ----------------------------

   procedure Alt_Slider_Target_Prev is
   begin
      Prev (G_Project.Alt_Slider_Target);
   end Alt_Slider_Target_Prev;

   ---------------------------
   -- Alt_Slider_Track_Next --
   ---------------------------

   procedure Alt_Slider_Track_Next is
   begin
      Next (G_Project.Alt_Slider_Track);
   end Alt_Slider_Track_Next;

   ---------------------------
   -- Alt_Slider_Track_Prev --
   ---------------------------

   procedure Alt_Slider_Track_Prev is
   begin
      Prev (G_Project.Alt_Slider_Track);
   end Alt_Slider_Track_Prev;

   --------------------------------
   -- Synchronize_Synth_Settings --
   --------------------------------

   procedure Synchronize_Synth_Settings (T : Tracks) is
   begin
      for S in Track_Settings loop
         Synchronize_Synth_Setting (T, S);
      end loop;
   end Synchronize_Synth_Settings;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      --  G_Project := (others => <>); Unforunately the statement above is
      --  not usable as it first create a full instance of the project on the
      --  stack before copying it into G_Project. Of course there's not enough
      --  room on the stack to hold the full project.

      G_Project.BPM := BPM_Default;
      G_Project.Tracks := Tracks_Defaults;

      G_Project.Alt_Slider_Track := Lead_Track;
      G_Project.Alt_Slider_Target := Alt_Slider_Control'First;

      WNM.Project_Load_Broadcast.Broadcast;
   end Clear;

   ---------------------------
   -- Project_Load_Callback --
   ---------------------------

   procedure Project_Load_Callback is
   begin
      --  Update all track settings on project load
      for T in Tracks loop
         Synchronize_Synth_Settings (T);
      end loop;
   end Project_Load_Callback;

   -----------------
   -- Handle_MIDI --
   -----------------

   procedure Handle_MIDI (Msg : MIDI.Message) is
      use MIDI;
      use WNM.Coproc;

      -------------------
      -- Send_To_Synth --
      -------------------

      procedure Send_To_Synth is
         New_Msg : MIDI.Message := Msg;
      begin
         case Mode (Lead_Track) is
         when Synth_Track_Mode_Kind =>
            New_Msg.Chan := Voice_MIDI_Chan (Mode (Lead_Track));

         when MIDI_Mode =>
            New_Msg.Chan := MIDI_Chan (Lead_Track);
         end case;

         WNM.Coproc.Push_To_Synth ((Kind => MIDI_Event,
                                    MIDI_Evt => New_Msg));
      end Send_To_Synth;

   begin
      Send_To_Synth;
   end Handle_MIDI;

   ---------------------
   -- Save_Play_State --
   ---------------------

   procedure Save_Play_State is
   begin
      G_Play_State_Save := G_Play_State;
   end Save_Play_State;

   ------------------------
   -- Restore_Play_State --
   ------------------------

   procedure Restore_Play_State is
   begin
      G_Play_State := G_Play_State_Save;
   end Restore_Play_State;

   ----------
   -- Roll --
   ----------

   procedure Roll (Kind : Roll_Kind) is
   begin
      G_Roll_Next_State := Kind;
   end Roll;

   ----------------
   -- Roll_State --
   ----------------

   function Roll_State return Roll_Kind
   is (G_Roll_State);

   ---------------
   -- Auto_Fill --
   ---------------

   procedure Auto_Fill (Kind : Auto_Fill_Kind) is
   begin
      if Kind = Auto_Buildup and then G_Auto_Fill_State /= Auto_Buildup then
         G_Fill_Buildup_Proba := 1;
      end if;

      G_Auto_Fill_State := Kind;
   end Auto_Fill;

   ---------------------
   -- Auto_Fill_State --
   ---------------------

   function Auto_Fill_State return Auto_Fill_Kind
   is (G_Auto_Fill_State);

   ----------------------
   -- Step_Fill_Toogle --
   ----------------------

   procedure Step_Fill_Toogle is
   begin
      G_Step_Fill := not G_Step_Fill;
   end Step_Fill_Toogle;

   ---------------
   -- Step_Fill --
   ---------------

   function Step_Fill return Boolean
   is (G_Step_Fill);

   -------------
   -- Add_Sat --
   -------------

   function Add_Sat (A, B : Octave_Offset) return Octave_Offset is
      Sum : constant Integer := Integer (A) + Integer (B);
   begin
      if Sum < Integer (Octave_Offset'First) then
         return Octave_Offset'First;
      elsif Sum > Integer (Octave_Offset'Last) then
         return Octave_Offset'Last;
      else
         return Octave_Offset (Sum);
      end if;
   end Add_Sat;

begin
   G_Project.Pattern (Kick, 1) := Hit;
   G_Project.Pattern (Kick, 5) := Hit;

   G_Project.Pattern (Snare, 5) := Hit;

   G_Project.Pattern (Hihat_Closed, 1) := Hit;
   G_Project.Pattern (Hihat_Closed, 2) := Hit;
   G_Project.Pattern (Hihat_Closed, 3) := Accent;
   G_Project.Pattern (Hihat_Closed, 4) := Ghost;
   G_Project.Pattern (Hihat_Closed, 5) := Hit;
   G_Project.Pattern (Hihat_Closed, 6) := Hit;
   G_Project.Pattern (Hihat_Closed, 7) := Accent;
   G_Project.Pattern (Hihat_Closed, 8) := Hit;

   G_Project.Chords (C2).Quality := WNM.Chord_Settings.Min_Triad;
   G_Project.Chords (C3).Quality := WNM.Chord_Settings.Dim_Triad;
   G_Project.Chords (C4).Quality := WNM.Chord_Settings.Maj_7th;
   G_Project.Chords (C5).Quality := WNM.Chord_Settings.Min_7th;
   G_Project.Chords (C6).Quality := WNM.Chord_Settings.Dim_7th;
   G_Project.Chords (C7).Quality := WNM.Chord_Settings.Sus2;
   G_Project.Chords (C8).Quality := WNM.Chord_Settings.Sus4;

end WNM.Project;
