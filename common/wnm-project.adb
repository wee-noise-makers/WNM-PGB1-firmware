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
with WNM.Speech;
with WNM.Sample_Library;
with WNM.UI;

package body WNM.Project is

   -------------
   -- Do_Copy --
   -------------

   procedure Do_Copy (T : in out WNM.Sequence_Copy.Copy_Transaction) is
      use WNM.Sequence_Copy;
   begin
      case T.From.Kind is
         when WNM.Sequence_Copy.Pattern =>
            G_Project.Seqs (T.To.P) := G_Project.Seqs (T.From.P);

            --  Step back in destination address to allow for a new copy
            --  imediately.
            T.To.State := Sequence_Copy.None;
         when Track =>
            G_Project.Seqs (T.To.P) (T.To.T) :=
              G_Project.Seqs (T.From.P) (T.From.T);

            --  Step back in destination address to allow for a new copy
            --  imediately.
            T.To.State := Sequence_Copy.Pattern;
         when Step =>
            G_Project.Seqs (T.To.P) (T.To.T) (T.To.S) :=
              G_Project.Seqs (T.From.P) (T.From.T) (T.From.S);

            --  Step back in destination address to allow for a new copy
            --  imediately.
            T.To.State := Sequence_Copy.Track;
      end case;
   end Do_Copy;

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
      if Res in Beat_Per_Minute then
         G_Project.BPM := Res;
      end if;
   end Change_BPM;

   -------------
   -- Get_BPM --
   -------------

   function Get_BPM return Beat_Per_Minute
   is (G_Project.BPM);

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

   ---------
   -- Set --
   ---------

   function Set (Step : Sequencer_Steps) return Boolean
   is (G_Project.Seqs (Editing_Pattern) (Editing_Track) (Step).Trig /= None);

   ---------
   -- Set --
   ---------

   function Set (Track : Tracks; Step : Sequencer_Steps) return Boolean
   is (G_Project.Seqs (Editing_Pattern) (Track) (Step).Trig /= None);

   -------------
   -- Trigger --
   -------------

   function Trigger (Step : Sequencer_Steps := Editing_Step)
                     return Trigger_Kind
   is (G_Project.Seqs (Editing_Pattern) (Editing_Track) (Step).Trig);

   ------------
   -- Repeat --
   ------------

   function Repeat (Step : Sequencer_Steps := Editing_Step)
                    return Repeat_Cnt
   is (G_Project.Seqs (Editing_Pattern) (Editing_Track) (Step).Repeat);

   -----------------
   -- Repeat_Rate --
   -----------------

   function Repeat_Rate (Step : Sequencer_Steps := Editing_Step)
                         return Repeat_Rate_Kind
   is (G_Project.Seqs (Editing_Pattern) (Editing_Track) (Step).Repeat_Rate);

   ---------------
   -- Note_Mode --
   ---------------

   function Note_Mode (Step : Sequencer_Steps := Editing_Step)
                       return Note_Mode_Kind
   is (G_Project.Seqs (Editing_Pattern) (Editing_Track) (Step).Note_Mode);

   --------------
   -- Note_Img --
   --------------

   function Note_Img (Step : Sequencer_Steps := Editing_Step)
                      return String
   is
      use MIDI;

      S : Step_Rec renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step);

      Chord_Len : constant MIDI.MIDI_Key :=
        MIDI.MIDI_Key (Chord_Settings.Chord_Index_Range'Last) + 1;

      function Oct_Offset return String
      is (case S.Oct is
             when -8 => "-8",
             when -7 => "-7",
             when -6 => "-6",
             when -5 => "-5",
             when -4 => "-4",
             when -3 => "-3",
             when -2 => "-2",
             when -1 => "-1",
             when  0 => " 0",
             when  1 => "+1",
             when  2 => "+2",
             when  3 => "+3",
             when  4 => "+4",
             when  5 => "+5",
             when  6 => "+6",
             when  7 => "+7",
             when  8 => "+8");

   begin
      case S.Note_Mode is
         when Note =>
            return Key_Img (S.Note);
         when Chord =>
            return Oct_Offset & " oct";
         when Note_In_Chord =>
            return
              (case S.Note is
                  when 0 => "#1",
                  when 1 => "#2",
                  when 2 => "#3",
                  when 3 => "#4",
                  when Chord_Len .. MIDI.MIDI_Key'Last =>
                    raise Program_Error) & Oct_Offset;
         when Arp =>
            return "Arp" & Oct_Offset;
      end case;
   end Note_Img;

   --------------
   -- Duration --
   --------------

   function Duration (Step : Sequencer_Steps := Editing_Step)
                      return Note_Duration
   is (G_Project.Seqs (Editing_Pattern) (Editing_Track) (Step).Duration);

   --------------
   -- Velocity --
   --------------

   function Velocity (Step : Sequencer_Steps := Editing_Step)
                      return MIDI.MIDI_Data
   is (G_Project.Seqs (Editing_Pattern) (Editing_Track) (Step).Velo);

   ----------------
   -- CC_Enabled --
   ----------------

   function CC_Enabled (Step : Sequencer_Steps := Editing_Step;
                        Id : CC_Id) return Boolean
   is (G_Project.Seqs (Editing_Pattern) (Editing_Track) (Step).CC_Ena (Id));

   --------------
   -- CC_Value --
   --------------

   function CC_Value (Step : Sequencer_Steps := Editing_Step;
                      Id : CC_Id)
                      return MIDI.MIDI_Data
   is (G_Project.Seqs (Editing_Pattern) (Editing_Track) (Step).CC_Val (Id));

   ---------------
   -- CC_Limits --
   ---------------

   function CC_Limits (Id : CC_Id) return MIDI_Data_Next.Limits is
      use MIDI;
   begin
      if not G_Project.Tracks (Editing_Track).MIDI_Enabled then
         case Mode is
            when Sample1_Mode | Sample2_Mode =>
               if Id = A then
                  return (0,
                          MIDI.MIDI_Data
                            (Sample_Library.Valid_Sample_Index'Last) - 1);
               end if;
            when others =>
               null;
         end case;
      end if;

      return MIDI_Data_Next.Default_Limits;
   end CC_Limits;

   -------------------
   -- Engine_Limits --
   -------------------

   function Engine_Limits return MIDI_Data_Next.Limits is
      use MIDI;
   begin
      case Mode is
         when Lead_Mode =>
            return (MIDI_Data'First, Synth.Lead_Engine_Last);
         when Bass_Mode =>
            return (MIDI_Data'First, Synth.Lead_Engine_Last);
         when Snare_Mode =>
            return (MIDI_Data'First, Synth.Snare_Engine_Last);
         when others =>
            return (MIDI_Data'First, MIDI_Data'First);
      end case;
   end Engine_Limits;

   --------------
   -- CC_Image --
   --------------

   function CC_Image (Step : Sequencer_Steps := Editing_Step;
                      Id : CC_Id)
                      return String
   is
   begin
      case Mode (Editing_Track) is
         when MIDI_Mode | Kick_Mode | Snare_Mode | Cymbal_Mode | Lead_Mode |
              Chord_Mode | Bass_Mode | Reverb_Mode | Filter_Mode |
              Drive_Mode | Bitcrush_Mode =>
            return CC_Value (Step, Id)'Img;

         when Sample1_Mode | Sample2_Mode =>
            return "N/A";

         when Speech_Mode =>
            case Id is
               when A =>
                  declare
                     type Stretch_Img is delta 0.01 range 0.0 .. 10.0;
                     --  Use a fixed point time to get a 'Img without
                     --  scientific notation and only two decimals ^^
                  begin
                     return Stretch_Img
                       (Speech.MIDI_To_Stretch (CC_Value (Step, Id)))'Img;
                  end;
               when others =>
                  return "N/A";
            end case;
      end case;
   end CC_Image;

   --------------------
   -- Note_Mode_Next --
   --------------------

   procedure Note_Mode_Next (Step : Sequencer_Steps := Editing_Step) is
      S : Step_Rec renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step);
   begin
      S.Note_Mode := Next (S.Note_Mode);

      case S.Note_Mode is
         when Note =>
            S.Note := MIDI.C4;
         when Chord | Note_In_Chord | Arp =>
            S.Note := 0;
            S.Oct := 0;
      end case;
   end Note_Mode_Next;

   ---------------
   -- CC_Toggle --
   ---------------

   procedure CC_Toggle (Step : Sequencer_Steps; Id : CC_Id) is
      CC : Boolean renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Ena (Id);
   begin
      CC := not CC;
   end CC_Toggle;

   ---------------
   -- Note_Next --
   ---------------

   procedure Note_Next (Step : Sequencer_Steps) is
      use MIDI;

      S : Step_Rec renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step);
   begin
      case S.Note_Mode is
         when Note =>
            if S.Note /= MIDI_Key'Last then
               S.Note := S.Note + 1;
            end if;

         when Note_In_Chord =>
            if S.Note /= MIDI_Key (Chord_Settings.Chord_Index_Range'Last)
            then
               S.Note := S.Note + 1;
            elsif S.Oct /= Octave_Offset'Last then
               S.Oct := S.Oct + 1;
               S.Note := MIDI_Key (Chord_Settings.Chord_Index_Range'First);
            end if;

         when Chord | Arp =>
            if S.Oct /= Octave_Offset'Last then
               S.Oct := S.Oct + 1;
            end if;
      end case;

   end Note_Next;

   ---------------
   -- Note_Prev --
   ---------------

   procedure Note_Prev (Step : Sequencer_Steps) is
      use MIDI;

      S : Step_Rec renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step);
   begin
      case S.Note_Mode is
         when Note =>
            if S.Note /= MIDI_Key'First then
               S.Note := S.Note - 1;
            end if;

         when Note_In_Chord =>
            if S.Note /= MIDI_Key (Chord_Settings.Chord_Index_Range'First)
            then
               S.Note := S.Note - 1;
            elsif S.Oct /= Octave_Offset'First then
               S.Oct := S.Oct - 1;
               S.Note := MIDI_Key (Chord_Settings.Chord_Index_Range'Last);
            end if;

         when Chord | Arp =>
            if S.Oct /= Octave_Offset'First then
               S.Oct := S.Oct - 1;
            end if;
      end case;
   end Note_Prev;

   ------------------
   -- CC_Value_Inc --
   ------------------

   procedure CC_Value_Inc (Step : Sequencer_Steps;
                           Id   : CC_Id;
                           Fast : Boolean := False)
   is
      use MIDI;

      CC : MIDI.MIDI_Data renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Val (Id);

   begin

      if Fast then
         Next_Fast (CC, CC_Limits (Id));
      else
         Next (CC, CC_Limits (Id));
      end if;

      --  Enable when the value is changed
      G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Ena (Id) := True;
   end CC_Value_Inc;

   ------------------
   -- CC_Value_Dec --
   ------------------

   procedure CC_Value_Dec (Step : Sequencer_Steps;
                           Id   : CC_Id;
                           Fast : Boolean := False)
   is
      use MIDI;

      CC : MIDI.MIDI_Data renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Val (Id);
   begin
      if Fast then
         Prev_Fast (CC, CC_Limits (Id));
      else
         Prev (CC, CC_Limits (Id));
      end if;

      --  Enable when the value is changed
      G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Ena (Id) := True;
   end CC_Value_Dec;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (S : User_Step_Settings) is
      Step : Step_Rec renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Editing_Step);
   begin
      case S is
         when Condition    => Next (Step.Trig);
         when Note         => Note_Next (Editing_Step);
         when Duration     => Next (Step.Duration);
         when Velo         => Next (Step.Velo);
         when Repeat       => Next (Step.Repeat);
         when Repeat_Rate  => Next (Step.Repeat_Rate);
         when CC_A .. CC_D =>
            CC_Value_Inc (Editing_Step,
                          (case S is
                              when CC_A   => A,
                              when CC_B   => B,
                              when CC_C   => C,
                              when others => D));
      end case;
   end Next_Value;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value (S : User_Step_Settings) is
      Step : Step_Rec renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Editing_Step);
   begin
      case S is
         when Condition    => Prev (Step.Trig);
         when Note         => Note_Prev (Editing_Step);
         when Duration     => Prev (Step.Duration);
         when Velo         => Prev (Step.Velo);
         when Repeat       => Prev (Step.Repeat);
         when Repeat_Rate  => Prev (Step.Repeat_Rate);
         when CC_A .. CC_D =>
            CC_Value_Dec (Editing_Step,
                          (case S is
                              when CC_A   => A,
                              when CC_B   => B,
                              when CC_C   => C,
                              when others => D));
      end case;
   end Prev_Value;

   ---------------------
   -- Next_Value_Fast --
   ---------------------

   procedure Next_Value_Fast (S : User_Step_Settings) is
      Step : Step_Rec renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Editing_Step);
   begin
      case S is
         when Condition    => Next_Fast (Step.Trig);
         when Note         => Note_Next (Editing_Step);
         when Duration     => Next_Fast (Step.Duration);
         when Velo         => Next_Fast (Step.Velo);
         when Repeat       => Next_Fast (Step.Repeat);
         when Repeat_Rate  => Next_Fast (Step.Repeat_Rate);
         when CC_A .. CC_D =>
            CC_Value_Inc (Editing_Step,
                          (case S is
                              when CC_A   => A,
                              when CC_B   => B,
                              when CC_C   => C,
                              when others => D),
                          Fast => True);
      end case;
   end Next_Value_Fast;

   ---------------------
   -- Prev_Value_Fast --
   ---------------------

   procedure Prev_Value_Fast (S : User_Step_Settings) is
      Step : Step_Rec renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Editing_Step);
   begin
      case S is
         when Condition    => Prev_Fast (Step.Trig);
         when Note         => Note_Prev (Editing_Step);
         when Duration     => Prev_Fast (Step.Duration);
         when Velo         => Prev_Fast (Step.Velo);
         when Repeat       => Prev_Fast (Step.Repeat);
         when Repeat_Rate  => Prev_Fast (Step.Repeat_Rate);
         when CC_A .. CC_D =>
            CC_Value_Dec (Editing_Step,
                          (case S is
                              when CC_A   => A,
                              when CC_B   => B,
                              when CC_C   => C,
                              when others => D),
                          Fast => True);
      end case;
   end Prev_Value_Fast;

   ----------
   -- Mode --
   ----------

   function Mode (T : Tracks := Editing_Track) return Track_Mode_Kind is
   begin
      if G_Project.Tracks (T).MIDI_Enabled then
         return MIDI_Mode;
      else
         return (case T is
                    when Kick_Track     => Kick_Mode,
                    when Snare_Track    => Snare_Mode,
                    when Cymbal_Track   => Cymbal_Mode,
                    when Bass_Track     => Bass_Mode,
                    when Lead_Track     => Lead_Mode,
                    when Sample1_Track  => Sample1_Mode,
                    when Sample2_Track  => Sample2_Mode,
                    --  when Speech_Track   => Speech_Mode,
                    when Chord_Track    => Chord_Mode,
                    when Reverb_Track   => Reverb_Mode,
                    when Filter_Track   => Filter_Mode,
                    when Drive_Track    => Drive_Mode,
                    when Bitcrush_Track => Bitcrush_Mode,
                    when others         => MIDI_Mode);
      end if;
   end Mode;

   ---------------
   -- MIDI_Chan --
   ---------------

   function MIDI_Chan (T : Tracks := Editing_Track) return MIDI.MIDI_Channel
   is (G_Project.Tracks (T).Chan);

   ----------------
   -- Track_Name --
   ----------------

   function Track_Name (T : Tracks := Editing_Track) return String
   is (case Mode (T) is
          when MIDI_Mode => "MIDI" & MIDI_Chan (T)'Img,
          when others    => Img (Mode (T)));

   ------------------
   -- Track_Volume --
   ------------------

   function Track_Volume (T : Tracks := Editing_Track) return Audio_Volume
   is (G_Project.Tracks (T).Volume);

   ---------------
   -- Track_Pan --
   ---------------

   function Track_Pan (T : Tracks := Editing_Track) return Audio_Pan
   is (G_Project.Tracks (T).Pan);

   -------------------
   -- CC_Controller --
   -------------------

   function CC_Controller (T : Tracks := Editing_Track;
                           Id : CC_Id)
                           return MIDI.MIDI_Data
   is (G_Project.Tracks (T).CC (Id).Controller);

   ----------------
   -- CC_Default --
   ----------------

   function CC_Default (T : Tracks := Editing_Track;
                        Id : CC_Id)
                        return MIDI.MIDI_Data
   is (G_Project.Tracks (T).CC (Id).Value);

   ---------------
   -- Master_FX --
   ---------------

   function Master_FX (T : Tracks := Editing_Track) return FX_Kind
   is (G_Project.Tracks (T).FX);

   --------------
   -- LFO_Rate --
   --------------

   function LFO_Rate (T : Tracks := Editing_Track) return MIDI.MIDI_Data
   is (G_Project.Tracks (T).LFO_Rate);

   -------------
   -- LFO_Amp --
   -------------

   function LFO_Amp (T : Tracks := Editing_Track) return MIDI.MIDI_Data
   is (G_Project.Tracks (T).LFO_Amp);

   ----------------
   -- LFO_Target --
   ----------------

   function LFO_Target (T : Tracks := Editing_Track) return LFO_Target_Kind
   is (G_Project.Tracks (T).LFO_Target);

   ---------------
   -- LFO_Shape --
   ---------------

   function LFO_Shape (T : Tracks := Editing_Track) return LFO_Shape_Kind
   is (G_Project.Tracks (T).LFO_Shape);

   --------------
   -- LFO_Sync --
   --------------

   function LFO_Sync (T : Tracks := Editing_Track) return LFO_Sync_Kind
   is (G_Project.Tracks (T).LFO_Sync);

   --------------
   -- LFO_Loop --
   --------------

   function LFO_Loop (T : Tracks := Editing_Track) return LFO_Loop_Kind
   is (G_Project.Tracks (T).LFO_Loop);

   ------------------
   -- LFO_Amp_Mode --
   ------------------

   function LFO_Amp_Mode (T : Tracks := Editing_Track) return LFO_Amp_Kind
   is (G_Project.Tracks (T).LFO_Amp_Mode);

   ---------------------
   -- CC_Value_To_Use --
   ---------------------

   function CC_Value_To_Use (P : Patterns; T : Tracks; S : Sequencer_Steps;
                             Id : CC_Id)
                             return MIDI.MIDI_Data
   is
      Step : Step_Rec renames G_Project.Seqs (P)(T)(S);
   begin
      if Step.CC_Ena (Id) then
         return Step.CC_Val (Id);
      else
         return G_Project.Tracks (T).CC (Id).Value;
      end if;
   end CC_Value_To_Use;

   -------------------------
   -- CC_Controller_Label --
   -------------------------

   function CC_Controller_Label (T    : Tracks := Editing_Track;
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

         when Cymbal_Mode =>
            Utils.Copy_Str (Synth.Cymbal_Param_Label (Tresses_Id), Result);
            return Result;

         when Chord_Mode =>
            Utils.Copy_Str (Synth.Chord_Param_Label (Tresses_Id), Result);
            return Result;

         when Lead_Mode | Bass_Mode =>
            Utils.Copy_Str (Synth.Lead_Param_Label (Selected_Engine (T),
                                                    Tresses_Id),
                            Result);
            return Result;

         when Sample1_Mode | Sample2_Mode =>
            Utils.Copy_Str (Synth.Sampler_Param_Label (Tresses_Id), Result);
            return Result;

         when Reverb_Mode =>
            Utils.Copy_Str (Synth.Reverb_Param_Label (Tresses_Id), Result);
            return Result;

         when Filter_Mode =>
            Utils.Copy_Str (Synth.Filter_Param_Label (Tresses_Id), Result);
            return Result;

         when Drive_Mode =>
            Utils.Copy_Str (Synth.Drive_Param_Label (Tresses_Id), Result);
            return Result;

         when Bitcrush_Mode =>
            Utils.Copy_Str (Synth.Bitcrush_Param_Label (Tresses_Id), Result);
            return Result;

         when Speech_Mode =>
            Utils.Copy_Str (Synth.Speech_Param_Label (Tresses_Id), Result);
            return Result;
      end case;
   end CC_Controller_Label;

   -------------------------------
   -- CC_Controller_Short_Label --
   -------------------------------

   function CC_Controller_Short_Label (T    : Tracks := Editing_Track;
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

         when Cymbal_Mode =>
            return Synth.Cymbal_Param_Short_Label (Tresses_Id);

         when Chord_Mode =>
            return Synth.Chord_Param_Short_Label (Tresses_Id);

         when Lead_Mode | Bass_Mode =>
            return Synth.Lead_Param_Short_Label (Selected_Engine (T),
                                                 Tresses_Id);

         when Sample1_Mode | Sample2_Mode =>
            return Synth.Sampler_Param_Short_Label (Tresses_Id);

         when Reverb_Mode =>
            return Synth.Reverb_Param_Short_Label (Tresses_Id);

         when Filter_Mode =>
            return Synth.Filter_Param_Short_Label (Tresses_Id);

         when Drive_Mode =>
            return Synth.Drive_Param_Short_Label (Tresses_Id);

         when Bitcrush_Mode =>
            return Synth.Bitcrush_Param_Short_Label (Tresses_Id);

         when Speech_Mode =>
            return Synth.Speech_Param_Short_Label (Tresses_Id);
      end case;
   end CC_Controller_Short_Label;

   ---------------------
   -- Selected_Engine --
   ---------------------

   function Selected_Engine (T : Tracks := Editing_Track)
                             return MIDI.MIDI_Data
   is (G_Project.Tracks (T).Engine);

   -------------------------
   -- Selected_Engine_Img --
   -------------------------

   function Selected_Engine_Img (T : Tracks := Editing_Track)
                                 return String
   is
   begin
      case Mode (T) is
         when Lead_Mode | Bass_Mode =>
            return Synth.Lead_Engine_Img (Selected_Engine (T));
         when Snare_Mode =>
            return Synth.Snare_Engine_Img (Selected_Engine (T));
         when others =>
            return "No Engine";
      end case;
   end Selected_Engine_Img;

   --------------
   -- Arp_Mode --
   --------------

   function Arp_Mode (T : Tracks := Editing_Track) return Arp_Mode_Kind
   is (G_Project.Tracks (T).Arp_Mode);

   ---------------
   -- Arp_Notes --
   ---------------

   function Arp_Notes (T : Tracks := Editing_Track) return Arp_Notes_Kind
   is (G_Project.Tracks (T).Arp_Notes);

   ---------------------
   -- Notes_Per_Chord --
   ---------------------

   function Notes_Per_Chord (T : Tracks := Editing_Track)
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
                       when Reverb     => Synth.FX_Select_Reverb,
                       when Filter     => Synth.FX_Select_Filter),
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

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (S : User_Track_Settings) is
      Track : Track_Rec renames G_Project.Tracks (Editing_Track);
   begin
      case S is
         when Track_Mode      =>
            Next (Track.MIDI_Enabled);
            --  Switching from MIDI to synth, update all settings
            if not Track.MIDI_Enabled then
               Synchronize_Synth_Settings (Editing_Track);
            end if;

         when Engine          => Next (Track.Engine, Engine_Limits);
         when Volume          => Next (Track.Volume);
         when Pan             => Next (Track.Pan);
         when Master_FX       => Next (Track.FX);
         when LFO_Rate        => Next (Track.LFO_Rate);

         when LFO_Amplitude   =>
            Next (Track.LFO_Amp, Track.LFO_Amp_Mode, 1);
            Synchronize_Synth_Setting (Editing_Track, LFO_Amp_Mode);

         when LFO_Shape       =>
            Next (Track.LFO_Shape, Track.LFO_Sync, Track.LFO_Loop);
            Synchronize_Synth_Setting (Editing_Track, LFO_Sync);
            Synchronize_Synth_Setting (Editing_Track, LFO_Loop);

         when LFO_Target      => Next (Track.LFO_Target);
         when Arp_Mode        => Next (Track.Arp_Mode);
         when Arp_Notes       => Next (Track.Arp_Notes);
         when Notes_Per_Chord => Next (Track.Notes_Per_Chord);
         when MIDI_Chan       => Next (Track.Chan);
         when MIDI_Instrument => null;
         when CC_Default_A    => Next (Track.CC (A).Value, CC_Limits (A));
         when CC_Default_B    => Next (Track.CC (B).Value, CC_Limits (B));
         when CC_Default_C    => Next (Track.CC (C).Value, CC_Limits (C));
         when CC_Default_D    => Next (Track.CC (D).Value, CC_Limits (D));
         when CC_Ctrl_A       => Next (Track.CC (A).Controller);
         when CC_Ctrl_B       => Next (Track.CC (B).Controller);
         when CC_Ctrl_C       => Next (Track.CC (C).Controller);
         when CC_Ctrl_D       => Next (Track.CC (D).Controller);
         when CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D => null;
      end case;

      Synchronize_Synth_Setting (Editing_Track, S);

   end Next_Value;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value (S : User_Track_Settings) is
      Track : Track_Rec renames G_Project.Tracks (Editing_Track);
   begin
      case S is
         when Track_Mode      =>
            Prev (Track.MIDI_Enabled);
            --  Switching from MIDI to synth, update all settings
            if not Track.MIDI_Enabled then
               Synchronize_Synth_Settings (Editing_Track);
            end if;

         when Engine          => Prev (Track.Engine, Engine_Limits);
         when Volume          => Prev (Track.Volume);
         when Pan             => Prev (Track.Pan);
         when Master_FX       => Prev (Track.FX);
         when LFO_Rate        => Prev (Track.LFO_Rate);
         when LFO_Amplitude   =>
            Prev (Track.LFO_Amp, Track.LFO_Amp_Mode, 1);
            Synchronize_Synth_Setting (Editing_Track, LFO_Amp_Mode);

         when LFO_Shape       =>
            Prev (Track.LFO_Shape, Track.LFO_Sync, Track.LFO_Loop);
            Synchronize_Synth_Setting (Editing_Track, LFO_Sync);
            Synchronize_Synth_Setting (Editing_Track, LFO_Loop);

         when LFO_Target      => Prev (Track.LFO_Target);
         when Arp_Mode        => Prev (Track.Arp_Mode);
         when Arp_Notes       => Prev (Track.Arp_Notes);
         when Notes_Per_Chord => Prev (Track.Notes_Per_Chord);
         when MIDI_Chan       => Prev (Track.Chan);
         when MIDI_Instrument => null;
         when CC_Default_A    => Prev (Track.CC (A).Value, CC_Limits (A));
         when CC_Default_B    => Prev (Track.CC (B).Value, CC_Limits (B));
         when CC_Default_C    => Prev (Track.CC (C).Value, CC_Limits (C));
         when CC_Default_D    => Prev (Track.CC (D).Value, CC_Limits (D));
         when CC_Ctrl_A       => Prev (Track.CC (A).Controller);
         when CC_Ctrl_B       => Prev (Track.CC (B).Controller);
         when CC_Ctrl_C       => Prev (Track.CC (C).Controller);
         when CC_Ctrl_D       => Prev (Track.CC (D).Controller);
         when CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D => null;
      end case;

      Synchronize_Synth_Setting (Editing_Track, S);
   end Prev_Value;

   ---------------------
   -- Next_Value_Fast --
   ---------------------

   procedure Next_Value_Fast (S : User_Track_Settings) is
      Track : Track_Rec renames G_Project.Tracks (Editing_Track);
   begin
      case S is
         when Track_Mode      =>
            Next_Fast (Track.MIDI_Enabled);
            --  Switching from MIDI to synth, update all settings
            if not Track.MIDI_Enabled then
               Synchronize_Synth_Settings (Editing_Track);
            end if;

         when Engine          => Next_Fast (Track.Engine, Engine_Limits);
         when Volume          => Next_Fast (Track.Volume);
         when Pan             => Next_Fast (Track.Pan);
         when Master_FX       => Next_Fast (Track.FX);
         when LFO_Rate        => Next_Fast (Track.LFO_Rate);

         when LFO_Amplitude   =>
            Next (Track.LFO_Amp, Track.LFO_Amp_Mode, 10);
            Synchronize_Synth_Setting (Editing_Track, LFO_Amp_Mode);

         when LFO_Shape       =>
            Next (Track.LFO_Shape, Track.LFO_Sync, Track.LFO_Loop);
            Synchronize_Synth_Setting (Editing_Track, LFO_Sync);
            Synchronize_Synth_Setting (Editing_Track, LFO_Loop);

         when LFO_Target      => Next_Fast (Track.LFO_Target);
         when Arp_Mode        => Next_Fast (Track.Arp_Mode);
         when Arp_Notes       => Next_Fast (Track.Arp_Notes);
         when Notes_Per_Chord => Next_Fast (Track.Notes_Per_Chord);
         when MIDI_Chan       => Next_Fast (Track.Chan);
         when MIDI_Instrument => null;
         when CC_Default_A    => Next_Fast (Track.CC (A).Value, CC_Limits (A));
         when CC_Default_B    => Next_Fast (Track.CC (B).Value, CC_Limits (B));
         when CC_Default_C    => Next_Fast (Track.CC (C).Value, CC_Limits (C));
         when CC_Default_D    => Next_Fast (Track.CC (D).Value, CC_Limits (D));
         when CC_Ctrl_A       => Next_Fast (Track.CC (A).Controller);
         when CC_Ctrl_B       => Next_Fast (Track.CC (B).Controller);
         when CC_Ctrl_C       => Next_Fast (Track.CC (C).Controller);
         when CC_Ctrl_D       => Next_Fast (Track.CC (D).Controller);
         when CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D => null;
      end case;

      Synchronize_Synth_Setting (Editing_Track, S);
   end Next_Value_Fast;

   ---------------------
   -- Prev_Value_Fast --
   ---------------------

   procedure Prev_Value_Fast (S : User_Track_Settings) is
      Track : Track_Rec renames G_Project.Tracks (Editing_Track);
   begin
      case S is
         when Track_Mode      =>
            Prev_Fast (Track.MIDI_Enabled);
            --  Switching from MIDI to synth, update all settings
            if not Track.MIDI_Enabled then
               Synchronize_Synth_Settings (Editing_Track);
            end if;

         when Engine          => Prev_Fast (Track.Engine, Engine_Limits);
         when Volume          => Prev_Fast (Track.Volume);
         when Pan             => Prev_Fast (Track.Pan);
         when Master_FX       => Prev_Fast (Track.FX);
         when LFO_Rate        => Prev_Fast (Track.LFO_Rate);

         when LFO_Amplitude   =>
            Prev (Track.LFO_Amp, Track.LFO_Amp_Mode, 10);
            Synchronize_Synth_Setting (Editing_Track, LFO_Amp_Mode);

         when LFO_Shape       =>
            Prev (Track.LFO_Shape, Track.LFO_Sync, Track.LFO_Loop);
            Synchronize_Synth_Setting (Editing_Track, LFO_Sync);
            Synchronize_Synth_Setting (Editing_Track, LFO_Loop);

         when LFO_Target      => Prev_Fast (Track.LFO_Target);
         when Arp_Mode        => Prev_Fast (Track.Arp_Mode);
         when Arp_Notes       => Prev_Fast (Track.Arp_Notes);
         when Notes_Per_Chord => Prev_Fast (Track.Notes_Per_Chord);
         when MIDI_Chan       => Prev_Fast (Track.Chan);
         when MIDI_Instrument => null;
         when CC_Default_A    => Prev_Fast (Track.CC (A).Value, CC_Limits (A));
         when CC_Default_B    => Prev_Fast (Track.CC (B).Value, CC_Limits (B));
         when CC_Default_C    => Prev_Fast (Track.CC (C).Value, CC_Limits (C));
         when CC_Default_D    => Prev_Fast (Track.CC (D).Value, CC_Limits (D));
         when CC_Ctrl_A       => Prev_Fast (Track.CC (A).Controller);
         when CC_Ctrl_B       => Prev_Fast (Track.CC (B).Controller);
         when CC_Ctrl_C       => Prev_Fast (Track.CC (C).Controller);
         when CC_Ctrl_D       => Prev_Fast (Track.CC (D).Controller);
         when CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D => null;
      end case;

      Synchronize_Synth_Setting (Editing_Track, S);
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

   --------------------
   -- Selected_Tonic --
   --------------------

   function Selected_Tonic (C : WNM.Chords := Editing_Chord)
                            return MIDI.MIDI_Key
   is (G_Project.Chords (C).Tonic);

   -------------------
   -- Selected_Name --
   -------------------

   function Selected_Name (C : WNM.Chords :=  Editing_Chord)
                           return WNM.Chord_Settings.Chord_Name
   is (G_Project.Chords (C).Name);

   -----------------------
   -- Selected_Duration --
   -----------------------

   function Selected_Duration (C : WNM.Chords :=  Editing_Chord)
                               return Chord_Step_Duration
   is (G_Project.Chords (C).Duration);

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (S : User_Chord_Settings) is
      Chord : Chord_Rec renames G_Project.Chords (Editing_Chord);
   begin
      case S is
         when Tonic    => Next (Chord.Tonic);
         when Name     => Next (Chord.Name);
         when Duration => Next (Chord.Duration);
      end case;
   end Next_Value;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value (S : User_Chord_Settings) is
      Chord : Chord_Rec renames G_Project.Chords (Editing_Chord);
   begin
      case S is
         when Tonic    => Prev (Chord.Tonic);
         when Name     => Prev (Chord.Name);
         when Duration => Prev (Chord.Duration);
      end case;
   end Prev_Value;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value_Fast (S : User_Chord_Settings) is
      Chord : Chord_Rec renames G_Project.Chords (Editing_Chord);
   begin
      case S is
         when Tonic    => Next_Fast (Chord.Tonic);
         when Name     => Next_Fast (Chord.Name);
         when Duration => Next_Fast (Chord.Duration);
      end case;
   end Next_Value_Fast;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value_Fast (S : User_Chord_Settings) is
      Chord : Chord_Rec renames G_Project.Chords (Editing_Chord);
   begin
      case S is
         when Tonic =>    Prev_Fast (Chord.Tonic);
         when Name  =>    Prev_Fast (Chord.Name);
         when Duration => Prev_Fast (Chord.Duration);
      end case;
   end Prev_Value_Fast;

   ---------------------------------
   -- Randomly_Pick_A_Progression --
   ---------------------------------

   procedure Randomly_Pick_A_Progression is
   begin
      --  https://github.com/ldrolez/free-midi-chords
      null; -- TODO..
   end Randomly_Pick_A_Progression;

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
      G_Project := (others => <>);
      for T in Tracks loop
         Synchronize_Synth_Settings (T);
      end loop;
   end Clear;

   -----------------
   -- Handle_MIDI --
   -----------------

   procedure Handle_MIDI (Msg : MIDI.Message) is
      use MIDI;
      use WNM.UI;
      use WNM.Coproc;

      -------------------
      -- Send_To_Synth --
      -------------------

      procedure Send_To_Synth is
         New_Msg : MIDI.Message := Msg;
      begin
         case Mode (Editing_Track) is
         when Synth_Track_Mode_Kind =>
            New_Msg.Chan := Voice_MIDI_Chan (Mode (Editing_Track));

         when MIDI_Mode =>
            New_Msg.Chan := MIDI_Chan (Editing_Track);
         end case;

         WNM.Coproc.Push_To_Synth ((Kind => MIDI_Event,
                                    MIDI_Evt => New_Msg));
      end Send_To_Synth;

   begin
      case Msg.Kind is
         when Note_On =>
            if WNM.UI.Input_Mode = WNM.UI.Step_Mode then
               declare
                  Ed_Step : Step_Rec renames G_Project.Seqs
                    (Editing_Pattern)
                    (Editing_Track)
                    (Editing_Step);
               begin
                  Ed_Step.Note_Mode := Note;
                  Ed_Step.Note := Msg.Key;
                  Ed_Step.Velo := Msg.Velocity;

                  if Ed_Step.Trig = None then
                     Ed_Step.Trig := Always;
                  end if;
               end;
            else
               Send_To_Synth;
            end if;

         when Note_Off =>
            if WNM.UI.Input_Mode /= WNM.UI.Step_Mode then
               Send_To_Synth;
            end if;

         when Continous_Controller =>
            if WNM.UI.Input_Mode = WNM.UI.Step_Mode then
               null; -- TODO: Change track settings from CC messages
            else
               Send_To_Synth;
            end if;
         when others =>
            null;
      end case;
   end Handle_MIDI;

end WNM.Project;
