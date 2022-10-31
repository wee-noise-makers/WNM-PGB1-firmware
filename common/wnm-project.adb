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

with WNM.Project.Chord_Sequencer;
with WNM.Coproc;

package body WNM.Project is

   procedure Update_Coproc_Vol_Pan (T : Tracks);
   --  Send a message to coproc with Volume and pan value for the track

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

   ----------
   -- Note --
   ----------

   function Note (Step : Sequencer_Steps := Editing_Step)
                  return MIDI.MIDI_Key
   is
      S : Step_Rec renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step);
   begin
      case S.Note_Mode is
         when Note =>
            return S.Note;
         when Chord =>
            return S.Note;
         when Note_In_Chord =>
            declare
               use WNM.Chord_Settings;
               use WNM.Project.Chord_Sequencer;
               Chord : constant Chord_Notes := Current_Chord;
            begin
               return Chord (Chord_Index_Range (S.Note));
            end;
         when Arp =>
            return 0;

      end case;
   end Note;

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

   --------------
   -- CC_Image --
   --------------

   function CC_Image (Step : Sequencer_Steps := Editing_Step;
                      Id : CC_Id)
                      return String
   is
   begin
      case Mode (Editing_Track) is
         when MIDI_Mode =>
            return CC_Value (Step, Id)'Img;

         when Sample_Mode =>
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
         when Chord =>
            S.Note := MIDI.MIDI_Key
              (WNM.Chord_Settings.Chord_Index_Range'Last);
         when Note_In_Chord =>
            S.Note := MIDI.MIDI_Key
              (WNM.Chord_Settings.Chord_Index_Range'First);
         when Arp =>
            null;
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
            if S.Note /= MIDI.MIDI_Key'Last then
               S.Note := S.Note + 1;
            end if;

         when Chord | Note_In_Chord =>
            if S.Note /= MIDI_Key (WNM.Chord_Settings.Chord_Index_Range'Last)
            then
               S.Note := S.Note + 1;
            end if;

         when Arp =>
            null;
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
         when Note | Note_In_Chord | Chord =>
            if S.Note /= MIDI.MIDI_Key'First then
               S.Note := S.Note - 1;
            end if;

         when Arp =>
            null;
      end case;
   end Note_Prev;

   ------------------
   -- CC_Value_Inc --
   ------------------

   procedure CC_Value_Inc (Step : Sequencer_Steps; Id : CC_Id) is
      CC : MIDI.MIDI_Data renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Val (Id);
   begin
      if CC /= MIDI.MIDI_Data'Last then
         CC := CC + 1;
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

   procedure CC_Value_Dec (Step : Sequencer_Steps; Id : CC_Id) is
      CC : MIDI.MIDI_Data renames G_Project.Seqs
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Val (Id);
   begin
      if CC /= MIDI.MIDI_Data'First then
         CC := CC - 1;
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
                              when others => D));
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
                              when others => D));
      end case;
   end Prev_Value_Fast;

   ----------
   -- Mode --
   ----------

   function Mode (T : Tracks := Editing_Track) return Track_Mode_Kind
   is (G_Project.Tracks (T).Mode);

   ---------------
   -- MIDI_Chan --
   ---------------

   function MIDI_Chan (T : Tracks := Editing_Track) return MIDI.MIDI_Channel
   is (G_Project.Tracks (T).Chan);

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

   -------------------------
   -- CC_Controller_Label --
   -------------------------

   function CC_Controller_Label (T    : Tracks := Editing_Track;
                                 Id   : CC_Id)
                                 return Controller_Label
   is
   begin
      case Mode (T) is
         when MIDI_Mode =>
            return G_Project.Tracks (T).CC (Id).Label;

         when Sample_Mode =>
            return "Not Applicable   ";

         when Speech_Mode =>
            case Id is
               when A =>
                  return "Time Stretch     ";
               when others =>
                  return "Not Applicable   ";
            end case;
      end case;
   end CC_Controller_Label;

   ---------------------
   -- Selected_Sample --
   ---------------------

   function Selected_Sample (T : Tracks := Editing_Track)
                             return Sample_Library.Valid_Sample_Index
   is (G_Project.Tracks (T).Sample);

   -------------------
   -- Selected_Word --
   -------------------

   function Selected_Word (T : Tracks := Editing_Track) return Speech.Word
   is (G_Project.Tracks (T).Word);

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

   ---------------------------
   -- Update_Coproc_Vol_Pan --
   ---------------------------

   procedure Update_Coproc_Vol_Pan (T : Tracks) is
      use WNM.Coproc;
   begin
      Coproc.Push ((Kind       => Track_Vol_Pan,
                    TVP_Track  => T,
                    TVP_Vol    => G_Project.Tracks (T).Volume,
                    TVP_Pan    => G_Project.Tracks (T).Pan));
   end Update_Coproc_Vol_Pan;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (S : User_Track_Settings) is
      Track : Track_Rec renames G_Project.Tracks (Editing_Track);
   begin
      case S is
         when Track_Mode      => Next (Track.Mode);
         when Sample          => Next (Track.Sample);
         when Speech_Word     => Next (Track.Word);
         when Volume          => Next (Track.Volume);
         when Pan             => Next (Track.Pan);
         when Arp_Mode        => Next (Track.Arp_Mode);
         when Arp_Notes       => Next (Track.Arp_Notes);
         when MIDI_Chan       => Next (Track.Chan);
         when MIDI_Instrument => null;
         when CC_A            => Next (Track.CC (A).Controller);
         when CC_B            => Next (Track.CC (B).Controller);
         when CC_C            => Next (Track.CC (C).Controller);
         when CC_D            => Next (Track.CC (D).Controller);
         when CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D => null;
      end case;

      if S in Pan | Volume then
         Update_Coproc_Vol_Pan (Editing_Track);
      end if;
   end Next_Value;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value (S : User_Track_Settings) is
      Track : Track_Rec renames G_Project.Tracks (Editing_Track);
   begin
      case S is
         when Track_Mode      => Prev (Track.Mode);
         when Sample          => Prev (Track.Sample);
         when Speech_Word     => Prev (Track.Word);
         when Volume          => Prev (Track.Volume);
         when Pan             => Prev (Track.Pan);
         when Arp_Mode        => Prev (Track.Arp_Mode);
         when Arp_Notes       => Prev (Track.Arp_Notes);
         when MIDI_Chan       => Prev (Track.Chan);
         when MIDI_Instrument => null;
         when CC_A            => Prev (Track.CC (A).Controller);
         when CC_B            => Prev (Track.CC (B).Controller);
         when CC_C            => Prev (Track.CC (C).Controller);
         when CC_D            => Prev (Track.CC (D).Controller);
         when CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D => null;
      end case;

      if S in Pan | Volume then
         Update_Coproc_Vol_Pan (Editing_Track);
      end if;
   end Prev_Value;

   ---------------------
   -- Next_Value_Fast --
   ---------------------

   procedure Next_Value_Fast (S : User_Track_Settings) is
      Track : Track_Rec renames G_Project.Tracks (Editing_Track);
   begin
      case S is
         when Track_Mode      => Next_Fast (Track.Mode);
         when Sample          => Next_Fast (Track.Sample);
         when Speech_Word     => Next_Fast (Track.Word);
         when Volume          => Next_Fast (Track.Volume);
         when Pan             => Next_Fast (Track.Pan);
         when Arp_Mode        => Next_Fast (Track.Arp_Mode);
         when Arp_Notes       => Next_Fast (Track.Arp_Notes);
         when MIDI_Chan       => Next_Fast (Track.Chan);
         when MIDI_Instrument => null;
         when CC_A            => Next_Fast (Track.CC (A).Controller);
         when CC_B            => Next_Fast (Track.CC (B).Controller);
         when CC_C            => Next_Fast (Track.CC (C).Controller);
         when CC_D            => Next_Fast (Track.CC (D).Controller);
         when CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D => null;
      end case;
   end Next_Value_Fast;

   ---------------------
   -- Prev_Value_Fast --
   ---------------------

   procedure Prev_Value_Fast (S : User_Track_Settings) is
      Track : Track_Rec renames G_Project.Tracks (Editing_Track);
   begin
      case S is
         when Track_Mode      => Prev_Fast (Track.Mode);
         when Sample          => Prev_Fast (Track.Sample);
         when Speech_Word     => Prev_Fast (Track.Word);
         when Volume          => Prev_Fast (Track.Volume);
         when Pan             => Prev_Fast (Track.Pan);
         when Arp_Mode        => Prev_Fast (Track.Arp_Mode);
         when Arp_Notes       => Prev_Fast (Track.Arp_Notes);
         when MIDI_Chan       => Prev_Fast (Track.Chan);
         when MIDI_Instrument => null;
         when CC_A            => Prev_Fast (Track.CC (A).Controller);
         when CC_B            => Prev_Fast (Track.CC (B).Controller);
         when CC_C            => Prev_Fast (Track.CC (C).Controller);
         when CC_D            => Prev_Fast (Track.CC (D).Controller);
         when CC_Label_A | CC_Label_B | CC_Label_C | CC_Label_D => null;
      end case;
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

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (S : User_Chord_Settings) is
      Chord : Chord_Rec renames G_Project.Chords (Editing_Chord);
   begin
      case S is
         when Tonic => Next (Chord.Tonic);
         when Name => Next (Chord.Name);
      end case;
   end Next_Value;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value (S : User_Chord_Settings) is
      Chord : Chord_Rec renames G_Project.Chords (Editing_Chord);
   begin
      case S is
         when Tonic => Prev (Chord.Tonic);
         when Name  => Prev (Chord.Name);
      end case;
   end Prev_Value;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value_Fast (S : User_Chord_Settings) is
      Chord : Chord_Rec renames G_Project.Chords (Editing_Chord);
   begin
      case S is
         when Tonic => Next_Fast (Chord.Tonic);
         when Name  => Next_Fast (Chord.Name);
      end case;
   end Next_Value_Fast;

   ----------------
   -- Prev_Value --
   ----------------

   procedure Prev_Value_Fast (S : User_Chord_Settings) is
      Chord : Chord_Rec renames G_Project.Chords (Editing_Chord);
   begin
      case S is
         when Tonic => Prev_Fast (Chord.Tonic);
         when Name  => Prev_Fast (Chord.Name);
      end case;
   end Prev_Value_Fast;

   ---------------------------------
   -- Randomly_Pick_A_Progression --
   ---------------------------------

   procedure Randomly_Pick_A_Progression is
   begin
      null; -- TODO..
   end Randomly_Pick_A_Progression;

end WNM.Project;
