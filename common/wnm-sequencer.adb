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

with WNM.Short_Term_Sequencer;
with WNM.Pattern_Sequencer;
with WNM.Arpeggiator;
with WNM.Chord_Sequencer;
with WNM.UI; use WNM.UI;
with WNM.MIDI.Queues;
with HAL;                   use HAL;

package body WNM.Sequencer is

   --  Next_Start : Synth.Sample_Time := Synth.Sample_Time'First;
   Next_Start : Time.Time_Microseconds := Time.Time_Microseconds'First;

   Pattern_Counter : array (Patterns) of UInt32;
   --  Count how many times a pattern has played

   type Microstep_Cnt is mod 2;
   Microstep : Microstep_Cnt := 1;

   procedure Process_Step (Pattern : Patterns; Step : Sequencer_Steps);

   type Sequencer_State_Event is (Play_Event,
                                  Rec_Event,
                                  Rec_Long_Event,
                                  Rec_Release_Event);
   procedure Do_Preview_Trigger (T : Tracks);
   procedure Play_Step (P : Patterns; T : Tracks; S : Sequencer_Steps;
                        Now : Time.Time_Microseconds := Time.Clock);

   --  ----------------
   --  -- Transition --
   --  ----------------
   --
   --  function Transition (State : Sequencer_State;
   --                       Evt   : Sequencer_State_Event)
   --                       return Sequencer_State
   --  is
   --    (case State is
   --        when Pause =>        (case Evt is
   --                                 when Play_Event        => Play,
   --                                 when Rec_Event         => Edit,
   --                                 when Rec_Long_Event    => Pause,
   --                                 when Rec_Release_Event => Pause),
   --        when Play =>         (case Evt is
   --                                 when Play_Event        => Pause,
   --                                 when Rec_Event         => Play_And_Edit,
   --                                 when Rec_Long_Event    => Play_And_Rec,
   --                                 when Rec_Release_Event => Play),
   --        when Edit =>         (case Evt is
   --                                 when Play_Event        => Play_And_Edit,
   --                                 when Rec_Event         => Pause,
   --                                 when Rec_Long_Event    => Edit,
   --                                 when Rec_Release_Event => Edit),
   --        when Play_And_Edit =>         (case Evt is
   --                                 when Play_Event        => Edit,
   --                                 when Rec_Event         => Play,
   --                                 when Rec_Long_Event    => Play_And_Rec,
   --                                 when Rec_Release_Event => Play_And_Edit),
   --        when Play_And_Rec => (case Evt is
   --                                 when Play_Event        => Pause,
   --                                 when Rec_Event         => Play_And_Rec,
   --                                 when Rec_Long_Event    => Play_And_Rec,
   --                                 when Rec_Release_Event => Play));

   ------------------
   -- Playing_Step --
   ------------------

   function Playing_Step return Sequencer_Steps
   is (Current_Playing_Step);

   ---------------------
   -- Playing_Pattern --
   ---------------------

   function Playing_Pattern return Patterns
   is (Pattern_Sequencer.Playing_Pattern);

   -------------------
   -- Editing_Step --
   -------------------

   function Editing_Step return Sequencer_Steps
   is (Current_Editing_Step);

   --------------------
   -- Editing_Track --
   --------------------

   function Editing_Track return Tracks
   is (Current_Editing_Track);

   ----------------------
   -- Editing_Pattern --
   ----------------------

   function Editing_Pattern return Patterns
   is (Current_Editing_Pattern);

   procedure Set_Editing_Step    (S : Sequencer_Steps) is
   begin
      Current_Editing_Step := S;
   end Set_Editing_Step;

   -----------------------
   -- Set_Editing_Track --
   -----------------------

   procedure Set_Editing_Track   (T : Tracks) is
   begin
      Current_Editing_Track := T;
   end Set_Editing_Track;

   -------------------------
   -- Set_Editing_Pattern --
   -------------------------

   procedure Set_Editing_Pattern (P : Patterns) is
   begin
      Current_Editing_Pattern := P;
   end Set_Editing_Pattern;

   ------------------------
   -- Do_Preview_Trigger --
   ------------------------

   procedure Do_Preview_Trigger (T : Tracks) is
      use WNM.Synth;

      Channel : constant MIDI.MIDI_Channel :=
        Track_Settings (T).Chan;

   begin
      WNM.MIDI.Queues.Sequencer_Push
        ((MIDI.Note_On,
         Channel,
         MIDI.C4,
         MIDI.MIDI_Data'Last));
      WNM.Short_Term_Sequencer.Push
        ((MIDI.Note_Off,
         Channel,
         MIDI.C4,
         0),
         Time.Clock + Microseconds_Per_Beat);
   end Do_Preview_Trigger;

--      Octave : MIDI.Octaves := 4;

--     function To_Key (B : Keyboard_Buttons; Oct : MIDI.Octaves)
--  return MIDI.MIDI_Key is
--       (case B is
--           when B9  => MIDI.Key (Oct, MIDI.C),
--           when B2  => MIDI.Key (Oct, MIDI.Cs),
--           when B10 => MIDI.Key (Oct, MIDI.D),
--           when B3  => MIDI.Key (Oct, MIDI.Ds),
--           when B11 => MIDI.Key (Oct, MIDI.E),
--           when B12 => MIDI.Key (Oct, MIDI.F),
--           when B5  => MIDI.Key (Oct, MIDI.Fs),
--           when B13 => MIDI.Key (Oct, MIDI.G),
--           when B6  => MIDI.Key (Oct, MIDI.Gs),
--           when B14 => MIDI.Key (Oct, MIDI.A),
--           when B7  => MIDI.Key (Oct, MIDI.As),
--           when B15 => MIDI.Key (Oct, MIDI.B),
--           when B16 => MIDI.Key (Oct + 1, MIDI.C),
--           when others => MIDI.A0);

   ----------------
   -- Play_Pause --
   ----------------

   procedure Play_Pause is
   begin
      if not Pattern_Sequencer.Playing then
         Current_Playing_Step := Sequencer_Steps'First;
         Microstep := 1;
         Execute_Step;
      end if;

      Pattern_Sequencer.Play_Pause;

   end Play_Pause;

   --------------
   -- On_Press --
   --------------

   procedure On_Press (Button : Keyboard_Button;
                       Mode   : WNM.UI.Main_Modes)
   is
      V : constant Keyboard_Value := To_Value (Button);
   begin
      case Mode is
         when UI.Pattern_Mode =>
            Current_Editing_Pattern := V;
            Pattern_Sequencer.On_Press (Button, Mode);

         when UI.Track_Mode | UI.Step_Mode =>

            if Mode = UI.Track_Mode and then not UI.Recording then
               Current_Editing_Track := V;
            else
               Current_Editing_Step := V;
            end if;

            --  if UI.Recording and then Pattern_Sequencer.Playing then
            --
            --     --  Live record the trigger
            --     Sequences (Current_Editing_Pattern) (V) (Current_Playing_Step).Trig
            --       := Always;
            --
            --     if Microstep /= 1 then
            --        --  If user play later than the step time, play a preview
            --        Do_Preview_Trigger (V);
            --     end if;
            --  else
            --     Do_Preview_Trigger (V);
            --  end if;

            if UI.Recording then
               declare
                  S : Step_Rec renames Sequences
                    (Editing_Pattern)
                    (Editing_Track)
                    (To_Value (Button));
               begin
                  if S.Trig /= None then
                     S.Trig := None;
                  else
                     S.Trig := Always;
                  end if;
               end;
            else
               if Mode = UI.Step_Mode then
                  Play_Step (Editing_Pattern,
                             Editing_Track,
                             To_Value (Button));
               else
                  Do_Preview_Trigger (V);

               end if;
            end if;
      end case;
   end On_Press;

   ----------------
   -- On_Release --
   ----------------

   procedure On_Release (Button : Keyboard_Button;
                         Mode   : WNM.UI.Main_Modes)
   is
   begin
      Pattern_Sequencer.On_Release (Button, Mode);
   end On_Release;

   --------------------
   -- Select_Channel --
   --------------------

   procedure Select_Track (Track : Tracks) is
   begin
      Current_Track := Track;
   end Select_Track;

   -------------
   -- Do_Copy --
   -------------

   procedure Do_Copy (T : in out WNM.Sequence_Copy.Copy_Transaction) is
      use WNM.Sequence_Copy;
   begin
      case T.From.Kind is
         when WNM.Sequence_Copy.Pattern =>
            Sequences (T.To.P) := Sequences (T.From.P);

            --  Step back in destination address to allow for a new copy
            --  imediately.
            T.To.State := Sequence_Copy.None;
         when Track =>
            Sequences (T.To.P) (T.To.T) :=
              Sequences (T.From.P) (T.From.T);

            --  Step back in destination address to allow for a new copy
            --  imediately.
            T.To.State := Sequence_Copy.Pattern;
         when Step =>
            Sequences (T.To.P) (T.To.T) (T.To.S) :=
              Sequences (T.From.P) (T.From.T) (T.From.S);

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
      Sequencer_BPM := BPM;
   end Set_BPM;

   ----------------
   -- Change_BPM --
   ----------------

   procedure Change_BPM (BPM_Delta : Integer) is
      Res : Integer;
   begin
      Res := Integer (Sequencer_BPM) + BPM_Delta;
      if Res in Beat_Per_Minute then
         Sequencer_BPM := Res;
      end if;
   end Change_BPM;

   ---------
   -- BPM --
   ---------

   function BPM return Beat_Per_Minute
   is (Sequencer_BPM);

   ----------------------
   -- Samples_Per_Beat --
   ----------------------

   function Samples_Per_Beat return Synth.Sample_Time is
      use Synth;

      Samples_Per_Minute : constant Sample_Time := 60 * Sample_Frequency;
   begin
      return Samples_Per_Minute / Sample_Time (Sequencer_BPM);
   end Samples_Per_Beat;

   ---------------------------
   -- Microseconds_Per_Beat --
   ---------------------------

   function Microseconds_Per_Beat return Time.Time_Microseconds is
   begin
      return (60 * 1_000 * 1_000) / Time.Time_Microseconds (Sequencer_BPM);
   end Microseconds_Per_Beat;

   ---------------
   -- MIDI_Play --
   ---------------

   procedure MIDI_Play (Key         : MIDI.MIDI_Key;
                        Channel     : MIDI.MIDI_Channel;
                        Velo        : MIDI.MIDI_Data;
                        Rep         : WNM.Repeat;
                        Now         : Time.Time_Microseconds;
                        Duration    : Time.Time_Microseconds;
                        Repeat_Span : Time.Time_Microseconds)
   is
      Repeat_Time : Time.Time_Microseconds := Now;
   begin
      WNM.MIDI.Queues.Sequencer_Push
        ((MIDI.Note_On,
         Channel,
         Key,
         Velo));

      WNM.Short_Term_Sequencer.Push
        ((MIDI.Note_Off,
         Channel,
         Key,
         0),
         Now + Duration);

      for X in 1 .. Rep loop
         Repeat_Time := Repeat_Time + Repeat_Span;

         WNM.Short_Term_Sequencer.Push
           ((MIDI.Note_On,
            Channel,
            Key,
            Velo),
            Repeat_Time);
         WNM.Short_Term_Sequencer.Push
           ((MIDI.Note_Off,
            Channel,
            Key,
            0),
            Repeat_Time + Duration);
      end loop;

   end MIDI_Play;

   -------------------
   -- MIDI_Play_Arp --
   -------------------

   procedure MIDI_Play_Arp (T           : Tracks;
                            Channel     : MIDI.MIDI_Channel;
                            Velo        : MIDI.MIDI_Data;
                            Rep         : WNM.Repeat;
                            Now         : Time.Time_Microseconds;
                            Duration    : Time.Time_Microseconds;
                            Repeat_Span : Time.Time_Microseconds)
   is
      Repeat_Time : Time.Time_Microseconds := Now;
      Key : MIDI.MIDI_Key := Arpeggiator.Next_Note (T);
   begin
      WNM.MIDI.Queues.Sequencer_Push
        ((MIDI.Note_On,
         Channel,
         Key,
         Velo));

      WNM.Short_Term_Sequencer.Push
        ((MIDI.Note_Off,
         Channel,
         Key,
         0),
         Now + Duration);

      for X in 1 .. Rep loop
         Repeat_Time := Repeat_Time + Repeat_Span;
         Key := Arpeggiator.Next_Note (T);

         WNM.Short_Term_Sequencer.Push
           ((MIDI.Note_On,
            Channel,
            Key,
            Velo),
            Repeat_Time);
         WNM.Short_Term_Sequencer.Push
           ((MIDI.Note_Off,
            Channel,
            Key,
            0),
            Repeat_Time + Duration);
      end loop;

   end MIDI_Play_Arp;

   ---------------------
   -- MIDI_Play_Chord --
   ---------------------

   procedure MIDI_Play_Chord (Chord       : Chord_Sequencer.Chord_Notes;
                              Last_Note   : Chord_Sequencer.Chord_Index_Range;
                              Channel     : MIDI.MIDI_Channel;
                              Velo        : MIDI.MIDI_Data;
                              Rep         : WNM.Repeat;
                              Now         : Time.Time_Microseconds;
                              Duration    : Time.Time_Microseconds;
                              Repeat_Span : Time.Time_Microseconds)
   is
      Repeat_Time : Time.Time_Microseconds := Now;
   begin
      for X in Chord'First .. Last_Note loop
         WNM.MIDI.Queues.Sequencer_Push
           ((MIDI.Note_On,
            Channel,
            Chord (X),
            Velo));

         WNM.Short_Term_Sequencer.Push
           ((MIDI.Note_Off,
            Channel,
            Chord (X),
            0),
            Now + Duration);
      end loop;

      for X in 1 .. Rep loop
         Repeat_Time := Repeat_Time + Repeat_Span;

         for X in Chord'First .. Last_Note loop
            WNM.Short_Term_Sequencer.Push
              ((MIDI.Note_On,
               Channel,
               Chord (X),
               Velo),
               Repeat_Time);
            WNM.Short_Term_Sequencer.Push
              ((MIDI.Note_Off,
               Channel,
               Chord (X),
               0),
               Repeat_Time + Duration);
         end loop;
      end loop;
   end MIDI_Play_Chord;

   ---------------
   -- Play_Step --
   ---------------

   procedure Play_Step (P : Patterns; T : Tracks; S : Sequencer_Steps;
                        Now : Time.Time_Microseconds := Time.Clock)
   is
      use Chord_Sequencer;
      use MIDI;

      Step : Step_Rec renames Sequences (P) (T) (S);

      Note_Duration : constant Time.Time_Microseconds :=
        (case Step.Duration
         is
            when Double  => Microseconds_Per_Beat * 2,
            when Whole   => Microseconds_Per_Beat,
            when Half    => Microseconds_Per_Beat / 2,
            when Quarter => Microseconds_Per_Beat / 4,
            when N_8th   => Microseconds_Per_Beat / 8,
            when N_16th  => Microseconds_Per_Beat / 16,
            when N_32nd  => Microseconds_Per_Beat / 32);

      Channel : constant MIDI.MIDI_Channel :=  Track_Settings (T).Chan;

      Repeat_Span : constant Time.Time_Microseconds :=
        Microseconds_Per_Beat / (case Step.Repeat_Rate
                                 is
                                    when Rate_1_2  => 2,
                                    when Rate_1_3  => 3,
                                    when Rate_1_4  => 4,
                                    when Rate_1_5  => 5,
                                    when Rate_1_6  => 6,
                                    when Rate_1_8  => 8,
                                    when Rate_1_10 => 10,
                                    when Rate_1_12 => 12,
                                    when Rate_1_16 => 16,
                                    when Rate_1_20 => 20,
                                    when Rate_1_24 => 24,
                                    when Rate_1_32 => 32);

      Repeat_Duration : constant Time.Time_Microseconds :=
        (if Step.Repeat /= 0 and then Repeat_Span < Note_Duration
         then Repeat_Span
         else Note_Duration);

   begin

      case Step.Note_Mode is
         when Note =>
            MIDI_Play (Step.Note,
                       Channel, Step.Velo, Step.Repeat,
                       Now, Repeat_Duration, Repeat_Span);

         when Note_In_Chord =>

            MIDI_Play (Current_Chord (Chord_Index_Range (Step.Note)),
                       Channel, Step.Velo, Step.Repeat,
                       Now, Repeat_Duration, Repeat_Span);

         when Note_In_Scale =>

            MIDI_Play (Current_Scale (Scale_Range (Step.Note)),
                       Channel, Step.Velo, Step.Repeat,
                       Now, Repeat_Duration, Repeat_Span);

         when Arp =>

            MIDI_Play_Arp (T,
                           Channel, Step.Velo, Step.Repeat,
                           Now, Repeat_Duration, Repeat_Span);

         when Chord =>

            MIDI_Play_Chord
              (Chord_Sequencer.Current_Chord,
               Chord_Index_Range (Integer'Min (Integer (Chord_Index_Range'Last),
                 Integer (Step.Note))),

               Channel, Step.Velo, Step.Repeat,
               Now, Repeat_Duration, Repeat_Span);
      end case;

   end Play_Step;

   ------------------
   -- Process_Step --
   ------------------

   procedure Process_Step (Pattern : Patterns;
                           Step : Sequencer_Steps)
   is
      use Synth;

      Condition : Boolean := False;

      Now : constant Time.Time_Microseconds := Time.Clock;
      Note_Duration : constant Time.Time_Microseconds :=
        Microseconds_Per_Beat / 4;

      --  Now : constant Sample_Time := Sample_Clock;
      --  Note_Duration : constant Sample_Time := Samples_Per_Beat / 4;
   begin
      if Step = Sequencer_Steps'First then
         Pattern_Counter (Pattern) := Pattern_Counter (Pattern) + 1;
      end if;

      for Track in Tracks loop
         declare
            S : Step_Rec renames Sequences (Pattern) (Track) (Step);
         begin
            case S.Trig is
            when None =>
               Condition := False;
            when Always =>
               Condition := True;
            when Fill =>
               Condition := WNM.UI.FX_On (B1);
            when Percent_25 =>
               Condition := Random <= 25;
            when Percent_50 =>
               Condition := Random <= 50;
            when Percent_75 =>
               Condition := Random <= 75;
            when One_Of_Two =>
               Condition := Pattern_Counter (Pattern) mod 2 = 0;
            when One_Of_Three =>
               Condition := Pattern_Counter (Pattern) mod 3 = 0;
            end case;

            --  Send CC first
            declare
               Channel : constant MIDI.MIDI_Channel :=
                 Track_Settings (Track).Chan;
            begin
               for Id in CC_Id loop
                  if S.CC_Ena (Id) then
                     WNM.MIDI.Queues.Sequencer_Push
                       ((MIDI.Continous_Controller,
                        Channel,
                        Track_Settings (Track).CC (Id).Controller,
                        S.CC_Val (Id)));
                  end if;
               end loop;
            end;

            --  Play note
            if Condition and then not UI.Muted (Track) then
               Play_Step (Pattern, Track, Step, Now);
            end if;

         end;
      end loop;
   end Process_Step;

   ------------------
   -- Execute_Step --
   ------------------

   procedure Execute_Step is
      use Synth;
   begin

      Next_Start := Next_Start + (Microseconds_Per_Beat / Steps_Per_Beat) / 2;

      if Pattern_Sequencer.Playing then
         case Microstep is

            --  Begining of a new step
            when 0 =>

               if Current_Playing_Step /= Sequencer_Steps'Last then

                  if Current_Playing_Step = 8 then
                     Pattern_Sequencer.Signal_Mid_Pattern;
                  end if;

                  Current_Playing_Step := Current_Playing_Step + 1;
               else
                  Current_Playing_Step := Sequencer_Steps'First;
                  Pattern_Sequencer.Signal_End_Of_Pattern;
               end if;

               --  At the middle of the step we play the recorded notes
            when 1 =>
               Process_Step (Pattern_Sequencer.Playing_Pattern,
                             Playing_Step);
         end case;

         Microstep := Microstep + 1;
      end if;
   end Execute_Step;

   ------------
   -- Update --
   ------------

   function Update return Time.Time_Microseconds is
      use Synth;

      Now : constant Time.Time_Microseconds := Time.Clock;
      Success : Boolean;
      Msg     : WNM.MIDI.Message;
   begin
      if Now >= Next_Start then
         Execute_Step;
      end if;

      loop
         WNM.Short_Term_Sequencer.Pop (Now, Msg, Success);
         exit when not Success;

         WNM.MIDI.Queues.Sequencer_Push (Msg);
      end loop;

      return Time.Time_Microseconds'First;
   end Update;

   ---------
   -- Set --
   ---------

   function Set (Step : Sequencer_Steps) return Boolean is
   begin
      return Sequences (Editing_Pattern) (Editing_Track) (Step).Trig /= None;
   end Set;

   ---------
   -- Set --
   ---------

   function Set (Track : Tracks; Step : Sequencer_Steps) return Boolean is
   begin
      return Sequences (Editing_Pattern) (Track) (Step).Trig /= None;
   end Set;

   ----------
   -- Trig --
   ----------

   function Trig (Step : Sequencer_Steps) return Trigger is
   begin
      return Sequences (Editing_Pattern) (Editing_Track) (Step).Trig;
   end Trig;

   ---------------
   -- Trig_Next --
   ---------------

   procedure Trig_Next (Step : Sequencer_Steps) is
   begin
      Next (Sequences (Editing_Pattern)(Editing_Track) (Step).Trig);
   end Trig_Next;

   ---------------
   -- Trig_Prev --
   ---------------

   procedure Trig_Prev (Step : Sequencer_Steps) is
   begin
      Prev (Sequences (Editing_Pattern)(Editing_Track) (Step).Trig);
   end Trig_Prev;

   ------------
   -- Repeat --
   ------------

   function Repeat (Step : Sequencer_Steps) return WNM.Repeat is
   begin
      return Sequences
        (Editing_Pattern) (Editing_Track) (Step).Repeat;
   end Repeat;

   -----------------
   -- Repeat_Next --
   -----------------

   procedure Repeat_Next (Step : Sequencer_Steps) is
   begin
      Next (Sequences
            (Editing_Pattern) (Editing_Track) (Step).Repeat);
   end Repeat_Next;

   -----------------
   -- Repeat_Prev --
   -----------------

   procedure Repeat_Prev (Step : Sequencer_Steps) is
   begin
      Prev (Sequences
            (Editing_Pattern) (Editing_Track) (Step).Repeat);
   end Repeat_Prev;

   -----------------
   -- Repeat_Rate --
   -----------------

   function Repeat_Rate (Step : Sequencer_Steps) return WNM.Repeat_Rate is
   begin
      return Sequences
        (Editing_Pattern) (Editing_Track) (Step).Repeat_Rate;
   end Repeat_Rate;

   ----------------------
   -- Repeat_Rate_Next --
   ----------------------

   procedure Repeat_Rate_Next (Step : Sequencer_Steps) is
   begin
      Next (Sequences
            (Editing_Pattern)
            (Editing_Track)
            (Step).Repeat_Rate);
   end Repeat_Rate_Next;

   ----------------------
   -- Repeat_Rate_Prev --
   ----------------------

   procedure Repeat_Rate_Prev (Step : Sequencer_Steps) is
   begin
      Prev (Sequences
            (Editing_Pattern)
            (Editing_Track)
            (Step).Repeat_Rate);
   end Repeat_Rate_Prev;

   ---------------
   -- Note_Mode --
   ---------------

   function Note_Mode (Step : Sequencer_Steps) return Note_Mode_Kind is
   begin
      return Sequences
        (Editing_Pattern) (Editing_Track) (Step).Note_Mode;
   end Note_Mode;

   --------------------
   -- Note_Mode_Next --
   --------------------

   procedure Note_Mode_Next (Step : Sequencer_Steps) is
      S : Step_Rec renames Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step);
   begin
      S.Note_Mode := Next (S.Note_Mode);

      case S.Note_Mode is
         when Note =>
            S.Note := MIDI.C4;
         when Chord =>
            S.Note := MIDI.MIDI_Key (Chord_Sequencer.Chord_Index_Range'Last);
         when Note_In_Chord =>
            S.Note := MIDI.MIDI_Key (Chord_Sequencer.Chord_Index_Range'First);
         when Note_In_Scale =>
            S.Note := MIDI.MIDI_Key (Chord_Sequencer.Scale_Range'First);
         when Arp =>
            null;
      end case;
   end Note_Mode_Next;

   ----------
   -- Note --
   ----------

   function Note (Step : Sequencer_Steps) return MIDI.MIDI_Key is
      S : Step_Rec renames Sequences
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
               use Chord_Sequencer;
               Chord : constant Chord_Notes := Current_Chord;
            begin
               return Chord (Chord_Index_Range (S.Note));
            end;
         when Note_In_Scale =>
            declare
               use Chord_Sequencer;
               Scale : constant Scale_Notes := Current_Scale;
            begin
               return Scale (Scale_Range (S.Note));
            end;

         when Arp =>
            return 0;

      end case;
   end Note;

   ---------------
   -- Note_Next --
   ---------------

   procedure Note_Next (Step : Sequencer_Steps) is
      use MIDI;

      S : Step_Rec renames Sequences
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
            if S.Note /= MIDI_Key (Chord_Sequencer.Chord_Index_Range'Last) then
               S.Note := S.Note + 1;
            end if;

         when Note_In_Scale =>
            if S.Note /= MIDI_Key (Chord_Sequencer.Scale_Range'Last) then
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

      S : Step_Rec renames Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step);
   begin
      case S.Note_Mode is
         when Note | Note_In_Chord | Chord | Note_In_Scale =>
            if S.Note /= MIDI.MIDI_Key'First then
               S.Note := S.Note - 1;
            end if;

         when Arp =>
            null;
      end case;
   end Note_Prev;

   -------------------
   -- Note_Duration --
   -------------------

   function Duration (Step : Sequencer_Steps) return Note_Duration is
   begin
      return Sequences (Editing_Pattern) (Editing_Track) (Step).Duration;
   end Duration;

   -------------------
   -- Duration_Next --
   -------------------

   procedure Duration_Next (Step : Sequencer_Steps) is
   begin
      Next (Sequences
            (Editing_Pattern)
            (Editing_Track)
            (Step).Duration);
   end Duration_Next;

   -------------------
   -- Duration_Prev --
   -------------------

   procedure Duration_Prev (Step : Sequencer_Steps) is
   begin
      Prev (Sequences
            (Editing_Pattern)
            (Editing_Track)
            (Step).Duration);
   end Duration_Prev;

   ----------
   -- Velo --
   ----------

   function Velo (Step : Sequencer_Steps) return MIDI.MIDI_Data is
      V : MIDI.MIDI_Data renames Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step).Velo;
   begin
      return V;
   end Velo;

   ---------------
   -- Velo_Next --
   ---------------

   procedure Velo_Next (Step : Sequencer_Steps) is
      V : MIDI.MIDI_Data renames Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step).Velo;
   begin
      if V /= MIDI.MIDI_Data'Last then
         V := V + 1;
      end if;
   end Velo_Next;

   ---------------
   -- Velo_Prev --
   ---------------

   procedure Velo_Prev (Step : Sequencer_Steps) is
      V : MIDI.MIDI_Data renames Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step).Velo;
   begin
      if V /= MIDI.MIDI_Data'First then
         V := V - 1;
      end if;
   end Velo_Prev;

   ---------------
   -- MIDI_Chan --
   ---------------

   function MIDI_Chan (T : Tracks) return MIDI.MIDI_Channel is
   begin
      return Track_Settings (T).Chan;
   end MIDI_Chan;

   --------------------
   -- MIDI_Chan_Next --
   --------------------

   procedure MIDI_Chan_Next (T : Tracks) is
      use MIDI;
   begin
      if Track_Settings (T).Chan /= MIDI.MIDI_Channel'Last then
         Track_Settings (T).Chan :=  Track_Settings (T).Chan + 1;
      end if;
   end MIDI_Chan_Next;

   --------------------
   -- MIDI_Chan_Prev --
   --------------------

   procedure MIDI_Chan_Prev (T : Tracks) is
      use MIDI;
   begin
      if Track_Settings (T).Chan /= MIDI.MIDI_Channel'First then
         Track_Settings (T).Chan :=  Track_Settings (T).Chan - 1;
      end if;
   end MIDI_Chan_Prev;

   -------------------
   -- CC_Controller --
   -------------------

   function CC_Controller (T : Tracks; Id : CC_Id) return MIDI.MIDI_Data
   is (Track_Settings (T).CC (Id).Controller);

   -----------------------
   -- Set_CC_Controller --
   -----------------------

   procedure Set_CC_Controller (T : Tracks; Id : CC_Id; C : MIDI.MIDI_Data) is
   begin
      Track_Settings (T).CC (Id).Controller := C;
   end Set_CC_Controller;

   ------------------------
   -- CC_Controller_Next --
   ------------------------

   procedure CC_Controller_Next (T : Tracks; Id : CC_Id) is
      use MIDI;

      Controller : MIDI_Data renames Track_Settings (T).CC (Id).Controller;
   begin
      if Controller /= MIDI_Data'Last then
         Controller := Controller + 1;
      end if;
   end CC_Controller_Next;

   ------------------------
   -- CC_Controller_Prev --
   ------------------------

   procedure CC_Controller_Prev (T : Tracks; Id : CC_Id) is
      use MIDI;

      Controller : MIDI_Data renames Track_Settings (T).CC (Id).Controller;
   begin
      if Controller /= MIDI_Data'First then
         Controller := Controller - 1;
      end if;
   end CC_Controller_Prev;

   ----------------------------
   -- Set_CC_Controller_Name --
   ----------------------------

   procedure Set_CC_Controller_Label (T     : Tracks;
                                      Id    : CC_Id;
                                      Label : Controller_Label)
   is
   begin
      Track_Settings (T).CC (Id).Label := Label;
   end Set_CC_Controller_Label;

   -------------------------
   -- CC_Controller_Label --
   -------------------------

   function CC_Controller_Label (T    : Tracks;
                                 Id   : CC_Id)
                                 return Controller_Label
   is (Track_Settings (T).CC (Id).Label);

   ----------------
   -- CC_Enabled --
   ----------------

   function CC_Enabled (Step : Sequencer_Steps; Id : CC_Id) return Boolean is
   begin
      return Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Ena (Id);
   end CC_Enabled;

   ---------------
   -- CC_Toggle --
   ---------------

   procedure CC_Toggle (Step : Sequencer_Steps; Id : CC_Id) is
      CC : Boolean renames Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Ena (Id);
   begin
      CC := not CC;
   end CC_Toggle;

   --------------
   -- CC_Value --
   --------------

   function CC_Value (Step : Sequencer_Steps; Id : CC_Id) return MIDI.MIDI_Data
   is
   begin
      return Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Val (Id);
   end CC_Value;

   ------------------
   -- CC_Value_Inc --
   ------------------

   procedure CC_Value_Inc (Step : Sequencer_Steps; Id : CC_Id) is
      CC : MIDI.MIDI_Data renames Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Val (Id);
   begin
      if CC /= MIDI.MIDI_Data'Last then
         CC := CC + 1;
      end if;

      --  Enable when the value is changed
      Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Ena (Id) := True;
   end CC_Value_Inc;

   ------------------
   -- CC_Value_Dec --
   ------------------

   procedure CC_Value_Dec (Step : Sequencer_Steps; Id : CC_Id) is
      CC : MIDI.MIDI_Data renames Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Val (Id);
   begin
      if CC /= MIDI.MIDI_Data'First then
         CC := CC - 1;
      end if;

      --  Enable when the value is changed
      Sequences
        (Editing_Pattern)
        (Editing_Track)
        (Step).CC_Ena (Id) := True;
   end CC_Value_Dec;

   ---------------------
   -- Selected_Sample --
   ---------------------

   function Selected_Sample (T : Tracks)
                             return Sample_Library.Valid_Sample_Index
   is
   begin
      return Track_Settings (T).Sample;
   end Selected_Sample;

   -----------------
   -- Next_Sample --
   -----------------

   procedure Next_Sample (T : Tracks) is
   begin
      if Selected_Sample (T) /= Sample_Library.Valid_Sample_Index'Last then
         Track_Settings (T).Sample := Track_Settings (T).Sample + 1;
      end if;
   end Next_Sample;

   -----------------
   -- Prev_Sample --
   -----------------

   procedure Prev_Sample (T : Tracks) is
   begin
      if Selected_Sample (T) /= Sample_Library.Valid_Sample_Index'First then
         Track_Settings (T).Sample := Track_Settings (T).Sample - 1;
      end if;
   end Prev_Sample;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (S : User_Step_Settings) is
   begin
      case S is
         when Condition    => Trig_Next (Current_Editing_Step);
         when Note         => Note_Next (Current_Editing_Step);
         when Duration     => Duration_Next (Current_Editing_Step);
         when Velo         => Velo_Next (Current_Editing_Step);
         when Repeat       => Repeat_Next (Current_Editing_Step);
         when Repeat_Rate  => Repeat_Rate_Next (Current_Editing_Step);
         when CC_A .. CC_D =>
            CC_Value_Inc (Current_Editing_Step,
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
   begin
      case S is
         when Condition    => Trig_Prev (Current_Editing_Step);
         when Note         => Note_Prev (Current_Editing_Step);
         when Duration     => Duration_Prev (Current_Editing_Step);
         when Velo         => Velo_Prev (Current_Editing_Step);
         when Repeat       => Repeat_Prev (Current_Editing_Step);
         when Repeat_Rate  => Repeat_Rate_Prev (Current_Editing_Step);
         when CC_A .. CC_D =>
            CC_Value_Dec (Current_Editing_Step,
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
   begin
      null;
   end Next_Value_Fast;

   ---------------------
   -- Prev_Value_Fast --
   ---------------------

   procedure Prev_Value_Fast (S : User_Step_Settings) is
   begin
      null;
   end Prev_Value_Fast;

begin
   Sequences (1) (1) (1)  := (Always, 3, Rate_1_8, others => <>);
   --  Sequences (1) (2) (2)  := (Always, 0, Rate_1_8);
   --  Sequences (1) (3) (3)  := (Always, 0, Rate_1_8);
   --  Sequences (1) (4) (4)  := (Always, 0, Rate_1_8);
   Sequences (1) (1) (5)  := (Always, 3, Rate_1_8, others => <>);
   --  Sequences (1) (6) (6)  := (Always, 0, Rate_1_8);
   --  Sequences (1) (7) (7)  := (Always, 0, Rate_1_8);
   --  Sequences (1) (8) (8)  := (Always, 0, Rate_1_8);
   Sequences (1) (1) (9)  := (Always, 3, Rate_1_8, others => <>);
   --  Sequences (1) (10) (10) := (Always, 0, Rate_1_8);
   --  Sequences (1) (11) (11) := (Always, 0, Rate_1_8);
   --  Sequences (1) (12) (12) := (Always, 0, Rate_1_8);
   Sequences (1) (1) (13) := (Always, 3, Rate_1_8, others => <>);
   --  Sequences (1) (14) (14) := (Always, 0, Rate_1_8);
   --  Sequences (1) (15) (15) := (Always, 0, Rate_1_8);
   --  Sequences (1) (16) (16) := (Always, 0, Rate_1_8);
end WNM.Sequencer;
