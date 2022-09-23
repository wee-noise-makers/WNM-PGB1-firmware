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
with WNM.Chord_Settings;
with WNM.Project.Arpeggiator;
with WNM.UI; use WNM.UI;
with WNM.MIDI.Queues;
with WNM.Coproc;
with WNM.Sample_Stream;
with WNM.Project.Chord_Sequencer;

with HAL;                   use HAL;

package body WNM.Project.Step_Sequencer is

   --  Next_Start : Synth.Sample_Time := Synth.Sample_Time'First;
   Next_Start : Time.Time_Microseconds := Time.Time_Microseconds'First;

   Pattern_Counter : array (Patterns) of UInt32;
   --  Count how many times a pattern has played

   type Note_On_Off is (On, Off);

   type Microstep_Cnt is mod 2;
   Microstep : Microstep_Cnt := 1;

   procedure Process_Step (Pattern : Patterns; Step : Sequencer_Steps);

   procedure Do_Preview_Trigger (T : Tracks);
   procedure Play_Step (P : Patterns; T : Tracks; S : Sequencer_Steps;
                        Now : Time.Time_Microseconds := Time.Clock);

   ------------------
   -- Playing_Step --
   ------------------

   function Playing_Step return Sequencer_Steps
   is (Current_Playing_Step);

   ---------------------
   -- Playing_Pattern --
   ---------------------

   function Playing_Pattern return Patterns
   is (Pattern_Sequencer.Playing);

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
      WNM.Project.Chord_Sequencer.Play_Pause;

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
            Editing_Pattern := V;
            Pattern_Sequencer.On_Press (Button, Mode);

         when UI.Chord_Mode =>
            Editing_Chord := V;
            Project.Chord_Sequencer.Chain.On_Press (Button, Mode);

         when UI.Track_Mode | UI.Step_Mode =>

            if Mode = UI.Track_Mode and then not UI.Recording then
               Editing_Track := V;
            else
               Editing_Step := V;
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
                  S : Step_Rec renames G_Project.Seqs
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
      case Mode is
         when UI.Pattern_Mode =>
            Pattern_Sequencer.On_Release (Button, Mode);

         when UI.Chord_Mode =>
            Project.Chord_Sequencer.Chain.On_Release (Button, Mode);

         when UI.Track_Mode | UI.Step_Mode =>
            null;
      end case;
   end On_Release;

   --------------
   -- Send_Now --
   --------------

   procedure Send_Now (T    : Tracks;
                       Key  : MIDI.MIDI_Key;
                       Velo : MIDI.MIDI_Data;
                       Kind : Note_On_Off)
   is
      use WNM.Sample_Stream;
   begin
      case Mode (T) is
         when Sample_Mode =>
            WNM.Coproc.Push ((WNM.Coproc.Sampler_Event,
                              (On       => Kind = On,
                               Track    => To_Stream_Track (T),
                               Sample   => Selected_Sample (T),
                               Key      => Key,
                               Velocity => Velo)
                              )
                            );

         when Speech_Mode =>
            WNM.Coproc.Push ((WNM.Coproc.Speech_Event,
                             (On => Kind = On,
                              Track => T,
                              W     => Selected_Word (T),
                              Key   => Key)
                             )
                            );

         when MIDI_Mode =>
            case Kind is
               when On =>
                  WNM.MIDI.Queues.Sequencer_Push
                    ((MIDI.Note_On, MIDI_Chan (T), Key, Velo));
               when Off =>
                  WNM.MIDI.Queues.Sequencer_Push
                    ((MIDI.Note_Off, MIDI_Chan (T), Key, Velo));
            end case;
      end case;
   end Send_Now;

   ----------------
   -- Send_Later --
   ----------------

   procedure Send_Later (T           : Tracks;
                         Deadline    : Time.Time_Microseconds;
                         Key         : MIDI.MIDI_Key;
                         Velo        : MIDI.MIDI_Data;
                         Kind        : Note_On_Off)
   is
      use WNM.Sample_Stream;
   begin
      case Mode (T) is
         when Sample_Mode =>
            WNM.Short_Term_Sequencer.Push
              ((Short_Term_Sequencer.Sampler_Event,
               (On       => Kind = On,
                Track    => To_Stream_Track (T),
                Sample   => Selected_Sample (T),
                Key      => Key,
                Velocity => Velo)
               ),
               Deadline);

         when Speech_Mode =>
            WNM.Short_Term_Sequencer.Push
              ((Short_Term_Sequencer.Speech_Event,
                          (On => Kind = On,
                           Track => T,
                           W     => Selected_Word (T),
                           Key   => Key)
                          ),
               Deadline);

         when MIDI_Mode =>
            case Kind is
               when On =>
                  WNM.Short_Term_Sequencer.Push
                    ((Short_Term_Sequencer.MIDI_Event,
                      (MIDI.Note_On,
                       MIDI_Chan (T),
                       Key,
                       Velo)
                      ),
                     Deadline);
               when Off =>
                  WNM.Short_Term_Sequencer.Push
                    ((Short_Term_Sequencer.MIDI_Event,
                      (MIDI.Note_Off,
                       MIDI_Chan (T),
                       Key,
                       Velo)
                      ),
                     Deadline);
            end case;
      end case;
   end Send_Later;

   ------------------------
   -- Do_Preview_Trigger --
   ------------------------

   procedure Do_Preview_Trigger (T : Tracks) is
   begin
      Send_Now (T,
                MIDI.C4,
                MIDI.MIDI_Data'Last,
                On);

      Send_Later (T,
                  Time.Clock + Microseconds_Per_Beat,
                  MIDI.C4,
                  MIDI.MIDI_Data'Last,
                  Off);
   end Do_Preview_Trigger;

   ---------------
   -- Play_Note --
   ---------------

   procedure Play_Note (T           : Tracks;
                        Key         : MIDI.MIDI_Key;
                        Velo        : MIDI.MIDI_Data;
                        Rep         : Repeat_Cnt;
                        Now         : Time.Time_Microseconds;
                        Duration    : Time.Time_Microseconds;
                        Repeat_Span : Time.Time_Microseconds)
   is
      Repeat_Time : Time.Time_Microseconds := Now;
   begin

      Send_Now (T, Key, Velo, On);
      Send_Later (T, Now + Duration, Key, 0, Off);

      for X in 1 .. Rep loop
         Repeat_Time := Repeat_Time + Repeat_Span;

         Send_Later (T, Repeat_Time, Key, Velo, On);
         Send_Later (T, Repeat_Time + Duration, Key, 0, Off);
      end loop;

   end Play_Note;

   --------------
   -- Play_Arp --
   --------------

   procedure Play_Arp (T           : Tracks;
                       Velo        : MIDI.MIDI_Data;
                       Rep         : Repeat_Cnt;
                       Now         : Time.Time_Microseconds;
                       Duration    : Time.Time_Microseconds;
                       Repeat_Span : Time.Time_Microseconds)
   is
      Repeat_Time : Time.Time_Microseconds := Now;
      Key : MIDI.MIDI_Key := Arpeggiator.Next_Note (T);
   begin

      Send_Now (T, Key, Velo, On);
      Send_Later (T, Now + Duration, Key, 0, Off);

      for X in 1 .. Rep loop
         Repeat_Time := Repeat_Time + Repeat_Span;
         Key := Arpeggiator.Next_Note (T);

         Send_Later (T, Repeat_Time, Key, Velo, On);
         Send_Later (T, Repeat_Time + Duration, Key, 0, Off);
      end loop;

   end Play_Arp;

   ----------------
   -- Play_Chord --
   ----------------

   procedure Play_Chord (T           : Tracks;
                         Chord       : WNM.Chord_Settings.Chord_Notes;
                         Last_Note   : WNM.Chord_Settings.Chord_Index_Range;
                         Velo        : MIDI.MIDI_Data;
                         Rep         : Repeat_Cnt;
                         Now         : Time.Time_Microseconds;
                         Duration    : Time.Time_Microseconds;
                         Repeat_Span : Time.Time_Microseconds)
   is
      Repeat_Time : Time.Time_Microseconds := Now;
   begin
      for X in Chord'First .. Last_Note loop

         Send_Now (T, Chord (X), Velo, On);
         Send_Later (T, Now + Duration, Chord (X), 0, Off);

      end loop;

      for X in 1 .. Rep loop
         Repeat_Time := Repeat_Time + Repeat_Span;

         for X in Chord'First .. Last_Note loop
            Send_Later (T, Repeat_Time, Chord (X), Velo, On);
            Send_Later (T, Repeat_Span + Duration, Chord (X), 0, Off);
         end loop;
      end loop;
   end Play_Chord;

   ---------------
   -- Play_Step --
   ---------------

   procedure Play_Step (P : Patterns; T : Tracks; S : Sequencer_Steps;
                        Now : Time.Time_Microseconds := Time.Clock)
   is
      use WNM.Chord_Settings;
      use WNM.Project.Chord_Sequencer;
      use MIDI;

      Step : Step_Rec renames G_Project.Seqs (P) (T) (S);

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
            Play_Note (T, Step.Note,
                       Step.Velo, Step.Repeat,
                       Now, Repeat_Duration, Repeat_Span);

         when Note_In_Chord =>

            Play_Note (T, Current_Chord (Chord_Index_Range (Step.Note)),
                       Step.Velo, Step.Repeat,
                       Now, Repeat_Duration, Repeat_Span);

         when Arp =>

            Play_Arp (T,
                      Step.Velo, Step.Repeat,
                      Now, Repeat_Duration, Repeat_Span);

         when Chord =>

            Play_Chord
              (T, Current_Chord,
               Chord_Index_Range
                 (Integer'Min (Integer (Chord_Index_Range'Last),
                 Integer (Step.Note))),

               Step.Velo, Step.Repeat,
               Now, Repeat_Duration, Repeat_Span);
      end case;

   end Play_Step;

   -----------------------
   -- Process_CC_Values --
   -----------------------

   procedure Process_CC_Values (T : Tracks; S : Step_Rec) is
      Channel : constant MIDI.MIDI_Channel :=
        G_Project.Tracks (T).Chan;
   begin
      case Mode (T) is

      when MIDI_Mode =>
         for Id in CC_Id loop
            if S.CC_Ena (Id) then
               WNM.MIDI.Queues.Sequencer_Push
                 ((MIDI.Continous_Controller,
                  Channel,
                  G_Project.Tracks (T).CC (Id).Controller,
                  S.CC_Val (Id)));
            end if;
         end loop;

         when Speech_Mode =>
            if S.CC_Ena (A) then
               WNM.Coproc.Push ((Kind => WNM.Coproc.Speech_CC_Event,
                                 Speech_CC_Evt => (T, S.CC_Val (A))));
            end if;


         when Sample_Mode =>
            null;
      end case;
   end Process_CC_Values;

   ------------------
   -- Process_Step --
   ------------------

   procedure Process_Step (Pattern : Patterns;
                           Step : Sequencer_Steps)
   is
      Condition : Boolean := False;

      Now : constant Time.Time_Microseconds := Time.Clock;

      --  Now : constant Sample_Time := Sample_Clock;
      --  Note_Duration : constant Sample_Time := Samples_Per_Beat / 4;
   begin
      if Step = Sequencer_Steps'First then
         Pattern_Counter (Pattern) := Pattern_Counter (Pattern) + 1;
      end if;

      for Track in Tracks loop
         declare
            S : Step_Rec renames G_Project.Seqs (Pattern) (Track) (Step);
         begin
            --  Send CC first
            Process_CC_Values (Track, S);

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

            --  Play step?
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
   begin

      Next_Start := Next_Start + (Microseconds_Per_Beat / Steps_Per_Beat) / 2;

      if Pattern_Sequencer.Playing then
         case Microstep is

            --  Begining of a new step
            when 0 =>

               if Current_Playing_Step /= Sequencer_Steps'Last then

                  if Current_Playing_Step = 8 then
                     Pattern_Sequencer.Signal_Mid_Pattern;
                     Chord_Sequencer.Signal_Mid_Pattern;
                     WNM.Project.Chord_Sequencer.Signal_Mid_Pattern;
                  end if;

                  Current_Playing_Step := Current_Playing_Step + 1;
               else
                  Current_Playing_Step := Sequencer_Steps'First;
                  Pattern_Sequencer.Signal_End_Of_Pattern;
                  Chord_Sequencer.Signal_End_Of_Pattern;
                  WNM.Project.Chord_Sequencer.Signal_End_Of_Pattern;
               end if;

               --  At the middle of the step we play the recorded notes
            when 1 =>
               Process_Step (Pattern_Sequencer.Playing, Playing_Step);
         end case;

         Microstep := Microstep + 1;
      end if;
   end Execute_Step;

   ------------
   -- Update --
   ------------

   function Update return Time.Time_Microseconds is
      Now      : constant Time.Time_Microseconds := Time.Clock;
      Success : Boolean;
      Data    : Short_Term_Sequencer.Event_Data;
   begin
      if Now >= Next_Start then
         Execute_Step;
      end if;

      loop
         WNM.Short_Term_Sequencer.Pop (Now, Data, Success);
         exit when not Success;

         case Data.Kind is
            when Short_Term_Sequencer.Sampler_Event =>
               WNM.Coproc.Push ((WNM.Coproc.Sampler_Event, Data.Sampler_Evt));
            when Short_Term_Sequencer.Speech_Event =>
               WNM.Coproc.Push ((WNM.Coproc.Speech_Event, Data.Speech_Evt));
            when Short_Term_Sequencer.MIDI_Event =>
               WNM.MIDI.Queues.Sequencer_Push (Data.Msg);
         end case;
      end loop;

      return Time.Time_Microseconds'First;
   end Update;

end WNM.Project.Step_Sequencer;
