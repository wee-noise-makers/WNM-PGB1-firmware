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

with MIDI;

with WNM.Short_Term_Sequencer;
with WNM.Note_Off_Sequencer;
with WNM.Chord_Settings;
with WNM.Project.Arpeggiator;
with WNM.UI; use WNM.UI;
with WNM.Coproc;
with WNM.Project.Song_Part_Sequencer;
with WNM.Project.Chord_Sequencer;
with WNM.Step_Event_Broadcast;
with WNM.Part_Event_Broadcast;
with WNM.Song_Start_Broadcast;
with WNM.Song_Stop_Broadcast;
with WNM.Song_Continue_Broadcast;
with WNM.MIDI_Clock;
with HAL;                   use HAL;

package body WNM.Project.Step_Sequencer is

   Pattern_Counter : array (Tracks, Patterns) of UInt32;
   --  Count how many times a pattern has played

   procedure Process_Step (Track   : Tracks;
                           Pattern : Patterns;
                           Step    : Sequencer_Steps);

   procedure Do_Preview_Trigger (T : Tracks);
   procedure Play_Step (P : Patterns; T : Tracks; S : Sequencer_Steps;
                        Now : Time.Time_Microseconds := Time.Clock);

   Playing : Boolean := False;

   procedure Part_Callback;
   package Part_Listener
   is new Part_Event_Broadcast.Register (Part_Callback'Access);
   pragma Unreferenced (Part_Listener);

   procedure Song_Start_Callback;
   package Song_Start_Listener
   is new Song_Start_Broadcast.Register (Song_Start_Callback'Access);
   pragma Unreferenced (Song_Start_Listener);

   procedure Song_Stop_Callback;
   package Song_Stop_Listener
   is new Song_Stop_Broadcast.Register (Song_Stop_Callback'Access);
   pragma Unreferenced (Song_Stop_Listener);

   procedure Song_Continue_Callback;
   package Song_Continue_Listener
   is new Song_Continue_Broadcast.Register (Song_Continue_Callback'Access);
   pragma Unreferenced (Song_Continue_Listener);

   ------------
   -- Offset --
   ------------

   function Offset (K : MIDI.MIDI_Key; Oct : Octave_Offset)
                    return MIDI.MIDI_Key
   is
      use MIDI;

      Result : MIDI.MIDI_Key := K;
   begin
      if Oct >= 0 then
         for X in 1 .. Natural (Oct) loop
            if Result <= (MIDI.MIDI_Key'Last - 12) then
               Result := Result + 12;
            else
               return Result;
            end if;
         end loop;
      else
         for X in 1 .. Natural (-Oct) loop
            if Result >= 12 then
               Result := Result - 12;
            else
               return Result;
            end if;
         end loop;
      end if;
      return Result;
   end Offset;

   -------------
   -- Playing --
   -------------

   function Playing_Step (T : Tracks) return Playhead
   is (G_Play_State.Playheads (T));

   ----------------
   -- Play_Pause --
   ----------------

   procedure Play_Pause is
   begin
      if not Playing then
         MIDI_Clock.Internal_Start;
      else
         MIDI_Clock.Internal_Stop;
      end if;
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
            Editing_Pattern := Patterns (V);

         when UI.Song_Mode =>
            pragma Warnings (Off, "lower bound test optimized");
            if V in
              Keyboard_Value (WNM.Parts'First) ..
              Keyboard_Value (WNM.Parts'Last)
            then
               G_Project.Part_Origin := Parts (V);
            end if;
            Editing_Song_Elt := Song_Element (V);

         when UI.Track_Mode | UI.Step_Mode =>

            if Mode = UI.Track_Mode and then not UI.Recording then
               Editing_Track := Tracks (V);
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
                  S : Step_Rec renames
                    G_Project.Steps
                      (Editing_Track)(Editing_Pattern)(To_Value (Button));
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
                  Do_Preview_Trigger (Tracks (V));

               end if;
            end if;

         when UI.FX_Mode =>
            null;
      end case;
   end On_Press;

   ----------------
   -- On_Release --
   ----------------

   procedure On_Release (Button : Keyboard_Button;
                         Mode   : WNM.UI.Main_Modes)
   is
      pragma Unreferenced (Button);
   begin
      case Mode is
         when UI.Song_Mode =>
            null;

         when UI.Track_Mode | UI.Step_Mode | UI.FX_Mode | UI.Pattern_Mode =>
            null;
      end case;
   end On_Release;

   --------------
   -- Play_Now --
   --------------

   procedure Play_Now (Now      : Time.Time_Microseconds;
                       T        : Tracks;
                       Key      : MIDI.MIDI_Key;
                       Velo     : MIDI.MIDI_Data;
                       Duration : Time.Time_Microseconds)
   is
   begin
      case Mode (T) is
         when Synth_Track_Mode_Kind =>
            declare
               Chan : constant MIDI.MIDI_Channel :=
                 Voice_MIDI_Chan (Mode (T));
            begin

               WNM.Coproc.Push_To_Synth ((WNM.Coproc.MIDI_Event,
                                         (MIDI.Note_On, Chan, Key, Velo)));

               WNM.Note_Off_Sequencer.Note_Off
                 (Internal, Chan, Key, Now + Duration);
            end;

         when MIDI_Mode =>
            WNM_HAL.Send_External
              ((MIDI.Note_On, MIDI_Chan (T), Key, Velo));

            WNM.Note_Off_Sequencer.Note_Off
              (External, MIDI_Chan (T), Key, Now + Duration);
      end case;
   end Play_Now;

   ----------------
   -- Play_Later --
   ----------------

   procedure Play_Later (T           : Tracks;
                         Deadline    : Time.Time_Microseconds;
                         Key         : MIDI.MIDI_Key;
                         Velo        : MIDI.MIDI_Data;
                         Duration    : Time.Time_Microseconds)
   is
      Target : MIDI_Target;
      Chan : MIDI.MIDI_Channel;
   begin
      case Mode (T) is
         when Synth_Track_Mode_Kind =>
            Chan := Voice_MIDI_Chan (Mode (T));
            Target := Internal;

         when MIDI_Mode =>
            Chan := MIDI_Chan (T);
            Target := External;
      end case;

      WNM.Short_Term_Sequencer.Play_At
        (Start  => Deadline,
         Target => Target,
         Chan => Chan,
         Key => Key,
         Velocity => Velo,
         Duration => Duration);

   end Play_Later;

   -----------------------
   -- Process_CC_Values --
   -----------------------

   procedure Process_CC_Values (P : Patterns;
                                T : Tracks;
                                S : Sequencer_Steps)
   is
      Val  : MIDI.MIDI_Data;
      Ctrl : MIDI.MIDI_Data;
      M    : constant Track_Mode_Kind := Mode (T);
      Chan : constant MIDI.MIDI_Channel :=
        (case M is
            when Synth_Track_Mode_Kind =>
              Voice_MIDI_Chan (M),
            when MIDI_Mode =>
              G_Project.Tracks (T).Chan);
   begin
      for Id in CC_Id loop
         Val := CC_Value_To_Use (P, T, S, Id);
         case Mode (T) is

            when Synth_Track_Mode_Kind =>
               Ctrl := (case Id is
                           when A => Synth.Voice_Param_1_CC,
                           when B => Synth.Voice_Param_2_CC,
                           when C => Synth.Voice_Param_3_CC,
                           when D => Synth.Voice_Param_4_CC);

               WNM.Coproc.Push_To_Synth ((Kind => WNM.Coproc.MIDI_Event,
                                          MIDI_Evt =>
                                            (MIDI.Continous_Controller,
                                             Chan, Ctrl, Val)));

            when MIDI_Mode =>
               Ctrl := G_Project.Tracks (T).CC (Id).Controller;
               WNM_HAL.Send_External
                 ((MIDI.Continous_Controller,
                  Chan, Ctrl, Val));

         end case;
      end loop;
   end Process_CC_Values;

   ------------------------
   -- Do_Preview_Trigger --
   ------------------------

   procedure Do_Preview_Trigger (T : Tracks) is
   begin
      Process_CC_Values (Editing_Pattern, T, 1);
      Play_Now (Time.Clock,
                T,
                MIDI.C4,
                MIDI.MIDI_Data'Last,
                Microseconds_Per_Beat);
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

      Play_Now (Now, T, Key, Velo, Duration);

      for X in 1 .. Rep loop
         Repeat_Time := Repeat_Time + Repeat_Span;
         Play_Later (T, Repeat_Time, Key, Velo, Duration);
      end loop;

   end Play_Note;

   --------------
   -- Play_Arp --
   --------------

   procedure Play_Arp (T           : Tracks;
                       Velo        : MIDI.MIDI_Data;
                       Oct         : Octave_Offset;
                       Rep         : Repeat_Cnt;
                       Now         : Time.Time_Microseconds;
                       Duration    : Time.Time_Microseconds;
                       Repeat_Span : Time.Time_Microseconds)
   is
      Repeat_Time : Time.Time_Microseconds := Now;
      Key : MIDI.MIDI_Key := Offset (Arpeggiator.Next_Note (T), Oct);
   begin

      Play_Now (Now, T, Key, Velo, Duration);

      for X in 1 .. Rep loop
         Repeat_Time := Repeat_Time + Repeat_Span;
         Key := Arpeggiator.Next_Note (T);

         Play_Later (T, Repeat_Time, Key, Velo, Duration);
      end loop;

   end Play_Arp;

   ----------------
   -- Play_Chord --
   ----------------

   procedure Play_Chord (T           : Tracks;
                         Chord       : WNM.Chord_Settings.Chord_Notes;
                         Last_Note   : WNM.Chord_Settings.Chord_Index_Range;
                         Oct         : Octave_Offset;
                         Velo        : MIDI.MIDI_Data;
                         Rep         : Repeat_Cnt;
                         Now         : Time.Time_Microseconds;
                         Duration    : Time.Time_Microseconds;
                         Repeat_Span : Time.Time_Microseconds)
   is
      Repeat_Time : Time.Time_Microseconds := Now;
      Offset_Chord : WNM.Chord_Settings.Chord_Notes;
   begin
      for X in Chord'First .. Last_Note loop
         Offset_Chord (X) := Offset (Chord (X), Oct);
      end loop;

      for X in Chord'First .. Last_Note loop
         Play_Now (Now, T, Offset_Chord (X), Velo, Duration);
      end loop;

      for X in 1 .. Rep loop
         Repeat_Time := Repeat_Time + Repeat_Span;

         for X in Chord'First .. Last_Note loop
            Play_Later (T, Repeat_Time, Offset_Chord (X), Velo, Duration);
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

      Step : Step_Rec renames G_Project.Steps (T)(P)(S);

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

            Play_Note (T,
                       Offset
                         (Current_Chord (Chord_Index_Range (Step.Note)),
                          Step.Oct),
                       Step.Velo, Step.Repeat,
                       Now, Repeat_Duration, Repeat_Span);

         when Arp =>

            Play_Arp (T,
                      Step.Velo, Step.Oct, Step.Repeat,
                      Now, Repeat_Duration, Repeat_Span);

         when Chord =>

            Play_Chord
              (T, Current_Chord,
               G_Project.Tracks (T).Notes_Per_Chord,
               Step.Oct,
               Step.Velo, Step.Repeat,
               Now, Repeat_Duration, Repeat_Span);
      end case;

   end Play_Step;

   ------------------
   -- Process_Step --
   ------------------

   procedure Process_Step (Track   : Tracks;
                           Pattern : Patterns;
                           Step    : Sequencer_Steps)
   is
      Condition : Boolean := False;

      Now : constant Time.Time_Microseconds := Time.Clock;
   begin

      if Step = Sequencer_Steps'First then
         Pattern_Counter (Track, Pattern) := @ + 1;
      end if;

      declare
         S : Step_Rec renames
           G_Project.Steps (Track)(Pattern)(Step);
      begin
         --  Send CC first
         Process_CC_Values (Pattern, Track, Step);

         if not UI.Muted (Track)
           and then
            not Song_Part_Sequencer.Muted (Track)
         then
            case S.Trig is
               when None =>
                  Condition := False;
               when Always =>
                  Condition := True;
               when Fill =>
                  Condition := WNM.UI.FX_On (B1);
               when Not_Fill =>
                  Condition := not WNM.UI.FX_On (B1);
               when Percent_25 =>
                  Condition := Random <= 25;
               when Percent_50 =>
                  Condition := Random <= 50;
               when Percent_75 =>
                  Condition := Random <= 75;
               when One_Of_Two =>
                  Condition := Pattern_Counter (Track, Pattern) mod 2 = 0;
               when One_Of_Three =>
                  Condition := Pattern_Counter (Track, Pattern) mod 3 = 0;
               when One_Of_Four =>
                  Condition := Pattern_Counter (Track, Pattern) mod 4 = 0;
               when One_Of_Five =>
                  Condition := Pattern_Counter (Track, Pattern) mod 5 = 0;
            end case;

            --  Play step?
            if Condition then
               Play_Step (Pattern, Track, Step, Now);
            end if;
         end if;
      end;
   end Process_Step;

   --------------------
   -- Move_Playheads --
   --------------------

   procedure Move_Playheads is
      Current_Part : constant Parts := Song_Part_Sequencer.Playing;
   begin
      --  Put_Line ("Move Playheads!");
      for Track_Id in Tracks loop
         declare
            PH    : Playhead renames G_Play_State.Playheads (Track_Id);
            Pat   : Pattern_Rec renames G_Project.Patterns (Track_Id)(PH.P);
         begin
            --  The Step we're about to play
            PH.Steps_Count := @ + 1;

            if PH.Steps_Count > Natural (Pat.Length) then
               --  We are at the end of the pattern, what do we do next?
               --  First, find the next pattern to play.

               --  Check if there's a pattern link
               if PH.P /= Patterns'Last
                 and then
                  Pat.Has_Link
               then
                  --  Use pattern link
                  PH.P := @ + 1;
               else
                  --  No link, look at song part to pick which pattern to play
                  --  for this track.
                  PH.P :=
                    G_Project.Parts (Current_Part).Pattern_Select (Track_Id);
               end if;

               --  We're about to play the first step of this new pattern
               PH.Steps_Count := 1;
               Arpeggiator.Signal_Start_Of_Pattern (Track_Id);
            end if;
         end;
      end loop;
   end Move_Playheads;

   ------------------
   -- Execute_Step --
   ------------------

   procedure Execute_Step is
   begin

      if Playing then

         if G_Roll_State /= Off then
            if (G_Roll_State in Quarter | Eighth)
              or else
                (G_Roll_State = Half and then G_Roll_Step_Count >= 2)
              or else
                (G_Roll_State = Beat and then G_Roll_Step_Count >= 4)
            then
               Restore_Play_State;
               G_Roll_Step_Count := 1;
            else
               G_Roll_Step_Count := @ + 1;
               WNM.Step_Event_Broadcast.Broadcast;
               Move_Playheads;
            end if;
         else
            WNM.Step_Event_Broadcast.Broadcast;
            Move_Playheads;
         end if;

         --  if (for some B in B2 .. B5 => WNM.UI.FX_On (B)) then
         --     if (WNM.UI.FX_On (B2) or else WNM.UI.FX_On (B3))
         --       or else
         --         (G_Roll_Step_Count >= 2 and then WNM.UI.FX_On (B4))
         --         or else
         --           (G_Roll_Step_Count >= 4 and then WNM.UI.FX_On (B5))
         --     then
         --        Restore_Play_State;
         --        G_Roll_Step_Count := 1;
         --     else
         --        G_Roll_Step_Count := @ + 1;
         --        WNM.Step_Event_Broadcast.Broadcast;
         --        Move_Playheads;
         --     end if;
         --  else
         --     WNM.Step_Event_Broadcast.Broadcast;
         --     Move_Playheads;
         --  end if;

         for Track in Tracks loop
            Process_Step (Track,
                          G_Play_State.Playheads (Track).P,
                          Sequencer_Steps
                            (G_Play_State.Playheads (Track).Steps_Count));
         end loop;
      end if;

   end Execute_Step;

   -------------------
   -- Part_Callback --
   -------------------

   procedure Part_Callback is
      Current_Part : constant Parts := Song_Part_Sequencer.Playing;
   begin
      --  There's a new part

      for Track_Id in Tracks loop
         declare
            Pattern : constant Patterns :=
              G_Project.Parts (Current_Part).Pattern_Select (Track_Id);
            PH    : Playhead renames G_Play_State.Playheads (Track_Id);
         begin
            PH.Steps_Count := 0;
            PH.P := Pattern;
            Arpeggiator.Signal_Start_Of_Pattern (Track_Id);
         end;
      end loop;
   end Part_Callback;

   ---------------------
   -- MIDI_Clock_Tick --
   ---------------------

   procedure MIDI_Clock_Tick (Step : MIDI.Time.Step_Count) is
      use MIDI.Time;

      Clock_Div : constant MIDI.Time.Step_Count :=
        (if G_Roll_State = Eighth then 3 else 6);

      S : constant MIDI.Time.Step_Count := Step mod Clock_Div;
   begin
      if S = 0 then
         Execute_Step;
      end if;
   end MIDI_Clock_Tick;

   -------------------------
   -- Song_Start_Callback --
   -------------------------

   procedure Song_Start_Callback is
   begin
      Playing := True;

      --  Clear counters
      Pattern_Counter := (others => (others => 0));

      Current_Playing_Step := Sequencer_Steps'First;
   end Song_Start_Callback;

   ------------------------
   -- Song_Stop_Callback --
   ------------------------

   procedure Song_Stop_Callback is
   begin
      Playing := False;
      G_Roll_State := Off;
   end Song_Stop_Callback;

   ----------------------------
   -- Song_Continue_Callback --
   ----------------------------

   procedure Song_Continue_Callback is
   begin
      Playing := True;
   end Song_Continue_Callback;

end WNM.Project.Step_Sequencer;
