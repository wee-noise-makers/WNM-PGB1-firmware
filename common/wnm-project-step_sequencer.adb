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

with WNM.MIDI_Clock;
with WNM.Short_Term_Sequencer;
with WNM.Note_Off_Sequencer;
with WNM.Chord_Settings;
with WNM.Project.Arpeggiator;
with WNM.UI; use WNM.UI;
with WNM.Coproc;
with WNM.Song_Start_Broadcast;
with WNM.Step_Event_Broadcast;
with WNM.Song_Stop_Broadcast;
with WNM.Song_Continue_Broadcast;
with HAL;                   use HAL;

package body WNM.Project.Step_Sequencer is

   Playing : Boolean := False;

   type Note_Play_State is record
      On_Notes : WNM.Chord_Settings.Chord_Notes := (others => 0);

      On_Lead : MIDI.MIDI_Key := 0;
   end record;

   G_Note_Play_State : Note_Play_State;

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

   procedure Play_Chord (T           : Tracks;
                         Chord       : WNM.Chord_Settings.Chord_Notes;
                         Last_Note   : WNM.Chord_Settings.Chord_Index_Range;
                         Oct         : Octave_Offset;
                         Velo        : MIDI.MIDI_Data;
                         Now         : Time.Time_Microseconds;
                         Shuffle     : Time.Time_Microseconds;
                         Duration    : Time.Time_Microseconds;
                         Repeat_Span : Time.Time_Microseconds);

   procedure Play_Now (Now      : Time.Time_Microseconds;
                       T        : Tracks;
                       Key      : MIDI.MIDI_Key;
                       Velo     : MIDI.MIDI_Data;
                       Duration : Time.Time_Microseconds);

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

   ------------
   -- Offset --
   ------------

   function Offset (Chord : WNM.Chord_Settings.Chord_Notes)
                    return WNM.Chord_Settings.Chord_Notes
   is
      T   : constant Tracks := 6;
      Oct : constant Octave_Offset := G_Project.Tracks (T).Offset;
   begin
      return (Offset (Chord (0), Oct),
              Offset (Chord (1), Oct),
              Offset (Chord (2), Oct),
              Offset (Chord (3), Oct));
   end Offset;

   --------------
   -- Chord_On --
   --------------

   procedure Chord_On (Chord : WNM.Chord_Settings.Chord_Notes;
                       Velocity : MIDI.MIDI_Data)
   is
      use MIDI;

      T : constant Tracks := 6;
      Chan : constant MIDI.MIDI_Channel := Voice_MIDI_Chan (Mode (T));

      Offset_Chord : constant WNM.Chord_Settings.Chord_Notes
        := Offset (Chord);
   begin
      for Key of G_Note_Play_State.On_Notes loop
         if Key /= 0 then
            WNM.Coproc.Push_To_Synth ((WNM.Coproc.MIDI_Event,
                                      (MIDI.Note_Off, Chan, Key, 0)));
         end if;
      end loop;

      for Key of Offset_Chord loop
         WNM.Coproc.Push_To_Synth ((WNM.Coproc.MIDI_Event,
                                   (MIDI.Note_On, Chan, Key, Velocity)));
      end loop;

      G_Note_Play_State.On_Notes := Offset_Chord;
      G_Current_Chord := Offset_Chord;
   end Chord_On;

   ---------------
   -- Chord_Off --
   ---------------

   procedure Chord_Off (Chord : WNM.Chord_Settings.Chord_Notes) is
      use WNM.Chord_Settings;
      T : constant Tracks := 6;
      Chan : constant MIDI.MIDI_Channel := Voice_MIDI_Chan (Mode (T));
      Offset_Chord : constant WNM.Chord_Settings.Chord_Notes
        := Offset (Chord);
   begin
      if Offset_Chord = G_Note_Play_State.On_Notes then
         for Key of Offset_Chord loop
            WNM.Coproc.Push_To_Synth ((WNM.Coproc.MIDI_Event,
                                      (MIDI.Note_Off, Chan, Key, 0)));
         end loop;
         G_Note_Play_State.On_Notes := (others => 0);
      end if;

   end Chord_Off;

   ----------------------
   -- Chord_For_Button --
   ----------------------

   function Chord_For_Button (B : Chord_Button)
                              return WNM.Chord_Settings.Chord_Notes
   is
      use WNM.Chord_Settings;

   begin
      return G_Project.Chords (B).Root +
        WNM.Chord_Settings.Chords (G_Project.Chords (B).Quality);
   end Chord_For_Button;

   --------------------
   -- Key_For_Button --
   --------------------

   function Key_For_Button (B : Lead_Button) return MIDI.MIDI_Key is
   begin
      return G_Project.Leads (B).Key;
   end Key_For_Button;

   -------------
   -- Lead_On --
   -------------

   procedure Lead_On (Key : MIDI.MIDI_Key;
                      Velocity : MIDI.MIDI_Data)
   is
      use MIDI;

      T : constant Tracks := 5;
      Chan : constant MIDI.MIDI_Channel := Voice_MIDI_Chan (Mode (T));
   begin
      if G_Note_Play_State.On_Lead /= 0 then
         WNM.Coproc.Push_To_Synth ((WNM.Coproc.MIDI_Event,
                                   (MIDI.Note_Off, Chan,
                                    G_Note_Play_State.On_Lead,
                                    Velocity)));

      end if;

      G_Note_Play_State.On_Lead := Key;

      WNM.Coproc.Push_To_Synth ((WNM.Coproc.MIDI_Event,
                                (MIDI.Note_On, Chan,
                                 G_Note_Play_State.On_Lead,
                                 Velocity)));

   end Lead_On;

   --------------
   -- Lead_Off --
   --------------

   procedure Lead_Off (Key : MIDI.MIDI_Key;
                      Velocity : MIDI.MIDI_Data)
   is
      use MIDI;

      T : constant Tracks := 5;
      Chan : constant MIDI.MIDI_Channel := Voice_MIDI_Chan (Mode (T));
   begin
      if G_Note_Play_State.On_Lead = Key then
         WNM.Coproc.Push_To_Synth ((WNM.Coproc.MIDI_Event,
                                   (MIDI.Note_Off, Chan,
                                    G_Note_Play_State.On_Lead,
                                    Velocity)));

         G_Note_Play_State.On_Lead := 0;
      end if;
   end Lead_Off;

   --------------
   -- On_Press --
   --------------

   procedure On_Press (Button : Keyboard_Button;
                       Mode   : WNM.UI.Main_Modes)
   is
   begin
      case Mode is
         when UI.Main_Modes =>
            case Button is
               when C1 .. C8 =>
                  Chord_On (Chord_For_Button (Button), 127);

               when L1 .. L8 =>
                  declare
                     Key : constant MIDI.MIDI_Key := Key_For_Button (Button);
                     Offset_Key : constant MIDI.MIDI_Key :=
                       Offset (Key, G_Project.Tracks (5).Offset);
                  begin
                     Lead_On (Offset_Key, 127);
                  end;
            end case;
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
         when UI.Main_Modes =>
            case Button is
               when C1 .. C8 =>
                  case G_Project.Chord_Play_Mode is
                     when Press_Release =>
                        Chord_Off (Chord_For_Button (Button));
                     when others =>
                        null;
                  end case;

               when L1 .. L8 =>
                  declare
                     Key : constant MIDI.MIDI_Key := Key_For_Button (Button);
                     Offset_Key : constant MIDI.MIDI_Key :=
                       Offset (Key, G_Project.Tracks (5).Offset);
                  begin
                     Lead_Off (Offset_Key, 127);
                  end;
            end case;
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

   ---------------
   -- Play_Note --
   ---------------

   procedure Play_Note (T           : Tracks;
                        Key         : MIDI.MIDI_Key;
                        Velo        : MIDI.MIDI_Data;
                        Now         : Time.Time_Microseconds;
                        Shuffle     : Time.Time_Microseconds;
                        Duration    : Time.Time_Microseconds;
                        Repeat_Span : Time.Time_Microseconds)
   is
      Start_Time : Time.Time_Microseconds := Now;
   begin

      if Shuffle = 0 then
         Play_Now (Now, T, Key, Velo, Duration);
      else
         Start_Time := Start_Time + Shuffle;
         Play_Later (T, Start_Time, Key, Velo, Duration);
      end if;
   end Play_Note;

   --------------
   -- Play_Arp --
   --------------

   procedure Play_Arp (T           : Tracks;
                       Velo        : MIDI.MIDI_Data;
                       Oct         : Octave_Offset;
                       Now         : Time.Time_Microseconds;
                       Shuffle     : Time.Time_Microseconds;
                       Duration    : Time.Time_Microseconds;
                       Repeat_Span : Time.Time_Microseconds)
   is
      Start_Time : Time.Time_Microseconds := Now;
      Key : constant MIDI.MIDI_Key := Offset (Arpeggiator.Next_Note (T), Oct);
   begin

      if Shuffle = 0 then
         Play_Now (Now, T, Key, Velo, Duration);
      else
         Start_Time := Start_Time + Shuffle;
         Play_Later (T, Start_Time, Key, Velo, Duration);
      end if;
   end Play_Arp;

   ----------------
   -- Play_Chord --
   ----------------

   procedure Play_Chord (T           : Tracks;
                         Chord       : WNM.Chord_Settings.Chord_Notes;
                         Last_Note   : WNM.Chord_Settings.Chord_Index_Range;
                         Oct         : Octave_Offset;
                         Velo        : MIDI.MIDI_Data;
                         Now         : Time.Time_Microseconds;
                         Shuffle     : Time.Time_Microseconds;
                         Duration    : Time.Time_Microseconds;
                         Repeat_Span : Time.Time_Microseconds)
   is
      Start_Time : Time.Time_Microseconds := Now;
      Offset_Chord : WNM.Chord_Settings.Chord_Notes;
   begin
      --  Put_Line ("Play_Chord: " &
      --              Key_Img (Chord (0)) &
      --              " " & Key_Img (Chord (1)) &
      --              " " & Key_Img (Chord (2)) &
      --              " " & Key_Img (Chord (3)));

      for X in Chord'First .. Last_Note loop
         Offset_Chord (X) := Offset (Chord (X), Oct);
      end loop;

      if Shuffle = 0 then
         for X in Chord'First .. Last_Note loop
            Play_Now (Now, T, Offset_Chord (X), Velo, Duration);
         end loop;
      else
         Start_Time := Start_Time + Shuffle;
         for X in Chord'First .. Last_Note loop
            Play_Later (T, Start_Time, Offset_Chord (X), Velo, Duration);
         end loop;
      end if;
   end Play_Chord;

   --------------------
   -- Move_Playheads --
   --------------------

   procedure Move_Playheads (State : in out Play_State; Roll : Roll_Kind) is
   begin
      --  The Step we're about to play
      case Roll is
         when Off =>
            if State.Next_Step >= G_Project.Pattern_Len then
               State.Next_Step := 1;
            else
               State.Next_Step := @ + 1;
            end if;

         when Beat =>
            case State.Next_Step is
               when 8 | 16 => State.Next_Step := 1;
               when others => State.Next_Step := @ + 1;
            end case;

         when Half =>
            case State.Next_Step is
               when 4 | 8 | 12 | 16 => State.Next_Step := 1;
               when others          => State.Next_Step := @ + 1;
            end case;

         when Quarter =>
            case State.Next_Step is
               when 2 | 4 | 6 | 8 | 10 | 12 | 14 | 16
                  => State.Next_Step := 1;
               when others   => State.Next_Step := @ + 1;
            end case;

         when Eighth =>
            State.Next_Step := 1;
      end case;
   end Move_Playheads;

   ------------------
   -- Process_Step --
   ------------------

   procedure Process_Step (DT   : Drum_Tracks;
                           Step : WNM.Pattern_Length;
                           Now  : Time_Microseconds)
   is
      Trig : constant Trigger_Kind := G_Project.Pattern (DT, Step);
   begin
      if Trig /= None then
         Play_Now (Now,
                   (case DT is
                       when Kick => Kick_Track,
                       when Snare => Snare_Track,
                       when Hihat_Closed => Cymbal_Track,
                       when Hihat_Open => Cymbal_Track,
                       when Sample => Sample1_Track),
                   (case DT is
                       when Kick => MIDI.C1,
                       when Snare => MIDI.C3,
                       when Hihat_Closed => MIDI.C4,
                       when Hihat_Open => MIDI.C4,
                       when Sample => MIDI.C4),
                   (case Trig is
                       when None   => 0,
                       when Ghost  => 30,
                       when Hit    => 100,
                       when Accent => 127),
                   Microseconds_Per_Beat / 4);
      end if;
   end Process_Step;

   ------------------
   -- Execute_Step --
   ------------------

   procedure Execute_Step is
      Build_Up_Step : constant := 4;
      Now : constant Time.Time_Microseconds := Clock;
   begin

      if G_Roll_Next_State /= Off and then G_Roll_State = Off then
         Save_Play_State;
      elsif G_Roll_Next_State = Off and then G_Roll_State /= Off then
         Restore_Play_State;
      end if;

      G_Roll_State := G_Roll_Next_State;

      if Playing then

         WNM.Step_Event_Broadcast.Broadcast;

         for DT in Drum_Tracks loop
            Process_Step (DT, G_Play_State.Next_Step, Now);
         end loop;

         Move_Playheads (G_Play_State, G_Roll_State);

         if G_Roll_State /= Off then
            --  Keep the saved state going without rolls
            Move_Playheads (G_Play_State_Save, Off);
         end if;

         if G_Auto_Fill_State = Auto_Buildup
           and then
            G_Fill_Buildup_Proba <= (Rand_Percent'Last - Build_Up_Step)
         then
            G_Fill_Buildup_Proba := @ + Build_Up_Step;
         end if;

      end if;

   end Execute_Step;

   ---------------------
   -- MIDI_Clock_Tick --
   ---------------------

   procedure MIDI_Clock_Tick (Step : MIDI.Time.Step_Count) is
      use MIDI.Time;

      Clock_Div : constant MIDI.Time.Step_Count := 6;
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

      Current_Playing_Step := Sequencer_Steps'First;
   end Song_Start_Callback;

   ------------------------
   -- Song_Stop_Callback --
   ------------------------

   procedure Song_Stop_Callback is
   begin
      Playing := False;
      G_Roll_State := Off;
      G_Auto_Fill_State := Off;
   end Song_Stop_Callback;

   ----------------------------
   -- Song_Continue_Callback --
   ----------------------------

   procedure Song_Continue_Callback is
   begin
      Playing := True;
   end Song_Continue_Callback;

end WNM.Project.Step_Sequencer;
