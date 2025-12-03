-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2023 Fabien Chouteau                    --
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

with Tresses.DSP;
with Tresses.Envelopes.AR; use Tresses.Envelopes.AR;
with Tresses.Resources; use Tresses.Resources;
with WNM.Synth;

package body WNM.Voices.Chord_Voice is

   type Wave_Range is range 0 .. 12;
   Waveforms : constant array (Wave_Range) of
     not null access constant Table_257_S16
       := (WAV_Sine'Access,
           WAV_Sine_Warp1'Access,
           WAV_Sine_Warp2'Access,
           WAV_Sine_Warp3'Access,
           WAV_Triangle'Access,
           WAV_Sawtooth'Access,
           WAV_Chip_Pulse_25'Access,
           WAV_Chip_Pulse_50'Access,
           WAV_Chip_Triangle'Access,
           WAV_Screech'Access,
           WAV_Combined_Sin_Saw'Access,
           WAV_Combined_Trig_Sin'Access,
           WAV_Combined_Square_Sin'Access);

   ------------
   -- Engine --
   ------------

   function Engine (This : Instance) return Chord_Engine
   is (This.Engine);

   ----------------
   -- Set_Engine --
   ----------------

   procedure Set_Engine (This : in out Instance; E : Chord_Engine) is
   begin
      pragma Warnings (Off, "can only be True");
      if This.Engine /= E then
         This.Engine := E;
         This.Do_Init := True;
      end if;
      pragma Warnings (On, "can only be True");
   end Set_Engine;

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Instance) is
   begin
      for V of This.Voices loop
         V.On := False;

         V.Phase := 0;
      end loop;

      Envelopes.AR.Init (This.Env, Do_Hold => True);

      Init (This.Glide_Env, Do_Hold => False,
            Attack_Speed  => Envelopes.AR.S_Quarter_Second,
            Release_Curve => Envelopes.AR.Exponential,
            Release_Speed => Envelopes.AR.S_1_Seconds);
      Set_Attack (This.Glide_Env, 0);

   end Init;

   ----------------
   -- Voice_Wave --
   ----------------

   function Voice_Wave (This : Instance;
                        Wave_Select : Wave_Range;
                        V : Voice_Id)
                        return not null access constant Table_257_S16
   is
      Mix_Sel : constant Wave_Range :=
        Wave_Range ((U32 (Wave_Select) + U32 (V)) mod
                      (U32 (Wave_Range'Last) + 1));
   begin
      return (case This.Engine is
                 when Waveform        => Waveforms (Wave_Select),
                 when Mixed_Waveforms => Waveforms (Mix_Sel),
                 when Custom_Waveform => WNM.Synth.User_Waveform'Access);
   end Voice_Wave;

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Instance;
                     Buffer :    out Tresses.Mono_Buffer)
   is

      Mod_Param : constant Param_Range :=
        This.Params (P_Waveform) / (Param_Range'Last / Waveforms'Length);

      Wave_Select : constant Wave_Range :=
        Wave_Range
          (Param_Range'Min (Mod_Param, Param_Range (Wave_Range'Last)));

      Waveform_1 : constant not null access constant Table_257_S16 :=
        Voice_Wave (This, Wave_Select, 1);
      Waveform_2 : constant not null access constant Table_257_S16 :=
        Voice_Wave (This, Wave_Select, 2);
      Waveform_3 : constant not null access constant Table_257_S16 :=
        Voice_Wave (This, Wave_Select, 3);
      Waveform_4 : constant not null access constant Table_257_S16 :=
        Voice_Wave (This, Wave_Select, 4);

      Sample : S32;
      Diff, Moded : U32;
      Amount : Param_Range;
   begin

      if This.Do_Init then
         This.Do_Init := False;
         This.Init;
      end if;

      Set_Attack (This.Env, This.Params (P_Attack));
      Set_Release (This.Env, This.Params (P_Release));

      Set_Release (This.Glide_Env, This.Params (P_Glide));
      Render (This.Glide_Env);

      Amount :=
        Param_Range'Last - Param_Range (Render (This.Glide_Env));

      for V of This.Voices loop
         if V.Target_Phase_Incr > V.Start_Phase_Incr then
            Diff := V.Target_Phase_Incr - V.Start_Phase_Incr;
            Moded := DSP.Modulate (Diff, Amount);
            V.Current_Phase_Incr := V.Start_Phase_Incr + Moded;
         else
            Diff := V.Start_Phase_Incr - V.Target_Phase_Incr;
            Moded := DSP.Modulate (Diff, Amount);
            V.Current_Phase_Incr := V.Start_Phase_Incr - Moded;
         end if;

      end loop;

      if Envelopes.AR.Current_Segment (This.Env) = Envelopes.AR.Dead then
         Buffer := (others => 0);
      else

         declare
            V1 : Voice renames This.Voices (Voice_Id'First + 0);
            V2 : Voice renames This.Voices (Voice_Id'First + 1);
            V3 : Voice renames This.Voices (Voice_Id'First + 2);
            V4 : Voice renames This.Voices (Voice_Id'First + 3);

            S1, S2, S3, S4 : S16;
         begin
            for Elt of Buffer loop

               --  We render the glide envelope for every sample to not have
               --  it depend on the size of the buffer.
               Render (This.Glide_Env);

               V1.Phase := V1.Phase + V1.Current_Phase_Incr;
               V2.Phase := V2.Phase + V2.Current_Phase_Incr;
               V3.Phase := V3.Phase + V3.Current_Phase_Incr;
               V4.Phase := V4.Phase + V4.Current_Phase_Incr;

               S1 := DSP.Interpolate824 (Waveform_1.all, V1.Phase);
               S2 := DSP.Interpolate824 (Waveform_2.all, V2.Phase);
               S3 := DSP.Interpolate824 (Waveform_3.all, V3.Phase);
               S4 := DSP.Interpolate824 (Waveform_4.all, V4.Phase);

               Sample := (S32 (S1) + S32 (S2) + S32 (S3) + S32 (S4)) / 4;
               Envelopes.AR.Render (This.Env);
               Sample := (Sample * Low_Pass (This.Env)) / 2**15;

               Elt := S16 (DSP.Clip_S16 (Sample));
            end loop;
         end;
      end if;
   end Render;

   ------------
   -- Key_On --
   ------------

   procedure Key_On (This     : in out Instance;
                     Key      :       MIDI.MIDI_Key;
                     Velocity :       Tresses.Param_Range)
   is
      On_Before : constant Boolean := (for some V of This.Voices => V.On);

      procedure Start_Voice (Id : Voice_Id) is
         V : Voice renames This.Voices (Id);
      begin
         V.Target_Phase_Incr :=
           DSP.Compute_Phase_Increment (S16 (Tresses.MIDI_Pitch (Key)));

         if V.Current_Phase_Incr = 0 then
            V.Current_Phase_Incr := V.Target_Phase_Incr;
            V.Start_Phase_Incr := V.Target_Phase_Incr;
         else
            V.Start_Phase_Incr := V.Current_Phase_Incr;
         end if;

         V.Note := Key;
         V.On := True;

         if not On_Before then
            Envelopes.AR.On (This.Env, Velocity);
            Envelopes.AR.On (This.Glide_Env, Velocity);
         end if;
      end Start_Voice;

   begin

      --  Try to find a free voice
      for Id in This.Voices'Range loop
         if not This.Voices (Id).On then
            Start_Voice (Id);
            return;
         end if;
      end loop;

      --  All voices are in use, just pick the next one...
      Start_Voice (This.Next);

      if This.Next = Voice_Id'Last then
         This.Next := Voice_Id'First;
      else
         This.Next := This.Next + 1;
      end if;
   end Key_On;

   --------------
   -- Note_Off --
   --------------

   procedure Key_Off (This : in out Instance;
                       Key  :        MIDI.MIDI_Key)
   is
      use MIDI;

   begin
      for V of This.Voices loop
         if V.On and then V.Note = Key then
            V.On := False;
         end if;
      end loop;

      if (for all V of This.Voices => not V.On) then
         Envelopes.AR.Off (This.Env);
         Envelopes.AR.Off (This.Glide_Env);
      end if;
   end Key_Off;

end WNM.Voices.Chord_Voice;
