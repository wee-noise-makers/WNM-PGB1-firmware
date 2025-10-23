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

with Tresses.Drums.Clap;
with Tresses.Macro;
with Tresses.Drums.Snare;
with Tresses.Drums.Wave_Snare;
with Tresses.Resources;
with WNM.Synth;

package body WNM.Voices.Snare_Voice is

   ------------
   -- Engine --
   ------------

   function Engine (This : Instance) return Snare_Engine
   is (This.Engine);

   ----------------
   -- Set_Engine --
   ----------------

   procedure Set_Engine (This : in out Instance; E : Snare_Engine) is
   begin
      if E /= This.Engine then
         This.Engine := E;
         Init (This);
      end if;
   end Set_Engine;

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Instance) is
   begin
      This.Do_Init := True;
   end Init;

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Instance;
                     Buffer :    out Tresses.Mono_Buffer)
   is
   begin
      case This.Engine is
         when Sine_Snare =>
            Drums.Wave_Snare.Render_Snare
              (Buffer,
               Params => This.Params,
               Tone_Waveform => Resources.WAV_Sine'Access,
               Phase => This.Phase,
               Phase_Increment => This.Phase_Increment,
               Target_Phase_Increment => This.Target_Phase_Increment,
               Tone_Env => This.Env0,
               Noise_Env => This.Env1,
               Rng => This.Rng,
               Pitch => This.Pitch,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when Saw_Snare =>
            Drums.Wave_Snare.Render_Snare
              (Buffer,
               Params => This.Params,
               Tone_Waveform => Resources.WAV_Sawtooth'Access,
               Phase => This.Phase,
               Phase_Increment => This.Phase_Increment,
               Target_Phase_Increment => This.Target_Phase_Increment,
               Tone_Env => This.Env0,
               Noise_Env => This.Env1,
               Rng => This.Rng,
               Pitch => This.Pitch,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when Triangle_Snare =>
            Drums.Wave_Snare.Render_Snare
              (Buffer,
               Params => This.Params,
               Tone_Waveform => Resources.WAV_Triangle'Access,
               Phase => This.Phase,
               Phase_Increment => This.Phase_Increment,
               Target_Phase_Increment => This.Target_Phase_Increment,
               Tone_Env => This.Env0,
               Noise_Env => This.Env1,
               Rng => This.Rng,
               Pitch => This.Pitch,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when User_Wave_Snare =>
            Drums.Wave_Snare.Render_Snare
              (Buffer,
               Params => This.Params,
               Tone_Waveform => Synth.User_Waveform'Access,
               Phase => This.Phase,
               Phase_Increment => This.Phase_Increment,
               Target_Phase_Increment => This.Target_Phase_Increment,
               Tone_Env => This.Env0,
               Noise_Env => This.Env1,
               Rng => This.Rng,
               Pitch => This.Pitch,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when Virt_Analog =>
            Tresses.Drums.Snare.Render_Snare
              (Buffer,
               Params => This.Params,
               Pulse0 => This.Pulse0,
               Pulse1 => This.Pulse1,
               Pulse2 => This.Pulse2,
               Pulse3 => This.Pulse3,
               Filter0 => This.Filter0,
               Filter1 => This.Filter1,
               Filter2 => This.Filter2,
               Rng => This.Rng,
               Pitch =>  This.Pitch,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when Clap =>
            Tresses.Drums.Clap.Render_Clap (Buffer,
                                            Params => This.Params,
                                            Filter => This.Filter0,
                                            Rng =>  This.Rng,
                                            Env => This.Env0,
                                            Re_Trig => This.Re_Trig,
                                            Pitch => This.Pitch,
                                            Do_Init => This.Do_Init,
                                            Do_Strike => This.Do_Strike);

      end case;
   end Render;

   -------------------
   -- Tresse_Engine --
   -------------------

   function Tresse_Engine (E : Snare_Engine) return Tresses.Engines
   is (case E is
          when Sine_Snare => Tresses.Drum_Sine_Snare,
          when Saw_Snare => Tresses.Drum_Saw_Snare,
          when Triangle_Snare => Tresses.Drum_Triangle_Snare,
          when User_Wave_Snare => Tresses.Drum_User_Wave_Snare,
          when Virt_Analog => Tresses.Drum_Snare,
          when Clap  => Tresses.Drum_Clap);

   ---------
   -- Img --
   ---------

   function Img (E : Snare_Engine) return String
   is (Tresses.Img (Tresse_Engine (E)));

   -----------------
   -- Param_Label --
   -----------------

   overriding
   function Param_Label (This : Instance; Id : Param_Id) return String
   is (Tresses.Macro.Param_Label (Tresse_Engine (This.Engine), Id));

   -----------------------
   -- Param_Short_Label --
   -----------------------

   overriding
   function Param_Short_Label (This : Instance; Id : Param_Id)
                               return Short_Label
   is (Tresses.Macro.Param_Short_Label (Tresse_Engine (This.Engine), Id));

end WNM.Voices.Snare_Voice;
