-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2024 Fabien Chouteau                    --
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

with Tresses.Envelopes.AR; use Tresses.Envelopes.AR;

with WNM.Project;

with WNM.MIDI_Clock;
with MIDI.Time; use MIDI.Time;

package body WNM.Voices.Stutter_FX is

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (This   : in out Instance;
                       Mode :        Mode_Kind)
   is
   begin
      This.Mode := Mode;
   end Set_Mode;

   ----------
   -- Mode --
   ----------

   function Mode (This : Instance) return Mode_Kind
   is (This.Mode);

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Instance;
                     Buffer : in out WNM_HAL.Stereo_Buffer)
   is

      Last_Mute : constant Boolean := This.Mute;
      Amp : S32;
   begin
      if This.Do_Init then
         This.Do_Init := False;

         --  Very short attack and release
         Init (This.Env,
               Do_Hold => True,
               Attack_Speed => S_Half_Second,
               Release_Speed => S_Half_Second);
      end if;

      Set_Attack (This.Env, MIDI_Param (WNM.Project.Stutter_Attack));
      Set_Release (This.Env, MIDI_Param (WNM.Project.Stutter_Release));

      This.Mute :=
        (case This.Mode is
            when Off => False,
            when On_Short =>
              WNM.MIDI_Clock.Running
             and then
              WNM.Project.Stutter_Step_Mute
                (WNM.Project.Stutter_Pattern_A,
                 WNM.MIDI_Clock.Step mod 24),
            when On_Trip =>
              WNM.MIDI_Clock.Running
               and then
              WNM.Project.Stutter_Step_Mute
                (WNM.Project.Stutter_Pattern_B,
                 WNM.MIDI_Clock.Step mod 24));

      if not Last_Mute and then This.Mute then
         --  Entering mute
         On (This.Env, Param_Range'Last);

      elsif Last_Mute and then not This.Mute then
         --  Leaving mute
         Off (This.Env);
      end if;

      case Current_Segment (This.Env) is
         when Attack | Release =>

            for Elt of Buffer loop
               Amp := S32 (S16'Last) - S32 (Render (This.Env));
               Elt.L := S16 ((S32 (Elt.L) * Amp) / 2**15);
               Elt.R := S16 ((S32 (Elt.R) * Amp) / 2**15);
            end loop;

         when Hold =>
            Buffer := (others => (0, 0));

         when Dead =>
            null;

      end case;

   end Render;

end WNM.Voices.Stutter_FX;
