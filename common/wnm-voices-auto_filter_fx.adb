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

with Tresses.Filters.SVF; use Tresses.Filters.SVF;
with Tresses.LFO; use Tresses.LFO;
with Tresses.DSP; use Tresses.DSP;
with WNM.Project;

package body WNM.Voices.Auto_Filter_FX is

   G_Last_Cutoff : Pitch_Range := 0 with Volatile;
   G_Last_Cutoff_After_LFO : Pitch_Range := 0 with Volatile;

   function Last_Cutoff return Pitch_Range
   is (G_Last_Cutoff);

   function Last_Cutoff_After_LFO return Pitch_Range
   is (G_Last_Cutoff_After_LFO);

   ----------------
   -- Set_Motion --
   ----------------

   procedure Set_Mode (This   : in out Instance;
                         Mode :        Mode_Kind)
   is
   begin
      if Mode /= This.Mode then
         This.Mode := Mode;

         case Mode is
            when Sweep_Low_Pass =>
               --  Shift low pass phase to start at high frequency and go down
               Sync (This.LFO, U32'Last / 2);

            when Sweep_Band_Pass =>
               --  Shift band pass phase to start at mid frequency and go up
               --  then down.
               Sync (This.LFO, U32'Last / 4);

            when others =>
               Sync (This.LFO);
         end case;
      end if;
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
      Cutoff : Pitch_Range;
      Reso : Param_Range;
   begin
      if This.Do_Init then
         This.Do_Init := False;

         Init (This.Left);
         Init (This.Right);

         Init (This.LFO);
         Set_Shape (This.LFO, Sine);
         Set_Amp_Mode (This.LFO, LFO.Positive);
         Set_Loop_Mode (This.LFO, Repeat);
         Sync (This.LFO);

         Set_Resonance (This.Left, Param_Range'Last / 2);
         Set_Frequency (This.Left, Param_Range (MIDI_Pitch (MIDI.C6)));

         Set_Resonance (This.Right, Param_Range'Last / 2);
         Set_Frequency (This.Right, Param_Range (MIDI_Pitch (MIDI.C6)));
      end if;

      Set_Rate (This.LFO,
                MIDI_Param (Project.FX_Filter_Sweep_Rate),

                --  Scale down the max LFO frequency by 32 (160/32 = 5Hz)
                WNM_Configuration.Audio.Samples_Per_Buffer  / 32);

      Set_Amplitude (This.LFO,
                     MIDI_Param (Project.FX_Filter_Sweep_Amp) / 2);

      case This.Mode is
         when Fix_Low_Pass | Sweep_Low_Pass =>
            Set_Mode (This.Left, Low_Pass);
            Set_Mode (This.Right, Low_Pass);

         when Fix_Band_Pass | Sweep_Band_Pass =>
            Set_Mode (This.Left, Band_Pass);
            Set_Mode (This.Right, Band_Pass);

         when Fix_High_Pass | Sweep_High_Pass =>
            Set_Mode (This.Left, High_Pass);
            Set_Mode (This.Right, High_Pass);

         when Off =>
            null;

      end case;

      case This.Mode is
         when Fix_Low_Pass | Sweep_Low_Pass  =>
            Cutoff := MIDI_Pitch (WNM.Project.FX_Filter_LP_Cutoff);

         when Fix_Band_Pass | Sweep_Band_Pass =>
            Cutoff := MIDI_Pitch (WNM.Project.FX_Filter_BP_Cutoff);

         when Fix_High_Pass | Sweep_High_Pass =>
            Cutoff := MIDI_Pitch (WNM.Project.FX_Filter_HP_Cutoff);

         when Off =>
            Cutoff := Pitch_Range'Last / 2;
      end case;

      G_Last_Cutoff := Cutoff;

      if This.Mode in Sweep_Low_Pass | Sweep_Band_Pass | Sweep_High_Pass then
         declare
            LFO_Val : constant S32 := S32 (Render (This.LFO)) / 4;
            Cutoff_S32 : constant S32 := S32 (Cutoff);
         begin
            Cutoff := Pitch_Range
              (Clip (Cutoff_S32 + LFO_Val,
               S32 (Pitch_Range'First),
               S32 (Pitch_Range'Last)));
         end;
      end if;

      This.Cutoff_LP :=
        This.Cutoff_LP + ((S32 (Cutoff) - This.Cutoff_LP) / 2**3);
      Cutoff := Pitch_Range (Clip (This.Cutoff_LP,
                             S32 (Pitch_Range'First),
                             S32 (Pitch_Range'Last)));

      G_Last_Cutoff_After_LFO := Cutoff;

      case This.Mode is
         when Fix_Low_Pass | Sweep_Low_Pass =>
            Reso := MIDI_Param (WNM.Project.FX_Filter_LP_Reso);
         when Fix_Band_Pass | Sweep_Band_Pass =>
            Reso := MIDI_Param (WNM.Project.FX_Filter_BP_Reso);
         when Fix_High_Pass | Sweep_High_Pass =>
            Reso := MIDI_Param (WNM.Project.FX_Filter_HP_Reso);
         when Off =>
            Reso := Param_Range'Last / 2;
      end case;

      Set_Frequency (This.Left, Param_Range (Cutoff));
      Set_Frequency (This.Right, Param_Range (Cutoff));
      Set_Resonance (This.Left, Reso);
      Set_Resonance (This.Right, Reso);

      if This.Mode /= Off then
         for Elt of Buffer loop
            Elt.L := S16 (Process (This.Left, S32 (Elt.L)));
            Elt.R := S16 (Process (This.Right, S32 (Elt.R)));
         end loop;
      end if;
   end Render;

end WNM.Voices.Auto_Filter_FX;
