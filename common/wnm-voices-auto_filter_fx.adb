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

with Ada.Text_IO; use Ada.Text_IO;

with Tresses.Filters.SVF; use Tresses.Filters.SVF;
with Tresses.Envelopes.AR; use Tresses.Envelopes.AR;
with Tresses.LFO; use Tresses.LFO;

package body WNM.Voices.Auto_Filter_FX is

   Low_Pass_Cutoff : constant Param_Range :=
     Param_Range (MIDI_Pitch (MIDI.C3));

   Band_Pass_Cutoff : constant Param_Range :=
     Param_Range (MIDI_Pitch (MIDI.C4));

   High_Pass_Cutoff : constant Param_Range :=
     Param_Range (MIDI_Pitch (MIDI.C5));

   ----------------
   -- Set_Motion --
   ----------------

   procedure Set_Mode (This   : in out Instance;
                         Mode :        Mode_Kind)
   is
   begin
      if Mode /= This.Mode then
         Sync (This.LFO);
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
      LFO_Val : S16;
   begin
      if This.Do_Init then
         This.Do_Init := False;

         Init (This.Left);
         Init (This.Right);

         Init (This.LFO);
         Set_Shape (This.LFO, Sine);
         Set_Rate (This.LFO,
                   Param_Range'Last / 5,
                   WNM_Configuration.Audio.Samples_Per_Buffer);

         Set_Amplitude (This.LFO, Param_Range'Last / 4);
         Set_Amp_Mode (This.LFO, LFO.Positive);
         Set_Loop_Mode (This.LFO, Repeat);

         Sync (This.LFO);

         Set_Resonance (This.Left, Param_Range'Last / 2);
         Set_Frequency (This.Left, Param_Range (MIDI_Pitch (MIDI.C6)));

         Set_Resonance (This.Right, Param_Range'Last / 2);
         Set_Frequency (This.Right, Param_Range (MIDI_Pitch (MIDI.C6)));
      end if;

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
         when Sweep_Low_Pass | Sweep_Band_Pass | Sweep_High_Pass =>

            LFO_Val := Render (This.LFO);

            Set_Frequency (This.Left,
                           Param_Range'Last / 5 + Param_Range (LFO_Val));
            Set_Frequency (This.Right,
                           Param_Range'Last / 5 + Param_Range (LFO_Val));

         when Fix_Low_Pass =>
            Set_Frequency (This.Left, Low_Pass_Cutoff);
            Set_Frequency (This.Right, Low_Pass_Cutoff);

         when Fix_Band_Pass =>
            Set_Frequency (This.Left, Band_Pass_Cutoff);
            Set_Frequency (This.Right, Band_Pass_Cutoff);

         when Fix_High_Pass =>
            Set_Frequency (This.Left, High_Pass_Cutoff);
            Set_Frequency (This.Right, High_Pass_Cutoff);

         when Off =>
            null;

      end case;

      if This.Mode /= Off then
         for Elt of Buffer loop
            Elt.L := S16 (Process (This.Left, S32 (Elt.L)));
            Elt.R := S16 (Process (This.Right, S32 (Elt.R)));
         end loop;
      end if;
   end Render;

end WNM.Voices.Auto_Filter_FX;
