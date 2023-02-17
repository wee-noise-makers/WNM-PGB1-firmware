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

with Interfaces; use Interfaces;

with Tresses.Envelopes.AR; use Tresses.Envelopes.AR;
with Tresses.DSP;
with Tresses.Resources;

with WNM.Sample_Library; use WNM.Sample_Library;

with GNAT.IO;

package body WNM.Synth.Sampler_Voice is

   Pitch_Table : constant array (MIDI.MIDI_Key) of Sample_Pitch :=
     (
      00 => 0.03125 * 1.000000,
      01 => 0.03125 * 1.059463,
      02 => 0.03125 * 1.122462,
      03 => 0.03125 * 1.189207,
      04 => 0.03125 * 1.259921,
      05 => 0.03125 * 1.334840,
      06 => 0.03125 * 1.414214,
      07 => 0.03125 * 1.498307,
      08 => 0.03125 * 1.587401,
      09 => 0.03125 * 1.681793,
      10 => 0.03125 * 1.781797,
      11 => 0.03125 * 1.887749,

      MIDI.C0  => 0.0625 * 1.000000,
      MIDI.Cs0 => 0.0625 * 1.059463,
      MIDI.D0  => 0.0625 * 1.122462,
      MIDI.Ds0 => 0.0625 * 1.189207,
      MIDI.E0  => 0.0625 * 1.259921,
      MIDI.F0  => 0.0625 * 1.334840,
      MIDI.Fs0 => 0.0625 * 1.414214,
      MIDI.G0  => 0.0625 * 1.498307,
      MIDI.Gs0 => 0.0625 * 1.587401,
      MIDI.A0  => 0.0625 * 1.681793,
      MIDI.As0 => 0.0625 * 1.781797,
      MIDI.B0  => 0.0625 * 1.887749,

      MIDI.C1  => 0.125 * 1.000000,
      MIDI.Cs1 => 0.125 * 1.059463,
      MIDI.D1  => 0.125 * 1.122462,
      MIDI.Ds1 => 0.125 * 1.189207,
      MIDI.E1  => 0.125 * 1.259921,
      MIDI.F1  => 0.125 * 1.334840,
      MIDI.Fs1 => 0.125 * 1.414214,
      MIDI.G1  => 0.125 * 1.498307,
      MIDI.Gs1 => 0.125 * 1.587401,
      MIDI.A1  => 0.125 * 1.681793,
      MIDI.As1 => 0.125 * 1.781797,
      MIDI.B1  => 0.125 * 1.887749,

      MIDI.C2  => 0.25 * 1.000000,
      MIDI.Cs2 => 0.25 * 1.059463,
      MIDI.D2  => 0.25 * 1.122462,
      MIDI.Ds2 => 0.25 * 1.189207,
      MIDI.E2  => 0.25 * 1.259921,
      MIDI.F2  => 0.25 * 1.334840,
      MIDI.Fs2 => 0.25 * 1.414214,
      MIDI.G2  => 0.25 * 1.498307,
      MIDI.Gs2 => 0.25 * 1.587401,
      MIDI.A2  => 0.25 * 1.681793,
      MIDI.As2 => 0.25 * 1.781797,
      MIDI.B2  => 0.25 * 1.887749,

      MIDI.C3  => 0.5 * 1.000000,
      MIDI.Cs3 => 0.5 * 1.059463,
      MIDI.D3  => 0.5 * 1.122462,
      MIDI.Ds3 => 0.5 * 1.189207,
      MIDI.E3  => 0.5 * 1.259921,
      MIDI.F3  => 0.5 * 1.334840,
      MIDI.Fs3 => 0.5 * 1.414214,
      MIDI.G3  => 0.5 * 1.498307,
      MIDI.Gs3 => 0.5 * 1.587401,
      MIDI.A3  => 0.5 * 1.681793,
      MIDI.As3 => 0.5 * 1.781797,
      MIDI.B3  => 0.5 * 1.887749,

      MIDI.C4  => 1.0 * 1.000000,
      MIDI.Cs4 => 1.0 * 1.059463,
      MIDI.D4  => 1.0 * 1.122462,
      MIDI.Ds4 => 1.0 * 1.189207,
      MIDI.E4  => 1.0 * 1.259921,
      MIDI.F4  => 1.0 * 1.334840,
      MIDI.Fs4 => 1.0 * 1.414214,
      MIDI.G4  => 1.0 * 1.498307,
      MIDI.Gs4 => 1.0 * 1.587401,
      MIDI.A4  => 1.0 * 1.681793,
      MIDI.As4 => 1.0 * 1.781797,
      MIDI.B4  => 1.0 * 1.887749,

      MIDI.C5  => 2.0 * 1.000000,
      MIDI.Cs5 => 2.0 * 1.059463,
      MIDI.D5  => 2.0 * 1.122462,
      MIDI.Ds5 => 2.0 * 1.189207,
      MIDI.E5  => 2.0 * 1.259921,
      MIDI.F5  => 2.0 * 1.334840,
      MIDI.Fs5 => 2.0 * 1.414214,
      MIDI.G5  => 2.0 * 1.498307,
      MIDI.Gs5 => 2.0 * 1.587401,
      MIDI.A5  => 2.0 * 1.681793,
      MIDI.As5 => 2.0 * 1.781797,
      MIDI.B5  => 2.0 * 1.887749,

      MIDI.C6  => 4.0 * 1.000000,
      MIDI.Cs6 => 4.0 * 1.059463,
      MIDI.D6  => 4.0 * 1.122462,
      MIDI.Ds6 => 4.0 * 1.189207,
      MIDI.E6  => 4.0 * 1.259921,
      MIDI.F6  => 4.0 * 1.334840,
      MIDI.Fs6 => 4.0 * 1.414214,
      MIDI.G6  => 4.0 * 1.498307,
      MIDI.Gs6 => 4.0 * 1.587401,
      MIDI.A6  => 4.0 * 1.681793,
      MIDI.As6 => 4.0 * 1.781797,
      MIDI.B6  => 4.0 * 1.887749,

      MIDI.C7  => 8.0 * 1.000000,
      MIDI.Cs7 => 8.0 * 1.059463,
      MIDI.D7  => 8.0 * 1.122462,
      MIDI.Ds7 => 8.0 * 1.189207,
      MIDI.E7  => 8.0 * 1.259921,
      MIDI.F7  => 8.0 * 1.334840,
      MIDI.Fs7 => 8.0 * 1.414214,
      MIDI.G7  => 8.0 * 1.498307,
      MIDI.Gs7 => 8.0 * 1.587401,
      MIDI.A7  => 8.0 * 1.681793,
      MIDI.As7 => 8.0 * 1.781797,
      MIDI.B7  => 8.0 * 1.887749,

      MIDI.C8  => 16.0 * 1.000000,
      MIDI.Cs8 => 16.0 * 1.059463,
      MIDI.D8  => 16.0 * 1.122462,
      MIDI.Ds8 => 16.0 * 1.189207,
      MIDI.E8  => 16.0 * 1.259921,
      MIDI.F8  => 16.0 * 1.334840,
      MIDI.Fs8 => 16.0 * 1.414214,
      MIDI.G8  => 16.0 * 1.498307,
      MIDI.Gs8 => 16.0 * 1.587401,
      MIDI.A8  => 16.0 * 1.681793,
      MIDI.As8 => 16.0 * 1.781797,
      MIDI.B8  => 16.0 * 1.887749,

      MIDI.C9  => 32.0 * 1.000000,
      MIDI.Cs9 => 32.0 * 1.059463,
      MIDI.D9  => 32.0 * 1.122462,
      MIDI.Ds9 => 32.0 * 1.189207,
      MIDI.E9  => 32.0 * 1.259921,
      MIDI.F9  => 32.0 * 1.334840,
      MIDI.Fs9 => 32.0 * 1.414214,
      MIDI.G9  => 32.0 * 1.498307);

   ----------------
   -- Set_Sample --
   ----------------

   procedure Set_Sample (This : in out Instance; Id : MIDI.MIDI_Data) is
      use HAL;
      New_Sample : constant Valid_Sample_Index :=
        Valid_Sample_Index (Integer (Id) + 1);
   begin
      if This.Sample_Id /= New_Sample then
         This.Sample_Id := New_Sample;
         This.Init;
      end if;
   end Set_Sample;

   --------------------
   -- Set_MIDI_Pitch --
   --------------------

   procedure Set_MIDI_Pitch (This : in out Instance;
                             Key  :        MIDI.MIDI_Key)
   is
   begin
      This.S_Pitch := Pitch_Table (Key);
   end Set_MIDI_Pitch;

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
      Sample : Single_Sample_Data renames Sample_Data.all (This.Sample_Id);

      Start_P : constant Param_Range := This.Params (P_Start);
      Start_Point : constant Sample_Point_Count :=
        Sample_Point_Count
          (U32 (Single_Sample_Point_Cnt * U32 (Start_P) /
             U32 (Param_Range'Last)));
   begin
      if This.Do_Init then
         This.Do_Init := False;

         Init (This.Env, Do_Hold => True);
         Set_Attack (This.Env, U7 (0));

      end if;

      case This.Do_Strike.Event is
         when On =>
            This.Do_Strike.Event := None;

            This.Cursor := 1.0 + Sample_Pitch (Start_Point);

            On (This.Env, This.Do_Strike.Velocity);

         when Off =>
            This.Do_Strike.Event := None;

            Off (This.Env);
         when None => null;
      end case;

      Set_Release (This.Env, This.Params (P_Release));

      declare
         Out_Index : Natural := Buffer'First;
         Sample_Index : Sample_Point_Index;
         Sample_Point : S32;

         Fuzzed : S16;
         Drive_Amount : U32;

      begin

         --  Control curve: Drive to the power of 2
         Drive_Amount := U32 (This.Params (P_Drive));
         Drive_Amount := Shift_Right (Drive_Amount**2, 15);
         Drive_Amount := Drive_Amount * 2;

         loop
            exit when Out_Index > Buffer'Last;

            exit when Integer (This.Cursor) >
              Integer (Single_Sample_Point_Cnt);

            Render (This.Env);

            Sample_Index := Sample_Point_Count (This.Cursor);

            Sample_Point := S32 (Sample (Sample_Index));

            --  Symmetrical soft clipping
            Fuzzed := DSP.Interpolate88
              (Resources.WS_Violent_Overdrive,
               U16 (Sample_Point + 32_768));

            --  Mix clean and overdrive signals
            Sample_Point := S32 (Tresses.DSP.Mix (S16 (Sample_Point),
                                                  Fuzzed,
                                                  U16 (Drive_Amount)));

            --  Amplitude envelope
            Sample_Point := (Sample_Point * Low_Pass (This.Env)) / 2**15;

            Buffer (Out_Index) := S16 (Sample_Point);

            Out_Index := Out_Index + 1;

            This.Cursor := This.Cursor + This.S_Pitch;
         end loop;

         --  Fill remaining point, if any...
         Buffer (Out_Index .. Buffer'Last) := (others => 0);
      end;
   end Render;

end WNM.Synth.Sampler_Voice;
