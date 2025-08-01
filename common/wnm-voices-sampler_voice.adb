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

with Interfaces;
with Ada.Unchecked_Conversion;

with Tresses.Envelopes.AR; use Tresses.Envelopes.AR;
with Tresses.DSP;
with Tresses.Resources;
with Tresses.FX.Bitcrusher;

with WNM.Sample_Library; use WNM.Sample_Library;
with WNM.Sample_Recording;

package body WNM.Voices.Sampler_Voice is
   use WNM_Configuration.Samples;

   C4_Phase_Incr : constant := 2.0**(Phase_Frac_Bits);

   Pitch_Table2 : constant array (MIDI.MIDI_Key) of Sample_Phase :=
     (
      00 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.000000),
      01 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.059463),
      02 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.122462),
      03 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.189207),
      04 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.259921),
      05 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.334840),
      06 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.414214),
      07 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.498307),
      08 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.587401),
      09 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.681793),
      10 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.781797),
      11 => Sample_Phase (C4_Phase_Incr * 0.03125 * 1.887749),

      12 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.000000),
      13 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.059463),
      14 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.122462),
      15 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.189207),
      16 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.259921),
      17 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.334840),
      18 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.414214),
      19 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.498307),
      20 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.587401),
      21 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.681793),
      22 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.781797),
      23 => Sample_Phase (C4_Phase_Incr * 0.0625 * 1.887749),

      MIDI.C1  => Sample_Phase (C4_Phase_Incr * 0.125 * 1.000000),
      MIDI.Cs1 => Sample_Phase (C4_Phase_Incr * 0.125 * 1.059463),
      MIDI.D1  => Sample_Phase (C4_Phase_Incr * 0.125 * 1.122462),
      MIDI.Ds1 => Sample_Phase (C4_Phase_Incr * 0.125 * 1.189207),
      MIDI.E1  => Sample_Phase (C4_Phase_Incr * 0.125 * 1.259921),
      MIDI.F1  => Sample_Phase (C4_Phase_Incr * 0.125 * 1.334840),
      MIDI.Fs1 => Sample_Phase (C4_Phase_Incr * 0.125 * 1.414214),
      MIDI.G1  => Sample_Phase (C4_Phase_Incr * 0.125 * 1.498307),
      MIDI.Gs1 => Sample_Phase (C4_Phase_Incr * 0.125 * 1.587401),
      MIDI.A1  => Sample_Phase (C4_Phase_Incr * 0.125 * 1.681793),
      MIDI.As1 => Sample_Phase (C4_Phase_Incr * 0.125 * 1.781797),
      MIDI.B1  => Sample_Phase (C4_Phase_Incr * 0.125 * 1.887749),

      MIDI.C2  => Sample_Phase (C4_Phase_Incr * 0.25 * 1.000000),
      MIDI.Cs2 => Sample_Phase (C4_Phase_Incr * 0.25 * 1.059463),
      MIDI.D2  => Sample_Phase (C4_Phase_Incr * 0.25 * 1.122462),
      MIDI.Ds2 => Sample_Phase (C4_Phase_Incr * 0.25 * 1.189207),
      MIDI.E2  => Sample_Phase (C4_Phase_Incr * 0.25 * 1.259921),
      MIDI.F2  => Sample_Phase (C4_Phase_Incr * 0.25 * 1.334840),
      MIDI.Fs2 => Sample_Phase (C4_Phase_Incr * 0.25 * 1.414214),
      MIDI.G2  => Sample_Phase (C4_Phase_Incr * 0.25 * 1.498307),
      MIDI.Gs2 => Sample_Phase (C4_Phase_Incr * 0.25 * 1.587401),
      MIDI.A2  => Sample_Phase (C4_Phase_Incr * 0.25 * 1.681793),
      MIDI.As2 => Sample_Phase (C4_Phase_Incr * 0.25 * 1.781797),
      MIDI.B2  => Sample_Phase (C4_Phase_Incr * 0.25 * 1.887749),

      MIDI.C3  => Sample_Phase (C4_Phase_Incr * 0.5 * 1.000000),
      MIDI.Cs3 => Sample_Phase (C4_Phase_Incr * 0.5 * 1.059463),
      MIDI.D3  => Sample_Phase (C4_Phase_Incr * 0.5 * 1.122462),
      MIDI.Ds3 => Sample_Phase (C4_Phase_Incr * 0.5 * 1.189207),
      MIDI.E3  => Sample_Phase (C4_Phase_Incr * 0.5 * 1.259921),
      MIDI.F3  => Sample_Phase (C4_Phase_Incr * 0.5 * 1.334840),
      MIDI.Fs3 => Sample_Phase (C4_Phase_Incr * 0.5 * 1.414214),
      MIDI.G3  => Sample_Phase (C4_Phase_Incr * 0.5 * 1.498307),
      MIDI.Gs3 => Sample_Phase (C4_Phase_Incr * 0.5 * 1.587401),
      MIDI.A3  => Sample_Phase (C4_Phase_Incr * 0.5 * 1.681793),
      MIDI.As3 => Sample_Phase (C4_Phase_Incr * 0.5 * 1.781797),
      MIDI.B3  => Sample_Phase (C4_Phase_Incr * 0.5 * 1.887749),

      MIDI.C4  => Sample_Phase (C4_Phase_Incr * 1.0 * 1.000000),
      MIDI.Cs4 => Sample_Phase (C4_Phase_Incr * 1.0 * 1.059463),
      MIDI.D4  => Sample_Phase (C4_Phase_Incr * 1.0 * 1.122462),
      MIDI.Ds4 => Sample_Phase (C4_Phase_Incr * 1.0 * 1.189207),
      MIDI.E4  => Sample_Phase (C4_Phase_Incr * 1.0 * 1.259921),
      MIDI.F4  => Sample_Phase (C4_Phase_Incr * 1.0 * 1.334840),
      MIDI.Fs4 => Sample_Phase (C4_Phase_Incr * 1.0 * 1.414214),
      MIDI.G4  => Sample_Phase (C4_Phase_Incr * 1.0 * 1.498307),
      MIDI.Gs4 => Sample_Phase (C4_Phase_Incr * 1.0 * 1.587401),
      MIDI.A4  => Sample_Phase (C4_Phase_Incr * 1.0 * 1.681793),
      MIDI.As4 => Sample_Phase (C4_Phase_Incr * 1.0 * 1.781797),
      MIDI.B4  => Sample_Phase (C4_Phase_Incr * 1.0 * 1.887749),

      MIDI.C5  => Sample_Phase (C4_Phase_Incr * 2.0 * 1.000000),
      MIDI.Cs5 => Sample_Phase (C4_Phase_Incr * 2.0 * 1.059463),
      MIDI.D5  => Sample_Phase (C4_Phase_Incr * 2.0 * 1.122462),
      MIDI.Ds5 => Sample_Phase (C4_Phase_Incr * 2.0 * 1.189207),
      MIDI.E5  => Sample_Phase (C4_Phase_Incr * 2.0 * 1.259921),
      MIDI.F5  => Sample_Phase (C4_Phase_Incr * 2.0 * 1.334840),
      MIDI.Fs5 => Sample_Phase (C4_Phase_Incr * 2.0 * 1.414214),
      MIDI.G5  => Sample_Phase (C4_Phase_Incr * 2.0 * 1.498307),
      MIDI.Gs5 => Sample_Phase (C4_Phase_Incr * 2.0 * 1.587401),
      MIDI.A5  => Sample_Phase (C4_Phase_Incr * 2.0 * 1.681793),
      MIDI.As5 => Sample_Phase (C4_Phase_Incr * 2.0 * 1.781797),
      MIDI.B5  => Sample_Phase (C4_Phase_Incr * 2.0 * 1.887749),

      MIDI.C6  => Sample_Phase (C4_Phase_Incr * 4.0 * 1.000000),
      MIDI.Cs6 => Sample_Phase (C4_Phase_Incr * 4.0 * 1.059463),
      MIDI.D6  => Sample_Phase (C4_Phase_Incr * 4.0 * 1.122462),
      MIDI.Ds6 => Sample_Phase (C4_Phase_Incr * 4.0 * 1.189207),
      MIDI.E6  => Sample_Phase (C4_Phase_Incr * 4.0 * 1.259921),
      MIDI.F6  => Sample_Phase (C4_Phase_Incr * 4.0 * 1.334840),
      MIDI.Fs6 => Sample_Phase (C4_Phase_Incr * 4.0 * 1.414214),
      MIDI.G6  => Sample_Phase (C4_Phase_Incr * 4.0 * 1.498307),
      MIDI.Gs6 => Sample_Phase (C4_Phase_Incr * 4.0 * 1.587401),
      MIDI.A6  => Sample_Phase (C4_Phase_Incr * 4.0 * 1.681793),
      MIDI.As6 => Sample_Phase (C4_Phase_Incr * 4.0 * 1.781797),
      MIDI.B6  => Sample_Phase (C4_Phase_Incr * 4.0 * 1.887749),

      MIDI.C7  => Sample_Phase (C4_Phase_Incr * 8.0 * 1.000000),
      MIDI.Cs7 => Sample_Phase (C4_Phase_Incr * 8.0 * 1.059463),
      MIDI.D7  => Sample_Phase (C4_Phase_Incr * 8.0 * 1.122462),
      MIDI.Ds7 => Sample_Phase (C4_Phase_Incr * 8.0 * 1.189207),
      MIDI.E7  => Sample_Phase (C4_Phase_Incr * 8.0 * 1.259921),
      MIDI.F7  => Sample_Phase (C4_Phase_Incr * 8.0 * 1.334840),
      MIDI.Fs7 => Sample_Phase (C4_Phase_Incr * 8.0 * 1.414214),
      MIDI.G7  => Sample_Phase (C4_Phase_Incr * 8.0 * 1.498307),
      MIDI.Gs7 => Sample_Phase (C4_Phase_Incr * 8.0 * 1.587401),
      MIDI.A7  => Sample_Phase (C4_Phase_Incr * 8.0 * 1.681793),
      MIDI.As7 => Sample_Phase (C4_Phase_Incr * 8.0 * 1.781797),
      MIDI.B7  => Sample_Phase (C4_Phase_Incr * 8.0 * 1.887749),

      MIDI.C8  => Sample_Phase (C4_Phase_Incr * 16.0 * 1.000000),
      MIDI.Cs8 => Sample_Phase (C4_Phase_Incr * 16.0 * 1.059463),
      MIDI.D8  => Sample_Phase (C4_Phase_Incr * 16.0 * 1.122462),
      MIDI.Ds8 => Sample_Phase (C4_Phase_Incr * 16.0 * 1.189207),
      MIDI.E8  => Sample_Phase (C4_Phase_Incr * 16.0 * 1.259921),
      MIDI.F8  => Sample_Phase (C4_Phase_Incr * 16.0 * 1.334840),
      MIDI.Fs8 => Sample_Phase (C4_Phase_Incr * 16.0 * 1.414214),
      MIDI.G8  => Sample_Phase (C4_Phase_Incr * 16.0 * 1.498307),
      MIDI.Gs8 => Sample_Phase (C4_Phase_Incr * 16.0 * 1.587401),
      MIDI.A8  => Sample_Phase (C4_Phase_Incr * 16.0 * 1.681793),
      MIDI.As8 => Sample_Phase (C4_Phase_Incr * 16.0 * 1.781797),
      MIDI.B8  => Sample_Phase (C4_Phase_Incr * 16.0 * 1.887749),

      120 => Sample_Phase (C4_Phase_Incr * 32.0 * 1.000000),
      121 => Sample_Phase (C4_Phase_Incr * 32.0 * 1.059463),
      122 => Sample_Phase (C4_Phase_Incr * 32.0 * 1.122462),
      123 => Sample_Phase (C4_Phase_Incr * 32.0 * 1.189207),
      124 => Sample_Phase (C4_Phase_Incr * 32.0 * 1.259921),
      125 => Sample_Phase (C4_Phase_Incr * 32.0 * 1.334840),
      126 => Sample_Phase (C4_Phase_Incr * 32.0 * 1.414214),
      127 => Sample_Phase (C4_Phase_Incr * 32.0 * 1.498307));

   ------------
   -- Engine --
   ------------

   function Engine (This : Instance) return Sampler_Engine
   is (This.Engine);

   ----------------
   -- Set_Engine --
   ----------------

   procedure Set_Engine (This : in out Instance; E : Sampler_Engine) is
   begin
      if E /= This.Engine then
         This.Engine := E;
         Init (This);
      end if;
   end Set_Engine;

   ----------------
   -- Set_Sample --
   ----------------

   procedure Set_Sample (This : in out Instance; Id : MIDI.MIDI_Data) is
      use MIDI;

      New_Sample : Sample_Index;
   begin
      if Id > MIDI.MIDI_Data (Sample_Index'Last) then
         New_Sample := Sample_Index'Last;
      else
         New_Sample := Sample_Index (Id);
      end if;

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
      This.Phase_Increment := Pitch_Table2 (Key);
   end Set_MIDI_Pitch;

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Instance) is
   begin
      This.Do_Init := True;
   end Init;

   ----------------------
   -- Render_Overdrive --
   ----------------------

   procedure Render_Overdrive (This   : in out Instance;
                               Buffer :    out Tresses.Mono_Buffer)
   is

      Points : Sample_Audio_Data renames
        Sample_Data.all (This.Sample_Id).Audio;

      Sample_Len : constant U32 :=
        U32'Min (U32 (Sample_Point_Index'Last),
                 U32 (Sample_Data.all (This.Sample_Id).Len));

      P : U32;
   begin

      if This.Do_Init then
         This.Do_Init := False;

         Init (This.Env,
               Do_Hold => True,
               Attack_Speed => S_Half_Second);
         Set_Attack (This.Env, Param_Range'Last / 4);

         This.Phase := 0;
      end if;

      case This.Do_Strike.Event is
         when On =>
            This.Do_Strike.Event := None;

            declare
               Start_Point : constant U32 :=
                 U32 (Points_Per_Sample * U32 (This.Params (P_Start)) /
                          U32 (Param_Range'Last));
            begin
               This.Phase := Start_Point * U32 (C4_Phase_Incr);
            end;

            On (This.Env, This.Do_Strike.Velocity);

         when Off =>
            This.Do_Strike.Event := None;

            Off (This.Env);
         when None => null;
      end case;

      Set_Release (This.Env, This.Params (P_Release));

      declare
         Out_Index : Natural := Buffer'First;
         Sample_Point : S32;

         Fuzzed : S16;
         Drive_Amount : U32;

      begin

         --  Control curve: Drive to the power of 2
         Drive_Amount := U32 (This.Params (P_Drive));
         Drive_Amount := Shift_Right (Drive_Amount**2, 15);
         Drive_Amount := Drive_Amount * 2;

         if Sample_Len > 0 then
            loop
               exit when Out_Index > Buffer'Last;

               P := Shift_Right (This.Phase, Phase_Frac_Bits);

               exit when P > Sample_Len - 1;

               --  Interpolation between two sample points
               declare
                  V : constant S32 :=
                    S32 (Shift_Right
                         (This.Phase, Phase_Frac_Bits - 15) and 16#7FFF#);

                  A : constant S32 := S32 (Points (Sample_Point_Index (P)));
                  B : constant S32 :=
                    S32 (Points (Sample_Point_Index (P) + 1));
               begin
                  Sample_Point := A + ((B - A) * V) / 2**15;
               end;

               This.Phase := This.Phase + This.Phase_Increment;

               --  Symmetrical soft clipping
               Fuzzed := DSP.Interpolate88
                 (Resources.WS_Extreme_Overdrive,
                  U16 (Sample_Point + 32_768));

               --  Mix clean and overdrive signals
               Sample_Point := S32 (Tresses.DSP.Mix (S16 (Sample_Point),
                                    Fuzzed,
                                                  U16 (Drive_Amount)));

               --  Amplitude envelope
               Render (This.Env);
               Sample_Point := (Sample_Point * Low_Pass (This.Env)) / 2**15;

               Buffer (Out_Index) := S16 (Sample_Point);

               Out_Index := Out_Index + 1;
            end loop;
         end if;

         --  Fill remaining point, if any...
         Buffer (Out_Index .. Buffer'Last) := (others => 0);
      end;
   end Render_Overdrive;

   --------------------
   -- Render_Crusher --
   --------------------

   procedure Render_Crusher (This   : in out Instance;
                             Buffer :    out Tresses.Mono_Buffer)
   is
      use Standard.Interfaces;

      Points : Sample_Audio_Data renames
        Sample_Data.all (This.Sample_Id).Audio;

      Sample_Len : constant U32 :=
        U32'Min (U32 (Sample_Point_Index'Last),
                 U32 (Sample_Data.all (This.Sample_Id).Len));

      Depth : constant FX.Bitcrusher.Bitdepth :=
        FX.Bitcrusher.Param_To_Depth (This.Params (P_Drive));
      Shift : constant Natural := 16 - Natural (Depth);
      Mask : constant U16 := Shift_Left (U16'Last, Shift);

      function To_U16 is new Ada.Unchecked_Conversion (S16, U16);
      function To_S16 is new Ada.Unchecked_Conversion (U16, S16);
      function "and" (A : S16; B : U16) return S16
      is (To_S16 (To_U16 (A) and B));

      P : U32;
   begin
      if This.Do_Init then
         This.Do_Init := False;

         Init (This.Env,
               Do_Hold => True,
               Attack_Speed => S_Half_Second);
         Set_Attack (This.Env, Param_Range'Last / 4);

         This.Phase := 0;
      end if;

      case This.Do_Strike.Event is
         when On =>
            This.Do_Strike.Event := None;

            declare
               Start_Point : constant U32 :=
                 U32 (Points_Per_Sample * U32 (This.Params (P_Start)) /
                          U32 (Param_Range'Last));
            begin
               This.Phase := Start_Point * U32 (C4_Phase_Incr);
            end;

            On (This.Env, This.Do_Strike.Velocity);

         when Off =>
            This.Do_Strike.Event := None;

            Off (This.Env);
         when None => null;
      end case;

      Set_Release (This.Env, This.Params (P_Release));

      declare
         Out_Index : Natural := Buffer'First;
         Sample_Point : S32;
      begin

         if Sample_Len > 0 then
            loop
               exit when Out_Index > Buffer'Last;

               P := Shift_Right (This.Phase, Phase_Frac_Bits);

               exit when P > Sample_Len - 1;

               --  Interpolation between two sample points
               declare
                  V : constant S32 :=
                    S32 (Shift_Right
                         (This.Phase, Phase_Frac_Bits - 15) and 16#7FFF#);

                  A : constant S32 := S32 (Points (Sample_Point_Index (P)));
                  B : constant S32 :=
                    S32 (Points (Sample_Point_Index (P) + 1));
               begin
                  Sample_Point := A + ((B - A) * V) / 2**15;

                  Sample_Point := S32 (S16 (Sample_Point) and Mask);
               end;

               This.Phase := This.Phase + This.Phase_Increment;

               --  Amplitude envelope
               Render (This.Env);
               Sample_Point := (Sample_Point * Low_Pass (This.Env)) / 2**15;

               Buffer (Out_Index) := S16 (Sample_Point);

               Out_Index := Out_Index + 1;
            end loop;
         end if;

         --  Fill remaining point, if any...
         Buffer (Out_Index .. Buffer'Last) := (others => 0);
      end;
   end Render_Crusher;

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Instance;
                     Buffer :    out Tresses.Mono_Buffer)
   is
   begin
      case This.Engine is
         when Overdrive => Render_Overdrive (This, Buffer);
         when Crusher   => Render_Crusher (This, Buffer);
      end case;
   end Render;

   -------------
   -- Note_On --
   -------------

   procedure Note_On (This : in out Sample_Rec_Playback_Instance;
                      Key  :        MIDI.MIDI_Key)
   is
   begin
      This.Phase_Increment := Pitch_Table2 (Key);
      This.On := True;
      This.Phase := 0;
   end Note_On;

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Sample_Rec_Playback_Instance;
                     Buffer :    out Tresses.Mono_Buffer)
   is
      Out_Index : Natural := Buffer'First;
      Sample_Point : S32;
      Sample_Len : constant U32 := U32 (WNM.Sample_Recording.Recorded_Length);
      P : U32;
   begin

      if Sample_Len > 0 then
         loop

            P := Shift_Right (This.Phase, Phase_Frac_Bits);
            This.On := P <= Sample_Len - 1;

            exit when
                not This.On
              or else
                Out_Index > Buffer'Last
              or else
                P > Sample_Len - 1;

            --  Interpolation between two sample points
            declare
               V : constant S32 :=
                 S32 (Shift_Right
                      (This.Phase, Phase_Frac_Bits - 15) and 16#7FFF#);

               A : constant S32 :=
                 S32 (Sample_Recording.Get_Point (Sample_Point_Index (P)));
               B : constant S32 :=
                 S32 (Sample_Recording.Get_Point (Sample_Point_Index (P) + 1));
            begin
               Sample_Point := A + ((B - A) * V) / 2**15;
            end;

            This.Phase := This.Phase + This.Phase_Increment;

            Buffer (Out_Index) := S16 (Sample_Point);

            Out_Index := Out_Index + 1;
         end loop;
      end if;

      --  Fill remaining point, if any...
      Buffer (Out_Index .. Buffer'Last) := (others => 0);
   end Render;

end WNM.Voices.Sampler_Voice;
