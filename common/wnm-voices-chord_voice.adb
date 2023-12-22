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
with Tresses.DSP;
with Tresses.Envelopes.AR; use Tresses.Envelopes.AR;

package body WNM.Voices.Chord_Voice is

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Instance) is
   begin
      for V of This.Voices loop
         V.On := False;

         Analog_Oscillator.Init (V.Osc);
         Analog_Oscillator.Set_Shape (V.Osc, Analog_Oscillator.Square);
         Envelopes.AR.Init (V.Env, Do_Hold => True);

      end loop;
   end Init;

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Instance;
                     Buffer :    out Tresses.Mono_Buffer)
   is
      use Tresses.Analog_Oscillator;

      Mix_Buffer : array (Buffer'Range) of S32;

      Shape_Last : constant Param_Range :=
        Shape_Kind'Enum_Rep (Shape_Kind'Last);
      Step : constant Param_Range :=
        Param_Range'Last / (Shape_Last + 1);

      Shape_Val : constant Param_Range :=
        Param_Range'Min (This.Params (P_Shape) / Step, Shape_Last);
      Shape : constant Shape_Kind := Shape_Kind'Enum_Val (Shape_Val);
   begin

      if This.Do_Init then
         This.Do_Init := False;
         This.Init;
      end if;

      Mix_Buffer := (others => 0);

      for V of This.Voices loop
         Set_Attack (V.Env, This.Params (P_Attack));
         Set_Release (V.Env, This.Params (P_Release));
         Analog_Oscillator.Set_Param (V.Osc, 0, This.Params (P_Color));
         Analog_Oscillator.Set_Shape (V.Osc, Shape);
         Analog_Oscillator.Render (V.Osc, Buffer);

         for Idx in Buffer'Range loop
            Envelopes.AR.Render (V.Env);

            declare
               Input : constant S16 := Buffer (Idx);
               Input_Amp : constant S32 :=
                 (S32 (Input) * Envelopes.AR.Low_Pass (V.Env)) / 2**(15 + 2);

               Output : S32 renames Mix_Buffer (Idx);
            begin

               Output := Output + Input_Amp;
            end;
         end loop;
      end loop;

      for Idx in Buffer'Range loop
         Buffer (Idx) := S16 (DSP.Clip_S16 (Mix_Buffer (Idx)));
      end loop;

   end Render;

   ------------
   -- Key_On --
   ------------

   procedure Key_On (This     : in out Instance;
                      Key      :       MIDI.MIDI_Key;
                      Velocity :       Tresses.Param_Range)
   is
      procedure Start_Voice (Id : Voice_Id) is
         V : Voice renames This.Voices (Id);
      begin
         V.Osc.Set_Pitch (Tresses.MIDI_Pitch (Key));
         Envelopes.AR.On (V.Env, Velocity);
         V.Note := Key;
         V.On := True;
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
            Envelopes.AR.Off (V.Env);
            V.On := False;
         end if;
      end loop;
   end Key_Off;

end WNM.Voices.Chord_Voice;
