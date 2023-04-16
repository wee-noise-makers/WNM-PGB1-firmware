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

with WNM.Speech_Dictionary;

package body WNM.Synth.Speech_Voice is

   --------------
   -- Set_Word --
   --------------

   procedure Set_Word (This : in out Instance; Id : MIDI.MIDI_Data) is
   begin
      This.Selected_Word := Speech.Word (Id);
   end Set_Word;

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Instance) is
   begin
      null;
   end Init;

   --------------------
   -- Set_MIDI_Pitch --
   --------------------

   procedure Set_MIDI_Pitch (This : in out Instance;
                             Key  :        MIDI.MIDI_Key)
   is
   begin
      This.Speech_Pitch := MIDI.Key_To_Frequency (Key);
   end Set_MIDI_Pitch;

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Instance;
                     Buffer :    out Tresses.Mono_Buffer)
   is
      LPC_Out : LPC_Synth.Out_Array (Buffer'Range)
        with Address => Buffer'Address;

      Stretch : LPC_Synth.Time_Stretch_Factor :=
        Speech.MIDI_To_Stretch (MIDI.MIDI_Data (This.Params (P_Time) / 258));

   begin
      case This.Do_Strike.Event is
         when On =>
            This.Do_Strike.Event := None;

            LPC_Synth.Set_Data
              (This.LPC,
               WNM.Speech_Dictionary.Data (This.Selected_Word));

            --  On (This.Env, This.Do_Strike.Velocity);

         when Off =>
            This.Do_Strike.Event := None;

            --  Off (This.Env);
         when None => null;
      end case;

      if LPC_Synth.Has_Data (This.LPC) then

         LPC_Synth.Next_Points
           (This.LPC, LPC_Out,
            Sample_Rate => WNM_Configuration.Audio.Sample_Frequency,
            Pitch => This.Speech_Pitch,
            Time_Stretch => Stretch);

      else
         Buffer := (others => 0);
      end if;

   end Render;

end WNM.Synth.Speech_Voice;
