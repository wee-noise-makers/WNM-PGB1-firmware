-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2022 Fabien Chouteau                    --
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

with WNM.Audio; use WNM.Audio;
with LPC_Synth;

with WNM.Speech_Dictionary;

package body WNM.Speech is

   LPC_Out : LPC_Synth.Out_Array (Mono_Buffer'Range);
   LPC_Arr : array (Tracks) of LPC_Synth.Instance;
   Pitch : array (Tracks) of Float :=
     (others => MIDI.Key_To_Frequency (MIDI.C4));

   ---------
   -- Img --
   ---------

   function Img (W : Word) return String
   is (WNM.Speech_Dictionary.Image (W).all);

   -----------
   -- Start --
   -----------

   procedure Start (T : Tracks; W : Word; K : MIDI.MIDI_Key) is
   begin
      LPC_Synth.Set_Data (LPC_Arr (T), WNM.Speech_Dictionary.Data (W));
      Pitch (T) := MIDI.Key_To_Frequency (K);
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (T : Tracks) is
   begin
      null;
   end Stop;

   -----------------
   -- Next_Points --
   -----------------

   procedure Next_Points (Buffer : in out Audio.Stereo_Buffer) is
   begin
      for T in Tracks loop
         declare
            LPC : LPC_Synth.Instance renames LPC_Arr (T);
         begin
            if LPC_Synth.Has_Data (LPC) then
               LPC_Synth.Next_Points (LPC, LPC_Out,
                                      Sample_Rate => WNM.Sample_Frequency,
                                      Pitch => Pitch (T));

               for Idx in Buffer'Range loop
                  Buffer (Idx).L :=
                    Buffer (Idx).L + Mono_Point (LPC_Out (Idx));
                  Buffer (Idx).R :=
                    Buffer (Idx).R + Mono_Point (LPC_Out (Idx));
               end loop;
            end if;
         end;
      end loop;
   end Next_Points;

end WNM.Speech;
