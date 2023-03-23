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

with WNM.Speech_Dictionary;

package body WNM.Speech is

   LPC_Out : LPC_Synth.Out_Array (Mono_Buffer'Range);
   LPC     : LPC_Synth.Instance;
   Pitch   : Float := Key_To_Frequency (MIDI.C4);
   Selected_Word : Word := Word'First;
   Stretch : LPC_Synth.Time_Stretch_Factor :=
     MIDI_To_Stretch (No_Strech_MIDI_Val);

   ---------
   -- Img --
   ---------

   function Img (W : Word) return String
   is (WNM.Speech_Dictionary.Image (W).all);

   --------------
   -- Set_Word --
   --------------

   procedure Set_Word (W : Word) is
   begin
      Selected_Word := W;
   end Set_Word;

   -----------
   -- Start --
   -----------

   procedure Start (K : MIDI.MIDI_Key) is
   begin
      LPC_Synth.Set_Data (LPC, WNM.Speech_Dictionary.Data (Selected_Word));
      Pitch := Key_To_Frequency (K);
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      null;
   end Stop;

   -----------------
   -- Set_Stretch --
   -----------------

   procedure Set_Stretch (V : MIDI.MIDI_Data) is
   begin
      Stretch := MIDI_To_Stretch (V);
   end Set_Stretch;

   -----------------
   -- Next_Points --
   -----------------

   procedure Next_Points (Buffer  : out WNM_HAL.Mono_Buffer;
                          Success : out Boolean)
   is
   begin
      if LPC_Synth.Has_Data (LPC) then
         LPC_Synth.Next_Points
           (LPC, LPC_Out,
            Sample_Rate => WNM_Configuration.Audio.Sample_Frequency,
            Pitch => Pitch,
            Time_Stretch => Stretch);

         for Idx in Buffer'Range loop
            Buffer (Idx) := LPC_Out (Idx);
         end loop;
         Success := True;
      else
         Success := False;
      end if;
   end Next_Points;

end WNM.Speech;
