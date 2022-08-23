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
with LPC_Synth.Vocab_US_TI99; use LPC_Synth.Vocab_US_TI99;

package body WNM.Speech is

   LPC_Out_Arr : array (Tracks) of LPC_Synth.Out_Array (Mono_Buffer'Range);
   LPC_Out_Idx_Arr : array (Tracks) of  Natural :=
     (others => Mono_Buffer'Last + 1);
   LPC_Arr : array (Tracks) of LPC_Synth.Instance;

   function Data (W : Word) return not null LPC_Synth.LPC_Data_Const_Acc
   is (case W is
          when Word (MIDI.C4) => spt_WE'Access,
          when Word (MIDI.Cs4) => spt_MAKE'Access,
          when others => spt_OTHER'Access);

   ---------
   -- Img --
   ---------

   function Img (W : Word) return String
   is (case W is
          when Word (MIDI.C4) => "we",
          when Word (MIDI.Cs4) => "make",
          when others => "other");

   -----------
   -- Start --
   -----------

   procedure Start (T : Tracks; W : Word) is
   begin
      LPC_Synth.Set_Data (LPC_Arr (T), Data (W));
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (T : Tracks) is
   begin
      LPC_Synth.Set_Data (LPC_Arr (T), null);
   end Stop;

   -----------------
   -- Next_Points --
   -----------------

   procedure Next_Points (Buffer : in out Audio.Stereo_Buffer) is
   begin
      for T in Tracks loop
         declare
            LPC_Out_Idx : Natural renames LPC_Out_Idx_Arr (T);
            LPC_Out : LPC_Synth.Out_Array renames LPC_Out_Arr (T);
            LPC : LPC_Synth.Instance renames LPC_Arr (T);

            type Repeat is mod 5;
            R : Repeat := 0;
         begin
            for Idx in Buffer'Range loop
               if LPC_Out_Idx in LPC_Out'Range then

                  Buffer (Idx).L :=
                    Buffer (Idx).L + Mono_Point (LPC_Out (LPC_Out_Idx));

                  Buffer (Idx).R :=
                    Buffer (Idx).R + Mono_Point (LPC_Out (LPC_Out_Idx));

                  R := R + 1;
                  if R = 0 then
                     LPC_Out_Idx := LPC_Out_Idx + 1;
                  end if;
               end if;

               if LPC_Out_Idx not in LPC_Out'Range
                 and then
                   LPC_Synth.Has_Data (LPC)
               then
                  LPC_Synth.Next_Points (LPC, LPC_Out);
                  LPC_Out_Idx := LPC_Out'First;
               end if;

            end loop;
         end;
      end loop;

      --
      --  Ada.Text_IO.Put_Line (" -> LPC_Out_Idx:" & LPC_Out_Idx'Img);
      --  if LPC_Out_Idx in LPC_Out'Range then
      --     declare
      --        Idx : Natural := Buffer'First;
      --     begin
      --        while LPC_Out_Idx in LPC_Out'Range loop
      --           for Cnt in 1 .. 100 loop
      --              exit when Idx > Buffer'Last;
      --
      --              Buffer (Idx).L :=
      --                Buffer (Idx).L + Mono_Point (LPC_Out (LPC_Out_Idx));
      --
      --              Buffer (Idx).R :=
      --                Output (Idx).R + Mono_Point (LPC_Out (LPC_Out_Idx));
      --
      --              Idx := Idx + 1;
      --           end loop;
      --
      --           LPC_Out_Idx := LPC_Out_Idx + 1;
      --        end loop;
      --     end;
      --  end if;
      --  Ada.Text_IO.Put_Line (" <- LPC_Out_Idx:" & LPC_Out_Idx'Img);
   end Next_Points;

end WNM.Speech;
