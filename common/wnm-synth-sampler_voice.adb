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

with Tresses.Envelopes.AR; use Tresses.Envelopes.AR;

with WNM.Sample_Library; use WNM.Sample_Library;

package body WNM.Synth.Sampler_Voice is

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
      Cursor : Sample_Point_Index renames This.Cursor;
      Sample : Single_Sample_Data renames Sample_Data.all (This.Sample_Id);
   begin
      if This.Do_Init then
         This.Do_Init := False;

         Init (This.Env, Do_Hold => True);

      end if;

      case This.Do_Strike.Event is
         when On =>
            This.Do_Strike.Event := None;

            This.Cursor := 1;

            On (This.Env, This.Do_Strike.Velocity);

         when Off =>
            This.Do_Strike.Event := None;

            Off (This.Env);
         when None => null;
      end case;

      declare
         Remaining : constant Sample_Point_Count := Sample'Last - Cursor;
         Len       : constant Sample_Point_Count :=
           Sample_Point_Count'Min (Remaining, Buffer'Length);

         Out_Index : Natural := Buffer'First;
      begin
         if Cursor /= 0 and then Len /= 0 then
            for Index in Cursor .. Cursor + Len - 1 loop
               Buffer (Out_Index) := Sample (Index);
               Out_Index := Out_Index + 1;
            end loop;


            for Index in Out_Index .. Buffer'Last loop
               Buffer (Index) := 0;
            end loop;

            Cursor := Cursor + Len;
         else
            Buffer := (others => 0);
         end if;
      end;
   end Render;

end WNM.Synth.Sampler_Voice;
