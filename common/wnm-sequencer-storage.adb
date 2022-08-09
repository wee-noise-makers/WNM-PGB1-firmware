-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with HAL; use HAL;

with WNM.Projects; use WNM.Projects;

package body WNM.Sequencer.Storage is

   ----------
   -- Save --
   ----------

   procedure Save_Sequences (F : in out Projects.File_Out) is

      Out_Pattern : Patterns := Patterns'Last;
      Out_Track   : Tracks   := Tracks'Last;
 begin
      F.Start_Sequence;
      for P in Patterns loop
         for T in Tracks loop
            for S in Sequencer_Steps loop
               declare
                  Step : Step_Rec renames Sequences (P)(T)(S);
               begin
                  if Step /= Default_Step then

                     if P /= Out_Pattern then
                        F.Change_Pattern (P);
                        Out_Pattern := P;
                     end if;

                     if T /= Out_Track then
                        F.Change_Track (T);
                        Out_Track := T;
                     end if;

                     F.Start_Step (S);
                     if Step.Trig /= Default_Step.Trig then
                        F.Push (Condition'Enum_Rep, Step.Trig'Enum_Rep);
                     end if;

                     if Step.Repeat /= Default_Step.Repeat then
                        F.Push (Repeat'Enum_Rep, Step.Repeat'Enum_Rep);
                     end if;

                     if Step.Repeat_Rate /= Default_Step.Repeat_Rate then
                        F.Push (Repeat_Rate'Enum_Rep, Step.Repeat_Rate'Enum_Rep);
                     end if;

                     if Step.Note_Mode /= Default_Step.Note_Mode then
                        F.Push (Note_Mode'Enum_Rep, Step.Note_Mode'Enum_Rep);
                     end if;

                     if Step.Note /= Default_Step.Note then
                        F.Push (Step_Settings'Enum_Rep (Note), 0);
                        F.Push (Step.Note);
                     end if;

                     if Step.Duration /= Default_Step.Duration then
                        F.Push (Duration'Enum_Rep, Step.Duration'Enum_Rep);
                     end if;

                     if Step.Velo /= Default_Step.Velo then
                        F.Push (Step_Settings'Enum_Rep (Velo), 0);
                        F.Push (Step.Velo);
                     end if;

                     for CC in CC_Id loop
                        if Step.CC_Ena (CC) then
                           F.Push ((case CC is
                                      when A => CC_A'Enum_Rep,
                                      when B => CC_B'Enum_Rep,
                                      when C => CC_C'Enum_Rep,
                                      when D => CC_D'Enum_Rep),
                                   0);
                           F.Push (Step.CC_Val (A));
                        end if;
                     end loop;

                     F.End_Section;
                  end if;
               end;
            end loop;
         end loop;
      end loop;
      F.End_Section;
   end Save_Sequences;

   -------------------------
   -- Save_Track_Settings --
   -------------------------

   procedure Save_Track_Settings (F : in out Projects.File_Out)
   is
   begin
      for T in Tracks loop
         F.Start_Track (T);
         F.Push (MIDI_Chan'Enum_Rep, UInt4 (Track_Settings (T).Chan));
         F.End_Section;
      end loop;
   end Save_Track_Settings;

   ----------
   -- Load --
   ----------

   procedure Load is
   begin
      raise Program_Error with "Unimplemented procedure Load";
   end Load;

end WNM.Sequencer.Storage;
