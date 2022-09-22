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

with HAL; use HAL;
with WNM.Project.Storage.File_Out;

package body WNM.Project.Storage is

   --------------------
   -- Save_Sequences --
   --------------------

   procedure Save_Sequences (Output : in out File_Out.Instance) is
      Out_Pattern : Patterns := Patterns'Last;
      Out_Track   : Tracks   := Tracks'Last;
   begin
      Output.Start_Sequence;

      for P in Patterns loop
         for T in Tracks loop
            for S in Sequencer_Steps loop
               declare
                  Step : Step_Rec renames G_Project.Seqs (P)(T)(S);
               begin
                  if Step /= Default_Step then

                     if P /= Out_Pattern then
                        Output.Change_Pattern_In_Seq (P);
                        Out_Pattern := P;
                     end if;

                     if T /= Out_Track then
                        Output.Change_Track_In_Seq (T);
                        Out_Track := T;
                     end if;

                     Output.Start_Step_Settings (S);

                     --  Using a loop over all settings and a case statement,
                     --  we make sure all settings are handled. This will
                     --  hopefully prevent mistakes when new settings are
                     --  introduced.
                     for Set in Step_Settings loop

                        --  To save space in the output file, we only output
                        --  step setting values if they differ from the
                        --  default ones.

                        case Set is
                           when Condition =>
                              if Step.Trig /= Default_Step.Trig then
                                 Output.Push (Set);
                                 Output.Push (UInt8 (Step.Trig'Enum_Rep));
                              end if;
                           when Repeat =>

                              if Step.Repeat /= Default_Step.Repeat then
                                 Output.Push (Set);
                                 Output.Push (UInt8 (Step.Repeat'Enum_Rep));
                              end if;

                           when Repeat_Rate =>
                              if Step.Repeat_Rate /= Default_Step.Repeat_Rate
                              then
                                 Output.Push (Set);
                                 Output.Push
                                   (UInt8 (Step.Repeat_Rate'Enum_Rep));
                              end if;

                           when Note_Mode =>
                              if Step.Note_Mode /= Default_Step.Note_Mode then
                                 Output.Push (Set);
                                 Output.Push (UInt8 (Step.Note_Mode'Enum_Rep));
                              end if;

                           when Note =>
                              if Step.Note /= Default_Step.Note then
                                 Output.Push (Set);
                                 Output.Push (UInt8 (Step.Note));
                              end if;

                           when Duration =>
                              if Step.Duration /= Default_Step.Duration then
                                 Output.Push (Set);
                                 Output.Push (UInt8 (Step.Duration'Enum_Rep));
                              end if;

                           when Velo =>
                              if Step.Velo /= Default_Step.Velo then
                                 Output.Push (Set);
                                 Output.Push (UInt8 (Step.Velo));
                              end if;

                           when CC_A =>
                              if Step.CC_Ena (A) then
                                 Output.Push (Set);
                                 Output.Push (Step.CC_Val (A));
                              end if;

                           when CC_B =>
                              if Step.CC_Ena (B) then
                                 Output.Push (Set);
                                 Output.Push (Step.CC_Val (B));
                              end if;

                           when CC_C =>
                              if Step.CC_Ena (C) then
                                 Output.Push (Set);
                                 Output.Push (Step.CC_Val (C));
                              end if;

                           when CC_D =>
                              if Step.CC_Ena (D) then
                                 Output.Push (Set);
                                 Output.Push (Step.CC_Val (D));
                              end if;

                           when Extended | Reserved =>
                              null; -- We don't save this
                        end case;
                     end loop;
                     Output.End_Section;
                  end if;
               end;

               if Output.Status /= Ok then
                  return;
               end if;
            end loop;
         end loop;
      end loop;
      Output.End_Section;
   end Save_Sequences;

   -----------------
   -- Save_Tracks --
   -----------------

   procedure Save_Tracks (Output : in out File_Out.Instance) is
   begin
      for T_Id in Tracks loop
         declare
            Track : Track_Rec renames G_Project.Tracks (T_Id);
         begin
            Output.Start_Track_Settings (T_Id);

            --  Using a loop over all settings and a case statement, we make
            --  sure all settings are handled. This will hopefully prevent
            --  mistakes when new settings are introduced.
            for Set in Track_Settings loop
               Output.Push (Set);

               case Set is
                  when Track_Mode =>
                     Output.Push (UInt8 (Track.Mode'Enum_Rep));

                  when Sample =>
                     Output.Push (UInt8 (Track.Sample'Enum_Rep));

                  when Speech_Word =>
                     Output.Push (UInt8 (Track.Word'Enum_Rep));

                  when Volume =>
                     Output.Push (UInt8 (Track.Volume));

                  when Pan =>
                     Output.Push (UInt8 (Track.Pan));

                  when Arp_Mode =>
                     Output.Push (UInt8 (Track.Arp_Mode'Enum_Rep));

                  when Arp_Notes =>
                     Output.Push (UInt8 (Track.Arp_Notes'Enum_Rep));

                  when MIDI_Chan =>
                     Output.Push (UInt8 (Track.Chan));

                  when CC_A =>
                     Output.Push (UInt8 (Track.CC (A).Controller));

                  when CC_B =>
                     Output.Push (UInt8 (Track.CC (B).Controller));

                  when CC_C =>
                     Output.Push (UInt8 (Track.CC (C).Controller));

                  when CC_D =>
                     Output.Push (UInt8 (Track.CC (D).Controller));

                  when CC_Label_A =>
                     Output.Push (Track.CC (A).Label);

                  when CC_Label_B =>
                     Output.Push (Track.CC (D).Label);

                  when CC_Label_C =>
                     Output.Push (Track.CC (C).Label);

                  when CC_Label_D =>
                     Output.Push (Track.CC (D).Label);

                  when MIDI_Instrument | Extended | Reserved =>
                     null; -- We don't save this
               end case;

               if Output.Status /= Ok then
                  return;
               end if;
            end loop;

            Output.End_Section;
         end;

         if Output.Status /= Ok then
            return;
         end if;
      end loop;

   end Save_Tracks;

   -----------------
   -- Save_Chords --
   -----------------

   procedure Save_Chords (Output : in out File_Out.Instance)
   is
   begin
      for C_Id in Chords loop
         declare
            Chord : Chord_Setting renames G_Project.Chords (C_Id);
         begin
            Output.Start_Chord_Settings (C_Id);

            --  Using a loop over all settings and a case statement, we make
            --  sure all settings are handled. This will hopefully prevent
            --  mistakes when new settings are introduced.
            for Set in Chord_Setting_Kind loop

               Output.Push (UInt8 (Set'Enum_Rep));

               case Set is
                  when Tonic =>
                     Output.Push (UInt8 (Chord.Tonic));

                  when Name =>
                     Output.Push (UInt8 (Chord.Name'Enum_Rep));

                  when Extended | Reserved =>
                     null; -- We don't save this
               end case;

               if Output.Status /= Ok then
                  return;
               end if;

            end loop;

            Output.End_Section;
            if Output.Status /= Ok then
               return;
            end if;
         end;
      end loop;

   end Save_Chords;

   ----------
   -- Save --
   ----------

   function Save (Name : Project_Name) return Storage_Error is

      Output : File_Out.Instance := File_Out.Open (Name & ".wnm_prj");
   begin
      Output.Push (G_Project.BPM);

      if Output.Status = Ok then
         Save_Tracks (Output);
      end if;

      if Output.Status = Ok then
         Save_Chords (Output);
      end if;

      if Output.Status = Ok then
         Save_Sequences (Output);
      end if;

      Output.Close;
      return Output.Status;
   end Save;

   ----------
   -- Load --
   ----------

   function Load (Name : Project_Name) return Storage_Error is
      pragma Unreferenced (Name);
   begin
      return Ok;
   end Load;

end WNM.Project.Storage;
