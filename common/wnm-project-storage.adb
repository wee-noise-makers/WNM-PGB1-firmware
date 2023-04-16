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

--  The WNM storage format is a binary format that encodes projects.
--
--  The file is split in several sections (track settings, chords settings,
--  etc.). Sections are made of lists of setting identifier followed by a
--  value.
--
--  To reduce the output size, the sequences section is a bit different in
--  that only Steps that are different than the Default_Step will be encoded.
--
--  Except for string characters, data is always encoded using variable
--  length encoding (LEB128). This means that it will be possible to add
--  more settings in the future without breaking format.
--
--  For instance let's say in format version 1 (v1) there are only 2 chord
--  settings possible. It is tempting to encode the identifiers in the with
--  1-bit. However if a future version 2 (v2) of the format has, say, 8 chord
--  settings the encoding will have to be larger. This means that the decoder
--  for v2 can only read v2 format or needs to know that the file in v1 and
--  adjust accordingly.
--
--  Using LEB128 encoding, future versions of the format will be able
--  to introduce new setting identifiers without breaking backwards
--  compatibility. v2 decoder will still be able to read v1 identifiers.
--
--
--  Here is a basic layout of project format:
--
--  <Global Section>
--    <Setting ID><Value>
--    ...
--  <End of Section>
--  <Track Section>
--    <Track ID>
--    <Setting ID><Value>
--    ...
--  <End of Section>
--  <Chord Section>
--    <Chord ID>
--    <Setting ID><Value>
--    ...
--  <End of Section>
--  <Pattern Section>
--    <Pattern ID>
--    <Setting ID><Value>
--    ...
--  <End of Section>
--  <Sequence Section>
--    <Seq Change Pattern> <Pattern ID>
--    <Seq Change Track> <TrackID>
--    <Step Section>
--       <Step ID >
--       <Setting ID><Value>
--       ...
--    <End of Section>
--    ...
--  <End of Section>
--  <End of File>

with HAL; use HAL;
with WNM.File_System.LEB128_File_Out; use WNM.File_System.LEB128_File_Out;
with WNM.File_System.LEB128_File_In; use WNM.File_System.LEB128_File_In;
with WNM.Project.Storage.File_Out;
with WNM.Project.Storage.File_In;
with WNM.File_System; use WNM.File_System;

with WNM.Project.Chord_Sequencer;
with WNM.Pattern_Sequencer;

package body WNM.Project.Storage is

   --------------------
   -- Save_Sequences --
   --------------------

   procedure Save_Sequences (Output : in out File_Out.Instance) is
      use MIDI;

      Out_Pattern : Patterns := Patterns'First;
      Out_Track   : Tracks   := Tracks'First;
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
                                 Output.Push (Out_UInt (Step.Trig'Enum_Rep));
                              end if;
                           when Repeat =>

                              if Step.Repeat /= Default_Step.Repeat then
                                 Output.Push (Set);
                                 Output.Push (Out_UInt (Step.Repeat'Enum_Rep));
                              end if;

                           when Repeat_Rate =>
                              if Step.Repeat_Rate /= Default_Step.Repeat_Rate
                              then
                                 Output.Push (Set);
                                 Output.Push
                                   (Out_UInt (Step.Repeat_Rate'Enum_Rep));
                              end if;

                           when Note_Mode =>
                              if Step.Note_Mode /= Default_Step.Note_Mode then
                                 Output.Push (Set);
                                 Output.Push
                                   (Out_UInt (Step.Note_Mode'Enum_Rep));
                              end if;

                           when Note =>
                              if Step.Note /= Default_Step.Note then
                                 Output.Push (Set);
                                 Output.Push (Out_UInt (Step.Note));
                              end if;

                           when Octave_Shift =>
                              if Step.Oct /= Default_Step.Oct then
                                 Output.Push (Set);

                                 declare
                                    Oct : constant Integer :=
                                      Integer (Step.Oct) -
                                      Integer (Octave_Offset'First);
                                 begin
                                    Output.Push
                                      (Out_UInt (UInt8 (Oct)));
                                 end;
                              end if;

                           when Duration =>
                              if Step.Duration /= Default_Step.Duration then
                                 Output.Push (Set);
                                 Output.Push (Out_UInt
                                              (Step.Duration'Enum_Rep));
                              end if;

                           when Velo =>
                              if Step.Velo /= Default_Step.Velo then
                                 Output.Push (Set);
                                 Output.Push (Out_UInt (Step.Velo));
                              end if;

                           when CC_A =>
                              if Step.CC_Ena (A) then
                                 Output.Push (Set);
                                 Output.Push (Out_UInt (Step.CC_Val (A)));
                              end if;

                           when CC_B =>
                              if Step.CC_Ena (B) then
                                 Output.Push (Set);
                                 Output.Push (Out_UInt (Step.CC_Val (B)));
                              end if;

                           when CC_C =>
                              if Step.CC_Ena (C) then
                                 Output.Push (Set);
                                 Output.Push (Out_UInt (Step.CC_Val (C)));
                              end if;

                           when CC_D =>
                              if Step.CC_Ena (D) then
                                 Output.Push (Set);
                                 Output.Push (Out_UInt (Step.CC_Val (D)));
                              end if;
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
                     Output.Push (Out_UInt (Track.MIDI_Enabled'Enum_Rep));

                  when Engine =>
                     Output.Push (Out_UInt (Track.Engine));

                  when Volume =>
                     Output.Push (Out_UInt (Track.Volume));

                  when Pan =>
                     Output.Push (Out_UInt (Track.Pan));

                  when Master_FX =>
                     Output.Push (Out_UInt (Track.FX_Kind'Enum_Rep));

                  when LFO_Rate =>
                     Output.Push (Out_UInt (Track.LFO_Rate));

                  when LFO_Amplitude =>
                     Output.Push (Out_UInt (Track.LFO_Amp));

                  when LFO_Shape =>
                     Output.Push (Out_UInt (Track.LFO_Shape'Enum_Rep));

                  when LFO_Target =>
                     Output.Push (Out_UInt (Track.LFO_Target'Enum_Rep));

                  when LFO_Amp_Mode =>
                     Output.Push (Out_UInt (Track.LFO_Amp_Mode'Enum_Rep));

                  when LFO_Sync =>
                     Output.Push (Out_UInt (Track.LFO_Sync'Enum_Rep));

                  when LFO_Loop =>
                     Output.Push (Out_UInt (Track.LFO_Loop'Enum_Rep));

                  when Arp_Mode =>
                     Output.Push (Out_UInt (Track.Arp_Mode'Enum_Rep));

                  when Arp_Notes =>
                     Output.Push (Out_UInt (Track.Arp_Notes'Enum_Rep));

                  when Notes_Per_Chord =>
                     Output.Push (Out_UInt (Track.Notes_Per_Chord'Enum_Rep));

                  when MIDI_Chan =>
                     Output.Push (Out_UInt (Track.Chan));

                  when CC_Default_A =>
                     Output.Push (Track.CC (A).Value);

                  when CC_Default_B =>
                     Output.Push (Track.CC (B).Value);

                  when CC_Default_C =>
                     Output.Push (Track.CC (C).Value);

                  when CC_Default_D =>
                     Output.Push (Track.CC (D).Value);

                  when CC_Ctrl_A =>
                     Output.Push (Track.CC (A).Controller);

                  when CC_Ctrl_B =>
                     Output.Push (Track.CC (B).Controller);

                  when CC_Ctrl_C =>
                     Output.Push (Track.CC (C).Controller);

                  when CC_Ctrl_D =>
                     Output.Push (Track.CC (D).Controller);

                  when CC_Label_A =>
                     Output.Push (Track.CC (A).Label);

                  when CC_Label_B =>
                     Output.Push (Track.CC (D).Label);

                  when CC_Label_C =>
                     Output.Push (Track.CC (C).Label);

                  when CC_Label_D =>
                     Output.Push (Track.CC (D).Label);

                  when MIDI_Instrument =>
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
            Chord : Chord_Rec renames G_Project.Chords (C_Id);
         begin
            Output.Start_Chord_Settings (C_Id);

            --  Using a loop over all settings and a case statement, we make
            --  sure all settings are handled. This will hopefully prevent
            --  mistakes when new settings are introduced.
            for Set in Chord_Setting_Kind loop

               Output.Push (Out_UInt (Set'Enum_Rep));

               case Set is
                  when Tonic =>
                     Output.Push (Out_UInt (Chord.Tonic));

                  when Name =>
                     Output.Push (Out_UInt (Chord.Name'Enum_Rep));

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

   -------------
   -- Save_FX --
   -------------

   procedure Save_FX (Output : in out File_Out.Instance) is
   begin
      Output.Start_FX;
      --  Using a loop over all settings and a case statement, we make
      --  sure all settings are handled. This will hopefully prevent
      --  mistakes when new settings are introduced.
      for Set in FX_Setting_Kind loop

         Output.Push (Out_UInt (Set'Enum_Rep));

         case Set is
            when Drive_Amount =>
               Output.Push (G_Project.FX.Drive_Amt);

            when Reverb_Amount =>
               Output.Push (G_Project.FX.Reverb_Amount);

            when Reverb_Time =>
               Output.Push (G_Project.FX.Reverb_Time);

            when Reverb_Diffusion =>
               Output.Push (G_Project.FX.Reverb_Diffusion);

            when Reverb_Low_Pass =>
               Output.Push (G_Project.FX.Reverb_Low_Pass);

            when Filter_Mode =>
               Output.Push (Out_UInt (G_Project.FX.Filter_Mode'Enum_Rep));

            when Filter_Cutoff =>
               Output.Push (G_Project.FX.Filter_Cutoff);

            when Filter_Reso =>
               Output.Push (G_Project.FX.Filter_Reso);

         end case;

         if Output.Status /= Ok then
            return;
         end if;

      end loop;
      Output.End_Section;
   end Save_FX;

   -----------------
   -- Save_Global --
   -----------------

   procedure Save_Global (Output : in out File_Out.Instance) is
   begin
      Output.Start_Global;
      Output.Push (Out_UInt (Global_Settings'Enum_Rep (BPM)));
      Output.Push (G_Project.BPM);
      Output.End_Section;
   end Save_Global;

   ----------
   -- Save --
   ----------

   function Save (Filename :     String;
                  Size     : out File_System.File_Signed_Size)
                  return File_System.Storage_Error
   is

      Output : File_Out.Instance := File_Out.Open (Filename);
   begin
      Size := 0;

      if Output.Status = Ok then
         Save_Global (Output);
      end if;

      if Output.Status = Ok then
         Save_Tracks (Output);
      end if;

      if Output.Status = Ok then
         Save_Chords (Output);
      end if;

      if Output.Status = Ok then
         Output.Start_Chord_Chain;
         Chord_Sequencer.Chain.Save (Output);
         Output.End_Section;
      end if;

      if Output.Status = Ok then
         Output.Start_Pattern_Chain;
         Pattern_Sequencer.Save (Output);
         Output.End_Section;
      end if;

      if Output.Status = Ok then
         Save_FX (Output);
      end if;

      if Output.Status = Ok then
         Save_Sequences (Output);
      end if;

      Output.End_File;

      if Output.Status = Ok then
         Size := WNM.File_System.Size;
      end if;

      Output.Close;
      return Output.Status;
   end Save;

   ----------------
   -- Load_Track --
   ----------------

   procedure Load_Track (Input : in out File_In.Instance) is
      procedure To_Track_Settings is new Convert_To_Enum (Track_Settings);

      procedure Read is new File_In.Read_Gen_Enum (Boolean);
      procedure Read is new File_In.Read_Gen_Int (Audio_Volume);
      procedure Read is new File_In.Read_Gen_Int (Audio_Pan);
      procedure Read is new File_In.Read_Gen_Enum (LFO_Shape_Kind);
      procedure Read is new File_In.Read_Gen_Enum (LFO_Target_Kind);
      procedure Read is new File_In.Read_Gen_Enum (LFO_Amp_Kind);
      procedure Read is new File_In.Read_Gen_Enum (LFO_Loop_Kind);
      procedure Read is new File_In.Read_Gen_Enum (LFO_Sync_Kind);
      procedure Read is new File_In.Read_Gen_Enum (Master_FX_Kind);
      procedure Read is new File_In.Read_Gen_Enum (Arp_Mode_Kind);
      procedure Read is new File_In.Read_Gen_Enum (Arp_Notes_Kind);
      procedure Read is new File_In.Read_Gen_Mod (MIDI.MIDI_Channel);
      procedure Read is new File_In.Read_Gen_Mod (MIDI.MIDI_Data);
      procedure Read is new File_In.Read_Gen_Int
        (Chord_Settings.Chord_Index_Range);

      T_Id : Tracks;
      S : Track_Settings;
      Raw : In_UInt := 0;
      Success : Boolean;
   begin
      Input.Read (T_Id);

      if Input.Status /= Ok then
         return;
      end if;

      declare
         Track : Track_Rec renames G_Project.Tracks (T_Id);
      begin

         Track := Default_Track;

         loop
            Input.Read (Raw);

            exit when Input.Status /= Ok
              or else Raw = End_Of_Section_Value;

            To_Track_Settings (Raw, S, Success);

            exit when not Success;

            case S is
               when Track_Mode  => Read (Input, Track.MIDI_Enabled);
               when Engine      => Read (Input, Track.Engine);
               when Volume      => Read (Input, Track.Volume);
               when Pan         => Read (Input, Track.Pan);
               when Master_FX   => Read (Input, Track.FX_Kind);
               when LFO_Rate    => Read (Input, Track.LFO_Rate);
               when LFO_Amplitude => Read (Input, Track.LFO_Amp);
               when LFO_Shape => Read (Input, Track.LFO_Shape);
               when LFO_Target => Read (Input, Track.LFO_Target);
               when LFO_Amp_Mode => Read (Input, Track.LFO_Amp_Mode);
               when LFO_Sync    => Read (Input, Track.LFO_Sync);
               when LFO_Loop    => Read (Input, Track.LFO_Loop);
               when Arp_Mode    => Read (Input, Track.Arp_Mode);
               when Arp_Notes   => Read (Input, Track.Arp_Notes);
               when Notes_Per_Chord => Read (Input, Track.Notes_Per_Chord);
               when MIDI_Chan   => Read (Input, Track.Chan);
               when MIDI_Instrument => null;
               when CC_Default_A => Read (Input, Track.CC (A).Value);
               when CC_Default_B => Read (Input, Track.CC (B).Value);
               when CC_Default_C => Read (Input, Track.CC (C).Value);
               when CC_Default_D => Read (Input, Track.CC (D).Value);
               when CC_Ctrl_A    => Read (Input, Track.CC (A).Controller);
               when CC_Ctrl_B    => Read (Input, Track.CC (B).Controller);
               when CC_Ctrl_C    => Read (Input, Track.CC (C).Controller);
               when CC_Ctrl_D    => Read (Input, Track.CC (D).Controller);
               when CC_Label_A   => File_In.Read (Input, Track.CC (A).Label);
               when CC_Label_B   => File_In.Read (Input, Track.CC (B).Label);
               when CC_Label_C   => File_In.Read (Input, Track.CC (C).Label);
               when CC_Label_D   => File_In.Read (Input, Track.CC (D).Label);
            end case;

            exit when Input.Status /= Ok;
         end loop;
      end;
   end Load_Track;

   ----------------
   -- Load_Chord --
   ----------------

   procedure Load_Chord (Input : in out File_In.Instance) is
      procedure To_Chord_Settings is new Convert_To_Enum (Chord_Setting_Kind);

      procedure Read is new File_In.Read_Gen_Mod (MIDI.MIDI_Key);
      procedure Read is new File_In.Read_Gen_Enum
        (WNM.Chord_Settings.Chord_Name);

      C_Id : Chords;
      S : Chord_Setting_Kind;
      Raw : In_UInt;
      Success : Boolean;
   begin
      Input.Read (C_Id);

      if Input.Status /= Ok then
         return;
      end if;

      declare
         Chord : Chord_Rec renames G_Project.Chords (C_Id);
      begin
         Chord := Default_Chord;

         loop
            Input.Read (Raw);

            exit when Input.Status /= Ok
              or else Raw = End_Of_Section_Value;

            To_Chord_Settings (Raw, S, Success);

            exit when not Success;

            case S is
               when Tonic  => Read (Input, Chord.Tonic);
               when Name      => Read (Input, Chord.Name);
            end case;

            exit when Input.Status /= Ok;
         end loop;
      end;
   end Load_Chord;

   ---------------
   -- Load_Step --
   ---------------

   procedure Load_Step (Input : in out File_In.Instance;
                        P : Patterns; T : Tracks; S : Sequencer_Steps)
   is
      procedure To_Step_Settings is new Convert_To_Enum (Step_Settings);

      procedure Read is new File_In.Read_Gen_Mod (HAL.UInt8);
      procedure Read is new File_In.Read_Gen_Enum (Trigger_Kind);
      procedure Read is new File_In.Read_Gen_Mod (MIDI.MIDI_Data);
      procedure Read is new File_In.Read_Gen_Enum (Note_Duration);
      procedure Read is new File_In.Read_Gen_Mod (Repeat_Cnt);
      procedure Read is new File_In.Read_Gen_Enum (Repeat_Rate_Kind);
      procedure Read is new File_In.Read_Gen_Enum (Note_Mode_Kind);

      Step : Step_Rec renames G_Project.Seqs (P)(T)(S);
      Set : Step_Settings;
      Raw : In_UInt;
      Success : Boolean;

      Val : UInt8;
   begin
      loop
         Input.Read (Raw);

         exit when Input.Status /= Ok
           or else Raw = End_Of_Section_Value;

         To_Step_Settings (Raw, Set, Success);

         exit when not Success;

         case Set is
            when Condition    => Read (Input, Step.Trig);
            when Note         => Read (Input, Step.Note);

            when Octave_Shift =>
               Read (Input, Val);
               declare
                  Oct : constant Integer :=
                    Integer (Val) + Integer (Octave_Offset'First);
               begin
                  Step.Oct := Octave_Offset (Oct);
               end;

            when Duration     => Read (Input, Step.Duration);
            when Velo         => Read (Input, Step.Velo);
            when Repeat       => Read (Input, Step.Repeat);
            when Repeat_Rate  => Read (Input, Step.Repeat_Rate);
            when CC_A         =>
               Step.CC_Ena (A) := True;
               Read (Input, Step.CC_Val (A));
            when CC_B         =>
               Step.CC_Ena (B) := True;
               Read (Input, Step.CC_Val (B));
            when CC_C         =>
               Step.CC_Ena (C) := True;
               Read (Input, Step.CC_Val (C));
            when CC_D         =>
               Step.CC_Ena (D) := True;
               Read (Input, Step.CC_Val (D));

            when Note_Mode => Read (Input, Step.Note_Mode);
         end case;

         exit when Input.Status /= Ok;
      end loop;
   end Load_Step;

   --------------------
   -- Load_Sequences --
   --------------------

   procedure Load_Sequences (Input : in out File_In.Instance) is
      Token : Token_Kind;

      In_Pattern : Patterns        := Patterns'First;
      In_Track   : Tracks          := Tracks'First;
      In_Step    : Sequencer_Steps := Sequencer_Steps'First;
   begin
      --  Init all steps to the default value
      for P in Patterns loop
         for T in Tracks loop
            for S in Sequencer_Steps loop
               G_Project.Seqs (P)(T)(S) := Default_Step;
            end loop;
         end loop;
      end loop;

      loop

         Input.Read (Token);

         if Input.Status /= Ok then
            return;
         end if;

         case Token is
            when Seq_Change_Track =>
               Input.Read (In_Track);

            when Seq_Change_Pattern =>
               Input.Read (In_Pattern);

            when Step_Section =>
               Input.Read (In_Step);

               exit when Input.Status /= Ok;

               Load_Step (Input, In_Pattern, In_Track, In_Step);

            when End_Of_Section =>
               return;

            when others =>
               Input.Set_Format_Error;
               return;
         end case;

         exit when Input.Status /= Ok;
      end loop;
   end Load_Sequences;

   -----------------
   -- Load_Global --
   -----------------

   procedure Load_Global (Input : in out File_In.Instance) is
      procedure To_Global_Settings is new Convert_To_Enum (Global_Settings);

      procedure Read_BPM is new File_In.Read_Gen_Int (Beat_Per_Minute);

      Set : Global_Settings;
      Raw : In_UInt;
      Success : Boolean;
   begin
      loop
         Input.Read (Raw);

         exit when Input.Status /= Ok
           or else Raw = End_Of_Section_Value;

         To_Global_Settings (Raw, Set, Success);

         exit when not Success;

         case Set is
            when BPM   => Read_BPM (Input, G_Project.BPM);
         end case;

         exit when Input.Status /= Ok;
      end loop;
   end Load_Global;

   -------------
   -- Load_FX --
   -------------

   procedure Load_FX (Input : in out File_In.Instance) is
      procedure To_FX_Settings is new Convert_To_Enum (FX_Setting_Kind);

      procedure Read is new File_In.Read_Gen_Enum (Filter_Mode_Kind);
      procedure Read is new File_In.Read_Gen_Mod (MIDI.MIDI_Data);

      S : FX_Setting_Kind;
      Raw : In_UInt := 0;
      Success : Boolean;
   begin
      loop
         Input.Read (Raw);

         exit when Input.Status /= Ok
           or else Raw = End_Of_Section_Value;

         To_FX_Settings (Raw, S, Success);

         exit when not Success;

         case S is
            when Drive_Amount => Read (Input, G_Project.FX.Drive_Amt);
            when Reverb_Amount => Read (Input, G_Project.FX.Reverb_Amount);
            when Reverb_Time => Read (Input, G_Project.FX.Reverb_Time);
            when Reverb_Diffusion => Read (Input, G_Project.FX.Reverb_Diffusion);
            when Reverb_Low_Pass => Read (Input, G_Project.FX.Reverb_Low_pass);
            when Filter_Mode => Read (Input, G_Project.FX.Filter_Mode);
            when Filter_Cutoff => Read (Input, G_Project.FX.Filter_Cutoff);
            when Filter_Reso => Read (Input, G_Project.FX.Filter_Reso);
         end case;

         exit when Input.Status /= Ok;
      end loop;
   end Load_FX;

   ----------
   -- Load --
   ----------

   function Load (Filename :     String;
                  Size     : out File_System.File_Signed_Size)
                  return File_System.Storage_Error
   is
      Input : File_In.Instance := File_In.Open (Filename);

      Token : Token_Kind;
   begin
      Size := 0;

      loop

         Input.Read (Token);

         exit when Input.Status /= Ok;

         case Token is
            when Global_Section =>
               Load_Global (Input);

            when Track_Section =>
               Load_Track (Input);

            when Chord_Section =>
               Load_Chord (Input);

            when FX_Section =>
               Load_FX (Input);

            when Sequence_Section =>
               Load_Sequences (Input);

            when Chord_Chain_Section =>
               Project.Chord_Sequencer.Chain.Load (Input);
               declare
                  End_Sec : Token_Kind;
               begin
                  Input.Read (End_Sec);
                  if End_Sec /= End_Of_Section then
                     Input.Set_Format_Error;
                  end if;
               end;

            when Pattern_Chain_Section =>
               WNM.Pattern_Sequencer.Load (Input);
               declare
                  End_Sec : Token_Kind;
               begin
                  Input.Read (End_Sec);
                  if End_Sec /= End_Of_Section then
                     Input.Set_Format_Error;
                  end if;
               end;

            when End_Of_File =>
               exit;

            when others =>
               Input.Set_Format_Error;
               exit;
         end case;

         exit when Input.Status /= Ok;

      end loop;

      Size := WNM.File_System.Size;

      File_System.Close;

      --  Update all the synth settings after loading a project
      for T in Tracks loop
         Synchronize_Voice_Settings (T);
         end loop;
      Synchronize_All_FX_Settings;

      return Input.Status;
   end Load;

end WNM.Project.Storage;
