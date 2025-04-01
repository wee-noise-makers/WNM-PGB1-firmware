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
--  <Pattern Section>
--    <Track ID>
--    <Pattern ID>
--    <Setting ID><Value>
--    ...
--  <End of Section>
--  <Parts Section>
--    <Part ID>
--    <Setting ID><Value>
--    ...
--  <End of Section>
--  <Chord Progression Section>
--    <Chord Progression ID>
--    <Chord Section>
--      <Setting ID><Value>
--      ...
--    <End of Section>
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

with WNM.Project_Load_Broadcast;
with WNM.File_System.LEB128_File_Out; use WNM.File_System.LEB128_File_Out;
with WNM.File_System.LEB128_File_In; use WNM.File_System.LEB128_File_In;
with WNM.Project.Storage.File_Out;
with WNM.Project.Storage.File_In;
with WNM.File_System; use WNM.File_System;

package body WNM.Project.Storage is

   Format_Version : constant := 1;

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
                     Output.Push (Out_UInt (Track.FX'Enum_Rep));

                  when Track_Octave_Offset =>
                     Output.Push
                       (Out_UInt (Integer (Track.Offset) -
                          Integer (Octave_Offset'First)));

                  when Shuffle =>
                     Output.Push (Out_UInt (Track.Shuffle));

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
   -- Save_Global --
   -----------------

   procedure Save_Global (Output : in out File_Out.Instance) is
   begin
      Output.Start_Global;
      Output.Push (Out_UInt (Global_Settings'Enum_Rep (BPM)));

      --  Only store the integer part of the BPM
      Output.Push (Out_UInt (G_Project.BPM));

      Output.End_Section;
   end Save_Global;

   ----------
   -- Save --
   ----------

   function Save (Filename :     String;
                  Size     : out File_System.File_Signed_Size)
                  return File_System.Storage_Error
   is

      Output : File_Out.Instance;
   begin
      Output.Open (Filename);

      Size := 0;

      --  First, set the format version
      Output.Push (Out_UInt (Format_Version));

      if Output.Status = Ok then
         Save_Global (Output);
      end if;

      if Output.Status = Ok then
         Save_Tracks (Output);
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
      procedure Read is new File_In.Read_Gen_Int (Tracks);
      procedure Read is new File_In.Read_Gen_Int (Audio_Volume);
      procedure Read is new File_In.Read_Gen_Int (Audio_Pan);
      procedure Read is new File_In.Read_Gen_Enum (LFO_Shape_Kind);
      procedure Read is new File_In.Read_Gen_Enum (LFO_Target_Kind);
      procedure Read is new File_In.Read_Gen_Enum (LFO_Amp_Kind);
      procedure Read is new File_In.Read_Gen_Enum (LFO_Loop_Kind);
      procedure Read is new File_In.Read_Gen_Enum (LFO_Sync_Kind);
      procedure Read is new File_In.Read_Gen_Enum (FX_Kind);
      procedure Read is new File_In.Read_Gen_Enum (Arp_Mode_Kind);
      procedure Read is new File_In.Read_Gen_Enum (Arp_Notes_Kind);
      procedure Read is new File_In.Read_Gen_Mod (MIDI.MIDI_Channel);
      procedure Read is new File_In.Read_Gen_Mod (MIDI.MIDI_Data);
      procedure Read is new File_In.Read_Gen_Int
        (WNM.Chord_Settings.Chord_Index_Range);
      procedure Read is new File_In.Read_Gen_Int (Shuffle_Value);

      T_Id : Tracks;
      S : Track_Settings;
      Raw : In_UInt := 0;
      Success : Boolean;
   begin
      Read (Input, T_Id);

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
               when Master_FX   => Read (Input, Track.FX);
               when Track_Octave_Offset =>
                  --  TODO: Implement generic read and write for signed int
                  Input.Read (Raw);
                  declare
                     Tmp : constant Integer :=
                       Integer (Raw) + Integer (Octave_Offset'First);
                  begin
                     if Tmp in Integer (Octave_Offset'First) ..
                       Integer (Octave_Offset'Last)
                     then
                        Track.Offset := Octave_Offset (Tmp);
                     else
                        Input.Set_Format_Error;
                     end if;
                  end;
               when Shuffle     => Read (Input, Track.Shuffle);
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

   -----------------
   -- Load_Global --
   -----------------

   procedure Load_Global (Input : in out File_In.Instance) is
      procedure To_Global_Settings is new Convert_To_Enum (Global_Settings);

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
            when BPM  => Input.Read (Raw);
               if Raw
                      in In_UInt (Beat_Per_Minute'First) ..
                        In_UInt (Beat_Per_Minute'Last)
               then
                  G_Project.BPM := Beat_Per_Minute (Raw);
               end if;
         end case;

         exit when Input.Status /= Ok;
      end loop;
   end Load_Global;

   ----------
   -- Load --
   ----------

   function Load (Filename :     String;
                  Size     : out File_System.File_Signed_Size)
                  return File_System.Storage_Error
   is
      Input : File_In.Instance;
      Token : Token_Kind;

      Version : In_UInt;
   begin
      Input.Open (Filename);
      Size := 0;

      --  The first data in a project file is a version number for the format.
      --  Right now it should always be 1 and we don't use this information.
      Input.Read (Version);

      loop

         Input.Read (Token);

         exit when Input.Status /= Ok;

         case Token is
            when Global_Section =>
               Load_Global (Input);

            when Track_Section =>
               Load_Track (Input);

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

      --  Signal project load
      WNM.Project_Load_Broadcast.Broadcast;

      return Input.Status;
   end Load;

end WNM.Project.Storage;
