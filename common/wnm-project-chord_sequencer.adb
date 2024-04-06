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

with MIDI; use MIDI;
with WNM.Step_Event_Broadcast;
with WNM.Song_Start_Broadcast;
with WNM.Project_Load_Broadcast;
with WNM.Project.Song_Part_Sequencer;

package body WNM.Project.Chord_Sequencer is

   procedure Step_Callback;
   package Step_Listener
   is new Step_Event_Broadcast.Register (Step_Callback'Access);
   pragma Unreferenced (Step_Listener);

   procedure Song_Start_Callback;
   package Song_Start_Listener
   is new Song_Start_Broadcast.Register (Song_Start_Callback'Access);
   pragma Unreferenced (Song_Start_Listener);

   procedure Project_Load_Callback;
   package Project_Load_Listener
   is new Project_Load_Broadcast.Register (Project_Load_Callback'Access);
   pragma Unreferenced (Project_Load_Listener);

   procedure Update_Current;
   --  Update the current tonic, name, and chord notes

   --  Init_Scale_Root : constant MIDI.MIDI_Key := MIDI.C4;
   --  Init_Scale      : constant Scale_Name := Minor_Scale;
   --  Init_Chords     : constant Chord_Arr :=
   --    (1 => (Init_Scale_Root + Scales (Init_Scale)(0),
   --           Substitutions (Scale_Chords (Init_Scale)(0)).Sub (1),
   --           Default_Chord.Duration),
   --
   --     2 => (Init_Scale_Root + Scales (Init_Scale)(1),
   --           Substitutions (Scale_Chords (Init_Scale)(1)).Sub (1),
   --           Default_Chord.Duration),
   --
   --     3 => (Init_Scale_Root + Scales (Init_Scale)(2),
   --           Substitutions (Scale_Chords (Init_Scale)(2)).Sub (1),
   --           Default_Chord.Duration),
   --
   --     4 => (Init_Scale_Root + Scales (Init_Scale)(3),
   --           Substitutions (Scale_Chords (Init_Scale)(3)).Sub (1),
   --           Default_Chord.Duration),
   --
   --     5 => (Init_Scale_Root + Scales (Init_Scale)(4),
   --           Substitutions (Scale_Chords (Init_Scale)(4)).Sub (1),
   --           Default_Chord.Duration),
   --
   --     6 => (Init_Scale_Root + Scales (Init_Scale)(5),
   --           Substitutions (Scale_Chords (Init_Scale)(5)).Sub (1),
   --           Default_Chord.Duration),
   --
   --     7 => (Init_Scale_Root + Scales (Init_Scale)(6),
   --           Substitutions (Scale_Chords (Init_Scale)(6)).Sub (1),
   --           Default_Chord.Duration),
   --
   --     8 => (Init_Scale_Root + Scales (Init_Scale)(0),
   --           Substitutions (Scale_Chords (Init_Scale)(0)).Sub (3),
   --           Default_Chord.Duration),
   --
   --     9 => (Init_Scale_Root + Scales (Init_Scale)(0),
   --           Substitutions (Scale_Chords (Init_Scale)(0)).Sub (2),
   --           Default_Chord.Duration),
   --
   --     10 => (Init_Scale_Root + Scales (Init_Scale)(1),
   --            Substitutions (Scale_Chords (Init_Scale)(1)).Sub (2),
   --            Default_Chord.Duration),
   --
   --     11 => (Init_Scale_Root + Scales (Init_Scale)(2),
   --            Substitutions (Scale_Chords (Init_Scale)(2)).Sub (2),
   --            Default_Chord.Duration),
   --
   --     12 => (Init_Scale_Root + Scales (Init_Scale)(3),
   --            Substitutions (Scale_Chords (Init_Scale)(3)).Sub (2),
   --            Default_Chord.Duration),
   --
   --     13 => (Init_Scale_Root + Scales (Init_Scale)(4),
   --            Substitutions (Scale_Chords (Init_Scale)(4)).Sub (2),
   --            Default_Chord.Duration),
   --
   --     14 => (Init_Scale_Root + Scales (Init_Scale)(5),
   --            Substitutions (Scale_Chords (Init_Scale)(5)).Sub (2),
   --            Default_Chord.Duration),
   --
   --     15 => (Init_Scale_Root + Scales (Init_Scale)(6),
   --            Substitutions (Scale_Chords (Init_Scale)(6)).Sub (2),
   --            Default_Chord.Duration),
   --
   --     16 => (Init_Scale_Root + Scales (Init_Scale)(0),
   --            Substitutions (Scale_Chords (Init_Scale)(0)).Sub (4),
   --            Default_Chord.Duration)
   --    );

   -------------------------
   -- Song_Start_Callback --
   -------------------------

   procedure Song_Start_Callback is
      Part : constant Parts := Project.Song_Part_Sequencer.Playing;
      Prog : constant WNM.Chord_Progressions :=
        G_Project.Parts (Part).Progression;
   begin
      G_Play_State.Progression := Prog;
      G_Play_State.Chord_Id := Chord_Slot_Id'First;
      G_Play_State.Chord_Steps_Count := 0;

      Update_Current;
   end Song_Start_Callback;

   -------------------
   -- Step_Callback --
   -------------------

   procedure Step_Callback is
      Part : constant Parts := Project.Song_Part_Sequencer.Playing;
      New_Prog : constant WNM.Chord_Progressions :=
        G_Project.Parts (Part).Progression;
   begin

      --  Ada.Text_IO.Put_Line ("--------------------------------");
      --  Ada.Text_IO.Put_Line ("Step callback");
      if New_Prog /= G_Play_State.Progression then
         --  Ada.Text_IO.Put_Line ("New PROGRESSION");
         --  There's a new progression in town
         G_Play_State.Chord_Steps_Count := 1;
         G_Play_State.Progression := New_Prog;
         G_Play_State.Chord_Id := Chord_Slot_Id'First;
         Update_Current;
      else

         --  The Step we're about to play
         G_Play_State.Chord_Steps_Count := @ + 1;

         declare
            Prog : Chord_Progression_Rec renames
              G_Project.Progressions (G_Play_State.Progression);
            Chord : Chord_Rec renames Prog.Chords (G_Play_State.Chord_Id);
         begin
            --  Ada.Text_IO.Put_Line ("G_Steps_Count:" & G_Steps_Count'Img);
            --  Ada.Text_IO.Put_Line ("Chord_Duration:" & Chord.Duration'Img);
            if G_Play_State.Chord_Steps_Count > Natural (Chord.Duration) then
               --  End of this chord, going to the next one

               if G_Play_State.Chord_Id >= Prog.Len then
                  G_Play_State.Chord_Id := Chord_Slot_Id'First;
               else
                  G_Play_State.Chord_Id := @ + 1;
               end if;

               --  We're about to play the first step of the new chord
               G_Play_State.Chord_Steps_Count := 1;

               Update_Current;
            else
               G_Play_State.Chord_Steps_Count := @ + 1;
            end if;
         end;
      end if;
   end Step_Callback;

   ---------------------------
   -- Project_Load_Callback --
   ---------------------------

   procedure Project_Load_Callback is
   begin
      Update_Current;
   end Project_Load_Callback;

   -------------------
   -- Current_Tonic --
   -------------------

   function Current_Tonic return MIDI.MIDI_Key
   is (G_Play_State.Tonic);

   ------------------------
   -- Current_Chord_Name --
   ------------------------

   function Current_Chord_Name return Chord_Name
   is (G_Play_State.Chord_Name);

   -----------------------------
   -- Current_Chord_Intervals --
   -----------------------------

   function Current_Chord_Intervals return Chord_Intervals is
   begin
      return WNM.Chord_Settings.Chords (Current_Chord_Name);
   end Current_Chord_Intervals;

   -------------------
   -- Current_Chord --
   -------------------

   function Current_Chord return Chord_Notes
   is (G_Play_State.Chord);

   --------------------
   -- Update_Current --
   --------------------

   procedure Update_Current is
   begin
      G_Play_State.Tonic :=
        G_Project.Progressions (G_Play_State.Progression)
        .Chords (G_Play_State.Chord_Id).Tonic;

      G_Play_State.Chord_Name :=
        G_Project.Progressions (G_Play_State.Progression)
        .Chords (G_Play_State.Chord_Id).Name;

      G_Play_State.Chord := G_Play_State.Tonic +
        WNM.Chord_Settings.Chords (G_Play_State.Chord_Name);
   end Update_Current;

end WNM.Project.Chord_Sequencer;
