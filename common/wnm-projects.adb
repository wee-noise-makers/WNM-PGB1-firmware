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

with WNM.Sequencer.Storage;

with Ada.Text_IO;
with HAL; use HAL;

package body WNM.Projects is

   type Token_Kind is (Pattern_Start,
                       Track_Start,
                       Sequence_Start,

                       Seq_Change_Pattern,
                       Seq_Change_Track,
                       Seq_Step_Settings,

                       End_Section)
     with Size => 4;

   for Token_Kind use (Pattern_Start      => 0,
                       Track_Start        => 1,
                       Sequence_Start     => 2,
                       Seq_Change_Pattern => 3,
                       Seq_Change_Track   => 4,
                       Seq_Step_Settings  => 5,
                       End_Section        => 15);

   type Step_Settings is (Condition,
                          Note,
                          Duration,
                          Velo,
                          Repeat,
                          Repeat_Rate,
                          CC_A,
                          CC_B,
                          CC_C,
                          CC_D,
                          Extended,
                          Reserved)
   with Size => 4;

   for Step_Settings use (Condition   => 0,
                          Note        => 1,
                          Duration    => 2,
                          Velo        => 3,
                          Repeat      => 4,
                          Repeat_Rate => 5,
                          CC_A        => 6,
                          CC_B        => 7,
                          CC_C        => 8,
                          CC_D        => 9,
                          Extended    => 14,
                          Reserved    => 15);


   ----------
   -- Open --
   ----------

   procedure Open (This : in out File_Out; Filename : String) is
   begin
      null;
   end Open;

   -----------
   -- Flush --
   -----------

   procedure Flush (This : in out File_Out) is
   begin
      if This.Count /= 0 then
         Ada.Text_IO.Put_Line ("Flush buffer!!!");
         This.Count := 0;
      end if;
   end Flush;

   -----------
   -- Close --
   -----------

   procedure Close (This : in out File_Out) is
   begin
      This.Flush;
   end Close;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out File_Out; A, B : UInt4) is
      Result : constant UInt8 := Shift_Left (UInt8 (A), 4) or UInt8 (B);
      function To_Char (X : UInt4) return Character
      is (case X is
             when 0 => '0',
             when 1 => '1',
             when 2 => '2',
             when 3 => '3',
             when 4 => '4',
             when 5 => '5',
             when 6 => '6',
             when 7 => '7',
             when 8 => '8',
             when 9 => '9',
             when 10 => 'A',
             when 11 => 'B',
             when 12 => 'C',
             when 13 => 'D',
             when 14 => 'E',
             when 15 => 'F');
   begin
      Ada.Text_IO.Put (To_Char (A) & To_Char (B));
      Ada.Text_IO.New_Line;
   end Push;

   -------------------
   -- Start_Pattern --
   -------------------

   procedure Start_Pattern (This : in out File_Out; P : Patterns) is
   begin
      Ada.Text_IO.Put_Line ("Start Pattern");
      Push (This, Pattern_Start'Enum_Rep, UInt4 (P));
   end Start_Pattern;

   -----------------
   -- Start_Track --
   -----------------

   procedure Start_Track (This : in out File_Out; T : Tracks) is
   begin
      Ada.Text_IO.Put_Line ("Start Track");
      Push (This, Track_Start'Enum_Rep, UInt4 (T));
   end Start_Track;

   --------------------
   -- Start_Sequence --
   --------------------

   procedure Start_Sequence (This : in out File_Out) is
   begin
      Ada.Text_IO.Put_Line ("Start Sequence");
      Push (This, Sequence_Start'Enum_Rep, 0);
   end Start_Sequence;

   ----------------
   -- Start_Step --
   ----------------

   procedure Start_Step (This : in out File_Out; S : Sequencer_Steps) is
   begin
      Ada.Text_IO.Put_Line ("Start Step");
      Push (This, Seq_Step_Settings'Enum_Rep, UInt4 (S));
   end Start_Step;

   --------------------
   -- Change_Pattern --
   --------------------

   procedure Change_Pattern (This : in out File_Out; P : Patterns)
   is
   begin
      Ada.Text_IO.Put_Line ("Change Pattern");
      Push (This, Seq_Change_Pattern'Enum_Rep, UInt4 (P));
   end Change_Pattern;

   ------------------
   -- Change_Track --
   ------------------

   procedure Change_Track (This : in out File_Out; T : Tracks) is
   begin
      Ada.Text_IO.Put_Line ("Change Track");
      Push (This, Seq_Change_Track'Enum_Rep, UInt4 (T));
   end Change_Track;

   -----------------
   -- End_Section --
   -----------------

   procedure End_Section (This : in out File_Out) is
   begin
      Ada.Text_IO.Put_Line ("End Section");
      Push (This, End_Section'Enum_Rep, 0);
   end End_Section;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out File_Out; A : UInt8) is
   begin
      This.Buffer (This.Buffer'First + This.Count) := A;
      This.Count := This.Count + 1;

      if This.Count = This.Buffer_Size then
         This.Flush;
      end if;
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out File_Out; A : Character) is
   begin
      This.Push (UInt8 (A'Enum_Rep));
   end Push;

   ----------
   -- Save --
   ----------

   procedure Save is
      F : File_Out (5);
   begin
      WNM.Sequencer.Storage.Save_Track_Settings (F);
      WNM.Sequencer.Storage.Save_Sequences (F);
   end Save;

   ----------
   -- Load --
   ----------

   procedure Load is
   begin
      raise Program_Error with "Unimplemented procedure Load";
   end Load;

end WNM.Projects;
