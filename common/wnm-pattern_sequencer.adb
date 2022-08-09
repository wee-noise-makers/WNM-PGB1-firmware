-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2017 Fabien Chouteau                  --
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

with WNM.Chord_Sequencer;

package body WNM.Pattern_Sequencer is

   Max_Patterns_In_Sequence : constant := 30;

   subtype Sequence_Range is Positive range 1 .. Max_Patterns_In_Sequence;
   type Sequence is array (Sequence_Range) of Patterns;
   type In_Seq_Array is array (Patterns) of Boolean;

   type Pattern_Seq is record
      Sequence_Of_Pattern : Sequence := (others => 1);
      Is_In_Sequence      : In_Seq_Array :=  (1 => True, others => False);
      Playing             : Sequence_Range := 1;
      Last_In             : Sequence_Range := 1;
   end record;

   Seq_Flip : Boolean := False;
   Cue_Next : Boolean := False;
   Sequences : array (Boolean) of Pattern_Seq;

   type Play_Kind is (Stop, Play_Loop);
   Playing_State : Play_Kind := Stop;

   type Recording_Kind is (None, Waiting_First_Pattern, Rec);
   Recording_State : Recording_Kind := None;

   procedure Start (S : in out Pattern_Seq;
                    P : Patterns);
   --  Start a new sequence with the given pattern as a first pattern

   -----------
   -- Start --
   -----------

   procedure Start (S : in out Pattern_Seq;
                    P : Patterns)
   is
   begin
      S.Playing := S.Sequence_Of_Pattern'First;
      S.Last_In := S.Playing;

      S.Is_In_Sequence := (others => False);
      S.Is_In_Sequence (P) := True;

      S.Sequence_Of_Pattern (S.Sequence_Of_Pattern'First) := P;
   end Start;

   ---------------------------
   -- Signal_End_Of_Pattern --
   ---------------------------

   procedure Signal_End_Of_Pattern (S : in out Pattern_Seq) is
   begin
      if S.Playing >= S.Last_In then
         S.Playing := S.Sequence_Of_Pattern'First;
      else
         S.Playing := S.Playing + 1;
      end if;
   end Signal_End_Of_Pattern;

   ----------
   -- Play --
   ----------

   procedure Play (S : in out Pattern_Seq) is
   begin
      S.Playing := S.Sequence_Of_Pattern'First;
   end Play;

   ---------------------
   -- Add_To_Sequence --
   ---------------------

   procedure Add_To_Sequence (S       : in out Pattern_Seq;
                              Pattern : Patterns)
   is
   begin
      if S.Last_In /= S.Sequence_Of_Pattern'Last then
         S.Last_In := S.Last_In + 1;
         S.Sequence_Of_Pattern (S.Last_In) := Pattern;
         S.Is_In_Sequence (Pattern) := True;
      end if;
   end Add_To_Sequence;

   ---------------------
   -- Playing_Pattern --
   ---------------------

   function Playing_Pattern (S : Pattern_Seq) return Patterns is
   begin
      return S.Sequence_Of_Pattern (S.Playing);
   end Playing_Pattern;

   ---------------------
   -- Start_Recording --
   ---------------------

   procedure Start_Recording is
   begin
      case Recording_State is
         when None =>
            Recording_State := Waiting_First_Pattern;
         when Waiting_First_Pattern =>
            null;
         when Rec =>
            null;
      end case;
   end Start_Recording;

   -------------------
   -- End_Recording --
   -------------------

   procedure End_Recording is
   begin
      Recording_State := None;
   end End_Recording;

   ----------------
   -- Play_Pause --
   ----------------

   procedure Play_Pause is
   begin
      if Playing_State = Stop then
         if Cue_Next then

            Cue_Next := False;
            Seq_Flip := not Seq_Flip;
         end if;

         Play (Sequences (Seq_Flip));
         Playing_State := Play_Loop;
      else
         Playing_State := Stop;
      end if;
   end Play_Pause;

   -------------
   -- Playing --
   -------------

   function Playing return Boolean
   is (Playing_State /= Stop);

   --------------
   -- On_Press --
   --------------

   procedure On_Press (Button : Keyboard_Button;
                       Mode : WNM.UI.Main_Modes)
   is
      V : constant Keyboard_Value := To_Value (Button);
   begin
      case Recording_State is
         when None =>
            Single_Play (V);
         when Waiting_First_Pattern =>
            if Playing then
               Start (Sequences (not Seq_Flip), V);
               Cue_Next := True;
            else
               Start (Sequences (Seq_Flip), V);
               Cue_Next := False;
            end if;

            Recording_State := Rec;
         when Rec =>
            if Cue_Next then
               Add_To_Sequence (Sequences (not Seq_Flip), V);
            else
               Add_To_Sequence (Sequences (Seq_Flip), V);
            end if;
      end case;
   end On_Press;

   ----------------
   -- On_Release --
   ----------------

   procedure On_Release (Button : Keyboard_Button;
                         Mode : WNM.UI.Main_Modes)
   is null;

   -----------------
   -- Single_Play --
   -----------------

   procedure Single_Play (P : Patterns) is
   begin

      if Playing then
         Cue_Next := True;
         Start (Sequences (not Seq_Flip), P);
      else
         Start (Sequences (Seq_Flip), P);
         Playing_State := Play_Loop;
      end if;
   end Single_Play;

   ---------------------
   -- Add_To_Sequence --
   ---------------------

   procedure Add_To_Sequence (Pattern : Patterns) is
   begin
      Add_To_Sequence (Sequences (Seq_Flip), Pattern);
   end Add_To_Sequence;

   ---------------------
   -- Playing_Pattern --
   ---------------------

   function Playing_Pattern return Patterns is
   begin
      return Playing_Pattern (Sequences (Seq_Flip));
   end Playing_Pattern;

   ----------------------------
   -- Is_In_Pattern_Sequence --
   ----------------------------

   function Is_In_Pattern_Sequence (Pattern : Patterns) return Boolean
   is (Sequences (Seq_Flip). Is_In_Sequence (Pattern));

   ---------------------------
   -- Signal_End_Of_Pattern --
   ---------------------------

   procedure Signal_End_Of_Pattern is
   begin

      Chord_Sequencer.Signal_End_Of_Pattern;

      if Cue_Next then

         Cue_Next := False;
         Seq_Flip := not Seq_Flip;
         Play (Sequences (Seq_Flip));

      else

         case Playing_State is
         when Stop =>
            raise Program_Error;
         --  when One_Shot =>
         --     Playing_State := Stop;
         when Play_Loop =>
            Signal_End_Of_Pattern (Sequences (Seq_Flip));
         end case;
      end if;

   end Signal_End_Of_Pattern;

   ------------------------
   -- Signal_Mid_Pattern --
   ------------------------

   procedure Signal_Mid_Pattern is
   begin
      Chord_Sequencer.Signal_Mid_Pattern;
   end Signal_Mid_Pattern;

end WNM.Pattern_Sequencer;
