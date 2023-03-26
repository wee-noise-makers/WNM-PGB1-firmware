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

package body WNM.Gen_Chain_Sequencer is

   subtype Sequence_Range is Positive range 1 .. Max_Patterns_In_Sequence;
   type Sequence is array (Sequence_Range) of Keyboard_Value;
   type In_Seq_Array is array (Keyboard_Value) of Boolean;

   type Pattern_Seq is record
      Sequence_Of_Pattern : Sequence := (others => Keyboard_Value'First);
      Is_In_Sequence      : In_Seq_Array :=  (Keyboard_Value'First => True,
                                              others               => False);
      Playing             : Sequence_Range := Sequence_Range'First;
      Last_In             : Sequence_Range := Sequence_Range'First;
   end record;

   Seq_Flip : Boolean := False;
   Cue_Next : Boolean := False;
   Sequences : array (Boolean) of Pattern_Seq;

   type Play_Kind is (Stop, Play_Loop);
   Playing_State : Play_Kind := Stop;

   type Recording_Kind is (None, Waiting_First_Pattern, Rec);
   Recording_State : Recording_Kind := None;

   procedure Start (S : in out Pattern_Seq;
                    K : Keyboard_Value);
   --  Start a new sequence with the given key as first

   -----------
   -- Start --
   -----------

   procedure Start (S : in out Pattern_Seq;
                    K : Keyboard_Value)
   is
   begin
      S.Playing := S.Sequence_Of_Pattern'First;
      S.Last_In := S.Playing;

      S.Is_In_Sequence := (others => False);
      S.Is_In_Sequence (K) := True;

      S.Sequence_Of_Pattern (S.Sequence_Of_Pattern'First) := K;
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

   procedure Add_To_Sequence (S : in out Pattern_Seq;
                              K : Keyboard_Value)
   is
   begin
      if S.Last_In /= S.Sequence_Of_Pattern'Last then
         S.Last_In := S.Last_In + 1;
         S.Sequence_Of_Pattern (S.Last_In) := K;
         S.Is_In_Sequence (K) := True;
      end if;
   end Add_To_Sequence;

   -------------
   -- Playing --
   -------------

   function Playing (S : Pattern_Seq) return Keyboard_Value is
   begin
      return S.Sequence_Of_Pattern (S.Playing);
   end Playing;

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

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      if Cue_Next then

         Cue_Next := False;
         Seq_Flip := not Seq_Flip;
      end if;

      Play (Sequences (Seq_Flip));
      Playing_State := Play_Loop;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Playing_State := Stop;
   end Stop;

   -------------
   -- Playing --
   -------------

   function Playing return Boolean
   is (Playing_State /= Stop);

   --------------
   -- On_Press --
   --------------

   procedure On_Press (Button : Keyboard_Button)
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

   procedure On_Release (Button : Keyboard_Button)
   is null;

   -----------------
   -- Single_Play --
   -----------------

   procedure Single_Play (K : Keyboard_Value) is
   begin
      if Playing then
         Cue_Next := True;
         Start (Sequences (not Seq_Flip), K);
      else
         Start (Sequences (Seq_Flip), K);
      end if;
   end Single_Play;

   ---------------------
   -- Playing_Pattern --
   ---------------------

   function Playing return Keyboard_Value is
   begin
      return Playing (Sequences (Seq_Flip));
   end Playing;

   --------------------
   -- Is_In_Sequence --
   --------------------

   function Is_In_Sequence (K : Keyboard_Value) return Boolean
   is (Sequences (Seq_Flip). Is_In_Sequence (K));

   ---------------------------
   -- Signal_End_Of_Pattern --
   ---------------------------

   procedure Signal_End_Of_Pattern is
   begin
      if Cue_Next then

         Cue_Next := False;
         Seq_Flip := not Seq_Flip;
         Play (Sequences (Seq_Flip));

      else

         case Playing_State is
         when Stop =>
            null;
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
      null;
   end Signal_Mid_Pattern;

   ----------
   -- Save --
   ----------

   procedure Save
     (Output : in out File_System.LEB128_File_Out.Instance'Class)
   is
      use File_System.LEB128_File_Out;
      use File_System;

      Seq : Pattern_Seq renames Sequences (Seq_Flip);
   begin
      for Idx in Seq.Sequence_Of_Pattern'First .. Seq.Last_In loop
         Output.Push (Out_UInt (Seq.Sequence_Of_Pattern (Idx)));

         if Output.Status /= Ok then
            return;
         end if;

      end loop;

      Output.Push (0);
   end Save;

   ----------
   -- Load --
   ----------

   procedure Load
     (Input : in out File_System.LEB128_File_In.Instance'Class)
   is
      use File_System.LEB128_File_In;
      use File_System;

      Seq : Pattern_Seq renames Sequences (Seq_Flip);
      Raw : In_UInt;
      V : Keyboard_Value;
      Last_In : Natural := 0;
   begin
      Seq.Last_In := Sequence_Range'First;
      Seq.Playing := Sequence_Range'First;
      Seq.Is_In_Sequence := (others => False);

      loop
         Input.Read (Raw);
         if Input.Status /= Ok then
            return;
         end if;

         exit when Raw = 0; -- End of sequence

         if Seq.Last_In = Sequence_Range'Last then
            Input.Set_Format_Error;
            return;
         end if;

         --  Try to convert to Keyboard_Value
         if Raw not in
           In_UInt (Keyboard_Value'First) .. In_UInt (Keyboard_Value'Last)
         then
            Input.Set_Format_Error;
            return;
         end if;

         --  Add to sequence
         V := Keyboard_Value (Raw);
         Last_In := Last_In + 1;
         Seq.Sequence_Of_Pattern (Last_In) := V;
         Seq.Is_In_Sequence (V) := True;
      end loop;

      Seq.Last_In := Last_In;
   end Load;

end WNM.Gen_Chain_Sequencer;
