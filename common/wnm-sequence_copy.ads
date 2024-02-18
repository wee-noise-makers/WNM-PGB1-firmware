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

package WNM.Sequence_Copy is

   type Addr_State is (None, Track, Pattern, Step);
   type Copy_Kind is (Track, Pattern, Step);

   type Copy_Addr is record
      State   : Addr_State := None;
      Kind    : Copy_Kind := Track;
      T       : Tracks := 1;
      P       : Patterns := 1;
      S       : Sequencer_Steps := 1;
   end record;

   function Is_Complete (A : Copy_Addr) return Boolean
   is (case A.Kind is
          when Track   => A.State = Track,
          when Pattern => A.State = Pattern,
          when Step    => A.State = Step);

   function Image (A : Copy_Addr; Q : String := "??") return String
   is (case A.Kind is

          when Track =>
         (case A.State is
             when None  => "T" & Q,
             when Track => "T" & Img (A.T),
             when others => raise Program_Error),

          when Pattern =>
         (case A.State is
             when None    => "T" & Q & "-P__",
             when Track   => "T" & Img (A.T) & "-P" & Q,
             when Pattern => "T" & Img (A.T) & "-P" & Img (A.P),
             when others => raise Program_Error),

          when Step =>
         (case A.State is
             when None    => "T" & Q & "-P__-S__",
             when Track   => "T" & Img (A.T) & "-P" & Q & "-S__",
             when Pattern => "T" & Img (A.T) & "-P" & Img (A.P) & "-S" & Q,
             when Step    => "T" & Img (A.T) & "-P" & Img (A.P) & "-S" &
            Img (A.S)));

   type Copy_Transaction is record
      From, To : Copy_Addr;
   end record;

   function Is_Complete (T : Copy_Transaction) return Boolean
   is (Is_Complete (T.From) and then Is_Complete (T.To));

   procedure Apply (T : in out Copy_Transaction;
                    B :        Button);

   function Start_Copy_Track return Copy_Transaction;

   function Start_Copy_Pattern (Current_Track : Tracks)
                                return Copy_Transaction;

   function Start_Copy_Step (Current_Track   : Tracks;
                             Current_Pattern : Patterns)
                             return Copy_Transaction;
end WNM.Sequence_Copy;
