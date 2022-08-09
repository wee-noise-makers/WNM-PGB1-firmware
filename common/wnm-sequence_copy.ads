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

   type Addr_State is (None, Pattern, Track, Step);
   type Copy_Kind is (Pattern, Track, Step);

   type Copy_Addr is record
      State   : Addr_State;
      Kind    : Copy_Kind;
      P, T, S : Keyboard_Value := 1;
   end record;

   function Is_Complete (A : Copy_Addr) return Boolean
   is (case A.Kind is
          when Pattern => A.State = Pattern,
          when Track   => A.State = Track,
          when Step    => A.State = Step);

   function Image (A : Copy_Addr; Q : String := "??") return String
   is (case A.Kind is

          when Pattern =>
         (if A.State = Pattern then "P" & Img (A.P) else "P" & Q),

          when Track =>
         (case A.State is
             when None => "P" & Q & "-T__",
             when Pattern => "P" & Img (A.P) & "-T" & Q,
             when Track => "P" & Img (A.P) & "-T" & Img (A.T),
             when others => raise Program_Error),

          when Step =>
         (case A.State is
             when None => "P" & Q & "-T__-S__",
             when Pattern => "P" & Img (A.P) & "-T" & Q & "-S__",
             when Track => "P" & Img (A.P) & "-T" & Img (A.T) & "-S" & Q,
             when Step => "P" & Img (A.P) & "-T" & Img (A.T) & "-S" & Img (A.S)));


   type Copy_Transaction is record
      From, To : Copy_Addr;
   end record;

   function Is_Complete (T : Copy_Transaction) return Boolean
   is (Is_Complete (T.From) and then Is_Complete (T.To));

   procedure Apply (T : in out Copy_Transaction;
                    B :        Button);

   function Start_Copy_Pattern return Copy_Transaction;

   function Start_Copy_Track (Current_Pattern : Keyboard_Value)
                              return Copy_Transaction;

   function Start_Copy_Step (Current_Pattern, Current_Track : Keyboard_Value)
                             return Copy_Transaction;
end WNM.Sequence_Copy;
