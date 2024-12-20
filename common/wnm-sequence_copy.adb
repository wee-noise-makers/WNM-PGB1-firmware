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

package body WNM.Sequence_Copy is

   -----------
   -- Apply --
   -----------

   procedure Apply (A : in out Copy_Addr; B : Button) is
   begin
      case B is
         when Track_Button =>
            A.State := None;
         when Pattern_Button =>
            if A.Kind in Pattern | Step then
               A.State := Track;
            end if;
         when Step_Button =>
            if A.Kind in Step then
               A.State := Pattern;
            end if;
         when Song_Button =>
            A.State := None;
         when B1 .. B16 =>
            case A.State is
               when None =>
                  if A.Kind = Song_Elt then
                     A.E := To_Value (B);
                     A.State := Song_Elt;
                  else
                     A.T := To_Value (B);
                     A.State := Track;
                  end if;
               when Track =>
                  A.P := To_Value (B);
                  A.State := Pattern;
               when Pattern =>
                  A.S := To_Value (B);
                  A.State := Step;
               when Step =>
                  A.S := To_Value (B);
               when Song_Elt =>
                  null;
            end case;
         when others =>
            null;
      end case;
   end Apply;

   -----------
   -- Apply --
   -----------

   procedure Apply (T : in out Copy_Transaction; B : Button) is
   begin
      if not Is_Complete (T.From) then
         Apply (T.From, B);
      else
         Apply (T.To, B);
      end if;
   end Apply;

   ----------------------
   -- Start_Copy_Track --
   ----------------------

   function Start_Copy_Track return Copy_Transaction
   is (From => Copy_Addr'(State => None, Kind => Track, others => <>),
       To   => Copy_Addr'(State => None, Kind => Track, others => <>));

   ------------------------
   -- Start_Copy_Pattern --
   ------------------------

   function Start_Copy_Pattern (Current_Track : Tracks)
                                return Copy_Transaction
   is (From => Copy_Addr'(State => Track, Kind => Pattern,
                          T => Current_Track,
                          others => <>),
       To   => Copy_Addr'(State => Track, Kind => Pattern,
                          T => Current_Track,
                          others => <>));

   ---------------------
   -- Start_Copy_Step --
   ---------------------

   function Start_Copy_Step (Current_Track   : Tracks;
                             Current_Pattern : Patterns)
                             return Copy_Transaction
   is (From => Copy_Addr'(State => Pattern, Kind => Step,
                          P => Current_Pattern,
                          T => Current_Track,
                          others => <>),
       To   => Copy_Addr'(State => Pattern, Kind => Step,
                          P => Current_Pattern,
                          T => Current_Track,
                          others => <>));

   ---------------------
   -- Start_Copy_Song --
   ---------------------

   function Start_Copy_Song return Copy_Transaction
   is (From => Copy_Addr'(State => None, Kind => Song_Elt, others => <>),
       To   => Copy_Addr'(State => None, Kind => Song_Elt, others => <>));

end WNM.Sequence_Copy;
