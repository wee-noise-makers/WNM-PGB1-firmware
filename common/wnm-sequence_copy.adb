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
         when Pattern_Button =>
            A.State := None;
         when Track_Button =>
            if A.Kind in Track | Step then
               A.State := Pattern;
            end if;
         when Step_Button =>
            if A.Kind in Step then
               A.State := Track;
            end if;
         when B1 .. B16 =>
            case A.State is
               when None =>
                  A.P := To_Value (B);
                  A.State := Pattern;
               when Pattern =>
                  A.T := To_Value (B);
                  A.State := Track;
               when Track =>
                  A.S := To_Value (B);
                  A.State := Step;
               when Step =>
                  A.S := To_Value (B);
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

   ------------------------
   -- Start_Copy_Pattern --
   ------------------------

   function Start_Copy_Pattern return Copy_Transaction
   is (From => Copy_Addr'(State => None, Kind => Pattern, others => <>),
       To   => Copy_Addr'(State => None, Kind => Pattern, others => <>));

   ----------------------
   -- Start_Copy_Track --
   ----------------------

   function Start_Copy_Track (Current_Pattern : Keyboard_Value)
                              return Copy_Transaction
   is (From => Copy_Addr'(State => Pattern, Kind => Track,
                          P => Current_Pattern,
                          others => <>),
       To   => Copy_Addr'(State => Pattern, Kind => Track,
                          P => Current_Pattern,
                          others => <>));


   ---------------------
   -- Start_Copy_Step --
   ---------------------

   function Start_Copy_Step (Current_Pattern, Current_Track : Keyboard_Value)
                             return Copy_Transaction
   is (From => Copy_Addr'(State => Track, Kind => Step,
                          P => Current_Pattern,
                          T => Current_Track,
                          others => <>),
       To   => Copy_Addr'(State => Track, Kind => Step,
                          P => Current_Pattern,
                          T => Current_Track,
                          others => <>));


end WNM.Sequence_Copy;
