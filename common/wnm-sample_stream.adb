-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2017 Fabien Chouteau                    --
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

with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with WNM.Sample_Library; use WNM.Sample_Library;

package body WNM.Sample_Stream is

   procedure Close_Stream (ST : Stream_Track);

   ---------------------
   -- To_Stream_Track --
   ---------------------

   function To_Stream_Track (T : Tracks) return Stream_Track
   is (case T is
          when 1 => ST_1,
          when 2 => ST_2,
          when 3 => ST_3,
          when 4 => ST_4,
          when 5 => ST_5,
          when 6 => ST_6,
          when 7 => ST_7,
          when 8 => ST_8,
          when 9 => ST_9,
          when 10 => ST_10,
          when 11 => ST_11,
          when 12 => ST_12,
          when 13 => ST_13,
          when 14 => ST_14,
          when 15 => ST_15,
          when 16 => ST_16);

   --------------
   -- To_Track --
   --------------

   function To_Track (ST : Stream_Track) return Tracks
   is (case ST is
          when ST_1   => 1,
          when ST_2   => 2,
          when ST_3   => 3,
          when ST_4   => 4,
          when ST_5   => 5,
          when ST_6   => 6,
          when ST_7   => 7,
          when ST_8   => 8,
          when ST_9   => 9,
          when ST_10  => 10,
          when ST_11  => 11,
          when ST_12  => 12,
          when ST_13  => 13,
          when ST_14  => 14,
          when ST_15  => 15,
          when others => 16);

   ------------------
   -- Close_Stream --
   ------------------

   procedure Close_Stream (ST : Stream_Track) is
   begin
      if Streams (St).State /= Ready then

         Streams (St).State := Ready;
      end if;
   end Close_Stream;

   -------------------
   -- Assign_Sample --
   -------------------

   procedure Assign_Sample (Track    : Stream_Track;
                            Filepath : String)
   is
   begin
      null;

      --  Close_Stream (Track);
      --
      --  Open_Read (Streams (Track).FD, Filepath);
      --
      --  Streams (Track).State := Assigned;
   end Assign_Sample;

   -----------
   -- Start --
   -----------

   procedure Start (Track       : Stream_Track;
                    Sample      : Sample_Library.Sample_Index;
                    Start_Point : Sample_Library.Sample_Point_Index;
                    End_Point   : Sample_Library.Sample_Point_Index;
                    Looping     : Boolean)
   is
   begin
      Streams (Track).State := Running;

      Streams (Track).Sample := Sample;
      Streams (Track).Looping := Looping;
      Streams (Track).Start_Point := Start_Point;
      Streams (Track).Cursor := Start_Point;
      Streams (Track).End_Point := End_Point;

      if Sample /= Invalid_Sample_Entry then
         Streams (Track).End_Point := Sample_Library.Entry_Len (Sample);
      end if;

   end Start;

   -----------------
   -- Next_Buffer --
   -----------------

   procedure Next_Buffer (Track   :     Stream_Track;
                          Buffer  : out Audio.Mono_Buffer;
                          Success : out Boolean)
   is
   begin

      if Streams (Track).State /= Running
        and then
          Streams (Track).Sample = Invalid_Sample_Entry
      then
         Success := False;
         return;
      end if;

      declare
         Cursor : Sample_Point_Index renames Streams (Track).Cursor;

         Sample : Single_Sample_Data renames
           Sample_Library.Sample_Data (Streams (Track).Sample);

         Remaining : constant Sample_Point_Count := Sample'Last - Cursor;
         Len       : constant Sample_Point_Count :=
           Sample_Point_Count'Min (Remaining, Buffer'Length);

         Out_Index : Natural := Buffer'First;
      begin
         if Len /= 0 then
            for Index in Cursor .. Cursor + Len - 1 loop
               Buffer (Out_Index) := Sample (Index);
               Out_Index := Out_Index + 1;
            end loop;

            for Index in Out_Index .. Buffer'Last loop
               Buffer (Index) := 0;
            end loop;

            Streams (Track).Cursor := Streams (Track).Cursor + Len;
            Success := True;
         else
            Success := False;
         end if;
      end;


      if not Success
        or else
          Streams (Track).Cursor >= Streams (Track).End_Point
      then
         Streams (Track).State := Ready;
      end if;
   end Next_Buffer;

end WNM.Sample_Stream;
