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

with WNM.Sample_Library; use WNM.Sample_Library;

package body WNM.Sample_Stream is

   ---------------------
   -- To_Stream_Track --
   ---------------------

   function To_Stream_Track (T : Tracks) return Stream_Track
   is (Stream_Track (T));

   --------------
   -- To_Track --
   --------------

   function To_Track (ST : Stream_Track) return Tracks
   is (Tracks (ST));

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

      if Sample /= Invalid_Sample_Entry
        and then
          Sample_Library.Entry_Len (Sample) > 0
      then
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
