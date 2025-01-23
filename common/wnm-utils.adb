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

with HAL;

package body WNM.Utils is

   --------------
   -- Copy_Str --
   --------------

   procedure Copy_Str (From :     String;
                       To   : out String;
                       Fill :     Character := ' ')
   is
   begin
      if From'Length = To'Length then
         To := From;
      elsif From'Length > To'Length then
         To := From (From'First .. From'First + To'Length - 1);
      else
         To (To'First .. To'First + From'Length - 1) := From;
         To (To'First + From'Length .. To'Last) := (others => Fill);
      end if;
   end Copy_Str;

   ----------
   -- Trim --
   ----------

   function Trim (Str : String) return String is
      First : Natural := Str'First;
      Last : Natural := Str'Last;
   begin
      while First in Str'Range and then Str (First) = ' ' loop
         First := First + 1;
      end loop;

      while Last in Str'Range and then Str (Last) = ' ' loop
         Last := Last - 1;
      end loop;

      return Str (First .. Last);
   end Trim;

   -----------
   -- Start --
   -----------

   procedure Start (This : in out Perf_Timer) is
   begin
      This.Done := False;
      This.Start_Time := WNM_HAL.Clock;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Perf_Timer) is
      use HAL;

      End_Time : constant WNM_HAL.Time_Microseconds := WNM_HAL.Clock;

      Elapsed : constant WNM_HAL.Time_Microseconds :=
        End_Time - This.Start_Time;

      Elapsed_Sec : constant Float := Float (Elapsed) / 1_000_000.0;
   begin
      This.Elapsed := Elapsed;
      This.Load :=
        CPU_Load ((Elapsed_Sec / This.Full_Load_Time) * 100.0);

      if This.Load > This.Max_Load then
         This.Max_Load := This.Load;
      end if;

      This.Done := True;
   end Stop;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Perf_Timer;
                    Full_Load_Time : Float := Buffer_Duration_Sec)
   is
   begin
      This.Max_Load := 0.0;
      This.Full_Load_Time := Full_Load_Time;
   end Reset;

   --------------
   -- Duration --
   --------------

   function Duration (This : Perf_Timer) return WNM_HAL.Time_Microseconds is
   begin
      if not This.Done then
         return 0;
      else
         return This.Elapsed;
      end if;
   end Duration;

   ----------
   -- Load --
   ----------

   function Load (This : Perf_Timer) return CPU_Load
   is (This.Load);

   --------------
   -- Max_Load --
   --------------

   function Max_Load (This : Perf_Timer) return CPU_Load
   is (This.Max_Load);

end WNM.Utils;
