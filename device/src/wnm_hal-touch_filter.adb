-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2024 Fabien Chouteau                    --
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

package body WNM_HAL.Touch_Filter is

   Max_Count : constant := 10_000;
   Measure_Per_Round : constant := 2;

   ----------------------
   -- Trigger_Measures --
   ----------------------

   procedure Trigger_Measures (This : in out Filtered_Touch_Sensor) is
      Success : Boolean;
   begin
      for X in 1 .. Measure_Per_Round loop
         This.PIO.Try_Put (This.SM, Max_Count, Success);
         exit when not Success;
      end loop;
   end Trigger_Measures;

   ----------------------
   -- Process_Measures --
   ----------------------

   procedure Process_Measures (This : in out Filtered_Touch_Sensor) is
      use type HAL.UInt32;
      Data : HAL.UInt32;
      Success : Boolean;
   begin
      while not This.PIO.RX_FIFO_Empty (This.SM) loop

         This.PIO.Try_Get (This.SM, Data, Success);
         exit when not Success;

         This.Values (This.Next_In) := Max_Count - Data;
         This.Next_In := @ + 1;
      end loop;
   end Process_Measures;

   ----------
   -- Read --
   ----------

   function Read (This : in out Filtered_Touch_Sensor) return HAL.UInt32 is
      use type HAL.UInt32;
      Sum : HAL.UInt32 := 0;
   begin
      for Elt of This.Values loop
         Sum := Sum + Elt;
      end loop;

      return Sum / This.Values'Length;
   end Read;

   -------------
   -- Process --
   -------------

   function Process (This   : in out Scale;
                     P1, P2 :        Integer)
                     return Touch_Value
   is
      Big : constant Integer := Integer'Max (P1, P2);
      Diff : constant Integer := P2 - P1;

      Diff_F : constant Float := Float (Diff) / Float (Big);
   begin
      if Diff > This.Max then
         This.Max := Diff;
      end if;

      if Diff < This.Min then
         This.Min := Diff;
      end if;

      if Diff_F > This.Max_F then
         This.Max_F := Diff_F;
      end if;

      if Diff_F < This.Min_F then
         This.Min_F := Diff_F;
      end if;

      declare
         Based : constant Float := Diff_F - This.Min_F;
         Span  : constant Float := This.Max_F - This.Min_F;
      begin
         if Span = 0.0 then
            return 0.0;
         else
            return Touch_Value (Based / Span);
         end if;
      end;
      --  declare
      --     Based : constant Float := Float (Value - This.Min);
      --     Span  : constant Float := Float (This.Max - This.Min);
      --  begin
      --     if Span = 0.0 then
      --        return 0.0;
      --     else
      --        return Touch_Value (Based / Span);
      --     end if;
      --  end;
   end Process;

end WNM_HAL.Touch_Filter;
