with Ada.Calendar; use Ada.Calendar;

package body WNM.Time is

   Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   function Clock return Time_Microseconds is
      Delt_Sec : constant Duration := Ada.Calendar.Clock - Start_Time;
   begin
      return UInt64 (Delt_Sec * 1_000_000.0);
   end Clock;

   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   procedure Delay_Milliseconds (Milliseconds : UInt64) is
   begin
      delay Duration (Milliseconds) / 1_000;
   end Delay_Milliseconds;

   ------------------------
   -- Delay_Microseconds --
   ------------------------

   procedure Delay_Microseconds (Microseconds : UInt64) is
   begin
      delay Duration (Microseconds) / 1_000;
   end Delay_Microseconds;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Wakeup_Time : Time_Microseconds) is
      Now : constant Time_Microseconds := Clock;
   begin
      if Wakeup_Time > Now then
         Delay_Microseconds (Wakeup_Time - Now);
      end if;
   end Delay_Until;

end WNM.Time;
