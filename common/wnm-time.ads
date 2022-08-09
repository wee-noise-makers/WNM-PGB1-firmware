with HAL;      use HAL;
--  with HAL.Time;

package WNM.Time is

   subtype Time_Microseconds is UInt64;

   function Clock return Time_Microseconds;

   procedure Delay_Milliseconds (Milliseconds : UInt64);

   procedure Delay_Microseconds (Microseconds : UInt64);

   procedure Delay_Until (Wakeup_Time : Time_Microseconds);

end WNM.Time;
