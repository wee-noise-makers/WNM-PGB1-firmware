with HAL;
with WNM_HAL;

package WNM.Time is

   subtype Time_Microseconds is WNM_HAL.Time_Microseconds;

   function Clock return Time_Microseconds
   renames WNM_HAL.Clock;

   function Milliseconds (Ms : Natural) return Time_Microseconds
   renames WNM_HAL.Milliseconds;

   procedure Delay_Milliseconds (Milliseconds : HAL.UInt64)
   renames WNM_HAL.Delay_Milliseconds;

   procedure Delay_Microseconds (Microseconds : HAL.UInt64)
   renames WNM_HAL.Delay_Microseconds;

   procedure Delay_Until (Wakeup_Time : Time_Microseconds)
   renames WNM_HAL.Delay_Until;

end WNM.Time;
