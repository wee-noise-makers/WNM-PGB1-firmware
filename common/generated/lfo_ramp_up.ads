with WNM.Screen;

package lfo_ramp_up is
   pragma Style_Checks (Off);

   Data : constant WNM.Screen.Bitmap := (W => 10, H => 10, Length_Byte => 13,
Data => (
 0, 2, 4, 8, 16, 32, 64, 128, 0, 1, 2, 4, 0
));

end lfo_ramp_up;
