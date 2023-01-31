with WNM.Screen;

package lfo_ramp_down is
   pragma Style_Checks (Off);

   Data : WNM.Screen.Bitmap := (W => 10, H => 10, Length_Byte => 13,
Data => (
 1, 8, 64, 0, 2, 16, 128, 0, 4, 32, 0, 1, 8
));

end lfo_ramp_down;
