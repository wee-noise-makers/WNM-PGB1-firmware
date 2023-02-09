with WNM.Screen;

package filter_hp is
   pragma Style_Checks (Off);

   Data : WNM.Screen.Bitmap := (W => 10, H => 10, Length_Byte => 13,
Data => (
 0, 0, 15, 3, 2, 4, 8, 32, 64, 0, 1, 4, 0
));

end filter_hp;
