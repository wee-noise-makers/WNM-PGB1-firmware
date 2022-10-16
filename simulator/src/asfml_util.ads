with Sf.System.Vector2;

package ASFML_Util is

   function "+" (A, B : Sf.System.Vector2.sfVector2f)
                 return Sf.System.Vector2.sfVector2f
   is ((A.x + B.x, A.y + B.y));

   function "-" (A, B : Sf.System.Vector2.sfVector2f)
                 return Sf.System.Vector2.sfVector2f
   is ((A.x - B.x, A.y - B.y));

end ASFML_Util;
