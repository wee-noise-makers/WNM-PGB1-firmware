
with WNM.Screen;
with WNM.GUI.Bitmap_Fonts; use WNM.GUI.Bitmap_Fonts;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;

package body WNM.GUI.Parameters is

   function Y_Offset (Slot : Parameter_Slot) return Integer;

   --------------
   -- Y_Offset --
   --------------

   function Y_Offset (Slot : Parameter_Slot) return Integer
   is (if Slot = Up then Box_Top else Box_Top + 8);

   ---------------
   -- Print_Int --
   ---------------

   procedure Print_Int
     (Slot  : Parameter_Slot;
      Name  : String;
      Value : Integer;
      Min   : Integer;
      Max   : Integer)
   is
      B     : Integer := 5;
      Limit : Integer;
   begin
      Limit := Integer ((Float (Value - Min) / Float (Max - Min)) *
                          Float (Screen.Width));

      Print (X_Offset    => B,
             Y_Offset    => Y_Offset (Slot),
             Str         => String'(Name & ":" & Value'Img),
             Invert_From => 0,
             Invert_To   => Limit);
   end Print_Int;

   ----------------------
   -- Print_Percentage --
   ----------------------

   procedure Print_Percentage
     (Slot  : Parameter_Slot;
      Name  : String;
      Value : Natural)
   is
   begin
      Print_Int (Slot  => Slot,
                 Name  => Name,
                 Value => Value,
                 Min   => 0,
                 Max   => 100);
   end Print_Percentage;

   ---------------
   -- Print_Pan --
   ---------------

   procedure Print_Pan (Slot  : Parameter_Slot;
                        Name  : String;
                        Value : Pan)
   is
      B    : Integer := 5;
      From : Integer;
      To   : Integer;
      Cnt  : Integer;
      Mid  : constant Integer := Screen.Width / 2;
   begin
      Cnt := Integer ((Float (abs Value) / 100.0) * Float (Mid));

      if Value = 0 then
         From := Mid - 1;
         To   := Mid + 1;
      elsif Value > 0 then
         From := Mid;
         To   := Mid + Cnt;
      else
         From := Mid - Cnt;
         To   := Mid;
      end if;

      Print (X_Offset    => B,
             Y_Offset    => Y_Offset (Slot),
             Str         => String'(Name),
             Invert_From => From,
             Invert_To   => To);
   end Print_Pan;

end WNM.GUI.Parameters;
