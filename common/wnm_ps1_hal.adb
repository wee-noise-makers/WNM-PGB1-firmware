pragma Ada_2012;
package body WNM_PS1_HAL is

   -----------
   -- State --
   -----------

   function State return Buttons_State is
   begin
      pragma Compile_Time_Warning (Standard.True, "State unimplemented");
      return raise Program_Error with "Unimplemented function State";
   end State;

   ------------------
   -- Left_Encoder --
   ------------------

   function Left_Encoder return Integer is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Left_Encoder unimplemented");
      return raise Program_Error with "Unimplemented function Left_Encoder";
   end Left_Encoder;

   -------------------
   -- Right_Encoder --
   -------------------

   function Right_Encoder return Integer is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Right_Encoder unimplemented");
      return raise Program_Error with "Unimplemented function Right_Encoder";
   end Right_Encoder;

   ---------
   -- Set --
   ---------

   procedure Set (L : LED; R, G, B : HAL.UInt8) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Set unimplemented");
      raise Program_Error with "Unimplemented procedure Set";
   end Set;

   ----------------
   -- Clear_LEDs --
   ----------------

   procedure Clear_LEDs is
   begin
      pragma Compile_Time_Warning (Standard.True, "Clear_LEDs unimplemented");
      raise Program_Error with "Unimplemented procedure Clear_LEDs";
   end Clear_LEDs;

   -----------------
   -- Update_LEDs --
   -----------------

   procedure Update_LEDs is
   begin
      pragma Compile_Time_Warning (Standard.True, "Update_LEDs unimplemented");
      raise Program_Error with "Unimplemented procedure Update_LEDs";
   end Update_LEDs;

   ------------------
   -- Clear_Pixels --
   ------------------

   procedure Clear_Pixels is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Clear_Pixels unimplemented");
      raise Program_Error with "Unimplemented procedure Clear_Pixels";
   end Clear_Pixels;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (X : Pix_X; Y : Pix_Y; On : Boolean := True) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Set_Pixel unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Pixel";
   end Set_Pixel;

   -------------------
   -- Update_Screen --
   -------------------

   procedure Update_Screen is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Update_Screen unimplemented");
      raise Program_Error with "Unimplemented procedure Update_Screen";
   end Update_Screen;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (Ms : Natural) return Time is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Milliseconds unimplemented");
      return raise Program_Error with "Unimplemented function Milliseconds";
   end Milliseconds;

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      pragma Compile_Time_Warning (Standard.True, "Clock unimplemented");
      return raise Program_Error with "Unimplemented function Clock";
   end Clock;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Deadline : Time) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Delay_Until unimplemented");
      raise Program_Error with "Unimplemented procedure Delay_Until";
   end Delay_Until;

   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   procedure Delay_Milliseconds (Ms : HAL.UInt64) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Delay_Milliseconds unimplemented");
      raise Program_Error with "Unimplemented procedure Delay_Milliseconds";
   end Delay_Milliseconds;

end WNM_PS1_HAL;
