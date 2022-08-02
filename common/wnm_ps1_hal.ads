with HAL;

with WNM_PS1_HAL_Params; use WNM_PS1_HAL_Params;

package WNM_PS1_HAL is

   -------------
   -- Buttons --
   -------------

   type Buttons_State is array (Button) of Boolean;

   function State return Buttons_State;
   --  Scan buttons and return current state

   --------------
   -- Encoders --
   --------------

   function Left_Encoder return Integer;
   --  Number of increments since last call

   function Right_Encoder return Integer;
   --  Number of increments since last call

   ----------
   -- LEDs --
   ----------

   procedure Set (L : LED; R, G, B : HAL.UInt8);
   --  Set the internal RGB value for the given LED

   procedure Clear_LEDs;
   --  Set the internal RGB value to zero for all LEDs

   procedure Update_LEDs;
   --  Update the LEDs using the internal RGB values

   ------------
   -- Screen --
   ------------

   type Pix_X is range 0 .. Screen_Width - 1;
   type Pix_Y is range 0 .. Screen_Height - 1;

   procedure Clear_Pixels;
   --  Turn off all pixels in the internal state

   procedure Set_Pixel (X : Pix_X; Y : Pix_Y; On : Boolean := True);
   --  Set the state of a pixel in the internal state

   procedure Update_Screen;
   --  Update the screen using the internal pixels state

   ----------
   -- Time --
   ----------

   type Time is new HAL.UInt64;

   function Milliseconds (Ms : Natural) return Time;

   function Clock return Time;

   procedure Delay_Until (Deadline : Time);

   procedure Delay_Milliseconds (Ms : HAL.UInt64);

end WNM_PS1_HAL;
