with System;

with HAL;

with Littlefs;

with WNM_Configuration; use WNM_Configuration;

package WNM_HAL is

   -------------
   -- Buttons --
   -------------

   type Button_State is (Up, Down);
   type Buttons_State is array (Button) of Button_State;

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

   -----------
   -- Audio --
   -----------

   type Audio_Input_Kind is (None, Line_In);
   procedure Select_Audio_Input (Kind : Audio_Input_Kind);

   type Audio_Volume is range 0 .. 100;
   procedure Set_Audio_Volume (Volume : Audio_Volume);

   ----------
   -- Time --
   ----------

   subtype Time_Microseconds is HAL.UInt64;

   function Milliseconds (Ms : Natural) return Time_Microseconds;

   function Clock return Time_Microseconds;

   procedure Delay_Until (Deadline : Time_Microseconds);

   procedure Delay_Milliseconds (Ms : HAL.UInt64);

   procedure Delay_Microseconds (Us : HAL.UInt64);

   -------------
   -- Storage --
   -------------

   function Get_LFS_Config return not null access Littlefs.LFS_Config;

   function Sample_Data_Base return System.Address;

   ---------------------
   -- Coproc Messages --
   ---------------------

   type Coproc_Data is mod 2**Coproc_Data_Size
     with size => Coproc_Data_Size;

   procedure Push (D : Coproc_Data);
   --  Send data to the synth coprocessor. Fails silently if the data cannot
   --  be pushed (e.g. queue is full).

   procedure Pop (D : out Coproc_Data; Success : out Boolean);
   --  Tentatively get data for the synth coprocessor. Success is False if
   --  no data is available.

end WNM_HAL;
