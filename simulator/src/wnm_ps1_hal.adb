with Ada.Synchronous_Task_Control;

with Sf;
with Sf.Graphics.Color;

with ASFML_Sim;

package body WNM_PS1_HAL is

   LEDs_Internal : ASFML_Sim.SFML_LED_Strip := ASFML_Sim.SFML_LEDs;

   Buttons_Internal : Buttons_State := (others => False);

   Pixels_Internal : array (Pix_X, Pix_Y) of Boolean :=
     (others => (others => False));

   -----------
   -- State --
   -----------

   function State return Buttons_State is
   begin
      for B in Button loop
         Buttons_Internal (B) :=
           (ASFML_Sim.SFML_Pressed (B)
            or else
            ASFML_Sim.Force_Pressed (B));
      end loop;

      Ada.Synchronous_Task_Control.Set_True (ASFML_Sim.Button_Scan_Signal);

      return Buttons_Internal;
   end State;

   ------------------
   -- Left_Encoder --
   ------------------

   function Left_Encoder return Integer
   is (0);

   -------------------
   -- Right_Encoder --
   -------------------

   function Right_Encoder return Integer
   is (0);

   ---------
   -- Set --
   ---------

   procedure Set (L : LED; R, G, B : HAL.UInt8) is
      use Sf;
   begin
      LEDs_Internal (L) := (a => 255,
                            r => sfUint8 (R),
                            g => sfUint8 (G),
                            b => sfUint8 (B));
   end Set;

   ----------------
   -- Clear_LEDs --
   ----------------

   procedure Clear_LEDs is
   begin
      LEDs_Internal := (others => Sf.Graphics.Color.sfTransparent);
   end Clear_LEDs;

   -----------------
   -- Update_LEDs --
   -----------------

   procedure Update_LEDs is
   begin
      ASFML_Sim.SFML_LEDs := LEDs_Internal;
   end Update_LEDs;

   ------------------
   -- Clear_Pixels --
   ------------------

   procedure Clear_Pixels is
   begin
      Pixels_Internal := (others => (others => False));
   end Clear_Pixels;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (X : Pix_X; Y : Pix_Y; On : Boolean := True) is
   begin
      Pixels_Internal (X, Y) := On;
   end Set_Pixel;

   -------------------
   -- Update_Screen --
   -------------------

   procedure Update_Screen is
      use Sf.Graphics.Color;

      FB : array (0 .. (Screen_Width * Screen_Height) - 1) of sfColor
        with Address => ASFML_Sim.Frame_Buffer'Address;
      Pix_Color : constant sfColor := fromRGB (0, 153, 255);
      BG_Color  : constant sfColor := fromRGB (0, 0, 0);
   begin
      FB := (others => BG_Color);

      for X in Pixels_Internal'Range (1) loop
         for Y in Pixels_Internal'Range (2) loop
            if Pixels_Internal (X, Y) then
               FB (Integer (X) + Integer (Y) * Screen_Width) := Pix_Color;
            end if;
         end loop;
      end loop;
   end Update_Screen;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (Ms : Natural) return Time is (0);

   -----------
   -- Clock --
   -----------

   function Clock return Time is (0);

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Deadline : Time) is null;

   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   procedure Delay_Milliseconds (Ms : HAL.UInt64) is null;

end WNM_PS1_HAL;
