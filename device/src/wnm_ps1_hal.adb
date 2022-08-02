with RP.GPIO;
with RP.Device;
with RP.Clock;
with RP.DMA;
with RP.PIO; use RP.PIO;

with RP.PIO.WS2812;

package body WNM_PS1_HAL is

   XOSC_Frequency : constant RP.Clock.XOSC_Hertz := 12_000_000;

   -------------
   -- Buttons --
   -------------

   B_Row_1 : aliased RP.GPIO.GPIO_Point := (Pin => 26);
   B_Row_2 : aliased RP.GPIO.GPIO_Point := (Pin => 20);
   B_Row_3 : aliased RP.GPIO.GPIO_Point := (Pin => 19);
   B_Row_4 : aliased RP.GPIO.GPIO_Point := (Pin => 16);
   B_Row_5 : aliased RP.GPIO.GPIO_Point := (Pin => 17);
   B_Row_6 : aliased RP.GPIO.GPIO_Point := (Pin => 18);

   B_Col_1 : aliased RP.GPIO.GPIO_Point := (Pin => 14);
   B_Col_2 : aliased RP.GPIO.GPIO_Point := (Pin => 13);
   B_Col_3 : aliased RP.GPIO.GPIO_Point := (Pin => 12);
   B_Col_4 : aliased RP.GPIO.GPIO_Point := (Pin => 11);
   B_Col_5 : aliased RP.GPIO.GPIO_Point := (Pin => 10);

   ----------
   -- LEDs --
   ----------

   WS2812_PIO    : PIO_Device renames RP.Device.PIO_0;
   WS2812_SM     : constant PIO_SM := 0;

   LED_Pin : aliased RP.GPIO.GPIO_Point := (Pin => 5);

   Number_Of_LEDs : constant := LED_Position'Length;
   --  subtype LED_ID is Natural range 1 .. Number_Of_LEDs;

   LED_Strip : aliased RP.PIO.WS2812.Strip (LED_Pin'Access,
                                            WS2812_PIO'Access,
                                            WS2812_SM,
                                            Number_Of_LEDs);
   -----------
   -- State --
   -----------

   function State return Buttons_State is
   begin
      return (others => False);
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
   begin
      LED_Strip.Set_RGB (LED_Position (L), R, G, B);
   end Set;

   ----------------
   -- Clear_LEDs --
   ----------------

   procedure Clear_LEDs is
   begin
      LED_Strip.Clear;
   end Clear_LEDs;

   -----------------
   -- Update_LEDs --
   -----------------

   procedure Update_LEDs is
   begin
      LED_Strip.Update;
   end Update_LEDs;

   ------------------
   -- Clear_Pixels --
   ------------------

   procedure Clear_Pixels is null;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (X : Pix_X; Y : Pix_Y; On : Boolean := True) is null;

   -------------------
   -- Update_Screen --
   -------------------

   procedure Update_Screen is null;

begin
   RP.Clock.Initialize (XOSC_Frequency);
   RP.Clock.Enable (RP.Clock.PERI);
   RP.Device.Timer.Enable;
   RP.GPIO.Enable;
   RP.DMA.Enable;

   RP.Device.PIO_0.Enable;
   RP.Device.PIO_1.Enable;

   LED_Strip.Initialize;

end WNM_PS1_HAL;
