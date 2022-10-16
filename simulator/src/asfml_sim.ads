with Ada.Synchronous_Task_Control;

with Sf;
with Sf.Window.Keyboard; use Sf.Window.Keyboard;
with Sf.Graphics.Color;

with WNM_Configuration; use WNM_Configuration;

with Stopwatch;

package ASFML_Sim is

   procedure Start;

   Sim_Clock : Stopwatch.Instance;

   Screen_Width : constant := WNM_Configuration.Screen_Width;
   Screen_Height : constant := WNM_Configuration.Screen_Height;

   Frame_Buffer : array (0 .. (Screen_Width * Screen_Height * 4) - 1) of
     aliased Sf.sfUint8
       := (others => 0);

   To_SFML_Evt : array (WNM_Configuration.Button) of
     Sf.Window.Keyboard.sfKeyCode
     := (B1 => sfKeyW,
         B2 => sfKeyE,
         B3 => sfKeyR,
         B4 => sfKeyT,
         B5 => sfKeyY,
         B6 => sfKeyU,
         B7 => sfKeyI,
         B8 => sfKeyO,
         B9 => sfKeyS,
         B10 => sfKeyD,
         B11 => sfKeyF,
         B12 => sfKeyG,
         B13 => sfKeyH,
         B14 => sfKeyJ,
         B15 => sfKeyK,
         B16 => sfKeyL,
         Rec => sfKeyEnter,
         Play => sfKeyBackslash,
         Menu => sfKeyNum9,
         Func => sfKeyEqual,
         Step_Button => sfKeyA,
         Track_Button => sfKeyQ,
         Pattern_Button => sfKeyDash,
         Chord_Button => sfKeyNum0,
         Encoder_L => sfKeyNum1,
         Encoder_R => sfKeyNum2);

   SFML_Pressed : array (WNM_Configuration.Button) of Boolean :=
     (others => False);
   pragma Volatile_Components (SFML_Pressed);

   Force_Pressed : array (WNM_Configuration.Button) of Boolean :=
     (others => False);
   pragma Volatile_Components (Force_Pressed);

   Button_Scan_Signal : Ada.Synchronous_Task_Control.Suspension_Object;

   Encoder_Right : Integer := 0;
   Encoder_Left  : Integer := 0;

   type SFML_LED_Strip is array (WNM_Configuration.LED) of
     Sf.Graphics.Color.sfColor;

   SFML_LEDs : SFML_LED_Strip :=
     (others => Sf.Graphics.Color.sfTransparent);

end ASFML_Sim;
