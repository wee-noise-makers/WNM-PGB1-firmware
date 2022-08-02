with Ada.Synchronous_Task_Control;

with GNAT.Strings;

with Sf;
with Sf.Window.Keyboard; use Sf.Window.Keyboard;
with Sf.Graphics.Color;

with WNM_PS1_HAL_Params; use WNM_PS1_HAL_Params;

package ASFML_Sim
with Elaborate_Body
is

   Switch_Storage_Image : aliased GNAT.Strings.String_Access;

   Screen_Width : constant := WNM_PS1_HAL_Params.Screen_Width;
   Screen_Height : constant := WNM_PS1_HAL_Params.Screen_Height;

   Frame_Buffer : array (0 .. (Screen_Width * Screen_Height * 4) - 1) of
     aliased Sf.sfUint8
       := (others => 0);

   To_SFML_Evt : array (WNM_PS1_HAL_Params.Button) of
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
         Chord => sfKeyNum0,
         Encoder_L => sfKeyNum1,
         Encoder_R => sfKeyNum2);

   SFML_Pressed : array (WNM_PS1_HAL_Params.Button) of Boolean :=
     (others => False);
   pragma Volatile_Components (SFML_Pressed);

   Force_Pressed : array (WNM_PS1_HAL_Params.Button) of Boolean :=
     (others => False);
   pragma Volatile_Components (Force_Pressed);

   Button_Scan_Signal : Ada.Synchronous_Task_Control.Suspension_Object;

   Encoder_Right : Integer := 0;
   Encoder_Left  : Integer := 0;

   type SFML_LED_Strip is array (WNM_PS1_HAL_Params.LED) of
     Sf.Graphics.Color.sfColor;

   SFML_LEDs : SFML_LED_Strip :=
     (others => Sf.Graphics.Color.sfTransparent);

end ASFML_Sim;
