with Ada.Synchronous_Task_Control;

with GNAT.Strings;

with Sf;
with Sf.Window.Keyboard; use Sf.Window.Keyboard;
with Sf.Graphics.Color;

with WNM.Screen;
with WNM;

package WNM_Sim
with Elaborate_Body
is

   Switch_Storage_Image : aliased GNAT.Strings.String_Access;

   Screen_Width : constant := WNM.Screen.Width;
   Screen_Height : constant := WNM.Screen.Height;

   Frame_Buffer : array (0 .. (Screen_Width * Screen_Height * 4) - 1) of
     aliased Sf.sfUInt8
       := (others => 0);

   type SFML_Keys is (B1, B2, B3, B4, B5, B6, B7, B8,
                      B9, B10, B11, B12, B13, B14, B15, B16,
                      Rec, Play,
                      Menu, Func,
                      Step_Button, Track_Button, Pattern_Button,
                      Encoder_L, Encoder_R);

   To_SFML_Evt : array (SFML_Keys) of sf.Window.Keyboard.sfKeyCode
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
         Rec => sfKeyBackslash,
         Play => sfKeyEnter,
         Menu => sfKeyEqual,
         Func => sfKeyDash,
         Step_Button => sfKeyA,
         Track_Button => sfKeyQ,
         Pattern_Button => sfKeyNum9,
         Encoder_L => sfKeyNum1,
         Encoder_R => sfKeyNum2);

   SFML_Pressed : array (SFML_Keys) of Boolean := (others => False);
   pragma Volatile_Components (SFML_Pressed);

   Force_Pressed : array (WNM.Button) of Boolean := (others => False);
   pragma Volatile_Components (Force_Pressed);

   Button_Scan_Signal : Ada.Synchronous_Task_Control.Suspension_Object;

   Encoder_Right : Integer := 0;
   Encoder_Left  : Integer := 0;

   SFML_LEDs : array (WNM.LEDs) of sf.Graphics.Color.sfColor :=
     (others => sf.Graphics.Color.sfTransparent);

end WNM_Sim;
