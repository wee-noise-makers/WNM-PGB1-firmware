with Ada.Synchronous_Task_Control;
with GNAT.Bounded_Buffers;
with System;

with Sf;
with Sf.Window.Keyboard; use Sf.Window.Keyboard;
with Sf.Graphics.Color; use Sf.Graphics.Color;

with WNM_Configuration; use WNM_Configuration;
with WNM.UI;
with WNM_HAL;

with Interfaces; use Interfaces;

with Stopwatch;
with Enum_Next;

package ASFML_Sim is

   procedure Start;

   Sim_Clock : Stopwatch.Instance;

   Screen_Width : constant := WNM_Configuration.Screen_Width;
   Screen_Height : constant := WNM_Configuration.Screen_Height;

   Frame_Buffer : array (0 .. (Screen_Width * Screen_Height * 4) - 1) of
     aliased Sf.sfUint8
       := (others => 0);

   Pix_Color : constant sfColor := fromRGB (0, 153, 255);
   BG_Color  : constant sfColor := fromRGB (0, 0, 0);

   type Keyboard_Layout is array (WNM_Configuration.Button) of
     Sf.Window.Keyboard.sfKeyCode;

   type Sim_Keyboard_Layout_Kind is (Qwerty_Layout,
                                     Azerty_Layout,
                                     Qwertz_Layout
                                    );

   function Img (L : Sim_Keyboard_Layout_Kind) return String
   is (case L is
          when Qwerty_Layout => "Qwerty",
          when Azerty_Layout => "Azerty",
          when Qwertz_Layout => "Qwertz");

   package Sim_Keyboard_Layout_Kind_Next
   is new Enum_Next (Sim_Keyboard_Layout_Kind, Wrap => True);
   use Sim_Keyboard_Layout_Kind_Next;

   Current_Layout : Sim_Keyboard_Layout_Kind := Qwerty_Layout;

   Layout_Maps : constant
     array (Sim_Keyboard_Layout_Kind) of Keyboard_Layout :=
     (Qwerty_Layout => (B1             => sfKeyW,
                        B2             => sfKeyE,
                        B3             => sfKeyR,
                        B4             => sfKeyT,
                        B5             => sfKeyY,
                        B6             => sfKeyU,
                        B7             => sfKeyI,
                        B8             => sfKeyO,
                        B9             => sfKeyS,
                        B10            => sfKeyD,
                        B11            => sfKeyF,
                        B12            => sfKeyG,
                        B13            => sfKeyH,
                        B14            => sfKeyJ,
                        B15            => sfKeyK,
                        B16            => sfKeyL,
                        Rec            => sfKeyRShift,
                        Play           => sfKeyEnter,
                        Menu           => sfKeyNum9,
                        Func           => sfKeyEqual,
                        Step_Button    => sfKeyA,
                        Track_Button   => sfKeyQ,
                        Pattern_Button => sfKeyDash,
                        Song_Button   => sfKeyNum0,
                        PAD_Up         => sfKeyUp,
                        PAD_Down       => sfKeyDown,
                        PAD_Left       => sfKeyLeft,
                        PAD_Right      => sfKeyRight,
                        PAD_A          => sfKeyNum2,
                        PAD_B          => sfKeyNum1),

      Azerty_Layout => (B1             => sfKeyZ,
                        B2             => sfKeyE,
                        B3             => sfKeyR,
                        B4             => sfKeyT,
                        B5             => sfKeyY,
                        B6             => sfKeyU,
                        B7             => sfKeyI,
                        B8             => sfKeyO,
                        B9             => sfKeyS,
                        B10            => sfKeyD,
                        B11            => sfKeyF,
                        B12            => sfKeyG,
                        B13            => sfKeyH,
                        B14            => sfKeyJ,
                        B15            => sfKeyK,
                        B16            => sfKeyL,
                        Rec            => sfKeyRShift,
                        Play           => sfKeyEnter,
                        Menu           => sfKeyNum9,
                        Func           => sfKeyEqual,
                        Step_Button    => sfKeyQ,
                        Track_Button   => sfKeyA,
                        Pattern_Button => sfKeyLBracket,
                        Song_Button   => sfKeyNum0,
                        PAD_Up         => sfKeyUp,
                        PAD_Down       => sfKeyDown,
                        PAD_Left       => sfKeyLeft,
                        PAD_Right      => sfKeyRight,
                        PAD_A          => sfKeyNum2,
                        PAD_B          => sfKeyNum1),

      Qwertz_Layout => (B1             => sfKeyW,
                        B2             => sfKeyE,
                        B3             => sfKeyR,
                        B4             => sfKeyT,
                        B5             => sfKeyZ,
                        B6             => sfKeyU,
                        B7             => sfKeyI,
                        B8             => sfKeyO,
                        B9             => sfKeyS,
                        B10            => sfKeyD,
                        B11            => sfKeyF,
                        B12            => sfKeyG,
                        B13            => sfKeyH,
                        B14            => sfKeyJ,
                        B15            => sfKeyK,
                        B16            => sfKeyL,
                        Rec            => sfKeyRShift,
                        Play           => sfKeyEnter,
                        Menu           => sfKeyNum9,
                        Func           => sfKeyRBracket,
                        Step_Button    => sfKeyA,
                        Track_Button   => sfKeyQ,
                        Pattern_Button => sfKeyDash,
                        Song_Button   => sfKeyNum0,
                        PAD_Up         => sfKeyUp,
                        PAD_Down       => sfKeyDown,
                        PAD_Left       => sfKeyLeft,
                        PAD_Right      => sfKeyRight,
                        PAD_A          => sfKeyNum2,
                        PAD_B          => sfKeyNum1)
     );

   function To_SFML_Evt (B : WNM_Configuration.Button)
                         return Sf.Window.Keyboard.sfKeyCode
   is (Layout_Maps (Current_Layout) (B));

   Change_Keyboard_Layout_Key : constant Sf.Window.Keyboard.sfKeyCode :=
     sfKeyF2;

   SFML_Pressed : array (WNM_Configuration.Button) of Boolean :=
     (others => False);
   pragma Volatile_Components (SFML_Pressed);

   Force_Pressed : array (WNM_Configuration.Button) of Boolean :=
     (others => False);
   pragma Volatile_Components (Force_Pressed);

   Fuzz_Key_Presses : Boolean := False;

   Strip_Touch : Boolean := False;
   Strip_Value : WNM_HAL.Touch_Value := 0.0;

   Main_Volume : Interfaces.Integer_16 := Interfaces.Integer_16'Last / 2;

   type SFML_LED_Strip is array (WNM_Configuration.LED) of
     Sf.Graphics.Color.sfColor;

   SFML_LEDs : SFML_LED_Strip :=
     (others => Sf.Graphics.Color.sfTransparent);

   procedure Take_Screenshot (Path : String);

   -- User input logs --
   type Input_Event_Kind is (Button, Up_Down, Left_Right);

   type Input_Event (Kind : Input_Event_Kind := Button) is record
      case Kind is
         when Button =>
            B : WNM_Configuration.Button;
            Evt : WNM.UI.Button_Event;
         when Up_Down | Left_Right =>
            Delt : Integer;
      end case;
   end record;

   package Input_Event_Queues
   is new GNAT.Bounded_Buffers (Input_Event);

   User_Input_Event_Logs : Input_Event_Queues.Bounded_Buffer
     (2048, System.Priority'Last);

   package Audio_Block_Queues
   is new GNAT.Bounded_Buffers (WNM_HAL.Stereo_Buffer);

   Audio_Block_Queue : Audio_Block_Queues.Bounded_Buffer
     (128, System.Priority'Last);

end ASFML_Sim;
