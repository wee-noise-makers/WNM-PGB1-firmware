with System;
with Interfaces.C; use Interfaces.C;

with Ada.Exceptions;
with GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with Sf.Window.VideoMode; use Sf.Window.VideoMode;
with Sf.Graphics; use Sf.Graphics;
with Sf.Graphics.Sprite; use Sf.Graphics.Sprite;
with Sf.Graphics.Texture; use Sf.Graphics.Texture;
with Sf.Graphics.RenderTexture; use Sf.Graphics.RenderTexture;
with Sf.Graphics.View; use Sf.Graphics.View;
with Sf.Graphics.RenderWindow; use Sf.Graphics.RenderWindow;
with Sf.Graphics.RectangleShape; use Sf.Graphics.RectangleShape;
with Sf.Graphics.Font;
with Sf.Graphics.Text;
with Sf.Graphics.Image;
with Sf.Window.Window; use Sf.Window.Window;
with Sf.Window.Event; use Sf.Window.Event;
with Sf; use Sf;

with WNM.Synth;
with WNM.GUI.Update;

with ASFML_Util; use ASFML_Util;

with ASFML_SIM_Menu;
with ASFML_Sim_Resources;
with Ada.Real_Time; use Ada.Real_Time;
with Sf.System.Vector2; use Sf.System.Vector2;
with Sf.Graphics.Rect; use Sf.Graphics.Rect;
with WNM_HAL;

with Tresses.Resources;

package body ASFML_Sim is

   LED_Offset : constant array (WNM_Configuration.LED) of sfVector2f :=
     (
      Menu           => (753.0, 293.0),
      Chord_Button   => (869.0, 293.0),
      Pattern_Button => (985.0, 293.0),
      Func           => (1114.0, 293.0),

      Track_Button => (45.0, 434.0),
      B1           => (174.0, 434.0),
      B2           => (290.0, 434.0),
      B3           => (406.0, 434.0),
      B4           => (522.0, 434.0),
      B5           => (638.0, 434.0),
      B6           => (753.0, 434.0),
      B7           => (869.0, 434.0),
      B8           => (985.0, 434.0),
      Play         => (1114.0, 434.0),

      Step_Button  => (45.0, 576.0),
      B9           => (174.0, 576.0),
      B10          => (290.0, 576.0),
      B11          => (406.0, 576.0),
      B12          => (522.0, 576.0),
      B13          => (638.0, 576.0),
      B14          => (753.0, 576.0),
      B15          => (869.0, 576.0),
      B16          => (985.0, 576.0),
      Rec          => (1114.0, 576.0));

   Font : Sf.Graphics.sfFont_Ptr;

   ------------- Audio

   procedure RTaudio_Callback (Buf    : System.Address;
                               Frames : Interfaces.C.unsigned);
   pragma Export (C, RTaudio_Callback, "wnm_rtaudio_callback");

   ----------------------
   -- RTaudio_Callback --
   ----------------------

   procedure RTaudio_Callback (Buf : System.Address;
                               Frames : Interfaces.C.unsigned)
   is
      In_Buffer : constant WNM_HAL.Stereo_Buffer := (others => (0, 0));

      Out_Buffer : WNM_HAL.Stereo_Buffer
        with Address => Buf;
   begin
      if Frames /= WNM_Configuration.Audio.Samples_Per_Buffer then
         raise Program_Error with "Invalid buffer size from RTAudio: " &
           Frames'Img;
      end if;

      WNM.Synth.Next_Points (Out_Buffer, In_Buffer);

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         GNAT.OS_Lib.OS_Exit (42);
   end RTaudio_Callback;

   --------------
   -- Set_View --
   --------------

   procedure Set_View (View : Sf.Graphics.sfView_Ptr;
                       Width, Height : sfUint32)
   is
      Win_Ratio  : constant Float := Float (Width) / Float (Height);
      View_Ratio : constant Float := getSize (View).x / getSize (View).y;
      Size_X : Float := 1.0;
      Size_Y : Float := 1.0;
      Pos_X : Float := 0.0;
      Pos_Y : Float := 0.0;

   begin
      if Win_Ratio < View_Ratio then
         Size_Y := Win_Ratio / View_Ratio;
         Pos_Y := (1.0 - Size_Y) / 2.0;
      else
         Size_X := View_Ratio / Win_Ratio;
         Pos_X := (1.0 - Size_X) / 2.0;
      end if;

      setViewport (View, (Pos_X, Pos_Y, Size_X, Size_Y));
   end Set_View;

   function Key_Image (K : Sf.Window.Keyboard.sfKeyCode) return String
   is ("[" & (case K is
          when sfKeyW => "W",
          when sfKeyE => "E",
          when sfKeyR => "R",
          when sfKeyT => "T",
          when sfKeyY => "Y",
          when sfKeyU => "U",
          when sfKeyI => "I",
          when sfKeyO => "O",
          when sfKeyS => "S",
          when sfKeyD => "D",
          when sfKeyF => "F",
          when sfKeyG => "G",
          when sfKeyH => "H",
          when sfKeyJ => "J",
          when sfKeyK => "K",
          when sfKeyL => "L",
          when sfKeyEnter => "Enter",
          when sfKeyBackslash => "\",
          when sfKeyNum9 => "9",
          when sfKeyEqual => "=",
          when sfKeyA => "A",
          when sfKeyQ => "Q",
          when sfKeyDash => "-",
          when sfKeyNum0 => "0",
          when sfKeyNum1 => "1",
          when sfKeyNum2 => "2",
          when others => "UNKNOWN KEY EVT"
      ) & "]");

   ---------------
   -- Draw_Text --
   ---------------

   Text : constant Sf.Graphics.sfText_Ptr := Sf.Graphics.Text.create;

   procedure Draw_Text
     (W      : Sf.Graphics.sfRenderWindow_Ptr;
      Pos    : sfVector2f;
      Str    : String;
      Color  : Sf.Graphics.Color.sfColor := Sf.Graphics.Color.sfWhite)
   is
      use Sf.Graphics.Text;
      Rect : sfFloatRect;
   begin
      setFont (Text, Font);
      setString (Text, Str);
      setColor (Text, Color);

      Rect := getLocalBounds (Text);
      setOrigin (Text, (Rect.left + (Rect.width / 2.0),
                        Rect.top  + (Rect.height / 2.0)));
      setPosition (Text, Pos);

      drawText (W, Text);
   end Draw_Text;

   ---------------
   -- Draw_LEDS --
   ---------------

   Rect : constant sfRectangleShape_Ptr := create;

   procedure Draw_LEDS (W : Sf.Graphics.sfRenderWindow_Ptr) is
   begin
      setOutlineColor (Rect, Sf.Graphics.Color.sfBlack);
      setOutlineThickness (Rect, 1.0);
      setSize (Rect, (85.0, 30.0));

      for L in SFML_LEDs'Range loop
         setFillColor (Rect, SFML_LEDs (L));
         setPosition (Rect, LED_Offset (L));
         drawRectangleShape (W, Rect);
      end loop;
   end Draw_LEDS;

   ------------------
   -- Draw_Buttons --
   ------------------

   procedure Draw_Buttons (W : Sf.Graphics.sfRenderWindow_Ptr) is

      Rect_Size : constant sfVector2f := (83.0, 83.0);
      Text_Offset : constant sfVector2f := (Rect_Size.x / 2.0,
                                            Rect_Size.y / 2.0);

      Pos : sfVector2f;
   begin
      setOutlineColor (Rect, Sf.Graphics.Color.sfBlack);
      setOutlineThickness (Rect, 1.0);
      setSize (Rect, Rect_Size);

      for B in SFML_Pressed'Range loop

         case B is
            when LED_Offset'Range =>
               Pos := LED_Offset (B);
            when Encoder_L =>
               Pos := (77.0, 241.5);
            when Encoder_R =>
               Pos := (271.0, 241.5);
         end case;

         Pos := Pos + (-3.0, 39.0);

         if SFML_Pressed (B) or else Force_Pressed (B) then
            setFillColor (Rect, Sf.Graphics.Color.sfBlue);
            setOutlineColor (Rect, Sf.Graphics.Color.sfTransparent);
            setPosition (Rect, Pos);
            drawRectangleShape (W, Rect);
         end if;
         Draw_Text (W,
                    Pos + Text_Offset,
                    Key_Image (To_SFML_Evt (B)));
      end loop;
   end Draw_Buttons;

   ----------------------------
   -- Screenshot_From_Window --
   ----------------------------

   procedure Screenshot_From_Window (Window : sfRenderWindow_Ptr;
                                     Path : String)
   is
      Size : constant Sf.System.Vector2.sfVector2u :=
        Sf.Graphics.RenderWindow.getSize (Window);

      Tex : constant Sf.Graphics.sfTexture_Ptr :=
        Sf.Graphics.Texture.create (Size.x, Size.y);

      Img : Sf.Graphics.sfImage_Ptr;
   begin
      Sf.Graphics.Texture.updateFromRenderWindow
        (Tex, Window, 0, 0);

      Img := Sf.Graphics.Texture.copyToImage (Tex);

      if not Sf.Graphics.Image.saveToFile (Img, Path) then
         raise Program_Error with "Cannot save screenshot...";
      end if;

      Sf.Graphics.Image.destroy (Img);
      Sf.Graphics.Texture.destroy (Tex);
   end Screenshot_From_Window;

   task Periodic_Update is
      entry Start;
      entry Take_Screenshot (Path : String);
   end Periodic_Update;

   ---------------------
   -- Periodic_Update --
   ---------------------

   task body Periodic_Update is
      BG_Width : constant := 1236;
      BG_Height : constant := 804;
      Mode   : constant Sf.Window.VideoMode.sfVideoMode :=
        (BG_Width, BG_Height, 32);

      Params : constant sfContextSettings := sfDefaultContextSettings;
      Window : Sf.Graphics.sfRenderWindow_Ptr;
      Framebuffer_Texture : Sf.Graphics.sfTexture_Ptr;
      BG_Texture : Sf.Graphics.sfTexture_Ptr;
      BG_Sprite : Sf.Graphics.sfSprite_Ptr;
      Render_Texture : Sf.Graphics.sfRenderTexture_Ptr;
      Screen_Sprite : Sf.Graphics.sfSprite_Ptr;
      Sprite_Right : Sf.Graphics.sfSprite_Ptr;
      Letter_Box_View : Sf.Graphics.sfView_Ptr;
      Event   : sfEvent;

      Period : constant Time_Span := Milliseconds (1000 / 60);
      Next_Release : Time := Clock + Period;

      Screen_Scale : constant := 296.0 / Float (Screen_Width);
      Screen_Offset : constant sfVector2f := (469.0, 33.0);
   begin

      accept Start;

      Framebuffer_Texture := create (Screen_Width, Screen_Height);
      if Framebuffer_Texture = null then
         Put_Line ("Failed to create screen texture");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Render_Texture := create (Screen_Width, Screen_Height, False);
      if Render_Texture = null then
         Put_Line ("Could not create render texture");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Screen_Sprite := create;
      if Screen_Sprite = null then
         Put_Line ("Could not create screen sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      setTexture (Screen_Sprite, getTexture (Render_Texture));
      scale (Screen_Sprite, (Screen_Scale, Screen_Scale));
      setPosition (Screen_Sprite, Screen_Offset);

      Sprite_Right := create;
      if Sprite_Right = null then
         Put_Line ("Could not create sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      setTexture (Sprite_Right, Framebuffer_Texture);

      BG_Texture := Sf.Graphics.Texture.createFromFile
        (ASFML_Sim_Resources.Resource_Path & "/WNM-PS1.png");

      BG_Sprite := create;
      if BG_Sprite = null then
         Put_Line ("Could not create sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      setTexture (BG_Sprite, BG_Texture);

      Font := Sf.Graphics.Font.createFromFile
        (ASFML_Sim_Resources.Resource_Path & "/ZeroesOne.ttf");
      if Font = null then
         Put_Line ("Could not load font");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Window := create (Mode, "WNM-PS1 Simulator",
                        sfResize or sfClose, Params);
      if Window = null then
         Put_Line ("Failed to create window");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      setVerticalSyncEnabled (Window, sfFalse);
      setVisible (Window, sfTrue);

      Letter_Box_View := create;
      if Letter_Box_View = null then
         Put_Line ("Failed to create view");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      setSize (Letter_Box_View, (Float (BG_Width), Float (BG_Height)));
      setCenter (Letter_Box_View,
                 (Float (BG_Width) / 2.0, Float (BG_Height) / 2.0));

      Set_View (Letter_Box_View, getSize (Window).x, getSize (Window).y);

      loop
         delay until Next_Release;
         Next_Release := Next_Release + Period;

         while pollEvent (Window, Event) loop

            if Event.eventType = sfEvtClosed then
               close (Window);
               Put_Line ("Attempting to close");
               GNAT.OS_Lib.OS_Exit (0);
            end if;

            if Event.eventType = sfEvtResized then
               Set_View (Letter_Box_View, Event.size.width, Event.size.height);
            end if;

            if Sim_Clock.Is_Held then
               case ASFML_SIM_Menu.Event_Handler (Event) is
                  when ASFML_SIM_Menu.None =>
                     null;
                  when ASFML_SIM_Menu.Resume =>
                     Sim_Clock.Release;
                  when ASFML_SIM_Menu.Quit =>
                     close (Window);
                     Put_Line ("Attempting to close");
                     GNAT.OS_Lib.OS_Exit (0);
               end case;

            else

               if Event.eventType in sfEvtKeyPressed then
                  if Event.key.code = sfKeyEscape then
                     Sim_Clock.Hold;
                     --  close (Window);
                     --  Put_Line ("Attempting to close");
                     --  GNAT.OS_Lib.OS_Exit (0);
                  elsif Event.key.code = sfKeyRight then
                     Encoder_Left := (if Event.key.shift then 2 else 1);
                  elsif Event.key.code = sfKeyLeft then
                     Encoder_Left := (if Event.key.shift then -2 else -1);
                  elsif Event.key.code = sfKeyDown then
                     Encoder_Right := (if Event.key.shift then -2 else -1);
                  elsif Event.key.code = sfKeyUp then
                     Encoder_Right := (if Event.key.shift then 2 else 1);
                  elsif Event.key.code = sfKeyF12 then
                     Screenshot_From_Window (Window, "WNM-screenshot.png");
                  end if;
               end if;

               if Event.eventType in sfEvtKeyPressed | sfEvtKeyReleased then
                  for K in Button loop
                     if Event.key.code = To_SFML_Evt (K) then
                        SFML_Pressed (K) := Event.eventType = sfEvtKeyPressed;
                     end if;
                  end loop;
               end if;
            end if;
         end loop;

         WNM.GUI.Update.Update;

         updateFromPixels (texture => Framebuffer_Texture,
                           pixels  => Frame_Buffer (Frame_Buffer'First)'Access,
                           width   => Screen_Width,
                           height  => Screen_Height,
                           x       => 0,
                           y       => 0);

         setPosition (Sprite_Right, (0.0, 0.0));

         drawSprite (Render_Texture, Sprite_Right);
         display (Render_Texture);

         clear (Window);
         Draw_LEDS (Window);
         drawSprite (Window, BG_Sprite);
         drawSprite (Window, Screen_Sprite);
         Draw_Buttons (Window);

         select
            accept Take_Screenshot (Path : String) do
               Screenshot_From_Window (Window, Path);
            end Take_Screenshot;
         else
            null;
         end select;

         if Sim_Clock.Is_Held then
            ASFML_SIM_Menu.Render (Window, Font);
         end if;

         setView (Window, Letter_Box_View);
         display (Window);

         --  Print_MIDI_Out;

      end loop;
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Message (E));
         GNAT.OS_Lib.OS_Exit (1);
   end Periodic_Update;

   ----------------
   -- Init_Audio --
   ----------------

   procedure Init_Audio is
      function RTaudio_Init (Sample_Rate : Interfaces.C.unsigned;
                             Frames      : Interfaces.C.unsigned)
                             return Interfaces.C.int;
      pragma Import (C, RTaudio_Init, "wnm_rtaudio_init");
   begin

      if GNAT.OS_Lib.Getenv ("OS").all = "Windows_NT" then
         --  Select driver for openal on Windows
         GNAT.OS_Lib.Setenv ("ALSOFT_DRIVERS", "dsound");
      end if;

      if RTaudio_Init (Tresses.Resources.SAMPLE_RATE,
                       WNM_Configuration.Audio.Samples_Per_Buffer) /= 0
      then
         Ada.Text_IO.Put_Line ("rtaudio init error");
         GNAT.OS_Lib.OS_Exit (42);
      end if;

   end Init_Audio;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin

      Sim_Clock.Reset;
      Sim_Clock.Hold;

      Init_Audio;

      Periodic_Update.Start;
   end Start;

   ---------------------
   -- Take_Screenshot --
   ---------------------

   procedure Take_Screenshot (Path : String) is
   begin
      Periodic_Update.Take_Screenshot (Path);
   end Take_Screenshot;

end ASFML_Sim;
