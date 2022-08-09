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
with Sf.Window.Window; use Sf.Window.Window;
with Sf.Window.Event; use Sf.Window.Event;
with Sf; use Sf;

with Ada.Real_Time; use Ada.Real_Time;
with Sf.System.Vector2; use Sf.System.Vector2;

with Ada.Unchecked_Conversion;

with simulator_assets;
with GNAT.Command_Line; use GNAT.Command_Line;
with BBqueue.Buffers;
with BBqueue;
with WNM.MIDI.Queues;

with System;

package body WNM_Sim is

   pragma Warnings (Off);
   function Hack_SF_Binding is new Ada.Unchecked_Conversion (sfSprite_Ptr,
                                                             sfView_Ptr);
   pragma Warnings (On);

   --------------------
   -- Print_MIDI_Out --
   --------------------

   procedure Print_MIDI_Out is
      use BBqueue;
      use BBqueue.Buffers;
      use WNM.MIDI;
      use Ada.Text_IO;

      Out_Grant : Buffers.Read_Grant;
   begin

      loop
         WNM.MIDI.Queues.MIDI_Out_Read (Out_Grant, 3);

         exit when State (Out_Grant) /= Valid;

         declare
            Addr : constant System.Address := Slice (Out_Grant).Addr;
            Msg : WNM.MIDI.Message with Import, Address => Addr;
         begin
            Put_Line (Img (Msg));
         end;

         WNM.MIDI.Queues.MIDI_Out_Release (Out_Grant);

      end loop;
   end Print_MIDI_Out;

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

   ---------------
   -- Draw_LEDS --
   ---------------

   Rect : constant sfRectangleShape_Ptr := create;

   procedure Draw_LEDS (W : Sf.Graphics.sfRenderWindow_Ptr) is
      LED_Offset : constant array (WNM.LEDs) of sfVector2f :=
        (WNM.B1 => (159.0, 401.0),
         WNM.B2 => (256.0, 401.0),
         WNM.B3 => (353.0, 401.0),
         WNM.B4 => (451.0, 401.0),
         WNM.B5 => (548.0, 401.0),
         WNM.B6 => (644.0, 401.0),
         WNM.B7 => (741.0, 401.0),
         WNM.B8 => (840.0, 401.0),
         WNM.B9 => (159.0, 514.0),
         WNM.B10 => (256.0, 514.0),
         WNM.B11 => (353.0, 514.0),
         WNM.B12 => (451.0, 514.0),
         WNM.B13 => (548.0, 514.0),
         WNM.B14 => (644.0, 514.0),
         WNM.B15 => (741.0, 514.0),
         WNM.B16 => (840.0, 514.0),
         WNM.Rec => (943.0, 514.0),
         WNM.Play => (943.0, 401.0));

      LED_Size : constant := 20.0;
   begin
      setOutlineColor (Rect, Sf.Graphics.Color.sfBlack);
      setOutlineThickness (Rect, 1.0);
      setSize (Rect, (LED_Size, LED_Size));

      for L in SFML_LEDs'Range loop
         setFillColor (Rect, SFML_LEDs (L));
         setPosition (Rect, LED_Offset (L));
         drawRectangleShape (W, Rect);
      end loop;
   end Draw_LEDS;

   task Periodic_Update is

   end Periodic_Update;

   ---------------------
   -- Periodic_Update --
   ---------------------

   task body Periodic_Update is
      BG_Width : constant := 1024;
      BG_Height : constant := 655;
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

      Screen_Scale : constant := 263.0 / Float (Screen_Width);
      Screen_Offset : constant sfVector2f := (377.0, 6.0);
   begin

      Framebuffer_Texture := Create (Screen_Width, Screen_Height);
      if Framebuffer_Texture = null then
         Put_Line ("Failed to create screen texture");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Render_Texture := create (Screen_Width, Screen_Height, False);
      if Render_Texture = null then
         Put_Line ("Could not create render texture");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Screen_Sprite := Create;
      if Screen_Sprite = null then
         Put_Line ("Could not create screen sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      SetTexture (Screen_Sprite, getTexture (Render_Texture));
      scale (Screen_Sprite, (Screen_Scale, Screen_Scale));
      setPosition (Screen_Sprite, Screen_Offset);

      Sprite_Right := Create;
      if Sprite_Right = null then
         Put_Line ("Could not create sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      SetTexture (Sprite_Right, Framebuffer_Texture);

      declare
         Ptr : constant simulator_assets.Content_Access :=
           simulator_assets.Get_Content ("wnm_seq_pict.png").Content;
      begin
         BG_Texture := createFromMemory (Ptr.all'Address, Ptr.all'Length);
      end;

      BG_Sprite := Create;
      if BG_Sprite = null then
         Put_Line ("Could not create sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      SetTexture (BG_Sprite, BG_Texture);
      Window := Create (Mode, "PyGamer simulator", sfResize or sfClose, Params);
      if Window = null then
         Put_Line ("Failed to create window");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      SetVerticalSyncEnabled (Window, sfFalse);
      SetVisible (Window, sfTrue);

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
               Close (Window);
               Put_Line ("Attempting to close");
               GNAT.OS_Lib.OS_Exit (0);
            end if;

            if Event.eventType = sfEvtResized then
               Set_View (Letter_Box_View, Event.size.width, Event.size.height);
            end if;

            if Event.eventType in sfEvtKeyPressed then
               if Event.key.code = sfKeyEscape then
                  Close (Window);
                  Put_Line ("Attempting to close");
                  GNAT.OS_Lib.OS_Exit (0);
               elsif Event.key.code = sfKeyRight then
                  Encoder_Left := 1;
               elsif Event.key.code = sfKeyLeft then
                  Encoder_Left := -1;
               elsif Event.key.code = sfKeyDown then
                  Encoder_Right := -1;
               elsif Event.key.code = sfKeyUp then
                  Encoder_Right := 1;
               end if;
            end if;

            if Event.eventType in sfEvtKeyPressed | sfEvtKeyReleased then
               for K in SFML_Keys loop
                  if Event.key.code = To_SFML_Evt (K) then
                     SFML_Pressed (K) := Event.eventType = sfEvtKeyPressed;
                  end if;
               end loop;
            end if;
         end loop;

         updateFromPixels (texture => Framebuffer_Texture,
                           pixels  => Frame_Buffer (Frame_Buffer'First)'access,
                           width   => Screen_Width,
                           height  => Screen_Height,
                           x       => 0,
                           y       => 0);

         setPosition (Sprite_Right, (0.0, 0.0));

         drawSprite (Render_Texture, Hack_SF_Binding (Sprite_Right));
         display (Render_Texture);

         clear (Window);
         drawSprite (Window, BG_Sprite);
         drawSprite (Window, Screen_Sprite);
         Draw_LEDS (Window);

         setView (Window, Letter_Box_View);
         display (Window);

         Print_MIDI_Out;

      end loop;
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Message (E));
         GNAT.OS_Lib.OS_Exit (1);
   end Periodic_Update;

begin

   declare
      Config : Command_Line_Configuration;
   begin
      Define_Switch
        (Config,
         WNM_Sim.Switch_Storage_Image'Access,
         "-i:",
         Long_Switch => "--img=",
         Help => "Internal storage image (littlefs format)");

      Set_Usage
        (Config,
         "--img=<filesystem-image>",
         "Wee-Noise-Maker Simulator");

      Getopt (Config);
   exception
      when GNAT.Command_Line.Invalid_Switch =>
         GNAT.OS_Lib.OS_Exit (1);
      when GNAT.Command_Line.Exit_From_Command_Line =>
         GNAT.OS_Lib.OS_Exit (0);
   end;
end WNM_Sim;
