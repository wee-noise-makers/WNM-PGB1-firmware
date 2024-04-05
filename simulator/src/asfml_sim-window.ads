with Sf; use Sf;
with Sf.Window.Event;
with Sf.Graphics;
with Sf.System.Vector2;

private with ASFML_Sim.FFT;

private package ASFML_Sim.Window is

   type Instance is tagged private;

   procedure Init (This   : in out Instance);

   procedure Update (This : in out Instance);

   procedure Resize (This : in out Instance);

   procedure Toggle_Side_Panel (This : in out Instance);

   procedure Show_Menu (This : in out Instance; Show : Boolean := True);

   function Poll_Event (This  : in out Instance;
                        Event : in out Sf.Window.Event.sfEvent)
                        return Boolean;

   function To_Touch_Point_Value (This  : in out Instance;
                                  Mouse_Pos_X, Mouse_Pos_Y : sfInt32)
                                  return Float;
   --  Return a value in range 0.0 .. 1.0 if the point is within the touch
   --  strip.

   procedure Close (This : in out Instance);

   procedure Screenshot (This : in out Instance;
                         Path :        String);

private

   type Panel is tagged record
      Enabled : Boolean := True;
      Render_Texture : Sf.Graphics.sfRenderTexture_Ptr := null;
      Sprite : Sf.Graphics.sfSprite_Ptr := null;
      Shader : Sf.Graphics.sfShader_Ptr := null;
   end record;

   procedure Init (This          : in out Panel;
                   Width, Height :        sfUint32;
                   Shader        :        Sf.Graphics.sfShader_Ptr := null);

   procedure Set_Enable (This : in out Panel; Enable : Boolean := True);

   procedure Draw (This : in out Panel;
                   Window : Sf.Graphics.sfRenderWindow_Ptr);

   procedure Resize (This : in out Panel;
                     X, Y, Width, Height : Float;
                     Center     : Boolean := False;
                     Keep_Ratio : Boolean := False);

   function Size (This : Panel) return Sf.System.Vector2.sfVector2u;

   Analyzer_FFT_Size : constant := 4096;
   subtype Bin_Range is Natural range 1 .. Analyzer_FFT_Size / 2;
   type Bin_Array is array (Bin_Range) of Float;

   type Instance
   is tagged
           record
              Show_Data_Panel : Boolean := False;
              Show_Splashscreen : Boolean := True;

              Font : Sf.Graphics.sfFont_Ptr;
              Text : Sf.Graphics.sfText_Ptr;
              Rect : Sf.Graphics.sfRectangleShape_Ptr;

              Window : Sf.Graphics.sfRenderWindow_Ptr;
              Letter_Box_View : Sf.Graphics.sfView_Ptr;

              Sim_Panel : Panel;
              Menu_Panel : Panel;
              Data_Panel : Panel;
              Keylog_Panel : Panel;

              BG_Texture : Sf.Graphics.sfTexture_Ptr;
              BG_Sprite : Sf.Graphics.sfSprite_Ptr;

              OLED_Texture : Sf.Graphics.sfTexture_Ptr;
              OLED_Sprite : Sf.Graphics.sfSprite_Ptr;

              Wave_Left, Wave_Right : Sf.Graphics.sfVertexArray_Ptr;

              Glow_Shader : Sf.Graphics.sfShader_Ptr;

              FFT : ASFML_Sim.FFT.Instance (Analyzer_FFT_Size,
                                            Analyzer_FFT_Size / 4);
              Wave_FFT, Wave_FFT_Peak : Sf.Graphics.sfVertexArray_Ptr;
              Energy        : Bin_Array := (others => 0.0);
              Peak_Energy   : Bin_Array := (others => 0.0);
           end record;

   procedure Draw_Text
     (This   : in out Instance;
      W      : Sf.Graphics.sfRenderTexture_Ptr;
      Pos    : Sf.System.Vector2.sfVector2f;
      Str    : String;
      Color  : Sf.Graphics.Color.sfColor := Sf.Graphics.Color.sfWhite;
      Centered : Boolean := True);

   procedure Draw_LEDS (This : in out Instance;
                        W    :        Sf.Graphics.sfRenderTexture_Ptr);

   procedure Draw_Buttons (This : in out Instance;
                           W    :        Sf.Graphics.sfRenderTexture_Ptr);

   procedure Draw_Buttons_Label
     (This : in out Instance;
      W    :        Sf.Graphics.sfRenderTexture_Ptr);

end ASFML_Sim.Window;
