with Sf; use Sf;
with Sf.Window.Event;
with Sf.Graphics;
with Sf.System.Vector2;

private package ASFML_Sim.Window is

   type Instance is tagged private;

   procedure Init (This : in out Instance);

   procedure Update (This : in out Instance);

   procedure Resize (This : in out Instance);

   procedure Toggle_Side_Panel (This : in out Instance);

   procedure Show_Menu (This : in out Instance; Show : Boolean := True);

   function Poll_Event (This  : in out Instance;
                        Event : in out Sf.Window.Event.sfEvent)
                        return Boolean;

   procedure Close (This : in out Instance);

   procedure Screenshot (This : in out Instance;
                         Path :        String);

private

   type Panel is tagged record
      Enabled : Boolean := True;
      Render_Texture : Sf.Graphics.sfRenderTexture_Ptr;
      Sprite : Sf.Graphics.sfSprite_Ptr;
   end record;

   procedure Init (This : in out Panel; Width, Height : sfUint32);

   procedure Set_Enable (This : in out Panel; Enable : Boolean := True);

   procedure Draw (This : in out Panel;
                   Window : Sf.Graphics.sfRenderWindow_Ptr);

   procedure Resize (This : in out Panel;
                     X, Y, Width, Height : Float;
                     Center     : Boolean := False;
                     Keep_Ratio : Boolean := False);

   function Size (This : Panel) return Sf.System.Vector2.sfVector2u;

   type Instance
   is tagged
           record
              Show_Data_Panel : Boolean := False;

              Font : Sf.Graphics.sfFont_Ptr;
              Text : Sf.Graphics.sfText_Ptr;
              Rect : Sf.Graphics.sfRectangleShape_Ptr;

              Window : Sf.Graphics.sfRenderWindow_Ptr;
              Letter_Box_View : Sf.Graphics.sfView_Ptr;

              Sim_Panel : Panel;
              Data_Panel : Panel;
              Menu_Panel : Panel;

              BG_Texture : Sf.Graphics.sfTexture_Ptr;
              BG_Sprite : Sf.Graphics.sfSprite_Ptr;

              OLED_Texture : Sf.Graphics.sfTexture_Ptr;
              OLED_Sprite : Sf.Graphics.sfSprite_Ptr;
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

end ASFML_Sim.Window;
