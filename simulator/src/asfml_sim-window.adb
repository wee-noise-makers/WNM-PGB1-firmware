with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with GNAT.OS_Lib;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ASFML_Sim_Resources;
with AAA.Strings;

with Sf.Graphics; use Sf.Graphics;
with Sf.Window.VideoMode; use Sf.Window.VideoMode;
with Sf.Graphics.Sprite; use Sf.Graphics.Sprite;
with Sf.Graphics.Color; use Sf.Graphics.Color;
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
with Sf.System.Vector2; use Sf.System.Vector2;
with Sf.Graphics.Rect; use Sf.Graphics.Rect;

with ASFML_Util; use ASFML_Util;
with ASFML_SIM_Menu;
with WNM_HAL;
with WNM.LEDs;
package body ASFML_Sim.Window is

   BG_Width : constant := 1236;
   BG_Height : constant := 804;

   Menu_Size : constant Sf.System.Vector2.sfVector2f := (350.0, 170.0);

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

   type User_Input_Event (Kind : Input_Event_Kind := Button) is record
      case Kind is
         when Button =>
            B : WNM_Configuration.Button;
         when Left_Encoder | Right_Encoder =>
            Delt : Integer;
      end case;
   end record;

   package User_Input_Event_List
   is new Ada.Containers.Doubly_Linked_Lists (User_Input_Event);
   use User_Input_Event_List;

   package User_Input_Event_List_List
   is new Ada.Containers.Doubly_Linked_Lists (User_Input_Event_List.List);

   B_List : User_Input_Event_List.List;
   User_Input_List : User_Input_Event_List_List.List;

   ----------------------
   -- Log_Button_Event --
   ----------------------

   procedure Log_Button_Event (Evt : Input_Event)
   is
      use Ada.Containers;
      use WNM.UI;

   begin
      if User_Input_List.Is_Empty then
         User_Input_List.Append (User_Input_Event_List.Empty_List);

      end if;

      case Evt.Kind is
         when Left_Encoder | Right_Encoder =>

            if not B_List.Is_Empty
              and then
               B_List.First_Element.Kind /= Button
              and then
               B_List.First_Element.Kind /= Evt.Kind
            then
               --  Start a new line of logs
               B_List.Clear;
               User_Input_List.Append (User_Input_Event_List.Empty_List);
            end if;

            if not B_List.Is_Empty
              and then
                B_List.Last_Element.Kind = Evt.Kind
            then
               --  Modify the current encoder event
               B_List.Reference (B_List.Last).Delt :=
                 B_List.Reference (B_List.Last).Delt + Evt.Delt;
            else

               --  Add new encoder event
               if Evt.Kind = Left_Encoder then
                  B_List.Append ((Left_Encoder, Evt.Delt));
               else
                  B_List.Append ((Right_Encoder, Evt.Delt));
               end if;
            end if;

         when Button =>

            if not B_List.Is_Empty
              and then
                B_List.First_Element.Kind /= Button
            then
               --  Start a new line of logs
               B_List.Clear;
               User_Input_List.Append (User_Input_Event_List.Empty_List);
            end if;

            case Evt.Evt is
            when On_Press | On_Long_Press =>
               --  if B_List.Find ((Button, Evt.B)) =
               --    User_Input_Event_List.No_Element
               --  then
                  B_List.Append ((Button, Evt.B));
               --  end if;

            when On_Release =>
               if not B_List.Is_Empty
                 and then
                   B_List.First_Element = (Button, Evt.B)
               then
                  B_List.Clear;
                  User_Input_List.Append (User_Input_Event_List.Empty_List);
               end if;

            when Waiting_For_Long_Press =>
               null;
            end case;
      end case;

      User_Input_List.Replace_Element (User_Input_List.Last, B_List);

      if User_Input_List.Length > 10 then
         User_Input_List.Delete_First;
      end if;
   end Log_Button_Event;

   ---------------
   -- Key_Image --
   ---------------

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

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Instance) is
      Mode   : constant Sf.Window.VideoMode.sfVideoMode :=
        (BG_Width, BG_Height, 32);

      Screen_Scale : constant := 296.0 / Float (Screen_Width);
      Screen_Offset : constant sfVector2f := (470.0, 36.0);

      Params : constant sfContextSettings := sfDefaultContextSettings;

   begin
      This.Window := create (Mode, "WNM-PS1 Simulator",
                             sfResize or sfClose, Params);
      if This.Window = null then
         Put_Line ("Failed to create window");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      setVerticalSyncEnabled (This.Window, sfFalse);
      setVisible (This.Window, sfTrue);

      This.Letter_Box_View := create;
      if This.Letter_Box_View = null then
         Put_Line ("Failed to create view");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      --  Sim background
      This.BG_Texture := Sf.Graphics.Texture.createFromFile
        (ASFML_Sim_Resources.Resource_Path & "/WNM-PS1.png");

      This.BG_Sprite := create;
      if This.BG_Sprite = null then
         Put_Line ("Could not create sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      setTexture (This.BG_Sprite, This.BG_Texture);
      ------------------

      --  OLED Screen
      This.OLED_Texture := create (Screen_Width, Screen_Height);
      if This.OLED_Texture = null then
         Put_Line ("Failed to create screen texture");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      This.OLED_Sprite := create;
      if This.OLED_Sprite = null then
         Put_Line ("Could not create sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      setTexture (This.OLED_Sprite, This.OLED_Texture);
      scale (This.OLED_Sprite, (Screen_Scale, Screen_Scale));
      setPosition (This.OLED_Sprite, Screen_Offset);
      ---------------

      --  Text
      This.Text := Sf.Graphics.Text.create;
      This.Font := Sf.Graphics.Font.createFromFile
        (ASFML_Sim_Resources.Resource_Path & "/ZeroesOne.ttf");
      if This.Font = null then
         Put_Line ("Could not load font");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      --------

      This.Rect := create;

      This.Sim_Panel.Init (BG_Width, BG_Height);
      This.Data_Panel.Init ((BG_Width / 10) * 4, BG_Height);

      This.Menu_Panel.Init (sfUint32 (Menu_Size.x), sfUint32 (Menu_Size.y));
      This.Menu_Panel.Set_Enable (False);

      This.Resize;
   end Init;

   ---------------
   -- Draw_Text --
   ---------------

   procedure Draw_Text
     (This   : in out Instance;
      W      : Sf.Graphics.sfRenderTexture_Ptr;
      Pos    : Sf.System.Vector2.sfVector2f;
      Str    : String;
      Color  : Sf.Graphics.Color.sfColor := Sf.Graphics.Color.sfWhite;
      Centered : Boolean := True)
   is
      use Sf.Graphics.Text;
      Rect : sfFloatRect;
   begin
      setFont (This.Text, This.Font);
      setString (This.Text, Str);
      setColor (This.Text, Color);

      Rect := getLocalBounds (This.Text);
      if Centered then
         setOrigin (This.Text, (Rect.left + (Rect.width / 2.0),
                           Rect.top  + (Rect.height / 2.0)));
      else
         setOrigin (This.Text, (Rect.left, Rect.top));
      end if;

      setPosition (This.Text, Pos);

      drawText (W, This.Text);
   end Draw_Text;

   ---------------
   -- Draw_LEDS --
   ---------------

   procedure Draw_LEDS (This : in out Instance;
                        W    :        Sf.Graphics.sfRenderTexture_Ptr)
   is
   begin
      setOutlineColor (This.Rect, Sf.Graphics.Color.sfBlack);
      setOutlineThickness (This.Rect, 1.0);
      setSize (This.Rect, (85.0, 30.0));

      for L in SFML_LEDs'Range loop
         setFillColor (This.Rect, SFML_LEDs (L));
         setPosition (This.Rect, LED_Offset (L));
         drawRectangleShape (W, This.Rect);
      end loop;
   end Draw_LEDS;

   ------------------
   -- Draw_Buttons --
   ------------------

   procedure Draw_Buttons (This : in out Instance;
                           W    :        Sf.Graphics.sfRenderTexture_Ptr)
   is

      Rect_Size : constant sfVector2f := (83.0, 83.0);
      Text_Offset : constant sfVector2f := (Rect_Size.x / 2.0,
                                            Rect_Size.y / 2.0);

      Pos : sfVector2f;
   begin
      setOutlineColor (This.Rect, Sf.Graphics.Color.sfBlack);
      setOutlineThickness (This.Rect, 1.0);
      setSize (This.Rect, Rect_Size);

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
            setFillColor (This.Rect, Sf.Graphics.Color.sfBlue);
            setOutlineColor (This.Rect, Sf.Graphics.Color.sfTransparent);
            setPosition (This.Rect, Pos);
            drawRectangleShape (W, This.Rect);
         end if;
         This.Draw_Text (W,
                         Pos + Text_Offset,
                         Key_Image (To_SFML_Evt (B)));
      end loop;
   end Draw_Buttons;

   --------------
   -- To_Color --
   --------------

   function To_Color (RGB : WNM_HAL.RGB_Rec) return Sf.Graphics.Color.sfColor
   is (Sf.Graphics.Color.fromRGB (sfUint8 (RGB.R),
       sfUint8 (RGB.G),
       sfUint8 (RGB.B)));

   ------------------
   -- Button_Color --
   ------------------

   function Button_Color (B : WNM_Configuration.Button)
                          return Sf.Graphics.Color.sfColor
   is
      use WNM.LEDs;
   begin
      case B is
         when WNM_Configuration.Play =>
            return To_Color (Hue_To_RGB (WNM.LEDs.Play));
         when Rec => return To_Color (Hue_To_RGB (Recording));
         when Step_Button => return To_Color (Hue_To_RGB (Step));
         when Track_Button => return To_Color (Hue_To_RGB (Track));
         when Pattern_Button => return To_Color (Hue_To_RGB (Pattern));
         when Chord_Button => return To_Color (Hue_To_RGB (Chord));
         when Func => return To_Color (Hue_To_RGB (FX));
         when others =>
            return Sf.Graphics.Color.sfWhite;
      end case;
   end Button_Color;

   --------------------------
   -- Draw_User_Input_Logs --
   --------------------------

   procedure Draw_User_Input_Logs (This : in out Instance;
                                   W : Sf.Graphics.sfRenderTexture_Ptr)
   is

      function Button_Img (B : WNM_Configuration.Button) return String
      is (case B is
             when Rec            => "Rec",
             when Play           => "Play",
             when Menu           => "Menu",
             when Step_Button    => "Step",
             when Track_Button   => "Track",
             when Pattern_Button => "Pattern",
             when Chord_Button   => "Chord",
             when Func           => "FX/Copy",
             when Encoder_L      => "Left Encoder",
             when Encoder_R      => "Right Encoder",
             when others => B'Img);

      function Event_Img (Evt : User_Input_Event) return String
      is (case Evt.Kind is
             when Button        => Button_Img (Evt.B),
             when Left_Encoder  => "L" & Evt.Delt'Img,
             when Right_Encoder => "R" & Evt.Delt'Img);

      Line_Height : constant := 25.0;
      Max_Line_Length : constant := 25;
      Max_Nbr_Lines : constant := 15;
      Botton : constant Float := Float (getSize (W).y) - Line_Height;
      Left : constant Float := 10.0;

      Y : Float := Botton;
      Evt : Input_Event;

      Line_Count : Natural := 0;
   begin

      --  process new events
      while not User_Input_Event_Logs.Empty loop
         User_Input_Event_Logs.Remove (Evt);
         Log_Button_Event (Evt);
      end loop;

      for Elt of reverse User_Input_List loop
         if not Elt.Is_Empty then
            declare
               First : constant User_Input_Event := Elt.First_Element;
               Color : constant Sf.Graphics.Color.sfColor :=
                 (if First.Kind = Button
                  then Button_Color (First.B)
                  else sfWhite);

               Lines : AAA.Strings.Vector;
               Str : Unbounded_String;
               Is_First : Boolean := True;
            begin
               for Evt of Elt loop
                  if Length (Str) > Max_Line_Length then
                     Lines.Append (To_String (Str));
                     Str := Null_Unbounded_String;
                  end if;

                  if not Is_First then
                     Append (Str, " + ");
                  end if;
                  Is_First := False;

                  Append (Str, Event_Img (Evt));
               end loop;

               if Length (Str) /= 0 then
                  Lines.Append (To_String (Str));
               end if;

               for Line of reverse Lines loop
                  Draw_Text (This, W, (Left, Y),
                             Line, Color,
                             Centered => False);
                  Y := Y - Line_Height;
                  Line_Count := Line_Count + 1;
               end loop;
            end;
         end if;

         if Line_Count > Max_Nbr_Lines then
            return;
         end if;
      end loop;
   end Draw_User_Input_Logs;

   ------------
   -- Update --
   ------------

   procedure Update (This : in out Instance) is
   begin
      clear (This.Window, sfBlack);

      --  Draw Data
      clear (This.Data_Panel.Render_Texture, sfBlack);
      Draw_User_Input_Logs (This, This.Data_Panel.Render_Texture);

      This.Data_Panel.Draw (This.Window);
      ------------

      --  Draw Sim
      clear (This.Sim_Panel.Render_Texture, sfBlack);
      updateFromPixels (texture => This.OLED_Texture,
                        pixels  => Frame_Buffer (Frame_Buffer'First)'Access,
                        width   => Screen_Width,
                        height  => Screen_Height,
                        x       => 0,
                        y       => 0);

      This.Draw_LEDS (This.Sim_Panel.Render_Texture);
      drawSprite (This.Sim_Panel.Render_Texture, This.BG_Sprite);
      drawSprite (This.Sim_Panel.Render_Texture, This.OLED_Sprite);
      This.Draw_Buttons (This.Sim_Panel.Render_Texture);

      This.Sim_Panel.Draw (This.Window);
      -----------

      --  Draw Menu
      ASFML_SIM_Menu.Render (This.Menu_Panel.Render_Texture, This.Font);
      This.Menu_Panel.Draw (This.Window);
      -------------

      display (This.Window);
   end Update;

   --------------
   -- Set_View --
   --------------

   procedure Update_View (This : in out Instance) is
      Render_Width : constant Float :=
        Float (This.Sim_Panel.Size.x) + (if This.Show_Data_Panel
                                         then Float (This.Data_Panel.Size.x)
                                         else 0.0);
      Render_Height : constant Float := Float (This.Sim_Panel.Size.y);

   begin

      setSize (This.Letter_Box_View,
               (Render_Width, Render_Height));

      setCenter (This.Letter_Box_View,
                 (Render_Width / 2.0, Render_Height / 2.0));

      declare
         Window_Size : constant sfVector2u := getSize (This.Window);
         Width : constant Float := Float (Window_Size.x);
         Height : constant Float := Float (Window_Size.y);

         Win_Ratio  : constant Float := Width / Height;
         View_Ratio : constant Float :=
           getSize (This.Letter_Box_View).x / getSize (This.Letter_Box_View).y;

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

         setViewport (This.Letter_Box_View, (Pos_X, Pos_Y, Size_X, Size_Y));
      end;

      setView (This.Window, This.Letter_Box_View);
   end Update_View;

   ------------
   -- Resize --
   ------------

   procedure Resize (This : in out Instance) is

      --  Size of the rendering area
      Width  : constant Float := Float (BG_Width);
      Height : constant Float := Float (BG_Height);

      Data_Target_W : constant Float := Width / 3.0;
      Data_Target_H : constant Float := Height;

      Sim_Target_W : constant Float := Width;
      Sim_Target_H : constant Float := Height;
   begin

      This.Data_Panel.Resize (X          => Sim_Target_W,
                              Y          => 0.0,
                              Width      => Data_Target_W,
                              Height     => Data_Target_H,
                              Keep_Ratio => True,
                              Center     => False);

      This.Sim_Panel.Resize (X      => 0.0,
                             Y      => 0.0,
                             Width  => Sim_Target_W,
                             Height => Sim_Target_H,
                             Keep_Ratio => True,
                             Center     => True);

      This.Menu_Panel.Resize (X => Sim_Target_W / 2.0 - Menu_Size.x / 2.0,
                              Y => Sim_Target_H / 2.0 - Menu_Size.y / 2.0,
                              Width => Menu_Size.x,
                              Height => Menu_Size.y,
                              Keep_Ratio => True);

      Update_View (This);
   end Resize;

   -----------------------
   -- Toggle_Side_Panel --
   -----------------------

   procedure Toggle_Side_Panel (This : in out Instance) is
   begin
      This.Show_Data_Panel := not This.Show_Data_Panel;
      This.Resize;
   end Toggle_Side_Panel;

   ---------------
   -- Show_Menu --
   ---------------

   procedure Show_Menu (This : in out Instance; Show : Boolean := True) is
   begin
      This.Menu_Panel.Set_Enable (Show);
   end Show_Menu;

   ----------------
   -- Poll_Event --
   ----------------

   function Poll_Event (This  : in out Instance;
                        Event : in out Sf.Window.Event.sfEvent)
                        return Boolean
   is
   begin
      return Boolean
        (Sf.Graphics.RenderWindow.pollEvent (This.Window, Event));
   end Poll_Event;

   -----------
   -- Close --
   -----------

   procedure Close (This : in out Instance) is
   begin
      close (This.Window);
   end Close;

   ----------------
   -- Screenshot --
   ----------------

   procedure Screenshot (This : in out Instance;
                         Path :        String)
   is
      Size : constant Sf.System.Vector2.sfVector2u :=
        Sf.Graphics.RenderWindow.getSize (This.Window);

      Tex : constant Sf.Graphics.sfTexture_Ptr :=
        Sf.Graphics.Texture.create (Size.x, Size.y);

      Img : Sf.Graphics.sfImage_Ptr;
   begin
      Sf.Graphics.Texture.updateFromRenderWindow
        (Tex, This.Window, 0, 0);

      Img := Sf.Graphics.Texture.copyToImage (Tex);

      if not Sf.Graphics.Image.saveToFile (Img, Path) then
         raise Program_Error with "Cannot save screenshot...";
      end if;

      Sf.Graphics.Image.destroy (Img);
      Sf.Graphics.Texture.destroy (Tex);
   end Screenshot;

   ----------------
   -- Set_Enable --
   ----------------

   procedure Set_Enable (This : in out Panel; Enable : Boolean := True) is
   begin
      This.Enabled := Enable;
   end Set_Enable;

   ----------
   -- Draw --
   ----------

   procedure Draw (This : in out Panel;
                   Window : Sf.Graphics.sfRenderWindow_Ptr)
   is
   begin
      if This.Enabled then
         display (This.Render_Texture);
         drawSprite (Window, This.Sprite);
      end if;
   end Draw;

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Panel; Width, Height : sfUint32) is
   begin
      This.Render_Texture := create (Width, Height, False);
      if This.Render_Texture = null then
         Put_Line ("Could not create render texture");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      setRepeated (This.Render_Texture, True);
      setSmooth (This.Render_Texture, True);

      This.Sprite := create;
      if This.Sprite = null then
         Put_Line ("Could not create sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      setTexture (This.Sprite, getTexture (This.Render_Texture));
   end Init;

   -----------
   -- Reize --
   -----------

   procedure Resize (This : in out Panel;
                     X, Y, Width, Height : Float;
                     Center     : Boolean := False;
                     Keep_Ratio : Boolean := False)
   is
      Texture_Size : constant sfVector2u := getSize (This.Render_Texture);

      Texture_W    : constant Float := Float (Texture_Size.x);
      Texture_H    : constant Float := Float (Texture_Size.y);

      W_Scale : Float := Width / Texture_W;
      H_Scale : Float := Height / Texture_H;

      Min_Scale : constant Float := Float'Min (W_Scale, H_Scale);

      New_Width, New_Height : Float;
      Pos_X  : Float := 0.0;
      Pos_Y  : Float := 0.0;
   begin

      if Keep_Ratio then
         W_Scale := Min_Scale;
         H_Scale := Min_Scale;
      end if;

      New_Width := W_Scale * Texture_W;
      New_Height := H_Scale * Texture_H;

      Pos_X := X;
      Pos_Y := Y;

      if Center then
         Pos_X := Pos_X + ((Width - New_Width) / 2.0);
         Pos_Y := Pos_Y + ((Height - New_Height) / 2.0);
      end if;

      W_Scale := New_Width / Texture_W;
      H_Scale := New_Height / Texture_H;

      setScale (This.Sprite, (W_Scale, H_Scale));
      setPosition (This.Sprite, (Pos_X, Pos_Y));
   end Resize;

   ----------
   -- Size --
   ----------

   function Size (This : Panel) return Sf.System.Vector2.sfVector2u
   is (getSize (This.Render_Texture));

end ASFML_Sim.Window;
