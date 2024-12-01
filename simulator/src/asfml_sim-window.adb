with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Numerics.Elementary_Functions;
with GNAT.OS_Lib;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ASFML_Sim_Resources;
with AAA.Strings;

with Interfaces.C.Strings;

with Sf.Graphics; use Sf.Graphics;
with Sf.Graphics.BlendMode;
with Sf.Window.VideoMode; use Sf.Window.VideoMode;
with Sf.Graphics.Sprite; use Sf.Graphics.Sprite;
with Sf.Graphics.Texture; use Sf.Graphics.Texture;
with Sf.Graphics.RenderTexture; use Sf.Graphics.RenderTexture;
with Sf.Graphics.View; use Sf.Graphics.View;
with Sf.Graphics.RenderWindow; use Sf.Graphics.RenderWindow;
with Sf.Graphics.RectangleShape; use Sf.Graphics.RectangleShape;
with Sf.Graphics.Vertex;
with Sf.Graphics.RenderStates;
with Sf.Graphics.VertexArray; use Sf.Graphics.VertexArray;
with Sf.Graphics.PrimitiveType;
with Sf.Graphics.Font;
with Sf.Graphics.Text;
with Sf.Graphics.Image;
with Sf.Window.Window; use Sf.Window.Window;
with Sf.Window.Event; use Sf.Window.Event;
with Sf.System.Vector2; use Sf.System.Vector2;
with Sf.Graphics.Rect; use Sf.Graphics.Rect;
with Sf.Graphics.Transform; use Sf.Graphics.Transform;

with ASFML_Util; use ASFML_Util;
with ASFML_SIM_Menu;
with ASFML_Sim.Window.Shaders;

with WNM_HAL;
with WNM.LEDs;

package body ASFML_Sim.Window is

   FFT_Skip_Bins : constant := 2;

   BG_Width : constant := 1382;
   BG_Height : constant := 900;

   Menu_Size : constant Sf.System.Vector2.sfVector2f := (350.0, 170.0);

   LED_Offset : constant array (WNM_Configuration.LED) of sfVector2f :=
     (
      Menu           => (842.0, 327.0),
      Song_Button   => (971.0, 327.0),
      Pattern_Button => (1101.0, 327.0),
      Func           => (1245.0, 327.0),

      Track_Button => (50.0,   485.0),
      B1           => (194.0,  485.0),
      B2           => (324.0,  485.0),
      B3           => (453.0,  485.0),
      B4           => (583.0,  485.0),
      B5           => (712.0,  485.0),
      B6           => (842.0,  485.0),
      B7           => (971.0,  485.0),
      B8           => (1101.0,  485.0),
      Play         => (1245.0, 485.0),

      Step_Button  => (50.0, 643.0),
      B9           => (194.0, 643.0),
      B10          => (324.0, 643.0),
      B11          => (453.0, 643.0),
      B12          => (583.0, 643.0),
      B13          => (712.0, 643.0),
      B14          => (842.0, 643.0),
      B15          => (971.0, 643.0),
      B16          => (1101.0, 643.0),
      Rec          => (1245.0, 643.0));

   Button_Rect_Size : constant sfVector2f := (95.0, 95.0);
   Button_Text_Offset : constant sfVector2f := (Button_Rect_Size.x / 2.0,
                                                Button_Rect_Size.y / 2.0);

   type User_Input_Event (Kind : Input_Event_Kind := Button) is record
      case Kind is
         when Button =>
            B : WNM_Configuration.Button;
         when Up_Down | Left_Right =>
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
         when Up_Down | Left_Right =>

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

               if B_List.Reference (B_List.Last).Delt = 0 then
                  B_List.Delete_Last;
               end if;

            else

               --  Add new encoder event
               if Evt.Kind = Up_Down then
                  B_List.Append ((Up_Down, Evt.Delt));
               else
                  B_List.Append ((Left_Right, Evt.Delt));
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
          when sfKeyA => "A",
          when sfKeyB => "B",
          when sfKeyC => "C",
          when sfKeyD => "D",
          when sfKeyE => "E",
          when sfKeyF => "F",
          when sfKeyG => "G",
          when sfKeyH => "H",
          when sfKeyI => "I",
          when sfKeyJ => "J",
          when sfKeyK => "K",
          when sfKeyL => "L",
          when sfKeyM => "M",
          when sfKeyN => "N",
          when sfKeyO => "O",
          when sfKeyP => "P",
          when sfKeyQ => "Q",
          when sfKeyR => "R",
          when sfKeyS => "S",
          when sfKeyT => "T",
          when sfKeyU => "U",
          when sfKeyV => "V",
          when sfKeyW => "W",
          when sfKeyX => "X",
          when sfKeyY => "Y",
          when sfKeyZ => "Z",
          when sfKeyEscape    => "Esc",
          when sfKeyLControl  => "L-Ctrl",
          when sfKeyLShift    => "L-Shift",
          when sfKeyLAlt      => "L-Alt",
          when sfKeyLSystem   => "L-Mod",
          when sfKeyRControl  => "R-Ctrl",
          when sfKeyRShift    => "R-Shift",
          when sfKeyRAlt      => "R-Alt",
          when sfKeyRSystem   => "R-Mod",
          when sfKeyMenu      => "Menu",
          when sfKeyLBracket  => "[",
          when sfKeyRBracket  => "]",
          when sfKeySemicolon => ";",
          when sfKeyComma     => ",",
          when sfKeyPeriod    => ".",
          when sfKeyQuote     => """",
          when sfKeySlash     => "/",
          when sfKeyBackslash => "\",
          when sfKeyTilde     => "~",
          when sfKeyEqual     => "=",
          when sfKeyHyphen    => "-",
          when sfKeySpace     => "Space",
          when sfKeyEnter     => "Enter",
          when sfKeyBack      => "<-",
          when sfKeyTab       => "Tab",
          when sfKeyPageUp    => "Page-Up",
          when sfKeyPageDown  => "Page-Down",
          when sfKeyEnd       => "End",
          when sfKeyHome      => "Home",
          when sfKeyInsert    => "Ins",
          when sfKeyDelete    => "Del",
          when sfKeyAdd       => "+",
          when sfKeySubtract  => "-",
          when sfKeyMultiply  => "*",
          when sfKeyDivide    => "/",
          when sfKeyLeft      => "Left",
          when sfKeyRight     => "Right",
          when sfKeyUp        => "Up",
          when sfKeyDown      => "Down",
          when sfKeyNum0 => "0",
          when sfKeyNum1 => "1",
          when sfKeyNum2 => "2",
          when sfKeyNum3 => "3",
          when sfKeyNum4 => "4",
          when sfKeyNum5 => "5",
          when sfKeyNum6 => "6",
          when sfKeyNum7 => "7",
          when sfKeyNum8 => "8",
          when sfKeyNum9 => "9",
          when sfKeyF1   => "F1",
          when sfKeyF2   => "F2",
          when sfKeyF3   => "F3",
          when sfKeyF4   => "F4",
          when sfKeyF5   => "F5",
          when sfKeyF6   => "F6",
          when sfKeyF7   => "F7",
          when sfKeyF8   => "F8",
          when sfKeyF9   => "F9",
          when sfKeyF10  => "F10",
          when sfKeyF11  => "F11",
          when sfKeyF12  => "F12",
          when others => "UNKNOWN KEY EVT"
      ) & "]");

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Instance) is
      Mode   : constant Sf.Window.VideoMode.sfVideoMode :=
        (BG_Width, BG_Height, 32);

      Screen_Offset : constant sfVector2f := (526.0, 41.0);
      Screen_Scale : constant :=
        (856.0 - 526.0) / Float (Screen_Width);

      Params : constant sfContextSettings := sfDefaultContextSettings;

   begin
      This.Window := create (Mode, "WNM-PGB1 Simulator",
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
        (ASFML_Sim_Resources.Resource_Path & "/WNM-PGB1-v2.1.png");

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

      --  Waveform
      This.Wave_Left := create;
      This.Wave_Right := create;
      resize (This.Wave_Left, 1024);
      resize (This.Wave_Right, getVertexCount (This.Wave_Left));
      setPrimitiveType (This.Wave_Left,
                        Sf.Graphics.PrimitiveType.sfLinesStrip);
      setPrimitiveType (This.Wave_Right,
                        Sf.Graphics.PrimitiveType.sfLinesStrip);
      for Index in 0 .. getVertexCount (This.Wave_Left) - 1 loop
         declare
            L : Sf.Graphics.Vertex.sfVertex
              renames getVertex (This.Wave_Left, Index).all;
            R : Sf.Graphics.Vertex.sfVertex
              renames getVertex (This.Wave_Right, Index).all;
         begin
            L.color := sfMagenta;
            L.position := (Float (Index), 50.0);
            R.color := sfWhite;
            R.position := (Float (Index), 50.0);
         end;
      end loop;
      ------------

      --  FFT

      This.FFT.Init (WNM_Configuration.Audio.Sample_Frequency);

      This.Wave_FFT := create;
      resize (This.Wave_FFT,
              sfSize_t (This.FFT.Window_Size / 2) - FFT_Skip_Bins);
      setPrimitiveType (This.Wave_FFT,
                        Sf.Graphics.PrimitiveType.sfLinesStrip);

      This.Wave_FFT_Peak := create;
      resize (This.Wave_FFT_Peak,
              sfSize_t (This.FFT.Window_Size / 2) - FFT_Skip_Bins);
      setPrimitiveType (This.Wave_FFT_Peak,
                        Sf.Graphics.PrimitiveType.sfLinesStrip);

      for Index in 0 .. getVertexCount (This.Wave_FFT) - 1 loop
         declare
            F : Sf.Graphics.Vertex.sfVertex
              renames getVertex (This.Wave_FFT, Index).all;
            P : Sf.Graphics.Vertex.sfVertex
              renames getVertex (This.Wave_FFT_Peak, Index).all;
         begin
            F.color := sfBlue;
            P.color := sfYellow;
         end;
      end loop;
      -------

      --  Shaders
      declare
         use Interfaces.C.Strings;
         function createFromMemory_chars_ptr
           (vertexShaderFilename : chars_ptr;
            geometryShaderFilename : chars_ptr;
            fragmentShaderFilename : chars_ptr) return sfShader_Ptr;
         pragma Import
           (C, createFromMemory_chars_ptr, "sfShader_createFromMemory");

         procedure setCurrentTextureUniform_chars_ptr
           (shader : sfShader_Ptr;
                                             name : chars_ptr);
         pragma Import
           (C, setCurrentTextureUniform_chars_ptr,
               "sfShader_setCurrentTextureUniform");
      begin
         This.Glow_Shader := createFromMemory_chars_ptr
           (Null_Ptr,
            Null_Ptr,
            New_String (ASFML_Sim.Window.Shaders.Glow));

         if This.Glow_Shader = null then
            Put_Line ("Could not create shader");
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         setCurrentTextureUniform_chars_ptr
           (This.Glow_Shader, New_String ("source"));
      end;

      -----------

      This.Rect := create;

      This.Sim_Panel.Init (BG_Width, BG_Height);
      This.Data_Panel.Init ((BG_Width / 10) * 4, BG_Height / 2,
                            This.Glow_Shader);

      This.Keylog_Panel.Init ((BG_Width / 10) * 4, BG_Height / 2);

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
      setSize (This.Rect, (90.0, 30.0));

      for L in SFML_LEDs'Range loop
         setFillColor (This.Rect, SFML_LEDs (L));
         setPosition (This.Rect, LED_Offset (L));
         drawRectangleShape (W, This.Rect);
      end loop;
   end Draw_LEDS;

   ---------------------
   -- Button_Position --
   ---------------------

   function Button_Position (B : WNM_Configuration.Button) return sfVector2f is
   begin
      case B is
         when LED_Offset'Range =>
            return LED_Offset (B) + (-4.0, 44.0);
         when PAD_Up =>
            return (171.0, 103.0);
         when PAD_Down =>
            return (171.0, 285.0);
         when PAD_Left =>
            return (78.0, 194.0);
         when PAD_Right =>
            return (262.0, 194.0);
         when PAD_A =>
            return (1123.0, 148.0);
         when PAD_B =>
            return (1031.0, 240.0);
      end case;
   end Button_Position;

   ------------------
   -- Draw_Buttons --
   ------------------

   procedure Draw_Buttons (This : in out Instance;
                           W    :        Sf.Graphics.sfRenderTexture_Ptr)
   is
   begin
      setOutlineColor (This.Rect, Sf.Graphics.Color.sfBlack);
      setOutlineThickness (This.Rect, 1.0);
      setSize (This.Rect, Button_Rect_Size);

      for B in SFML_Pressed'Range loop
         if SFML_Pressed (B) or else Force_Pressed (B) then
            setFillColor (This.Rect, Sf.Graphics.Color.sfBlue);
            setOutlineColor (This.Rect, Sf.Graphics.Color.sfTransparent);
            setPosition (This.Rect, Button_Position (B));
            if B not in LED_Offset'Range then
               setRotation (This.Rect, -45.0);
            end if;
            drawRectangleShape (W, This.Rect);
         end if;
      end loop;
      setRotation (This.Rect, 0.0);
   end Draw_Buttons;

   ------------------------
   -- Draw_Buttons_Label --
   ------------------------

   procedure Draw_Buttons_Label (This : in out Instance;
                                 W    :        Sf.Graphics.sfRenderTexture_Ptr)
   is
   begin
      for B in SFML_Pressed'Range loop
         This.Draw_Text (W,
                         Button_Position (B) + Button_Text_Offset +
                         (if B not in LED_Offset'Range
                            then (20.0, -45.0)
                            else (0.0, 0.0)),
                         Key_Image (To_SFML_Evt (B)));
      end loop;
   end Draw_Buttons_Label;

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
         when Song_Button => return To_Color (Hue_To_RGB (Chord));
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
             when Song_Button   => "Song",
             when Func           => "FX/Copy",
             when PAD_Up         => "Up",
             when PAD_Down       => "Down",
             when PAD_Left       => "Left",
             when PAD_Right      => "Right",
             when PAD_A          => "A",
             when PAD_B          => "B",
             when others => B'Img);

      function Event_Img (Evt : User_Input_Event) return String
      is (case Evt.Kind is
             when Button        => Button_Img (Evt.B),

             when Up_Down       =>
               (if Evt.Delt > 0
                then "Up" & Evt.Delt'Img
                else "Down" & Integer'Image (abs Evt.Delt)),

             when Left_Right    =>
               (if Evt.Delt > 0
                then "Right" & Evt.Delt'Img
                else "Left" & Integer'Image (abs Evt.Delt)));

      Line_Height : constant := 30.0;
      Max_Line_Length : constant := 25;
      Max_Nbr_Lines : constant := 11;
      Botton : constant Float := Float (getSize (W).y) - Line_Height - 10.0;
      Top : constant Float := Botton - Float (Max_Nbr_Lines * Line_Height);
      Left : constant Float := 10.0;

      Y : Float := Botton;
      Line_Count : Natural := 0;
   begin

      --  Keyboard Layout
      Draw_Text (This, W, (Left, Top - Float (Line_Height)),
                 "Keyboard Layout [F2]: " & Img (Current_Layout),
                 Centered => False);

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

   ----------------
   -- Update_FFT --
   ----------------

   procedure Update_FFT (This                         : in out Instance;
                         FFT_Y, FFT_Height, FFT_Width :        Float)
   is
      use Ada.Numerics.Elementary_Functions;

      subtype Display_Bins
        is Bin_Range range Bin_Range'First + FFT_Skip_Bins .. Bin_Range'Last;
   begin
      --  Compute and current energy
      for Index in Bin_Range loop
         declare
            Amp : constant Float := This.FFT.Amp (Index);
         begin
            This.Energy (Index) := Float'Max (Amp, 0.000001);
         end;
      end loop;

      --  Compute decaying peak energy
      for Index in Display_Bins loop
         declare
            Local_Average : Float := 0.0;
            Num_Points : Natural := 0;
         begin
            --  Compute local average
            for K in Integer'Max (Display_Bins'First, Index - 5) ..
              Integer'Min (Display_Bins'Last, Index + 5)
            loop
               Local_Average := Local_Average + This.Energy (K);
               Num_Points := Num_Points + 1;
            end loop;
            Local_Average := Local_Average / Float (Num_Points);

            --  Decay
            This.Peak_Energy (Index) :=
              This.Peak_Energy (Index) * 0.95;

            --  Check for new peak
            if Local_Average > This.Peak_Energy (Index) then
               This.Peak_Energy (Index) := Local_Average;
            end if;

         end;
      end loop;

      declare
         Max_Freq : constant Float :=
           This.FFT.Center_Frequency (Display_Bins'Last);
         Min_Freq : constant Float :=
           This.FFT.Center_Frequency (Display_Bins'First);

         ---------------
         -- Db_Offset --
         ---------------

         function Db_Offset (Amp : Float) return Float is
            Max_Db : constant Float := 50.0;
            Min_Db : constant Float := -40.0;

            Range_Db : constant Float := Max_Db - Min_Db;
         begin
            return
              (((20.0 * Log (Amp, 10.0)) - Min_Db) / Range_Db) * FFT_Height;
         end Db_Offset;

         -----------------
         -- Freq_Offset --
         -----------------

         function Freq_Offset (F : Float) return Float is
            Range_Freq : constant Float := abs (Max_Freq - Min_Freq);
         begin
            if F <= Min_Freq then
               return 0.0;
            end if;

            return (Log (F - Min_Freq, 2.0) / Log (Range_Freq, 2.0)) *
              (FFT_Width - 5.0);
         end Freq_Offset;

         function Offset_To_Alpha (Offset : Float) return sfUint8
         is (sfUint8 (Float'Max (0.0,
             Float'Min (255.0,
               255.0 * Offset / FFT_Height / 1.5))));

      begin

         for Index in 0 .. getVertexCount (This.Wave_FFT) - 1 loop
            declare
               Bin : constant Natural := Natural (Index) + 1 + FFT_Skip_Bins;

               Energy_Vert : Vertex.sfVertex
               renames getVertex (This.Wave_FFT, Index).all;

               Energy_Offset : constant Float := Db_Offset (This.Energy (Bin));

               Peak_Vert : Vertex.sfVertex
               renames getVertex (This.Wave_FFT_Peak, Index).all;
               Peak_Offset : constant Float :=
                 Db_Offset (This.Peak_Energy (Bin));

            begin
               Energy_Vert.position.y := FFT_Y - Energy_Offset;
               Energy_Vert.position.x :=
                 Freq_Offset (This.FFT.Center_Frequency (Bin));
               Energy_Vert.color.a := Offset_To_Alpha (Energy_Offset);

               Peak_Vert.position.y := FFT_Y - Peak_Offset;
               Peak_Vert.position.x :=
                 Freq_Offset (This.FFT.Center_Frequency (Bin));
               Peak_Vert.color.a := Offset_To_Alpha (Peak_Offset);
            end;
         end loop;
      end;
   end Update_FFT;

   -------------------
   -- Draw_Waveform --
   -------------------

   procedure Draw_Waveform (This : in out Instance;
                            W : Sf.Graphics.sfRenderTexture_Ptr)
   is
      Block : WNM_HAL.Stereo_Buffer;

      Frame_L, Frame_R : Float;

      Index : sfSize_t := 0;
      Vert_Count : constant sfSize_t := getVertexCount (This.Wave_Left);

      Wave_Height : constant Float := (Float (BG_Height) / 4.0) / 2.0;
      Wave_Amplitude : constant Float := Wave_Height / 1.0;
      L_Y : constant Float := Wave_Height / 2.0;
      R_Y : constant Float := Wave_Height + Wave_Height / 2.0;

      FFT_Width : constant Float := Float (getSize (W).x);
      FFT_Height : constant Float := Float (BG_Height) / 4.0;
      FFT_Y : constant Float := Wave_Height * 2.0 + FFT_Height;
   begin

      while not  Audio_Block_Queue.Empty and then Index < Vert_Count loop
         Audio_Block_Queue.Remove (Block);

         for S16_Frame of Block loop

            Frame_L := Float (S16_Frame.L) / Float (WNM_HAL.Mono_Point'Last);
            Frame_R := Float (S16_Frame.R) / Float (WNM_HAL.Mono_Point'Last);

            This.FFT.Push_Frame ((Frame_L + Frame_R) / 2.0);

            if Index < Vert_Count then

               getVertex (This.Wave_Left, Index).position.y :=
                 L_Y + Wave_Amplitude * Frame_L;

               getVertex (This.Wave_Right, Index).position.y :=
                 R_Y + Wave_Amplitude * Frame_R;

               Index := Index + 1;
            end if;
         end loop;
      end loop;

      Update_FFT (This, FFT_Y, FFT_Height, FFT_Width);

      drawVertexArray (W, This.Wave_Right);
      drawVertexArray (W, This.Wave_Left);
      drawVertexArray (W, This.Wave_FFT);
      drawVertexArray (W, This.Wave_FFT_Peak);
   end Draw_Waveform;

   ------------
   -- Update --
   ------------

   procedure Update (This : in out Instance) is
   begin
      clear (This.Window, sfBlack);

      --  Process user input logs
      declare
         Evt : Input_Event;
      begin
         while not User_Input_Event_Logs.Empty loop
            This.Show_Splashscreen := False;
            User_Input_Event_Logs.Remove (Evt);
            Log_Button_Event (Evt);
         end loop;
      end;

      if This.Show_Data_Panel then
         --  Draw Data
         clear (This.Data_Panel.Render_Texture, sfBlack);
         Draw_Waveform (This, This.Data_Panel.Render_Texture);
         This.Data_Panel.Draw (This.Window);
         ------------

         --  Keylog
         clear (This.Keylog_Panel.Render_Texture, sfBlack);
         Draw_User_Input_Logs (This, This.Keylog_Panel.Render_Texture);
         This.Keylog_Panel.Draw (This.Window);
         ----------
      end if;

      --  Draw Sim
      clear (This.Sim_Panel.Render_Texture, sfBlack);
      updateFromPixels (texture => This.OLED_Texture,
                        pixels  => Frame_Buffer (Frame_Buffer'First)'Access,
                        width   => Screen_Width,
                        height  => Screen_Height,
                        x       => 0,
                        y       => 0);

      --  Draw stuff behind the front panel
      This.Draw_LEDS (This.Sim_Panel.Render_Texture);
      This.Draw_Buttons (This.Sim_Panel.Render_Texture);

      --  Draw front panel
      drawSprite (This.Sim_Panel.Render_Texture, This.BG_Sprite);

      --  Draw stuff in front of the front panel
      drawSprite (This.Sim_Panel.Render_Texture, This.OLED_Sprite);
      This.Draw_Buttons_Label (This.Sim_Panel.Render_Texture);

      if This.Show_Splashscreen then
         ASFML_SIM_Menu.Splashscreen (This.Sim_Panel.Render_Texture,
                                      This.Font);
      end if;

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
      Data_Target_W : constant Float := Float (This.Data_Panel.Size.x);
      Data_Target_H : constant Float := Float (This.Data_Panel.Size.y);

      Keylog_Target_W : constant Float := Float (This.Keylog_Panel.Size.x);
      Keylog_Target_H : constant Float := Float (This.Keylog_Panel.Size.y);

      Sim_Target_W : constant Float := Float (This.Sim_Panel.Size.x);
      Sim_Target_H : constant Float := Float (This.Sim_Panel.Size.y);
   begin
      This.Data_Panel.Resize (X          => Sim_Target_W,
                              Y          => 0.0,
                              Width      => Data_Target_W,
                              Height     => Data_Target_H,
                              Keep_Ratio => True,
                              Center     => True);

      This.Keylog_Panel.Resize (X          => Sim_Target_W,
                                Y          => Data_Target_H,
                                Width      => Keylog_Target_W,
                                Height     => Keylog_Target_H,
                                Keep_Ratio => True,
                                Center     => True);

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

   --------------------------
   -- To_Touch_Point_Value --
   --------------------------

   function To_Touch_Point_Value (This  : in out Instance;
                                  Mouse_Pos_X, Mouse_Pos_Y : sfInt32)
                                  return Float
   is
      Top_Left : constant Sf.System.Vector2.sfVector2f := (407.0, 285.0);
      Bot_Right : constant Sf.System.Vector2.sfVector2f := (766.0, 399.0);

      Coords : constant sfVector2f :=
        mapPixelToCoords (This.Window,
                          (Mouse_Pos_X, Mouse_Pos_Y),
                          This.Letter_Box_View);
   begin
      if Coords.x in Top_Left.x .. Bot_Right.x
        and then
          Coords.y in Top_Left.y .. Bot_Right.y
      then
         return (Coords.x - Top_Left.x) / (Bot_Right.x - Top_Left.x);
      else
         return -1.0;
      end if;
   end To_Touch_Point_Value;

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

   ---------------------
   -- OLED_Screenshot --
   ---------------------

   procedure OLED_Screenshot (This          : in out Instance;
                              Path          :        String;
                              Black_N_White :        Boolean := False)
   is
      Img : Sf.Graphics.sfImage_Ptr;
   begin
      Img := Sf.Graphics.Texture.copyToImage (This.OLED_Texture);

      if Black_N_White then
         declare
            use Sf.Graphics.Image;
            Size : constant sfVector2u := getSize (Img);
            White : constant sfColor := fromRGB (255, 255, 255);
            Black : constant sfColor := fromRGB (0, 0, 0);
         begin
            for X in 0 .. Size.x - 1 loop
               for Y in 0 .. Size.y - 1 loop
                  if getPixel (Img, X, Y) = BG_Color then
                     setPixel (Img, X, Y, White);
                  else
                     setPixel (Img, X, Y, Black);
                  end if;
               end loop;
            end loop;
         end;
      end if;

      if not Sf.Graphics.Image.saveToFile (Img, Path) then
         raise Program_Error with "Cannot save screenshot...";
      end if;

      Sf.Graphics.Image.destroy (Img);
      --  Sf.Graphics.Texture.destroy (Tex);
   end OLED_Screenshot;

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
         declare
            RS : aliased Sf.Graphics.RenderStates.sfRenderStates
              := (texture   => null,
                  shader    => This.Shader,
                  blendMode => Sf.Graphics.BlendMode.sfBlendAlpha,
                  transform => Identity);
         begin
            display (This.Render_Texture);
            drawSprite (Window, This.Sprite, RS'Unrestricted_Access);
         end;
      end if;
   end Draw;

   ----------
   -- Init --
   ----------

   procedure Init (This          : in out Panel;
                   Width, Height :        sfUint32;
                   Shader        :        Sf.Graphics.sfShader_Ptr := null)
   is
   begin
      This.Shader := Shader;

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
