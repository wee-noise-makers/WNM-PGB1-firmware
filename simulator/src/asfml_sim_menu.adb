with Sf.Graphics; use Sf.Graphics;

with Sf.Graphics.Text;
with Sf.Graphics.Color;
with Sf.System.Vector2;
with Sf.Window.Event; use Sf.Window.Event;
with Sf.Window.Keyboard; use Sf.Window.Keyboard;
with Sf.Graphics.RenderTexture; use Sf.Graphics.RenderTexture;
with Sf.Graphics.RectangleShape; use Sf.Graphics.RectangleShape;

with ASFML_Util; use ASFML_Util;
with ASFML_SIM_Storage;

with Enum_Next;

with WNM.Power_Control;

package body ASFML_SIM_Menu is

   Text : constant Sf.Graphics.sfText_Ptr := Sf.Graphics.Text.create;
   Rect : constant sfRectangleShape_Ptr := create;

   type Top_Menu_Item is (Resume, Save_And_Resume, Save_And_Quit, Quit);
   package Top_Menu_Item_Next is new Enum_Next (Top_Menu_Item, Wrap => True);
   use Top_Menu_Item_Next;

   Top_Selected : Top_Menu_Item := Top_Menu_Item'First;

   function Img (T : Top_Menu_Item) return String
   is (case T is
          when Resume => "Resume",
          when Save_And_Resume => "Save and resume",
          when Save_And_Quit => "Save and exit",
          when Quit => "Exit");

   -------------------
   -- Event_Handler --
   -------------------

   function Event_Handler (Event : Sf.Window.Event.sfEvent)
                           return Event_Result
   is
   begin
      if Event.eventType in sfEvtKeyPressed then
         case Event.key.code is

               when sfKeyDown =>
               Next (Top_Selected);

            when sfKeyUp =>
               Prev (Top_Selected);

            when sfKeyEnter =>
               case Top_Selected is
                  when Quit =>
                     return Quit;

                  when Resume =>
                     return Resume;

                  when Save_And_Resume =>
                     declare
                        Result : constant String :=
                          ASFML_SIM_Storage.Save_ROM
                            (ASFML_SIM_Storage.ROM_Path);
                     begin
                        if Result /= "" then
                           raise Program_Error with Result;
                        end if;
                        return Resume;
                     end;

                  when Save_And_Quit =>

                     WNM.Power_Control.Power_Down;

               end case;
            when others =>
               null;
         end case;
      end if;
      return None;
   end Event_Handler;

   ------------
   -- Render --
   ------------

   procedure Render (Window : Sf.Graphics.sfRenderTexture_Ptr;
                     Font : Sf.Graphics.sfFont_Ptr)
   is
      use Sf.Graphics.Text;

      Menu_Pos : constant Sf.System.Vector2.sfVector2f := (0.0, 0.0);

      Menu_Size : constant Sf.System.Vector2.sfVector2u := getSize (Window);
      Width : constant Float := Float (Menu_Size.x);
      Height : constant Float := Float (Menu_Size.y);

   begin
      clear (Window, Sf.Graphics.Color.sfBlack);

      --  Background
      setOutlineColor (Rect, Sf.Graphics.Color.sfWhite);
      setFillColor (Rect, Sf.Graphics.Color.sfBlack);
      setOutlineThickness (Rect, 2.0);
      setSize (Rect, (Width - 4.0, Height - 4.0));
      setPosition (Rect, (Menu_Pos.x + 2.0, Menu_Pos.y + 2.0));
      drawRectangleShape (Window, Rect);

      --  Title
      setPosition (Text, Menu_Pos + (5.0, 5.0));
      setCharacterSize (Text, 20);
      setFont (Text, Font);
      setString (Text, "WNM-PGB-1 Simulator Menu");
      setColor (Text, Sf.Graphics.Color.sfWhite);
      drawText (Window, Text);

      --  Error Message
      if Ada.Strings.Unbounded.Length (Error_Message) /= 0 then
         setPosition (Text, Menu_Pos + (10.0, Width - 100.0));
         setCharacterSize (Text, 12);
         setFont (Text, Font);
         setString (Text, "Error: " &
                      Ada.Strings.Unbounded.To_String (Error_Message));
         setColor (Text, Sf.Graphics.Color.sfRed);
         drawText (Window, Text);
      end if;

      --  Help
      setPosition (Text, Menu_Pos + (10.0, Width - 15.0));
      setCharacterSize (Text, 12);
      setFont (Text, Font);
      setString (Text, "(Keyboard arrows to navigate, Enter to select)");
      setColor (Text, Sf.Graphics.Color.sfWhite);
      drawText (Window, Text);

      for Elt in Top_Menu_Item loop
         setPosition (Text, Menu_Pos + (0.0, 50.0) +
                      (5.0, 20.0 * Float (Elt'Enum_Rep)));

         setCharacterSize (Text, 15);
         setFont (Text, Font);
         setString (Text, (if Top_Selected = Elt
                    then "> "
                    else "  ") & Img (Elt));
         setColor (Text, Sf.Graphics.Color.sfWhite);
         drawText (Window, Text);
      end loop;
   end Render;

   ------------------
   -- Splashscreen --
   ------------------

   procedure Splashscreen (Window : Sf.Graphics.sfRenderTexture_Ptr;
                           Font   : Sf.Graphics.sfFont_Ptr)
   is
      use Sf.Graphics.Text;
      use Sf;

      Win_Size : constant Sf.System.Vector2.sfVector2u := getSize (Window);

      Splash_Size : constant Sf.System.Vector2.sfVector2f :=
        (Float (Win_Size.x) / 1.2, Float (Win_Size.y) / 1.5);

      Splash_Pos : constant Sf.System.Vector2.sfVector2f :=
        ((Float (Win_Size.x) - Splash_Size.x) / 2.0,
         (Float (Win_Size.y) - Splash_Size.y) / 2.0);
   begin
      --  Background
      setOutlineColor (Rect, Sf.Graphics.Color.sfWhite);
      setFillColor (Rect, Sf.Graphics.Color.sfBlack);
      setOutlineThickness (Rect, 2.0);
      setSize (Rect, Splash_Size);
      setPosition (Rect, Splash_Pos);
      drawRectangleShape (Window, Rect);

      setPosition (Text, (Splash_Pos.x + 40.0, Splash_Pos.y + 100.0));
      setCharacterSize (Text, 40);
      setFont (Text, Font);
      setString
        (Text,
         "Hello there, welcome to the PGB-1 simulator!"  & ASCII.LF &
         ASCII.LF &
         "Quick disclaimer, this program is a development" & ASCII.LF &
         "tool and does not represent the final features," & ASCII.LF &
         "sound, or performances of the PGB-1." & ASCII.LF &
         ASCII.LF &
         "https://weenoisemakers.com/pgb-1 for more info." & ASCII.LF &
         ASCII.LF &
         "Have fun :)");
      setColor (Text, Sf.Graphics.Color.sfWhite);
      drawText (Window, Text);

   end Splashscreen;

end ASFML_SIM_Menu;
