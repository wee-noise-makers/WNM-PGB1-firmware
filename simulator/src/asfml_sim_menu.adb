with Sf.Graphics; use Sf.Graphics;

with Sf.Graphics.Text;
with Sf.Graphics.Color;

with Sf.Window.Event; use Sf.Window.Event;
with Sf.Window.Keyboard; use Sf.Window.Keyboard;
with Sf.Graphics.RenderWindow; use Sf.Graphics.RenderWindow;
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

   procedure Render (Window : Sf.Graphics.sfRenderWindow_Ptr;
                     Font : Sf.Graphics.sfFont_Ptr)
   is
      use Sf.Graphics.Text;
   begin
      --  Background
      setOutlineColor (Rect, Sf.Graphics.Color.sfWhite);
      setFillColor (Rect, Sf.Graphics.Color.sfBlack);
      setOutlineThickness (Rect, 4.0);
      setSize (Rect, Menu_Size);
      setPosition (Rect, Menu_Pos);
      drawRectangleShape (Window, Rect);

      --  Title
      setPosition (Text, Menu_Pos + (5.0, 5.0));
      setCharacterSize (Text, 20);
      setFont (Text, Font);
      setString (Text, "WNM-PS1 Simulator Menu");
      setColor (Text, Sf.Graphics.Color.sfWhite);
      drawText (Window, Text);

      --  Error Message
      if Ada.Strings.Unbounded.Length (Error_Message) /= 0 then
         setPosition (Text, Menu_Pos + (10.0, Menu_Size.y - 100.0));
         setCharacterSize (Text, 12);
         setFont (Text, Font);
         setString (Text, "Error: " &
                      Ada.Strings.Unbounded.To_String (Error_Message));
         setColor (Text, Sf.Graphics.Color.sfRed);
         drawText (Window, Text);
      end if;

      --  Help
      setPosition (Text, Menu_Pos + (10.0, Menu_Size.y - 15.0));
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

end ASFML_SIM_Menu;
