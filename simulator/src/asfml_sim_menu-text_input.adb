with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Sf.Graphics.RenderWindow;
with Sf.Window.Event; use Sf.Window.Event;
with Sf.Window.Keyboard; use Sf.Window.Keyboard;
with Sf.Graphics; use Sf.Graphics;
with Sf.Graphics.Text;
with Sf.Graphics.Color;

package body ASFML_SIM_Menu.Text_Input is

   Input : Unbounded_String;
   Text : constant Sf.Graphics.sfText_Ptr := Sf.Graphics.Text.create;

   -------------------
   -- Event_Handler --
   -------------------

   function Event_Handler (Event : Sf.Window.Event.sfEvent)
                           return Event_Result
   is
      procedure Append (C : Character) is
      begin
         if Length (Input) < 15 then
            Append (Input, C);
         end if;
      end Append;
   begin
      if Event.eventType in sfEvtKeyPressed then
         case Event.key.code is

            when sfKeyA .. sfKeyZ =>
               if Event.key.shift then
                  Append ((Character'Val
                          (Character'Pos ('A') +
                               Event.key.code - sfKeyA)));
               else
                  Append ((Character'Val
                             (Character'Pos ('a') +
                                  Event.key.code - sfKeyA)));
               end if;

            when sfKeyNum0 .. sfKeyNum9 =>
               Append ((Character'Val
                       (Character'Pos ('0') +
                            Event.key.code - sfKeyNum0)));

            when sfKeySpace =>
               Append (' ');

            when sfKeyBack | sfKeyDelete =>
               if Length (Input) > 0 then
                  Delete (Input, Length (Input), Length (Input));
               end if;

            when sfKeyEnter =>
               return Resume;

            when sfKeyEscape =>
               return Quit;

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
      setPosition (Text, Menu_Pos + (0.0, 50.0) +
                   (5.0, 20.0));

      setCharacterSize (Text, 15);
      setFont (Text, Font);
      setString (Text, "> " & To_String (Input));
      setColor (Text, Sf.Graphics.Color.sfWhite);
      Sf.Graphics.RenderWindow.drawText (Window, Text);
   end Render;

   ------------
   -- Result --
   ------------

   function Result return String
   is (To_String (Input));

end ASFML_SIM_Menu.Text_Input;
