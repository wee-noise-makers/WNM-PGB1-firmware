with Sf.Graphics.RenderWindow;
with Sf.Window.Event; use Sf.Window.Event;
with Sf.Window.Keyboard; use Sf.Window.Keyboard;
with Sf.Graphics; use Sf.Graphics;
with Sf.Graphics.Text;
with Sf.Graphics.Color;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ASFML_SIM_Storage;

package body ASFML_SIM_Menu.Save_ROM is

   Text : constant Sf.Graphics.sfText_Ptr := Sf.Graphics.Text.create;

   -----------------
   -- Save_ROM_To --
   -----------------

   procedure Save_ROM_To (Name : String) is
   begin
      Error_Message :=
        To_Unbounded_String
          (ASFML_SIM_Storage.Save_ROM (ROM_Dir & "/" & Name & ROM_Ext));
   end Save_ROM_To;

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
               if Selected_ROM < ROMs.Last_Index then
                  Selected_ROM := Selected_ROM + 1;
               end if;

            when sfKeyUp =>
               if Selected_ROM > ROMs.First_Index then
                  Selected_ROM := Selected_ROM - 1;
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
      for Idx in ROMs.First_Index .. ROMs.Last_Index loop
         setPosition (Text, Menu_Pos + (0.0, 50.0) +
                      (5.0, 20.0 * Float (Idx)));

         setCharacterSize (Text, 15);
         setFont (Text, Font);
         setString (Text, (if Selected_ROM = Idx
                    then "> "
                    else "  ") & ROMs (Idx));
         setColor (Text, Sf.Graphics.Color.sfWhite);
         Sf.Graphics.RenderWindow.drawText (Window, Text);
      end loop;
   end Render;

end ASFML_SIM_Menu.Save_ROM;
