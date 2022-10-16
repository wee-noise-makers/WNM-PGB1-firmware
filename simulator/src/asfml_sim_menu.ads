with Sf.Window.Event;
with Sf.Graphics;
with Sf.System.Vector2;

private with Ada.Strings.Unbounded;

package ASFML_SIM_Menu is

   type Event_Result is (None, Quit, Resume);

   function Event_Handler (Event : Sf.Window.Event.sfEvent)
                           return Event_Result;

   procedure Render (Window : Sf.Graphics.sfRenderWindow_Ptr;
                     Font : Sf.Graphics.sfFont_Ptr);

private

   BG_Width : constant := 1236.0;
   BG_Height : constant := 804.0;

   Menu_Size : constant Sf.System.Vector2.sfVector2f :=
     (350.0, 170.0);

   Menu_Pos : constant Sf.System.Vector2.sfVector2f :=
     ((BG_Width - Menu_Size.x) / 2.0,
      (BG_Height - Menu_Size.y) / 2.0);

   Error_Message : Ada.Strings.Unbounded.Unbounded_String;

end ASFML_SIM_Menu;
