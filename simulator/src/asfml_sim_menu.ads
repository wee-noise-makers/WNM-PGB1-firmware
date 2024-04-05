with Sf.Window.Event;
with Sf.Graphics;

private with Ada.Strings.Unbounded;

package ASFML_SIM_Menu is

   type Event_Result is (None, Quit, Resume);

   function Event_Handler (Event : Sf.Window.Event.sfEvent)
                           return Event_Result;

   procedure Render (Window : Sf.Graphics.sfRenderTexture_Ptr;
                     Font   : Sf.Graphics.sfFont_Ptr);

   procedure Splashscreen (Window : Sf.Graphics.sfRenderTexture_Ptr;
                           Font   : Sf.Graphics.sfFont_Ptr);

private

   Error_Message : Ada.Strings.Unbounded.Unbounded_String;

end ASFML_SIM_Menu;
