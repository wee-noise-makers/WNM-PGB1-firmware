with Sf.Window.Event;
with Sf.Graphics;

private package ASFML_SIM_Menu.Text_Input is

   function Event_Handler (Event : Sf.Window.Event.sfEvent)
                           return Event_Result;

   procedure Render (Window : Sf.Graphics.sfRenderWindow_Ptr;
                     Font : Sf.Graphics.sfFont_Ptr);

   function Result return String;

end ASFML_SIM_Menu.Text_Input;
