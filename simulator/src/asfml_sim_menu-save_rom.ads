with Sf.Window.Event;
with Sf.Graphics;

private package ASFML_SIM_Menu.Save_ROM is

   function Event_Handler (Event : Sf.Window.Event.sfEvent)
                           return Event_Result;

   procedure Render (Window : Sf.Graphics.sfRenderWindow_Ptr;
                     Font : Sf.Graphics.sfFont_Ptr);

   procedure Save_ROM_To (Name : String);

end ASFML_SIM_Menu.Save_ROM;
