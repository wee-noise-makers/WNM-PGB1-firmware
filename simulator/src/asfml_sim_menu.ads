with Sf.System.Vector2;
with Sf.Window.Event;
with Sf.Graphics;

private with Ada.Strings.Unbounded;
private with AAA.Strings;

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
     (BG_Width / 2.0, BG_Height / 2.0);

   Menu_Pos : constant Sf.System.Vector2.sfVector2f := (100.0, 100.0);

   New_ROM : constant String := "New ROM";
   ROM_Dir : constant String := "/home/chouteau/.config/wnm-ps1/";
   ROM_Ext : constant String := ".wnm_rom";

   Error_Message : Ada.Strings.Unbounded.Unbounded_String;

   ROMs : AAA.Strings.Vector;
   Selected_ROM : Natural := 0;

   function "+" (A, B : Sf.System.Vector2.sfVector2f)
                 return Sf.System.Vector2.sfVector2f
   is ((A.x + B.x, A.y + B.y));

   function "-" (A, B : Sf.System.Vector2.sfVector2f)
                 return Sf.System.Vector2.sfVector2f
   is ((A.x - B.x, A.y - B.y));

end ASFML_SIM_Menu;
