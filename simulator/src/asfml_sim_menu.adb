with Ada.Directories;
with Ada.Text_IO;

with Sf.Graphics; use Sf.Graphics;

with Sf.Graphics.Text;
with Sf.Graphics.Color;

with Sf.Window.Event; use Sf.Window.Event;
with Sf.Window.Keyboard; use Sf.Window.Keyboard;
with Sf.Graphics.RenderWindow; use Sf.Graphics.RenderWindow;
with Sf.Graphics.RectangleShape; use Sf.Graphics.RectangleShape;

with Enum_Next;

with ASFML_SIM_Storage;

with ASFML_SIM_Menu.Save_ROM;
with ASFML_SIM_Menu.Text_Input;

package body ASFML_SIM_Menu is

   type Menu_Step is (Load_ROM, Paused, Select_ROM_To_Save, ROM_Name_Input);
   Step : Menu_Step := Menu_Step'First;

   Text : constant Sf.Graphics.sfText_Ptr := Sf.Graphics.Text.create;
   Rect : constant sfRectangleShape_Ptr := create;

   type Top_Menu_Item is (Resume, Save_ROM_K, Quit);
   package Top_Menu_Item_Next is new Enum_Next (Top_Menu_Item, Wrap => True);
   use Top_Menu_Item_Next;

   Top_Selected : Top_Menu_Item := Top_Menu_Item'First;

   -------------------
   -- Event_Handler --
   -------------------

   function Event_Handler (Event : Sf.Window.Event.sfEvent)
                           return Event_Result
   is
   begin
      if Event.eventType in sfEvtKeyPressed then
         case Step is
            when Load_ROM =>
               case Event.key.code is

               when sfKeyEscape =>
                  return Quit;

               when sfKeyDown =>
                  if Selected_ROM < ROMs.Last_Index then
                     Selected_ROM := Selected_ROM + 1;
                  end if;

               when sfKeyUp =>
                  if Selected_ROM > ROMs.First_Index then
                     Selected_ROM := Selected_ROM - 1;
                  end if;

               when sfKeyEnter =>

                  if ROMs (Selected_ROM) = New_ROM then
                     declare
                        use Ada.Strings.Unbounded;
                        Result : constant String :=
                          ASFML_SIM_Storage.Create_ROM;
                     begin
                        if Result /= "" then
                           Error_Message := To_Unbounded_String (Result);
                           return None;
                        else
                           Step := Paused;
                           return Resume;
                        end if;
                     end;
                  else
                     declare
                        use Ada.Strings.Unbounded;
                        Result : constant String :=
                          ASFML_SIM_Storage.Load_ROM
                            (ROM_Dir & "/" & ROMs (Selected_ROM) & ROM_Ext);
                     begin
                        if Result /= "" then
                           Error_Message := To_Unbounded_String (Result);
                           return None;
                        else
                           Step := Paused;
                           return Resume;
                        end if;
                     end;
                  end if;
               when others =>
                  null;
               end case;

            when Paused =>
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
                     when Save_ROM_K =>
                        Step := Select_ROM_To_Save;
                  end case;
               when others =>
                  null;
               end case;

            when Select_ROM_To_Save =>
               case ASFML_SIM_Menu.Save_ROM.Event_Handler (Event) is
                  when None =>
                     null;

                  when Quit =>
                     Step := Paused;

                  when Resume =>
                     if ROMs (Selected_ROM) = New_ROM then
                        Step := ROM_Name_Input;
                     else
                        ASFML_SIM_Menu.Save_ROM.Save_ROM_To
                          (ROMs (Selected_ROM));
                        Step := Paused;
                     end if;
               end case;

            when ROM_Name_Input =>
               case ASFML_SIM_Menu.Text_Input.Event_Handler (Event) is
                  when None =>
                     null;

                  when Quit =>
                     Step := Select_ROM_To_Save;

                  when Resume =>
                     ASFML_SIM_Menu.Save_ROM.Save_ROM_To
                       (ASFML_SIM_Menu.Text_Input.Result);
                     Step := Paused;
               end case;
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
         setString (Text, "Load error: " &
                      Ada.Strings.Unbounded.To_String (Error_Message));
         setColor (Text, Sf.Graphics.Color.sfRed);
         drawText (Window, Text);
      end if;

      --  Help
      setPosition (Text, Menu_Pos + (10.0, Menu_Size.y - 15.0));
      setCharacterSize (Text, 12);
      setFont (Text, Font);
      setString (Text, "(Keyboard arrows to navigate, Enter to select,"
                 & " Esc to quit)");
      setColor (Text, Sf.Graphics.Color.sfWhite);
      drawText (Window, Text);

      case Step is
         when Load_ROM =>
            for Idx in ROMs.First_Index .. ROMs.Last_Index loop
               setPosition (Text, Menu_Pos + (0.0, 50.0) +
                            (5.0, 20.0 * Float (Idx)));

               setCharacterSize (Text, 15);
               setFont (Text, Font);
               setString (Text, (if Selected_ROM = Idx
                          then "> "
                          else "  ") & ROMs (Idx));
               setColor (Text, Sf.Graphics.Color.sfWhite);
               drawText (Window, Text);
            end loop;

         when Paused =>
            for Elt in Top_Menu_Item loop
               setPosition (Text, Menu_Pos + (0.0, 50.0) +
                            (5.0, 20.0 * Float (Elt'Enum_Rep)));

               setCharacterSize (Text, 15);
               setFont (Text, Font);
               setString (Text, (if Top_Selected = Elt
                          then "> "
                          else "  ") & Elt'Img);
               setColor (Text, Sf.Graphics.Color.sfWhite);
               drawText (Window, Text);
            end loop;

         when Select_ROM_To_Save =>
            ASFML_SIM_Menu.Save_ROM.Render (Window, Font);

         when ROM_Name_Input =>
            ASFML_SIM_Menu.Text_Input.Render (Window, Font);
      end case;

   end Render;

   ---------------------
   -- Add_To_ROM_List --
   ---------------------

   procedure Add_To_ROM_List
     (Directory_Entry : Ada.Directories.Directory_Entry_Type)
   is
      Simple_Name : constant String :=
        Ada.Directories.Simple_Name (Directory_Entry);
   begin
      Ada.Text_IO.Put_Line ("Found ROM at: " &
                              Ada.Directories.Full_Name (Directory_Entry));

      ROMs.Append (AAA.Strings.Head (Simple_Name, ROM_Ext));
   end Add_To_ROM_List;

begin
   ROMs.Append (New_ROM);

   Ada.Directories.Create_Path (ROM_Dir);
   Ada.Directories.Search (ROM_Dir,
                           Pattern => "*" & ROM_Ext,
                           Filter => (Ada.Directories.Ordinary_File => True,
                                      others => False),
                           Process =>  Add_To_ROM_List'Access);

   Selected_ROM := ROMs.First_Index;

end ASFML_SIM_Menu;
