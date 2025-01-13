-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                    Copyright (C) 2022 Fabien Chouteau                     --
--                                                                           --
--    Wee Noise Maker is free software: you can redistribute it and/or       --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Maker is distributed in the hope that it will be useful,     --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with We Noise Maker. If not, see <http://www.gnu.org/licenses/>. --
--                                                                           --
-------------------------------------------------------------------------------

with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Persistent;

package body WNM.GUI.Menu.Project_Select is

   package Valid_Prj_Index_Next is new Enum_Next (T    => Valid_Prj_Index,
                                                  Wrap => False);
   use Valid_Prj_Index_Next;

   Project_Select : aliased Project_Select_Window;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window (Show_Empty : Boolean := True) is
   begin
      Project_Select.Show_Empty := Show_Empty;
      Push (Project_Select'Access);
   end Push_Window;

   --------------
   -- Selected --
   --------------

   function Selected return Valid_Prj_Index
   is (Persistent.Data.Last_Project);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Project_Select_Window)
   is
   begin
      Draw_Menu_Box ("Select project", 0, 0);
      Draw_Project_Select (Persistent.Data.Last_Project, This.Show_Empty);
   end Draw;

   ---------------
   -- Find_Prev --
   ---------------

   procedure Find_Prev is
      Candidate : constant Prj_Index :=
        Find_Prev_Available (Persistent.Data.Last_Project);
   begin
      if Candidate /= Invalid_Prj_Entry then
         Persistent.Data.Last_Project := Candidate;
      end if;
   end Find_Prev;

   ---------------
   -- Find_Next --
   ---------------

   procedure Find_Next is
      Candidate : constant Prj_Index :=
        Find_Next_Available (Persistent.Data.Last_Project);
   begin
      if Candidate /= Invalid_Prj_Entry then
         Persistent.Data.Last_Project := Candidate;
      end if;
   end Find_Next;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Project_Select_Window;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when A_Press =>
            Menu.Pop (Exit_Value => Success);

         when B_Press =>
            Menu.Pop (Exit_Value => Failure);

         when Down_Press =>
            if This.Show_Empty then
               Next (Persistent.Data.Last_Project);
            else
               Find_Next;
            end if;

         when Up_Press =>
            if This.Show_Empty then
               Prev (Persistent.Data.Last_Project);
            else
               Find_Prev;
            end if;

         when Right_Press =>
            null;

         when Left_Press =>
            null;

         when Slider_Touch =>
            Set (Persistent.Data.Last_Project, Event.Slider_Value);

      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Project_Select_Window)
   is
   begin
      --  Find a project that is not empty
      if not This.Show_Empty then

         Find_Prev;

         if not Has_Project (Persistent.Data.Last_Project) then
            Find_Next;
         end if;
      end if;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding procedure On_Focus
     (This       : in out Project_Select_Window;
      Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Project_Select;
