-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2022 Fabien Chouteau                  --
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

with WNM.Project;
with HAL;

package WNM.GUI.Menu.Chord_Settings is

   procedure Push_Window;

private

   type Part_Top_Settings is (Part_Main);
   function Part_Top_Settings_Count is new Enum_Count (Part_Top_Settings);

   subtype Part_Sub_Settings is WNM.Project.User_Part_Settings;
   function Part_Sub_Settings_Count is new Enum_Count (Part_Sub_Settings);

   type Chord_Top_Settings is (Prog_Edit, Add_Remove, Magic_Hat);
   function Chord_Top_Settings_Count is new Enum_Count (Chord_Top_Settings);

   subtype Chord_Sub_Settings is WNM.Project.User_Chord_Settings;
   function Chord_Sub_Settings_Count is new Enum_Count (Chord_Sub_Settings);

   type Pattern_Settings_Menu is new Menu_Window with record

      --  Song Parts
      Current_Part_Setting : Part_Sub_Settings := Part_Sub_Settings'First;
      Selected : Tracks := Tracks'First;

      --  Chord Progression
      Selected_Chord : Project.Chord_Slot_Id := Project.Chord_Slot_Id'First;
      Current_Chord_Setting : Chord_Sub_Settings := Chord_Sub_Settings'First;
      Top_Chord_Setting : Chord_Top_Settings := Chord_Top_Settings'First;

      Hat_Scale : Project.Scale_Choice := Project.Random;

      Hat_Anim_Step : HAL.UInt32 := 0;
   end record;

   overriding
   procedure Draw (This : in out Pattern_Settings_Menu);

   overriding
   procedure On_Event (This  : in out Pattern_Settings_Menu;
                       Event : Menu_Event);

   overriding
   procedure On_Pushed (This  : in out Pattern_Settings_Menu);

   overriding
   procedure On_Focus (This       : in out Pattern_Settings_Menu;
                       Exit_Value : Window_Exit_Value);

end WNM.GUI.Menu.Chord_Settings;
