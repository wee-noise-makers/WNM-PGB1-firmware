-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2023 Fabien Chouteau                  --
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

private with WNM.Project;
private with HAL;

package WNM.GUI.Menu.Magic_Hat is

   procedure Push_Window;

private

   type Top_Settings is (Magic_Hat, Progression);

   function Top_Settings_Count is new Enum_Count (Top_Settings);

   type Sub_Settings is (Hat_Scale, Hat_Key, Prog_Scale, Prog_Key, Prog_Id);

   function Sub_Settings_Count is new Enum_Count (Sub_Settings);

   package Sub_Settings_Next is new Enum_Next (Sub_Settings);
   use Sub_Settings_Next;

   type Instance is new Menu_Window with record
      Current_Setting : Sub_Settings :=  Prog_Scale; -- Sub_Settings'First;

      Hat_Scale : Project.Scale_Choice := Project.Random;
      Hat_Key : Project.Key_Choice := Project.Random;
      Hat_Anim_Step : HAL.UInt32 := 0;

      Prog_Scale : Project.Valid_Scale_Choice :=
        Project.Valid_Scale_Choice'First;
      Prog_Key   : MIDI.MIDI_Key := MIDI.C3;
      Prog_Id : HAL.UInt16 := 0;
   end record;

   overriding
   procedure Draw (This   : in out Instance);

   overriding
   procedure On_Event (This  : in out Instance;
                       Event : Menu_Event);

   overriding
   procedure On_Pushed (This  : in out Instance) is null;

   overriding
   procedure On_Focus (This       : in out Instance;
                       Exit_Value : Window_Exit_Value) is null;

end WNM.GUI.Menu.Magic_Hat;
