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
private with Enum_Next;

package WNM.GUI.Menu.Track_Mixer is

   procedure Push_Window;

private

   subtype Mixer_Settings is Project.User_Track_Settings
   range Project.Volume .. Project.Pan;

   package Mixer_Settings_Next is new Enum_Next (Mixer_Settings);
   use Mixer_Settings_Next;

   function Mixer_Settings_Count is new Enum_Count (Mixer_Settings);

   subtype Mixer_Track is WNM.Tracks range 1 .. 8;
   package Mixer_Track_Next is new Enum_Next (Mixer_Track);

   use Mixer_Track_Next;

   type Instance is new Menu_Window with record
      Setting : Mixer_Settings := Mixer_Settings'First;
      Selected_T : Mixer_Track := Mixer_Track'First;
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

end WNM.GUI.Menu.Track_Mixer;
