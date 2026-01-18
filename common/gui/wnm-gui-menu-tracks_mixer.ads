-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2026 Fabien Chouteau                  --
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

private with Enum_Next;

package WNM.GUI.Menu.Tracks_Mixer is

   procedure Push_Window;

private

   subtype Top_Settings
     is Project.User_Track_Settings
     range Project.Volume .. Project.Master_FX;

   function Img (S : Top_Settings) return String
   is (case S is
          when Project.Volume    => "Volume",
          when Project.Pan       => "Pan",
          when Project.Master_FX => "FX");

   function Top_Settings_Count is new Enum_Count (Top_Settings);
   package Top_Settings_Next is new Enum_Next (Top_Settings);
   use Top_Settings_Next;

   subtype Track_Id is Tracks range 1 .. 8;
   package Track_Id_Next is new Enum_Next (Track_Id, Wrap => False);
   use Track_Id_Next;

   type Instance is new Menu_Window with record
      Current_Setting : Top_Settings := Top_Settings'First;
      Selected_Track : Track_Id := Track_Id'First;
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

end WNM.GUI.Menu.Tracks_Mixer;
