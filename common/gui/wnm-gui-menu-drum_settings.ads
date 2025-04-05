-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2017 Fabien Chouteau                  --
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

package WNM.GUI.Menu.Drum_Settings is

   procedure Push_Window;

private

   package Next_Track is new Enum_Next (WNM.Project.Drum_Tracks);
   use Next_Track;

   type Mode_Kind is (Step_Edit, Len_Edit);

   type Synth_Track_Id is (BD, SD, HH, SP);

   type Synth_Settings is
     (Engine, Param_A, Param_B, Param_C, Param_D, Master_FX);

   type Synth_Top_Settings is (Engine, Params, Master_FX);

   function Top_Settings_Count is new Enum_Count (Synth_Top_Settings);
   function Synth_Track_Count is new Enum_Count (Synth_Track_Id);

   type Instance is new Menu_Window with record
      Mode : Mode_Kind := Step_Edit;
      Selected_Step : WNM.Pattern_Length := WNM.Pattern_Length'First;
      Selected_Track : WNM.Project.Drum_Tracks :=
        WNM.Project.Drum_Tracks'First;
   end record;

   overriding
   procedure Draw (This : in out Instance);

   overriding
   procedure On_Event (This  : in out Instance;
                       Event : Menu_Event);

   overriding
   procedure On_Pushed (This  : in out Instance);

   overriding
   procedure On_Focus (This       : in out Instance;
                       Exit_Value : Window_Exit_Value);

end WNM.GUI.Menu.Drum_Settings;
