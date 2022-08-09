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

with WNM.GUI.Menu.Sample_Src_Select;
with WNM.GUI.Menu.Recording;
with WNM.GUI.Menu.Sample_Trim;
with WNM.GUI.Menu.Text_Dialog;
with WNM.GUI.Menu.Assign_To_Track;
with WNM.GUI.Menu.Yes_No_Dialog;
with WNM.GUI.Menu.FM_Settings;
with WNM.Sample_Stream;
with WNM.Synth;

package body WNM.GUI.Menu.Create_Sample is

   Create_Sample_Singleton : aliased Create_Sample_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Create_Sample_Singleton'Access);
   end Push_Window;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Create_Sample_Menu)
   is
   begin
      This.State := Select_Source;
      This.Sample_Entry := Invalid_Sample_Entry;
      This.Input_Before := WNM.Synth.Get_Passthrough;
      Sample_Src_Select.Push_Window;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus
     (This       : in out Create_Sample_Menu;
      Exit_Value : Window_Exit_Value)
   is
      New_State : Create_Sample_State;
      use WNM.Audio;
   begin

      --  Transition to the new state
      case This.State is
         when Select_Source =>

            if Exit_Value = Success then
               New_State := Rec_In_Progress;

               --  if WNM.Synth.Get_Passthrough = Audio.FM then
               --     New_State := FM_Tune;
               --  else
               --     New_State := Rec_In_Progress;
               --  end if;
               New_State := Rec_In_Progress;
            else
               WNM.Synth.Set_Passthrough (This.Input_Before);
               Menu.Pop (Exit_Value);
               return;
            end if;

         when FM_Tune =>
            if Exit_Value = Success then
               New_State := Rec_In_Progress;
            else
               New_State := Select_Source;
            end if;

         when Rec_In_Progress =>
            if Exit_Value = Success then
               WNM.Synth.Set_Passthrough (This.Input_Before);
               New_State := Trim;
            else
               New_State := Select_Source;
            end if;

         when Trim =>

            if Exit_Value = Success then
               New_State := Enter_Name;
            else
               New_State := Select_Source;
            end if;

         when Enter_Name =>

            if Exit_Value = Success then
               New_State := Confirm;
            else
               New_State := Trim;
            end if;

         when Confirm =>

            if Exit_Value = Success then
               declare
                  --  New_Sample_Path : constant String :=
                  --    Root_Samples_Path & Text_Dialog.Value;
                  --  Unref : Boolean with Unreferenced;
               begin
                  --  --  Copy sample with it's new name
                  --  WNM.Sample_Stream.Copy_File (Sample_Rec_Filepath,
                  --                               Menu.Sample_Trim.Start,
                  --                               Menu.Sample_Trim.Stop,
                  --                               New_Sample_Path);
                  --
                  --  --  Add it to the library
                  --  This.Sample_Entry := Add_Sample (Text_Dialog.Value);
                  New_State := Assign_To_Track;
               end;
            else
               New_State := Trim;
            end if;

         when Assign_To_Track =>
            --  if Exit_Value = Success
            --    and then
            --      This.Sample_Entry /= Invalid_Sample_Entry
            --  then
            --     WNM.Synth.Assign_Sample
            --       (Menu.Assign_To_Track.Selected_Track,
            --        Sample_Library.Entry_Path (This.Sample_Entry));
            --  end if;
            Menu.Exit_Menu;
            return;
      end case;

      This.State := New_State;

      --  Push the next window
      case New_State is
         when Select_Source =>
            Sample_Src_Select.Push_Window;
         when FM_Tune =>
            FM_Settings.Push_Window;
         when Rec_In_Progress =>
            Synth.Start_Recording
              (Filename => Sample_Rec_Filepath,
               Source   => Sample_Src_Select.Src,
               Max_Size => 332000 * 10 * 2);
            Recording.Push_Window;
         when Trim =>
            Sample_Trim.Push_Window;
         when Confirm =>
            Yes_No_Dialog.Push_Window;
         when Enter_Name =>
            Text_Dialog.Set_Title ("Sample name?");
            Text_Dialog.Push_Window;
         when Assign_To_Track =>
            Menu.Assign_To_Track.Push_Window;
      end case;

   end On_Focus;

end WNM.GUI.Menu.Create_Sample;
