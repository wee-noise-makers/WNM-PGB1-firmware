-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2021 Fabien Chouteau                  --
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

with Ada.Synchronous_Task_Control;

with WNM_Sim;

package body WNM.Buttons is

   Key_State : array (Button) of Raw_Button_State := (others => Up);

   To_SFML_Key : constant array (Button) of WNM_Sim.SFML_Keys
     := (B1 => WNM_Sim.B1,
         B2 => WNM_Sim.B2,
         B3 => WNM_Sim.B3,
         B4 => WNM_Sim.B4,
         B5 => WNM_Sim.B5,
         B6 => WNM_Sim.B6,
         B7 => WNM_Sim.B7,
         B8 => WNM_Sim.B8,
         B9 => WNM_Sim.B9,
         B10 => WNM_Sim.B10,
         B11 => WNM_Sim.B11,
         B12 => WNM_Sim.B12,
         B13 => WNM_Sim.B13,
         B14 => WNM_Sim.B14,
         B15 => WNM_Sim.B15,
         B16 => WNM_Sim.B16,
         Rec => WNM_Sim.Rec,
         Play => WNM_Sim.Play,
         Menu => WNM_Sim.Menu,
         Func => WNM_Sim.Func,
         Step_Button => WNM_Sim.Step_Button,
         Track_Button => WNM_Sim.Track_Button,
         Pattern_Button => WNM_Sim.Pattern_Button,
         Encoder_L => WNM_Sim.Encoder_L,
         Encoder_R => WNM_Sim.Encoder_R);

   ----------
   -- Scan --
   ----------

   procedure Scan is
   begin
      for B in Button loop
         Key_State (B) :=
           (if WNM_Sim.SFML_Pressed (To_SFML_Key (B))
            or else
            WNM_Sim.Force_Pressed (B)
            then Down else Up);
      end loop;

      Ada.Synchronous_Task_Control.Set_True (WNM_Sim.Button_Scan_Signal);
   end Scan;

   -----------
   -- State --
   -----------

   function State (B : Button) return Raw_Button_State
   is (Key_State (B));

   ----------------
   -- Is_Pressed --
   ----------------

   function Is_Pressed (B : Button) return Boolean
   is (Key_State (B) = Down);


   ----------
   -- Left --
   ----------

   function Left_Diff return Integer is
      Res : constant Integer := WNM_Sim.Encoder_Left;
   begin
      WNM_Sim.Encoder_Left := 0;
      return Res;
   end Left_Diff;

   -----------
   -- Right --
   -----------

   function Right_Diff return Integer is
      Res : constant Integer := WNM_Sim.Encoder_Right;
   begin
      WNM_Sim.Encoder_Right := 0;
      return Res;
   end Right_Diff;

end WNM.Buttons;
