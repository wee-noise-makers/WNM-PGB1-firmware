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

with STM32.GPIO;   use STM32.GPIO;
with STM32.Device; use STM32.Device;

package body WNM.Buttons is

   type Row_Index is range 1 .. 3;
   type Col_Index is range 1 .. 11;

   Row_To_Point : constant array (Row_Index) of access GPIO_Point :=
     (1 => PB12'Access,
      2 => PD14'Access,
      3 => PA2'Access);

   Col_To_Point : constant array (Col_Index) of access GPIO_Point :=
     (1  => PC4'Access,
      2  => PB0'Access,
      3  => PE7'Access,
      4  => PE9'Access,
      5  => PE11'Access,
      6  => PE13'Access,
      7  => PE15'Access,
      8  => PB11'Access,
      9  => PB14'Access,
      10 => PB15'Access,
      11 => PD10'Access);

   type Key_Address is record
      Row : Row_Index;
      Col : Col_Index;
   end record;

   Key_To_Address : constant array (Button) of Key_Address :=
     (B1           => (Row => 1, Col =>  2),
      B2           => (Row => 1, Col =>  3),
      B3           => (Row => 1, Col =>  4),
      B4           => (Row => 1, Col =>  5),
      B5           => (Row => 1, Col =>  6),
      B6           => (Row => 1, Col =>  7),
      B7           => (Row => 1, Col =>  8),
      B8           => (Row => 1, Col =>  9),
      B9           => (Row => 2, Col =>  2),
      B10          => (Row => 2, Col =>  3),
      B11          => (Row => 2, Col =>  4),
      B12          => (Row => 2, Col =>  5),
      B13          => (Row => 2, Col =>  6),
      B14          => (Row => 2, Col =>  7),
      B15          => (Row => 2, Col =>  8),
      B16          => (Row => 2, Col =>  9),
      Rec          => (Row => 2, Col => 10),
      Play         => (Row => 1, Col =>  1),  -- Play is actually not connected to the matrix...
      Menu         => (Row => 1, Col => 10),
      Func         => (Row => 2, Col => 11),
      Track_Button => (Row => 1, Col =>  1),
      Pattern      => (Row => 2, Col =>  1),
      Encoder_L    => (Row => 3, Col =>  1),
      Encoder_R    => (Row => 3, Col =>  3));

   Wakeup : GPIO_Point renames PA0;

   Key_State : array (Button) of Raw_Button_State := (others => Up);

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Config : GPIO_Port_Configuration;
   begin

      Config.Speed := Speed_2MHz;

      -- Buttons --

      Config.Mode        := Mode_In;
      Config.Output_Type := Push_Pull;
      Config.Resistors   := Pull_Down;

      for Pt of Col_To_Point loop
         Enable_Clock (Pt.all);
         Pt.Configure_IO (Config);
      end loop;

      Config.Mode        := Mode_Out;
      Config.Resistors   := Pull_Up;

      for Pt of Row_To_Point loop
         Enable_Clock (Pt.all);
         Pt.Configure_IO (Config);
         Pt.Clear;
      end loop;

      Enable_Clock (Wakeup);
      Config.Mode        := Mode_In;
      Config.Resistors   := Floating;
      Wakeup.Configure_IO (Config);
   end Initialize;

   ----------
   -- Scan --
   ----------

   procedure Scan is
   begin
      for Row in Row_Index loop
         Row_To_Point (Row).Set;
         for B in Button loop
            if B /= Play and then Key_To_Address (B).Row = Row then
               Key_State (B) :=
                 (if Col_To_Point (Key_To_Address (B).Col).Set then Down else Up);
            end if;
         end loop;
         Row_To_Point (Row).Clear;
      end loop;

      Key_State (Play) := (if Wakeup.Set then Down else Up);
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

begin
   Initialize;
end WNM.Buttons;
