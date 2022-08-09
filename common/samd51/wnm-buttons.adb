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

with HAL.GPIO;
with HAL.I2C; use HAL.I2C;

with WNM.Samd51.Encoders;
--  with WNM.Time;

with HAL; use HAL;

package body WNM.Buttons is

   IO_Ext_Addr : constant := 16#74#;
   Reg_Input_0 : constant := 16#00#;
   Reg_Output_1 : constant := 16#03#;
   Reg_Config_0 : constant := 16#06#;
   Reg_Config_1 : constant := 16#07#;

   Col_State : UInt8_Array (1 .. 5) := (others => 0);

   procedure Write_Reg (Reg : UInt8; Val : UInt8);
   procedure Read_Reg (Reg : UInt8; Val : out UInt8);
   procedure Initialize;

   ---------------
   -- Write_Reg --
   ---------------

   procedure Write_Reg (Reg : UInt8; Val : UInt8) is
      Status : HAL.I2C.I2C_Status;
   begin
      Samd51.I2C_Port.Master_Transmit (Addr    => IO_Ext_Addr,
                                       Data    => (0 => Reg, 1 => Val),
                                       Status  => Status);

      if Status /= Ok then
         raise Program_Error;
      end if;
   end Write_Reg;

   --------------
   -- Read_Reg --
   --------------

   procedure Read_Reg (Reg : UInt8; Val : out UInt8) is
      Status : HAL.I2C.I2C_Status;
      Data : UInt8_Array (0 .. 0);
   begin
      Samd51.I2C_Port.Mem_Read (Addr          => IO_Ext_Addr,
                                Mem_Addr      => UInt16 (Reg),
                                Mem_Addr_Size => HAL.I2C.Memory_Size_8b,
                                Data          => Data,
                                Status        => Status);

      if Status /= Ok then
         raise Program_Error;
      end if;
      Val := Data (Data'First);
   end Read_Reg;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Config IO bank 0 as input
      Write_Reg (Reg_Config_0, 16#FF#); -- 1 for output

      --  Config IO bank 1 as output
      Write_Reg (Reg_Config_1, 16#00#); -- 0 for output
   end Initialize;

   ----------
   -- Scan --
   ----------

   procedure Scan is
      Col_Pin : UInt8 := 1;
   begin

      for Col of Col_State loop
         Write_Reg (Reg_Output_1, Col_Pin);

         Read_Reg (Reg_Input_0, Col);

         --  Set the next column
         Col_Pin := Shift_Left (Col_Pin, 1);
      end loop;
   end Scan;

   Row1 : constant := 2#00001#;
   Row2 : constant := 2#00010#;
   Row3 : constant := 2#00100#;
   Row4 : constant := 2#10000#;
   Row5 : constant := 2#01000#;
   -----------
   -- State --
   -----------

   function State (B : Button) return Raw_Button_State
   is (if (case B is
              when B1 => (Col_State (4) and Row4) /= 0,
              when B2 => (Col_State (3) and Row4) /= 0,
              when B3 => (Col_State (2) and Row4) /= 0,
              when B4 => (Col_State (1) and Row4) /= 0,
              when B5 => (Col_State (1) and Row2) /= 0,
              when B6 => (Col_State (2) and Row2) /= 0,
              when B7 => (Col_State (3) and Row2) /= 0,
              when B8 => (Col_State (4) and Row2) /= 0,
              when B9 => (Col_State (4) and Row5) /= 0,
              when B10 => (Col_State (3) and Row5) /= 0,
              when B11 => (Col_State (2) and Row5) /= 0,
              when B12 => (Col_State (1) and Row5) /= 0,
              when B13 => (Col_State (1) and Row3) /= 0,
              when B14 => (Col_State (2) and Row3) /= 0,
              when B15 => (Col_State (3) and Row3) /= 0,
              when B16 => (Col_State (4) and Row3) /= 0,
              when Rec => (Col_State (5) and Row3) /= 0,
              when Play => (Col_State (5) and Row2) /= 0,
              when Menu => (Col_State (4) and Row1) /= 0,
              when Func => (Col_State (5) and Row1) /= 0,
              when Track_Button => (Col_State (5) and Row4) /= 0,
              when Pattern => (Col_State (5) and Row5) /= 0,
              when Encoder_L => (Col_State (3) and Row1) /= 0,
              when Encoder_R => (Col_State (2) and Row1) /= 0)
       then Down
       else Up);

   ----------------
   -- Is_Pressed --
   ----------------

   function Is_Pressed (B : Button) return Boolean
   is (State (B) = Down);


   ----------
   -- Left --
   ----------

   function Left_Diff return Integer is
   begin
      return WNM.Samd51.Encoders.Left;
   end Left_Diff;

   -----------
   -- Right --
   -----------

   function Right_Diff return Integer is
   begin
      return WNM.Samd51.Encoders.Right;
   end Right_Diff;

begin
   Initialize;
end WNM.Buttons;
