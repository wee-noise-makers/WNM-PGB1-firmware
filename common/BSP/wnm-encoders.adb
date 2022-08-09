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

with HAL;          use HAL;
with STM32.Device; use STM32.Device;
with STM32.GPIO;   use STM32.GPIO;
with STM32.Timers; use STM32.Timers;

package body  WNM.Encoders is

   Point_L_A : constant GPIO_Point := PA6;
   Point_L_B : constant GPIO_Point := PA7;

   Point_R_A : constant GPIO_Point := PA5;
   Point_R_B : constant GPIO_Point := PA1;

   Timer_L : Timer renames Timer_3;
   Timer_R : Timer renames Timer_2;

   AF_L : constant STM32.GPIO_Alternate_Function := GPIO_AF_TIM3_2;
   AF_R : constant STM32.GPIO_Alternate_Function := GPIO_AF_TIM2_1;

   Capture_Filter : constant Timer_Input_Capture_Filter := 6;

   Last_Count_L : Integer := 0;
   Last_Count_R : Integer := 0;

   procedure Initialize;

   ----------
   -- Left --
   ----------

   function Left_Diff return Integer is
      Now : constant Integer := -Integer (Current_Counter (Timer_L));
      Ret : constant Integer := Now - Last_Count_L;
   begin
      Last_Count_L := Now;
      return Ret;
   end Left_Diff;

   -----------
   -- Right --
   -----------

   function Right_Diff return Integer is
      Now : constant Integer := -Integer (Current_Counter (Timer_R));
      Ret : constant Integer := Now - Last_Count_R;
   begin
      Last_Count_R := Now;
      return Ret;
   end Right_Diff;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Config : GPIO_Port_Configuration;
   begin
      Enable_Clock (Point_L_A);
      Enable_Clock (Point_L_B);
      Enable_Clock (Point_R_A);
      Enable_Clock (Point_R_B);

      Enable_Clock (Timer_L);
      Enable_Clock (Timer_R);

      Config.Mode        := Mode_AF;
      Config.Output_Type := Push_Pull;
      Config.Resistors   := Pull_Up;
      Config.Speed       := Speed_2MHz;

      Point_L_A.Configure_IO (Config);
      Point_L_B.Configure_IO (Config);
      Point_R_A.Configure_IO (Config);
      Point_R_B.Configure_IO (Config);

      Configure_Alternate_Function (Point_L_A, AF_L);
      Configure_Alternate_Function (Point_L_B, AF_L);
      Configure_Alternate_Function (Point_R_A, AF_R);
      Configure_Alternate_Function (Point_R_B, AF_R);

      Configure_Encoder_Interface
        (Timer_L,
         Mode         => Encoder_Mode_TI1,
         IC1_Polarity => Rising,
         IC2_Polarity => Rising);

      Configure_Encoder_Interface
        (Timer_R,
         Mode         => Encoder_Mode_TI1,
         IC1_Polarity => Rising,
         IC2_Polarity => Rising);

      Configure
        (Timer_L,
         Prescaler     => 0,
         Period        => UInt32 (UInt16'Last),
         Clock_Divisor => Div1,
         Counter_Mode  => Up);

      Configure
        (Timer_R,
         Prescaler     => 0,
         Period        => UInt32 (UInt16'Last),
         Clock_Divisor => Div1,
         Counter_Mode  => Up);

      Configure_Channel_Input
        (Timer_L,
         Channel   => Channel_1,
         Polarity  => Rising,
         Selection => Direct_TI,
         Prescaler => Div1,
         Filter    => Capture_Filter);

      Configure_Channel_Input
        (Timer_L,
         Channel   => Channel_2,
         Polarity  => Rising,
         Selection => Direct_TI,
         Prescaler => Div1,
         Filter    => Capture_Filter);

      Configure_Channel_Input
        (Timer_R,
         Channel   => Channel_1,
         Polarity  => Rising,
         Selection => Direct_TI,
         Prescaler => Div1,
         Filter    => Capture_Filter);

      Configure_Channel_Input
        (Timer_R,
         Channel   => Channel_2,
         Polarity  => Rising,
         Selection => Direct_TI,
         Prescaler => Div1,
         Filter    => Capture_Filter);

      Enable_Channel (Timer_L, Channel_1);
      Enable_Channel (Timer_L, Channel_2);
      Enable_Channel (Timer_R, Channel_1);
      Enable_Channel (Timer_R, Channel_2);

      Set_Counter (Timer_L, UInt16'(0));
      Set_Counter (Timer_R, UInt16'(0));

      Enable (Timer_L);
      Enable (Timer_R);
   end Initialize;

begin
   Initialize;
end WNM.Encoders;
