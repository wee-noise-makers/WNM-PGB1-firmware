-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2023 Fabien Chouteau                    --
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

with Interfaces;

with Tresses.DSP;

with WNM.Tasks;

with HAL; use HAL;
with RP.GPIO;
with RP.Timer;
with RP.Device;
with RP.Multicore;
with RP.Multicore.FIFO;
with RP.DMA;

with Noise_Nugget_SDK.WS2812;
with Noise_Nugget_SDK.Audio;
with Noise_Nugget_SDK.Button_Matrix_Definition;
with Noise_Nugget_SDK.Button_Matrix;
with Noise_Nugget_SDK.Screen.SSD1306;

with Cortex_M.Systick;

package body WNM_HAL is

   package BM_Definition is new Noise_Nugget_SDK.Button_Matrix_Definition
     (Row_Count    => 6,
      Column_Count => 5);

   package BM is new Noise_Nugget_SDK.Button_Matrix
     (Definition => BM_Definition,
      Column_Pins => (1 => 15,
                      2 => 20,
                      3 => 26,
                      4 => 28,
                      5 => 29),
      Row_Pins    => (1 => 21,
                      2 => 22,
                      3 => 23,
                      4 => 24,
                      5 => 25,
                      6 => 27)
     );

   B_Play_Power : RP.GPIO.GPIO_Point := (Pin => 14);

   Enable_Indicator_IO : constant Boolean := True;
   Indicator_IO : array (Indicator_IO_Line) of
     RP.GPIO.GPIO_Point := (GP16 => (Pin => 16),
                            GP17 => (Pin => 17),
                            GP18 => (Pin => 18),
                            GP19 => (Pin => 19));


   package Screen is new Noise_Nugget_SDK.Screen.SSD1306
     (SPI         => RP.Device.SPI_1'Access,
      DMA_Trigger => RP.DMA.SPI1_TX,
      N_Reset_Pin => 13,
      DC_Pin      => 12,
      SCK_Pin     => 10,
      MOSI_Pin    => 11);

   ----------
   -- LEDs --
   ----------

   package LED_Strip is new Noise_Nugget_SDK.WS2812
     (Pin => 5,
      Number_Of_LEDs => LED_Position'Length);

   procedure Systick;
   pragma Export (ASM, Systick, "isr_systick");

   -------------
   -- Systick --
   -------------

   procedure Systick is
   begin
      WNM.Tasks.Sequencer_1khz_Tick;
   end Systick;

   -----------
   -- State --
   -----------

   function State return Buttons_State is
   begin
      BM.Update;
      declare
         IO : constant BM_Definition.Button_Pressed_Array := BM.State;
         S_PAD_Left       : constant Boolean := IO (1, 1);
         S_Track_Button   : constant Boolean := IO (1, 2);
         S_Step_Button    : constant Boolean := IO (1, 3);
         S_Rec            : constant Boolean := IO (1, 4);
         S_PAD_A          : constant Boolean := IO (1, 5);
         S_Func           : constant Boolean := IO (1, 6);

         S_PAD_Down       : constant Boolean := IO (2, 1);
         S_B1             : constant Boolean := IO (2, 2);
         S_B9             : constant Boolean := IO (2, 3);
         S_B16            : constant Boolean := IO (2, 4);
         S_B8             : constant Boolean := IO (2, 5);
         S_Pattern_Button : constant Boolean := IO (2, 6);

         S_PAD_Up         : constant Boolean := IO (3, 1);
         S_B2             : constant Boolean := IO (3, 2);
         S_B10            : constant Boolean := IO (3, 3);
         S_B15            : constant Boolean := IO (3, 4);
         S_B7             : constant Boolean := IO (3, 5);
         S_Chord_Button   : constant Boolean := IO (3, 6);

         S_PAD_Right      : constant Boolean := IO (4, 1);
         S_B3             : constant Boolean := IO (4, 2);
         S_B11            : constant Boolean := IO (4, 3);
         S_B14            : constant Boolean := IO (4, 4);
         S_B6             : constant Boolean := IO (4, 5);
         S_Menu           : constant Boolean := IO (4, 6);

         S_B4             : constant Boolean := IO (5, 2);
         S_B12            : constant Boolean := IO (5, 3);
         S_B13            : constant Boolean := IO (5, 4);
         S_B5             : constant Boolean := IO (5, 5);
         S_PAD_B          : constant Boolean := IO (5, 6);

         S_Play           : constant Boolean := not B_Play_Power.Set;
      begin

         return
           (B1             => (if S_B1 then Down else Up),
            B2             => (if S_B2 then Down else Up),
            B3             => (if S_B3 then Down else Up),
            B4             => (if S_B4 then Down else Up),
            B5             => (if S_B5 then Down else Up),
            B6             => (if S_B6 then Down else Up),
            B7             => (if S_B7 then Down else Up),
            B8             => (if S_B8 then Down else Up),
            B9             => (if S_B9 then Down else Up),
            B10            => (if S_B10 then Down else Up),
            B11            => (if S_B11 then Down else Up),
            B12            => (if S_B12 then Down else Up),
            B13            => (if S_B13 then Down else Up),
            B14            => (if S_B14 then Down else Up),
            B15            => (if S_B15 then Down else Up),
            B16            => (if S_B16 then Down else Up),
            Rec            => (if S_Rec then Down else Up),
            Play           => (if S_Play then Down else Up),
            Menu           => (if S_Menu then Down else Up),
            Func           => (if S_Func then Down else Up),
            Step_Button    => (if S_Step_Button then Down else Up),
            Track_Button   => (if S_Track_Button then Down else Up),
            Pattern_Button => (if S_Pattern_Button then Down else Up),
            Chord_Button   => (if S_Chord_Button then Down else Up),
            PAD_Up         => (if S_PAD_Up then Down else Up),
            PAD_Down       => (if S_PAD_Down then Down else Up),
            PAD_Left       => (if S_PAD_Left then Down else Up),
            PAD_Right      => (if S_PAD_Right then Down else Up),
            PAD_A          => (if S_PAD_A then Down else Up),
            PAD_B          => (if S_PAD_B then Down else Up));
      end;
   end State;

   ------------------
   -- Left_Encoder --
   ------------------

   function Left_Encoder return Integer is
   begin
      return 0;
   end Left_Encoder;

   -------------------
   -- Right_Encoder --
   -------------------

   function Right_Encoder return Integer is
   begin
      return 0;
   end Right_Encoder;

   ---------
   -- Set --
   ---------

   procedure Set (L : LED; RGB : RGB_Rec) is
   begin
      LED_Strip.Set_RGB (LED_Strip.LED_Id (LED_Position (L)),
                         RGB.R / 6, RGB.G / 6, RGB.B / 6);
   end Set;

   ----------------
   -- Clear_LEDs --
   ----------------

   procedure Clear_LEDs is
   begin
      LED_Strip.Clear;
   end Clear_LEDs;

   -----------------
   -- Update_LEDs --
   -----------------

   procedure Update_LEDs is
   begin
      LED_Strip.Update;
   end Update_LEDs;

   ------------------
   -- Clear_Pixels --
   ------------------

   procedure Clear_Pixels
     renames Screen.Clear;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (X : Pix_X; Y : Pix_Y; On : Boolean := True) is
   begin
      Screen.Set_Pixel (Screen.Pix_X (X), Screen.Pix_Y (Y), On => On);
   end Set_Pixel;

   -------------------
   -- Update_Screen --
   -------------------

   procedure Update_Screen
     renames Screen.Update;

   ------------------------
   -- Select_Audio_Input --
   ------------------------

   procedure Select_Audio_Input (Kind : Audio_Input_Kind)
   is null;

   ---------------------
   -- Set_Main_Volume --
   ---------------------

   procedure Set_Main_Volume (Volume : Audio_Volume) is
      Value : constant Float :=
        Float (Volume) / Float (Audio_Volume'Last);

      SDK_Value : constant Noise_Nugget_SDK.Audio.Audio_Volume :=
        Noise_Nugget_SDK.Audio.Audio_Volume (Value);

   begin
      Noise_Nugget_SDK.Audio.Set_HP_Volume (SDK_Value, SDK_Value);
   end Set_Main_Volume;

   ---------
   -- Mix --
   ---------


   procedure Mix (Out_L, Out_R : in out Mono_Buffer;
                  Input        :        Mono_Buffer;
                  Volume       :        Audio_Volume;
                  Pan          :        Audio_Pan)
   is
      Volume_F : constant Float := Float (Volume) / Float (Audio_Volume'Last);

      procedure Point_Mix (P_Out : in out Mono_Point;
                           P_In  :        Mono_Point)
      is
         use Interfaces;

         Res : constant Integer_32 :=
           Integer_32 (P_Out) + Integer_32 (P_In) / 2;
      begin

         if Res > Integer_32 (Mono_Point'Last) then
            P_Out := Mono_Point'Last;
         elsif Res < Integer_32 (Mono_Point'First) then
            P_Out := Mono_Point'First;
         else
            P_Out := Mono_Point (Res);
         end if;
      end Point_Mix;
   begin

      for Idx in Out_L'Range loop
         Point_Mix (Out_L (Idx), Input (Idx));
         Point_Mix (Out_R (Idx), Input (Idx));
      end loop;
   end Mix;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (Ms : Natural) return Time_Microseconds
   is (Time_Microseconds (Ms) * 1000);

   -----------
   -- Clock --
   -----------

   function Clock return Time_Microseconds
   is (Time_Microseconds (RP.Timer.Clock));

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Deadline : Time_Microseconds) is
   begin
      RP.Device.Timer.Delay_Until (RP.Timer.Time (Deadline));
   end Delay_Until;

   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   procedure Delay_Milliseconds (Ms : HAL.UInt64) is
   begin
      RP.Device.Timer.Delay_Milliseconds (Integer (Ms));
   end Delay_Milliseconds;

   ------------------------
   -- Delay_Microseconds --
   ------------------------

   procedure Delay_Microseconds (Us : HAL.UInt64) is
   begin
      RP.Device.Timer.Delay_Microseconds (Integer (Us));
   end Delay_Microseconds;

   -------------
   -- Storage --
   -------------

   function Get_LFS_Config return access Littlefs.LFS_Config
   is (null);

   ----------------------
   -- Sample_Data_Base --
   ----------------------

   Plop_Sample : Integer;
   pragma Import (C, Plop_Sample, "test_sample_1");

   function Sample_Data_Base return System.Address
   is (Plop_Sample'Address);
   --  is (System.Storage_Elements.To_Address
   --      (WNM_Configuration.Storage.Sample_Library_Base_Addr));

   ----------
   -- Push --
   ----------

   procedure Push (Target : Coproc_Target;
                   D      : Coproc_Data)
   is
      pragma Unreferenced (Target);
      --  Target is meaningless in the device code since a CPU can only push
      --  to the FIFO of the other CPU.
   begin


      RP.Multicore.FIFO.Push_Blocking (UInt32 (D));
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Target  : Coproc_Target;
                  D       : out Coproc_Data;
                  Success : out Boolean)
   is
      pragma Unreferenced (Target);
      --  Target is meaningless in the device code since a CPU can only pop
      --  from its own FIFO.
   begin
      Success := RP.Multicore.FIFO.Try_Pop (UInt32 (D));
   end Pop;

   ---------------
   -- Send_MIDI --
   ---------------

   procedure Send_MIDI (Data : System.Storage_Elements.Storage_Array)
   is null;

   -----------
   -- Power --
   -----------

   procedure Power_Down is
   begin
      B_Play_Power.Configure (RP.GPIO.Output, RP.GPIO.Pull_Down);
      B_Play_Power.Clear;
   end Power_Down;

   ----------------------
   -- Set_Indicator_IO --
   ----------------------

   procedure Set_Indicator_IO (Id : Indicator_IO_Line) is
   begin
      if Enable_Indicator_IO then
         Indicator_IO (Id).Set;
      end if;
   end Set_Indicator_IO;

   ------------------------
   -- Clear_Indicator_IO --
   ------------------------

   procedure Clear_Indicator_IO (Id : Indicator_IO_Line) is
   begin
      if Enable_Indicator_IO then
         Indicator_IO (Id).Clear;
      end if;
   end Clear_Indicator_IO;


begin
   B_Play_Power.Configure (RP.GPIO.Input, RP.GPIO.Pull_Up);

   --  1kHz tick
   Cortex_M.Systick.Configure
     (Cortex_M.Systick.CPU_Clock,
      Generate_Interrupt => True,
      Reload_Value       => UInt24 ((133_000_000 / 1_000) - 1));

   Cortex_M.Systick.Enable;

   if Enable_Indicator_IO then
      for Id in Indicator_IO_Line loop
         Indicator_IO (Id).Configure (RP.GPIO.Output);
         Indicator_IO (Id).Clear;
      end loop;
   end if;
end WNM_HAL;
