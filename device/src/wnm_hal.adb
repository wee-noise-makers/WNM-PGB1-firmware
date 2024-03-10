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

with Ada.Unchecked_Conversion;
with Interfaces;
with System.Machine_Code;
with System.Storage_Elements;

with WNM.Tasks;
with WNM.GUI.Bitmap_Fonts;
with WNM.Coproc;

with HAL; use HAL;
with RP.GPIO;
with RP.Timer;
with RP.Device;
with RP.Multicore;
with RP.Multicore.FIFO;
with RP.Multicore.Spinlocks;
with RP.DMA;
with RP.ROM;
with RP.Watchdog;
with RP2040_SVD.Interrupts;

with Noise_Nugget_SDK.WS2812;
with Noise_Nugget_SDK.Audio;
with Noise_Nugget_SDK.Button_Matrix_Definition;
with Noise_Nugget_SDK.Button_Matrix;
with Noise_Nugget_SDK.Screen.SSD1306;
with Noise_Nugget_SDK.MIDI;

with Atomic.Critical_Section;

with Cortex_M.Systick;
with Cortex_M.Debug;

with WNM_HAL.File_System;
with RP.Flash;

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

   package External_MIDI is new Noise_Nugget_SDK.MIDI
     (UART           => RP.Device.UART_1'Access,
      UART_Interrupt => RP2040_SVD.Interrupts.UART1_Interrupt,
      DMA_TX_Trigger => RP.DMA.UART1_TX,
      TX_Pin         => 8,
      RX_Pin         => 9);

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);

   procedure ISR_Hard_Fault;
   pragma Export (ASM, ISR_Hard_Fault, "isr_hardfault");

   procedure ISR_NMI;
   pragma Export (ASM, ISR_NMI, "isr_nmi");

   procedure ISR_Invalid;
   pragma Export (ASM, ISR_Invalid, "isr_invalid");

   procedure Breakpoint_If_Debug;

   Synth_CPU_LCH_Msg  : System.Address := System.Null_Address
     with Volatile, Atomic;
   Synth_CPU_LCH_Line : Integer := 0
     with Volatile, Atomic;

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

   ------------------------
   -- Set_Line_In_Volume --
   ------------------------

   procedure Set_Line_In_Volume (Volume : Audio_Volume) is
      Value : constant Float :=
        Float (Volume) / Float (Audio_Volume'Last);

      SDK_Value : constant Noise_Nugget_SDK.Audio.Audio_Volume :=
        Noise_Nugget_SDK.Audio.Audio_Volume (Value);

   begin
      Noise_Nugget_SDK.Audio.Set_Line_Volume (2, SDK_Value, SDK_Value);
   end Set_Line_In_Volume;

   -----------------------------
   -- Set_Internal_Mic_Volume --
   -----------------------------

   procedure Set_Mic_Volumes (Headset, Internal : Audio_Volume) is
      HS_Value : constant Float :=
        Float (Headset) / Float (Audio_Volume'Last);

      HS_SDK_Value : constant Noise_Nugget_SDK.Audio.Audio_Volume :=
        Noise_Nugget_SDK.Audio.Audio_Volume (HS_Value);

      Internal_Value : constant Float :=
        Float (Internal) / Float (Audio_Volume'Last);

      Internal_SDK_Value : constant Noise_Nugget_SDK.Audio.Audio_Volume :=
        Noise_Nugget_SDK.Audio.Audio_Volume (Internal_Value);

   begin
      Noise_Nugget_SDK.Audio.Set_Mic_Boost
        (Internal_SDK_Value,
         HS_SDK_Value);
   end Set_Mic_Volumes;

   ----------------------------
   -- Set_Headset_Mic_Volume --
   ----------------------------

   procedure Set_Headset_Mic_Volume (Volume : Audio_Volume) is
      Value : constant Float :=
        Float (Volume) / Float (Audio_Volume'Last);

      SDK_Value : constant Noise_Nugget_SDK.Audio.Audio_Volume :=
        Noise_Nugget_SDK.Audio.Audio_Volume (Value);

   begin
      Noise_Nugget_SDK.Audio.Set_Mic_Boost (SDK_Value, SDK_Value);
   end Set_Headset_Mic_Volume;

   ---------
   -- Mix --
   ---------

   procedure Mix (Out_L, Out_R : in out Mono_Buffer;
                  Input        :        Mono_Buffer;
                  Volume       :        Audio_Volume;
                  Pan          :        Audio_Pan)
   is
      use Tresses;
      use Interfaces;

      procedure Point_Mix (P_Out    : in out Mono_Point;
                           P_In     :        Mono_Point;
                           Chan_Vol :        S16)
      is

         Sample : constant S32 := (S32 (P_In) * S32 (Chan_Vol)) / 2**15;
         Res    : constant S32 := S32 (P_Out) + Sample;
      begin

         if Res > S32 (Mono_Point'Last) then
            P_Out := Mono_Point'Last;
         elsif Res < S32 (Mono_Point'First) then
            P_Out := Mono_Point'First;
         else
            P_Out := Mono_Point (Res);
         end if;
      end Point_Mix;

      Pan_R_S16 : constant S16 :=
        (S16'Last / S16 (Audio_Pan'Last)) * S16 (Pan);

      Pan_L_S16 : constant S16 :=
        (S16'Last / S16 (Audio_Pan'Last)) * S16 (Audio_Pan'Last - Pan);

      Vol_S16 : constant S16 :=
        (S16'Last / S16 (Audio_Volume'Last)) * S16 (Volume);

      Vol_R_S16 : constant S16 :=
        S16 ((S32 (Vol_S16) * S32 (Pan_R_S16)) / 2**15);

      Vol_L_S16 : constant S16 :=
        S16 ((S32 (Vol_S16) * S32 (Pan_L_S16)) / 2**15);
   begin

      for Idx in Out_L'Range loop
         Point_Mix (Out_L (Idx), Input (Idx), Vol_L_S16);
         Point_Mix (Out_R (Idx), Input (Idx), Vol_R_S16);
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

   --------------------------
   -- Start_Sequencer_Tick --
   --------------------------

   procedure Start_Sequencer_Tick is
   begin
      --  1kHz tick
      Cortex_M.Systick.Configure
        (Cortex_M.Systick.CPU_Clock,
         Generate_Interrupt => True,
         Reload_Value       => UInt24 ((133_000_000 / 1_000) - 1));

      Cortex_M.Systick.Enable;
   end Start_Sequencer_Tick;

   -------------
   -- Storage --
   -------------

   function Get_LFS_Config return access Littlefs.LFS_Config
   is (WNM_HAL.File_System.Get_LFS_Config);

   ----------------------
   -- Sample_Data_Base --
   ----------------------

   function Sample_Data_Base return System.Address
   is (System.Storage_Elements.To_Address
       (WNM_Configuration.Storage.Sample_Library_Base_Addr));

   ----------------------
   -- Write_To_Stroage --
   ----------------------

   procedure Write_To_Storage (Id   : Sample_Sector_Id;
                               Data : Storage_Sector_Data)
   is
      use RP.Flash;

      Offset : constant Flash_Offset :=
        Flash_Offset
          (WNM_Configuration.Storage.Sample_Library_Offset
           + Flash_Offset (Id) * WNM_Configuration.Storage.Sector_Byte_Size);
   begin
      WNM_HAL.Wait_Synth_CPU_Hold;

      RP.Flash.Erase (Offset, Data'Length);
      RP.Flash.Program (Offset, Data'Address, Data'Length);

      WNM_HAL.Release_Synth_CPU_Hold;
   end Write_To_Storage;

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

   -------------------
   -- Send_External --
   -------------------

   procedure Send_External (Msg : MIDI.Message) is
   begin
      External_MIDI.Send (Msg);
   end Send_External;

   ------------------
   -- Flush_Output --
   ------------------

   procedure Flush_Output is
   begin
      External_MIDI.Flush_Output;
   end Flush_Output;

   ------------------
   -- Get_External --
   ------------------

   procedure Get_External (Msg : out MIDI.Message; Success : out Boolean)
   renames External_MIDI.Get_Input;

   -----------
   -- Power --
   -----------

   procedure Power_Down is
   begin
      B_Play_Power.Configure (RP.GPIO.Output, RP.GPIO.Pull_Down);
      B_Play_Power.Clear;

      loop
         Breakpoint_If_Debug;
      end loop;
   end Power_Down;

   --------------------
   -- Enter_DFU_Mode --
   --------------------

   procedure Enter_DFU_Mode is
   begin
      RP.ROM.reset_to_usb_boot (0, 0);
   end Enter_DFU_Mode;

   -------------------
   -- Watchdog_Init --
   -------------------

   procedure Watchdog_Init is
   begin
      --  RP.Watchdog.Configure (500);
      null;
   end Watchdog_Init;

   --------------------
   -- Watchdog_Check --
   --------------------

   procedure Watchdog_Check is
   begin
      --  RP.Watchdog.Reload;
      null;
   end Watchdog_Check;

   -------------------------
   -- Wait_Synth_CPU_Hold --
   -------------------------

   Hold_Request_Lock   : constant RP.Multicore.Spinlocks.Lock_Id := 0;
   Hold_Confirmed_Lock : constant RP.Multicore.Spinlocks.Lock_Id := 1;
   procedure RAM_Hold_Wait_Loop
     with No_Inline, Linker_Section => ".time_critical.synth_cpu_hold_loop";

   procedure Wait_Synth_CPU_Hold is
      use RP.Multicore.Spinlocks;
   begin

      if Locked (Hold_Confirmed_Lock) then
         raise Program_Error with "Synth CPU should be running";
      end if;

      if not Try_Lock (Hold_Request_Lock) then
         raise Program_Error with "This lock should not be contested...";
      end if;

      loop
         exit when Locked (Hold_Confirmed_Lock);
      end loop;
   end Wait_Synth_CPU_Hold;

   ----------------------------
   -- Release_Synth_CPU_Hold --
   ----------------------------

   procedure Release_Synth_CPU_Hold is
      use RP.Multicore.Spinlocks;
   begin
      Release (Hold_Request_Lock);
   end Release_Synth_CPU_Hold;

   ------------------------
   -- RAM_Hold_Wait_Loop --
   ------------------------

   procedure RAM_Hold_Wait_Loop is
      use RP.Multicore.Spinlocks;

   begin
      if not Try_Lock (Hold_Confirmed_Lock) then
         raise Program_Error with "This lock should not be contested...";
      end if;

      loop
         exit when not Locked (Hold_Request_Lock);
      end loop;

      Release (Hold_Confirmed_Lock);

   end RAM_Hold_Wait_Loop;

   --------------------------
   -- Synth_CPU_Check_Hold --
   --------------------------

   procedure Synth_CPU_Check_Hold is
      use RP.Multicore.Spinlocks;

      State : Atomic.Critical_Section.Interrupt_State;
   begin
      if Locked (Hold_Request_Lock) then

         Atomic.Critical_Section.Enter (State);

         RAM_Hold_Wait_Loop;

         Atomic.Critical_Section.Leave (State);

      end if;
   end Synth_CPU_Check_Hold;

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

   -------------------------
   -- Breakpoint_If_Debug --
   -------------------------

   procedure Breakpoint_If_Debug is
      use System.Machine_Code;
   begin
      --  If a debugger is attached, triggger an breakpoint event, otherwise
      --  do nothing and return.
      if Cortex_M.Debug.Halting_Debug_Enabled then
         Asm ("bkpt",
              Volatile => True);
      end if;
   end Breakpoint_If_Debug;

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      use System;

      CPU_Id : constant Natural := RP.Multicore.CPU_Id;

      ---------------
      -- Print_Msg --
      ---------------

      procedure Print_Msg (Str : System.Address; Y : in out Integer) is
         Message : String (1 .. 80)
           with Address => Str;
         X : Integer := 0;
      begin
         for C of Message loop
            exit when C = Character'Val (0);
            WNM.GUI.Bitmap_Fonts.Print (X, Y, C);
            if X > Screen_Width - WNM.GUI.Bitmap_Fonts.Height then
               X := 0;
               Y := Y + WNM.GUI.Bitmap_Fonts.Height;
            end if;
         end loop;
      end Print_Msg;


      ----------------
      -- Print_Code --
      ----------------

      procedure Print_Code (Code :        Integer;
                            X    : in out Integer;
                            Y    :        Integer)
      is
         function To_UInt32
         is new Ada.Unchecked_Conversion (Integer, UInt32);

         U32 : constant UInt32 := To_UInt32 (Code);
      begin
         WNM.GUI.Bitmap_Fonts.Print (X, Y, 'x');

         for Cnt in reverse 0 .. 7 loop
            WNM.GUI.Bitmap_Fonts.Print
              (X, Y,
               (case Shift_Right (U32, 4 * Cnt) and 16#F# is
                   when 0 => '0',
                   when 1 => '1',
                   when 2 => '2',
                   when 3 => '3',
                   when 4 => '4',
                   when 5 => '5',
                   when 6 => '6',
                   when 7 => '7',
                   when 8 => '8',
                   when 9 => '9',
                   when 10 => 'A',
                   when 11 => 'B',
                   when 12 => 'C',
                   when 13 => 'D',
                   when 14 => 'E',
                   when 15 => 'F',
                   when others => '-'));
         end loop;
      end Print_Code;

      X, Y : Integer;

   begin

      if CPU_Id = 1 then
         Synth_CPU_LCH_Line := Line;
         Synth_CPU_LCH_Msg  := Msg;
         WNM.Coproc.Push_To_Main ((Kind => WNM.Coproc.Synth_CPU_Crash));

         loop
            Breakpoint_If_Debug;
         end loop;
      end if;

      --  All LEDs in red
      Clear_LEDs;
      for L in LED loop
         Set (L, (255, 0, 0));
      end loop;
      Update_LEDs;

      --  Write error message on screen
      Clear_Pixels;
      X := 0;
      WNM.GUI.Bitmap_Fonts.Print (X, 0, "CPU-0 Err:");
      Print_Code (Line, X, -2);

      Y := WNM.GUI.Bitmap_Fonts.Height;
      Print_Msg (Msg, Y);

      --  Check and Write synth CPU error message on screen
      if Synth_CPU_LCH_Msg /= System.Null_Address then
         X := 0;
         Y := Y + WNM.GUI.Bitmap_Fonts.Height;
         WNM.GUI.Bitmap_Fonts.Print (X, Y, "CPU-1 Err:");
         Print_Code (Synth_CPU_LCH_Line, X, Y);

         Y := Y + WNM.GUI.Bitmap_Fonts.Height;
         Print_Msg (Synth_CPU_LCH_Msg, Y);
      end if;

      Update_Screen;

      loop
         Watchdog_Check;
         Breakpoint_If_Debug;
      end loop;
   end Last_Chance_Handler;

   ---------------------------
   -- Cortex_M_Fault_Status --
   ---------------------------

   function Cortex_M_Fault_Status return Integer is
      CFSR : Integer
        with Volatile, Address => System'To_Address (16#E000ED28#);
   begin
      return CFSR;
   end Cortex_M_Fault_Status;

   --------------------
   -- ISR_Hard_Fault --
   --------------------

   procedure ISR_Hard_Fault is
      Message : constant String := "ISR Hardfault" & ASCII.NUL;
   begin
      Last_Chance_Handler (Message'Address, Cortex_M_Fault_Status);
   end ISR_Hard_Fault;

   -------------
   -- ISR_NMI --
   -------------

   procedure ISR_NMI is
      Message : constant String := "ISR NMI" & ASCII.NUL;
   begin
      Last_Chance_Handler (Message'Address, Cortex_M_Fault_Status);
   end ISR_NMI;

   -----------------
   -- ISR_Invalid --
   -----------------

   procedure ISR_Invalid is
      Message : constant String := "ISR Invalid" & ASCII.NUL;
   begin
      Last_Chance_Handler (Message'Address, Cortex_M_Fault_Status);
   end ISR_Invalid;


begin
   B_Play_Power.Configure (RP.GPIO.Input, RP.GPIO.Pull_Up);

   if Enable_Indicator_IO then
      for Id in Indicator_IO_Line loop
         Indicator_IO (Id).Configure (RP.GPIO.Output);
         Indicator_IO (Id).Clear;
      end loop;
   end if;
end WNM_HAL;
