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
with RP.PIO; use RP.PIO;
with RP.Multicore;
with RP.Multicore.FIFO;
with RP.PIO.WS2812;

with WNM_HAL.OLED;
with WNM_HAL.Encoders;
with WNM_HAL.Synth_Core;

with WNM_HAL_Settings; use WNM_HAL_Settings;

with Cortex_M.Systick;

package body WNM_HAL is

   -------------
   -- Buttons --
   -------------

   type Row_Id is range 1 .. 6;
   type Col_Id is range 1 .. 5;
   type IO_State is array (Col_Id, Row_Id) of Boolean;

   B_Rows : array (Row_Id) of RP.GPIO.GPIO_Point :=
     (1 => (Pin => 25),
      2 => (Pin => 24),
      3 => (Pin => 23),
      4 => (Pin => 20),
      5 => (Pin => 21),
      6 => (Pin => 22));

   B_Cols : array (Col_Id) of RP.GPIO.GPIO_Point :=
     (1 => (Pin => 14),
      2 => (Pin => 15),
      3 => (Pin => 16),
      4 => (Pin => 17),
      5 => (Pin => 18));

   B_Play_Power : RP.GPIO.GPIO_Point := (Pin => 19);

   ----------
   -- LEDs --
   ----------

   LED_Pin : aliased RP.GPIO.GPIO_Point := (Pin => 5);

   Number_Of_LEDs : constant := LED_Position'Length;
   --  subtype LED_ID is Natural range 1 .. Number_Of_LEDs;

   LED_Strip : aliased RP.PIO.WS2812.Strip (LED_Pin'Access,
                                            WS2812_PIO'Access,
                                            WS2812_SM,
                                            Number_Of_LEDs);

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
      IO : IO_State;
   begin
      for Col in Col_Id loop
         B_Cols (Col).Set;
         for Row in Row_Id loop
            IO (Col, Row) := B_Rows (Row).Set;
         end loop;
         B_Cols (Col).Clear;
      end loop;

      declare
         S_B1             : constant Boolean := IO (2, 2);
         S_B2             : constant Boolean := IO (3, 2);
         S_B3             : constant Boolean := IO (4, 2);
         S_B4             : constant Boolean := IO (5, 2);
         S_B5             : constant Boolean := IO (5, 5);
         S_B6             : constant Boolean := IO (4, 5);
         S_B7             : constant Boolean := IO (3, 5);
         S_B8             : constant Boolean := IO (2, 5);
         S_B9             : constant Boolean := IO (2, 3);
         S_B10            : constant Boolean := IO (3, 3);
         S_B11            : constant Boolean := IO (4, 3);
         S_B12            : constant Boolean := IO (5, 3);
         S_B13            : constant Boolean := IO (5, 4);
         S_B14            : constant Boolean := IO (4, 4);
         S_B15            : constant Boolean := IO (3, 4);
         S_B16            : constant Boolean := IO (2, 4);
         S_Rec            : constant Boolean := IO (1, 4);
         S_Menu           : constant Boolean := IO (4, 6);
         S_Func           : constant Boolean := IO (1, 6);
         S_Step_Button    : constant Boolean := IO (1, 3);
         S_Track_Button   : constant Boolean := IO (1, 2);
         S_Pattern_Button : constant Boolean := IO (2, 6);
         S_Chord_Button   : constant Boolean := IO (3, 6);
         S_Encoder_L      : constant Boolean := IO (1, 1);
         S_Encoder_R      : constant Boolean := IO (2, 1);

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
            Encoder_L      => (if S_Encoder_L then Down else Up),
            Encoder_R      => (if S_Encoder_R then Down else Up));
      end;
   end State;

   ------------------
   -- Left_Encoder --
   ------------------

   function Left_Encoder return Integer
     renames WNM_HAL.Encoders.Left;

   -------------------
   -- Right_Encoder --
   -------------------

   function Right_Encoder return Integer
     renames WNM_HAL.Encoders.Right;

   ---------
   -- Set --
   ---------

   procedure Set (L : LED; RGB : RGB_Rec) is
   begin
      LED_Strip.Set_RGB (LED_Position (L), RGB.R, RGB.G, RGB.B);
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
     renames WNM_HAL.OLED.Clear;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (X : Pix_X; Y : Pix_Y; On : Boolean := True)
     renames WNM_HAL.OLED.Set_Pixel;

   -------------------
   -- Update_Screen --
   -------------------

   procedure Update_Screen
     renames WNM_HAL.OLED.Update;

   ------------------------
   -- Select_Audio_Input --
   ------------------------

   procedure Select_Audio_Input (Kind : Audio_Input_Kind)
   is null;

   ---------------------
   -- Set_Main_Volume --
   ---------------------

   procedure Set_Main_Volume (Volume : Audio_Volume)
   is null;

   ---------
   -- Mix --
   ---------

   procedure Mix (Output : in out Stereo_Buffer;
                  Input  :        Mono_Buffer;
                  Volume :        Audio_Volume;
                  Pan    :        Audio_Pan)
   is
      pragma Unreferenced (Volume, Pan);
      use Interfaces;
      use Tresses;
      Point : S32;
   begin
      for Index in Output'Range loop

         Point := S32 (Output (Index).L) + S32 (Input (Index));
         Tresses.DSP.Clip_S16 (Point);
         Output (Index).L := S16 (Point);

         Point := S32 (Output (Index).R) + S32 (Input (Index));
         Tresses.DSP.Clip_S16 (Point);
         Output (Index).R := S16 (Point);
      end loop;
   end Mix;

   ---------
   -- Mix --
   ---------

   procedure Mix (Out_L, Out_R : in out Mono_Buffer;
                  Input        :        Mono_Buffer;
                  Volume       :        Audio_Volume;
                  Pan          :        Audio_Pan)
   is
      pragma Unreferenced (Volume, Pan);
      use Interfaces;
      use Tresses;
      Point : S32;
   begin
      for Index in Input'Range loop
         Point := S32 (Out_L (Index)) + S32 (Input (Index));
         Tresses.DSP.Clip_S16 (Point);
         Out_L (Index) := S16 (Point);

         Point := S32 (Out_R (Index)) + S32 (Input (Index));
         Tresses.DSP.Clip_S16 (Point);
         Out_R (Index) := S16 (Point);
      end loop;
   end Mix;

   ---------
   -- Mix --
   ---------

   procedure Mix (Output      : in out Stereo_Buffer;
                  In_L, In_R  :        Mono_Buffer)
   is
      use Interfaces;
      use Tresses;
      Point : S32;
   begin
      for Index in Output'Range loop
         Point := S32 (Output (Index).L) + S32 (In_L (Index));
         Tresses.DSP.Clip_S16 (Point);
         Output (Index).L := S16 (Point);

         Point := S32 (Output (Index).R) + S32 (In_R (Index));
         Tresses.DSP.Clip_S16 (Point);
         Output (Index).R := S16 (Point);
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

   Plop : aliased Littlefs.LFS_Config;
   function Get_LFS_Config return not null access Littlefs.LFS_Config
   is (Plop'Access);

   ----------------------
   -- Sample_Data_Base --
   ----------------------

   function Sample_Data_Base return System.Address
   is (System.Null_Address);

   ----------
   -- Push --
   ----------

   procedure Push (D : Coproc_Data) is
   begin
      RP.Multicore.FIFO.Push_Blocking (UInt32 (D));
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (D : out Coproc_Data; Success : out Boolean) is
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

   procedure Power_Down
   is null;

begin
   LED_Strip.Initialize (ASM_Offset => WS2812_Offset);

   for IO of B_Cols loop
      IO.Configure (RP.GPIO.Output);
      IO.Clear;
   end loop;

   for IO of B_Rows loop
      IO.Configure (RP.GPIO.Input, RP.GPIO.Pull_Down);
   end loop;

   B_Play_Power.Configure (RP.GPIO.Input, RP.GPIO.Pull_Up);

   --  1kHz tick
   Cortex_M.Systick.Configure
     (Cortex_M.Systick.CPU_Clock,
      Generate_Interrupt => True,
      Reload_Value       =>
        (UInt24 (WNM_HAL_Settings.XOSC_Frequency) / 1_000) - 1);

   Cortex_M.Systick.Enable;

   --  Start the second CPU core that will run the synth
   RP.Multicore.Launch_Core1
     (Trap_Vector   => WNM_HAL.Synth_Core.Trap_Vector,
      Stack_Pointer => WNM_HAL.Synth_Core.Stack_Pointer,
      Entry_Point   => WNM_HAL.Synth_Core.Entry_Point);
end WNM_HAL;
