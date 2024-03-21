with System.Storage_Elements;
with Ada.Synchronous_Task_Control;
with GNAT.OS_Lib;
with Interfaces;
with Interfaces.C;

with Sf;
with Sf.Graphics.Color;

with ASFML_Sim;
with ASFML_Sim.Coproc_Queues;
with ASFML_SIM_Storage;

with MIDI.Encoder;
with MIDI.Decoder.Queue;
with RtMIDI;

with Wnm_Ps1_Simulator_Config;

package body WNM_HAL is

   LEDs_Internal : ASFML_Sim.SFML_LED_Strip := ASFML_Sim.SFML_LEDs;

   Buttons_Internal : Buttons_State := (others => Up);

   Pixels_Internal : array (Pix_X, Pix_Y) of Boolean :=
     (others => (others => False));

   MIDI_Out : constant RtMIDI.MIDI_Out :=
     RtMIDI.Create (Wnm_Ps1_Simulator_Config.Crate_Name);
   MIDI_In : constant RtMIDI.MIDI_In :=
     RtMIDI.Create (Wnm_Ps1_Simulator_Config.Crate_Name);

   MIDI_In_Queue : MIDI.Decoder.Queue.Instance (1024);

   procedure MIDI_Input_Callback_C
     (Time_Stamp   : Interfaces.C.double;
      Message      : System.Address;
      Message_Size : Interfaces.C.size_t;
      User_Data    : System.Address)
     with Convention => C;

   -----------
   -- State --
   -----------

   function State return Buttons_State is
   begin
      for B in Button loop
         Buttons_Internal (B) :=
           (if ASFML_Sim.SFML_Pressed (B) or else ASFML_Sim.Force_Pressed (B)
            then Down
            else Up);
      end loop;

      Ada.Synchronous_Task_Control.Set_True (ASFML_Sim.Button_Scan_Signal);

      return Buttons_Internal;
   end State;

   -----------------------
   -- Touch_Strip_State --
   -----------------------

   function Touch_Strip_State return Touch_Data
   is (ASFML_Sim.Strip_Touch, ASFML_Sim.Strip_Value);

   ---------
   -- TP1 --
   ---------

   function TP1 return HAL.UInt32
   is (0);

   ---------
   -- TP2 --
   ---------

   function TP2 return HAL.UInt32
   is (0);

   ---------
   -- Set --
   ---------

   procedure Set (L : LED; RGB : RGB_Rec) is
      use Sf;
   begin
      LEDs_Internal (L) := (a => 255,
                            r => sfUint8 (RGB.R),
                            g => sfUint8 (RGB.G),
                            b => sfUint8 (RGB.B));
   end Set;

   ----------------
   -- Clear_LEDs --
   ----------------

   procedure Clear_LEDs is
   begin
      LEDs_Internal := (others => Sf.Graphics.Color.sfTransparent);
   end Clear_LEDs;

   -----------------
   -- Update_LEDs --
   -----------------

   procedure Update_LEDs is
   begin
      ASFML_Sim.SFML_LEDs := LEDs_Internal;
   end Update_LEDs;

   ------------------
   -- Clear_Pixels --
   ------------------

   procedure Clear_Pixels is
   begin
      Pixels_Internal := (others => (others => False));
   end Clear_Pixels;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (X : Pix_X; Y : Pix_Y; On : Boolean := True) is
   begin
      Pixels_Internal (X, Y) := On;
   end Set_Pixel;

   -------------------
   -- Update_Screen --
   -------------------

   procedure Update_Screen is
      use Sf.Graphics.Color;

      FB : array (0 .. (Screen_Width * Screen_Height) - 1) of sfColor
        with Address => ASFML_Sim.Frame_Buffer'Address;
      Pix_Color : constant sfColor := fromRGB (0, 153, 255);
      BG_Color  : constant sfColor := fromRGB (0, 0, 0);
   begin
      FB := (others => BG_Color);

      for X in Pixels_Internal'Range (1) loop
         for Y in Pixels_Internal'Range (2) loop
            if Pixels_Internal (X, Y) then
               FB (Integer (X) + Integer (Y) * Screen_Width) := Pix_Color;
            end if;
         end loop;
      end loop;
   end Update_Screen;

   ------------------------
   -- Select_Audio_Input --
   ------------------------

   procedure Select_Audio_Input (Kind : Audio_Input_Kind) is
   begin
      null;
   end Select_Audio_Input;

   -----------------------
   -- Set_Master_Volume --
   -----------------------

   procedure Set_Main_Volume (Volume : Audio_Volume) is
      use Interfaces;

      Step : constant Integer_16 := Integer_16'Last / 100;
   begin
      ASFML_Sim.Main_Volume := Step * Integer_16 (Volume);
   end Set_Main_Volume;

   procedure Set_Line_In_Volume (Volume : Audio_Volume) is null;
   procedure Set_Mic_Volumes (Headset, Internal : Audio_Volume) is null;

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
   is (Time_Microseconds (Ms * 1_000));

   -----------
   -- Clock --
   -----------

   function Clock return Time_Microseconds is
      Delt_Sec : constant Duration := ASFML_Sim.Sim_Clock.Elapsed;
   begin
      return HAL.UInt64 (Delt_Sec * 1_000_000.0);
   end Clock;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Deadline : Time_Microseconds) is
      use HAL;

      Now : constant Time_Microseconds := Clock;
   begin
      if Deadline > Now then
         Delay_Microseconds (Deadline - Now);
      end if;
   end Delay_Until;

   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   procedure Delay_Milliseconds (Ms : HAL.UInt64) is
   begin
      delay Duration (Ms) / 1_000.0;
   end Delay_Milliseconds;

   ------------------------
   -- Delay_Microseconds --
   ------------------------

   procedure Delay_Microseconds (Us : HAL.UInt64) is
   begin
      delay Duration (Us) / 1_000_000.0;
   end Delay_Microseconds;

   --------------------------
   -- Start_Sequencer_Tick --
   --------------------------

   procedure Start_Sequencer_Tick is
   begin
      null;
   end Start_Sequencer_Tick;

   --------------------
   -- Get_LFS_Config --
   --------------------

   function Get_LFS_Config return access Littlefs.LFS_Config
   is (ASFML_SIM_Storage.Get_LFS_Config);

   ----------------------
   -- Sample_Data_Base --
   ----------------------

   function Sample_Data_Base return System.Address
   is (ASFML_SIM_Storage.Sample_Data_Base);

   ----------------------
   -- Write_To_Storage --
   ----------------------

   procedure Write_To_Storage (Id   : Sample_Sector_Id;
                               Data : Storage_Sector_Data)
   is
   begin
      ASFML_SIM_Storage.Write_To_Sample_Data (Id, Data);
   end Write_To_Storage;

   ----------
   -- Push --
   ----------

   procedure Push (Target : Coproc_Target;
                   D      : Coproc_Data)
   renames ASFML_Sim.Coproc_Queues.Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Target  :     Coproc_Target;
                  D       : out Coproc_Data;
                  Success : out Boolean)
   renames ASFML_Sim.Coproc_Queues.Pop;

   -------------------
   -- Send_External --
   -------------------

   procedure Send_External (Msg : MIDI.Message) is
      use System.Storage_Elements;
      Data : constant Storage_Array := MIDI.Encoder.Encode (Msg);
      Success : Boolean;
   begin
      RtMIDI.Send_Message (MIDI_Out, Data, Success);

      if not Success then
         raise Program_Error with "MIDI OUT error";
      end if;
   end Send_External;

   ------------------
   -- Flush_Output --
   ------------------

   procedure Flush_Output
   is null;

   ---------------------------
   -- MIDI_Input_Callback_C --
   ---------------------------

   procedure MIDI_Input_Callback_C
     (Time_Stamp   : Interfaces.C.double;
      Message      : System.Address;
      Message_Size : Interfaces.C.size_t;
      User_Data    : System.Address)
   is
      pragma Unreferenced (Time_Stamp, User_Data);
      Data : HAL.UInt8_Array (1 .. Natural (Message_Size))
        with Address => Message;
   begin
      for Elt of Data loop
         MIDI.Decoder.Queue.Push (MIDI_In_Queue, Elt);
      end loop;
   end MIDI_Input_Callback_C;

   ------------------
   -- Get_External --
   ------------------

   procedure Get_External (Msg : out MIDI.Message; Success : out Boolean) is
   begin
      MIDI.Decoder.Queue.Pop (MIDI_In_Queue, Msg, Success);
   end Get_External;

   ------------------------
   -- Shutdown_Requested --
   ------------------------

   function Shutdown_Requested return Boolean
   is (False);

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down is
      Result : constant String :=
        ASFML_SIM_Storage.Save_ROM (ASFML_SIM_Storage.ROM_Path);
   begin
      if Result /= "" then
         raise Program_Error with Result;
      end if;
      GNAT.OS_Lib.OS_Exit (0);
   end Power_Down;

   --------------------
   -- Enter_DFU_Mode --
   --------------------

   procedure Enter_DFU_Mode is
   begin
      Power_Down;
   end Enter_DFU_Mode;

   -------------------
   -- Watchdog_Init --
   -------------------

   procedure Watchdog_Init
   is null;

   --------------------
   -- Watchdog_Check --
   --------------------

   procedure Watchdog_Check
   is null;

   -------------------------
   -- Wait_Synth_CPU_Hold --
   -------------------------

   procedure Wait_Synth_CPU_Hold
   is null;

   ----------------------------
   -- Release_Synth_CPU_Hold --
   ----------------------------

   procedure Release_Synth_CPU_Hold
   is null;

   --------------------------
   -- Synth_CPU_Check_Hold --
   --------------------------

   procedure Synth_CPU_Check_Hold
   is null;

   ------------------------
   -- Battery_Millivolts --
   ------------------------

   function Battery_Millivolts return Natural
   is (4000);

   ----------------------
   -- Set_Indicator_IO --
   ----------------------

   procedure Set_Indicator_IO (Id : Indicator_IO_Line) is null;

   ------------------------
   -- Clear_Indicator_IO --
   ------------------------

   procedure Clear_Indicator_IO (Id : Indicator_IO_Line) is null;

begin
   if not RtMIDI.Valid (MIDI_Out) then
      raise Program_Error with "Cannot create MIDI out device";
   end if;

   pragma Warnings (Off, "condition is always");
   if Wnm_Ps1_Simulator_Config.Alire_Host_OS = "windows" then
      RtMIDI.Open_Port (MIDI_Out, 1, "Output");
   else
      RtMIDI.Create_Virtual_Port (MIDI_Out, "Output");
   end if;

   if not RtMIDI.Valid (MIDI_In) then
      raise Program_Error with "Cannot create MIDI In device";
   end if;

   RtMIDI.Ignore_Messages (MIDI_In,
                           SysEx => True,
                           Time  => False,
                           Sense => True);

   RtMIDI.Set_Callback (MIDI_In, MIDI_Input_Callback_C'Access);

   pragma Warnings (Off, "condition is always *");
   if Wnm_Ps1_Simulator_Config.Alire_Host_OS = "windows" then
      RtMIDI.Open_Port (MIDI_In, 0, "Input");
   else
      RtMIDI.Create_Virtual_Port (MIDI_In, "Input");
   end if;

end WNM_HAL;
