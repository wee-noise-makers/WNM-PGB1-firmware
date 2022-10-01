with Ada.Calendar; use Ada.Calendar;
with Ada.Synchronous_Task_Control;

with GNAT.Bounded_Buffers;

with Sf;
with Sf.Graphics.Color;

with ASFML_Sim;
with ASFML_SIM_Storage;

with RtMIDI;

package body WNM_HAL is

   package Coproc_BB is new GNAT.Bounded_Buffers (Coproc_Data);

   Coproc_Queue : Coproc_BB.Bounded_Buffer
     (Capacity => Coproc_Queue_Capacity,
      Ceiling  => Coproc_BB.Default_Ceiling);

   LEDs_Internal : ASFML_Sim.SFML_LED_Strip := ASFML_Sim.SFML_LEDs;

   Buttons_Internal : Buttons_State := (others => Up);

   Pixels_Internal : array (Pix_X, Pix_Y) of Boolean :=
     (others => (others => False));

   MIDI_Out : constant RtMIDI.MIDI_Out := RtMIDI.Create ("WNM Simulator");

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

   ------------------
   -- Left_Encoder --
   ------------------

   function Left_Encoder return Integer is
      Res : constant Integer := ASFML_Sim.Encoder_Left;
   begin
      ASFML_Sim.Encoder_Left := 0;
      return Res;
   end Left_Encoder;

   -------------------
   -- Right_Encoder --
   -------------------

   function Right_Encoder return Integer is
      Res : constant Integer := ASFML_Sim.Encoder_Right;
   begin
      ASFML_Sim.Encoder_Right := 0;
      return Res;
   end Right_Encoder;

   ---------
   -- Set --
   ---------

   procedure Set (L : LED; R, G, B : HAL.UInt8) is
      use Sf;
   begin
      LEDs_Internal (L) := (a => 255,
                            r => sfUint8 (R),
                            g => sfUint8 (G),
                            b => sfUint8 (B));
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

   ----------------------
   -- Set_Audio_Volume --
   ----------------------

   procedure Set_Audio_Volume (Volume : Audio_Volume) is
   begin
      null;
   end Set_Audio_Volume;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (Ms : Natural) return Time_Microseconds
   is (Time_Microseconds (Ms * 1_000));

   -----------
   -- Clock --
   -----------

   Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   function Clock return Time_Microseconds is
      Delt_Sec : constant Duration := Ada.Calendar.Clock - Start_Time;
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

   --------------------
   -- Get_LFS_Config --
   --------------------

   function Get_LFS_Config return not null access Littlefs.LFS_Config
   is (ASFML_SIM_Storage.Get_LFS_Config);

   ----------------------
   -- Sample_Data_Base --
   ----------------------

   function Sample_Data_Base return System.Address
   is (ASFML_SIM_Storage.Sample_Data_Base);

   ----------
   -- Push --
   ----------

   procedure Push (D : Coproc_Data) is
   begin
      if not Coproc_Queue.Full then
         Coproc_Queue.Insert (D);
      else
         raise Program_Error with "Corproc queue is full";
      end if;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (D : out Coproc_Data; Success : out Boolean) is
   begin
      if not Coproc_Queue.Empty then
         Coproc_Queue.Remove (D);
         Success := True;
      else
         Success := False;
      end if;
   end Pop;

   ---------------
   -- Send_MIDI --
   ---------------

   procedure Send_MIDI (Data : System.Storage_Elements.Storage_Array) is
      Success : Boolean;
   begin
      RtMIDI.Send_Message (MIDI_Out, Data, Success);

      if not Success then
         raise Program_Error with "MIDI OUT error";
      end if;
   end Send_MIDI;

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down is
   begin
      raise Program_Error with "Power Down";
   end Power_Down;

begin
   if not RtMIDI.Valid (MIDI_Out) then
      raise Program_Error with "Cannot create MIDI device";
   end if;

   RtMIDI.Open_Port (MIDI_Out, 0, "Output");
end WNM_HAL;
