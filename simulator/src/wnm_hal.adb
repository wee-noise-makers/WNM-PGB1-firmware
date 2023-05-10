with Ada.Synchronous_Task_Control;
with GNAT.OS_Lib;
with Interfaces;

with Sf;
with Sf.Graphics.Color;

with ASFML_Sim;
with ASFML_SIM_Storage;

with RtMIDI;

with BBqueue;

package body WNM_HAL is

   use type System.Storage_Elements.Storage_Offset;

   Coproc_Queue : BBqueue.Offsets_Only (Coproc_Queue_Capacity);
   Coproc_Buffer : array (BBqueue.Buffer_Offset range
                            0 .. Coproc_Queue_Capacity - 1) of Coproc_Data;

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

   ---------
   -- Mix --
   ---------

   procedure Mix (Output : in out Stereo_Buffer;
                  Input  :        Mono_Buffer;
                  Volume :        Audio_Volume;
                  Pan    :        Audio_Pan)
   is
      Volume_F : constant Float := Float (Volume) / Float (Audio_Volume'Last);

      procedure Point_Mix (P_Out : in out Mono_Point;
                           P_In  :        Mono_Point;
                           Pan_F :        Float)
      is
         use Interfaces;

         Sample : constant Float := Float (P_In) * Volume_F * Pan_F;
         Res : constant Integer_32 := Integer_32 (P_Out) + Integer_32 (Sample);
      begin

         if Res > Integer_32 (Mono_Point'Last) then
            P_Out := Mono_Point'Last;
         elsif Res < Integer_32 (Mono_Point'First) then
            P_Out := Mono_Point'First;
         else
            P_Out := Mono_Point (Res);
         end if;
      end Point_Mix;

      Pan_F : constant Float := Float (Pan) / Float (Audio_Pan'Last);

      Pan_L : constant Float := 1.0 - Pan_F;
      Pan_R : constant Float := 0.0 + Pan_F;
   begin

      for Idx in Output'Range loop
         Point_Mix (Output (Idx).L, Input (Idx), Pan_L);
         Point_Mix (Output (Idx).R, Input (Idx), Pan_R);
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
      Volume_F : constant Float := Float (Volume) / Float (Audio_Volume'Last);

      procedure Point_Mix (P_Out : in out Mono_Point;
                           P_In  :        Mono_Point;
                           Pan_F :        Float)
      is
         use Interfaces;

         Sample : constant Float := Float (P_In) * Volume_F * Pan_F;
         Res : constant Integer_32 := Integer_32 (P_Out) + Integer_32 (Sample);
      begin

         if Res > Integer_32 (Mono_Point'Last) then
            P_Out := Mono_Point'Last;
         elsif Res < Integer_32 (Mono_Point'First) then
            P_Out := Mono_Point'First;
         else
            P_Out := Mono_Point (Res);
         end if;
      end Point_Mix;

      Pan_F : constant Float := Float (Pan) / Float (Audio_Pan'Last);

      Pan_L : constant Float := 1.0 - Pan_F;
      Pan_R : constant Float := 0.0 + Pan_F;
   begin

      for Idx in Out_L'Range loop
         Point_Mix (Out_L (Idx), Input (Idx), Pan_L);
         Point_Mix (Out_R (Idx), Input (Idx), Pan_R);
      end loop;
   end Mix;

   ---------
   -- Mix --
   ---------

   procedure Mix (Output      : in out Stereo_Buffer;
                  In_L, In_R  :        Mono_Buffer)
   is
      procedure Point_Mix (P_Out : in out Mono_Point;
                           P_In  :        Mono_Point)
      is
         use Interfaces;

         Res : constant Integer_32 := Integer_32 (P_Out) + Integer_32 (P_In);
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
      for Idx in Output'Range loop
         Point_Mix (Output (Idx).L, In_L (Idx));
         Point_Mix (Output (Idx).R, In_R (Idx));
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
      use BBqueue;

      WG : Write_Grant;
   begin

      Grant (Coproc_Queue, WG, 1);

      if State (WG) = Valid then
         Coproc_Buffer (Slice (WG).From) := D;
         Commit (Coproc_Queue, WG, 1);
      else
         raise Program_Error with "Corproc queue is full";
      end if;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (D : out Coproc_Data; Success : out Boolean) is
      use BBqueue;

      RG : Read_Grant;
   begin

      Read (Coproc_Queue, RG, 1);

      if State (RG) = Valid then
         D := Coproc_Buffer (Slice (RG).From);
         Release (Coproc_Queue, RG, 1);
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
      Result : constant String :=
        ASFML_SIM_Storage.Save_ROM (ASFML_SIM_Storage.ROM_Path);
   begin
      if Result /= "" then
         raise Program_Error with Result;
      end if;
      GNAT.OS_Lib.OS_Exit (0);
   end Power_Down;

begin
   if not RtMIDI.Valid (MIDI_Out) then
      raise Program_Error with "Cannot create MIDI device";
   end if;

   RtMIDI.Open_Port (MIDI_Out, 0, "Output");
end WNM_HAL;
