with WNM.Tasks;

with Ada.Synchronous_Task_Control;
with GNAT.OS_Lib;
with Ada.Text_IO;
with Interfaces;

with Sf;
with Sf.Graphics.Color;

with ASFML_Sim;
with ASFML_SIM_Storage;

with RtMIDI;

package body WNM_HAL is

   LEDs_Internal : ASFML_Sim.SFML_LED_Strip := ASFML_Sim.SFML_LEDs;

   Buttons_Internal : Buttons_State := (others => Up);

   Pixels_Internal : array (Pix_X, Pix_Y) of Boolean :=
     (others => (others => False));

   MIDI_Out : constant RtMIDI.MIDI_Out := RtMIDI.Create ("WNM Simulator");

   type Circular_Buffer_Content is array (Positive range <>) of Coproc_Data;

   protected type Circular_Buffer (Capacity : Positive) is
      entry Insert (Item : Coproc_Data);
      entry Remove (Item : out Coproc_Data);

      function Empty  return Boolean;
      function Full   return Boolean;

   private
      Values   : Circular_Buffer_Content (1 .. Capacity);
      Next_In  : Positive := 1;
      Next_Out : Positive := 1;
      Count    : Natural  := 0;
   end Circular_Buffer;

   Coproc_Queue : array (Coproc_Target)
     of Circular_Buffer (Coproc_Queue_Capacity);

   ---------------------
   -- Circular_Buffer --
   ---------------------

   protected body Circular_Buffer is

      ------------
      -- Insert --
      ------------

      entry Insert (Item : Coproc_Data) when Count /= Capacity is
      begin
         Values (Next_In) := Item;
         Next_In := (Next_In mod Capacity) + 1;
         Count := Count + 1;
      end Insert;

      ------------
      -- Remove --
      ------------

      entry Remove (Item : out Coproc_Data) when Count > 0 is
      begin
         Item := Values (Next_Out);
         Next_Out := (Next_Out mod Capacity) + 1;
         Count := Count - 1;
      end Remove;

      -----------
      -- Empty --
      -----------

      function Empty return Boolean is
      begin
         return Count = 0;
      end Empty;

      ----------
      -- Full --
      ----------

      function Full return Boolean is
      begin
         return Count = Capacity;
      end Full;

   end Circular_Buffer;

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

   ----------
   -- Push --
   ----------

   procedure Push (Target : Coproc_Target;
                   D      : Coproc_Data)
   is
   begin

      if not Coproc_Queue (Target).Full then
         Coproc_Queue (Target).Insert (D);

         --  Hack to simulate an interrupt
         if Target = Main_CPU then
            WNM.Tasks.Sequencer_Coproc_Receive;
         end if;
      else
         Ada.Text_IO.Put_Line (Coproc_Queue (Target)'Img);
         raise Program_Error with Target'Img &  " Corproc queue is full";
      end if;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Target  :     Coproc_Target;
                  D       : out Coproc_Data;
                  Success : out Boolean)
   is
   begin

      if Coproc_Queue (Target).Empty then
         Success := False;
      else
         Coproc_Queue (Target).Remove (D);
         Success := True;
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
      raise Program_Error with "Cannot create MIDI device";
   end if;

   RtMIDI.Open_Port (MIDI_Out, 0, "Output");
end WNM_HAL;
