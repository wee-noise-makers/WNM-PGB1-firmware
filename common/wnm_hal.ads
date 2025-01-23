with System;

with HAL;
with Littlefs;
with WNM_Configuration; use WNM_Configuration;

with MIDI;

with Tresses;

package WNM_HAL is

   function Firmware_Version return String;
   function CPU_Freq return Natural;

   -------------
   -- Buttons --
   -------------

   type Button_State is (Up, Down);
   type Buttons_State is array (Button) of Button_State;

   function State return Buttons_State;
   --  Scan buttons and return current state

   -----------------
   -- Touch Strip --
   -----------------

   subtype Touch_Value is Float range 0.0 .. 1.0;
   type Touch_Data is record
      Touch : Boolean;
      Value : Touch_Value;
   end record;

   function Touch_Strip_State return Touch_Data;
   procedure Set_Thresholds (TP1, TP2, TP3 : HAL.UInt32);
   function TP1 return HAL.UInt32;
   function TP2 return HAL.UInt32;
   function TP3 return HAL.UInt32;

   ----------
   -- LEDs --
   ----------

   type RGB_Rec is record
      R, G, B : HAL.UInt8;
   end record;

   procedure Set (L : LED; RGB : RGB_Rec);
   --  Set the internal RGB value for the given LED

   procedure Clear_LEDs;
   --  Set the internal RGB value to zero for all LEDs

   procedure Update_LEDs;
   --  Update the LEDs using the internal RGB values

   ------------
   -- Screen --
   ------------

   type Pix_X is range 0 .. Screen_Width - 1;
   type Pix_Y is range 0 .. Screen_Height - 1;

   procedure Clear_Pixels;
   --  Turn off all pixels in the internal state

   procedure Set_Pixel (X : Pix_X; Y : Pix_Y; On : Boolean := True);
   --  Set the state of a pixel in the internal state

   procedure Update_Screen;
   --  Update the screen using the internal pixels state

   -----------
   -- Audio --
   -----------

   --  It can be quite confusing to use the term sample for both audio/music
   --  sample (short bits of music) and PCM sample (signal processing). So in
   --  this project we use the word "Point" for the signal processing.

   subtype Mono_Point is Tresses.Mono_Point;

   type Stereo_Point is record
      L, R : Mono_Point;
   end record with Pack, Size => 32;

   subtype Mono_Buffer is Tresses.Mono_Buffer (1 .. Audio.Samples_Per_Buffer);

   type Stereo_Buffer is array (1 .. Audio.Samples_Per_Buffer) of Stereo_Point
     with Pack, Size => Audio.Stereo_Buffer_Size_In_Bytes * 8;

   type Audio_Output_Kind is (Headphones, Speakers);
   procedure Select_Audio_Output (Kind : Audio_Output_Kind);

   type Audio_Volume is range 0 .. 100;
   Init_Volume : constant Audio_Volume := 70;
   Init_Input_Volume : constant Audio_Volume := 0;

   type Audio_Pan is range 0 .. 100;
   Init_Pan : constant Audio_Pan := 50;

   procedure Set_Main_Volume (Volume : Audio_Volume);
   procedure Set_Input_Volume (Volume : Audio_Volume);

   type Audio_Input_Kind is (Line_In, Internal_Mic, Headset_Mic);
   procedure Mute (Kind : Audio_Input_Kind; Mute : Boolean := True);

   procedure Mix (Out_L, Out_R : in out Mono_Buffer;
                  Input        :        Mono_Buffer;
                  Volume       :        Audio_Volume;
                  Pan          :        Audio_Pan);

   ----------
   -- Time --
   ----------

   subtype Time_Microseconds is HAL.UInt64;

   function Milliseconds (Ms : Natural) return Time_Microseconds;

   function Clock return Time_Microseconds;

   procedure Delay_Until (Deadline : Time_Microseconds);

   procedure Delay_Milliseconds (Ms : HAL.UInt64);

   procedure Delay_Microseconds (Us : HAL.UInt64);

   procedure Start_Sequencer_Tick;

   -------------
   -- Storage --
   -------------

   function Get_LFS_Config return access Littlefs.LFS_Config;

   function Sample_Data_Base return System.Address;

   type Storage_Sector_Data
   is array (1 .. WNM_Configuration.Storage.Sector_Byte_Size) of HAL.UInt8;

   type Sample_Sector_Id
   is range 0 .. WNM_Configuration.Storage.Sample_Library_Sectors - 1;

   procedure Write_To_Storage (Id   : Sample_Sector_Id;
                               Data : Storage_Sector_Data);

   ---------------------
   -- Coproc Messages --
   ---------------------

   type Coproc_Target is (Synth_CPU, Main_CPU);

   type Coproc_Data is mod 2**Coproc_Data_Size
     with size => Coproc_Data_Size;

   procedure Push (Target : Coproc_Target;
                   D      : Coproc_Data);
   --  Send data to the synth coprocessor. Fails silently if the data cannot
   --  be pushed (e.g. queue is full).

   procedure Pop (Target  :     Coproc_Target;
                  D       : out Coproc_Data;
                  Success : out Boolean);
   --  Tentatively get data for the synth coprocessor. Success is False if
   --  no data is available.

   ----------
   -- MIDI --
   ----------

   procedure Send_External (Msg : MIDI.Message);
   procedure Flush_Output;
   procedure Get_External (Msg : out MIDI.Message; Success : out Boolean);

   -----------
   -- Power --
   -----------

   function Shutdown_Requested return Boolean;
   procedure Power_Down;
   procedure Enter_DFU_Mode;

   --------------
   -- Watchdog --
   --------------

   procedure Watchdog_Init;
   procedure Watchdog_Check;

   --------------
   -- CPU Hold --
   --------------

   --  Writing to the flash storage disables read access for a short period of
   --  time. During this periode the synth CPU must not execute code or read
   --  data from the flash. The following subprograms provide a way for the
   --  sequencer CPU to request the synth CPU to hold during flash operations.

   procedure Wait_Synth_CPU_Hold;
   --  This procedure will request the synth CPU to hold and only return when
   --  hold is confirmed by the synth CPU.

   procedure Release_Synth_CPU_Hold;
   --  This procedure releases the synth CPU from hold

   procedure Synth_CPU_Check_Hold;
   --  This procedure checks if the sequencer CPU asked for a hold, and if
   --  that's the case holds until released.

   -------------
   -- Battery --
   -------------

   procedure Read_Battery_Voltage;

   function Battery_Millivolts return Natural;

   ----------------------
   -- Headphone Detect --
   ----------------------

   procedure Read_HP_Detect;
   --  HP detect line is on an I2C IO expander, reading the state requires
   --  I2C coms and takes a bit of time. To avoid concurent use of the I2C
   --  peripherial we want run the query only from the sequencer CPU tick
   --  handler. On the other hand we may want to use the value at different
   --  places in the code (e.g. the GUI). So this procedure is only reading
   --  the state from the IO expander and stores it into a variable. To read
   --  the variable use the function below from anywhere in the code.

   function HP_Detect return Boolean;

end WNM_HAL;
