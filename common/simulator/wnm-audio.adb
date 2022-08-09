with Ada.Text_IO;
with Ada.Synchronous_Task_Control;

with GNAT.OS_Lib;

with Sf.Audio; use Sf.Audio;
with Sf.Audio.SoundStream; use Sf.Audio.SoundStream;
with Sf; use Sf;

with WNM.Synth;

with System;

--  with BBqueue;

--  with System.Storage_Elements; use System.Storage_Elements;

--  with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package body WNM.Audio is

   ------------- SFML

   Stream : sfSoundStream_Ptr;

   function SFML_Audio_Callback (chunk  : access sfSoundStreamChunk;
                                 Unused : System.Address)
                                 return sfBool
     with Convention => C;

   ---------------

   Flip : Boolean := False
     with Atomic, Volatile;

   Flip_Out_Buffers : array (Boolean) of Stereo_Buffer :=
     (others => (others => (0, 0)));

   Flip_In_Buffers : constant array (Boolean) of Stereo_Buffer :=
     (others => (others => (0, 0)));

   Synth_Trig : Ada.Synchronous_Task_Control.Suspension_Object;

   task Synth_Task is
   end Synth_Task;

   ----------------
   -- Synth_Task --
   ----------------

   task body Synth_Task is
   begin
      loop
         WNM.Synth.Next_Points (Flip_Out_Buffers (Flip),
                                Flip_In_Buffers (Flip));

         Ada.Synchronous_Task_Control.Suspend_Until_True (Synth_Trig);
         Ada.Synchronous_Task_Control.Set_False (Synth_Trig);
      end loop;
   end Synth_Task;

   -------------------------
   -- SFML_Audio_Callback --
   -------------------------

   function SFML_Audio_Callback (chunk  : access sfSoundStreamChunk;
                                 Unused : System.Address)
                                 return sfBool
   is
      Stream_Data : array (1 .. Mono_Buffer'Length * 2) of aliased sfInt16
      with Address => Flip_Out_Buffers (Flip)'Address;
   begin
      chunk.Samples := Stream_Data (1)'Unchecked_Access;
      chunk.NbSamples := Stream_Data'Length;

      Flip := not Flip;

      Ada.Synchronous_Task_Control.Set_True (Synth_Trig);
      return True;
   end SFML_Audio_Callback;

   --  -------------------------
   --  -- SFML_Audio_Callback --
   --  -------------------------
   --
   --  function SFML_Audio_Callback (chunk  : access sfSoundStreamChunk;
   --                                Unused : System.Address)
   --                                return sfBool
   --  is
   --     use BBqueue;
   --
   --     Out_Read : Read_Grant;
   --     In_Write : Write_Grant;
   --
   --     Stream_Index : Natural := Stream_Data'First;
   --
   --  begin
   --
   --     -- Output --
   --
   --     Read (Out_Queue, Out_Read, 1);
   --
   --     if State (Out_Read) /= Valid then
   --        Stream_Data := (others => 0);
   --     else
   --
   --        declare
   --           Out_Index : constant Buffer_Offset :=
   --             Out_L'First + Slice (Out_Read).From;
   --        begin
   --           for X in Mono_Buffer'Range loop
   --              Stream_Data (Stream_Index) := sfInt16 (Out_L (Out_Index) (X));
   --              Stream_Data (Stream_Index + 1) := sfInt16 (Out_R (Out_Index) (X));
   --              Stream_Index := Stream_Index + 2;
   --           end loop;
   --        end;
   --
   --        Release (Out_Queue, Out_Read, 1);
   --     end if;
   --
   --     -- Input --
   --
   --     Grant (In_Queue, In_Write, 1);
   --
   --     if State (In_Write) = Valid then
   --        declare
   --           In_Index : constant Buffer_Offset :=
   --             In_L'First + Slice (In_Write).From;
   --
   --           Line_In : Stereo_Buffer;
   --        begin
   --
   --           Line_In := (others => (0, 0));
   --           --  Line_In := Stereo_Buffer'Input (Radio_Input_Stream);
   --
   --           for X in Line_In'Range loop
   --              In_L (In_Index) (X) := Line_In (X).L;
   --              In_R (In_Index) (X) := Line_In (X).R;
   --           end loop;
   --
   --           Commit (In_Queue, In_Write, 1);
   --        end;
   --     end if;
   --
   --     chunk.Samples := Stream_Data (Stream_Data'First)'Access;
   --     chunk.NbSamples := Stream_Data'Length;
   --
   --     Ada.Synchronous_Task_Control.Set_True (Synth_Trig);
   --     return True;
   --  end SFML_Audio_Callback;
   --
   --
   --  --------------------
   --  -- Generate_Audio --
   --  --------------------
   --
   --  procedure Generate_Audio is
   --     use BBqueue;
   --
   --     In_Read : Read_Grant;
   --     Out_Write : Write_Grant;
   --  begin
   --     Grant (Out_Queue, Out_Write, 1);
   --
   --     if State (Out_Write) /= Valid then
   --        return;
   --     end if;
   --
   --     BBqueue.Read (In_Queue, In_Read, 1);
   --
   --     if State (In_Read) /= Valid then
   --        Commit (Out_Queue, Out_Write, 0);
   --        return;
   --     end if;
   --
   --     declare
   --        Out_Index : constant Buffer_Offset :=
   --          Out_L'First + Slice (Out_Write).From;
   --
   --        In_Index : constant Buffer_Offset :=
   --          In_L'First +  Slice (Out_Write).From;
   --     begin
   --        Process (Out_L (Out_Index),
   --                 Out_R (Out_Index),
   --                 In_L (In_Index),
   --                 In_R (In_Index));
   --
   --        Commit (Out_Queue, Out_Write, 1);
   --        Release (In_Queue, In_Read, 1);
   --     end;
   --  end Generate_Audio;

   ------------------
   -- Select_Input --
   ------------------

   procedure Select_Input (Kind : Input_Kind) is
   begin
      null;
   end Select_Input;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume (Volume : DAC_Volume) is
   begin
      null;
   end Set_Volume;

begin

   if GNAT.OS_Lib.Getenv ("OS").all = "Windows_NT" then
      --  Select driver for openal on Windows
      GNAT.OS_Lib.Setenv ("ALSOFT_DRIVERS", "dsound");
   end if;

   Stream := create (onGetData    => SFML_Audio_Callback'Access,
                     onSeek       => null,
                     channelCount => 2,
                     sampleRate   => WNM.Sample_Frequency,
                     userData     => System.Null_Address);

   if Stream = null then
      Ada.Text_IO.Put_Line ("Could not create audio stream");
      GNAT.OS_Lib.OS_Exit (1);
   else
      play (Stream);
   end if;

end WNM.Audio;
