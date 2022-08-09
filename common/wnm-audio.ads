with Interfaces; use Interfaces;

package WNM.Audio is

   --  It can be quite confusing to use the term sample for both audio/music
   --  sample (short bit of music) and PCM sample (signal processing). So in
   --  this project we use the word "Point" for the signal processing.

   type Mono_Point is new Integer_16 with Size => 16;

   type Stereo_Point is record
      L, R : Mono_Point;
   end record with Pack, Size => 32;

   type Mono_Buffer is array (1 .. WNM.Samples_Per_Buffer) of Mono_Point
     with Pack, Size => WNM.Mono_Buffer_Size_In_Bytes * 8;

   type Stereo_Buffer is array (1 .. WNM.Samples_Per_Buffer) of Stereo_Point
     with Pack, Size => WNM.Stereo_Buffer_Size_In_Bytes * 8;

   --  generic
   --     with procedure Process (Out_L, Out_R : out Mono_Buffer;
   --                             In_L,  In_R  :     Mono_Buffer);
   --  procedure Generate_Audio;

end WNM.Audio;
