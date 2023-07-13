with Ada.Numerics.Complex_Arrays; use Ada.Numerics.Complex_Arrays;
with Ada.Numerics.Real_Arrays;    use Ada.Numerics.Real_Arrays;

package ASFML_Sim.FFT is

   type Instance
     (Window_Size : Positive;
      Hop_Size    : Positive)
   is tagged
   private;

   type Window_Function_Kind is (Hann);

   procedure Init (This            : in out Instance;
                   Sample_Rate     :        Natural;
                   Window_Function :        Window_Function_Kind := Hann);

   function Push_Frame (This    : in out Instance;
                        Frame   :        Float)
                        return Boolean;
   --  Push an audio frame for analysis and return true if a new FFT was
   --  computed. (The FFT is computed every Hop_Size frame).

   procedure Push_Frame (This    : in out Instance;
                         Frame   :        Float);
   --  Push an audio frame for analysis

   function Rel (This : Instance; Bin : Natural) return Float
     with Pre => Bin in 1 .. This.Window_Size;
   --  Return the Real component of the FFT Bin

   function Img (This : Instance; Bin : Natural) return Float
     with Pre => Bin in 1 .. This.Window_Size;
   --  Return the Imaginary component of the FFT Bin

   function Amp (This : Instance; Bin : Natural) return Float
     with Pre => Bin in 1 .. This.Window_Size;
   --  Return the Amplitude of the FFT Bin

   function Center_Frequency (This : Instance; Bin : Natural) return Float
     with Pre => Bin in 1 .. This.Window_Size;
   --  Center frequency of the bin in Hz

   procedure Enable_Frequency_Estimate (This : in out Instance;
                                        Enable : Boolean := True);
   --  Enable the computation of estimated frequency for each bin

   function Estimated_Frequency (This : Instance; Bin : Natural) return Float
     with Pre => Bin in 1 .. This.Window_Size;
   --  Estimated frequency of the bin in Hz from phase diff

   function Copy_Frequency_Domain (This : Instance) return Complex_Vector;
   --  Return a copy of the frequency-domain bins

private

   type Instance
     (Window_Size : Positive;
      Hop_Size    : Positive)
   is tagged
      record
         Sample_Rate : Natural := 0;

         Input_Circular   : Real_Vector (1 .. Window_Size) :=
           (others => 0.0);

         Circular_Ptr : Positive := 1;
         Hop_Counter : Natural := 0;

         Frequency_Domain : Complex_Vector (1 .. Window_Size) :=
           (others => (0.0, 0.0));

         Do_Freq_Estimate : Boolean := False;
         Bin_Center_Frequency : Real_Vector (1 .. Window_Size) :=
           (others => 0.0);
         Bin_Phase : Real_Vector (1 .. Window_Size) :=
           (others => 0.0);
         Bin_Estimate_Frequency : Real_Vector (1 .. Window_Size) :=
           (others => 0.0);

         Window : Real_Vector (1 .. Window_Size) := (others => 0.0);

      end record;

end ASFML_Sim.FFT;
