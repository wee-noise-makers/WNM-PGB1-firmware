with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Complex_Types;        use Ada.Numerics.Complex_Types;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Complex_Elementary_Functions;

package body ASFML_Sim.FFT is

   ----------
   -- Init --
   ----------

   procedure Init (This            : in out Instance;
                   Sample_Rate     :        Natural;
                   Window_Function :        Window_Function_Kind := Hann)
   is
   begin
      This.Sample_Rate := Sample_Rate;

      --  Pre-compute bin frequencies
      for N in  1 .. This.Window_Size / 2 loop
         This.Bin_Center_Frequency (N) :=
           (Float (N) - 1.0) * Float (Sample_Rate) / Float (This.Window_Size);
      end loop;

      case Window_Function is
         when Hann =>
            for Index in This.Window'Range loop
               This.Window (Index) :=
                 0.5 *
                   (1.0 - Cos (2.0 * Pi * Float (Index - 1) /
                      Float (This.Window_Size - 1)));
            end loop;
      end case;
   end Init;

   ---------
   -- FFT --
   ---------

   function FFT (X : Complex_Vector) return Complex_Vector
   is
      function FFT_In (X : Complex_Vector; N, S : Positive)
                       return Complex_Vector is
      begin
         if N = 1 then
            return (1 .. 1 => X (X'First));
         else
            declare
               use Ada.Numerics.Complex_Elementary_Functions;
               F : constant Complex  := Exp (Pi * j / Float (N / 2));
               Even : Complex_Vector := FFT_In (X, N / 2, 2 * S);
               Odd  : Complex_Vector := FFT_In (X (X'First + S .. X'Last),
                                                N / 2,
                                                2 * S);
            begin
               for K in 0 .. N / 2 - 1 loop
                  declare
                     T : constant Complex := Odd (Odd'First + K) / F ** K;
                  begin
                     Odd  (Odd'First  + K) := Even (Even'First + K) - T;
                     Even (Even'First + K) := Even (Even'First + K) + T;
                  end;
               end loop;
               return Even & Odd;
            end;
         end if;
      end FFT_In;
   begin
      return FFT_In (X, X'Length, 1);
   end FFT;

   ----------------
   -- Wrap_Phase --
   ----------------

   function Wrap_Phase (P : Float) return Float is
      Res : Float := P;
   begin
      if Res >= 0.0 then
         while Res > Pi loop
            Res := Res - 2.0 * Pi;
         end loop;

         --  return Float'Remainder (P + Pi, 2.0 * Pi) - Pi;
      else
         while Res < -Pi loop
            Res := Res + 2.0 * Pi;
         end loop;

         --  return Float'Remainder (P - Pi, 2.0 * Pi) + Pi;
      end if;
      return Res;
   end Wrap_Phase;

   -----------------
   -- Process_FFT --
   -----------------

   procedure Process_FFT (This : in out Instance) is
      Ptr : Natural := This.Circular_Ptr;

      Time_Domain : Complex_Vector (1 .. This.Window_Size);

      FFT_Size : constant Float := Float (This.Window_Size);
      Hop_Size : constant Float := Float (This.Hop_Size);
   begin
      --  Unwrap the circular buffer into the time-domain window
      for Index in reverse Time_Domain'Range loop
         if Ptr = This.Input_Circular'First then
            Ptr := This.Input_Circular'Last;
         else
            Ptr := Ptr - 1;
         end if;

         Time_Domain (Index) :=
           (This.Input_Circular (Ptr) * This.Window (Index),
            0.0);
      end loop;

      This.Frequency_Domain := FFT (Time_Domain);

      if This.Do_Freq_Estimate then
         for Index in 1 .. This.Window_Size / 2 loop
            declare
               N : constant Float := Float (Index - 1);

               Re : constant Float := This.Frequency_Domain (Index).Re;
               Im : constant Float := This.Frequency_Domain (Index).Im;
               Phase : constant Float :=
                 (if Im = 0.0 and then Re = 0.0
                  then 0.0
                  else Arctan (Im, Re));

               Phase_Diff : constant Float := Phase - This.Bin_Phase (Index);
               --  Change in phase since last hop

               Center_Freq : constant Float :=
                 2.0 * Pi * N / FFT_Size;

               Phase_Deviation : constant Float :=
                 Wrap_Phase (Phase_Diff - Center_Freq * Hop_Size);
               --  Phase deviation from center frequency

               Bin_Deviation : constant Float :=
                 Phase_Deviation * FFT_Size / Hop_Size / (2.0 * Pi);
               --  Fractional bin number (between -0.5 and 0.5)
            begin
               This.Bin_Estimate_Frequency (Index) :=
                 (N + Bin_Deviation) * Float (This.Sample_Rate) / FFT_Size;

               This.Bin_Phase (Index) := Phase;
            end;
         end loop;
      end if;
   end Process_FFT;

   ----------------
   -- Push_Frame --
   ----------------

   function Push_Frame (This    : in out Instance;
                        Frame   :        Float)
                        return Boolean
   is
   begin
      This.Input_Circular (This.Circular_Ptr) := Frame;
      if This.Circular_Ptr = This.Input_Circular'Last then
         This.Circular_Ptr := This.Input_Circular'First;
      else
         This.Circular_Ptr := This.Circular_Ptr + 1;
      end if;

      This.Hop_Counter := This.Hop_Counter + 1;
      if This.Hop_Counter >= This.Hop_Size then
         Process_FFT (This);
         This.Hop_Counter := 0;
         return True;
      end if;

      return False;
   end Push_Frame;

   ----------------
   -- Push_Frame --
   ----------------

   procedure Push_Frame (This  : in out Instance;
                         Frame :        Float)
   is
      Unused : Boolean;
   begin
      Unused := This.Push_Frame (Frame);
   end Push_Frame;

   ---------
   -- Rel --
   ---------

   function Rel (This : Instance; Bin : Natural) return Float is
   begin
      return This.Frequency_Domain (Bin).Re;
   end Rel;

   ---------
   -- Img --
   ---------

   function Img (This : Instance; Bin : Natural) return Float is
   begin
      return This.Frequency_Domain (Bin).Im;
   end Img;

   ---------
   -- Amp --
   ---------

   function Amp (This : Instance; Bin : Natural) return Float is
      R : constant Complex := This.Frequency_Domain (Bin);
   begin
      return Sqrt (R.Re**2 + R.Im**2);
   end Amp;

   ----------------------
   -- Center_Frequency --
   ----------------------

   function Center_Frequency (This : Instance; Bin : Natural) return Float
   is (This.Bin_Center_Frequency (Bin));

   -------------------------------
   -- Enable_Frequency_Estimate --
   -------------------------------

   procedure Enable_Frequency_Estimate (This : in out Instance;
                                        Enable : Boolean := True)
   is
   begin
      This.Do_Freq_Estimate := Enable;
   end Enable_Frequency_Estimate;

   -------------------------
   -- Estimated_Frequency --
   -------------------------

   function Estimated_Frequency (This : Instance; Bin : Natural) return Float
   is (This.Bin_Estimate_Frequency (Bin));

   ---------------------------
   -- Copy_Frequency_Domain --
   ---------------------------

   function Copy_Frequency_Domain (This : Instance) return Complex_Vector
   is (This.Frequency_Domain);

end ASFML_Sim.FFT;
