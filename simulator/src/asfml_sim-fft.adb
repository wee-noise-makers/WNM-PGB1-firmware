with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Complex_Types;        use Ada.Numerics.Complex_Types;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Complex_Elementary_Functions;

package body ASFML_Sim.FFT is

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

   -----------------
   -- Process_FFT --
   -----------------

   procedure Process_FFT (This : in out Instance) is
      Ptr : Natural := This.Circular_Ptr;

      Time_Domain : Complex_Vector (1 .. This.Window_Size);
   begin
      --  Unwrap the circular buffer into the time-domain window
      for Index in reverse Time_Domain'Range loop
         if Ptr = This.Input_Circular'First then
            Ptr := This.Input_Circular'Last;
         else
            Ptr := Ptr - 1;
         end if;

         Time_Domain (Index) := (This.Input_Circular (Ptr), 0.0);
      end loop;

      This.Frequency_Domain := FFT (Time_Domain);
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

   ---------------------------
   -- Copy_Frequency_Domain --
   ---------------------------

   function Copy_Frequency_Domain (This : Instance) return Complex_Vector
   is (This.Frequency_Domain);

end ASFML_Sim.FFT;
