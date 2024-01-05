with AAA.Debug;
with Ada.Text_IO;
with GNAT.OS_Lib;
with WNM_HAL; use WNM_HAL;

with WNM.Tasks;

package body ASFML_Sim.Coproc_Queues is

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

   task Coproc_Interrupts is
   end Coproc_Interrupts;

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

   -----------------------
   -- Coproc_Interrupts --
   -----------------------

   task body Coproc_Interrupts is
   begin

      loop
         for Target in Coproc_Target loop
            if not Coproc_Queue (Target).Empty then
               case Target is
                  when Main_CPU =>
                     WNM.Tasks.Sequencer_Coproc_Receive;
                  when Synth_CPU =>
                     WNM.Tasks.Synth_Coproc_Receive;
               end case;
            end if;
         end loop;
         delay 0.00001;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("=== Copro Interrupts Task exception ===");
         AAA.Debug.Put_Exception (E);
         GNAT.OS_Lib.OS_Exit (1);
   end Coproc_Interrupts;

   ----------
   -- Push --
   ----------

   procedure Push (Target : Coproc_Target;
                   D      : Coproc_Data)
   is
   begin

      if not Coproc_Queue (Target).Full then
         Coproc_Queue (Target).Insert (D);
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

end ASFML_Sim.Coproc_Queues;
