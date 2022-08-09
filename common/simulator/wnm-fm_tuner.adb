package body WNM.FM_Tuner is

   Internal_State : State_Kind := Tuned;
   Chan_Save : HAL.UInt10 := 100;

   -----------
   -- State --
   -----------

   function State return State_Kind
   is (Internal_State);

   -------------
   -- Channel --
   -------------

   function Channel return HAL.UInt10
   is (Chan_Save);

   --------------
   -- Strength --
   --------------

   function Strength return HAL.UInt8
   is (127);

   ----------
   -- Seek --
   ----------

   procedure Seek (Dir : Seek_Direction) is
   begin
      case Dir is
         when Up =>
            Internal_State := Seeking_Up;
         when Down =>
            Internal_State := Seeking_Down;
      end case;
   end Seek;

   ----------
   -- Tune --
   ----------

   procedure Tune (Freq : HAL.UInt10) is
   begin
      Chan_Save := Freq;
      Internal_State := Tunning;
   end Tune;

end WNM.FM_Tuner;
