-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                    Copyright (C) 2024 Fabien Chouteau                     --
--                                                                           --
--    Wee Noise Maker is free software: you can redistribute it and/or       --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Maker is distributed in the hope that it will be useful,     --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with We Noise Maker. If not, see <http://www.gnu.org/licenses/>. --
--                                                                           --
-------------------------------------------------------------------------------

with WNM.Persistent;
with WNM.Mixer;

package body WNM.Audio_Routing is

   Selected_Output : WNM_HAL.Audio_Output_Kind := Headphones;

   type Input_Mute_State is array (WNM_HAL.Audio_Input_Kind) of Boolean;
   Sampling_Mute_State : Input_Mute_State := (others => True);

   ---------------------
   -- Periodic_Update --
   ---------------------

   procedure Periodic_Update is

      Sampling_With_Internal_Mic : constant Boolean :=
        Mixer.Get_Sample_Rec_Mode in Mixer.Preview | Mixer.Rec
        and then not Sampling_Mute_State (WNM_HAL.Internal_Mic);
      --  When sampling with interal microphone the speaker has to be disabled
      --  to avoid feedback.

   begin
      WNM_HAL.Read_HP_Detect;

      if WNM_HAL.HP_Detect
           or else
         Sampling_With_Internal_Mic
      then
         if Selected_Output = Speakers then
            Selected_Output := Headphones;
            WNM_HAL.Select_Audio_Output (Selected_Output);
         end if;
      else
         if Selected_Output = Headphones then
            Selected_Output := Speakers;
            WNM_HAL.Select_Audio_Output (Selected_Output);
         end if;
      end if;
   end Periodic_Update;

   --------------------
   -- Enter_Sampling --
   --------------------

   procedure Enter_Sampling is
   begin
      WNM_HAL.Mute (WNM_HAL.Internal_Mic, Mute => True);
      WNM_HAL.Mute (WNM_HAL.Headset_Mic,  Mute => True);
      WNM_HAL.Mute (WNM_HAL.Line_In,      Mute => True);
      Sampling_Mute_State := (others => True);
   end Enter_Sampling;

   ---------------------------
   -- Select_Sampling_Input --
   ---------------------------

   procedure Select_Sampling_Input (Kind : WNM_HAL.Audio_Input_Kind) is
   begin
      for K in WNM_HAL.Audio_Input_Kind loop
         Sampling_Mute_State (K) := Kind /= K;
         WNM_HAL.Mute (K, Mute => Sampling_Mute_State (K));
      end loop;

      --  When unmuting the internal microphone, check if speaker should be
      --  disabled.
      if not Sampling_Mute_State (WNM_HAL.Internal_Mic) then
         Periodic_Update;
      end if;
   end Select_Sampling_Input;

   --------------------
   -- Leave_Sampling --
   --------------------

   procedure Leave_Sampling is
   begin
      WNM_HAL.Mute (WNM_HAL.Internal_Mic,
                    Mute => WNM.Persistent.Data.Internal_Mic_Mute);
      WNM_HAL.Mute (WNM_HAL.Headset_Mic,
                    Mute => WNM.Persistent.Data.Headset_Mic_Mute);
      WNM_HAL.Mute (WNM_HAL.Line_In,
                    Mute => WNM.Persistent.Data.Line_In_Mute);
   end Leave_Sampling;

   -------------------
   -- Volume_Change --
   -------------------

   procedure Volume_Change (V : in out Audio_Volume; Delt : Integer) is
      Res : Integer;
   begin
      Res := Integer (V) + Delt;
      if Res in Integer (Audio_Volume'First) .. Integer (Audio_Volume'Last)
      then
         V := Audio_Volume (Res);
      end if;
   end Volume_Change;

   ------------------------
   -- Change_Main_Volume --
   ------------------------

   procedure Change_Main_Volume (Volume_Delta : Integer) is
   begin
      Volume_Change (Persistent.Data.Main_Volume, Volume_Delta);
      WNM_HAL.Set_Main_Volume (Persistent.Data.Main_Volume);
   end Change_Main_Volume;

   ---------------------
   -- Set_Main_Volume --
   ---------------------

   procedure Set_Main_Volume (Volume : Audio_Volume) is
   begin
      Persistent.Data.Main_Volume := Volume;
      WNM_HAL.Set_Main_Volume (Persistent.Data.Main_Volume);
   end Set_Main_Volume;

   ---------------------
   -- Get_Main_Volume --
   ---------------------

   function Get_Main_Volume return Audio_Volume
   is (WNM.Persistent.Data.Main_Volume);

   ------------------------------
   -- Toggle_Internal_Mic_Mute --
   ------------------------------

   procedure Toggle_Internal_Mic_Mute is
   begin
      WNM.Persistent.Data.Internal_Mic_Mute :=
        not WNM.Persistent.Data.Internal_Mic_Mute;

      WNM_HAL.Mute (WNM_HAL.Internal_Mic,
                    Mute => WNM.Persistent.Data.Internal_Mic_Mute);
   end Toggle_Internal_Mic_Mute;

   ---------------------------
   -- Get_Internal_Mic_Mute --
   ---------------------------

   function Get_Internal_Mic_Mute return Boolean
   is (WNM.Persistent.Data.Internal_Mic_Mute);

   -----------------------------
   -- Toggle_Headset_Mic_Mute --
   -----------------------------

   procedure Toggle_Headset_Mic_Mute is
   begin
      WNM.Persistent.Data.Headset_Mic_Mute :=
        not WNM.Persistent.Data.Headset_Mic_Mute;

      WNM_HAL.Mute (WNM_HAL.Headset_Mic,
                    Mute => WNM.Persistent.Data.Headset_Mic_Mute);
   end Toggle_Headset_Mic_Mute;

   --------------------------
   -- Get_Headset_Mic_Mute --
   --------------------------

   function Get_Headset_Mic_Mute return Boolean
   is (WNM.Persistent.Data.Headset_Mic_Mute);

   -------------------------
   -- Toggle_Line_In_Mute --
   -------------------------

   procedure Toggle_Line_In_Mute is
   begin
      WNM.Persistent.Data.Line_In_Mute :=
        not WNM.Persistent.Data.Line_In_Mute;

      WNM_HAL.Mute (WNM_HAL.Line_In,
                    Mute => WNM.Persistent.Data.Line_In_Mute);
   end Toggle_Line_In_Mute;

   ----------------------
   -- Get_Line_In_Mute --
   ----------------------

   function Get_Line_In_Mute return Boolean
   is (WNM.Persistent.Data.Line_In_Mute);

   -----------------------
   -- Change_ADC_Volume --
   -----------------------

   procedure Change_ADC_Volume (Volume_Delta : Integer) is
   begin
      Volume_Change (Persistent.Data.ADC_Volume, Volume_Delta);
      WNM_HAL.Set_Input_Volume (Persistent.Data.ADC_Volume);
   end Change_ADC_Volume;

   --------------------
   -- Get_ADC_Volume --
   --------------------

   function Get_ADC_Volume return Audio_Volume
   is (WNM.Persistent.Data.ADC_Volume);

end WNM.Audio_Routing;
