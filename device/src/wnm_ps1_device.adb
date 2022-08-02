
with WNM_PS1_HAL;
with WNM_PS1_HAL_Params; use WNM_PS1_HAL_Params;

with RP.Device;

procedure WNM_PS1_Device is
begin
   loop
      WNM_PS1_HAL.Clear_LEDs;

      for Id in LED loop
         WNM_PS1_HAL.Set (Id, 0, 255, 0);
      end loop;

      WNM_PS1_HAL.Update_LEDs;

      RP.Device.Timer.Delay_Milliseconds (1000);

   end loop;
end WNM_PS1_Device;
