
with WNM_HAL; use WNM_HAL;
with WNM_Configuration; use WNM_Configuration;

with WNM.Tasks;

with RP.Device;

procedure WNM_PS1_Device is
   State : WNM_HAL.Buttons_State;
begin
   WNM.Tasks.Sequencer_Core;

   loop

      WNM_HAL.Clear_Pixels;
      for X in 1 .. 50 loop
         WNM_HAL.Set_Pixel (Pix_X (X), Pix_Y (X));
      end loop;
      WNM_HAL.Update_Screen;

      WNM_HAL.Clear_LEDs;

      State := WNM_HAL.State;
      for Id in LED loop
         if State (Id) = Down then
            WNM_HAL.Set (Id, (0, 255, 0));
         else
            WNM_HAL.Set (Id, (0, 0, 255));
         end if;
      end loop;

      WNM_HAL.Update_LEDs;

      RP.Device.Timer.Delay_Milliseconds (10);

   end loop;

end WNM_PS1_Device;
