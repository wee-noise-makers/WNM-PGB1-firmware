
with WNM_PS1_HAL;
with WNM_PS1_HAL_Params; use WNM_PS1_HAL_Params;

procedure WNM_PS1_Main is
begin
   loop
      WNM_PS1_HAL.Clear_LEDs;
      WNM_PS1_HAL.Clear_Pixels;

      declare
         State : constant WNM_PS1_HAL.Buttons_State :=
           WNM_PS1_HAL.State;
      begin
         for Id in LED loop
            if State (Id) then
               WNM_PS1_HAL.Set (Id, 0, 255, 0);
               WNM_PS1_HAL.Set_Pixel (10, 10);
            end if;
         end loop;
      end;

      WNM_PS1_HAL.Update_LEDs;
      WNM_PS1_HAL.Delay_Milliseconds (100);

      WNM_PS1_HAL.Update_Screen;

   end loop;
end WNM_PS1_Main;
