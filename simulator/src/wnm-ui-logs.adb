with ASFML_Sim;

package body WNM.UI.Logs is

   ----------------------
   -- Log_Button_Event --
   ----------------------

   procedure Log_Button_Event (B : Button; Evt : Buttton_Event) is
   begin
      case B is
         when PAD_Up =>
            if Evt = On_Press then
               ASFML_Sim.User_Input_Event_Logs.Insert
                 ((ASFML_Sim.Up_Down, 1));
            end if;

         when PAD_Down =>
            if Evt = On_Press then
               ASFML_Sim.User_Input_Event_Logs.Insert
                 ((ASFML_Sim.Up_Down, -1));
            end if;

         when PAD_Left =>
            if Evt = On_Press then
               ASFML_Sim.User_Input_Event_Logs.Insert
                 ((ASFML_Sim.Left_Right, -1));
            end if;

         when PAD_Right =>
            if Evt = On_Press then
               ASFML_Sim.User_Input_Event_Logs.Insert
                 ((ASFML_Sim.Left_Right, 1));
            end if;

         when others =>
            ASFML_Sim.User_Input_Event_Logs.Insert
              ((ASFML_Sim.Button, B, Evt));
      end case;
   end Log_Button_Event;

end WNM.UI.Logs;
