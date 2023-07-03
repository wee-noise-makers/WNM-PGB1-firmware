with ASFML_Sim;

package body WNM.UI.Logs is

   ----------------------
   -- Log_Button_Event --
   ----------------------

   procedure Log_Button_Event (B : Button; Evt : Buttton_Event) is
   begin
      ASFML_Sim.User_Input_Event_Logs.Insert
        ((ASFML_Sim.Button, B, Evt));
   end Log_Button_Event;

   ----------------------
   -- Log_Left_Encoder --
   ----------------------

   procedure Log_Left_Encoder (Delt : Integer) is
   begin
      ASFML_Sim.User_Input_Event_Logs.Insert
        ((ASFML_Sim.Left_Encoder, Delt));
   end Log_Left_Encoder;

   -----------------------
   -- Log_Right_Encoder --
   -----------------------

   procedure Log_Right_Encoder (Delt : Integer) is
   begin
      ASFML_Sim.User_Input_Event_Logs.Insert
        ((ASFML_Sim.Right_Encoder, Delt));
   end Log_Right_Encoder;

end WNM.UI.Logs;
