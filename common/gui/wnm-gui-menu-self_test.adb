-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2025 Fabien Chouteau                    --
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

with HAL;

with WNM.GUI.Menu.Drawing;

with WNM.Screen;
with WNM.LEDs;

package body WNM.GUI.Menu.Self_Test is

   Self_Test_Singleton : aliased Self_Test_WIndow;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Self_Test_Singleton'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (This   : in out Self_Test_WIndow)
   is
      pragma Unreferenced (This);

   begin
      Drawing.Draw_Title ("Press A to start",
                          "...");
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Self_Test_WIndow;
      Event : Menu_Event)
   is
      pragma Unreferenced (This);

      use Drawing;
      use HAL;

      type Seen_Array is array (Button) of Boolean;

      function Count (A : Seen_Array) return Natural is
         Res : Natural := 0;
      begin
         for Elt of A loop
            if Elt then
               Res := Res + 1;
            end if;
         end loop;
         return Res;
      end Count;

      function Pass (A : Seen_Array) return Boolean
      is (not (for some B of A => not B));

      function First_Not_Seen (A : Seen_Array) return String is
      begin
         for B in A'Range loop
            if not A (B) then
               return B'Img;
            end if;
         end loop;

         return "PASS";
      end First_Not_Seen;

      Seen_Up, Seen_Down : Seen_Array := (others => False);

      type Hue_Index is mod 3;
      LED_Hues : constant array (Hue_Index) of LEDs.Hue :=
        (LEDs.Red, LEDs.Green, LEDs.Blue);
      Idx : Hue_Index := 0;
      Next_LED_Switch : WNM_HAL.Time_Microseconds := WNM_HAL.Clock;

      Seen_Touch_Low, Seen_Touch_High : Boolean := False;

      function All_Test_PASS return Boolean
      is (Pass (Seen_Up) and then Pass (Seen_Down)
          and then Seen_Touch_Low
          and then Seen_Touch_High);

   begin
      case Event.Kind is
         when A_Press =>
            --  We will capture the CPU here and not let it go
            while not All_Test_PASS and then not  WNM_HAL.Shutdown_Requested
            loop

               Screen.Clear;
               Draw_Str (Box_Left + 3, Box_Bottom - 34,
                         "Touch Low: " & (if Seen_Touch_Low
                                          then "PASS"
                                          else "NO"));
               Draw_Str (Box_Left + 3, Box_Bottom - 24,
                         "Touch High: " & (if Seen_Touch_High
                                           then "PASS"
                                           else "NO"));
               Draw_Str (Box_Left + 3, Box_Bottom - 16,
                         "Seen Up:" & Count (Seen_Up)'Img &
                           " " & First_Not_Seen (Seen_Up));
               Draw_Str (Box_Left + 3, Box_Bottom - 8,
                         "Seen Down:" & Count (Seen_Down)'Img &
                           " " & First_Not_Seen (Seen_Down));

               Screen.Update;

               LEDs.Turn_Off_All;
               LEDs.Set_Hue (LED_Hues (Idx));
               for B in LED loop
                  LEDs.Turn_On (B);
               end loop;
               LEDs.Update;

               if WNM_HAL.Clock > Next_LED_Switch then
                  Idx := Idx + 1;
                  Next_LED_Switch :=
                    WNM_HAL.Clock + WNM_HAL.Milliseconds (1000);
               end if;

               declare
                  State : constant WNM_HAL.Buttons_State := WNM_HAL.State;
               begin
                  for B in Button loop
                     if State (B) = Up then
                        Seen_Up (B) := True;
                     else
                        Seen_Down (B) := True;
                     end if;
                  end loop;
               end;

               declare
                  TP : constant WNM_HAL.Touch_Data :=
                    WNM_HAL.Touch_Strip_State;
               begin
                  if TP.Touch then
                     if TP.Value < 0.1 then
                        Seen_Touch_Low := True;
                     elsif TP.Value > 0.9 then
                        Seen_Touch_High := True;
                     end if;
                  end if;
               end;
               WNM_HAL.Delay_Milliseconds (30);
            end loop;

            --  Fill the screen and wait for shutdown
            Screen.Clear;
            Screen.Fill_Rect (((0, 0), Screen.Width, Screen.Height));
            Screen.Update;
            while not WNM_HAL.Shutdown_Requested loop
               WNM_HAL.Delay_Milliseconds (100);
            end loop;

            --  Wait for the battery to dtop below the threshold
            loop
               declare
                  Millivolts : constant Natural :=
                    WNM_HAL.Battery_Millivolts;

                  Goal : constant := 3220;
               begin
                  exit when Millivolts < Goal
                    or else
                      WNM_HAL.State (PAD_B) = Down;

                  Screen.Clear;
                  Draw_Str_Center (40, Millivolts'Img & " <" & Goal'Img);
                  Screen.Update;

                  WNM_HAL.Read_Battery_Voltage;
                  WNM_HAL.Delay_Milliseconds (100);
               end;
            end loop;

         when B_Press =>
            null;
         when others =>
            null;
      end case;
   end On_Event;

end WNM.GUI.Menu.Self_Test;
