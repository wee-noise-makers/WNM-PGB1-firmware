-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2024 Fabien Chouteau                  --
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

with WNM.Project;
with WNM.Audio_Routing;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Screen;
with WNM.GUI.Bitmap_Fonts;
with headset_icon;
with line_in_icon;
with internal_mic_icon;

package body WNM.GUI.Menu.Audio_Input_Select is

   package Inputs_Next
   is new Enum_Next (WNM_HAL.Audio_Input_Kind);
   use Inputs_Next;

   Input_Select : aliased Input_Select_Window;

   ------------
   -- Update --
   ------------

   procedure Update_Inputs (This : in out Input_Select_Window) is
   begin
      --  Skip headset mix if there is no headset/headphones detected
      if This.Sample_Rec_Input = Headset_Mic and then not WNM_HAL.HP_Detect
      then
         Next (This.Sample_Rec_Input);
      end if;

      WNM.Audio_Routing.Select_Sampling_Input (This.Sample_Rec_Input);
   end Update_Inputs;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Input_Select'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Input_Select_Window)
   is
      Input : constant Audio_Input_Kind := This.Sample_Rec_Input;

      Icon_Size : constant := 17;

      Icons_Y : constant :=
        Drawing.Box_Bottom - Bitmap_Fonts.Height - Icon_Size - 2;

      L_Icon_X : constant := Drawing.Box_Left + 10;
      M_Icon_X : constant := L_Icon_X + Icon_Size + 3;
      R_Icon_X : constant := M_Icon_X + Icon_Size + 3;

      Select_X : constant Natural := (case Input is
                                         when Line_In => L_Icon_X,
                                         when Internal_Mic => M_Icon_X,
                                         when Headset_Mic => R_Icon_X);
   begin

      Draw_Menu_Box ("Input Select", 1, 0);

      Draw_Volume (Id => WNM.Project.D,
                   Value => Audio_Routing.Get_ADC_Volume,
                   Label => "VOL",
                   Selected => True);

      Draw_Str (L_Icon_X,
                Icons_Y + Icon_Size + 3,
                (case Input is
                    when Line_In      => "Line-in",
                    when Headset_Mic  => "Headset",
                    when Internal_Mic => "Internal Mic"));

      Screen.Copy_Bitmap (line_in_icon.Data,
                          L_Icon_X, Icons_Y,
                          Invert_Color => Input = Line_In);
      Screen.Copy_Bitmap (internal_mic_icon.Data,
                          M_Icon_X, Icons_Y,
                          Invert_Color => Input = Internal_Mic);
      Screen.Copy_Bitmap (headset_icon.Data,
                          R_Icon_X, Icons_Y,
                          Invert_Color => Input = Headset_Mic);

      Screen.Draw_Rect (((Select_X - 1, Icons_Y - 1),
                        Icon_Size + 2, Icon_Size + 2));

      if not WNM_HAL.HP_Detect then
         Screen.Draw_Line ((R_Icon_X, Icons_Y),
                           (R_Icon_X + Icon_Size, Icons_Y + Icon_Size));

         Screen.Draw_Line ((R_Icon_X + Icon_Size, Icons_Y),
                           (R_Icon_X, Icons_Y + Icon_Size));
      end if;
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Input_Select_Window;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when A_Press =>
            Menu.Pop (Exit_Value => Success);

         when B_Press =>
            Menu.Pop (Exit_Value => Failure);

         when Right_Press =>
            Next (This.Sample_Rec_Input);
            Update_Inputs (This);

         when Left_Press =>
                  Prev (This.Sample_Rec_Input);
                  Update_Inputs (This);

         when Up_Press =>
            WNM.Audio_Routing.Change_ADC_Volume (1);

         when Down_Press =>
            WNM.Audio_Routing.Change_ADC_Volume (-1);

         when others =>
            null;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed
     (This  : in out Input_Select_Window)
   is
   begin
      Update_Inputs (This);
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus
     (This       : in out Input_Select_Window;
      Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Audio_Input_Select;
