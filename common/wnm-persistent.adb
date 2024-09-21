-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2022 Fabien Chouteau                    --
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

with WNM.File_System.LEB128_File_Out; use WNM.File_System.LEB128_File_Out;
with WNM.File_System.LEB128_File_In; use WNM.File_System.LEB128_File_In;
with WNM.File_System; use WNM.File_System;

package body WNM.Persistent is

   Filename : constant String := "persistent.leb128";

   type Persistent_Token is (P_Last_Project,
                             P_Main_Volume,
                             P_Line_In_Mute,
                             P_Internal_Mic_Mute,
                             P_Headset_Mic_Mute,
                             P_Input_FX,
                             P_ADC_Volume,
                             P_TP1_Threshold,
                             P_TP2_Threshold,
                             P_TP3_Threshold);

   for Persistent_Token use (P_Last_Project      => 0,
                             P_Main_Volume       => 1,
                             P_Line_In_Mute      => 2,
                             P_Internal_Mic_Mute => 3,
                             P_Headset_Mic_Mute  => 4,
                             P_Input_FX          => 5,
                             P_ADC_Volume        => 6,
                             P_TP1_Threshold     => 7,
                             P_TP2_Threshold     => 8,
                             P_TP3_Threshold     => 9);

   ----------
   -- Save --
   ----------

   procedure Save is
      Output : File_System.LEB128_File_Out.Instance;
   begin
      Output.Open (Filename);

      for Token in Persistent_Token loop

         Output.Push (Out_UInt (P_Last_Project'Enum_Rep));

         exit when Output.Status /= Ok;

         case Token is
            when P_Last_Project =>
               Output.Push (Out_UInt (Data.Last_Project));
            when P_Main_Volume =>
               Output.Push (Out_UInt (Data.Main_Volume));
            when P_Line_In_Mute =>
               Output.Push (Data.Line_In_Mute);
            when P_Internal_Mic_Mute =>
               Output.Push (Data.Internal_Mic_Mute);
            when P_Headset_Mic_Mute =>
               Output.Push (Data.Headset_Mic_Mute);
            when P_Input_FX =>
               Output.Push (Data.Input_FX'Enum_Rep);
            when P_ADC_Volume =>
               Output.Push (Out_UInt (Data.ADC_Volume));
            when P_TP1_Threshold =>
               Output.Push (Out_UInt (Data.TP1_Threshold));
            when P_TP2_Threshold =>
               Output.Push (Out_UInt (Data.TP2_Threshold));
            when P_TP3_Threshold =>
               Output.Push (Out_UInt (Data.TP3_Threshold));
         end case;

         exit when Output.Status /= Ok;

      end loop;

      Output.Close;
   end Save;

   ----------
   -- Load --
   ----------

   procedure Load is
      procedure To_P_Token is new Convert_To_Enum (Persistent_Token);
      procedure Read is new Read_Gen_Enum (FX_Kind);
      procedure Read_Prj is new Read_Gen_Int (Project.Library.Prj_Index);
      procedure Read_U32 is new Read_Gen_Mod (HAL.UInt32);
      procedure Read_Volume is new Read_Gen_Int (Audio_Volume);

      Input : LEB128_File_In.Instance;
      Set : Persistent_Token;
      Raw : In_UInt;
      Success : Boolean;
   begin
      Input.Open (Filename);

      Data := Default;

      if Input.Status /= Ok then
         return;
      end if;

      loop
         Input.Read (Raw);

         exit when Input.Status /= Ok;

         To_P_Token (Raw, Set, Success);

         exit when not Success;

         case Set is
            when P_Last_Project =>
               Read_Prj (Input, Data.Last_Project);
            when P_Main_Volume =>
               Read_Volume (Input, Data.Main_Volume);
            when P_Line_In_Mute =>
               Input.Read (Data.Line_In_Mute);
            when P_Internal_Mic_Mute =>
               Input.Read (Data.Internal_Mic_Mute);
            when P_Headset_Mic_Mute  =>
               Input.Read (Data.Headset_Mic_Mute);
            when P_Input_FX =>
               Read (Input, Data.Input_FX);
            when P_ADC_Volume  =>
               Read_Volume (Input, Data.ADC_Volume);
            when P_TP1_Threshold  =>
               Read_U32 (Input, Data.TP1_Threshold);
            when P_TP2_Threshold  =>
               Read_U32 (Input, Data.TP2_Threshold);
            when P_TP3_Threshold  =>
               Read_U32 (Input, Data.TP3_Threshold);
         end case;

         exit when Input.Status /= Ok;
      end loop;

      WNM_HAL.Set_Main_Volume (Data.Main_Volume);
      WNM_HAL.Mute (WNM_HAL.Line_In, Data.Line_In_Mute);
      WNM_HAL.Mute (WNM_HAL.Headset_Mic, Data.Headset_Mic_Mute);
      WNM_HAL.Mute (WNM_HAL.Internal_Mic, Data.Internal_Mic_Mute);
      WNM_HAL.Set_Input_Volume (Data.ADC_Volume);
      WNM_HAL.Set_Thresholds (Data.TP1_Threshold,
                              Data.TP2_Threshold,
                              Data.TP3_Threshold);

      Input.Close;
   end Load;

end WNM.Persistent;
