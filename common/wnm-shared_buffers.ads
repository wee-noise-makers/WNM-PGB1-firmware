-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2024 Fabien Chouteau                    --
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

with WNM.Sample_Library; use WNM.Sample_Library;

with Tresses;

package WNM.Shared_Buffers is

   Sample_Rec_Buffer    : Sample_Audio_Data := (others => 0);
   Sample_Sector_Buffer : WNM_HAL.Storage_Sector_Data;

   procedure Init;
   procedure Init_From_Sample (Id : Valid_Sample_Index);

   procedure Record_Buffer (Input : WNM_HAL.Mono_Buffer);

   function Recorded_Length return Sample_Point_Count;

   procedure Save_Sample (Id   : Valid_Sample_Index;
                          Name : String);

   type Waveform_Point is range 0 .. 20;

   type Waveform_Index is range 1 .. 119;
   --  This range based on the pixel width we can use to draw the waveform

   type Waveform is array (Waveform_Index) of Waveform_Point;

   function Waveform_Data return Waveform;

   function Start_Point return Sample_Point_Count;
   function End_Point return Sample_Point_Count;

   function Start_Point_Index return Waveform_Index;
   function End_Point_Index return Waveform_Index;

   function Last_Played_Point_Index return Waveform_Index;

   procedure Move_Start_Point (Count : Integer);
   procedure Move_End_Point (Count : Integer);

   function Get_Point (Index : Sample_Point_Index) return Tresses.S16;

private

   Rec_Start_Point : Sample_Point_Index := Sample_Point_Index'First;
   Rec_End_Point   : Sample_Point_Index := Sample_Point_Index'First;
   Length      : Sample_Point_Count := 0;

   Play_Start_Offset : Sample_Point_Count := 0;
   Play_End_Offset   : Sample_Point_Count := 0;

   Last_Played_Offset : Sample_Point_Count := 0
     with Atomic, Volatile;
   --  This will be set by the Synth CPU

   Next_Wave_Cnt : Natural := 0;
   Wave_Segment_Count : constant Natural :=
     Natural (Sample_Point_Count'Last) / Natural (Waveform_Index'Last);

   Next_Wave : Waveform_Index := Waveform_Index'First;
   Wave_Data : Waveform := (others => 0);
   Wave_Acc  : Tresses.S32 := 0;

end WNM.Shared_Buffers;
