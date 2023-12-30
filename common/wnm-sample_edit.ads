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

with WNM.Sample_Library; use WNM.Sample_Library;
with WNM.Screen;

package WNM.Sample_Edit is

   procedure Load (Index : Valid_Sample_Index;
                   Start, Stop : Sample_Point_Index);

   function Start return Sample_Point_Index;
   function Stop return Sample_Point_Index;

   procedure Inc_Start;
   procedure Inc_Stop;

   procedure Dec_Start;
   procedure Dec_Stop;

   type Waveform_Point is delta 0.02 range 0.0 .. 1.0;
   type Waveform_Range is range 0 .. WNM.Screen.Width - 20;

   Waveform : array (Waveform_Range) of Waveform_Point;

   procedure Update_Waveform;

end WNM.Sample_Edit;
