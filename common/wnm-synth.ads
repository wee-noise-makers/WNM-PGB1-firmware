-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2017 Fabien Chouteau                  --
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

with Interfaces;

with WNM;
with WNM.Sample_Library;
with WNM.Time;
with WNM.Audio;
with WNM.MIDI;

package WNM.Synth is

   type Sample_Time is new Interfaces.Unsigned_64;

   function Sample_Clock return Sample_Time;
   --  How many audio samples have been sent to the DAC so far.
   --  This number can be used to count time between two events.

   procedure Trig (Track : WNM.Tracks);

   procedure Event (Msg : MIDI.Message);

   procedure Change_Pan (Track : WNM.Tracks;
                         Pan   : Integer);
   function Pan (Track : WNM.Tracks) return Integer;

   procedure Change_Volume (Track  : WNM.Tracks;
                            Volume : Integer);
   function Volume (Track : WNM.Tracks) return Natural;

   function Update return WNM.Time.Time_Microseconds;

   procedure Next_Points (Output : out Audio.Stereo_Buffer;
                          Input  :     Audio.Stereo_Buffer);

   procedure Set_Passthrough (Kind : Audio.Input_Kind);
   function Get_Passthrough return Audio.Input_Kind;

   ---------------
   -- Recording --
   ---------------

   type Rec_Source is (None, Line_In, Master_Output);

   function Now_Recording return Rec_Source;

   procedure Start_Recording (Filename : String;
                              Source   : Rec_Source;
                              Max_Size : Positive)
     with Pre => Now_Recording = None and then Source /= None;

   procedure Stop_Recording
     with Post => Now_Recording = None;

   function Record_Size return Natural;
   --  with Pre => Now_Recording /= None;

end WNM.Synth;
