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

with WNM.Step_Event_Broadcast;

with Ada.Text_IO;

package body WNM.Project.Song_Part_Sequencer is

   Playing_Part : WNM.Parts := WNM.Parts'First;
   Origin       : WNM.Parts := WNM.Parts'First;

   procedure Step_Callback;

   Step_Listener : aliased Step_Event_Broadcast.Listener
     (Step_Callback'Access);

   G_Steps_Count : Natural := 0;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Origin := G_Project.Part_Origin;
      Playing_Part := Origin;
      G_Steps_Count := 0;
   end Start;

   -------------------
   -- Step_Callback --
   -------------------

   procedure Step_Callback is
   begin
      G_Steps_Count := G_Steps_Count + 1;

      if G_Steps_Count >= 16 then
         if G_Project.Part_Origin /= Origin then
            --  New origin, play this next
            Origin := G_Project.Part_Origin;
            Playing_Part := Origin;

         elsif G_Project.Parts (Playing_Part).Link then
            --  There's link
            Playing_Part := @ + 1;
         else
            --  Start back to origin
            Playing_Part := G_Project.Part_Origin;
         end if;

         Ada.Text_IO.Put_Line ("Going to Part" & Playing_Part'Img);
         G_Steps_Count := 0;
      end if;
   end Step_Callback;

   -------------
   -- Playing --
   -------------

   function Playing return Parts
   is (Playing_Part);

begin
   Step_Event_Broadcast.Register (Step_Listener'Access);
end WNM.Project.Song_Part_Sequencer;
