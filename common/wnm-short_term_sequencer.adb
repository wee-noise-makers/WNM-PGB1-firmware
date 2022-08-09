-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with HAL; use HAL;

with WNM.Synth; use WNM.Synth;

package body WNM.Short_Term_Sequencer is

   type Any_Node_Index is range 0 .. MAX_EVENT_NUMBER;
   subtype Node_Index is Any_Node_Index range 1 .. Any_Node_Index'Last;

   Nodes : array (Node_Index) of aliased Event;
   Allocator : array (Node_Index) of Event_Access := (others => null);
   Alloc_Head : Node_Index := Node_Index'First;
   Alloc_Tail : Node_Index := Node_Index'First;
   Alloc_Full : Boolean := False;

   List_Head : Event_Access := null;
   Last_Insert : Event_Access := null;

   function Alloc return Event_Access;
   procedure Free (Ptr : in out Event_Access);
   procedure Insert (Node : not null Event_Access);

   -----------
   -- Alloc --
   -----------

   function Alloc return Event_Access is
      Ret : Event_Access;
   begin
      if Alloc_Full then
         return null;
      else

         Ret := Allocator (Alloc_Head);

         if Alloc_Head = Node_Index'Last then
            Alloc_Head := Node_Index'First;
         else
            Alloc_Head := Alloc_Head + 1;
         end if;

         Alloc_Full := Alloc_Head = Alloc_Tail;
         return Ret;
      end if;
   end Alloc;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : in out Event_Access) is
   begin
      if Ptr = Last_Insert then
         Last_Insert := null;
      end if;

      Ptr.Next := null;
      Allocator (Alloc_Tail) := Ptr;
      Ptr := null;

      if Alloc_Tail = Node_Index'Last then
         Alloc_Tail := Node_Index'First;
      else
         Alloc_Tail := Alloc_Tail + 1;
      end if;
      Alloc_Full := False;
   end Free;

   ------------
   -- Insert --
   ------------

   procedure Insert (Node : not null Event_Access) is
      Exp : constant Expiration_Time := Node.Expiration;

      procedure Insert_After (First : Event_Access);

      ------------------
      -- Insert_After --
      ------------------

      procedure Insert_After (First : Event_Access) is
         Cursor : Event_Access := First;
      begin
         while Cursor.Next /= null and then Cursor.Next.Expiration <= Exp loop
            Cursor := Cursor.Next;
         end loop;

         Node.Next := Cursor.Next;
         Cursor.Next := Node;
      end Insert_After;

   begin

      if Last_Insert /= null and then Last_Insert.Expiration <= Exp then

         --  The last insert is valid and earlier than the event we want to
         --  insert. Therefore we don't have to walk through all the list,
         --  we can start at the last insert.

         Insert_After (Last_Insert);

      elsif List_Head = null or else List_Head.Expiration > Exp then

         --  Head insert
         Node.Next := List_Head;
         List_Head := Node;
      else

         --  Insert somewhere after the first node
         Insert_After (List_Head);
      end if;

      Last_Insert := Node;
   end Insert;

   ----------
   -- Push --
   ----------

   procedure Push (D : Data; Expiration : Expiration_Time) is
      Node : constant Event_Access := Alloc;
   begin
      if Node = null then
         --  This event is discarded...
         return;
      end if;

      Node.D := D;
      Node.Expiration := Expiration;

      Insert (Node);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Now : Expiration_Time; D : out Data; Success : out Boolean) is
      Node : Event_Access;
   begin
      if List_Head /= null and then List_Head.Expiration <= Now then
         Node := List_Head;

         if Last_Insert = Node then
            Last_Insert := null;
         end if;

         List_Head := Node.Next;

         D := Node.D;
         Success := True;

         Free (Node);
      else
         Success := False;
      end if;
   end Pop;

   --  -----------------
   --  -- Print_Queue --
   --  -----------------
   --
   --  procedure Print_Queue is
   --     Node : Event_Access;
   --  begin
   --     Ada.Text_IO.Put_Line ("Short_Term_Sequencer Queue:");
   --     Node := List_Head;
   --     while Node /= null loop
   --        Ada.Text_IO.Put_Line (Node.Expiration'Img & " " & MIDI.Img (Node.D));
   --        Node := Node.Next;
   --     end loop;
   --     Ada.Text_IO.Put_Line ("---------------------------");
   --  end Print_Queue;

begin
   for Index in Node_Index loop
      Allocator (Index) := Nodes (Index)'Access;
   end loop;
end WNM.Short_Term_Sequencer;
