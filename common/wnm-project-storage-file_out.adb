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

with System;
with Littlefs;
with Interfaces;
with Interfaces.C;

with WNM.File_System;

with Flux.Traits.LEB128;

package body WNM.Project.Storage.File_Out is

   use type Out_UInt;

   procedure Encode is new Flux.Traits.LEB128.Encode (Out_UInt,
                                                      File_System.Flux_Sink);

   Sink : File_System.Flux_Sink_Instance;

   function Write (Addr   : System.Address;
                   Len    : File_System.File_Size)
                   return Storage_Error;

   -----------
   -- Write --
   -----------

   function Write (Addr : System.Address;
                   Len  : File_System.File_Size)
                   return Storage_Error
   is
      use File_System;
      use Littlefs;
      use Interfaces;

      Res : File_Signed_Size;
   begin
      Res := File_System.Write (Addr, Len);

      if Res < 0 then
         case Interfaces.C.int (Res) is
            when LFS_ERR_IO | LFS_ERR_CORRUPT | LFS_ERR_NOENT |
                 LFS_ERR_EXIST | LFS_ERR_NOTDIR | LFS_ERR_ISDIR |
                 LFS_ERR_NOTEMPTY | LFS_ERR_BADF | LFS_ERR_FBIG |
                 LFS_ERR_INVAL | LFS_ERR_NOMEM |
                 LFS_ERR_NOATTR | LFS_ERR_NAMETOOLONG =>

               return Disk_Error;

            when LFS_ERR_NOSPC =>

               return Out_Of_Space;

            when others =>
               return Unknown_Error;
         end case;

      elsif File_Size (Res) /= Len then
         return Unknown_Error;
      else
         return Ok;
      end if;
   end Write;

   ----------
   -- Open --
   ----------

   function Open (Filename : String) return Instance is
   begin
      return Result : Instance do
         case File_System.Open_Write (Filename) is
            when File_System.Ok =>
               Result.Error := Ok;
            when others =>
               Result.Error := Unknown_Error;
         end case;
      end return;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (This : in out Instance) is
   begin
      File_System.Close;
   end Close;

   ------------
   -- Status --
   ------------

   function Status (This : Instance) return Storage_Error
   is (This.Error);

   ------------------
   -- Start_Global --
   ------------------

   procedure Start_Global (This : in out Instance) is
   begin
      This.Push (Global_Section);
   end Start_Global;

   --------------------------
   -- Start_Chord_Settings --
   --------------------------

   procedure Start_Chord_Settings (This : in out Instance; C : Chords) is
   begin
      This.Push (Chord_Section);
      This.Push (HAL.UInt32 (C'Enum_Rep));
   end Start_Chord_Settings;

   --------------------------
   -- Start_Track_Settings --
   --------------------------

   procedure Start_Track_Settings (This : in out Instance; T : Tracks) is
   begin
      This.Push (Track_Section);
      This.Push (HAL.UInt32 (T'Enum_Rep));
   end Start_Track_Settings;

   --------------------
   -- Start_Sequence --
   --------------------

   procedure Start_Sequence (This : in out Instance) is
   begin
      This.Push (Sequence_Section);
   end Start_Sequence;

   -------------------------
   -- Start_Step_Settings --
   -------------------------

   procedure Start_Step_Settings (This : in out Instance;
                                  S : Sequencer_Steps)
   is
   begin
      This.Push (Step_Section);
      This.Push (HAL.UInt32 (S'Enum_Rep));
   end Start_Step_Settings;

   ---------------------------
   -- Change_Pattern_In_Seq --
   ---------------------------

   procedure Change_Pattern_In_Seq (This : in out Instance; P : Patterns)
   is
   begin
      This.Push (Seq_Change_Pattern);
      This.Push (HAL.UInt32 (P'Enum_Rep));
   end Change_Pattern_In_Seq;

   -------------------------
   -- Change_Track_In_Seq --
   -------------------------

   procedure Change_Track_In_Seq (This : in out Instance; T : Tracks)
   is
   begin
      This.Push (Seq_Change_Track);
      This.Push (HAL.UInt32 (T'Enum_Rep));
   end Change_Track_In_Seq;

   -----------------
   -- End_Section --
   -----------------

   procedure End_Section (This : in out Instance) is
   begin
      This.Push (End_Of_Section);
   end End_Section;

   --------------
   -- End_File --
   --------------

   procedure End_File (This : in out Instance) is
   begin
      This.Push (End_Of_File);
   end End_File;

   --------------
   -- Push_Gen --
   --------------

   procedure Push_Gen (This : in out Instance; A : T) is
      Success : Boolean;
   begin
      if This.Status /= Ok then
         return;
      end if;

      pragma Compile_Time_Error (T'Last'Enum_Rep > Out_UInt'Last,
                                 "Invalid type size for storage output");

      pragma Compile_Time_Error (T'First'Enum_Rep < Out_UInt'First,
                                 "Invalid type size for storage output");

      --  pragma Compile_Time_Error (T'Last'Enum_Rep >= End_Of_Section_Value,
      --                             "Invalid type size for storage output");

      Encode (Sink, HAL.UInt32 (A'Enum_Rep), Success);

      if not Success then
         This.Error := Unknown_Error;
      else
         This.Error := Ok;
      end if;

   end Push_Gen;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : HAL.UInt32) is
      procedure Push_G is new Push_Gen (HAL.UInt32);
   begin
      Push_G (This, A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Character) is
      procedure Push_G is new Push_Gen (Character);
   begin
      Push_G (This, A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : String) is
   begin
      if A'Length >= Max_Str_Len_In_Storage then
         raise Program_Error with "String too long for storage";
      end if;
      This.Push (HAL.UInt32 (A'Length));
      This.Error := Write (A'Address, A'Length);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Beat_Per_Minute) is
      procedure Push_G is new Push_Gen (Beat_Per_Minute);
   begin
      Push_G (This, A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Step_Settings) is
      procedure Push_G is new Push_Gen (Step_Settings);
   begin
      Push_G (This, A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Track_Settings) is
      procedure Push_G is new Push_Gen (Track_Settings);
   begin
      Push_G (This, A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Chord_Setting_Kind) is
      procedure Push_G is new Push_Gen (Chord_Setting_Kind);
   begin
      Push_G (This, A);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance; A : Token_Kind) is
      procedure Push_G is new Push_Gen (Token_Kind);
   begin
      Push_G (This, A);
   end Push;

end WNM.Project.Storage.File_Out;
