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

with WNM.Project.Storage;

package WNM.Project.Library is

   subtype Prj_Entry_Name is String (1 .. 15);

   subtype Prj_Index is Natural range 0 .. 20;
   subtype Valid_Prj_Index is Prj_Index range 1 .. Prj_Index'Last;

   Invalid_Prj_Entry : constant Prj_Index := Prj_Index'First;

   function Last_Loaded return Prj_Index;

   function Has_Project (Index : Prj_Index) return Boolean;

   function Entry_Name (Index : Valid_Prj_Index) return Prj_Entry_Name;

   procedure Rename (Index : Valid_Prj_Index; Name : String);

   procedure Delete (Index : Valid_Prj_Index);

   procedure Load_Library;

   function Load_Project (Index : Valid_Prj_Index)
                          return Storage.Storage_Error;

   function Save_Project (Index : Valid_Prj_Index)
                          return Storage.Storage_Error;

   function Save_Project_With_Name (Index : Valid_Prj_Index;
                                    Name  : String)
                                    return Storage.Storage_Error;

end WNM.Project.Library;
