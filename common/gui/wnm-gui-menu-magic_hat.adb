-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2023 Fabien Chouteau                  --
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

with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.GUI.Bitmap_Fonts;

with HAL; use HAL;
with WNM.Chord_Settings;

package body WNM.GUI.Menu.Magic_Hat is

   package Scale_Choice_Next is new Enum_Next (Project.Scale_Choice,
                                               Wrap => True);
   use Scale_Choice_Next;

   package Valid_Scale_Choice_Next
   is new Enum_Next (Project.Valid_Scale_Choice,
                     Wrap => True);

   package Key_Next is new Enum_Next (Project.Key_Choice,
                                      Wrap => True);
   use Key_Next;

   package MIDI_Key_Next is new Enum_Next (MIDI.MIDI_Key);
   use MIDI_Key_Next;

   Singleton : aliased Instance;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Singleton'Access);
   end Push_Window;

   ------------
   -- To_Top --
   ------------

   function To_Top (S : Sub_Settings) return Top_Settings
   is (case S is
          when Hat_Scale => Magic_Hat,
          when Hat_Key   => Magic_Hat,
          when Prog_Scale => Progression,
          when Prog_Key   => Progression,
          when Prog_Id    => Progression);

   --------------------
   -- Get_Collection --
   --------------------

   function Get_Collection (Scale : Project.Valid_Scale_Choice) return
   not null access constant Chord_Settings.Progression_Collection
   is ((case Scale is
          when Project.Major => Chord_Settings.Major_Progressions'Access,
          when Project.Minor => Chord_Settings.Minor_Progressions'Access,
          when Project.Modal => Chord_Settings.Modal_Progressions'Access));

   ---------------
   -- Chord_Img --
   ---------------

   function Chord_Img (C : Chord_Settings.Roman_Numeral_Notation)
                       return String
   is
      use Chord_Settings;

      Major : constant Boolean :=
        (case C.Harmonic_Function is
            when Maj_Triad | Maj_7th | Maj_Inv1 | Maj_Inv2 |
                 Dim_Triad | Dim_7th | Chord_Settings.Sus2 |
                 Chord_Settings.Sus4 | Five => True,
            when others => False);

      Func : constant String :=
        (case C.Harmonic_Function is
            when Maj_Triad => "",
            when Min_Triad => "",
            when Dim_Triad => "" & Bitmap_Fonts.Dim,
            when Maj_7th   => "" & Bitmap_Fonts.Seven,
            when Min_7th   => "" & Bitmap_Fonts.Seven,
            when Dim_7th   => "" & Bitmap_Fonts.Dim & Bitmap_Fonts.Seven,
            when Min6      => "6",
            when Sus2      => "" & Bitmap_Fonts.Sus2,
            when Sus4      => "" & Bitmap_Fonts.Sus4,
            when Five      => "5",
            when Maj_Inv1  => "inv1",
            when Maj_Inv2  => "inv2",
            when Min_Inv1  => "inv1",
            when Min_Inv2  => "inv2");

   begin
      return Img (C.Accidental) & Img (C.Degree, Major) & Func;
   end Chord_Img;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This   : in out Instance) is
      Sub : constant Sub_Settings := This.Current_Setting;
      Top : constant Top_Settings := To_Top (Sub);
   begin
      Draw_Menu_Box
        ("Chord Progression",
         Count => Top_Settings_Count,
         Index => Top_Settings'Pos (To_Top (This.Current_Setting)));

      case Top is
         when Magic_Hat =>

            Draw_Title ("Magic Hat of Chords",
                        "Press A to load");

            Draw_Value_Pos (This.Hat_Scale'Img,
                            3,
                            Selected => Sub = Hat_Scale);
            Draw_Value_Pos (Project.Img (This.Hat_Key),
                            45,
                            Selected => Sub = Hat_Key);

            Draw_Magic_Hat (Box_Center.X + 25 + 10,
                            Box_Top + 13,
                            This.Hat_Anim_Step /= 0,
                            This.Hat_Anim_Step);
            if This.Hat_Anim_Step /= 0 then
               This.Hat_Anim_Step := @ - 1;
            end if;

         when Progression =>
            Draw_Title ("Load progression", "");

            Draw_Value_Pos (This.Prog_Scale'Img,
                            3,
                            Selected => Sub = Prog_Scale);
            Draw_Value_Pos (Key_Img (This.Prog_Key),
                            45,
                            Selected => Sub = Prog_Key);
            Draw_Value_Pos (This.Prog_Id'Img,
                            65,
                            Selected => Sub = Prog_Id);

            declare
               use Bitmap_Fonts;
               use WNM.Chord_Settings;
               use WNM.Project;

               Collection : constant not null access constant
                 Progression_Collection
                 := Get_Collection (This.Prog_Scale);

               Id : constant Natural :=
                 Natural (This.Prog_Id) mod Collection'Length;

               Prog : Roman_Numeral_Progression renames
                 Collection (Collection'First + Id).all;
               X : Natural := 3;
            begin
               for C of Prog loop
                  Print (X, Box_Center.Y, Chord_Img (C));
                  X := X + 2;
               end loop;
            end;
      end case;
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Instance;
                       Event : Menu_Event)
   is
      Sub : constant Sub_Settings := This.Current_Setting;
      Top : constant Top_Settings := To_Top (Sub);
   begin
      case Event.Kind is
         when Left_Press =>
            Prev (This.Current_Setting);
         when Right_Press =>
            Next (This.Current_Setting);

         when Up_Press =>
            case This.Current_Setting is
               when Hat_Scale =>
                  Next (This.Hat_Scale);
               when Hat_Key =>
                  Next (This.Hat_Key);
               when Prog_Scale =>
                  Valid_Scale_Choice_Next.Next (This.Prog_Scale);
               when Prog_Key =>
                  Next (This.Prog_Key);
               when Prog_Id =>
                  This.Prog_Id :=
                    (@ + 1) mod Get_Collection (This.Prog_Scale)'Length;
            end case;

         when Down_Press =>
            case This.Current_Setting is
               when Hat_Scale =>
                  Prev (This.Hat_Scale);
               when Hat_Key =>
                  Prev (This.Hat_Key);
               when Prog_Scale =>
                  Valid_Scale_Choice_Next.Prev (This.Prog_Scale);
               when Prog_Key =>
                  Prev (This.Prog_Key);
               when Prog_Id =>
                  This.Prog_Id :=
                    (@ - 1) mod Get_Collection (This.Prog_Scale)'Length;
            end case;

         when B_Press =>
            Menu.Pop (Exit_Value => Failure);

         when A_Press =>
            case Top is
               when Magic_Hat =>
                  This.Hat_Anim_Step := HAL.UInt32 (30 * 1);
                  Project.Randomly_Pick_A_Progression (This.Hat_Scale,
                                                       This.Hat_Key);
               when Progression =>
                  Project.Load_Progression (This.Prog_Scale,
                                            This.Prog_Key,
                                            Natural (This.Prog_Id));
            end case;

         when others =>
            null;
      end case;
   end On_Event;

end WNM.GUI.Menu.Magic_Hat;
