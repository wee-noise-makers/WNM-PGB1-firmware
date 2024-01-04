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

package WNM.GUI.Menu.Text_Dialog is

   procedure Push_Window;

   procedure Push_Window (Str : String);
   --  Push with a preset text

   Title_Max_Len : constant := 15;

   procedure Set_Title (Title : String)
     with Pre => Title'Length <= Title_Max_Len;

   function Value return String;

   type Valid_Character is
     ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
      'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',

      ' ', '_', '-', '(', ')', '[', ']', '{', '}', '<', '>', '!', '@',
      '#', '$', '%', '^', '&', '*', '+', '=',

      '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',

      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
      'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
     );

private

   subtype Text_Range is Natural range 1 .. 15;
   type Input_Text is array (Text_Range) of Valid_Character;

   type Text_Dialog_Window is new Menu_Window with record
      Dialog_Title : String (1 .. Title_Max_Len) := (others => ' ');
      Text          : Input_Text;
      Len           : Natural;
      Cursor        : Natural;
      Reset_On_Push : Boolean;
   end record;

   overriding
   procedure Draw (This   : in out Text_Dialog_Window);

   overriding
   procedure On_Event (This  : in out Text_Dialog_Window;
                       Event : Menu_Event);

   overriding
   procedure On_Pushed (This  : in out Text_Dialog_Window);

   overriding
   procedure On_Focus (This       : in out Text_Dialog_Window;
                       Exit_Value : Window_Exit_Value)
   is null;

   function To_String (This : Text_Dialog_Window) return String;

   --  These are a little bit of silly conversions. Even if we are using
   --  character literals in the definition of the Valid_Character type, the
   --  enumeration representation of the Valid_Character values do no match
   --  the enumeration representation of the standard Character type used in
   --  strings. So we need conversions.
   To_Char : constant array (Valid_Character) of Character :=
     ('A' => 'A',
      'B' => 'B',
      'C' => 'C',
      'D' => 'D',
      'E' => 'E',
      'F' => 'F',
      'G' => 'G',
      'H' => 'H',
      'I' => 'I',
      'J' => 'J',
      'K' => 'K',
      'L' => 'L',
      'M' => 'M',
      'N' => 'N',
      'O' => 'O',
      'P' => 'P',
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',
      ' ' => ' ',
      '_' => '_',
      '-' => '-',
      '(' => '(',
      ')' => ')',
      '[' => '[',
      ']' => ']',
      '{' => '{',
      '}' => '}',
      '<' => '<',
      '>' => '>',
      '!' => '!',
      '@' => '@',
      '#' => '#',
      '$' => '$',
      '%' => '%',
      '^' => '^',
      '&' => '&',
      '*' => '*',
      '+' => '+',
      '=' => '=',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',
      '0' => '0',
      'a' => 'a',
      'b' => 'b',
      'c' => 'c',
      'd' => 'd',
      'e' => 'e',
      'f' => 'f',
      'g' => 'g',
      'h' => 'h',
      'i' => 'i',
      'j' => 'j',
      'k' => 'k',
      'l' => 'l',
      'm' => 'm',
      'n' => 'n',
      'o' => 'o',
      'p' => 'p',
      'q' => 'q',
      'r' => 'r',
      's' => 's',
      't' => 't',
      'u' => 'u',
      'v' => 'v',
      'w' => 'w',
      'x' => 'x',
      'y' => 'y',
      'z' => 'z');
   To_Valid_Char : constant array (Character) of Valid_Character :=
     ('A' => 'A',
      'B' => 'B',
      'C' => 'C',
      'D' => 'D',
      'E' => 'E',
      'F' => 'F',
      'G' => 'G',
      'H' => 'H',
      'I' => 'I',
      'J' => 'J',
      'K' => 'K',
      'L' => 'L',
      'M' => 'M',
      'N' => 'N',
      'O' => 'O',
      'P' => 'P',
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',
      ' ' => ' ',
      '_' => '_',
      '-' => '-',
      '(' => '(',
      ')' => ')',
      '[' => '[',
      ']' => ']',
      '{' => '{',
      '}' => '}',
      '<' => '<',
      '>' => '>',
      '!' => '!',
      '@' => '@',
      '#' => '#',
      '$' => '$',
      '%' => '%',
      '^' => '^',
      '&' => '&',
      '*' => '*',
      '+' => '+',
      '=' => '=',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',
      '0' => '0',
      'a' => 'a',
      'b' => 'b',
      'c' => 'c',
      'd' => 'd',
      'e' => 'e',
      'f' => 'f',
      'g' => 'g',
      'h' => 'h',
      'i' => 'i',
      'j' => 'j',
      'k' => 'k',
      'l' => 'l',
      'm' => 'm',
      'n' => 'n',
      'o' => 'o',
      'p' => 'p',
      'q' => 'q',
      'r' => 'r',
      's' => 's',
      't' => 't',
      'u' => 'u',
      'v' => 'v',
      'w' => 'w',
      'x' => 'x',
      'y' => 'y',
      'z' => 'z',
      others => ' ');

end WNM.GUI.Menu.Text_Dialog;
