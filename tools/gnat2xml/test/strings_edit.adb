--                                                                    --
--  package Strings_Edit            Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  10:11 25 Jun 2005  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--____________________________________________________________________--

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Strings_Edit is
   function GetDigit (Symbol : Character) return Natural is
   begin
      case Symbol is
         when '0' => return 0;
         when '1' => return 1;
         when '2' => return 2;
         when '3' => return 3;
         when '4' => return 4;
         when '5' => return 5;
         when '6' => return 6;
         when '7' => return 7;
         when '8' => return 8;
         when '9' => return 9;
         when 'A' | 'a' => return 10;
         when 'B' | 'b' => return 11;
         when 'C' | 'c' => return 12;
         when 'D' | 'd' => return 13;
         when 'E' | 'e' => return 14;
         when 'F' | 'f' => return 15;
         when others => return 16;
      end case;
   end GetDigit;
--
-- Text_Edit
--
-- This is an internal package containing  implementation  of  all  text
-- editing subprograms. 
--
   package Text_Edit is
      function TrimCharacter
               (  Source : in String;
                  Blank  : in Character := ' '
               )  return String; 
      function TrimSet
               (  Source : in String;
                  Blanks : in Ada.Strings.Maps.Character_Set
               )  return String;
      procedure GetCharacter
                (  Source  : in String;
                   Pointer : in out Integer;
                   Blank   : in Character := ' '
                );
      procedure GetSet
                (  Source  : in String;
                   Pointer : in out Integer;
                   Blanks  : in Ada.Strings.Maps. Character_Set
                );
      procedure PutString
                (  Destination : in out String;
                   Pointer     : in out Integer;
                   Value       : in String;
                   Field       : in Natural := 0;
                   Justify     : in Alignment := Left;
                   Fill        : in Character := ' '
                );
      procedure PutCharacter
                (  Destination : in out String;
                   Pointer     : in out Integer;
                   Value       : in Character;
                   Field       : in Natural := 0;
                   Justify     : in Alignment := Left;
                   Fill        : in Character := ' '
                );
   end Text_Edit;
   package body Text_Edit is separate;

   function Trim
            (  Source : in String;
               Blank  : in Character := ' '
            )  return String renames Text_Edit.TrimCharacter;
   function Trim
            (  Source : in String;
               Blanks : in Ada.Strings.Maps.Character_Set
            )  return String renames Text_Edit.TrimSet;
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Blank   : in Character := ' '
             )  renames Text_Edit.GetCharacter;
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Blanks  : in Ada.Strings.Maps. Character_Set
             )  renames Text_Edit.GetSet;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in String;
                Field       : in Natural   := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  renames Text_Edit.PutString;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Character;
                Field       : in Natural   := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  renames Text_Edit.PutCharacter;

end Strings_Edit;
