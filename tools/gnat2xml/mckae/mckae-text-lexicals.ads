------------------------------------------------------------------------
--                                                                    --
--                     McKae Software Utilities                       --
--                                                                    --
--           Copyright (C) 2006 McKae Technologies                    --
--                                                                    --
-- The  McKae   software  utilities   are  free  software;   you  can --
-- redistribute it  and/or modify it  under terms of the  GNU General --
-- Public  License  as published  by  the  Free Software  Foundation; --
-- either version  2, or (at  your option) any later  version.  McKae --
-- Software Utilities are  distributed in the hope that  they will be --
-- useful,  but  WITHOUT  ANY  WARRANTY;  without  even  the  implied --
-- warranty of  MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE. --
-- See the GNU  General Public License for more  details.  You should --
-- have received a copy of the GNU General Public License distributed --
-- with DTraq; see file COPYING.   If not, write to the Free Software --
-- Foundation, 59  Temple Place -  Suite 330, Boston,  MA 02111-1307, --
-- USA.                                                               --
--                                                                    --
-- As a  special exception, if other files  instantiate generics from --
-- this unit,  or you link this  unit with other files  to produce an --
-- executable,  this unit  does  not by  itself  cause the  resulting --
-- executable to be covered by  the GNU General Public License.  This --
-- exception does  not however invalidate  any other reasons  why the --
-- executable file might be covered by the GNU Public License.        --
--                                                                    --
-- The McKae Software Utilities  are maintained by McKae Technologies --
-- (http://www.mckae.com).                                            --
------------------------------------------------------------------------

package McKae.Text.Lexicals is

   --  Utilities for manipulating software lexical elements, such as
   --  identifiers.

   --  Capitalization choices

   type Capitalizations is (nO_cHaNGe, UPPER, lower, Capitalized, camelBack);

   --  Some convenience definitions
   Ada_Ident    : constant Capitalizations := Capitalized;
   Ada_Reserved : constant Capitalizations := lower;
   Ada83_Ident  : constant Capitalizations := UPPER;

   CPP_Ident    : constant Capitalizations := camelBack;
   Cpp_Reserved : constant Capitalizations := lower;

   XML_Common   : constant Capitalizations := camelBack;
   XML_Elements : constant Capitalizations := camelBack;
   XML_Attrs    : constant Capitalizations := camelBack;

   --  Transform a string in accordance with the specified capitalization, and
   --  also optionally remove underscores from the element.  The application
   --  must handle any issues with different strings ending up looking the
   --  same.  Note for Capitalized and Camelback: This function cannot in and
   --  of itself find the words in the element to capitalize, they must be
   --  delimited by the start of the element, underscores, and the end of the
   --  element.
   function Transform
     (S : String;
   --  The string to transform

      Capitalization : Capitalizations;
   --  Which capitalization approach to use

      Remove_Underscores : Boolean := False
   --  Whether to remove underscores from the element
   )
     return String;

end McKae.Text.Lexicals;
