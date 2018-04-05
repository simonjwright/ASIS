------------------------------------------------------------------------
--                                                                    --
--                     McKae Software Utilities                       --
--                                                                    --
--           Copyright (C) 2005 McKae Technologies                    --
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

package McKae.XML.EZ_Out is

   --  This package is the parent package for a collection of packages
   --  that provide a simple means of XML output generation to a
   --  variety of output media.

   type Formatting_Options is
     (Continuous_Stream, -- No indenting, line breaks, or other
   --  extraneous whitespace.
   Spread_Indented -- Start and end tags are indented, and
   --  each resides on its own line.
   );

   Element_Not_Open : exception;
   --  An attempt was made to end, or add content to, an element when
   --  there were no open elements awaiting text or completion.

   Element_End_Mismatch : exception;
   --  The specified end tag does not match that of the currently open
   --  element.

   Nesting_Too_Deep : exception;
   --  The number of open, nested elements has exceeded the maximum
   --  level that was specified.

   Invalid_Construction : exception;
   --  An attempt was made to create a malformed document, such as
   --  inserting a process instruction into an open element.

end McKae.XML.EZ_Out;
