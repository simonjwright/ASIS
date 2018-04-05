------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                 A S I S _ U L . W I D E _ S T R I N G S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING.  If  not,  write  to  the Free Software Foundation, 51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a string storage mechanism for varied length wide
--  strings similar to what is defined in ASIS_UL.Strings package. Unlike to
--  ASIS_UL.Strings, it does not include any routines for computing strings
--  representing GNAT SLOC, this is why we define these two packages
--  separately, but not as the instantiations of the common generic that
--  defines a string storage

package ASIS_UL.Wide_Strings is

   type Wide_String_Loc is record
      First, Last : Natural;
   end record;
   --  This record contains the start and end positions of a string inside
   --  a (wide) character table

   Nil_Wide_String_Loc : Wide_String_Loc := (0, 0);
   --  Corresponds to an empty string

   function Enter_Wide_String (S : Wide_String) return Wide_String_Loc;
   --  Stores a string in a character array, returning its starting and ending
   --  positions in a String_Loc structure

   function Get_Wide_String (SL : Wide_String_Loc) return Wide_String;
   --  Retrieves a string from a character array, based on its starting
   --  and ending positions supplied by SL

   function Is_Equal (S : Wide_String; SL : Wide_String_Loc) return Boolean;
   --  Checks if S is equal to a string represented by SL. Returns False if
   --  SL represents null string of if SL does not represent a string stored
   --  in the string storage.

   procedure Init;
   --  Resets the string table

end ASIS_UL.Wide_Strings;
