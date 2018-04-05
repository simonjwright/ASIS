------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--              A S I S . E X T E N S I O N S . S T R I N G S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2004-2016, AdaCore                      --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your option)  any later --
-- version.  ASIS-for-GNAT  is  distributed  in  the  hope  that it will be --
-- useful,  but  WITHOUT ANY WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have  received  a copy of the  GNU General Public License and --
-- a copy of the  GCC Runtime Library Exception  distributed with GNAT; see --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package was a part of ASIS Utility Library (as ASIS_UL.Strings). It
--  has been moved into Asis.Extensions hierarchy to optimize dependencies on
--  GNATCOOL sources.

--  This package provides a string storage mechanism for varied length strings
--  Note that this mechanism is less efficient then direct using of access to
--  String values (for example GNAT.OS_Lib.String_Access). But from the other
--  side it allows to store and reuse varied length strings.

with Asis; use Asis;

package Asis.Extensions.Strings is

   Instance_SLOC_Txt : String := " instance at ";
   --  Part of the SLOC created for an element from expanded generic

   type String_Loc is record
      First, Last : Natural;
   end record;
   --  This record contains the start and end positions of a string inside
   --  a character table

   Nil_String_Loc : String_Loc := (0, 0);
   --  Corresponds to an empty string

   function Enter_String (S : String) return String_Loc;
   --  Stores a string in a character array, returning its starting and ending
   --  positions in a String_Loc structure

   function Get_String (SL : String_Loc) return String;
   --  Retrieves a string from a character array, based on its starting
   --  and ending positions supplied by SL

   function Is_Equal (S : String; SL : String_Loc) return Boolean;
   --  Checks if S is equal to a string represented by SL. Returns False if
   --  SL represents null string of if SL does not represent a string stored
   --  in the string storage.

   procedure Init;
   --  Resets the string table

   function Build_GNAT_Location_Old
     (For_Elem : Asis.Element;
      Line     : Natural := 0;
      Column   : Natural := 0)
      return     String_Loc;
   function Build_GNAT_Location
     (For_Elem : Asis.Element;
      Line     : Natural := 0;
      Column   : Natural := 0)
      return     String_Loc;
   --  Builds the string that describes the Element location in the form
   --  "file_name:line_number. For Elements from the instantiations
   --  the chain location_of_instantiation - location_in_template is created.
   --
   --  If For_Elem is Is_Part_Of_Implicit Elements and represents an implicit
   --  inherited subprogram or component thereof, the created SLOC has the
   --  format SLOC1(SLOC2), where SLOC1 is a SLOC of the corresponding
   --  explicit subprogram (or corresponding component thereof) from which
   --  this implicit subprogram has been inherited (directly or throufg a
   --  chain of other *implicit* derivations), and SLOC2 corresponds to the
   --  derived type declaration that is the "owner" of this implicit
   --  subprogram.
   --
   --  For any other Is_Part_Of_Implicit Element this function cannot produce
   --  any meaningful result.
   --
   --  Parameters Line and Column may have non-zero values only if the
   --  argument Element is not from expanded instantiation. If they are set,
   --  then the corresponding values are used as line and column numbers in
   --  the source location, otherwise line and column numbers are computed
   --  from the argument Element.
   --
   --  The the argument Element is from package Standard, the generated string
   --  is "Standard location", indepentently on the actuals for Line and
   --  Column parameters
   --
   --  Build_GNAT_Location_New differs from Build_GNAT_Location in the format
   --  of SLOC created for instantiations - instead of using square brackets is
   --  creates the chain that has the following structure:
   --
   --   SLOC_t instantiance at SLOC_i {instantiance at SLOC_i}
   --
   --  where SLOC_t is SLOC in template and SLOC_i - SLOC of the corresponding
   --  instantiation, from innermost to outermost when going from left to
   --  right.

   function Build_GNAT_Location_Old
     (For_Elem : Asis.Element;
      Line     : Natural := 0;
      Column   : Natural := 0)
      return     String;
   function Build_GNAT_Location
     (For_Elem : Asis.Element;
      Line     : Natural := 0;
      Column   : Natural := 0)
      return     String;
   --  Similar to the previous function, but this function returns the string
   --  result without allocating any information in the string storage.

   procedure Set_Full_Names (On : Boolean);
   --  After a call to this procedure with 'On => True' Build_GNAT_Location
   --  starts to use full file names in generated SLOCs, and after a call with
   --  'ON => False' Build_GNAT_Location starts to use short file names (usual
   --  GNAT style). Calls to this procedure does not affect
   --  Build_GNAT_Location_Old

   function Old_Format (SLOC : String) return String;
   --  If SLOC is the Source location pointing inside expended instantiation
   --  that uses new format (with "instance at") converts it into old format
   --  (with []). Otherwise returns the argument unchanged

   function SLOC_Less_Than (L, R : String) return Boolean;
--   function SLOC_Less_Than_New (L, R : String) return Boolean;
   --  Provided that L and R have the format of GNAT SLOC (that is,
   --  'file_name:line:col' compares these SLOCs alphabetically (first file
   --  names converted to lower case are compared, if they are equal, line
   --  numbers are compared, and if they are also eaual colons are compared).
   --  If L or R does not have the format of GNAT SLOC, the result is
   --  unpredictable.
   --
   --  SLOC_Less_Than expects the new format of SLOCs from expanded generics
   --  (with "instance at").

end Asis.Extensions.Strings;
