------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                         A S I S _ U L . M I S C                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2006-2016, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains various useful resources for which we can not find
--  at the moment a specific ASIS Utility Library package to place into

with GNAT.Lock_Files; use GNAT;

package ASIS_UL.Misc is

   --------------------------
   -- File list processing --
   --------------------------

   generic
      with procedure Process_File (Fname : String);
   procedure Parse_File_List (File_List_Name : String);
   --  Provided that File_List_Name is a name of some existing file, and
   --  assuming that this file contains a list of source file names, this
   --  procedure parses the argument file and applies Process_File to each file
   --  name.
   --
   --  This procedure assumes that the list of source file names in the
   --  argument file has the following structure:
   --  - file names are separated by white spaces, line breaks, page breaks,
   --    end of file;
   --  - if a file name contains spaces, it should be surrounded by string
   --    quotes.

   -----------------------
   -- String Hash table --
   -----------------------

   --  This is a slight modification from the Hash function from the GNAT Namet
   --  package. The way of computing the hash values is exactly the same as
   --  used in Namet.Hash, but we have changed the interface. There are three
   --  modifications. First, the original GNAT Hash function does not have a
   --  formal parameter and operates on an internal string buffer, but here we
   --  have added a parameter. Second - our hash function is not
   --  case-sensitive. And third, we use a wrapper generic package to give the
   --  clients the possibility to use hash tables for different string entry
   --  IDs types

   generic
      type Entry_Id is (<>);
   package String_Hash_Table is

      type Int is range -2 ** 31 .. +2 ** 31 - 1;

      Hash_Num : constant Int := 2**12;
      --  Number of headers in the hash table. Current hash algorithm is
      --  closely tailored to this choice, so it can only be changed if a
      --  corresponding change is made to the hash algorithm.

      Hash_Max : constant Int := Hash_Num - 1;
      --  Indexes in the hash header table run from 0 to Hash_Num - 1

      subtype Hash_Index_Type is Int range 0 .. Hash_Max;
      --  Range of hash index values

      Hash_Table : array (Hash_Index_Type) of Entry_Id;
      --  The hash table is used to locate existing entries in the strings
      --  table. The entries point to the first strings table entry whose hash
      --  value matches the hash code. Then subsequent string table entries
      --  with the same hash code value should be linked, and this link should
      --  be used for locating the needed entry

      function Hash (Name : String) return Hash_Index_Type;
      --  Compute hash code for its argument

   end String_Hash_Table;

   ------------------------------
   -- Simple String dictionary --
   ------------------------------

   generic
      Dictionary_Name : String;
   package Simple_String_Dictionary is
      --  This package defines a simple string dictionary. The dictionary
      --  entries are various strings, each string can be included in the
      --  dictionary only once. The dictionary is not case-sensitive. The
      --  initial state of the dictionary is empty

      procedure Add_To_Dictionary (S : String);
      --  If the dictionary does not contain S, adds S to the dictionary,
      --  Otherwise does nothing.

      procedure Remove_From_Dictionary (S : String);
      --  If the dictionary does not contain S, removes S from the dictionary,
      --  Otherwise does nothing.

      function Is_In_Dictionary (S : String) return Boolean;
      --  Checks if S is in the dictionary

      function Is_Empty return Boolean;
      --  Returns True if the dictionary contains no entries, otherwise returns
      --  False

      procedure Clear;
      --  Removes all the entries from the dictionary

      procedure Reset_Iterator;
      function Next_Entry return String;
      function Done return Boolean;
      --  These three routines implement iterator that allows to get all the
      --  dictionary entries. If a client adds or removes entries to/from a
      --  dictionary while using the iterator, the iterator behavior is
      --  erroneous.

      procedure Print_Dictionary;
      --  Prints into Stderr the content of the dictionary. Each entry is
      --  printed on a new line and is surrounded by ">>" and "<<". (To be
      --  used for debugging purposes).

   end Simple_String_Dictionary;

   --------------------------------------
   --  ASIS string processing routines --
   --------------------------------------

   --  Below there are the modified versions of the standard character and
   --  string processing routines.

   function ASIS_Trim (Source : String) return String;
   function ASIS_Trim (Source : Wide_String) return Wide_String;
   --  Similar to Ada.Strings.Fixed.Trim, but cuts out all the Is_White_Space
   --  characters (that is, both spaces and HTs)

   type Direction  is (Forward, Backward);
   --  ??? Or should we reuse Ada.Strings.Direction

   function ASIS_Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward)
      return Natural;
   function ASIS_Index_Non_Blank
     (Source : Wide_String;
      Going  : Direction := Forward)
      return Natural;
   --  Similar to Ada.Strings.Fixed.Trim, but cuts out all the Is_White_Space
   --  characters (that is, both spaces and HTs)

   function Proper_Case (S : String) return String;
   --  Converts the casing of the argument into proper case: a first character
   --  and each Is_Letter character that follows the underscore are converted
   --  to upper case, all the other Is_Letter characters are converted to
   --  lower case

   function Remove_Spaces (S : String) return String;
   --  Removes all the white spaces from the argument

   function To_Lower_Case (S : Wide_String) return Wide_String;
   function To_Upper_Case (S : Wide_String) return Wide_String;
   --  Folds the argument to lower/upper case, may be used for (wide) string
   --  normalization before comparing strings if the casing is not important
   --  for comparison.

   ------------------
   -- File Locking --
   ------------------

   --  Wrappers for GNAT.Lock_Files operations. See that package for
   --  documentation.

   procedure Lock_File (Lock_File_Name : Lock_Files.Path_Name);
   procedure Unlock_File (Lock_File_Name : Lock_Files.Path_Name);

   Lock_Error : exception renames Lock_Files.Lock_Error;

   -------------------
   -- Miscellaneous --
   -------------------

   function Is_White_Space (Ch  : Character)      return Boolean;
   function Is_White_Space (WCh : Wide_Character) return Boolean;
   --  Checks if the argument is either a space or HT character

   function Image (I : Integer) return String;
   --  Returns the string image of I, with no leading or trailing spaces

   function Is_Identifier (S : Wide_String) return Boolean;
   --  Checks if S has a syntax of an Ada identifier

   function Is_Ada_Name (S : Wide_String) return Boolean;
   --  Checks if S has a syntax of an Ada (expanded) name consisting on
   --  identifiers only (and dots) with no white spaces inside. The caller is
   --  responsible for cutting off all the leading and trailing white spaces
   --  from the parameter.

   function Is_Identifier_Prefix (Prefix : Wide_String) return Boolean;
   function Is_Identifier_Suffix (Suffix : Wide_String) return Boolean;
   --  Checks if the argument string can be a prefix/suffix of a valid Ada
   --  identifier. Returns True if the argument is an empty string.

   function Get_Nat_Switch_Parameter (Val : String) return Natural;
   --  This function is supposed to be used as a part of tool parameters
   --  processing. It computes a natural value from its string representation
   --  and raises ASIS_UL.Common.Parameter_Error if Val can not be considered
   --  as a string image of a natural number.

end ASIS_UL.Misc;
