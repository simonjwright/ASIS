------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--            G N A T C H E C K . N A M E _ D I C T I O N A R Y             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2005-2012, AdaCore                      --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the dictionary to be used by Restrict_Name_Space rule.
--  Note that this dictionary and the corresponding checks are not
--  case-sensitive.

package Gnatcheck.Name_Dictionary is

   ------------------------------------
   -- Structure of a dictionary file --
   ------------------------------------

   --  A dictionary file is a plain text file. The length of the lines in this
   --  file is limited by 1024 characters. If the line is longer then 1024
   --  characters, the part of the line that follows 1024th character is not
   --  processed.

   --  Each line can be either an empty line, a comment line or it may contain
   --  a list of identifiers separated by space or HT characters. A comment
   --  is an Ada-style comment - it starts from '--' and ends till the end
   --  of a line. Identifiers should follow the Ada syntax for identifiers. A
   --  line containing identifier(s) may end up with a comment.

   --  The dictionary and the corresponding checks in this dictionary are not
   --  case-sensitive.

   procedure Scan_Dictionary_File
     (Parameter : String;
      Success   : out Boolean);
   --  Supposing that Parameter is a name of a dictionary file, scans this
   --  file and fills in the dictionary to be used by Restrict_Name_Space rule.
   --  Success is set ON if at least one name has been successfully read and
   --  placed in the dictionary from at least one dictionary file (may be, by
   --  another run of this subprogram

   procedure Add_To_Dictionary (Name : String);
   --  If Name is not in the dictionary, adds it. (All the names stored in
   --  the dictionary are folded to lower case!)

   function Name_In_Dictionary (Name : Wide_String) return Boolean;
   --  Checks if the argument is in the dictionary.
   --
   --  Note, that the argument is of Wide_String type, but the dictionary keeps
   --  values of String type. The reason to have a parameter of the Wide_String
   --  type is that we get the actual from ASIS as the defining identifier
   --  image, so it is of the Asis.Program_text subtype. Now we just convert
   --  parameter to the String type.

end Gnatcheck.Name_Dictionary;
