------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
--                                                                          --
--                                                                          --
--                Copyright (c) 2007, McKae Technologies.                   --
--                                                                          --
-- Avatox is free software; you can redistribute it and/or modify it        --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Avatox is distributed in the hope  that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- Avatox is maintained by McKae Technologies (http://www.mckae.com)        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Gnat.Regexp;
with Vatox.Traversal;

package Vatox.Environment is

   -- The source of the units that are being processed
   type Units_Needed  is (Error_Case,
                          -- Indicate an error in the argument specification
                          Unit_Only,
                          -- Just the specified unit
                          App_Only,
                          -- Only unit that are part of the application
                          Predefined
                          -- Units provided by the language or vendor
                         );

   -- Units that are members of which compilation dependencies
   type Unit_Breadths is (Error_Case,
                          -- Indicate an error in the argument specification
                          Single_Unit,
                          -- Just the specified unit
                          Full_Closure,
                          -- The closure of the unit (which must be a valid
                          --    main program
                          Supporting
                          -- Those units whose presence is required for the
                          --    unit to compile
                         );

   -- Expression filter information
   type Filter_Information is record
      F     : Gnat.Regexp.Regexp;
      -- The compiled filter

      Match : Boolean;
      -- Whether to filter for values that match (true) or don't match (false)
      -- the filter
   end record;

   -- A tautological equivalency function (i.e., always returns true) is used
   -- because nothing being doing to the regular expressions requires genuinely
   -- comparing them.
   function Always_Equal (L, R : Filter_Information) return Boolean;

   -- Instantiate a list of filtering expressions
   package Filter_Entry_Handling is
     new Ada.Containers.Vectors (Positive, Filter_Information, Always_Equal);

   -- Shorthand for the filters list.
   subtype Filtering_Entries is Filter_Entry_Handling.Vector;

   -- Instantiate a list for holding unit names
   package File_Name_Handling is
     new Ada.Containers.Ordered_Sets (Unbounded_String);

   -- Shorthand for the unit names list.
   subtype File_Name_Entries is File_Name_Handling.Set;

   -- Retrieve all the specified arguments and options from the command line,
   -- then check for any conflicts.
   procedure Process_Args_And_Options
     (The_Node_Information :    out Vatox.Traversal.Info_Node;
      Unit_Names           :    out File_Name_Entries;
      Output_Filename      :    out Unbounded_String;
      Delete_Trees         : in out Boolean;
      Asis_Params          : in out Unbounded_String;
      Refed_Units          : in out Units_Needed;
      Unit_Breadth         : in out Unit_Breadths;
      Axf_Directory        : in out Unbounded_String;
      Multiple_Files       :    out Boolean;
      Filter_Entries       :    out Filtering_Entries;
      Continue             :    out Boolean);

   -- Output the command line USAGE arguments and options.
   procedure Show_Usage;

   -- Whether the unit name pass the set of filters.
   function Passes_Filter (Unit_Name : String;
                           -- Unit name to pass through the filter(s)

                           Filter    : Filtering_Entries
                           -- Set of filters to subject the unit name to
                          ) return Boolean;

   -- Given the list of filename specifications (which can be specific names or
   -- wildcarded), get the list of primary files that match the specification.
   procedure Collect_Primary_Files
     (File_Names        : in     File_Name_Entries;
      -- Filename specifications, explicit and wildcarded

      Primary_Filenames :    out File_Name_Entries
      -- List of filenames that match the specification
     );

end Vatox.Environment;
