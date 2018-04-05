------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                 G N A T C H E C K . C A T E G O R I E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

--  This package defines the rule categories hierarchy

with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Gnatcheck.Ids;     use Gnatcheck.Ids;
with Gnatcheck.Options; use Gnatcheck.Options;

package Gnatcheck.Categories is

   --  ????

   function No      (Id : Category_Id) return Boolean;
   function Present (Id : Category_Id) return Boolean;
   --  Check if the the argument represents an existing category

   subtype Category_Name_Str is String_Access;

   type Category is record
      Name        : Category_Name_Str;
      --  Category name, only short names are stored

      Parent      : Category_Id := No_Category;
      Next        : Category_Id := No_Category;
      First_Child : Category_Id := No_Category;

      Self_Id     : Category_Id := No_Category;
      --  This is the Id of the reference to the category in the category table

      First_Rule  : Rule_Id     := No_Rule;
   end record;

   type Category_Access is access all Category;

   Nil_Category : constant Category :=
     (Name        => null,
      Parent      => No_Category,
      Next        => No_Category,
      First_Child => No_Category,
      Self_Id     => No_Category,
      First_Rule  => No_Rule);

   Root_Category : aliased Category := Nil_Category;
   --  This is the root of the category hierarchy. The root does not have a
   --  name.

   procedure Add_Category
     (New_Category      : Category_Access;
      New_Category_Name : String;
      As_Subcateory_Of  : Category_Access);
   --  Adds a new category to the category hierarchy. New_Category_Name
   --  specifies the name of the new category (leading and trailing spaces are
   --  cut out, if any). If a category with the same name already exists at the
   --  given level of hierarchy, or if the parent (denoted by the actual for
   --  As_Subcateory_Of) is not set or if the parent is not already a part of
   --  hierarchy, or if the name of the category is not specified, Fatal_Error
   --  is raised.

   procedure Category_Help
     (Category_Name : String        := "";
      From_Status   : Rule_Statuses := Fully_Implemented;
      Recursively   : Boolean       := False;
      Level         : Natural       := 0);
   --  Prints into Sdterr the help info for the category indicated by
   --  Category_Name (or for a root category if the actual for it is an empty
   --  string).
   --  If Recursively is ON, prints out recursively all the subordinate
   --  categories, if any, otherwise prints only the rules and subcategories
   --  belonging to the argument category, without getting into the details of
   --  each subcategory.
   --  If Only_Documented_Rules is ON, only the rules that have the status of
   --  Fully_Implemented are printed out, otherwise both Fully_Implemented and
   --  Non_Documented rules are included in the output
   --  Level defines the indentation level of the output

   procedure XML_Categories_Help;
   --  Prints out the rule help (organized by categories) in XML format for
   --  GPS needs.

   procedure Print_Category_Hierarchy;
   --  Prints into Stderr the debug dump of the category hierarchy

   procedure Pretty_Print_Category_Hierarchy;
   --  Prints into Stderr the a nice-looking image of the category hierarchy
   --  !!! To be removed when the interface help will be completely rewritten
   --  for categories!

   procedure Process_Category_Help_Parameter
     (P       : String;
      Success : out Boolean);
   --  Scans P that is supposed to contain parameters of '-hc' option and sets
   --  the corresponding option flags. Success is set to True if P is a valid
   --  combination of parameters and False otherwise

   ------------------------------------------------
   --  Access routines for categories properties --
   ------------------------------------------------

   --  All the routines in this section assimes that their argument represents
   --  some existing category (Present (Id))

   function Category_Name (Id : Category_Id) return String;
   --  Returns the name of the category. Returns an empty string for
   --  Root_Category_Id.

   function Parent_Category (Id : Category_Id) return Category_Id;
   --  Returns the Id of the parent category. returns No_Category for
   --  Root_Category_Id.

   function Next (Id : Category_Id) return Category_Id;
   --  Return the next category at the same level. Returns No_Category if there
   --  is no  next category.

   function First_Child (Id : Category_Id) return Category_Id;
   --  Return the first child subcategory of the argument category. Returns
   --  No_Category if there is no  next category.

   function First_Rule_In_Category
     (Id          : Category_Id;
      From_Status : Rule_Statuses := Fully_Implemented)
      return        Rule_Id;
   --  Returns the first rule in the category that has the status not less than
   --  From_Status. Returns No_Rule if there is no such rule.

   function Nesting_Level (Id : Category_Id) return Natural;
   --  Returns the nesting level of the argument categoty in the category
   --  hierarchy. The root category has the nesting level of 0.

   -------------------------
   -- Cantegory hierarchy --
   -------------------------

   Metrics : aliased Category;
   --  For metric-specific checks

   Spark : aliased Category;
   --  Rules derived from the definition of Sparc Ada subset

   Feature_Use_Detection : aliased Category;
   --  Rules that are no more than just feature use detectors

   ----------------------------------------------
   -- Based of 'Ada 95 Quality and Style' book --
   ----------------------------------------------

   Ada_95_Quality_and_Style : aliased Category;
   --  Umbrella for the categories defined in 'Ada_95_Q&S' book

   Source_Code_Presentation : aliased Category;
   --  Compiler style checks should go here

   Readability : aliased Category;

   Program_Structure : aliased Category;

   Programming_Practices : aliased Category;

   Concurrency : aliased Category;

   Portability  : aliased Category;

   Object_Oriented_Features : aliased Category;

end Gnatcheck.Categories;
