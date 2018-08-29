------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                 G N A T C H E C K . R U L E S . T E X T                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2010-2017, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines gnatcheck rules dealing with source code elements
--  of the program (e.g. comments, ...)
--
--  The rules in this packages are ordered alphabetically

pragma Ada_2012;

package Gnatcheck.Rules.Text is

   ------------------------
   -- Annotated_Comments --
   ------------------------

   --  Flags comments of a special kind that are used as annotations or
   --  special sentinels/markers in a source code. As a "comment of a special
   --  kind" this rule considers comments having the following structure
   --
   --     --<special_character> <comment_marker>
   --
   --  where
   --
   --    <special_character>  - character indication that the comment is used
   --                           for a specific purpose (such as '#', '$', '|')
   --    <comment_marker>     - a word defining the kind of a specific comment
   --                           (kind of annotation)
   --
   --  It can be any number of white spaces (including zero) between
   --  <special_character> and <comment_marker>, but there is no white space
   --  between '--' and <special_character>. <comment_marker> does not contain
   --  any white space. <comment_marker> can be empty, in this case the rule
   --  flags all comment line that start from "--<special_character>" and
   --  do not contain any other character (except white spaces)
   --
   --  The rule has the following (mandatory) parameters for +R option:
   --
   --      S - String that is treated in the following way: the first character
   --          is treated as a special comment character, and the rest is
   --          treated as a comment marker. S should not contain white spaces.
   --
   --  -R option erases all the definitions of special comment annotations
   --  specified by the previous +R options.

   type Annotated_Comments_Rule_Type is new Text_Rule_Template
     with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Annotated_Comments_Rule_Type);
   --  Activates the rule with the following parameters:
   --
   --     #hide, #accept, %foo

   overriding procedure Line_Check
     (Rule               : in out Annotated_Comments_Rule_Type;
      Line_Num           :        Line_Number_Positive;
      Full_Line_Image    :        Program_Text_Access;
      Ada_Line_Image     :        Program_Text_Access;
      Comment_Line_Image :        Program_Text_Access;
      State              : in out Rule_Traversal_State);
   --  Checks if Comment_Line_Image can be interpreted as one of the special
   --  comment annotations indicated as a rule parameter

   overriding procedure Process_Rule_Parameter
    (Rule        : in out Annotated_Comments_Rule_Type;
     Param       :        String;
     Enable      :        Boolean;
      Defined_At : String);
   --  ??? Defined_At ???

   overriding procedure Print_Rule_To_File
     (Rule         : Annotated_Comments_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : Annotated_Comments_Rule_Type;
      Indent_Level : Natural := 0);

   overriding procedure Init_Rule (Rule : in out Annotated_Comments_Rule_Type);

   overriding procedure XML_Rule_Help
     (Rule  : Annotated_Comments_Rule_Type;
      Level : Natural);

   overriding function Rule_Option
     (Rule          : Annotated_Comments_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return          String;
   --  For Template_All_ON prints out rule option in the format "rule_name : #"

   overriding function More_Rule_Comment
     (Rule          : Annotated_Comments_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return          String;
   --  Adds the note that the rule parameter may be junk so it needs
   --  revising.

   Annotated_Comments_Rule : aliased Annotated_Comments_Rule_Type;

   ---------------------
   -- Printable_ASCII --
   ---------------------

   --  Flag source code text characters that are not part of the printable
   --  ASCII character set, a line feed, or a carriage return character (i.e.
   --  values 10, 13 and 32 .. 126 of the ASCII Character set).
   --
   --  If a code line contains more than one symbol that does not belong to the
   --  printable ASCII character set, the generated diagnosis points to the
   --  firts (leftmost) character and says that there are more in this line
   --
   --  This rule has no parameters.

   type Printable_ASCII_Rule_Type is new Text_Rule_Template with null record;

   overriding procedure Line_Check
     (Rule               : in out Printable_ASCII_Rule_Type;
      Line_Num           :        Line_Number_Positive;
      Full_Line_Image    :        Program_Text_Access;
      Ada_Line_Image     :        Program_Text_Access;
      Comment_Line_Image :        Program_Text_Access;
      State              : in out Rule_Traversal_State);
   --  Checks ???

   overriding procedure Init_Rule (Rule : in out Printable_ASCII_Rule_Type);

   Printable_ASCII_Rule : aliased Printable_ASCII_Rule_Type;

end Gnatcheck.Rules.Text;
