------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                   G N A T C H E C K . C O M P I L E R                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

--  This package defines "rules" for getting the information for the GNATCHECK
--  report from the check performed by the compiler.

with Ada.Text_IO;          use Ada.Text_IO;

with ASIS_UL.Source_Table; use ASIS_UL.Source_Table;

package Gnatcheck.Compiler is

   --------------------------------------------------------
   -- Using in GNATCHECK checks performed by the compiler --
   --------------------------------------------------------

   --  The compiler already performs a lot of checks that may be of interest
   --  for code certification and, therefore, it would be nice to perform
   --  these checks as a part of GNATCHECK run and to incorporate the results
   --  of these checks into the GNATCHECK report

   --  There are three kinds of the compiler-performed checks:
   --
   --  - checks initiated by the pragma Restriction_Warnings
   --  - style checks;
   --  - checks resulted in general (non-style) warnings;

   --  GNATCHECK uses three parametric rules to activate these checks:
   --  RESTRICTIONS, STYLE_CHECKS and WARNINGS, the parameters of these rules
   --  specify which restrictions, style rules and other conditions covered by
   --  compiler (non-style) warnings should be checked

   type Compiler_Message_Kinds is
     (Not_A_Compiler_Nessage,
      General_Warning,
      Style,
      Restriction);
   --  ??? Do we really need this? Are not Ids for compiler checks enough?

   subtype Compiler_Check is Compiler_Message_Kinds
     range General_Warning .. Restriction;
   --  Describes possible kinds of compiler messages analyzed by gnatcheck

   Use_gnaty_Option : Boolean := False;
   Use_gnatw_Option : Boolean := False;
   --  Boolean flags indicating if the corresponding GNAT option should be
   --  set in the compiler call used to generate the tree

   Check_Restrictions : Boolean := False;
   --  Boolean flag indicating if the configuration file with
   --  Restriction_Warnings pragmas generated by gnatcheck should be used in
   --  the compiler call used to generate the tree

   Suppess_Compiler_Check : Boolean;
   --  Flag indicating if we have to suppress all the compiler checks when
   --  creating a tree file.

   procedure Set_Compiler_Checks;
   --  Sets the values of Use_gnaty_Option, Use_gnatw_Option and
   --  Check_Restrictions on the base of setting made by the warnings, style
   --  checks and restrictions gnatcheck options

   Analyze_Compiler_Output : Boolean := False;
   --  Boolean flag indicating if gnatcheck should analyze the compiler output

   Gnatcheck_Config_File : constant String := "restriction_pragmas" & ".TMP";
   --  The name of the file to place configuration pragmas gnatcheck needs to
   --  add the compiler checks to its report. This file always starts with:
   --
   --     pragma Warnings (Off, "[enabled by default]");
   --
   --  pragma needed to disable warnings that do not have a switch to turn the
   --  warning ON/OFF. If Restrictions rules are specified, this file contains
   --  the corresponding Restriction_Warnings pragmas.
   --
   --  The file name must end in ".TMP", because that is the convention that
   --  indicates to gcc that it should not create a dependency on that file in
   --  the .ALI file. We are going to delete the file after running gcc, so if
   --  there were a dependency on it, then --incremental mode would not work
   --  (it would always rerun gnatcheck on all files).

   procedure Process_Restriction_Param
     (Parameter : String;
      Enable    : Boolean);
   --  Processes a parameter of a restriction (the restriction "rule") passed
   --  as an actual for Parameter. Only a single parameter should be passed,
   --  not a set of parameters separated by commas from the rule option.
   --  Depending on the value of Enable, the corresponding restriction is set
   --  ON or OFF.

   procedure Process_Style_Check_Param (Param : String);
   --  Processes a parameter of a style check (the style_check "rule") passed
   --  as an actual for Param. Only a single parameter should be passed, not a
   --  set of parameters separated by comma(s) from the rule option.

   procedure Process_Warning_Param (Param : String);
   --  Processes a parameter of a warning (the warning "rule") passed as an
   --  actual for Param. Only a single parameter should be passed, not a set of
   --  parameters separated by comma(s) from the rule option. Depending on the
   --  value of Enable, the corresponding warning(s) is (are) set ON or OFF

   procedure Create_Restriction_Pragmas_File;
   --  Creates in the temporary directory the configuration file containing
   --  the needed restriction pragmas

   function Get_Style_Option return String;
   --  Returns the '-gnatyxxx' option to be used in the compiler call, this
   --  function used the style check parameters saved as is, without any
   --  checks.

   function Get_Warning_Option return String;
   --  Returns the '-gnatwxxx' option to be used in the compiler call, this
   --  function used the warning parameters saved as is, without any
   --  checks. The returned warning option is prepended by AIOVZX to get rid of
   --  default settings for warning options. If no warning option has been
   --  saved as parameters of 'Warnings'  rule, "-gnatwAIOVZX" is returned.

   function Get_Specified_Warning_Option return String;
   --  Returns parameters of all the 'Warnings' rules specified for the given
   --  gnatcheck call, without adding anything to it. Returns null string if
   --  no 'Warnings' rule is specified.

   procedure Print_Active_Restrictions (Ident_Level : Natural := 0);
   --  Prints out the Restriction Identifiers of the checks that are set active
   --  for the given gnatcheck call (with the corresponding parameter value, if
   --  any). Restriction identifiers are printed in a column, Ident_Level is
   --  used to control the indentation.

   procedure Print_Active_Restrictions_To_File (Rule_File : File_Type);
   --  Similar to the previous one, but prints the active restrictions in the
   --  format of restriction rules and places the output into the specified
   --  file that is supposed to be an opened out file.

   procedure XML_Print_Active_Restrictions (Indent_Level : Natural := 0);
   --  Similar to the previous one, but prints the active restrictions from the
   --  coding standard in the the XMP report file.

   procedure Analyze_Compiler_Warnings
     (Compiler_Out :     String;
      For_File     :     SF_Id;
      Success      : out Boolean);
   --  Parses the file with compiler output and places the corresponding
   --  diagnoses into the gnatcheck report file. Only those diagnoses that
   --  refer to the same existing file (that is, a file specified as argument
   --  of gnatcheck call) are stored. More strong check (to store only those
   --  messages that refer to the file being compiled, that it - pointed by
   --  For_File) may result in loosing some warnings. For example, if a with
   --  clause is applied to a spec, but the withed unit is used only in the
   --  body, the corresponding message is generated when the body unit is
   --  compiled, but this message points to the spec unit. Possible
   --  duplications of compiler messages are filtered out at the stage of
   --  placing the message into diagnoses table, see
   --  Gnatcheck.Diagnose.Store_Rule_Violation_Internal.
   --
   --  Success is set ON if no problem is detected when analyzing the warning
   --  messages and False otherwise.

   procedure Analyze_Error_Messages
     (Compiler_Out :     String;
      Wrong_Option : out Boolean);
   --  Parses the file with compiler output and tries to locate compiler
   --  messages about wrong options set for the compiler call. If such a
   --  message is located, prints it into Stderr and sets Wrong_Option to
   --  True, otherwise Wrong_Option is set to False

   -------------------------------------------
   --  Routines for parametrized exemptions --
   -------------------------------------------

   function Is_Restriction_Exemption_Par (Par : String) return Boolean;
   --  Checks if Par can be used as a restriction rule parameter in the
   --  definition of exemptiopn section. Assumes that Par is all lowercase and
   --  that Par does not contain any leading or trailing space.

   function Restriction_Rule_Parameter (Diag : String) return String;
   --  Assuming that Diag is a diagnosis string corresponding to a violation of
   --  some restriction-based rule, returns the parameter of the rule (used
   --  in parametrized exemption sections for restrictions)

   function Is_Warning_Exemption_Par (Par : String) return Boolean;
   --  Checks if Par can be used as a Warnings rule parameter in the
   --  definition of exemptiopn section. Assumes that Par does not contain any
   --  leading or trailing space.

   function Warning_Rule_Parameter (Diag : String) return String;
   --  Assuming that Diag is a diagnosis string corresponding to a violation of
   --  some warning-based rule, returns the parameter of the rule (used
   --  in parametrized exemption sections for warnings).

end Gnatcheck.Compiler;
