------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              G N A T C H E C K . D I A G N O S E S _ O L D               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2006-2013, AdaCore                     --
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

--  This package defines the diagnostic message database that allows to
--  generate different formats of the gnatcheck reports
--
--  Because of historical reasons, at the moment two formats of the report file
--  are supported:
--
--  * so-called "old" format, it is rather verbose, it contains rule diagnosed
--    grouped in three different ways, it does not contain compiler error
--    messages for non-compilable files, it does not contain warnings related
--    to rule exemptions;
--
--  * so-called new format, that is supposed to be used as gnatcheck qualified
--    interface, it is much less verbose, and it contains compiler error
--    messages and rule exemption warnings;
--
--  At some point all the stuff related to the old format of the report file
--  should be moved into separate package.

with Asis;                       use Asis;
with Asis.Extensions.Strings;    use Asis.Extensions.Strings;

with ASIS_UL.Global_State;       use ASIS_UL.Global_State;
with ASIS_UL.Source_Table;       use ASIS_UL.Source_Table;

with Gnatcheck.Compiler;         use Gnatcheck.Compiler;
with Gnatcheck.Ids;              use Gnatcheck.Ids;
with Gnatcheck.Rules;            use Gnatcheck.Rules;

package Gnatcheck.Diagnoses_Old is

   procedure Create_Mapping_Table;
   --  This procedure creates (and initializes by null links) the
   --  File-Rule-Diagnosis Mapping Table for argument files. This table is
   --  needed to create links between diagnoses that allow to print out these
   --  diagnoses in different orders. This procedure should be called after
   --  registering all the rules and processing all the gnatcheck parameters
   --  (when the file table is filled in and we know how many files to process
   --  we have), but before the first call to Store_Rule_Violation routine.
   --  Note, that the call to this routine does not create entries for the
   --  files that are needed by the argument sources, but are not themselves
   --  the argument sources of the gnatcheck call, and
   --  Add_Line_To_Mapping_Table should be called each time we add a new needed
   --  file to the Mapping Table

   procedure Add_Line_To_Mapping_Table;
   --  Adds one line (corresponding to a source file) to the
   --  File-Rule-Diagnosis Mapping Table

   procedure Store_Rule_Violation
     (For_Rule      : Rule_Id;
      On            : Element;
      In_SF         : SF_Id;
      Justification : String_Loc;
      Diagnosis_Num : Diagnosis_Variant := 0;
      Diag_Pars     : String_Loc;
      Element_SLOC  : String_Loc);

   procedure Store_Rule_Violation
     (For_Rule : Rule_Id;
      On       : GS_Node_Id);

   --  These two routines store the information about the rule violation in the
   --  diagnosis table, setting all the links to create the diagnosis chains
   --  needed for different report formats

   procedure Store_Compiler_Message
     (In_SF        : SF_Id;
      Line_Num     : Natural;
      Col_Num      : Natural;
      Message      : String_Loc;
      Message_Kind : Compiler_Message_Kinds);
   --  Stores the compiler (warning) message in the diagnosis table, setting
   --  all the links to create the diagnosis chains needed to create different
   --  report formats

   procedure Generate_Regular_Report;
   --  This procedure  generates the old format of the rule violation reports.
   --  The name of the report file is specified by the '-o <filename>' option,
   --  if not set, the default './gnatcheck.out' is used.

   procedure Process_User_Filename (Fname : String);
   --  Checks if Fname is the name of the existing file. If it is, sets it as
   --  the value of Gnatcheck.Options.User_Info_File, otherwise generates
   --  warning and leaves User_Info_File unchanged. If User_Info_File is
   --  already set, and Fname denotes some existing file, generates a warning
   --  (user-defined part of the report file can be specified only once!) and
   --  leaves User_Info_File unchanged.

   procedure Diag_Structure_Debug_Image;
   --  Generates the debug image of the diagnosis structures

   procedure Process_Report_File_Format_Parameter
     (Parameter :     String;
      Success   : out Boolean);
   --  Processes parameter of '-sxxx' option and modify the corresponding
   --  control flags and variables setting the format of gnatcheck output file.
   --  Sets Success ON if the actual of Parameter represents a correct
   --  parameter setting for '-s' option, and OFF otherwise.

end Gnatcheck.Diagnoses_Old;
