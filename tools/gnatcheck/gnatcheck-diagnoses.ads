------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                  G N A T C H E C K . D I A G N O S E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

------------------------------------------------------------------------------
--  This package is under development for now. It should replace the        --
--  existing packages Gnatcheck.Diagnoses and Gnatcheck.Exemption (these    --
--  two packages should be combined into one because they are used internal --
--  data structures that depend on each other heavily).                     --
------------------------------------------------------------------------------

--  This package defines routines for storing diagnostic messages and
--  generating final gnatcheck report. It also provides routines that
--  supports rule exemption mechanism. Note, that most of the rule exemption
--  mechanism is hidden in the body of the package.

with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Asis;                       use Asis;

with ASIS_UL.Global_State;       use ASIS_UL.Global_State;
with ASIS_UL.Source_Table;       use ASIS_UL.Source_Table;

with Gnatcheck.Ids;              use Gnatcheck.Ids;

package Gnatcheck.Diagnoses is

   -----------------------
   -- Diagnoses storage --
   -----------------------

   type Diagnosis_Kinds is
     (Not_A_Diagnosis,
      Rule_Violation,
      --  Corresponds to all rule diagnoses, including compiler checks
      Exemption_Warning,
      --  Warnings generated for Annotate pragmas used to implement rule
      --  exemption mechanism
      Compiler_Error
      --  Compiler diagnoses generated for illegal (non-compilable) sources
      );

   procedure Store_Diagnosis
     (Text           : String;
      Diagnosis_Kind : Diagnosis_Kinds;
      SF             : SF_Id;
      Rule           : Rule_Id       := No_Rule;
      Justification  : String_Access := null);
   --  Stores the diagnosis in the internal data structure. The same procedure
   --  is used for all diagnosis kinds, in case of Exemption_Warning and
   --  Compiler_Error Rule should be set to No_Rule.

   procedure Store_Error_Messages
     (Compiler_Out_Fname : String;
      SF                 : SF_Id);
   --  Assuming that Compiler_Out_Fname is the name of the text file the
   --  compiler output from compiling the source file SF have been redirected,
   --  parses this file and stores compiler error messages in the internal data
   --  structure. If no error messages are located in the file, the artificial
   --  message "file cannot be compiled by unknown reason" is stored.

   ------------------------
   -- Diagnoses Counters --
   ------------------------

   Detected_Rule_Violations : Natural := 0; --  ??? not used???
   --  The total number of rule violations detected in the given gnatcheck run,
   --  exempted and non-exempted violations are counted together

   Detected_Non_Exempted_Violations : Natural := 0;
   Detected_Exempted_Violations     : Natural := 0;
   --  Separate counters for exempted and non-exempted violations.

   Detected_Exemption_Warning : Natural := 0;
   Detected_Compiler_Error    : Natural := 0;

   ------------------------
   --  Report generation --
   ------------------------

   procedure Generate_Qualification_Report;
   --  Generate the report oriented for using as qualification materials. There
   --  is no parameter to configure this report except
   --  Gnatcheck.Options.Short_Report flag.

   procedure Print_Report_Header;
   --  Generates the report header, including the date, tool version and
   --  tool command liner invocation sequence. (We need it in spec because it
   --  is used by Gnatcheck.Projects.Aggregate_Project_Report_Header.

   -------------------------
   -- Exemption mechanism --
   -------------------------

   procedure Init_Postponed_Check_Exemptions;
   --  Prepares the internal data structures for storing information about
   --  postponed checks exemption sections in a source. Should be called in the
   --  very beginning of analyzing a new source. For compiler checks, global
   --  rules, rules that are checked on expanded generics we can do rule
   --  exemptions only after completing processing of all the sources.

   procedure Init_Exemptions;
   --  Initializes all the internal data structures needed for exemption
   --  mechanism

   function Is_Exemption_Pragma (El : Asis.Element) return Boolean;
   --  Checks if the argument Element is Annotate pragma with first parameter
   --  equal to 'gnatcheck'.

   procedure Process_Exemption_Pragma (El : Asis.Element);
   --  Provided that Is_Exemption_Pragma (El), analyses the argument element
   --  and stores the information about exemption section. In most of the
   --  cases (for local rules, that are not checked on expanded instantiations)
   --  it is equivalent to turning the rule into exempted state, but for the
   --  following rule categories:
   --    * compiler checks
   --    * global rules
   --    * rules checked on expended instantiations
   --
   --  post-processing is needed. This postprocessing can be done when all the
   --  rule checking and processing of Annotate exemption pragmas on all the
   --  sources is completed. See ??? for more details.

   procedure Check_Unclosed_Rule_Exemptions
     (SF   : SF_Id;
      Unit : Asis.Element);
   --  Is supposed to be called in the very end of processing of the source
   --  corresponding to SF. Checks if there exist some exempted rules. For each
   --  such rule, a warning is issued and exemption is turned OFF. Unit
   --  parameter is used to compute the end of non-closed exemption sections
   --  for compiler checks, if any.

   function Exemption_Justification (Rule : Rule_Id) return String_Access;
   --  Returns justification for the argument Rule as it is set by processed
   --  Annotate pragmas.

end Gnatcheck.Diagnoses;
