------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--               G N A T C H E C K . R U L E S . O U T P U T                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2004-2010, AdaCore                      --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston,                                                                  --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the rule output routines

with ASIS_UL.Global_State;        use ASIS_UL.Global_State;

package Gnatcheck.Rules.Output is

   -------------------------------------------------
   -- Parametrization of rule diagnostic messages --
   -------------------------------------------------

   --  Rule diagnoses may have two levels of parametrization.
   --
   --  First, a rule may have more than one variant of the text of diagnostic
   --  message. If so, each variant should be prepended by the substring #N#
   --  where N is treated as the number of the variant, To specify the desired
   --  variant of the diagnostic message, one should pass the corresponding
   --  number as the actual for Diagnosis_Num parameter in the call to
   --  Report_Detection.
   --
   --  Second, a variant of diagnostic message may have "formal parameters" -
   --  that is, parts that should be replaced with some strings computed from
   --  the specific rule violation. In the text of a diagnostic variant, such
   --  formal parameters should be specified as %N% where N is a parameter
   --  number. In this case the Pre- or Post-operation that detects the
   --  violation of this rule, should form the string that contains actual
   --  parameters (actual substrings) for each of the formal parameters in
   --  the diagnostic variant. The parameter value should be prepended by
   --  %N% where N is the number of the corresponding formal. The order
   --  of actuals is not important, no delimiters between actuals are allowed.
   --  The string with actuals should be located in the string storage (see
   --  ASIS_UL.Strings), and the corresponding String_Loc should be passed
   --  as the actual for Diag_Actuals in the call to Report_Detection
   --
   --  # should be used in the string assigned to the rule Diagnosis field only
   --  as the "brackets" for the variant number.
   --
   --  % should be used in the string assigned to the rule Diagnosis field only
   --  as the "brackets" for the variant number.
   --
   --  Example. If for the given rule the Diagnosis field points to the string
   --
   --   "#1#first diagnosis for %1%"  &
   --   "#2#second diagnosis for %1%" &
   --   "#3#third diagnosis for %1% because of %2%"
   --
   --  and Report_Detection is called with Diagnosis_Num => 3 and Diag_Actuals
   --  points to the following string:
   --
   --   "%1%Name_One%2%Name_Two"
   --
   --  then this call will result in forming the following message:
   --
   --   "third diagnosis for Name_One because of Name_Two
   --
   --  At the moment, diagnosis parametrization is not allowed for global
   --  rules in case if the rule is checked by analyzing the call graph

   procedure Report_Detection
     (For_Rule      : Rule_Id;
      On            : Element;
      In_SF         : SF_Id;
      Justification : String_Access;
      Diagnosis_Num : Diagnosis_Variant := 0;
      Diag_Actuals  : String_Loc;
      Diag_Line     : Natural;
      Diag_Column   : Natural);
   --  Store the diagnostic information about the detected rule violations in
   --  the diagnoses database. At the moment this information is stored in two
   --  data structures - for old and for new report file format.

   procedure Report_Global_Rule_Detection
     (For_Rule : Rule_Id;
      On       : GS_Node_Id);
   --  Does the same, but during the global structure analysis

   function Select_Variant
     (Message : String;
      Num     : Diagnosis_Variant)
      return String;
   --  Using the value of Num, selects the corresponding variant of the
   --  diagnostic Message. Raises Diagnosis_Error if this selection is
   --  impossible because of any reason.

   function Insert_Actuals
     (Message : String;
      Actuals : String_Loc)
   return       String;
   --  Supposing that Message is a text of diagnostic message, and Actuals is
   --  not null, this function treats Message as a parametrized diagnosis and
   --  the string pointed by Actuals as the actual parameters for this
   --  diagnosis, it tries to insert actual parameters into the text of the
   --  diagnosis and returns the result of this insertion. Raises
   --  Diagnosis_Error if this insertion is impossible because of any reason.
   --  If Actuals is null, returns the first argument unchanged.

   Diagnosis_Error : exception;

end Gnatcheck.Rules.Output;
