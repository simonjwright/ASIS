------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              G N A T C H E C K . R U L E S . M E T R I C S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
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

--  This package defines the gnatcheck rules based on metrics.

pragma Ada_2012;

package Gnatcheck.Rules.Metrics is

   --  All the rules in this package have a positive numeric (integer or real,
   --  depending on a rule) parameter for +R option that specifies the upper
   --  or lower (also depending on a rule) limit for a given metric. -R option
   --  for metric rule does not have a parameter.

   -----------------------------------
   -- Metrics_Cyclomatic_Complexity --
   -----------------------------------

   type Metrics_Cyclomatic_Complexity_Rule_Type is new
     One_Integer_Parameter_Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Metrics_Cyclomatic_Complexity_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If cyclomatic complexity metric is applicable to the argument Element,
   --  checks if the metric value is not greater than is specified for this
   --  metric check. Flags a construct if this check fails.

   procedure Init_Rule (Rule : in out Metrics_Cyclomatic_Complexity_Rule_Type);

   Metrics_Cyclomatic_Complexity_Rule :
     aliased Metrics_Cyclomatic_Complexity_Rule_Type;

   ----------------------------------
   -- Metrics_Essential_Complexity --
   ----------------------------------

   type Metrics_Essential_Complexity_Rule_Type is new
     One_Integer_Parameter_Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Metrics_Essential_Complexity_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If essential complexity metric is applicable to the argument Element,
   --  checks if the metric value is not greater than is specified. Flags a
   --  construct if this check fails.

   procedure Init_Rule (Rule : in out Metrics_Essential_Complexity_Rule_Type);

   Metrics_Essential_Complexity_Rule :
     aliased Metrics_Essential_Complexity_Rule_Type;

   -------------------
   -- Metrics_LSLOC --
   -------------------

   --  Unlike other metrics rules, this rule has additional parameters (at the
   --  moment this is only one parameter) that define for which particular
   --  constructs the rule should be checked. Note, that to enable a rule you
   --  have to specify the metric value, that is, +R option with optional
   --  parameter does not enable the rule.
   --  So, the rule has one mandatory parameter for +R option that specify
   --  the upper bound for LSLOC metric under which constructs are not flagged
   --  and the following optional parameters that impose filters on the rule by
   --  specify constructs for that the rule should be checked:
   --
   --    Subprograms - check the rule for subprogram bodies only;
   --
   --    to be extended if needed
   --
   --  -R option does not have any parameter, it removes all the filters that
   --   may be imposed in +R options.

   type Metrics_LSLOC_Rule_Type is new One_Integer_Parameter_Rule_Template
     with record
        --  If one of the fields below is set ON by parameter processing
        --  then the rule is checked only for the constructs specified

        Subprograms_Only : Boolean := False;
        --  Check rule on subprogram bodies only;

      --  to be extended if needed;
     end record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Metrics_LSLOC_Rule_Type);
   --  Activates the rule with the parameter equals to 30 (the default 4 is
   --  too small for this rule).

   overriding procedure Print_Rule
     (Rule         : Metrics_LSLOC_Rule_Type;
      Indent_Level : Natural := 0);

   overriding procedure Print_Rule_To_File
     (Rule         : Metrics_LSLOC_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Metrics_LSLOC_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String);

   procedure Rule_Check_Pre_Op
     (Rule    : in out Metrics_LSLOC_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If LSLOC metric is applicable to the argument Element, checks if the
   --  metric value is not greater than is specified. Flags a construct if this
   --  check fails.

   procedure Init_Rule (Rule : in out Metrics_LSLOC_Rule_Type);

   Metrics_LSLOC_Rule : aliased Metrics_LSLOC_Rule_Type;

end Gnatcheck.Rules.Metrics;
