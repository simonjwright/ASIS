------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . R U L E S . C U S T O M _ 2              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

--  This package defines the a set of gnatcheck rules for gnatcheck developed
--  to satisfy some specific requests from gnatcheck users. There is not any
--  specific idea that used to group all these rules together or to make
--  a separate package for user-driven rules when we already have
--  Gnatcheck.Rules.Custom_1, the only reason is to keep the packages for
--  the rules that comes out from user requests under some reasonable size
--  limit.
--
--  The rules in this packages are ordered alphabetically

pragma Ada_2012;

package Gnatcheck.Rules.Custom_2 is

   ---------------------------------
   -- Complex_Inlined_Subprograms --
   ---------------------------------

   --  Flags a subprogram body if a pragma Inline is applied to this subprogram
   --  and at least one of the following conditions is met:
   --
   --  - the body contains at least one complex declaration such as a
   --    subprogram body, package, task, protected object declaration, or a
   --    generic instantiation (except instantiations of
   --    Ada.Unchecked_Conversion);
   --
   --  - the body contains at least one complex statement such as a loop, a
   --    case or a if statement, or a short circuit control form;
   --
   --  - the body contains more than N statements where N is the unique rule
   --    parameter
   --
   --  The rule has the following parameters:
   --
   --  * for +R option:
   --
   --      N - N is an integer literal, specifies the maximal allowed total
   --          number of local declarations and statements in subprogram body.

   type Complex_Inlined_Subprograms_Rule_Type is new
     One_Integer_Parameter_Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Complex_Inlined_Subprograms_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents a subprogram body, checks the conditions stated
   --  above

   procedure Init_Rule (Rule : in out Complex_Inlined_Subprograms_Rule_Type);

   Complex_Inlined_Subprograms_Rule :
     aliased Complex_Inlined_Subprograms_Rule_Type;

   -----------------------------
   -- Conditional_Expressions --
   -----------------------------

   --  Flags use of conditional expression.

   --  The rule has the following (optional) parameter for the `+R' option:

   --      Except_Assertions - if this parameter is specified, then the
   --         use of conditional expression is not flagged in the following
   --         contexts:

   --      * An argument of the following pragmas (or a part thereof):
   --        - Language-defined pragmas:
   --           Assert
   --        - GNAT-specific pragmas:
   --           Assert_And_Cut
   --           Assume
   --           Contract_Cases
   --           Debug
   --           Invariant
   --           Loop_Invariant
   --           Loop_Variant
   --           Postcondition
   --           Precondition
   --           Predicate
   --           Refined_Post

   --      * a definition of the following aspects (or a part thereof):
   --        - Language-defined attributes:
   --           Static_Predicate
   --           Dynamic_Predicate
   --           Pre
   --           Pre'Class
   --           Post
   --           Post'Class
   --           Type_Invariant
   --           Type_Invariant'Class
   --        - GNAT-specific attributes:
   --           Contract_Cases
   --           Invariant
   --           Invariant'Class
   --           Predicate
   --           Refined_Post

   type Conditional_Expressions_Rule_Type is new
     Rule_With_Exceptions_Template (1) with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Conditional_Expressions_Rule_Type);
   --  Activates the rule with Except_Assertions parameter.

   overriding function Exception_Name
     (Rule      : Conditional_Expressions_Rule_Type;
      Exc_Index : Exception_Index)
      return      String;

   overriding function Exception_Number
     (Rule     : Conditional_Expressions_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Conditional_Expressions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents an Ada 2012 conditional expression, checks if it
   --  is not in the context where such expressions are allowed (in case if
   --  Except_Assertions is specified)

   overriding procedure Init_Rule
     (Rule : in out Conditional_Expressions_Rule_Type);

   Conditional_Expressions_Rule : aliased Conditional_Expressions_Rule_Type;

   ----------------------------------
   -- Deep_Inheritance_Hierarchies --
   ----------------------------------

   --  Flags a tagged derived type declaration or an interface type declaration
   --  if its depth (in its inheritance hierarchy) exceeds the value specified
   --  by the `N' rule parameter. Types in generic instantiations which violate
   --  this rule are also flagged; generic formal types are not flagged. This
   --  rule also does not flag private extension declarations. In the case of a
   --  private extension, the corresponding full declaration is checked.
   --
   --  In most cases, the inheritance depth of a tagged type or interface type
   --  is defined as 0 for a type with no parent and no progenitor, and
   --  otherwise as 1 + max of the depths of the immediate parent and immediate
   --  progenitors. If the declaration of a formal derived type has no
   --  progenitor, or if the declaration of a formal interface type has exactly
   --  one progenitor, then the inheritance depth of such a formal
   --  derived/interface type is equal to the inheritance depth of its
   --  parent/progenitor type, otherwise the general rule is applied.
   --
   --  If the rule flags a type declaration inside the generic unit, this means
   --  that this type declaration will be flagged in any instantiation of the
   --  generic unit. But if a type is derived from a format type or has a
   --  formal progenitor and it is not flagged at the place where it is defined
   --  in a generic unit, it may or may not be flagged in instantiation, this
   --  depends of the inheritance depth of the actual parameters.
   --
   --
   --  This rule has the following (mandatory) parameter for the `+R' option:
   --
   --  N
   --      Integer not less than -1 specifying the maximal allowed depth of any
   --      inheritance hierarchy. If the rule parameter is set to -1, the rule
   --      flags all the declarations of tagged and interface types.

   type Deep_Inheritance_Hierarchies_Rule_Type is new
     One_Integer_Parameter_Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Deep_Inheritance_Hierarchies_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents a tagged derived type declaration, or formal
   --  derived type declaration with private extension computes the length of
   --  the derivation chain and compares it with the specified limit.

   procedure Init_Rule (Rule : in out Deep_Inheritance_Hierarchies_Rule_Type);

   Deep_Inheritance_Hierarchies_Rule :
     aliased Deep_Inheritance_Hierarchies_Rule_Type;

   ----------------------------------
   -- Deeply_Nested_Local_Inlining --
   ----------------------------------

   --  Flags a subprogram body if a pragma Inline is applied to the
   --  corresponding subprogram (or generic subprogram) and the body contains
   --  a call to another inlined subprogram that results in nested inlining
   --  with nesting depth more than N, where N is a rule parameter. This rule
   --  is similar to Deeply_Inlining rule, but it assumes that calls to
   --  subprograms in with'ed units are not inlined, so all the analysis of the
   --  depth of inlining is limited by the unit where the subprogram body is
   --  located and the units it depends semantically upon. Such analysis may be
   --  useful for the case when neither '-gnatn' nor '-gnatN' option is used
   --  when building the executable.
   --
   --  The rule has the following parameters:
   --
   --  * for +R option:
   --
   --      N - N is a positive integer specifying the maximal allowed depth of
   --          nested inlining

   type Deeply_Nested_Local_Inlining_Rule_Type is new
     One_Integer_Parameter_Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Deeply_Nested_Local_Inlining_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a body of inlined (generic) subprogram, checks if the
   --  body contains calls to another inlined subprogram from the same unit
   --  that results in nested inlining deeper then N levels. Assumes that
   --  recursive subprograms cannot be inlined.

   procedure Init_Rule (Rule : in out Deeply_Nested_Local_Inlining_Rule_Type);

   Deeply_Nested_Local_Inlining_Rule :
     aliased Deeply_Nested_Local_Inlining_Rule_Type;

   ----------------------------
   -- Deeply_Nested_Generics --
   ----------------------------

   --  Flags generic declarations nested in another generic declarations if
   --  the if the level of generics-in-generics nesting is higher then the
   --  specified limit. The level of generics-in-generics nesting is the
   --  number of generic declarations that enclose the given (generic)
   --  declaration. Formal packages are not flagged by this rule.
   --
   --  The rule has the following parameters:
   --
   --  * for +R option:
   --
   --      N - N is a non-negative integer literal, specifies the maximal
   --          allowed level of generics-in-generics  nesting. This parameter
   --          is mandatory for +R option.

   type Deeply_Nested_Generics_Rule_Type is new
     One_Integer_Parameter_Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Deeply_Nested_Generics_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents a generic declaration, checks in how many other
   --  generic declarations it is nested.

   procedure Init_Rule (Rule : in out Deeply_Nested_Generics_Rule_Type);

   Deeply_Nested_Generics_Rule : aliased Deeply_Nested_Generics_Rule_Type;

   --------------------------------
   -- Direct_Calls_To_Primitives --
   --------------------------------

   --  Flags any non-dispatching calls to a dispatching primitive operation
   --  is flagged except in one circumstance: when a primitive of a tagged
   --  type calls directly the same primitive of the immediate ancestor.
   --
   --  The rule may have the following parameter:
   --
   --  * for +R option:
   --
   --     Except_Constructors - if this parameter is specified, one more
   --                           exceptional case is added: non-dispatching call
   --                           to a primitive dispatching function is not
   --                           flagged if the function has no dispatching
   --                           parameter and the dispatching is made on the
   --                           base of the function result only

   type Direct_Calls_To_Primitives_Rule_Type is new
     Rule_With_Exceptions_Template (1) with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Direct_Calls_To_Primitives_Rule_Type);
   --  Activates the rule with Except_Constructors parameter.

   overriding function Exception_Name
     (Rule      : Direct_Calls_To_Primitives_Rule_Type;
      Exc_Index : Exception_Index)
      return      String;

   overriding function Exception_Number
     (Rule     : Direct_Calls_To_Primitives_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Direct_Calls_To_Primitives_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a non-dispatching procedure or function call, checks if
   --  the called subprogram is a primitive operation of a type, and if it is,
   --  checks if the call is located in the body of child's primitive

   overriding procedure Init_Rule
     (Rule : in out Direct_Calls_To_Primitives_Rule_Type);

   Direct_Calls_To_Primitives_Rule :
     aliased Direct_Calls_To_Primitives_Rule_Type;

   ----------------------------------
   -- Exits_From_Conditional_Loops --
   ----------------------------------

   --  Flags any exit statement if it transfers the control out of a FOR loop
   --  or a WHILE loop. This includes cases when the exit statement applies to
   --  a FOR or WHILE loop, and cases when in is enclosed in some FOR or WHILE
   --  loop, but transfers the control from some outer (inconditional) loop.
   --
   --  The rule does not have any parameter.

   type Exits_From_Conditional_Loops_Rule_Type is
     new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Exits_From_Conditional_Loops_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents an exit statement, checks what is the kind of the
   --  corresponding loop being exited.

   procedure Init_Rule (Rule : in out Exits_From_Conditional_Loops_Rule_Type);

   Exits_From_Conditional_Loops_Rule :
     aliased Exits_From_Conditional_Loops_Rule_Type;

   ----------------------
   -- Global_Variables --
   ----------------------

   --  Flag any variable declaration that appears immediately within the
   --  specification of a library package or library generic package. Variable
   --  declarations in nested packages and inside package instantiations are
   --  not flagged.
   --
   --  This rule has the following (optional) parameters for the `+R' option:

   --    Only_Public - if this parameter is set, the variable declarations in
   --                  private library (generic) packages and in package
   --                  private parts are not flagged.

   type Global_Variables_Rule_Type is new
     Rule_With_Exceptions_Template (1) with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Global_Variables_Rule_Type);
   --  Activates the rule with Only_Public parameter.

   overriding function Exception_Name
     (Rule      : Global_Variables_Rule_Type;
      Exc_Index : Exception_Index)
      return      String;

   overriding function Exception_Number
     (Rule     : Global_Variables_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Global_Variables_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents a variable declaration, checks if it is declared
   --  immediately within a library (generic) package.

   overriding procedure Init_Rule (Rule : in out Global_Variables_Rule_Type);

   Global_Variables_Rule : aliased Global_Variables_Rule_Type;

   ------------------------
   -- Maximum_Parameters --
   ------------------------

   --  Flag any subprogram declaration, subprogram body declaration, expression
   --  function declaration, null procedure declaration, subprogram body stub
   --  or generic subprogram declaration if the corresponding subprogram has
   --  more than 'N' formal parameters, where 'N' is a parameter of the rule.
   --
   --  A subprogram body, expression function, null procedure or a subprogram
   --  body stub is flagged only if there is no separate declaration for this
   --  subprogram. Subprogram renaming declarations and subprogram
   --  instantiations, as well as declarations inside expanded generic
   --  instantiations are never flagged.
   --
   --   This rule has the following (mandatory) parameters for the `+R' option:
   --
   --     N - positive integer specifying the maximal allowed number of
   --       subprogram formal parameters

   type Maximum_Parameters_Rule_Type is new
     One_Integer_Parameter_Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Maximum_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents a (generic) subprogram declaration/body/body stub
   --  and there is no separate spec for this subprogram in case of a body or a
   --  body stub, checks that the number of formal parameters is not greater
   --  than N.

   overriding procedure Init_Rule (Rule : in out Maximum_Parameters_Rule_Type);

   Maximum_Parameters_Rule : aliased Maximum_Parameters_Rule_Type;

   ----------------------
   -- Membership_Tests --
   ----------------------

   --  Flags membership test expressions
   --
   --  The rule has the following (optional) parameters for the `+R' option:
   --
   --      Multi_Alternative_Only - if this parameter is specified, then only
   --         those membership test expressions that are more than one
   --         membership choice in the membership choice list are flagged.
   --
   --      Except_Assertions - if this parameter is specified, then the
   --         use of the constructs described above is not flagged in the
   --         following contexts:

   --      * An argument of the following pragmas (or a part thereof):
   --        - Language-defined pragmas:
   --           Assert
   --        - GNAT-specific pragmas:
   --           Assert_And_Cut
   --           Assume
   --           Contract_Cases
   --           Debug
   --           Invariant
   --           Loop_Invariant
   --           Loop_Variant
   --           Postcondition
   --           Precondition
   --           Predicate
   --           Refined_Post

   --      * a definition of the following aspects (or a part thereof):
   --        - Language-defined attributes:
   --           Static_Predicate
   --           Dynamic_Predicate
   --           Pre
   --           Pre'Class
   --           Post
   --           Post'Class
   --           Type_Invariant
   --           Type_Invariant'Class
   --        - GNAT-specific attributes:
   --           Contract_Cases
   --           Invariant
   --           Invariant'Class
   --           Predicate
   --           Refined_Post
   --
   --      Float_Types_Only - if this parameter is specified, then only those
   --         membership test expressions that operates on float point types
   --         are flagged
   --
   --  These two parameters are independent on each other.

   type Membership_Tests_Rule_Type is new
     Rule_With_Exceptions_Template (3) with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Membership_Tests_Rule_Type);
   --  Activates the rule with Except_Assertions parameter.

   overriding function Exception_Name
     (Rule      : Membership_Tests_Rule_Type;
      Exc_Index : Exception_Index)
      return      String;

   overriding function Exception_Number
     (Rule     : Membership_Tests_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Membership_Tests_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents an Ada construct described above, checks if it
   --  is not in the context where such expressions are allowed (in case if
   --  Except_Assertions is specified)

   overriding procedure Init_Rule
     (Rule : in out Membership_Tests_Rule_Type);

   Membership_Tests_Rule : aliased Membership_Tests_Rule_Type;

   -------------------------------------
   -- Misnamed_Controlling_Parameters --
   -------------------------------------

   --  Flags a declaration of a dispatching operation, if the first parameter
   --  is not a controlling one and its name is not This (the check for
   --  parameter name is not case-sensitive). Declarations of dispatching
   --  functions with controlling result and no controlling parameter are never
   --  flagged.
   --
   --  A subprogram body declaration, subprogram renaming declaration of
   --  subprogram body stub is flagged only if it is not a completion of a
   --  subprogram declaration.
   --
   --  This rule has no parameters.

   type Misnamed_Controlling_Parameters_Rule_Type is
     new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Misnamed_Controlling_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents primitive operation of a tagged type, checks if
   --  it follows the requirements listed above.

   procedure Init_Rule
     (Rule : in out Misnamed_Controlling_Parameters_Rule_Type);

   Misnamed_Controlling_Parameters_Rule :
     aliased Misnamed_Controlling_Parameters_Rule_Type;

   ----------------------------------------
   --  No_Scalar_Storage_Order_Specified --
   ----------------------------------------

   --  Flag each record type declaration, record extension declaration, and
   --  non-tagged derived record type declaration if a
   --  record_representation_clause that has at least one component clause
   --  applies to it (or an ancestor), but neither the type nor any of its
   --  ancestors has an explicitly specified Scalar_Storage_Order attribute
   --  (it can be specified by the corresponding attribute definition clause,
   --  by the Attribute_Definition pragma, or by the aspect definition).
   --
   --  This rule has no parameters.

   type No_Scalar_Storage_Order_Specified_Rule_Type is
     new Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out No_Scalar_Storage_Order_Specified_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a record type declaration or a record extension
   --  declaration and if it has the corresponding record representation
   --  clause, checks if it also has the corresponding attribute representation
   --  clause for Scalar_Storage_Order.

   overriding procedure Init_Rule
     (Rule : in out No_Scalar_Storage_Order_Specified_Rule_Type);

   No_Scalar_Storage_Order_Specified_Rule :
     aliased No_Scalar_Storage_Order_Specified_Rule_Type;

   ------------------------
   -- Predicate_Testing --
   ------------------------

   --  Flags a subtype mark if it denotes a subtype defined with (static or
   --  dynamic) subtype predicate and is used as a membership choice in a
   --  membership test expression.
   --
   --  Flags 'Valid attribute reference if the nominal subtype of the attribute
   --  prefix has (static or dynamic) subtype predicate.
   --
   --  The rule has the following (optional) parameter for the `+R' option:

   --      Except_Assertions - if this parameter is specified, then the
   --         use of the constructs described above is not flagged in the
   --         following contexts:

   --      * An argument of the following pragmas (or a part thereof):
   --        - Language-defined pragmas:
   --           Assert
   --        - GNAT-specific pragmas:
   --           Assert_And_Cut
   --           Assume
   --           Contract_Cases
   --           Debug
   --           Invariant
   --           Loop_Invariant
   --           Loop_Variant
   --           Postcondition
   --           Precondition
   --           Predicate
   --           Refined_Post

   --      * a definition of the following aspects (or a part thereof):
   --        - Language-defined attributes:
   --           Static_Predicate
   --           Dynamic_Predicate
   --           Pre
   --           Pre'Class
   --           Post
   --           Post'Class
   --           Type_Invariant
   --           Type_Invariant'Class
   --        - GNAT-specific attributes:
   --           Contract_Cases
   --           Invariant
   --           Invariant'Class
   --           Predicate
   --           Refined_Post

   type Predicate_Testing_Rule_Type is new
     Rule_With_Exceptions_Template (1) with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Predicate_Testing_Rule_Type);
   --  Activates the rule with Except_Assertions parameter.

   overriding function Exception_Name
     (Rule      : Predicate_Testing_Rule_Type;
      Exc_Index : Exception_Index)
      return      String;

   overriding function Exception_Number
     (Rule     : Predicate_Testing_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Predicate_Testing_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents an Ada construct described above, checks if it
   --  is not in the context where such expressions are allowed (in case if
   --  Except_Assertions is specified)

   overriding procedure Init_Rule
     (Rule : in out Predicate_Testing_Rule_Type);

   Predicate_Testing_Rule : aliased Predicate_Testing_Rule_Type;

   --------------------------
   -- Quantified_Expresions --
   ---------------------------

   --  Flags use of quantified expression.

   --  The rule has the following (optional) parameter for the `+R' option:

   --      Except_Assertions - if this parameter is specified, then the
   --         use of quantified expression is not flagged in the following
   --         contexts:

   --      * An argument of the following pragmas (or a part thereof):
   --        - Language-defined pragmas:
   --           Assert
   --        - GNAT-specific pragmas:
   --           Assert_And_Cut
   --           Assume
   --           Contract_Cases
   --           Debug
   --           Invariant
   --           Loop_Invariant
   --           Loop_Variant
   --           Postcondition
   --           Precondition
   --           Predicate
   --           Refined_Post

   --      * a definition of the following aspects (or a part thereof):
   --        - Language-defined attributes:
   --           Static_Predicate
   --           Dynamic_Predicate
   --           Pre
   --           Pre'Class
   --           Post
   --           Post'Class
   --           Type_Invariant
   --           Type_Invariant'Class
   --        - GNAT-specific attributes:
   --           Contract_Cases
   --           Invariant
   --           Invariant'Class
   --           Predicate
   --           Refined_Post

   type Quantified_Expressions_Rule_Type is new
     Rule_With_Exceptions_Template (1) with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Quantified_Expressions_Rule_Type);
   --  Activates the rule with Except_Assertions parameter.

   overriding function Exception_Name
     (Rule      : Quantified_Expressions_Rule_Type;
      Exc_Index : Exception_Index)
      return      String;

   overriding function Exception_Number
     (Rule     : Quantified_Expressions_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Quantified_Expressions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents an Ada 2012 conditional expression, checks if it
   --  is not in the context where such expressions are allowed (in case if
   --  Except_Assertions is specified)

   overriding procedure Init_Rule
     (Rule : in out Quantified_Expressions_Rule_Type);

   Quantified_Expressions_Rule : aliased Quantified_Expressions_Rule_Type;

   -------------------------------------
   -- Separate_Numeric_Error_Handlers --
   -------------------------------------

   --  Flags any exception handler that contains the choice for
   --  Constrain_Error, but does not contains the choice for Numeric_Error, or
   --  that contains the choice for Numeric_Error, but does not contain the
   --  choice for Constraint_Error. (Constraint_Error and Numeric_Error mean
   --  predefined exceptions for this rule).
   --
   --  The rule does not mean very much sense for Ada 95 and Ada 2005 programs,
   --  where Numeric_Error is just a renaming of Constraint_Error, but it
   --  allows to verify if for Ada 83 programs (where Numeric_Error is
   --  different from Constraint_Error) the exception handling is the same in
   --  Ada 83 and Ada 95 (2005) modes.
   --
   --  The rule does not have any parameter.

   type Separate_Numeric_Error_Handlers_Rule_Type is
     new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Separate_Numeric_Error_Handlers_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents an exception handler, checks if its choices either
   --  contains both Constraint_Error and Numeric_Error or neither of these
   --  predefined exceptions. In case if an exception name representing a
   --  choice in the exception handler is defined by a renaming declaration,
   --  the procedure traverses the renaming chain.

   procedure Init_Rule
     (Rule : in out Separate_Numeric_Error_Handlers_Rule_Type);

   Separate_Numeric_Error_Handlers_Rule :
     aliased Separate_Numeric_Error_Handlers_Rule_Type;

   ----------------------
   -- Too_Many_Parents --
   ----------------------

   --  Flags any type declaration, single task declaration or single protected
   --  declaration that has more than N parents, N is a parameter of the rule.
   --  A parent here is either a (sub)type denoted by the subtype mark from the
   --  parent_subtype_indication (in case of a derived type declaration), or
   --  any of the progenitors from the interface list, if any.
   --
   --  The rule has the following parameters:
   --
   --  * for +R option:
   --
   --      N - N is a positive integer, specifies the maximal allowed number of
   --          parents.
   --

   type Too_Many_Parents_Rule_Type is new One_Integer_Parameter_Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Too_Many_Parents_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents a declaration that can have more than one parent,
   --  counts the number of parents and compares with the maximal allowed
   --  number of parents specified for the rule.
   --
   --  The following ASIS A_Declaration elements may have more than ore parent:
   --     An_Ordinary_Type_Declaration (provided that the corresponding
   --       A_Type_Definition Element is of
   --       A_Derived_Record_Extension_Definition or
   --       An_Interface_Type_Definition kind)
   --     A_Formal_Type_Declaration (provided that the corresponding
   --       A_Formal_Type_Definition Element is
   --       of A_Formal_Derived_Type_Definition kind)
   --     A_Private_Extension_Declaration
   --     A_Task_Type_Declaration
   --     A_Protected_Type_Declaration
   --     A_Single_Task_Declaration
   --     A_Single_Protected_Declaration

   procedure Init_Rule (Rule : in out Too_Many_Parents_Rule_Type);

   Too_Many_Parents_Rule : aliased Too_Many_Parents_Rule_Type;

   -------------------------
   -- Unconditional_Exits --
   -------------------------

   --  Flags all the unconditional exit statements.
   --
   --  The rule does not have any parameter.
   --
   type Unconditional_Exits_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unconditional_Exits_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents an exit statement checks if it contains a
   --  condition.

   procedure Init_Rule (Rule : in out Unconditional_Exits_Rule_Type);

   Unconditional_Exits_Rule : aliased Unconditional_Exits_Rule_Type;

   ------------------------
   -- Visible_Components --
   ------------------------

   --  Flags all the type declarations located in the visible part of a library
   --  package or a library generic package that can declare a visible
   --  component. A type is considered as declaring a visible component if it
   --  contains a record definition by its own or as a part of a record
   --  extension. Type declaration is flagged even if it contains a record
   --  definition that defines no components.
   --
   --  Declarations located in private parts of local (generic) packages are
   --  not flagged. Declarations in private packages are not flagged.
   --
   --  The rule has the following (optional) parameter for the `+R' option:
   --
   --     Tagged_Only - if this parameter is specified, the rule checks only
   --                   tagged record declarations and type extensions

   type Visible_Components_Rule_Type is new
     Rule_With_Exceptions_Template (1) with null record;

   overriding function Exception_Name
     (Rule      : Visible_Components_Rule_Type;
      Exc_Index : Exception_Index)
      return      String;

   overriding function Exception_Number
     (Rule     : Visible_Components_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Visible_Components_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents a type declaration that defines a record
   --  component checks if it is located in the visible part of a library
   --  package or a generic library package. If the type declaration is located
   --  in the private part of a local package, it is not flagged.

   overriding procedure Init_Rule (Rule : in out Visible_Components_Rule_Type);

   Visible_Components_Rule : aliased Visible_Components_Rule_Type;

end Gnatcheck.Rules.Custom_2;
