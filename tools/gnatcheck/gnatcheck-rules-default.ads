------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              G N A T C H E C K . R U L E S . D E F A U L T               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
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

--  This package defines the default set of rules for gnatcheck. Use ???
--  to extend the set of rules

pragma Ada_2012;

package Gnatcheck.Rules.Default is

   --------------------------------
   -- Abstract_Type_Declarations --
   --------------------------------

   --  The use of abstract types are detected. In case of an abstract private
   --  type, both private and full type declarations are detected.

   type Abstract_Type_Declarations_Rule_Type is new Rule_Template with
     null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Abstract_Type_Declarations_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is an abstract type declaration

   procedure Init_Rule (Rule : in out Abstract_Type_Declarations_Rule_Type);

   Abstract_Type_Declarations_Rule :
     aliased Abstract_Type_Declarations_Rule_Type;

   ------------------------
   -- Anonymous_Subtypes --
   ------------------------

   --  Any use of an anonymous subtype is detected.

   --  A use of an anonymous subtype is any use of a construct
   --  'subtype_mark constraint' different from immediately within a subtype
   --  declaration. Any use of range different from the situation when this
   --  range is a constraint used immediately within a subtype declaration is
   --  also considered as the use of an anonymous subtype and should be
   --  detected

   type Anonymous_Subtypes_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Anonymous_Subtypes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is an abstract type declaration

   procedure Init_Rule (Rule : in out Anonymous_Subtypes_Rule_Type);

   Anonymous_Subtypes_Rule : aliased Anonymous_Subtypes_Rule_Type;

   ------------
   -- Blocks --
   ------------

   --  Block statements are detected.

   type Blocks_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Blocks_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is a block statement

   procedure Init_Rule (Rule : in out Blocks_Rule_Type);

   Blocks_Rule : aliased Blocks_Rule_Type;

   ----------------------------------
   -- Boolean_Relational_Operators --
   ----------------------------------

   --  According to the definition of the Spark language, relation operations
   --  <, >, <=, >=, = and /= cannot be used for the type Boolean.

   --  Calls to predefined relation operators (<, >, <=, >=, = and /=) of
   --  the predefined Boolean type are detected.

   --  Calls to predefined relation operations of any type derived from
   --  Standard.Boolean are not detected.

   --  Calls to user-defined functions with these designators are not detected

   --  The use of an operation that is a renaming of a predefined relation
   --  operation for Standard.Boolean is not detected

   type Boolean_Relational_Operators_Rule_Type is new Rule_Template with
     null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Boolean_Relational_Operators_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents any of the comparison operators listed above,
   --  checks that the argument of this operation are both of the type
   --  Standard.Boolean

   procedure Init_Rule (Rule : in out Boolean_Relational_Operators_Rule_Type);

   Boolean_Relational_Operators_Rule :
     aliased Boolean_Relational_Operators_Rule_Type;

   ------------------------
   -- Ceiling_Violations --  turned off!!! --
   ------------------------

   --  Any caller to a protected operation should have the priority not higher
   --  then the called protected operation.

   --  The situations when a protected operation is called by a higher priority
   --  task is detected.

   --  At the moment, the detection of violations of ceiling priority
   --  consistency is performed with the following limitations:
   --
   --  - We consider only pragmas Priority and Interrupt_Priority as means to
   --    define a task/protected operation priority. We do not consider the
   --    effect of using Ada.Dynamic_Priorities.Set_Priority procedure;
   --
   --  - We consider only base task priorities, and no priority inheritance.
   --    That is, we do not make a difference between calls issued during task
   --    activation and execution of the sequence of statements from task body;
   --
   --  - Any situation when the priority of protected operation caller is set
   --    by a dynamic expression (that is, the corresponding Priority or
   --    Interrupt_Priority pragma has a non-static expression as an argument)
   --    we treat as a priority inconsistency (and, therefore, detect this
   --    situation).
   --
   --  At the moment the notion of the main subprogram is not implemented in
   --  gnatcheck, so any pragma Priority in a library level subprogram body
   --  (in case if this subprogram Can_Be_Main_Program) changes the priority
   --  of an environment task. So if we have more than one such pragma in the
   --  set of processed sources, the pragma that is processed last, defines the
   --  priority of an environment task.

--   type Ceiling_Violations_Rule_Type is new Global_Rule_Template with
--     null record;

--   procedure Collect_Global_Info_Pre_Op
--     (Rule    : in out Ceiling_Violations_Rule_Type;
--      Element :        Asis.Element;
--      Control : in out Traverse_Control;
--      State   : in out Rule_Traversal_State);
   --  Tries to detect the priority of the given scope from pragmas Priority
   --  and Interrupt_Priority (if any).

--   procedure Analyze_Global_Structure
--     (Rule : Ceiling_Violations_Rule_Type);
   --  If for the given node the priority is not defined, computes the priority
   --  from the call graph: for a protected operation, takes the priority from
   --  the node corresponding to protected definition, for other nodes, or in
   --  case if there is no priority setting pragmas applied to a protected
   --  definition, "inherits" the priority of an enclosing scope.

--   procedure Check_Global_Structure_Node
--     (Rule     :     Ceiling_Violations_Rule_Type;
--      N        :     Gnatcheck.Global_State.GS_Node_Id;
--      Detected : out Boolean);
   --  If the given call graph node represents a protected operation, checks
   --  that all the callers have priorities not higher, then the protected
   --  operation. Dynamic priority is considered as a highest priority level.

--   procedure Init_Rule (Rule : in out Ceiling_Violations_Rule_Type);

--   Ceiling_Violations_Rule : aliased Ceiling_Violations_Rule_Type;

   ----------------------------------
   -- Controlled_Type_Declarations --
   ----------------------------------

   --  Declarations of controlled types are detected.

   --  A declaration of a private type is detected if its full declaration
   --  defines a controlled type.

   --  A declaration of a derived type is detected if its ancestor type is
   --  controlled.

   --  Subtype declarations are not detected.

   --  The declaration of a type that itself is not a descendant of a type
   --  declared  in Ada.Finalization but has a controlled component is not
   --  detected

   type Controlled_Type_Declarations_Rule_Type is new Rule_Template with
     null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Controlled_Type_Declarations_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is a declaration of a controlled type as described
   --  above

   procedure Init_Rule (Rule : in out Controlled_Type_Declarations_Rule_Type);

   Controlled_Type_Declarations_Rule :
     aliased Controlled_Type_Declarations_Rule_Type;

   ----------------------------
   -- Declarations_In_Blocks --
   ----------------------------

   --  Block statements with local declarations are detected.

   --  A declare block with an empty declarative_part or with a declarative
   --  part containing only pragmas and/or use clauses is not detected.

   type Declarations_In_Blocks_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Declarations_In_Blocks_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is a block statement containing local declarations.

   procedure Init_Rule (Rule : in out Declarations_In_Blocks_Rule_Type);

   Declarations_In_Blocks_Rule : aliased Declarations_In_Blocks_Rule_Type;

   ------------------------
   -- Default_Parameters --
   ------------------------

   --  Default expressions for subprogram parameters are detected.
   --  Parameter declarations of formal and generic subprograms are also
   --  considered

   type Default_Parameters_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Default_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is a parameter specification containing a default
   --  initialization expression

   procedure Init_Rule (Rule : in out Default_Parameters_Rule_Type);

   Default_Parameters_Rule : aliased Default_Parameters_Rule_Type;

   ---------------------------
   -- Discriminated_Records --
   ---------------------------

   --  Declarations of record types with discriminants are detected.

   --  Only the declarations of record and record extension types are detected.
   --  Incomplete, formal, private, derived and private extension type
   --  declarations are not detected. Task and protected type declarations also
   --  are not detected.

   type Discriminated_Records_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Discriminated_Records_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is known discriminant part in a record or record
   --  extension definition

   procedure Init_Rule (Rule : in out Discriminated_Records_Rule_Type);

   Discriminated_Records_Rule : aliased Discriminated_Records_Rule_Type;

   ------------------------------
   -- Expanded_Loop_Exit_Names --
   ------------------------------

   --  According to the definition of the Spark language, the loop name in an
   --  exit statement should be only a simple loop name, expanded names are not
   --  allowed.

   --  Expanded loop name in an exit statement is detected

   type Expanded_Loop_Exit_Names_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Expanded_Loop_Exit_Names_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is an exit statement, checks if the statement contains a loop
   --  name and if it does, check that this is a simple name

   procedure Init_Rule (Rule : in out Expanded_Loop_Exit_Names_Rule_Type);

   Expanded_Loop_Exit_Names_Rule : aliased Expanded_Loop_Exit_Names_Rule_Type;

   ---------------------------
   -- Float_Equality_Checks --
   ---------------------------

   --  Calls to the predefined equality operations for floating point types
   --  are detected

   --  Both "=" and "/=" operations are checked.

   --  A floating point type here is a type declared with floating point
   --  definition or a type derived from a floating point type

   --  User-defined equality operations are not detected.

   --  "=" and "/=" operations for fixed point types are not detected

   type Float_Equality_Checks_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Float_Equality_Checks_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks if Element is a name of a predefined "=" or "/=" operator from
   --  a (function) call with float arguments

   procedure Init_Rule (Rule : in out Float_Equality_Checks_Rule_Type);

   Float_Equality_Checks_Rule : aliased Float_Equality_Checks_Rule_Type;

   ---------------------
   -- GOTO_Statements --
   ---------------------

   --  goto statements are detected.

   type GOTO_Statements_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out GOTO_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is a goto statement

   procedure Init_Rule (Rule : in out GOTO_Statements_Rule_Type);

   GOTO_Statements_Rule : aliased GOTO_Statements_Rule_Type;

   ----------------------
   -- Improper_Returns --
   ----------------------

   --  A return statement in a procedure body is detected

   --  The situation when there is more than exactly one return statement in a
   --  function body is detected (diagnostic message is generated for all the
   --  return statements except the first one).

   --  Should we should split this rule into two rules - one for procedures,
   --  and another one for functions?

   type Improper_Returns_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Improper_Returns_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a return statement, checks that the enclosing subprogram
   --  body is not a procedure body, and if it is a function body, checks that
   --  this is the first return statement in the body.

   procedure Init_Rule (Rule : in out Improper_Returns_Rule_Type);

   Improper_Returns_Rule : aliased Improper_Returns_Rule_Type;

   -----------------------------------------
   -- Improperly_Called_Protected_Entries --   turned off!!! --
   -----------------------------------------

   --  There are situations when it is required that the maximal number of
   --  tasks calling a given protected entry is 1.

   --  Protected entries that can be called more than one task are detected.

--   type Improperly_Called_Protected_Entries_Rule_Type is
--     new Global_Rule_Template with null record;

--   procedure Check_Global_Structure_Node
--     (Rule     :     Improperly_Called_Protected_Entries_Rule_Type;
--      N        :     Gnatcheck.Global_State.GS_Node_Id;
--      Detected : out Boolean);
--   --  Checks if there are more than one task calling the given protected
--   --  entry.

--   procedure Init_Rule
--     (Rule : in out Improperly_Called_Protected_Entries_Rule_Type);

--   Improperly_Called_Protected_Entries_Rule :
--     aliased Improperly_Called_Protected_Entries_Rule_Type;

   -----------------------
   -- Limited_Renamings --
   -----------------------

   --  This rule combines a part of the SPARK limitations imposed on renamings.
   --  It is formulated for renaming declarations only, not for the use of
   --  renamed entities. The following situations should be detected:

   --  Syntax of renaming declarations: all forms of  renaming declarations
   --  except package and subprogram renaming declarations should be detected.

   --  Renamed entities: only subprograms declared immediately within package
   --  specs and only child packages can be renamed. All other cases of
   --  subprogram and package renamings should be detected.

   --  Limitations on new and old names: the name after RENAMES should have
   --  the form package_name.entity_name, the new name should be exactly the
   --  same as entity_name, entity_name should not itself be detected by a
   --  renaming declaration. Moreover, in case of a subprogram renaming, the
   --  new and the old profiles should be the same (the same parameter and
   --  subtype names should be used). All the renamings that do not follow
   --  these rules should be detected.

   --  Placement of renamings. According to the definition of SPARK, the
   --  following restrictions are imposed on the placement of renaming
   --  declarations:
   --
   --  for subprogram renamings (that are not operator renamings):
   --  - if the package where the subprogram is declared is a local package,
   --    renaming is allowed immediately after the declaration of this
   --    package;
   --  - if the package where the subprogram is declared is a library-level
   --    package, renaming is allowed at the start of the body of a library
   --    package (or library subprogram) that "withes" this package;
   --
   --  for operator renamings: the same is allowed as for subprogram renamings
   --  plus renamings are allowed at the start of the visible or private part
   --  of the library package that "withes" the package where the operator is
   --  declared;
   --
   --  for package renamings: the renaming of a (child!) package is allowed
   --  only immediately within a library package or library subprogram that
   --  "withes" this package;
   --
   --  All the other cases of renaming declaration placement should be
   --  detected.
   --
   --  Note that the rules about allowed renaming placement are taken from the
   --  definition of the SPARK subset. This definition uses the following
   --  wording: "at the start of the body", "at the start of the visible
   --  (private) part", and it the same time the SPARK book contains examples
   --  where an (allowed) renaming follows an object declaration. So we do
   --  not check that renamings precede all the non-renaming declarations.

   --  All the things stated above are checked and detected in the order given.
   --  If some feature is detected for a renaming declaration, all the other
   --  things are not checked for this renaming declaration

   --  Current implementation limitation: an the moment this rule is not
   --  completely checked for renaming of predefined operators.

   type Limited_Renamings_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Limited_Renamings_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a renaming declaration, checks the rules stated above in
   --  the order given. As soon as the first rule violation is detected, stops
   --  further processing

   procedure Init_Rule (Rule : in out Limited_Renamings_Rule_Type);

   Limited_Renamings : aliased Limited_Renamings_Rule_Type;

   -------------------------------
   -- Library_Level_Subprograms --
   -------------------------------

   --  Library level subprograms (including subprogram instantiations) are
   --  detected.

   type Library_Level_Subprograms_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Library_Level_Subprograms_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is a library_Level subprogram

   procedure Init_Rule (Rule : in out Library_Level_Subprograms_Rule_Type);

   Library_Level_Subprograms_Rule :
     aliased Library_Level_Subprograms_Rule_Type;

   --------------------
   -- Local_Packages --
   --------------------

   --  Local packages in package and generic package specs are detected.

   --  Local packages in bodies are not detected

   type Local_Packages_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Local_Packages_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is a package spec enclosed in another (generic)
   --  package spec

   procedure Init_Rule (Rule : in out Local_Packages_Rule_Type);

   Local_Packages_Rule : aliased Local_Packages_Rule_Type;

   -----------------------------------------------
   -- Multiple_Entries_In_Protected_Definitions --
   -----------------------------------------------

   --  At most one entry is allowed in protected object (or in protected type).
   --  This is the requirement from the Ravenscar Profile.

   --  The situations when a protected definition defines more than one entry
   --  are detected (diagnostic messages are generated for all the entry
   --  declarations except the first one).

   --  An entry family is counted as one entry.

   --  Entries from the private part of the protected definition are also
   --  counted

   type Multiple_Entries_In_Protected_Definitions_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Multiple_Entries_In_Protected_Definitions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is An_Entry_Declaration, checks if the enclosing element is
   --  A_Protected_Definition and if there are some other entries declared for
   --  the given protected object or type. In case if the protected type/object
   --  has more than one entry, the diagnostic message is generated the all but
   --  the first entry.

   procedure Init_Rule
     (Rule : in out Multiple_Entries_In_Protected_Definitions_Rule_Type);

   Multiple_Entries_In_Protected_Definitions_Rule :
     aliased Multiple_Entries_In_Protected_Definitions_Rule_Type;

   ------------------
   -- Name_Clashes --
   ------------------

   --  Detect that some names are not used as defining identifiers. To activate
   --  this rule, one have to provide a reference to a dictionary file as a
   --  rule parameter(s), more than one dictionary file can be specified. If
   --  no dictionary file is set, nothing is detected.

   --  Only defining identifiers are detected. but not references. The check is
   --  not case-sensitive.

   --  See Gnatcheck.Name_Dictionary (spec) for the description of the
   --  dictionary structure

   type Name_Clashes_Rule_Type is new Rule_Template with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Name_Clashes_Rule_Type);
   --  Activates the rule with the dictionary containing "one", "two" and
   --  "three"

   procedure Rule_Check_Pre_Op
     (Rule    : in out Name_Clashes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a defining identifier, checks if it is not in the
   --  dictionary of the forbidden names

   procedure Init_Rule (Rule : in out Name_Clashes_Rule_Type);

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Name_Clashes_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At :        String);
   --  If Enable is OFF, this procedure does nothing. If Enable is ON, treats
   --  Param as a name of the dictionary file. Scans this file and fill in the
   --  dictionary of forbidden names. The rule is turned into 'Enabled" state
   --  if the dictionary contains at least one entry. (Note that this entry may
   --  be created by the previous call to this routine)
   --
   --  We may want to have more sophisticated way of controlling the dictionary
   --  of forbidden names. For example, the option "-RRestrict_Name_Space:dict"
   --  may remove from the dictionary of forbidden names the names that are
   --  stated in 'dict'
   --
   --  No warning about parameter redefinition is issued by this procedure
   --  because more than one dictionary file is allowed.

   procedure XML_Rule_Help
     (Rule  : Name_Clashes_Rule_Type;
      Level : Natural);
   --  Generates the field tag

   overriding function Rule_Option
     (Rule          : Name_Clashes_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return          String;
   --  For Template_All_ON  pints out rule option in the format
   --  "-- +R rule_name : dictionary" because we cannot activate this rule in
   --  the coding standard template because it requires a real dictionary file
   --  as a parameter

   overriding function More_Rule_Comment
     (Rule          : Name_Clashes_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return String;
   --  Adds the note that the rule may be activated only with a real dictionary
   --  file as a parameter.

   Name_Clashes_Rule : aliased Name_Clashes_Rule_Type;

   ------------------------------
   -- Non_Qualified_Aggregates --
   ------------------------------

   --  According to the definition of the Spark language, all the aggregates
   --  should be explicitly qualified

   --  Non-qualified aggregates are detected

   --  A non qualified aggregate is an aggregate that is not an immediate
   --  component of a qualified expression.

   --  A string literal is not considered as aggregate by this rule, but an
   --  array aggregate of some string type is considered as a normal aggregate.

   type Non_Qualified_Aggregates_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Non_Qualified_Aggregates_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is an aggregate, checks that the enclosing element is
   --  of A_Qualified_Expression kind

   procedure Init_Rule (Rule : in out Non_Qualified_Aggregates_Rule_Type);

   Non_Qualified_Aggregates_Rule : aliased Non_Qualified_Aggregates_Rule_Type;

   --------------------------
   -- Non_SPARK_Attributes --
   --------------------------

   --  The definition of the Spark language defines a subset  of Ada 95
   --  attribute designators that can be used in Spark programs.

   --  The use of any attribute that is not from this subset is detected

   type Non_SPARK_Attributes_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Non_SPARK_Attributes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is an attribute, checks that this is the attribute from the
   --  Spark subset of Ada 95 attributes

   procedure Init_Rule (Rule : in out Non_SPARK_Attributes_Rule_Type);

   Non_SPARK_Attributes_Rule : aliased Non_SPARK_Attributes_Rule_Type;

   ------------------------------
   -- Non_Tagged_Derived_Types --
   ------------------------------

   --  According to the definition of the Spark language, "derived types are
   --  only used for type extensions". Derived type declarations that do not
   --  have a record extension part are detected

   type Non_Tagged_Derived_Types_Rule_Type is new Rule_Template with
     null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Non_Tagged_Derived_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks if Element is of A_Derived_Type_Definition kind

   procedure Init_Rule (Rule : in out Non_Tagged_Derived_Types_Rule_Type);

   Non_Tagged_Derived_Types_Rule : aliased Non_Tagged_Derived_Types_Rule_Type;

   ----------------------
   -- Outer_Loop_Exits --
   ----------------------

   --  According to the definition of the Spark language, an exit statement can
   --  transfer control only out of the immediately enclosing loop statement.
   --  An exit statement containing a loop name that is not the name of the
   --  immediately enclosing loop statement is detected.

   type Outer_Loop_Exits_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Outer_Loop_Exits_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is an exit statement with a loop name that does not
   --  denote an immediately enclosing loop

   procedure Init_Rule (Rule : in out Outer_Loop_Exits_Rule_Type);

   Outer_Loop_Exits_Rule : aliased Outer_Loop_Exits_Rule_Type;

   --------------------------
   -- Overloaded_Operators --
   --------------------------

   --  According to the definition of the Spark language, the designator of a
   --  function cannot be an operator, because operator overloading is not
   --  allowed in Spark.

   --  A function declaration with an operator symbol as a defining designator
   --  is detected.

   --  A function body is detected only if the body does not have a separate
   --  spec

   --  Formal functions are also considered for this rule

   --  For a renaming declarations, only renaming-as-declaration is detected

   type Overloaded_Operators_Rule_Type is new Rule_Template with
     null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Overloaded_Operators_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is a defining operator symbol.

   procedure Init_Rule (Rule : in out Overloaded_Operators_Rule_Type);

   Overloaded_Operators_Rule : aliased Overloaded_Operators_Rule_Type;

   ------------
   -- Slices --
   ------------

   --  The definition of the Spark language does not have a slice as a variant
   --  of the name syntax structure.

   --  The use of slices is detected.

   type Slices_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Slices_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is a slice

   procedure Init_Rule (Rule : in out Slices_Rule_Type);

   Slices_Rule : aliased Slices_Rule_Type;

   ---------------------------------
   -- Unconstrained_Array_Returns --
   ---------------------------------

   --  Functions returning unconstrained arrays are detected

   --  Functions declarations, function bodies (and body stubs) having no
   --  separate declarations and function instantiations are detected. Function
   --  calls are not detected. Function renamings are not detected.
   --
   --  The rule has the following parameters:
   --
   --  * for +R option:
   --
   --       Except_String - if this parameter is specified, functions that
   --                       return predefined type String or any type derived
   --                       from it (directly or indirectly) are not flagged

   type Unconstrained_Array_Returns_Rule_Type is new
     Rule_With_Exceptions_Template (1) with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Unconstrained_Array_Returns_Rule_Type);
   --  Activates the rule with Except_String parameter.

   function Exception_Name
     (Rule      : Unconstrained_Array_Returns_Rule_Type;
      Exc_Index : Exception_Index)
      return      String;

   function Exception_Number
     (Rule     : Unconstrained_Array_Returns_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unconstrained_Array_Returns_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is the declaration of a function returning an
   --  unconstrained array type

   procedure Init_Rule (Rule : in out Unconstrained_Array_Returns_Rule_Type);

   Unconstrained_Array_Returns_Rule :
     aliased Unconstrained_Array_Returns_Rule_Type;

   ----------------------
   -- Universal_Ranges --
   ----------------------

   --  In the Spark language, ranges with the bounds of universal_integer
   --  type are not allowed in index constraints, constrained array definitions
   --  and FOR-loop parameter specifications.

   --  Discrete ranges that are a part of index constrains, constrained array
   --  definition or FOR-loop parameter specification, and that have both
   --  bounds of the universal type integer type are detected. Ranges that have
   --  at least one bound of a specific type, (such as '1 .. N', where N is a
   --  variable or an expression of non-universal type) are not detected.

   type Universal_Ranges_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Universal_Ranges_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element if of A_Discrete_Simple_Expression_Range kind and its
   --  enclosing Element is either A_Loop_Parameter_Specification, or
   --  An_Index_Constraint, or A_Discrete_Subtype_Definition, checks that
   --  both bounds are of universal_integer type.

   procedure Init_Rule (Rule : in out Universal_Ranges_Rule_Type);

   Universal_Ranges_Rule : aliased Universal_Ranges_Rule_Type;

   ------------------------
   -- Unused_Subprograms -- turned off!!! --
   ------------------------

   --  Unused subprograms are detected.

   --  In some sense this rule duplicates the functionality of gnatelim

   --  For the moment the main purpose of this rule is to use gnatelim tests
   --  to check the gnatcheck call graph.

   --  ??? What kinds of callable entities should be checked here?

--   type Unused_Subprograms_Rule_Type is new Global_Rule_Template with
--     null record;

--   procedure Analyze_Global_Structure (Rule : Unused_Subprograms_Rule_Type);
   --  Sets Is_Used flags by analyzing the Call Graph entry points and call
   --  chains. Does not detect any rule violation.

--   procedure Check_Global_Structure_Node
--     (Rule     :     Unused_Subprograms_Rule_Type;
--      N        :     Gnatcheck.Global_State.GS_Node_Id;
--      Detected : out Boolean);
   --  Checks if the given call graph node is called by some other (used!)
   --  node.

--   procedure Init_Rule (Rule : in out Unused_Subprograms_Rule_Type);

--   Unused_Subprograms_Rule : aliased Unused_Subprograms_Rule_Type;

   -------------------------
   -- USE_PACKAGE_Clauses --
   -------------------------

   --  Use package clauses are detected. Use type clauses are not detected.

   type USE_PACKAGE_Clauses_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out USE_PACKAGE_Clauses_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks that Element is a use package clause

   procedure Init_Rule (Rule : in out USE_PACKAGE_Clauses_Rule_Type);

   USE_PACKAGE_Clauses_Rule : aliased USE_PACKAGE_Clauses_Rule_Type;

   ----------------------------------------------
   -- Volatile_Objects_Without_Address_Clauses --
   ----------------------------------------------

   --  A volatile object that does not have an address clause applied to it is
   --  detected
   --
   --  Only variable declarations are checked. This rule treats an object as a
   --  volatile object if pragma or aspect Volatile is applied to the object
   --  or to its type, if the object is atomic or if the GNAT compiler
   --  considers this object as volatile because of some code generation
   --  reasons.

   type Volatile_Objects_Without_Address_Clauses_Rule_Type is
     new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Volatile_Objects_Without_Address_Clauses_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);

   procedure Init_Rule
     (Rule : in out Volatile_Objects_Without_Address_Clauses_Rule_Type);

   Volatile_Objects_Without_Address_Clauses_Rule :
     aliased Volatile_Objects_Without_Address_Clauses_Rule_Type;

end Gnatcheck.Rules.Default;
