------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . R U L E S . C U S T O M _ 1              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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
--  to satisfy some specific requests from gnatcheck users

pragma Ada_2012;

with Ada.Containers.Ordered_Sets;
package Gnatcheck.Rules.Custom_1 is

   function Eq (Left, Right : String_Access) return Boolean;
   function "<" (Left, Right : String_Access) return Boolean;

   package String_Access_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => String_Access,
      "="          => Eq);
   --  To be used as simple dictionaries

   use String_Access_Sets;

   ----------------------
   -- Anonymous_Arrays --
   ----------------------

   --  Definitions of anonymous array types in the object declarations should
   --  be detected.
   --
   --  The rule does not have any parameter.

   type Anonymous_Arrays_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Anonymous_Arrays_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents an array type definition, checks if the enclosing
   --  element is an object declaration

   procedure Init_Rule (Rule : in out Anonymous_Arrays_Rule_Type);

   Anonymous_Arrays_Rule : aliased Anonymous_Arrays_Rule_Type;

   ------------------------------------------
   -- Enumeration_Ranges_In_CASE_Statements --
   ------------------------------------------

   --  Detect use of a range of enumeration literals as a choice in a case
   --  statement. All the possible forms of specifying a range (explicit
   --  ranges, such as A .. B, subtype marks and 'Range attributes) are
   --  flagged. An enumeration range is flagged even if contains exactly one
   --  enumeration value or no values at all. A type derived from an
   --  enumeration type is considered as an enumeration type.
   --
   --  This rule has no parameters.
   --
   --  The original enhancement request says nothing about any exception to
   --  this rule. But when the range of the case control expression subtype is
   --  too big (for example, Character or even Wide_Character), using ranges
   --  is the only practical way to write a case statement. Should we have a
   --  parameter that somehow controls the exceptions to the rule ???

   type Enumeration_Ranges_In_CASE_Statements_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Enumeration_Ranges_In_CASE_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks if Element is an enumeration discrete range used as a choice in
   --  a case statement.

   procedure Init_Rule
     (Rule : in out Enumeration_Ranges_In_CASE_Statements_Rule_Type);

   Enumeration_Ranges_In_CASE_Statements_Rule :
     aliased Enumeration_Ranges_In_CASE_Statements_Rule_Type;

   --------------------------------
   -- Exceptions_As_Control_Flow --
   --------------------------------

   --  Detect cases when an exception is raised and handled in the same
   --  subprogram body. Raise statements that are part of exception handlers
   --  are not flagged. Raise statements that are parts of package bodies, task
   --  bodies or entry bodies are not flagged.
   --
   --  The rule does not have any parameter.
   --
   --  Rationale: this rule is aimed against using exceptions to implement
   --             a normal control flow.

   type Exceptions_As_Control_Flow_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Exceptions_As_Control_Flow_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a raise statement with an exception name, checks if it is
   --  located in a subprogram body and that body contains a handler for this
   --  exception or a handler with OTHERS choice.

   procedure Init_Rule (Rule : in out Exceptions_As_Control_Flow_Rule_Type);

   Exceptions_As_Control_Flow_Rule :
     aliased Exceptions_As_Control_Flow_Rule_Type;

   ---------------------------------------
   -- EXIT_Statements_With_No_Loop_Name --
   ---------------------------------------

   --  Exit statements that do not contain the name of the loop being exited
   --  are detected.
   --
   --  The rule does not have any parameter.

   type EXIT_Statements_With_No_Loop_Name_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out EXIT_Statements_With_No_Loop_Name_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is an exit statement checks if it contain the name of the
   --  loop being exited.

   procedure Init_Rule
     (Rule : in out EXIT_Statements_With_No_Loop_Name_Rule_Type);

   EXIT_Statements_With_No_Loop_Name_Rule :
     aliased EXIT_Statements_With_No_Loop_Name_Rule_Type;

   -----------------------------------
   -- Explicit_Full_Discrete_Ranges --
   -----------------------------------

   --  A discrete range of the form A'First .. A'Last should be detected.
   --
   --  The rule does not have any parameter.

   type Explicit_Full_Discrete_Ranges_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Explicit_Full_Discrete_Ranges_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a discrete range, checks if it has the form of
   --  A'First .. A'Last.

   procedure Init_Rule (Rule : in out Explicit_Full_Discrete_Ranges_Rule_Type);

   Explicit_Full_Discrete_Ranges_Rule :
     aliased Explicit_Full_Discrete_Ranges_Rule_Type;

   --------------------------
   -- Forbidden_Attributes --
   --------------------------

   --  A use of a specified attributes is detected. Attribute designator (s) to
   --  be checked for should be provided as the rule parameter(s).
   --
   --  The rule has the following parameters:
   --
   --  * For +R option
   --
   --      Attribute_Designator - adds the specified attribute to the set of
   --                    attributes to be checked and sets the checks for all
   --                    the specified attributes ON. If Attribute_Designator
   --                    does not correspond to any attribute designator
   --                    defined in the Ada Standard or to any designator of
   --                    the GNAT-specific attribute defined in the GNAT
   --                    Reference Manual, this is treated as a name of unknown
   --                    attribute.
   --
   --      GNAT        - all the GNAT-specific attributes are detected, this
   --                    also sets the checks for all the other specified
   --                    attributes ON.
   --
   --      ALL         - all the attributes are detected, this sets the rule
   --                    ON.
   --
   --  * For -R option
   --
   --      Attribute_Designator - removes the specified attribute from the set
   --                    of attributes to be checked. This does not turn the
   --                    rule (that is, checks for other attributes) ON or OFF.
   --                    If Attribute_Designator does not correspond to any
   --                    attribute designator defined in the Ada Standard or to
   --                    any designator of the GNAT-specific attribute defined
   --                    in the GNAT Reference Manual, this option is
   --                    treated as turning OFF detection of all the unknown
   --                    attributes.
   --
   --      GNAT        - turn OFF detection of all the GNAT-specific
   --                    attributes.
   --
   --      ALL         - cleans the list of the attributes to be detected and
   --                    turns the rule OFF.
   --
   --  Parameters are not case sensitive, If Attribute_Designator does not have
   --  a syntax of Ada identifier and, therefore, can not be considered as a
   --  (part of) the attribute designator, a diagnostic message is generated
   --  and the corresponding parameter is ignored. (If a specific attribute
   --  allows a static expression to be a part of the attribute designator,
   --  an expression should not be used as a part of the rule parameter, and
   --  for such attributes only an identifiers from the designator are
   --  checked.)
   --
   --  When more than one parameter is given in the same rule option, the
   --  parameters should be are separated by a comma.
   --
   --  If more than one option for this rule is specified for the gnatcheck
   --  call, a new option overrides the previous one(s).
   --
   --  +R option with no parameter turns the rule ON with the set of attributes
   --  to be detected defined by the previous rule options. (Note, that by
   --  default this set is empty, so if the only option specified for the rule
   --  is '+RForbidden_Attributes' (with no parameter), then the rule is
   --  enabled, but it does not detect anything). -R option with no parameter
   --  turns the rule OFF, but it does not affect a set of attributes to be
   --  detected.

   type Forbidden_Attributes_Rule_Type is new Rule_Template with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Forbidden_Attributes_Rule_Type);
   --  Enables rule with parameters Range, Access, Img (GNAT-specific) and
   --  Object_Size (GNAT-specific)

   procedure Rule_Check_Pre_Op
     (Rule    : in out Forbidden_Attributes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is an attribute, checks that this attribute should be
   --  detected according to the current rule settings

   procedure Init_Rule (Rule : in out Forbidden_Attributes_Rule_Type);

   procedure Process_Rule_Parameter
     (Rule       : in out Forbidden_Attributes_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String);
   --  Process the Param string according to the parameter structure described
   --  above
   --
   --  No warning about parameter redefinition is issued by this procedure.

   procedure XML_Rule_Help
     (Rule  : Forbidden_Attributes_Rule_Type;
      Level : Natural);

   procedure Print_Rule
     (Rule         : Forbidden_Attributes_Rule_Type;
      Indent_Level : Natural := 0);

   overriding procedure Print_Rule_To_File
     (Rule         : Forbidden_Attributes_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actual settings for forbidden attributes

   overriding procedure XML_Print_Rule
     (Rule         : Forbidden_Attributes_Rule_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actual settings for forbidden attributes

   overriding function Allows_Parametrized_Exemption
     (Rule : Forbidden_Attributes_Rule_Type)
      return Boolean is (True);

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Forbidden_Attributes_Rule_Type;
      Parameter : String)
      return  Boolean;

   overriding function Rule_Parameter
     (Rule : Forbidden_Attributes_Rule_Type;
      Diag : String)
      return String;

   Forbidden_Attributes_Rule : aliased Forbidden_Attributes_Rule_Type;

   -----------------------
   -- Forbidden_Pragmas --
   -----------------------

   --  A use of a specified pragmas is detected. Pragma name(s) to be checked
   --  for should be provided as the rule parameter(s).
   --
   --  The rule has the following parameters:
   --
   --  * For +R option
   --
   --      Pragma_Name - adds the specified pragma to the set of pragmas to
   --                    be checked and sets the checks for all the specified
   --                    pragmas ON. Pragma_Name is treated as a name of a
   --                    pragma. If it does not correspond to any pragma name
   --                    defined in the Ada Standard or to any name of the
   --                    GNAT-specific pragma defined in the GNAT Reference
   --                    Manual, this is treated as a name of unknown pragma.
   --
   --      GNAT        - all the GNAT-specific pragmas are detected, this sets
   --                    the checks for all the specified pragmas ON.
   --
   --      ALL         - all the pragmas are detected, this sets the rule ON.
   --
   --  * For -R option
   --
   --      Pragma_Name - removes the specified pragma from the set of pragmas
   --                    to be checked. This does not turn the rule (that is,
   --                    checks for other pragmas) ON or OFF. Pragma_Name is
   --                    treated as a name of a pragma. If it does not
   --                    correspond to any pragma name defined in the Ada
   --                    Standard or to any name of the GNAT-specific pragma
   --                    defined in the GNAT Reference Manual, this option is
   --                    treated as turning OFF detection of all the unknown
   --                    pragmas.
   --
   --      GNAT        - turn OFF detection of all the GNAT-specific pragmas
   --
   --      ALL         - cleans the list of the pragmas to be detected and
   --                    turns the rule OFF.
   --
   --  Parameters are not case sensitive, If Pragma_Name does not have a syntax
   --  of Ada identifier and, therefore, can not be considered as a pragma
   --  name, a diagnostic message is generated and the corresponding parameter
   --  is ignored.
   --
   --  When more than one parameter is given in the same rule option, the
   --  parameters should be are separated by a comma.
   --
   --  If more than one option for this rule is specified for the gnatcheck
   --  call, a new option overrides the previous one(s).
   --
   --  +R option with no parameter turns the rule ON with the set of pragmas
   --  to be detected defined by the previous rule options. (Note, that by
   --  default this set is empty, so if the only option specified for the rule
   --  is '+RForbidden_Pragmas' (with no parameter), then the rule is enabled,
   --  but it does not detect anything). -R option with no parameter turns the
   --  rule OFF, but it does not affect a set of pragmas to be detected.

   type Forbidden_Pragmas_Rule_Type is new Rule_Template with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Forbidden_Pragmas_Rule_Type);
   --  Enables rule with parameters Inline, Suppress, Initialize_Scalars
   --  (GNAT-specific) and Keep_Names  (GNAT-specific)

   procedure Rule_Check_Pre_Op
     (Rule    : in out Forbidden_Pragmas_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a pragma, checks that this pragma should be detected
   --  according to the current rule settings

   procedure Init_Rule (Rule : in out Forbidden_Pragmas_Rule_Type);

   procedure Process_Rule_Parameter
     (Rule       : in out Forbidden_Pragmas_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String);
   --  Process the Param string according to the parameter structure described
   --  above
   --
   --  No warning about parameter redefinition is issued by this procedure.

   procedure XML_Rule_Help
     (Rule  : Forbidden_Pragmas_Rule_Type;
      Level : Natural);

   procedure Print_Rule
     (Rule         : Forbidden_Pragmas_Rule_Type;
      Indent_Level : Natural := 0);

   overriding procedure Print_Rule_To_File
     (Rule         : Forbidden_Pragmas_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actual settings for forbidden pragmas

   overriding procedure XML_Print_Rule
     (Rule         : Forbidden_Pragmas_Rule_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actual settings for forbidden pragmas

   overriding function Allows_Parametrized_Exemption
     (Rule : Forbidden_Pragmas_Rule_Type)
      return Boolean is (True);

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Forbidden_Pragmas_Rule_Type;
      Parameter : String)
      return  Boolean;

   overriding function Rule_Parameter
     (Rule : Forbidden_Pragmas_Rule_Type;
      Diag : String)
      return String;

   Forbidden_Pragmas_Rule : aliased Forbidden_Pragmas_Rule_Type;

   -------------------------------
   -- Function_Style_Procedures --
   -------------------------------

   --  Detect procedures that can be rewritten as functions. A procedure can be
   --  converted into a function if it has exactly one parameter of mode IN and
   --  no parameter of IN OUT mode. Procedure declarations, formal procedure
   --  declarations and generic procedure declarations are always flagged.
   --  Procedure bodies and body stubs are flagged only if they do not have
   --  corresponding separate declarations. Procedure renamings and procedure
   --  instantiations are not flagged.
   --
   --  If a procedure can be rewritten as a function, but its OUT parameter is
   --  of limited type, this procedure is not flagged.
   --
   --  Protected procedures are not flagged. Null procedures also are not
   --  flagged.
   --
   --  The rule does not have any parameter.

   type Function_Style_Procedures_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Function_Style_Procedures_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a (generic/formal) procedure declaration or a procedure
   --  body (body stub) that acts as a spec, checks if this procedure can be
   --  rewritten as a function.

   procedure Init_Rule (Rule : in out Function_Style_Procedures_Rule_Type);

   Function_Style_Procedures_Rule :
     aliased Function_Style_Procedures_Rule_Type;

   -----------------------------
   -- Generics_In_Subprograms --
   -----------------------------

   --  Declarations of generic units in subprograms are detected. Generic
   --  declarations in the bodies of generic subprograms are also flagged.
   --  Generic nested into another generic is never flagged. If a generic is
   --  declared in a local package that is declared in a subprogram body, this
   --  generic is flagged.
   --
   --  The rule does not have any parameter.
   --
   --  Rationale: this rule was originally provided as being related to code
   --             safety, complexity and efficiency.
   --
   --  Problems:
   --
   --     What about generic units declared in task and entry bodies? In block
   --     statements?

   type Generics_In_Subprograms_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Generics_In_Subprograms_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element represents a generic unit declarations, checks if it is
   --  declared in a subprogram body

   procedure Init_Rule (Rule : in out Generics_In_Subprograms_Rule_Type);

   Generics_In_Subprograms_Rule : aliased Generics_In_Subprograms_Rule_Type;

   -----------------------
   -- Identifier_Casing --
   -----------------------

   --  Flag each defining identifier that does not have a casing corresponding
   --  to the kind of entity being declared. All the defining names are
   --  checked. For The defining names from the following kinds of declarations
   --  a special casing scheme can be defined:
   --
   --     * type and subtype declarations;
   --     * enumeration literal specifications (not including character
   --       literals), and function renaming declarations if the renaming
   --       entity is an enumeration literal;
   --     * constant and named number declarations (including object renaming
   --       declarations if the renamed object is a constant)
   --     * exception declarations and exception renaming declarations
   --
   --  The rule may have the following parameters:
   --
   --  o For the `+R' option:
   --
   --    Type=casing_scheme
   --          Specifies casing for names from type and subtype declarations.
   --
   --    Enum=casing_scheme
   --          Specifies casing for defining enumeration literals and of the
   --          defining name in a function renaming declaration if the renamed
   --          entity is an enumeration literal.
   --
   --    Constant=casing_scheme
   --          Specifies the casing for a defining name from a constant or
   --          number declaration, including the object renaming declaration if
   --          the renamed object is a constant
   --
   --    Exception=casing_scheme
   --          Specifies the casing for names from exception declarations and
   --          exception renaming declarations.
   --
   --    Others=casing_scheme
   --          Specifies the casing for all the defining names for that a
   --          special casing scheme is not specified. If this parameter is not
   --          set, the casing for the entities that do not correspond to
   --          the specified parameters is not checked
   --
   --    Exclude=dictionary_file
   --          Specifies casing exceptions. Is a defining identifier is listed
   --          in some dictionary file, it should be using the same casing as
   --          it is specified in the dictionary
   --
   --  Where
   --
   --     casing_scheme ::= upper|lower|mixed
   --        upper means that the identifier should be upper-case
   --        lower means that the identifier should be lower-case
   --        mixed means that the first identifier letter and the first letter
   --           after underscore should be upper-case, and all the other
   --           letters should be lower-case;
   --
   --  If a defining identifier is from a declaration for that a specific
   --  casing scheme can be set, but the corresponding parameter is not
   --  specified for the rule, then the casing scheme defined by 'Others'
   --  parameter is used to check this identifier. If 'Others' parameter also
   --  is not set, the identifier is not checked.
   --
   --  dictionary_file is the name of the text file that contains casing
   --  exceptions. There are two kinds of exceptions:
   --
   --  * identifier - if a dictionary file contains an identifier, then any
   --                 (defining) identifier in the checked source should use
   --                 exactly the same casing as the identifier included in
   --                 dictionary_file
   --
   --  * wildcard   - a wildcard has the following syntax:
   --
   --                   wildcard := *identifier | *identifier* | identifier*
   --
   --                 where 'identifier' is an identifier in Ada sense, but
   --                 it does not allowed to contain underscores. Wildcards
   --                 specify the casing of subwords. A subword of the defining
   --                 identifier to check is a part if the identifier separated
   --                 by an underscore or the beginning/end of identifier.
   --                 A wildcard of the form identifier* defines the casing of
   --                 the first subword if the subword is case-insensitively
   --                 equal to 'identifier', *identifier defines the casing of
   --                 the last subword, and *identifier* - of any subword. The
   --                 parts of the identifier that cannot be mapped onto any
   --                 wildcard should satisfy the casing scheme defined for the
   --                 corresponding kind of entity.
   --
   --  If some identifier is included in the exception dictionary as a whole
   --  identifier and if it can be mapped onto some wildcard from the
   --  dictionary then the whole identifier but not wildcard is used to check
   --  the identifier casing.
   --
   --  If more than one dictionary file is specified, or a dictionary file
   --  contains more than one exception variant for the same identifier, the
   --  new casing exception overrides the previous one.
   --
   --  Casing check against dictionary file has higher priority then checks
   --  against the casing scheme specified for a given entity/declaration kind.
   --
   --  '+R' option should contain at least one parameter
   --  There is no parameter for '-R' option, it just turns the rule off. Note
   --  that -R does NOT clean up any parameter setting that have been done by
   --  previous +R option, so if you enable the rule, then disable it and
   --  enable again, the rule will have parameter setting defined by the first
   --  and second enabling. This does not look user-friendly, probably we
   --  should do clean-up when disabling the rule.

   type Casing_Schemes is
     (Not_A_Casing_Scheme,
      Lower,
      Mixed,
      Upper);

   type Wildcard_Rec is record
      Img      : String_Access;
      Orig_Img : String_Access;
   end record;

   function Eq  (Left, Right : Wildcard_Rec) return Boolean;
   function "<" (Left, Right : Wildcard_Rec) return Boolean;

   package Wildcard_Sets is new  Ada.Containers.Ordered_Sets
     (Element_Type => Wildcard_Rec,
      "="          => Eq);

   type Identifier_Casing_Rule_Type is new Rule_Template with record
      Type_Casing      : Casing_Schemes;
      Enum_Casing      : Casing_Schemes;
      Constant_Casing  : Casing_Schemes;
      Exception_Casing : Casing_Schemes;
      Others_Casing    : Casing_Schemes;
      Exclude          : String_Access_Sets.Set;

      Type_Casing_Def_At      : String_Loc;
      Enum_Casing_Def_At      : String_Loc;
      Constant_Casing_Def_At  : String_Loc;
      Exception_Casing_Def_At : String_Loc;
      Others_Casing_Def_At    : String_Loc;

      Type_Casing_Synonym      : Rule_Name_Str;
      Enum_Casing_Synonym      : Rule_Name_Str;
      Constant_Casing_Synonym  : Rule_Name_Str;
      Exception_Casing_Synonym : Rule_Name_Str;
      Others_Casing_Synonym    : Rule_Name_Str;
      Exclude_Synonym          : Rule_Name_Str;

      Wilcards         : Wildcard_Sets.Set;
      Dictionaries     : String_Access_Sets.Set;
   end record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Identifier_Casing_Rule_Type);
   --  Activates the rule with the following parameters:
   --    Type=upper
   --    Enum=mixed
   --    Constant=lower
   --    Exception=upper
   --    Others=mixed
   --
   --  and casing exceptions specified as:
   --
   --    UNIT ASIS bits1 *_IO

   overriding function Annotate_Rule
     (Rule : Identifier_Casing_Rule_Type;
      Var  : Diagnosis_Variant := 0)
      return String;
   --  Returns the rule name followed by the specified parameter that is the
   --  most relevant for the given diagnosis variant

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Identifier_Casing_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a defining identifier check its casing

   overriding procedure Init_Rule
     (Rule : in out Identifier_Casing_Rule_Type);

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Casing_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String);
   --  Process the Param string according to the parameter structure described
   --  above
   --
   --  Issues a warning (with the corresponding debug option ON) if the casing
   --  scheme is redefined for the rule that is already enabled. The warhing
   --  is issued only for changing a case scheme for the specific kind of
   --  entities, but not for cnages in the description of casing exceptions.

   overriding procedure XML_Rule_Help
     (Rule  : Identifier_Casing_Rule_Type;
      Level : Natural);

   overriding procedure Print_Rule
     (Rule         : Identifier_Casing_Rule_Type;
      Indent_Level : Natural := 0);

   overriding procedure Print_Rule_To_File
     (Rule         : Identifier_Casing_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actually specified prefixes.

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Casing_Rule_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actually specified prefixes.

   overriding function Allows_Parametrized_Exemption
     (Rule : Identifier_Casing_Rule_Type)
      return Boolean is (True);

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Identifier_Casing_Rule_Type;
      Parameter : String)
      return  Boolean;

   overriding function Rule_Parameter
     (Rule : Identifier_Casing_Rule_Type;
      Diag : String)
      return String;

   Identifier_Casing_Rule : aliased Identifier_Casing_Rule_Type;

   -------------------------
   -- Identifier_Prefixes --
   -------------------------

   --  Flag each defining identifier that does not have a prefix corresponding
   --  to the kind of declaration it is defined by. The defining names in the
   --  following kinds of declarations are checked:
   --
   --     * type and subtype declarations (task, protected and access types are
   --       treated separately)
   --     * enumeration literal (not including character literals), and
   --       function renaming declarations if the renaming entity is an
   --       enumeration literal
   --     * exception declarations and exception renaming declarations
   --     * constant and named number declarations (including object renaming
   --       declarations if the renamed object is a constant)
   --
   --  Defining names declared by single task declarations or single protected
   --  declarations are not checked by this rule.
   --
   --  The defining name from the full type declaration corresponding to a
   --  private type declaration or a private extension declaration is never
   --   flagged. A defining name from an incomplete type declaration is never
   --  flagged.
   --
   --  The defining name from a body that is a completion of a program unit
   --  declaration or a proper body of a subunit is never flagged.
   --
   --  The defining name from a body stub that is a completion of a program
   --  unit declaration is never flagged.
   --
   --  The defining name from a subprogram renaming-as-body declaration is
   --  never flagged.
   --
   --  For a deferred constant, the defining name in the corresponding full
   --  constant declaration is never flagged.
   --
   --  Note that the rule checks only defining names. Usage name occurrence are
   --  not checked and are never flagged.
   --
   --  The rule may have the following parameters:
   --
   --  o For the `+R' option:
   --
   --    Type=string
   --          Specifies the prefix for a type or subtype name.
   --
   --    Concurrent=string
   --        Specifies the prefix for a task and protected type/subtype name.
   --        If this parameter is set, it overrides for task and protected
   --        types the prefix set by the Type parameter.
   --
   --    Access=string
   --        Specifies the prefix for an access type/subtype name. If this
   --        parameter is set, it overrides for access types the prefix set by
   --        the Type parameter.
   --
   --    Class_Acces=string
   --        Specifies the prefix for the name of an access type/subtype that
   --        points to some class-wide type. If this parameter is set, it
   --        overrides for such access types and subtypes the prefix set by the
   --        Type or Access parameter.
   --
   --    Subprogram_Access=string
   --        Specifies the prefix for the name of an access type/subtype that
   --        points to a subprogram. If this parameter is set, it overrides for
   --        such access types/subtypes the prefix set by the Type or Access
   --        parameter.
   --
   --    Derived=string1:string2
   --        Specifies the prefix for a type that is directly derived from a
   --        given type or from a subtype thereof. string1 should be a full
   --        expanded Ada name of the ancestor type (starting from the full
   --        expanded compilation unit name), string2 defines the prefix to
   --        check.
   --
   --    Constant=string
   --        Specifies the prefix for a defining name from a constant or number
   --        declaration, including the object renaming declaration if the
   --        renamed object is a constant
   --
   --    Enum=string
   --        Specifies the prefix for defining enumeration literals and of the
   --        defining name in a function renaming declaration if the renamed
   --        entity is an enumeration literal.
   --
   --    Exception=string
   --        Specifies the prefix for a defining name from an exception
   --        declaration and exception renaming declaration.
   --
   --    Exclusive
   --         Check that only those kinds of names for which specific prefix is
   --         defined have the specified prefix (e.g., only type/subtype names
   --         have prefix T_, but not variable or package names), and flag all
   --         the defining names that have some of the specified prefixes but
   --         does not belong to the kind of the entities this prefix is
   --         defined for. By default the exclusive check mode is ON.
   --
   --  o For the `-R' option:
   --
   --    All_Prefixes
   --        Removes all the prefixes specified for the identifier prefix
   --        checks, whether by default or as specified by other rule
   --        parameters and disables the rule.
   --
   --    Type
   --    Concurrent
   --    Access
   --    Class_Access
   --    Subprogram_Access
   --    Derived
   --        Removes the specified prefix. This does not disable the rule, and
   --        in case if a prefix for a specific type kind is removed, but there
   --        is a prefix specified for more general type kind, this prefix will
   --        be used for a specific type kind (E.g. if
   --        '-RSubprogram_Access_Prefix is given but the prefix for access
   --        type names is specified, the prefix for all the access subtypes
   --        will be used for access-to-subprogram type names)
   --
   --    Constant
   --        Removes the prefix specified for constant names and turns off the
   --        check for constant names.
   --
   --    Exception
   --        Removes the prefix specified for exception names and turns off the
   --        check for exception names.
   --
   --    Enum
   --        Removes the prefix specified for enumeration literal names and
   --        turns off the check for enumeration literals.
   --
   --    Exclusive
   --        Turns off strong check mode.
   --
   --  If more than one parameter is used, parameters must be separated by
   --  commas.
   --
   --  If more than one option is specified for the gnatcheck invocation, a new
   --  option overrides the previous one(s).
   --
   --  The `+R Identifier_Prefixes' option (with no parameter) enables checks
   --  for all the name prefixes specified by previous options used for this
   --  rule. If no prefix is specified, the rule is not enabled.
   --
   --  The `-RIdentifier_Prefixes' option (with no parameter) disables all the
   --  checks but keeps all the prefixes specified by previous options used for
   --  this rule.
   --
   --  There is no default prefix setting for this rule. All the checks for
   --  name prefixes are case-sensitive
   --
   --  If any error is detected in a rule parameter, the parameter is ignored.
   --  In such a case the options that are set for the rule are not specified.

   type Derived_Pref_Record is record
      Parent_Name : String_Access;
      Prefix      : String_Access;
   end record;

   function "=" (Left, Right : Derived_Pref_Record) return Boolean;
   function "<" (Left, Right : Derived_Pref_Record) return Boolean;

   package Derived_Prefixes is new
     Ada.Containers.Ordered_Sets (Derived_Pref_Record);
   use Derived_Prefixes;

   type Identifier_Prefixes_Rule_Type is new Rule_Template with record
      Type_Prefix                      : String_Access;
      Type_Prefix_Synonym              : Rule_Name_Str;

      Concurrent_Prefix                : String_Access;
      Concurrent_Prefix_Synonym        : Rule_Name_Str;

      Access_Prefix                    : String_Access;
      Access_Prefix_Synonym            : Rule_Name_Str;

      Class_Access_Prefix              : String_Access;
      Class_Access_Prefix_Synonym      : Rule_Name_Str;

      Subprogram_Access_Prefix         : String_Access;
      Subprogram_Access_Prefix_Synonym : Rule_Name_Str;

      Derived_Prefix                   : Derived_Prefixes.Set;
      Derived_Prefix_Synonym           : Rule_Name_Str;

      Constant_Prefix                  : String_Access;
      Constant_Prefix_Synonym          : Rule_Name_Str;

      Exception_Prefix                 : String_Access;
      Exception_Prefix_Synonym         : Rule_Name_Str;

      Enum_Prefix                      : String_Access;
      Enum_Prefix_Synonym              : Rule_Name_Str;

      Exclusive                        : Boolean := True;
      Exclusive_Prefix_Synonym         : Rule_Name_Str;
   end record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Identifier_Prefixes_Rule_Type);
   --  Activates the rule with the following parameters:
   --     Type=T_
   --     Concurrent=J_
   --     Access=P_
   --     Class_Access=CP_
   --     Subprogram_Access=F_
   --     Derived=Ada.Finalization.Controlled:CTRL_
   --     Enum=E_
   --     Constant=C_
   --     Exception= X_
   --     Exclusive

   overriding function Annotate_Rule
     (Rule : Identifier_Prefixes_Rule_Type;
      Var  : Diagnosis_Variant := 0)
      return String;
   --  Returns the rule name followed by the specified parameter that is the
   --  most relevant for the given diagnosis variant

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Identifier_Prefixes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a defining identifier check its prefix

   overriding procedure Init_Rule
     (Rule : in out Identifier_Prefixes_Rule_Type);

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Prefixes_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String);
   --  Process the Param string according to the parameter structure described
   --  above
   --  ??? Defined_At ???

   overriding procedure XML_Rule_Help
     (Rule  : Identifier_Prefixes_Rule_Type;
      Level : Natural);

   overriding procedure Print_Rule
     (Rule         : Identifier_Prefixes_Rule_Type;
      Indent_Level : Natural := 0);

   overriding procedure Print_Rule_To_File
     (Rule         : Identifier_Prefixes_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actually specified prefixes.

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Prefixes_Rule_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actually specified prefixes.

   overriding function Allows_Parametrized_Exemption
     (Rule : Identifier_Prefixes_Rule_Type)
      return Boolean is (True);

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Identifier_Prefixes_Rule_Type;
      Parameter : String)
      return  Boolean;

   overriding function Rule_Parameter
     (Rule : Identifier_Prefixes_Rule_Type;
      Diag : String)
      return String;

   Identifier_Prefixes_Rule : aliased Identifier_Prefixes_Rule_Type;

   -------------------------
   -- Identifier_Suffixes --
   -------------------------

   --  A defined name for a specific kind of entity should have a specific
   --  suffix. The defining names in the following declarations are checked:
   --
   --    * type declarations
   --    * subtype declarations
   --    * object declarations:
   --      - variable and constant declarations, but not number declarations
   --      - record component declarations
   --      - formal parameter declarations
   --      - formal object declarations
   --    * package renaming declarations (but not generic package renaming
   --      declarations
   --    * (protected) subprograms used as unterrupt handlers
   --
   --  This rule may have parameters. When used without parameters, the
   --  following checks are made:
   --
   --    * type defining name ends with '_T' unless the type is an access type,
   --      in this case the suffix should be '_A'
   --
   --    * constant names end with '_C'
   --
   --    * names defining package renamings end with '_R'
   --
   --    * the check for access type objects is not enabled
   --
   --    * the check for interrupt hundlers is not enabled
   --
   --  Defined names from incomplete type declarations are not checked.
   --
   --  For a private type declaration (including private extensions) - the
   --  defining identifier from the private type declaration is checked against
   --  the type suffix (even if the corresponding full declaration is an access
   --  type declaration), and the defining identifier from the corresponding
   --  full type declaration is not checked;
   --
   --  For a deferred constant, the defining name in the corresponding full
   --  constant declaration is not checked.
   --
   --  Defining names of formal types are not checked.
   --
   --  Check for the suffix of access type data objects is applied to the
   --  following kinds of declarations:
   --     * variable and constant declarations
   --     * record component declarations
   --     * return object declaration
   --     * formal parameter declarations
   --     * formal object declarations
   --
   --  ???
   --  If both checks for constant suffixes and for access object suffixes are
   --  enabled, and if different suffixes are defined for them, then the rule
   --  has a check conflict for constants of access type. At the moment the
   --  check for access object suffixes suffixes is applied, and no error or
   --  warning message is issued to mark this situation. We may want to change
   --  this in future(???)
   --  ???
   --
   --  The rule may have the following parameters:
   --
   --  o For +R option:
   --
   --      Default                - sets the default listed above for all
   --                               the names to be checked
   --
   --      Type_Suffix=string     - specifies the suffix for a type name
   --
   --      Access_Suffix=string   - specifies the suffix for an access type
   --                               name, if this parameter is set, it
   --                               overrides for access types the suffix set
   --                               by 'Type_Suffix' parameter. For access
   --                               types, 'string' may have the following
   --                               format: 'suffix1(suffix2). That means that
   --                               an access type name should have the
   --                               'suffix1' suffix except for the case when
   --                               the designated type is also an access type,
   --                               in this case the type name should have the
   --                               'suffix1 & suffix2' suffix.
   --
   --      Class_Access_Suffix=string - specify the suffix for access type in
   --                               case if it is an access-to-object
   --                               definition and the subtype mark in its
   --                               definition is 'Class attribute. This
   --                               overrides setting for access-to-class type
   --                               names specified by Type_Suffix and
   --                               Access_Suffix parameters, if any.
   --
   --      Class_Subtype_Suffix=string - specify the suffix for subtype name in
   --                               case when the subtype mark is 'Class
   --                               attribute.
   --
   --      Constant_Suffix=string -  specifies the suffix for a constant name
   --
   --      Renaming_Suffix=string -  specifies the suffix for package renaming
   --                                name
   --
   --      Access_Obj_Suffix=string -  specifies the suffix for objects that
   --                               have an access type (including types
   --                               derived from access types)
   --
   --      Interrupt_Suffix       - specifies the suffix for protected
   --                               subprograms used as interrupt handlers
   --
   --  o For -R option:
   --
   --      All_Suffixes           - remove all the suffixes specified for the
   --                               identifier suffix checks, either by default
   --                               or specified by other rule parameters. All
   --                               the checks for this rule are disabled as
   --                               the result.
   --
   --      Type_Suffix            - removes the suffix specified for types,
   --                               this disables checks for types. It does not
   --                               disable any other checks for this rule
   --                               (including the check for access type names
   --                               if Access_Suffix is set).
   --
   --      Access_Suffix          - removes the suffix specified for access
   --                               types, this disables checks for access
   --                               type names. It does not disable any other
   --                               checks for this rule. If Type_Suffix is
   --                               set, access type names are checked as
   --                               ordinary type names.
   --
   --      Class_Subtype_Suffix   - removes the suffix specified for subtypes
   --                               defined for class-wide types, this
   --                               disables checks for subtype names. It does
   --                               not disable any other checks for this rule.
   --
   --      Class_Access_Suffix    - removes the suffix specified for access
   --                               types pointing to class-wide types.  For
   --                               access type names the check is defined by
   --                               other parameters. It does not disable any
   --                               other checks for this rule.
   --
   --      Constant_Suffix        - removes the suffix specified for constants
   --                               this disables checks for constant names.
   --                               It does not disable any other checks for
   --                               this rule.
   --
   --      Renaming_Suffix        - removes the suffix specified for package
   --                               renamings, this disables checks for
   --                               package renamings. It does not disable any
   --                               other checks for this rule.
   --
   --      Access_Obj_Suffix      - removes the suffix specified for objects of
   --                               access types, this disables checks for
   --                               such objects. It does not disable any
   --                               other checks for this rule.
   --      Interrupt_Suffix       - removes the suffix for protected
   --                               subprograms used as interrupt handlers. It
   --                               does not disable any other checks for this
   --                               rule.
   --
   --  If more than one parameter is used, parameters should be separated by
   --  comma.
   --
   --  If more than one  option is specified for the gnatcheck call, a new
   --  option overrides the previous one(s).
   --
   --  +RIdentifier_Suffixes option (with no parameter) enables all the
   --  check for all the name suffixes specified by previous options used for
   --  this rule.
   --  -RIdentifier_Suffixes option (with no parameter) disables all the
   --  checks but keeps all the suffixes specified by previous options used for
   --  this rule.
   --
   --  'string' should be a valid suffix for an Ada identifier (after cutting
   --  out all the leading and trailing space characters, if any), except for
   --  the case when 'string' specifies suffixes for access and
   --  access-to access types, in the latter case both components of this
   --  'string' should be valid Ada identifier suffixes. Parameters are not
   --  case sensitive, except the 'string' part.
   --
   --  If any error is detected in a rule parameter, a parameter is ignored.
   --  Note, that in this case nothing definite can be said about options that
   --  are set for the rule.

   type Identifier_Suffixes_Rule_Type is new Rule_Template with record
      Type_Suffix                     : String_Access;
      Type_Suffix_Synonym             : Rule_Name_Str;

      Access_Suffix                   : String_Access;
      Access_Suffix_Synonym           : Rule_Name_Str;

      Access_To_Access_Suffix         : String_Access;
      --  Share Access_Suffix_Synonym for user-defined rule synonym

      Class_Subtype_Suffix            : String_Access;
      Class_Subtype_Suffix_Synonym    : Rule_Name_Str;

      Class_Access_Suffix             : String_Access;
      Class_Access_Suffix_Synonym     : Rule_Name_Str;

      Constant_Suffix                 : String_Access;
      Constant_Suffix_Synonym         : Rule_Name_Str;

      Renaming_Suffix                 : String_Access;
      Renaming_Suffix_Synonym         : Rule_Name_Str;

      Access_Obj_Suffix               : String_Access;
      Access_Obj_Suffix_Synonym       : Rule_Name_Str;

      Interrupt_Suffix                : String_Access;
      Interrupt_Suffix_Synonym        : Rule_Name_Str;
   end record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Identifier_Suffixes_Rule_Type);
   --  Activates the rule with the following parameters:
   --     Type_Suffix=_T
   --     Access_Suffix=_Access(_Access)
   --     Class_Access_Suffix=_Class_Access
   --     Class_Subtype_Suffix=_Class
   --     Constant_Suffix=_C
   --     Renaming_Suffix=_R

   overriding function Annotate_Rule
     (Rule : Identifier_Suffixes_Rule_Type;
      Var  : Diagnosis_Variant := 0)
      return String;
   --  Returns the rule name followed by the specified parameter that is the
   --  most relevant for the given diagnosis variant

   procedure Rule_Check_Pre_Op
     (Rule    : in out Identifier_Suffixes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a defining identifier from a type, constant or package
   --  renaming declaration, check that it has a specified suffix

   procedure Init_Rule (Rule : in out Identifier_Suffixes_Rule_Type);

   procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Suffixes_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String);
   --  Process the Param string according to the parameter structure described
   --  above
   --  ??? Defined_At ???

   procedure Set_Rule_Defaults
     (Rule : in out Identifier_Suffixes_Rule_Type);
   --  Set the default check mode and name suffixes described above. This
   --  procedure does not belong to the set of primitive operations of the
   --  parent type for Defining_Names_Suffixes_Rule_Type.

   procedure XML_Rule_Help
     (Rule  : Identifier_Suffixes_Rule_Type;
      Level : Natural);

   overriding procedure Print_Rule
     (Rule         : Identifier_Suffixes_Rule_Type;
      Indent_Level : Natural := 0);

   overriding procedure Print_Rule_To_File
     (Rule         : Identifier_Suffixes_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actually specified suffixes.

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Suffixes_Rule_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actually specified suffixes.

   overriding function Allows_Parametrized_Exemption
     (Rule : Identifier_Suffixes_Rule_Type)
      return Boolean is (True);

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Identifier_Suffixes_Rule_Type;
      Parameter : String)
      return  Boolean;

   overriding function Rule_Parameter
     (Rule : Identifier_Suffixes_Rule_Type;
      Diag : String)
      return String;

   Identifier_Suffixes_Rule : aliased Identifier_Suffixes_Rule_Type;

   ---------------------------------
   -- Implicit_IN_Mode_Parameters --
   ---------------------------------

   --  Implicit IN mode in formal parameter specification is detected.
   --
   --  The rule does not have any parameter.
   --
   --  Should we extend this rule on generic formal objects?
   --  Should we flag implicit IN mode in function parameters?

   type Implicit_IN_Mode_Parameters_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Implicit_IN_Mode_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a parameter specification, checks that the mode is
   --  specified explicitly

   procedure Init_Rule (Rule : in out Implicit_IN_Mode_Parameters_Rule_Type);

   Implicit_IN_Mode_Parameters_Rule :
     aliased Implicit_IN_Mode_Parameters_Rule_Type;

   ------------------------------------------
   -- Implicit_SMALL_For_Fixed_Point_Types --
   ------------------------------------------

   --  Fixed point definitions that do not have the corresponding
   --  representation clauses that define the 'Small value for a type are
   --  detected. 'Small can be defined only for ordinary fixed points, so
   --  decimal fixed point definitions are not checked.
   --
   --  The rule does not have any parameter.

   type Implicit_SMALL_For_Fixed_Point_Types_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Implicit_SMALL_For_Fixed_Point_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is an ordinary fixed point definition, checks that there is a
   --  representation clause that defines 'Small for the fixed point type.

   procedure Init_Rule
     (Rule : in out Implicit_SMALL_For_Fixed_Point_Types_Rule_Type);

   Implicit_SMALL_For_Fixed_Point_Types_Rule :
     aliased Implicit_SMALL_For_Fixed_Point_Types_Rule_Type;

   ---------------------------------------
   -- Improperly_Located_Instantiations --
   ---------------------------------------

   --  Detects generic instantiations that are done in library package
   --  specifications (including library generic packages) and in subprogram
   --  bodies.
   --
   --  Instantiations in task and entry bodies are not flagged. Instantiations
   --  in the bodies of protected subprograms are flagged.
   --
   --  The rule does not have any parameter.
   --
   --  Rationale:
   --
   --  - If an instantiation is placed in the spec of a library (generic)
   --    package, then modification of the generic body may cause a lot of
   --    recompilations;
   --
   --  - If an instantiation is placed in a subprogram body, the instantiation
   --    is expanded each time the subprogram is called, this may result in
   --    Storage_Error;
   --
   --  - Instantiations in task and entry bodies seems to need a special rule.
   --
   --  Problems:
   --
   --     Should this rule be split into two separate rules (for subprograms
   --     and for generic packages respectively)?

   type Improperly_Located_Instantiations_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Improperly_Located_Instantiations_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is an instantiation, checks if it corresponds to the
   --  requirements stated above.

   procedure Init_Rule
     (Rule : in out Improperly_Located_Instantiations_Rule_Type);

   Improperly_Located_Instantiations_Rule :
     aliased Improperly_Located_Instantiations_Rule_Type;

   ----------------------
   -- Numeric_Indexing --
   ----------------------

   --  Flag numeric literals, including those preceded by an predefined unary
   --  minus, if they are used as index expressions in array components.
   --  Literals that are subcomponents of index expressions are not flagged
   --  (other than the aforementioned case of unary minus)..
   --
   --  This rule has no parameters.

   type Numeric_Indexing_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Numeric_Indexing_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is an integer literal checks if it is used as an index
   --  expression.

   procedure Init_Rule (Rule : in out Numeric_Indexing_Rule_Type);

   Numeric_Indexing_Rule : aliased Numeric_Indexing_Rule_Type;

   ---------------------------------
   -- Non_Short_Circuit_Operators --
   ---------------------------------

   --  Detect calls to predefined AND and OR operators for any boolean type.
   --  Calls to user-defined AND and OR operators and to operators defined by
   --  renaming declarations are not flagged. Calls to predefined AND and OR
   --  operators for modular types or boolean array types are not flagged.
   --
   --  This rule has no parameters.

   type Non_Short_Circuit_Operators_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Non_Short_Circuit_Operators_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is of An_And_Operator or An_Or_Operator Operator_Kind, checks
   --  if it represents a call to a predefined operator of a boolean type.

   procedure Init_Rule (Rule : in out Non_Short_Circuit_Operators_Rule_Type);

   Non_Short_Circuit_Operators_Rule :
     aliased Non_Short_Circuit_Operators_Rule_Type;

   ----------------------------
   -- Non_Visible_Exceptions --
   ----------------------------

   --  Detect the possibility of raising of an exception then can never be
   --  handled properly. Two cases are detected.
   --
   --  - An exception declaration in a subprogram body, task body or in a block
   --    statement is flagged if the body or statement does not contain a
   --    handler for this particular exception or a handler with OTHERS choice;
   --
   --  - a raise statement in an exception handler of a subprogram body, a
   --    task body or a block statement is flagged if it (re)raises a locally
   --    declared exception (that is, if in contains a name of a locally
   --    declared exception or if it does not contain an exception name, but
   --    the enclosing handler contains a locally declared exception in its
   --    exception choices.
   --
   --  Renamings of local exceptions are not flagged
   --
   --  This rule has no parameters.
   --
   --  Rationale: This rule detect most of the cases when a program unit or
   --             a block statement can raise an exception that is not visible
   --             outside this construct and, therefore, can never be properly
   --             handled.

   type Non_Visible_Exceptions_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Non_Visible_Exceptions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element a defining exception name, checks if it is an
   --  exception declared in a local frame that can not be properly handled
   --  outside. If Element is of A_Raise_Statement kind, checks if it can
   --  propagate the local exception.

   procedure Init_Rule (Rule : in out Non_Visible_Exceptions_Rule_Type);

   function Has_Tip (Rule : Non_Visible_Exceptions_Rule_Type) return Boolean;

   procedure XML_Rule_Help_Tip
     (Rule  : Non_Visible_Exceptions_Rule_Type;
      Level : Natural);

   Non_Visible_Exceptions_Rule : aliased Non_Visible_Exceptions_Rule_Type;

   ----------------------
   -- Numeric_Literals --
   ----------------------

   --  Any use of a numeric literal is detected except the three following
   --  cases:
   --
   --  * a literal is a subcomponet of the initialization expression in a
   --    constant declaration or a number declaration;
   --
   --  * a literal is a part of a aspect clause or an aspect definition;
   --
   --  * a literal is an integer literal that is less than or equal to a value
   --    specified by the rule parameter (if no parameter is set, this value
   --    is 1;
   --
   --  The rule may have the following parameters:
   --
   --  * For +R option:
   --
   --      N   - N is an integer literal - sets the maximal value not to detect
   --           integer literals if they are less than or equal to it
   --
   --      ALL - no exception is made when checking integer literals
   --
   --      Statements_Only - the check for numeric literals is performed only
   --           in statements (except for the case of index expressions that
   --           are checked in any context);
   --
   --  The latest specified check limit (or the fact that there is no limit at
   --  all) is used in case when more than one +R option are used. -R option
   --  for this rule does not have any parameter, it turns the rule off and
   --  resets the default rule mode (all the literals except integer literals
   --  0 and 1 are checked in any context)

   type Numeric_Literals_Rule_Type is new Rule_Template with record
     Up_To           : Integer := 1;
     Statements_Only : Boolean := False;
   end record;

   overriding function Annotate_Rule
     (Rule : Numeric_Literals_Rule_Type;
      Var  : Diagnosis_Variant := 0)
      return String;
   --  Returns the rule annotation with the parameters specified for the rule.

   procedure Rule_Check_Pre_Op
     (Rule    : in out Numeric_Literals_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a numeric literal checks if it is to be detected according
   --  to the rules specified above.

   procedure Init_Rule (Rule : in out Numeric_Literals_Rule_Type);

   procedure Process_Rule_Parameter
     (Rule       : in out Numeric_Literals_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String);
   --  Process the Param string according to the parameter structure described
   --  above
   --  ??? Defined_At ???

   procedure XML_Rule_Help
     (Rule  : Numeric_Literals_Rule_Type;
      Level : Natural);

   Numeric_Literals_Rule : aliased Numeric_Literals_Rule_Type;

   --------------------------
   -- OTHERS_In_Aggregates --
   --------------------------

   --  Detect use of OTHERS choice in extension record and array aggregates.
   --  For record and array aggregates, use of OTHERS is not detected if its
   --  used to refer to all or to all but one components of the aggregate.

   --  If, in case of a named array aggregate, there are two associations, one
   --  with OTHERS choice, and another with a discrete range, the OTHERS choice
   --  is flagged even if this discrete range specifies exactly one component
   --  (for example, (1 .. 1 => 0, others => 1) )
   --
   --  This rule has no parameters.
   --
   --  The original enhancement request says nothing about extension
   --  aggregates - to be discussed???

   type OTHERS_In_Aggregates_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out OTHERS_In_Aggregates_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is an OTHERS choice from an aggregate component association,
   --  checks if this association represents all or all but one components.

   procedure Init_Rule (Rule : in out OTHERS_In_Aggregates_Rule_Type);

   OTHERS_In_Aggregates_Rule : aliased OTHERS_In_Aggregates_Rule_Type;

   -------------------------------
   -- OTHERS_In_CASE_Statements --
   -------------------------------

   --  Detect use of OTHERS choice in a case statement.
   --
   --  This rule has no parameters.
   --
   --  The original enhancement request says nothing about any exception to
   --  this rule. But in some cases OTHERS may make sense, for example when
   --  the range of the case control expression subtype is too big (for
   --  example, Integer or Wide_Character). Should we have a parameter that
   --  somehow controls the exceptions to the rule ???

   type OTHERS_In_CASE_Statements_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out OTHERS_In_CASE_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks if Element is an OTHERS choice from a case statement path.

   procedure Init_Rule (Rule : in out OTHERS_In_CASE_Statements_Rule_Type);

   OTHERS_In_CASE_Statements_Rule :
     aliased OTHERS_In_CASE_Statements_Rule_Type;

   ----------------------------------
   -- OTHERS_In_Exception_Handlers --
   ----------------------------------

   --  Detect use of OTHERS choice in exception handler.
   --
   --  This rule has no parameters.
   --
   --  Problems:
   --
   --     Should we combine all the rules about the OTHERS choice into a single
   --     parametrized rule?

   type OTHERS_In_Exception_Handlers_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out OTHERS_In_Exception_Handlers_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks if Element is an OTHERS choice from an exception handler

   procedure Init_Rule (Rule : in out OTHERS_In_Exception_Handlers_Rule_Type);

   OTHERS_In_Exception_Handlers_Rule :
     aliased OTHERS_In_Exception_Handlers_Rule_Type;

   --------------------------------------
   -- Overly_Nested_Control_Structures --
   --------------------------------------

   --  A control structure having the nested level more than N is detected, By
   --  default N is equal to four, it can be specified by the rule parameter.
   --  A control structure is any of the following operators:
   --     IF statement
   --     CASE statement
   --     LOOP statement
   --     selective accept statement
   --     timed entry call statement
   --     conditional entry call
   --     asynchronous select statement
   --
   --  The rule has the following parameters:
   --
   --  * for +R option:
   --
   --     N - positive integer specifying the maximal control structure nesting
   --         level that is not flagged
   --
   --  The parameter of +R option is mandatory, if it is not specified or if it
   --  is not a positive integer, the '+R" option is ignored.
   --
   --  If more than one  option is specified for the gnatcheck call, a new
   --  option and a new parameter override the previous one(s).
   --
   --  Note that this rule is something in common with the GNAT style
   --  -gnatyLnnn options, but the details of checking construct nesting level
   --  are different

   type Overly_Nested_Control_Structures_Rule_Type is new
     One_Integer_Parameter_Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Overly_Nested_Control_Structures_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a control structure from the list given above, checks if
   --  it nesting level is not greater than specified for the rule.

   procedure Init_Rule
     (Rule : in out Overly_Nested_Control_Structures_Rule_Type);

   Overly_Nested_Control_Structures_Rule :
     aliased Overly_Nested_Control_Structures_Rule_Type;

   -----------------------------
   -- Parameters_Out_Of_Order --
   -----------------------------

   --  Detects subprogram and entry declarations where the formal parameters
   --  are not properly ordered. The proper order is the following:
   --
   --  * IN parameters first, then IN OUT parameters, and then OUT parameters;
   --
   --  * for IN parameters, parameters with default initialization expressions
   --    do last
   --
   --  Only the first violation of the described order is flagged.
   --
   --  The following constructs are checked:
   --     subprogram declarations (including null procedures)
   --     generic subprogram declarations
   --     formal subprogram declarations
   --     entry declarations
   --     subprogram bodies and subprogram body stubs if they work as specs
   --
   --  Subprogram renamings are not checked.
   --
   --  The rule does not have any parameter.
   --
   --  This rule is directly derived from the gnatcheck enhancement request
   --  (F323-030). The following things may require additional discussion and
   --  possible changes in gnatcheck:
   --
   --  - This particular order IN - IN OUT - OUT does not have any obvious
   --    advantages before other ways of mode ordering. May be, it would make
   --    sense to have a parameter that sets a particular order to check.
   --
   --  - We may want to check parameter mode ordering and ordering of the
   --    parameters with default expressions separately, and this may require
   --    splitting this rule into two rules.

   type Parameters_Out_Of_Order_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Parameters_Out_Of_Order_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  ??? To be provided after clarifying the corresponding enhancement
   --  request.

   procedure Init_Rule (Rule : in out Parameters_Out_Of_Order_Rule_Type);

   Parameters_Out_Of_Order_Rule : aliased Parameters_Out_Of_Order_Rule_Type;

   ---------------------------------------------------------
   -- Positional_Actuals_For_Defaulted_Generic_Parameters --
   ---------------------------------------------------------

   --  Detect generic actual parameters that pass a value to a formal object or
   --  to a formal subprogram that has a default initialization if this
   --  parameter is in positional association form
   --
   --  The rule does not have any parameter.

   type Positional_Actuals_For_Defaulted_Generic_Parameters_Rule_Type is
     new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out
        Positional_Actuals_For_Defaulted_Generic_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is positional generic parameter association, checks if the
   --  corresponding generic formal parameter has a default initialization

   procedure Init_Rule (Rule : in out
     Positional_Actuals_For_Defaulted_Generic_Parameters_Rule_Type);

   Positional_Actuals_For_Defaulted_Generic_Parameters_Rule :
     aliased Positional_Actuals_For_Defaulted_Generic_Parameters_Rule_Type;

   -------------------------------------------------
   -- Positional_Actuals_For_Defaulted_Parameters --
   -------------------------------------------------

   --  Detect actual subprogram or entry call parameters that pass a value to
   --  a formal parameter that has a default expression if this parameter is
   --  in positional association form.
   --
   --  The rule does not have any parameter.

   type Positional_Actuals_For_Defaulted_Parameters_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Positional_Actuals_For_Defaulted_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is positional parameter association, checks if the
   --  corresponding formal parameter has a default initialization

   procedure Init_Rule
     (Rule : in out Positional_Actuals_For_Defaulted_Parameters_Rule_Type);

   Positional_Actuals_For_Defaulted_Parameters_Rule :
     aliased Positional_Actuals_For_Defaulted_Parameters_Rule_Type;

   ---------------------------
   -- Positional_Components --
   ---------------------------

   --  An (array, record or extension) aggregate that has a positional
   --  association should be detected.
   --
   --  The rule does not have any parameter.

   type Positional_Components_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Positional_Components_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is an aggregate, checks if its first association is a
   --  positional one. The rule is checked for an aggregate, but not for a
   --  component association to get the proper source location and to avoid
   --  multiple diagnosis for the same aggregate in case if it contains more
   --  then one positional association

   procedure Init_Rule
     (Rule : in out Positional_Components_Rule_Type);

   Positional_Components_Rule :
     aliased Positional_Components_Rule_Type;

   -----------------------------------
   -- Positional_Generic_Parameters --
   -----------------------------------

   --  A positional generic parameter associations should be detected.
   --
   --  The rule does not have any parameter.

   type Positional_Generic_Parameters_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Positional_Generic_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is of A_Generic_Association kind, checks if the named
   --  notation is used for it.

   procedure Init_Rule
     (Rule : in out Positional_Generic_Parameters_Rule_Type);

   Positional_Generic_Parameters_Rule :
     aliased Positional_Generic_Parameters_Rule_Type;

   ---------------------------
   -- Positional_Parameters --
   ---------------------------

   --  A subprogram or entry call positional parameter associations should be
   --  detected, except the following situations:
   --
   --  * parameters of a call to an operator function are not flagged;
   --
   --  * parameters of a call to attribute subprograms are not flagged;
   --
   --  * if the called subprogram or entry has only one formal parameter, the
   --    actual parameter in the call is not flagged;
   --
   --  * if a subprogram call uses a prefixed view of the called subprogram,
   --    then the first parameter of the call (that makes up a prefix in the
   --    prefixed view) is not flagged. If the called subprogram has two
   --    parameters, its second parameter also is not flagged.
   --
   --  This rule has the following (optional) parameters for the `+R' option:
   --
   --   All - if this parameter is specified, all the positional parameter
   --         associations that can be replaced with named associations
   --         according to language rules are flagged.

   type Positional_Parameters_Rule_Type is new
     Rule_With_Exceptions_Template (1) with null record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Positional_Parameters_Rule_Type);
   --  Activates the rule with All parameter.

   overriding function Exception_Name
     (Rule      : Positional_Parameters_Rule_Type;
      Exc_Index : Exception_Index)
      return      String;

   overriding function Exception_Number
     (Rule     : Positional_Parameters_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Positional_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is of A_Parameter_Association kind, checks if the named
   --  notation is used for it.

   overriding procedure Init_Rule
     (Rule : in out Positional_Parameters_Rule_Type);

   Positional_Parameters_Rule :
     aliased Positional_Parameters_Rule_Type;

   -------------------------------
   --  Predefined_Numeric_Types --
   -------------------------------

   --  Any explicit use of any name of a predefined numeric type or subtype
   --  defined in package Standard is detected.
   --
   --  The rule does not have any parameter.
   --
   --  The idea of this rule is to detect the situations when the behavior of
   --  the program may depend on platform-specific characteristics of the
   --  implementation of the predefined numeric types. Note, that this rule
   --  is over-pessimistic for this purpose: for example, a program that
   --  would like to do string indexing most probably needs a variable of
   --  an Integer type or of a subtype of integer type. Another example is
   --  the situation when predefined numeric types are used with explicit
   --  constraints:
   --
   --     subtype My_Integer is Integer range Left .. Right;
   --     My_Var : My_Integer;
   --
   --  Note also, that this rule detects only numeric types and subtypes
   --  defined in Standard. The use of numeric types and subtypes defined in
   --  other predefined packages (such as System.Any_Priority or
   --  Ada.Text_IO.Count) is not detected

   type Predefined_Numeric_Types_Rule_Type is new Rule_Template with
     null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Predefined_Numeric_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is of An_Identifier kind, checks if it is a reference to a
   --  predefined numeric (sub)type defined in Standard

   procedure Init_Rule (Rule : in out Predefined_Numeric_Types_Rule_Type);

   Predefined_Numeric_Types_Rule : aliased Predefined_Numeric_Types_Rule_Type;

   ---------------------------------
   -- Raising_External_Exceptions --
   ---------------------------------

   --  For any program unit declared in a library package or in a generic
   --  library package, checks if an exception that is explicitly raised by the
   --  program unit, is either a predefined exception or an exception that is
   --  also declared (or renamed) in the visible part of this package.
   --
   --  This rule has no parameters.
   --
   --  Rationale: this rule ensures that if some unit "withes" a package or
   --             instantiates a generic package, it already has access to all
   --             the exceptions that can be raised by anything that contains
   --             in the package without "withing" any other package.
   --
   --  Problems:
   --
   --  - the current implementation also checks raise statements from the
   --    statement sequence of the package body (and from the statement
   --    sequences of the bodies of the enclosed packages), if any.

   type Raising_External_Exceptions_Rule_Type is new Rule_Template with
     null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Raising_External_Exceptions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a raise statement, and if the enclosing unit is a package,
   --  a generic package or a body thereof, checks if the raised exception is
   --  either a predefined exception or that it is defined in the visible
   --  part of the package

   procedure Init_Rule (Rule : in out Raising_External_Exceptions_Rule_Type);

   Raising_External_Exceptions_Rule :
     aliased Raising_External_Exceptions_Rule_Type;

   -----------------------------------
   -- Raising_Predefined_Exceptions --
   -----------------------------------

   --  Raising of the predefined exceptions should be detected The predefined
   --  exceptions are: Constraint_Error, Numeric_Error, Program_Error,
   --  Storage_Error, Tasking_Error.
   --
   --  The rule does not have any parameter.

   type Raising_Predefined_Exceptions_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Raising_Predefined_Exceptions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a raise statement, checks if the raised exception is
   --  any of the predefined exceptions.

   procedure Init_Rule (Rule : in out Raising_Predefined_Exceptions_Rule_Type);

   Raising_Predefined_Exceptions_Rule :
     aliased Raising_Predefined_Exceptions_Rule_Type;

   -------------------------------
   -- Unassigned_OUT_Parameters --
   -------------------------------

   --  Detects procedure bodies that do not assign values to OUT parameters.
   --  A procedure body is flagged if the code of this body (excluding body
   --  exception handlers, if any) does not contain statements that set value
   --  for any OUT parameter. An exception handler in a procedure body is
   --  flagged if it does not contain statements that set value for any OUT
   --  parameter or a RAISE statement. Bodies of generic procedures are also
   --  considered.
   --
   --  Only two ways of setting a value to an OUT parameter are considered:
   --
   --  * an assignment statement (the parameter is in the left part);
   --
   --  * passing as an actual for OUT or IN OUT (???) parameter
   --
   --  If any component of an OUT parameter gets a value, we consider that a
   --  whole parameter gets a value.
   --
   --  The rule does not have any parameter.

   type Unassigned_OUT_Parameters_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unassigned_OUT_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a procedure body or an exception handler from a procedure
   --  body, implements the check described above.

   procedure Init_Rule (Rule : in out Unassigned_OUT_Parameters_Rule_Type);

   Unassigned_OUT_Parameters_Rule :
     aliased Unassigned_OUT_Parameters_Rule_Type;

   -----------------------------------------
   -- Uncommented_BEGIN_In_Package_Bodies --
   -----------------------------------------

   --  Checks if the BEGIN keyword in package body is marked by the trailing
   --  comment containing the package name. BEGIN is not flagged if the package
   --  body does not contain any declaration.
   --
   --  The case when the BEGIN keyword is placed not on a separate line, but
   --  on the same line with the last declaration or the first statement is
   --  flagged undependently on the  fact if the line contains a trailing
   --  comment, the diagnostic message is attached to the line containing the
   --  first statement
   --
   --  The rule does not have any parameter.

   type Uncommented_BEGIN_In_Package_Bodies_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Uncommented_BEGIN_In_Package_Bodies_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a package body, checks if it contains the BEGIN keyword,
   --  and if it does, checks if it is marked by a trailing comment with the
   --  package name.

   procedure Init_Rule
     (Rule : in out Uncommented_BEGIN_In_Package_Bodies_Rule_Type);

   Uncommented_BEGIN_In_Package_Bodies_Rule :
     aliased Uncommented_BEGIN_In_Package_Bodies_Rule_Type;

   ------------------------------
   -- Unnamed_Blocks_And_Loops --
   ------------------------------

   --  Non-named block statements are detected. Non-named nested loop
   --  statements are detected (both enclosing and enclosed non-named loop
   --  statements are flagged).
   --
   --  The rule does not have any parameter.

   type Unnamed_Blocks_And_Loops_Rule_Type is new Rule_Template
     with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unnamed_Blocks_And_Loops_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If Element is a block statement, checks if in contains a statement
   --  identifier If Element is a loop statement, checks if it is enclosed by
   --  or itself encloses another loop statement, and if it is, checks if it
   --  contains a statement identifier.

   procedure Init_Rule (Rule : in out Unnamed_Blocks_And_Loops_Rule_Type);

   Unnamed_Blocks_And_Loops_Rule :
     aliased Unnamed_Blocks_And_Loops_Rule_Type;

end Gnatcheck.Rules.Custom_1;
