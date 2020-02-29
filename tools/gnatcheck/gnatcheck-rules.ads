------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                      G N A T C H E C K . R U L E S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2004-2019, AdaCore                     --
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

pragma Ada_2012;

--  This is the top of gnatcheck hierarchy that defines individual rules, rule
--  table and rule checking process. It contains some basic type declarations

with Ada.Containers.Indefinite_Ordered_Sets;

with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Asis;                    use Asis;
with Asis.Extensions;         use Asis.Extensions;
with Asis.Extensions.Strings; use Asis.Extensions.Strings;
with Asis.Text;               use Asis.Text;

with ASIS_UL.Source_Table;    use ASIS_UL.Source_Table;

with ASIS_UL.Global_State;

with Gnatcheck.Ids;           use Gnatcheck.Ids;
with Gnatcheck.Options;       use Gnatcheck.Options;

package Gnatcheck.Rules is

   package Exemption_Parameters is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => String);
   --  Needed to keep/process strings that can be used as rule parameters for
   --  rule exemptions.

   type Rule_Checking_States is (Initial_State, All_Rules_Off);
   --  ???
   --  Used as a state type for traversing???

   subtype Diagnosis_Variant is Natural;
   --  Used to numerate the variants of diagnostic message for the same rule.
   --  Zero means that the rule has exactly one variant of the diagnostic
   --  message

   type Rule_Traversal_State is record
      Rule_Checking_State : Rule_Checking_States;
      --  This field is not used at the moment. In some perspective we may
      --  use it for some fine tuning of rule checking process.

      Detected : Boolean;
      --  Set OFF before calling any rule-specific Pre- or Post-operation. On
      --  return from this call, indicates whether or not the violation of
      --  the corresponding rule is detected

      SF : SF_Id;
      --  The Id of the source currently being processed.

      Diagnosis : Diagnosis_Variant;
      --  For some rules we may have more than one variant of the diagnostic
      --  message. In this case this field should be set to the number of the
      --  variant corresponding to the detected violation.

      Diag_Params : String_Loc;
      --  For some rules the diagnostic message may require parametrization.
      --  If so, this field contains the reference to the string with actual
      --  parameter(s)

      Level_Of_Nonexecutable_Construct : Natural := 0;
      --  Nonexecutable construct is any construct that does not result in any
      --  actions during the program execution. For example, a generic template
      --  and a type definition are nonexecutable constructs, and if they
      --  contain a subprogram call, this call can never be issued from their
      --  code, it can be issued only from the instantiation of this generic or
      --  from the declaration of an object of this type if the default
      --  expression defined in the type definition is really evaluated.

      Line   : Natural := 0;
      Column : Natural := 0;
      --  If these fields are not equal to 0, then they should be used as the
      --  source location for the corresponding diagnostic message. Otherwise
      --  the diagnostic message gets the source location from the Element is
      --  is being generated for.
   end record;

   procedure Increase_Nonexec_Level (State : in out Rule_Traversal_State);
   procedure Decrease_Nonexec_Level (State : in out Rule_Traversal_State);
   --  Non-executable constructs may be enclosed one in another. So during the
   --  traversal process we have to count the nesting level of such constructs
   --  to have the possibility to detect if we are in executable or in
   --  non-executable context. Increase_Nonexec_Level should be called by the
   --  traversal pre-operation when we enter a non-executable construct,
   --  and Decrease_Nonexec_Level should be called in the Post-operation for
   --  this construct

   function In_Executable_Code
     (State : Rule_Traversal_State)
      return  Boolean;
   --  Tells the caller if we are in the executable context at the given stage
   --  of traversal
   --  Do we really need this ?????

   subtype Rule_Name_Str  is String_Access;
   subtype Rule_Help      is String_Access;
   subtype Rule_Diagnosis is String_Access;
   --  Subtypes for storing the string information. What would be better -
   --  string access or using ASIS_UL.Strings utilities???

   -----------------
   -- Rule States --
   -----------------

   --  Each rule may have one of the following states:

   --   Enabled -  the rule should be checked at the moment, this state may
   --              be changed during the gnatcheck run (as a result of passing
   --              a control comment in the analyzed source)

   --   Disabled - the rule is disabled for the given gnatcheck run, this
   --              state cannot be changed during the gnatcheck run

   --   Temporary_Disabled - the rule is disabled at the moment, but it may be
   --              moved into Enabled state lately (as a result of passing
   --              a control comment in the analyzed source)

   --  ??? Something like Disabled_For_This_Source ???
   --  ??? Something like "Disable for all children of given Element", set it
   --      and store the Element in Pre_Op, and turn off in Post op?
   --  ??? Something like "Disable till the end of this source"

   type Rule_States is
     (Enabled,
      Disabled,
      Temporary_Disabled);
   --  To be used when temporary rule enabling/disabling is implemented.

   type Remediation_Levels is
     (Trivial,
      Easy,
      Medium,
      Major,
      High,
      Complex);
   --  How hard it could be to revove the rule violation.

   type Rule_Template is tagged record
      Name : Rule_Name_Str;
      --  The only means of rule identification outside gnatcheck. All the
      --  rules implemented in gnatcheck should have unique names, the casing
      --  is not important.

      Synonym : Rule_Name_Str;
      --  Synonym of the rule name. If we have to change the rule name, this
      --  synonym can be used for rule identification

      User_Synonym : Rule_Name_Str;
      --  User-specified synonym for the rule name. It is used for
      --  documentation purposes only (to map gnatcheck rules onto rules from
      --  user's coding standard), it is not used for rule identification.

      Defined_At : String_Loc;
      --  Location in the rule file where the rule has been enabled. Set to
      --  Nil_String_Loc if the rule has been enabled by command line option.

      Rule_Category    : Category_Id;
      Next_In_Category : Rule_Id;
      --  ????

      Rule_State : Rule_States;
      --  Is the rule active or not

      Rule_Status : Rule_Statuses;

      Remediation_Level : Remediation_Levels;

      Help_Info : Rule_Help;
      --  Short help information for the rule

      Diagnosis : Rule_Diagnosis;
      --  Message generated in case if a rule violation is detected. A rule may
      --  have more than one diagnostic message associated with it. A
      --  diagnostic message may have formal parameters that should be replaced
      --  with some actual information computed from a specific rule violation.
      --  See the documentation of Gnatcheck.Rules.Output.Report_Detection for
      --  more details about diagnosis parametrization and providing more than
      --  one diagnostic message for a rule.

      Check_In_Expanded_Generics : Boolean;
      --  If this flag is ON the rule is checked for expanded generic code.

   end record;

   type Rule_Access is access all Rule_Template'Class;

   --------------------------------------------------------
   -- Operations that may be redefined by specific rules --
   --------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Rule_Template;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);

   procedure Rule_Check_Post_Op
     (Rule    : in out Rule_Template;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  These two procedures are called for each ASIS Element as a part of the
   --  general ASIS traversal. They perform all the possible local checks.
   --
   --  The template Rule_Check_Pre_Op and Rule_Check_Post_Op do nothing.
   --
   --  Note that the State parameter is used as the means to pass information
   --  (including the number or the diagnostic variant and the reference to
   --  diagnosis actual parameters, see the spec of Gnatcheck.Rules.Output)
   --  to the Gnatcheck.Rules.Output.Report_Detection routine that stores the
   --  diagnoses in the diagnosis database.
   --
   --  See the body of Gnatcheck.Rules.Traversing routines All_Rules_Pre_Op and
   --  All_Rules_Post_Op for more details.

   function Allows_Parametrized_Exemption
     (Ignored_Rule : Rule_Template)
      return Boolean is (False);
   --  Says if you can specify a rule parameter when defining an exemption
   --  section for Rule. In case if a rule parameter has the form like
   --  'Param_Name => Param_Value', in the exemption section you can speify
   --  only "Param_Name'

   function Allowed_As_Exemption_Parameter
     (Ignored_Rule  : Rule_Template;
      Ignored_Param : String)
      return Boolean is (False);
   --  Checks if Param can be used as a rule parameter when defining an
   --  exemption section for Rule.

   function Rule_Parameter (Rule : Rule_Template; Diag : String) return String;
   --  Assuming that Allows_Prametrized_Exemption (Rule) and that Diag is a
   --  diagnosis generated for Rule (with all the variants and parameters
   --  resolved), defines the Rule parameter this Diag corresponds to. Returns
   --  an empty string if the definition is not possible because of any reason.

   procedure Init_Rule (Rule : in out Rule_Template);
   --  This is the means to provide the basic rule characteristics, such
   --  as rule name, texts of diagnostic and help messages, rule status etc.
   --  The version of this procedure defined for the top Rule_Template sets
   --  Rule_State to Disabled, Rule_Status - to Under_Construction,
   --  Rule_Category - to No_Category and Next_In_Category to No_Rule. These
   --  actions are common to all the rule initialization procedures.

   procedure Process_Rule_Parameter
     (Rule       : in out Rule_Template;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At :        String);
   --  Is supposed to process a single rule parameter of the gnatcheck call.
   --
   --  If the rule parameter string consists of more than one parameter
   --  separated by ',', then this procedure is called for each Param
   --  substring of the rule parameter that corresponds to the parts separated
   --  by commas. As the substring of the rule parameter, the Param string
   --  satisfies the following conditions:
   --
   --  * it starts from the first non-blank character after ',' or from the
   --    beginning of the rule parameter;
   --
   --  * it ends with the character immediately preceding the next ',';
   --
   --  * it contains at least one non-blank character;
   --
   --  * it does not contain a comma character;
   --
   --  The order of calling these routine for substrings of the rule parameter
   --  string corresponds to the order of this substrings in the rule parameter
   --  string. If the rule parameter string does not contain a comma character,
   --  this procedure is called only once for the whole rule parameter string
   --  without leading blank characters (if any).
   --
   --  See the body of Gnatcheck.Rules.Rule_Table.Process_Rule_Option for
   --  more details of parsing the rule parameter string.
   --
   --  Enable says if the given rule for the given parameter value
   --  should be set ON or OFF (the exact meaning of parameters and
   --  combinations of parameters values with Enable parameter should be
   --  defined individually for each rule). If the Param value (with the
   --  combination of Enable value) is not allowed for a given rule, this
   --  procedure should generate an error message (saying that the given
   --  parameter is ignored for a given rule) and does not do anything
   --  else.
   --
   --  Defined_At is used to store the place where the rule for the given
   --  parameter has been defined for the last time. It may be used to from a
   --  warning for redefinition of the rule parameter (this is not needed not
   --  for all rules). The actual should have the standard GNAT format for
   --  SLOC file:line:columm if the rule with this parameter is defined in a
   --  rule file, or whould be empty if the rule option is given in a command
   --  line. (The following situation may be treated as a rule parameter
   --  redefinition: the rule is enabled with the value 'Val1' for parameter
   --  'Par1' and then it is enabled again with some (different or the same)
   --  value for this parameter. This looks suspicious and may result in
   --  warning. The decisions wether or not a warning should be issued in this
   --  sutuation are made on rule-by-rule basis. The case when the rule is
   --  enabled, then disabled and then enabled again does not result in
   --  a warning.)
   --
   --  The template does the following:
   --  - if Param string is empty, it sets the rule ON or OFF depending on the
   --    value of the Enable parameter;
   --
   --  - otherwise generates the error message saying that no parameter can be
   --    set for a given rule and that the parameter is ignored.

   function Checked_On_Expanded_Code (Rule : Rule_Template) return Boolean;
   --  Tells if an argument rule should be checked on generic expanded code.

   procedure Print_Rule_Help (Rule : Rule_Template);
   --  Prints into Stderr the rule help information

   function Has_Tip (Rule : Rule_Template) return Boolean;
   --  Tells if the rule XML help info should contain a <tip> tag. The template
   --  returns False. If this function is redefined for some rule, it should
   --  return True.

   procedure XML_Rule_Help (Rule : Rule_Template; Level : Natural);
   --  Prints the rule help information (including the information about rule
   --  parameters) in XML format, The template prints out the following string:
   --
   --  <check switch="+RRule_Name" label="brief rule help"/>

   procedure XML_Rule_Help_Tip (Rule : Rule_Template; Level : Natural);
   --  Prints the <tip> tag for the rule XML help info. The template does
   --  nothing.

   function Rule_Name (Rule : Rule_Template) return String;
   --  Returns the rule name

   function Is_Enable (Rule : Rule_Template) return Boolean;
   --  Checks if the rule should be checked

   procedure Print_Rule (Rule : Rule_Template; Indent_Level : Natural := 0);
   --  Prints information about the (active) rule into the report file. For
   --  the rules that have parameters this procedure should be rewritten to
   --  print out actual parameter settings.
   --  This procedure does not close the last line it prints out.

   procedure Print_Rule_To_File
     (Rule         : Rule_Template;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);
   --  Similar to Print_Rule, but prints rule information into the specified
   --  file. Rule_File is supposed to be an existing file opened as Out_File.
   --  This is not a good thing to have two procedures that do almost the same,
   --  the only reason why we do have them is historical.

   procedure XML_Print_Rule
     (Rule         : Rule_Template;
      Indent_Level : Natural := 0);
   --  Similar to Print_Rule, but prints rule info into XML report file and
   --  does not add "+R" prefix to the rile name.

   function Rule_Option
     (Rule          : Rule_Template;
      Template_Kind : Template_Coding_Standard_Kinds)
      return          String;
   --  Returns the rule option (with the parameter, if needed) to be placed
   --  into the sample coding standard file (see Sample_Image). The result
   --  include "+R" or "-R" depending on the Template_Kind. The template
   --  returns the rule name, it should be rewritten for the rules that have
   --  parameters.

   function Rule_Comment (Rule : Rule_Template) return String;
   --  Returns the comment note to be placed into the sample coding standard
   --  file (see Sample_Image), the result does not include "--". The template
   --  returns the content of the Help_Info field of the argument.

   function More_Rule_Comment
     (Rule          : Rule_Template;
      Template_Kind : Template_Coding_Standard_Kinds)
     return           String;
   --  Returns the additional comment to be placed into the sample coding
   --  standard file (see Sample_Image) to add some warning about the need to
   --  revise the rule in the actual coding standard. The template returns null
   --  string, this means that no additional comment is needed. If non-null,
   --  the result should not include '--'

   procedure Activate_In_Test_Mode (Rule : in out Rule_Template);
   --  Enables the rule for test mode (running gnatcheck in Q4A test driver).
   --  The default implementation is just to enable the rule. Needs to be
   --  overridden for rules with parameters.

   function Annotate_Rule
     (Rule : Rule_Template;
      Var  : Diagnosis_Variant := 0)
      return String;
   --  If Gnatcheck.Options.Mapping_Mode is ON returns the rule annotation
   --  to be inserted into diagnosis, otherwise returns an empty string. The
   --  default annotation is "(<Rule_Name>) ".

   function Has_Synonym (Rule : Rule_Template) return Boolean;
   --  Checks if the rule has a user-defined synonym.

   function Rule_Synonym (Rule : Rule_Template) return String;
   --  If Has_Synonym (Rule) then returns the rule synonym, otherwise returns
   --  an empty string.

   ---------------------------
   -- Basic rules hierarchy --
   ---------------------------

   --  From the implementation point of view we have the following hierarchy
   --  of the rules:
   --
   --  Rule----
   --       |
   --       --- Internal rule
   --       |
   --       --- "One positive parameter" rule
   --       |
   --       --- Rule_With_Exceptions_Template (generic)
   --       |
   --       --- Global rule
   --       |
   --       --- Text rule
   --
   --  Internal rule is not visible for gnatcheck user and it is used to
   --  implement some publicly available rule
   --
   --  "One positive parameter" rule is a rule that checks that some property
   --  of some Ada construct is not greater than or less than a given value,
   --  property should be a countable property counted in positives (number of
   --  something), all these rules require a positive parameter for +R option.
   --
   --  global rule is a rule that requires collecting and analyzing some global
   --  information about all the units being checked by gnatcheck

   --------------------
   -- Internal rules --
   --------------------

   type Internal_Rule_Template is abstract new Rule_Template with record
      Implements : Rule_Access;
   end record;
   --  Internal rule is not supposed to be shown to the  customer, it is used
   --  as a means to implement some other rule, this rule should be pointed by
   --  the field Implements.
   --
   --  Note also, that if some "public" rule is implemented by some set of
   --  internal rules, the diagnoses are defined by the internal rules, and the
   --  "public" rule itself may not have any diagnosis associated with it, If
   --  the "public" rule does not have a diagnosis on its own, it is not
   --  listed in the lists of rules in the generated output.

   function Rule_Name (Rule : Internal_Rule_Template) return String;
   --  Returns the "full name" of the rule in the format:
   --
   --   Name_of_Implemented_Rule (Name_Of_Argumemt_Rule)
   --
   --  For example Metric(Cyclomatic_Complexity)

   procedure Print_Rule_Help (Rule : Internal_Rule_Template);
   --  When printing rule help information into Stderr, prints the rule name
   --  in the format returned by Internal_Rule_Name

   function Is_Enable (Rule : Internal_Rule_Template) return Boolean;
   --  Checks if the rule should be checked. The internal rule is enable if it
   --  itself is enable and the rule it is implemented is also enable

   -----------------------------------
   -- "One integer parameter" rule" --
   -----------------------------------

   type One_Integer_Parameter_Rule_Template is abstract new Rule_Template with
      record
         Rule_Bound : Integer;
         Rule_Limit : Integer;
      end record;
   --  Rule_Limit specifies the maximal (minimal) allowed number of some
   --  property to be checked by the rule, this is supposed to be set by the
   --  rule parameter. Rule_Bound specifies the minimal value that is allowed
   --  for Rule_Limit. All the rules in this hierarchy assume that the rule
   --  parameter should be equal or greater than Rule_Bount

   overriding procedure Activate_In_Test_Mode
     (Rule : in out One_Integer_Parameter_Rule_Template);
   --  Activates the rule with the parameter equals to 4. If this value of the
   --  rule parameter is not reasonable for a particular decsendent of
   --  One_Integer_Parameter_Rule_Template the procedure should be overriden.

   overriding procedure Init_Rule
     (Rule : in out One_Integer_Parameter_Rule_Template);
   --  Calls Init_Rule for Rule_Template and sets Rule_Bound to 1, that
   --  corresponds to a common case with the rule with a positive parameter

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Integer_Parameter_Rule_Template;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String);
   --  For '+R" option (Enable is ON) checks that the Param string represents
   --  an integer that is not less than Rule_Bound, and if it does, stores it
   --  as the value of the Rule_Limit field and turns the rule ON, otherwise
   --  generates the diagnostic message and turns the rule OFF.
   --  For '-R' option checks that Param string is empty and in case it is
   --  turns the rule OFF.
   --  Defined_At parameter is used to form warning in case of redefinition of
   --  the rule parameter.

   overriding procedure XML_Rule_Help
     (Rule  : One_Integer_Parameter_Rule_Template;
      Level : Natural);
   --  Prints out a spin tag describing the rule and its parameter

   overriding procedure Print_Rule
     (Rule         : One_Integer_Parameter_Rule_Template;
      Indent_Level : Natural := 0);

   overriding procedure Print_Rule_To_File
     (Rule         : One_Integer_Parameter_Rule_Template;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actual parameter value

   overriding procedure XML_Print_Rule
     (Rule         : One_Integer_Parameter_Rule_Template;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actual parameter value

   function Default_Parameter
     (Rule : One_Integer_Parameter_Rule_Template)
      return Integer;
   --  Return (most probably - completely meaningless!) default value of the
   --  rule parameter to be used in Rule_Option function

   overriding function Rule_Option
     (Rule          : One_Integer_Parameter_Rule_Template;
      Template_Kind : Template_Coding_Standard_Kinds)
      return String;
   --  Prints out rule option in the format
   --  "+/-R rule_name : default_par_value"

   overriding function More_Rule_Comment
     (Rule          : One_Integer_Parameter_Rule_Template;
      Template_Kind : Template_Coding_Standard_Kinds)
      return String;
   --  Adds the note that the rule parameter may be junk so it needs
   --  revising.

   ---------------------------
   --  Rule with exceptions --
   ---------------------------

   Max_Exceptions   : constant Integer := 128;
   Not_An_Exception : constant Integer := 0;
   First_Exception  : constant Integer := Not_An_Exception + 1;

   subtype Exception_Numbers is Integer range
      Not_An_Exception .. Max_Exceptions;
   subtype Exception_Index is Exception_Numbers range
     First_Exception .. Max_Exceptions;

   type Exception_Array is array (Exception_Index range <>) of Boolean;

   type Rule_With_Exceptions_Template (Exception_Num : Exception_Index) is
     abstract new Rule_Template with record
      Exceptions : Exception_Array (First_Exception .. Exception_Num);
   end record;
   --  The object of this type contains a list of exceptions. The use of
   --  these exceptions is up to the rule. In most cases specifying an
   --  exception means that the rule should not flag some specific cases, but
   --  you can use this in an opposite way: by default a rule may skip checks
   --  in some specific contexts, and by specifying an exception you can tell
   --  the rule NOT to skip checks here or there.

   function Exception_Name
     (Rule      : Rule_With_Exceptions_Template;
      Exc_Index : Exception_Index)
      return      String;
   function Exception_Number
     (Rule     : Rule_With_Exceptions_Template;
      Exc_Name : String)
      return     Exception_Numbers;
   --  The best abstraction for a set of rule's exception cases would be an
   --  enumeration type. But language rules do not allow to use this in the
   --  type inheritance hierarchy, and using generic rule type parametrized by
   --  an enumeration type results in elaboration problems. So we have to
   --  emulate the enumeration of rule exceptions by an integer subtype with
   --  two functions: one converts the exception position number into the
   --  rule exception name, and another one does the reverse conversion. This
   --  pair of functions shall be overridden for each type derived from
   --  Rule_With_Exceptions_Template.
   --
   --  For Exception_Name function, if Exc_Index is greater than
   --  Rule.Exception_Num, empty string is returned.
   --
   --  For Exception_Number function, if Exc_Name is not a name of some
   --  exception defined for the given rule, Not_An_Exception is returned

   overriding function Annotate_Rule
     (Rule : Rule_With_Exceptions_Template;
      Var  : Diagnosis_Variant := 0)
      return String;
   --  Returns the annotation of the form "(rule_name[:(exception{,exception}]

   overriding procedure Init_Rule
     (Rule : in out Rule_With_Exceptions_Template);
   --  Sets all the exceptions OFF

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Rule_With_Exceptions_Template;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String);
   --  For "+R" option checks if a rule parameter is one of the images of
   --  the exceptions defined for this rule, and if it is, turns the rule ON
   --  with the corresponding exception ON, otherwise generates the error
   --  message and turns the rule OFF.
   --  For '-R' option checks that Param string is empty and in case it is
   --  not generates the corresponding error message. In any case turns the
   --  rule OFF, and all the exceptions are also set OFF.
   --  This procedure issues a warning (with the corresponding debug option
   --  enabled) if parameter processing enables the rule that is already in
   --  an enabled state, and the set of exceptions for the rule is changed.

   overriding procedure XML_Rule_Help
     (Rule  : Rule_With_Exceptions_Template;
      Level : Natural);
   --  Prints out a ??? tag describing the rule and its parameter

   overriding procedure Print_Rule
     (Rule         : Rule_With_Exceptions_Template;
      Indent_Level : Natural := 0);

   overriding procedure Print_Rule_To_File
     (Rule         : Rule_With_Exceptions_Template;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actual parameter value(s)

   overriding procedure XML_Print_Rule
     (Rule         : Rule_With_Exceptions_Template;
      Indent_Level : Natural := 0);
   --  Prints the rule with the actual parameter value(s)

   ------------------
   -- Global rules --
   ------------------

   type Global_Rule_Template is abstract new Rule_Template with null record;
   --  The top of the global rules hierarchy.

   procedure Collect_Global_Info_Pre_Op
     (Rule    : in out Global_Rule_Template;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);

   procedure Collect_Global_Info_Post_Op
     (Rule    : in out Global_Rule_Template;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  These two procedures are called for each ASIS Element as a part of the
   --  general ASIS traversal (for both argument and needed sources). They
   --  compute and store in the global state data structure all the
   --  rule-specific global information. They do not perform any checks.
   --
   --  The template Collect_Global_Info_Pre_Op and Collect_Global_Info_Post_Op
   --  do nothing.

   procedure Init_Global_Structure (Rule : Global_Rule_Template);
   --  This procedure performs all the needed initializations and tunings for
   --  the global structure needed for checking the rule.
   --  The template sets the unconditional mode for the call graph creation ON.
   --  The versions of this procedure for specific global rules may specify
   --  conditions for creating the call graph instead.

   procedure Check_Global_Structure_Node
     (Rule          :     Global_Rule_Template;
      N             :     ASIS_UL.Global_State.GS_Node_Id;
      Detected      : out Boolean);
   --  For a given rule, checks the given node of the global structure in order
   --  to make the check for this rule. Detected is set ON if the rule
   --  violation is detected for this node and OFF otherwise.
   --  The template does nothing.

   procedure Analyze_Global_Structure (Rule : Global_Rule_Template);
   --  For a given rule, analyzes, changes and checks the integrated state of
   --  the global structure in order to make the check for this rule. This
   --  happens after making the transitive closure of all the calls.
   --  The template does nothing.
   --  ???

   procedure Init_Rule (Rule : in out Global_Rule_Template);
   --  The template sets ON Gnatcheck.Global_State flag.

   ----------------
   -- Text rules --
   ----------------

   type Text_Rule_Template is abstract new Rule_Template with null record;
   --  The top of the text rules hierarchy. Text rules are rules formulated and
   --  checked for the source code of compilation units processed by gnatcheck.
   --  The common thing for the rules from this hierarchy is that they cannot
   --  be somehow bind to specific syntax elements. The typical example is to
   --  flag comments of a specific kind.

   procedure Line_Check
     (Rule               : in out Text_Rule_Template;
      Line_Num           :        Line_Number_Positive;
      Full_Line_Image    :        Program_Text_Access;
      Ada_Line_Image     :        Program_Text_Access;
      Comment_Line_Image :        Program_Text_Access;
      State              : in out Rule_Traversal_State);
   --  This is a template for a procedure that is supposed to analyze an
   --  image of a single source line. Line_Num is supposed to be the number of
   --  the argument line in the source text, Full_Line_Image, Ada_Line_Image
   --  and Comment_Line_Image - (references to) the text images of the line -
   --  full image, image contained only Ada code (if any) and image containing
   --  only comments (see Asis.Text queries Line_Image, Non_Comment_Image and
   --  Comment_Image. State parameter has the same meaning as for
   --  Rule_Check_Pre_Op procedure.

   ----------------------------
   -- Common rule operations --
   ----------------------------

   procedure Register_Rules;
   --  Register all the rules that are currently implemented.

   procedure Sample_Image
     (Rule          : Rule_Template'Class;
      Template_Kind : Template_Coding_Standard_Kinds;
      Sample_File   : File_Type;
      Comment_From  : Positive);
   --  Prints into Sample_File the rule option that turns this rule ON or OFF,
   --  depending on the value of Template_Kind parameter. Is
   --  supposed to be used to create a template coding standard file.
   --  Comment_From indicates the position the comment that gives a short
   --  definition of the rule should start from

end Gnatcheck.Rules;
