------------------------------------------------------------------------------
--                                                                          --
--                        ASIS-for-GNAT COMPONENTS                          --
--                                                                          --
--                      A S I S . E X T E N S I O N S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2019, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your option)  any later --
-- version.  ASIS-for-GNAT  is  distributed  in  the  hope  that it will be --
-- useful,  but  WITHOUT ANY WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have  received  a copy of the  GNU General Public License and --
-- a copy of the  GCC Runtime Library Exception  distributed with GNAT; see --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

--  This package contains some ASIS extensions which are needed by the ASIS
--  implementation for GNAT itself, or which are considered to be useful for
--  ASIS applications.
--
--  Most of these extensions may be implemented as secondary ASIS queries,
--  but we often use some optimization based on direct traversal of the GNAT
--  tree.

--  In this package we follow the GNAT, but not ASIS coding and documentation
--  style, but for some queries we use the ASIS-style lists of Appropriate,
--  Expected and Returned kinds.

with Ada.Containers.Hashed_Sets;
with Ada.Unchecked_Deallocation;

with Asis.Elements; use Asis.Elements;
with Asis.Text;     use Asis.Text;

with GNAT.OS_Lib;   use GNAT.OS_Lib;

package Asis.Extensions is

   ------------------------------------------------
   --  Temporary solution for transition to GNSA --
   ------------------------------------------------

   procedure Switch_To_GNSA
     (Path   : in out String_Access;
      Target :      String := "");
   --  Assumes that Path contains the full path to a tool from the compiler
   --  installation (it can actually be gcc or gprbuild, but we use a general
   --  aproach just in case). Resents it to the same tool from GNSA used as
   --  ASIS tree builder. If Target is not a null string, assumes that this is
   --  a target-specific prefix of the tool name, and strips the corresponding
   --  target from the result (in GNSA we cannot have any target-specific
   --  names).

   ------------------------
   -- Element containers --
   ------------------------

   function Elements_Hash_Wrapper
     (E    : Asis.Element)
      return Ada.Containers.Hash_Type;
   --  Wrapper for Asis.Elements.Hash to be used in the instantiation of
   --  Ada.Containers.Hashed_Sets

   package Element_Containers is new Ada.Containers.Hashed_Sets
     (Element_Type        => Asis.Element,
      Hash                => Elements_Hash_Wrapper,
      Equivalent_Elements => Asis.Elements.Is_Equal,
      "="                 => Asis.Elements.Is_Equal);

   -----------------------
   -- List Access Types --
   -----------------------

   type Element_List_Access is access Element_List;
   type Compilation_Unit_List_Access is access Compilation_Unit_List;

   procedure Free is new Ada.Unchecked_Deallocation
     (Element_List, Element_List_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Compilation_Unit_List, Compilation_Unit_List_Access);

   ----------------------------------
   -- Access to Program_Text Type  --
   ----------------------------------

   type Program_Text_Access is access Program_Text;
   procedure Free is new Ada.Unchecked_Deallocation
     (Program_Text, Program_Text_Access);

   ---------------------------
   -- Tree creation routine --
   ---------------------------

   procedure Compile
     (Source_File           :     String_Access;
      Args                  :     Argument_List;
      Success               : out Boolean;
      GCC                   :     String_Access := null;
      Use_GPRBUILD          :     Boolean       := False;
      Result_In_Current_Dir :     Boolean       := True;
      Compiler_Out          :     String        := "";
      All_Warnings_Off      :     Boolean       := True;
      Display_Call          :     Boolean       := False);
   --  Calls GNAT to create the tree file. The command to create the tree is
   --  defined by the GCC parameter, if the actual for it is null, the standard
   --  'gcc' command is used'.
   --
   --  If Use_GPRBUILD is ON (and GCC is not null) it treats the command to be
   --  called as gprbuild, otherwise it treats it as gcc (native or cross) and
   --  forms the list of option for the generated call accordingly. It forms
   --  argument string for the called program that has the following structure:
   --
   --  - if Use_GPRBUILD is OFF:
   --
   --    -c -gnatct -gnatws -gnatyN -x ada <Args> &  <Source_File>
   --
   --    (except in case of calling GNAAMP, for GNAAMP '-x ada' is not set);
   --
   --  - if Use_GPRBUILD is ON
   --
   --  -c -q -f <Args> <Source_File> -cargs -gnatct -gnatws -gnatyN -o obj_file
   --
   --  If All_Warnings_Off is OFF, '-gnatws -gnatyN' are not used.
   --
   --  Result_In_Current_Dir is taken into account only Use_GPRBUILD is ON. If
   --  This flag is OFF, '-o obj_file' is not used. obj_file is Source_File
   --  with the suffix replaced '.o' (or appended with '.o' if it does not
   --  have a suffix) and prepended to the full path to the current directory.
   --
   --  So there is no need to set these options as a part of the value of Args
   --  parameter (basically Args contains only needed -I, -gnatA, -gnatec
   --  options, and project file in case of the GPRBUILD call) GCC parameter
   --  should contain the full path to [cross]gcc (or gprbuild) to be used to
   --  create the tree (use GNAT.OS_Lib.Locate_Exec_On_Path in the client code
   --  to detect this path). If this parameter is not set, the standard
   --  gcc/GNAT installation is used (even if Use_GPRBUILD is set ON). Success
   --  is set ON if the required tree has been created without detecting any
   --  compilation errors. Otherwise it is set OFF.
   --
   --  If Compiler_Out is a non-empty string, this string is treated as the
   --  name of a text file to redirect the compiler output into (if the file
   --  does not exist, it is created). Otherwise the compiler output is
   --  sent to Stderr
   --
   --  If Display_Call is ON, outputs into Stderr the command used to call
   --  GNAT.

   ------------------------------------------------------
   -- Placeholders for Traverse_Element instantiations --
   ------------------------------------------------------

   --  If you do not need the state of traversing, and if you do not need
   --  actual for Post-Operation in this case (this is the common case for
   --  many situations when some simple traversing is required), the following
   --  declarations may be used:

   type No_State is (Not_Used);
   --  Placeholder for the State_Information formal type

   procedure No_Op
     (Element : Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State);
   --  Placeholder for the formal Post_Operation procedure

   --------------------
   -- Test functions --
   --------------------

   function Is_Assertion (Element : Asis.Element) return Boolean;
   --  Checks if the argument element is an assertion. The definition of
   --  assertion is based on the definition of the GNAT-specific pragma
   --  Assertion_Policy (see GNAT Reference Manual): any aspect definition or
   --  any pragma that can be specified by a parameter if this pragma is
   --  considered as assertion. Returns False for any unexpected argument.
   --
   --  Expected Elenebt_Kinds:
   --     A_Pragma
   --
   --  Expected Definition_Kinds:
   --     An_Aspect_Specification

   function Is_RCI_Unit (C : Asis.Compilation_Unit) return Boolean;
   --  Checks if the argument compilation is a remote call interface (RCI)
   --  unit (See E.2.3). Returns False for any unexpected element.
   --
   --  Expected Unit_Kinds:
   --     A_Package
   --     A_Procedure_Body
   --     A_Function_Body
   --     A_Generic_Package

   function Acts_As_Spec (Declaration : Asis.Element) return Boolean;
   --  Checks if its argument is a subprogram body declaration for which no
   --  separate subprogram declaration exists. Returns False for any
   --  unexpected argument
   --
   --  Expected Declaration_Kinds:
   --     A_Procedure_Body_Declaration
   --     A_Function_Body_Declaration
   --     An_Expression_Function_Declaration
   --     A_Procedure_Body_Stub
   --     A_Function_Body_Stub

   function Is_Aspect_Mark (Element : Asis.Element) return Boolean;
   --  Checks if Element is an aspect mark from aspect_specification.
   --  Returns False for any unexpected Element
   --
   --  Expected Expression_Kinds
   --     An_Identifier
   --
   --  Expected Attribute_Kinds
   --     A_Class_Attribute

   function Is_Aspect_Specific_Name (Element : Asis.Element) return Boolean;
   --  Checks if Element is an identifier or a character literal specific for
   --  some aspect definition. Returns False for any unexpected Element
   --
   --  Expected Expression_Kinds
   --     A_Character_Literal
   --     An_Identifier

   function Is_Call_Through_Access_To_Subprogram
     (Call : Asis.Element)
      return Boolean;
   --  Checks if the argument is a function or procedure call when a called
   --  subprogram is defined by access-to-subprogram value.
   --  Returns False for any unexpected Element
   --
   --  Expected Statement_Kinds:
   --     A_Procedure_Call_Statement
   --
   --  Expected Expression_Kinds:
   --     A_Function_Call

   function Is_Class_Wide
     (Declaration : Asis.Declaration)
      return        Boolean;
   --  Checks if the argument subtype is a subtype of some class-wide type.
   --  Returns False for any unexpected Element
   --
   --  Expected Declaration_Kinds:
   --     A_Subtype_Declaration

   function Is_Default_For_Null_Procedure
     (Reference : Asis.Element)
      return      Boolean;
   --  Checks if Reference is a reference for the default actual that is used
   --  in expanded generic for a null procedure. Returns False for any
   --  unexpected argument.
   --
   --  Expected Expression_Kinds:
   --     An_Identifier

   function Is_Definite_Subtype (Declaration : Asis.Element) return Boolean;
   --  Checks if an argument is a declaration of a definite subtype (that is,
   --  if for a given type/subtype declaration this function return true, then
   --  it is possible to define an object of this (sub)type without imposing
   --  any constraint. Returns False for any unexpected argument.
   --
   --  Expected Declaration_Kinds:
    --    An_Ordinary_Type_Declaration
    --    A_Task_Type_Declaration
    --    A_Protected_Type_Declaration
    --    A_Private_Type_Declaration
    --    A_Private_Extension_Declaration
    --    A_Subtype_Declaration
    --    A_Formal_Type_Declaration

   function Is_From_SPARK_Aspect (E : Asis.Element) return Boolean;
   --  Checks if the argument is from the aspect specification that is specific
   --  for SPARK 2014. The code in such aspect specifications is an Ada
   --  extension, it does not follow the language visibility rules, so it may
   --  contain names that have no definition.

   function Is_Renaming_As_Body (Declaration : Asis.Element) return Boolean;
   --  Checks if its argument is a renaming-as-body declaration.
   --
   --
   --  Expected Element_Kinds:
   --     A_Procedure_Renaning_Declaration
   --     A_Function_Renaming_Declaration

   function Is_Completed (Declaration : Asis.Element) return Boolean;
   --  Checks is its argument (which is expected to be a declaration requiring
   --  completion) has a completion in its enclosed ASIS Context.
   --
   --  Expected Element_Kinds (this list is not complete ???)
   --     A_Procedure_Declaration
   --     A_Function_Declaration

   function Is_Bool_Eq_Declaration
     (Declaration : Asis.Element)
      return        Boolean;
   --  Checks if Declaration is a declaration of the equality operation
   --  returning the predefined boolean type that implicitly declares "/="
   --  (See RM 95). Returns False for any unexpected element
   --
   --  Expected Declaration_Kinds
   --     A_Function_Declaration
   --     A_Function_Body_Declaration
   --     A_Function_Renaming_Declaration

   function Is_Implicit_Neq_Declaration
     (Declaration : Asis.Element)
      return        Boolean;
   --  Checks if Declaration is an implicit declaration of "/=" operator that
   --  is declared as a consequence of some Is_Bool_Eq_Declaration declaration
   --  (See RM 95). Returns False for any unexpected element
   --
   --  Expected Declaration_Kinds
   --     A_Function_Declaration

   function Is_Tagged_Type_Eq
     (Declaration : Asis.Element)
      return        Boolean;
   --  Checks if Declaration is a Is_Bool_Eq_Declaration that corresponds to
   --  a tagged type and is a dispatching operation for it or the declaration
   --  of the corresponding complementary implicit "/=" operation (for a
   --  tagged type). Returns False for any unexpected element.
   --
   --  Expected Declaration_Kinds
   --     A_Function_Declaration

   function Is_Overriding_Operation
     (Declaration : Asis.Element)
      return        Boolean;
   --  Checks if the argument is a subprogram declaration or subprogram
   --  instantiation that overrides a user-defined type primitive operation.
   --  Always returns False for Is_Part_Of_Inherited arguments. Returns False
   --  for any unexpected element. Note that this function checks operation
   --  overriding, but not only operation hiding
   --
   --  Expected Declaration_Kinds
   --     A_Procedure_Declaration
   --     A_Function_Declaration
   --     A_Procedure_Instantiation
   --     A_Function_Instantiation
   --     A_Procedure_Body_Declaration
   --     A_Function_Body_Declaration
   --     A_Null_Procedure_Declaration
   --     A_Procedure_Renaming_Declaration
   --     A_Function_Renaming_Declaration

   function Is_Predefined_Operator
     (Operator : Asis.Element)
      return     Boolean;
   --  Checks if the argument is a reference to a predefined operator.
   --  Returns False for any unexpected Element
   --
   --  Expected Expression_Kinds:
   --     An_Operator_Symbol

   function Is_Shift_Operation
     (Operator : Asis.Element)
      return     Boolean;
   --  Checks if the argument is a reference to one of the five shift
   --  operations that are the result of pragma Provide_Shift_Operators
   --  (Shift_Left, Shift_Right, Shift_Right_Arithmetic, Rotate_Left and
   --  Rotate_Right)
   --
   --  Expected Expression_Kinds:
   --     An_Identifier

   function Is_Label (Defining_Name : Asis.Defining_Name) return Boolean;
   --  Check if the argument is a statement label

   function Is_True_Expression
     (Expression : Asis.Expression)
      return       Boolean;
   --  Checks if Expression is an expression in Ada sense, that is if it
   --  is an expression as defined in RM 4.4, and the type of this expression
   --  can be represented in ASIS. For An_Expression Element for which
   --  Is_True_Expression is False, the Corresponding_Expression_Type query
   --  should yield Nil_Element. Note that this function returns False for an
   --  expression having anonymous access type.
   --
   --  Expected Element_Kinds:
   --     An_Expression

   function Has_Anonymous_Type
     (Expression : Asis.Expression)
      return       Boolean;
   --  Assuming that Is_True_Expression (Expression) checks if Expression has
   --  an anonymous type (that is, the type of the expression is not defined
   --  by some type declaration in the program code. If for some expression
   --  Has_Anonymous_Type returns True, the Corresponding_Expression_Type query
   --  should yield Nil_Element for it.

   function Is_Static (Element : Asis.Element) return Boolean;
   --  Checks if Element represent a static expression or a static range
   --  constraint. "Static" is considered in the GNAT sense, that is if the
   --  compiler computes it during the compilation time. We believe,
   --  that GNAT notions of a static expression and a static range are close
   --  to the corresponding definitions in RM 95, but we can not guarantee
   --  this. Returns False for any unexpected Element
   --
   --  Expected Element_Kinds:
   --     An_Expression for which Is_True_Expression yields True.
   --
   --  Expected Constraint_Kinds:
   --     A_Range_Attribute_Reference

   function Has_Enumeration_Type
     (Expression : Asis.Expression)
      return       Boolean;
   --  Checks if Expression has some enumeration type (including types derived
   --  from enumeration types). Returns False for any unexpected Element
   --
   --  Expected Element_Kinds:
   --     An_Expression for which Is_True_Expression yields True.

   function Has_Integer_Type (Expression : Asis.Expression) return Boolean;
   --  Checks if Expression has some integer type (including types derived
   --  from integer types). Returns False for any unexpected Element
   --
   --  Expected Element_Kinds:
   --     An_Expression for which Is_True_Expression yields True.

   function Is_Check_Name
     (Reference : Asis.Expression)
      return      Boolean;
   --  Check if Reference denotes a check (either predefined of introduced by
   --  a GNAT Check_Name pragma. The Reference is expected to be of
   --  An_Identifier kind. Returns False for any unexpected argument.

   function Is_Uniquely_Defined
     (Reference : Asis.Expression)
      return      Boolean;
   --  Check if Reference has a unique definition. The Reference is expected
   --  to be of An_Identifier, A_Character_Literal, An_Enumeration_Literal or
   --  An_Operator_Symbol kind, that is, of the same kind as the argument of
   --  Asis.Expressions.Corresponding_Name_Definition). This test may be used
   --  to prevent calls of Asis.Expressions.Corresponding_Name_Definition and
   --  Asis.Expressions.Corresponding_Name_Declaration which raise
   --  ASIS_Inappropriate_Element (see the documentation of these queries).
   --  Returns False for any unexpected argument.
   --
   --  Expected Element_Kinds:
   --     An_Identifier
   --     An_Operator_Symbol
   --     A_Character_Literal
   --     An_Enumeration_Literal

   function Is_Private (Declaration : Asis.Element) return Boolean;
   --  Checks if Declaration is located in the private part of a package,
   --  a generic package, a task or protected  type or object declaration.
   --  If Declaration is located in the visible part of such a construct, but
   --  this enclosing construct is itself located in some private part
   --  (immediately or being nested in some other constructs), this function
   --  also returns True. Returns False for any unexpected argument.
   --
   --  Because of the performance reasons the implementation of this
   --  function is based on the direct traversal of the GNAT tree, so it is not
   --  an ASIS secondary query.
   --
   --  Expected Element_Kinds:
   --     A_Declaration
   --
   --  Expected Declaration_Kinds
   --     All except A_Loop_Parameter_Specification
   --                A_Generalized_Iterator_Specification
   --                An_Element_Iterator_Specification

   function Is_Exported (Defining_Name : Asis.Defining_Name) return Boolean;
   --  Checks if pragma Export is applied to the argument entity. In case if
   --  this entity is from a subprogram body declaration, this check is made
   --  for the entity from the corresponding subprogram spec (if any)
   --
   --  Because of the performance reasons the implementation of this
   --  function is based on the direct traversal of the GNAT tree, so it is not
   --  an ASIS secondary query.
   --
   --  Expected Element_Kinds:
   --     A_Defining_Name

   function Body_In_Library_Level_Instantiation
     (Declaration          :     Asis.Element;
      Instantiation_Source : out String_Access)
      return Boolean;
   --  This function is used in the implementation of
   --  Asis.Declarations.Corresponding_Body for compile-on-the-fly Context
   --  mode.
   --
   --  Assuming that Declaration is either a subprogram declaration or a
   --  subprogram instantiation it checks if the result of Corresponding_Body
   --  should be a part of expanded instantiation that corresponds to the
   --  library-level instantiation. If it is, Instantiation_Source is set to
   --  the full path to the source that contains this library-level
   --  instantiation (appended by ASCII.NUL), otherwise it is set to null.
   --
   --  Expected Declaration_Kinds
   --     A_Function_Declaration
   --     A_Function_Instantiation
   --     A_Generic_Package_Declaration
   --     A_Generic_Procedure_Declaration
   --     A_Generic_Function_Declaration
   --     A_Package_Declaration
   --     A_Package_Instantiation
   --     A_Procedure_Declaration
   --     A_Procedure_Instantiation
   --     A_Single_Task_Declaration
   --     A_Task_Type_Declaration
   --     A_Protected_Type_Declaration
   --     A_Single_Protected_Declaration

   ---------------------
   -- Run-Time checks --
   ---------------------

   type Run_Time_Checks is
     (Do_Accessibility_Check,
      Do_Discriminant_Check,
      Do_Division_Check,
      Do_Length_Check,
      Do_Overflow_Check,
      Do_Range_Check,
      Do_Storage_Check,
      Do_Tag_Check);

   type Run_Time_Check_Set is array (Run_Time_Checks) of Boolean;

   Empty_Check_Set : constant Run_Time_Check_Set := (others => False);

   function Needed_Checks
     (Element : Asis.Element)
      return    Run_Time_Check_Set;
   --  Any Element is accepted. The returned array represents a set of run-time
   --  checks that are needed for the argument Element. If the check is needed,
   --  the corresponding component of the result is set ON, and OFF otherwise.

   -----------------------------------------------------
   -- Modified versions of the "primary" ASIS queries --
   -----------------------------------------------------

   function Get_Call_Parameters
     (Call       : Asis.Element;
      Normalized : Boolean := False)
      return       Asis.Element_List;
   --  Returns the parameter list from the call. Combines the functionality of
   --  Asis.Statements.Call_Statement_Parameters and
   --  Asis.Expressions.Function_Call_Parameters
   --
   --  Appropriate Expression_Kinds:
   --    A_Function_Call
   --
   --  Appropriate Statement_Kinds:
   --    An_Entry_Call_Statement
   --    A_Procedure_Call_Statement
   --
   --  Returns Element_Kinds:
   --    A_Parameter_Association

   --  The rest of this section contains the modified versions of the queries
   --  defined in the standard ASIS packages. The names of these modified
   --  versions may or may not be the same as in the "core" ASIS

   -----------------------
   -- Asis.Declarations --
   -----------------------

   function Corresponding_Representation_Items
      (Defining_Name : Asis.Defining_Name)
       return          Asis.Element_List;
   --  This function tries to rectify shortcomings of the standard ASIS queries
   --  that provides representation information (such as
   --  Asis.Declarations.Corresponding_Representation_Clauses and
   --  Asis.Elements.Corresponding_Pragmas). First, it operates on a defining
   --  name, not on a declaration, so the result is a list of representation
   --  items applied to a specific entity. Second, the result contains all
   --  kinds of representation items - representation specifications, aspect
   --  definitions and pragmas.
   --
   --  The query provides the compiler view on the set of representation items
   --  provided for the argument entity - it traverses the chain of
   --  representation items in the tree.
   --
   --  Only explicit representation items are included in the returned result.
   --
   --  Appropriate Element kinds:
   --     A_Defining_Name
   --
   --  Result Element Kinds:
   --     A_Pragma
   --
   --  Result Clause_Kinds:
   --     A_Representation_Clause
   --     A_Component_Clause
   --
   --  Result Definition_Kinds:
   --     An_Aspect_Specification

   function Formal_Subprogram_Default
     (Declaration : Asis.Generic_Formal_Parameter)
      return        Asis.Expression;
   --  This is a modified version of the query Formal_Subprogram_Default
   --  adjusted for use in the implementation of Asis.Elements.Traverse_Element
   --  generic procedure. Similarly to that ASIS query, it returns the name
   --  appearing after the reserved word IS in the given generic for
   --  A_Name_Default Element, but if its argument is of another kind from
   --  Default_Kinds, it returns Nil_Element instead of raising
   --  ASIS_Inappropriate_Element.
   --
   --  Appropriate Declaration_Kinds:
   --      A_Formal_Function_Declaration
   --      A_Formal_Procedure_Declaration
   --
   --  Returns Element_Kinds:
   --      An_Expression

   function Primitive_Owner
     (Declaration : Asis.Declaration)
      return        Asis.Type_Definition;
   --  In the case that Declaration the explicit declaration of a subprogram
   --  which Is_Dispatching_Operation for some tagged type, this function
   --  returns the type definition for which it is a primitive operation. (Note
   --  that a subprogram declaration may be a primitive operation for more than
   --  one type, but it may be a primitive operation for at most one tagged
   --  type. Note  also, that for implicitly declared dispatching operations
   --  the primary ASIS query Asis.Declarations.Corresponding_Type may be used
   --  to find the type which "owns" the operation). Returns Nil_Element in all
   --  other cases.
   --
   --  In case of a (non-tagged!) private type that has a tagged full view, a
   --  type operation declared in visible part is classified as
   --  Is_Dispatching_Operation, and this function will return private
   --  non-tagged type definition.
   --
   --  Appropriate Declaration_Kinds (should be the same as expected kinds
   --  for Asis.Declarations.Is_Dispatching_Operation):
   --     A_Procedure_Declaration
   --     A_Function_Declaration
   --     An_Expression_Function_Declaration
   --     A_Procedure_Renaming_Declaration
   --     A_Function_Renaming_Declaration
   --     A_Null_Procedure_Declaration
   --     A_Procedure_Body_Declaration
   --     A_Function_Body_Declaration
   --     A_Procedure_Body_Stub
   --     A_Function_Body_Stub
   --
   --  Returns Definition_Kinds:
   --     A_Private_Type_Definition
   --     A_Tagged_Private_Type_Definition
   --     A_Private_Extension_Definition
   --     A_Task_Definition
   --     A_Protected_Definition
   --
   --  Returns Type_Kinds:
   --     A_Derived_Record_Extension_Definition
   --     A_Tagged_Record_Type_Definition
   --
   --  Returns Element_Kinds
   --     Not_An_Element

   ----------------------
   -- Asis.Expressions --
   ----------------------

   function Corresponding_Called_Function_Unwound
     (Expression : Asis.Expression)
      return       Asis.Declaration;
   --  A modification of Asis.Expressions.Corresponding_Called_Function which
   --  unwinds all the renamings in the case where the function name in the
   --  argument function call is defined by a renaming declaration. This
   --  function returns the declaration of the called function *entity*.
   --
   --  Appropriate Expression_Kinds:
   --      A_Function_Call
   --
   --  Returns Declaration_Kinds:
   --      Not_A_Declaration
   --      A_Function_Declaration
   --      A_Function_Body_Declaration
   --      A_Function_Body_Stub
   --      A_Function_Renaming_Declaration
   --      A_Function_Instantiation
   --      A_Formal_Function_Declaration

   function Corresponding_Called_Function_Unwinded
     (Expression : Asis.Expression)
      return       Asis.Declaration renames
        Corresponding_Called_Function_Unwound;
   --  For upward compatibility we have to keep the old ungrammatical names of
   --  this function

   ---------------------
   -- Asis.Statements --
   ---------------------

   function Corresponding_Called_Entity_Unwound
     (Statement : Asis.Statement)
      return      Asis.Declaration;

   --  A modification of Asis.Statements.Corresponding_Called_Entity which
   --  unwinds all the renamings in the case where the procedure or entry name
   --  in the argument call is defined by a renaming declaration. This function
   --  returns the declaration of the callable *entity*.
   --
   --  Appropriate Statement_Kinds:
   --      An_Entry_Call_Statement
   --      A_Procedure_Call_Statement
   --
   --  Returns Declaration_Kinds:
   --      Not_A_Declaration
   --      A_Procedure_Declaration
   --      A_Procedure_Body_Declaration
   --      A_Procedure_Body_Stub
   --      A_Procedure_Renaming_Declaration
   --      A_Procedure_Instantiation
   --      A_Formal_Procedure_Declaration
   --      An_Entry_Declaration

   function Corresponding_Called_Entity_Unwinded
     (Statement : Asis.Statement)
      return      Asis.Declaration renames
        Corresponding_Called_Entity_Unwound;
   --  For upward compatibility we have to keep the old ungrammatical names of
   --  this function

   -------------------
   -- Asis.Elements --
   -------------------

   function Pragmas_After
     (Compilation_Unit : Asis.Compilation_Unit;
      Include_Pragmas  : Boolean := True)
      return             Asis.Pragma_Element_List;
   --  Returns the list of pragmas the appear immediately following the
   --  Compilation_Unit, for example:
   --     with G;
   --     procedure P is new G;
   --     pragma Pure (P);

   --------------------------------------
   -- Extensions of ASIS functionality --
   --------------------------------------

   ----------------------------
   -- Asis.Compilation_Units --
   ----------------------------

   function CU_Requires_Body (Right : Asis.Compilation_Unit) return Boolean;
   --  Similar to Asis.Compilation_Units.Is_Body_Required, but also checks
   --  library subprogram declarations and library generic subprogram
   --  declarations. For (generic) library subprogram declarations,
   --  returns True unless the subprogram is completed by pragma Import.

   function Is_Obsolete (Right : Asis.Compilation_Unit) return Boolean;
   --  Checks if the argument unit, Right, is obsolete. A unit is not
   --  obsolete, if the source for this unit is available and if it
   --  is the same as the source used for creating the trees. All
   --  unit kinds are expected, except nil, unknown and nonexistent
   --  units. Always returns True for any non-expected unit. In case
   --  of '-SA' Context, always returns False for any expected unit.

   type Source_File_Statuses is (
      --  Status of the source file corresponding to a given unit

      No_File_Status,
      --  Nil value, used for nil, non-existent, and unknown units

      Absent,
      --  No source file available. This is always the case for the
      --  predefined Standard package, nil, unknown and non-existent
      --  units.

      Older,
      --  The available source file is older then the source used
      --  to create tree files

      Newer,
      --  The available source file is newer then the source used
      --  to create tree files

      Up_To_Date);
      --  The available source file is the same as the source used
      --  to create tree files

   function Source_File_Status
     (Right : Asis.Compilation_Unit)
      return  Source_File_Statuses;
   --  Checks the status of the source file for the argument unit.

   function Is_Main_Unit_In_Tree
     (Right : Asis.Compilation_Unit)
      return  Boolean;
   --  Checks if the argument unit, Right, is a main unit from some compilation
   --  which has created a tree within the set of tree files making up the
   --  enclosing Context of this unit.

   function Main_Unit_In_Current_Tree
     (The_Context : Asis.Context)
      return        Asis.Compilation_Unit;
   --  If the tree currently accessed by ASIS is from the set of trees making
   --  up The_Context, then this function returns the corresponding main unit,
   --  that is, the Compilation_Unit corresponding to the source file which
   --  has been compiled to create this tree file. Otherwise (this also
   --  includes the case when the currently accessed tree is null tree),
   --  returns the main unit for the first tree in the set of trees making up
   --  The_Context (the meaning of the notion "the first tree" is
   --  implementation-dependent), and if this set is empty, returns
   --  Nil_Compilation_Unit.
   --
   --  This function does not check if the argument Context is open.
   --
   --  This function is practically useful for "-C1" Contexts

   function Compilation_Dependencies
     (Main_Unit : Asis.Compilation_Unit)
      return      Asis.Compilation_Unit_List;
   --  Provides the full list of units upon which Main_Unit depends
   --  in the GNAT compilation system. The kind of dependencies
   --  reported by this query combine semantic dependencies as
   --  defined by RM 95 and GNAT-specific dependencies. Main_Unit
   --  should be recompiled if any of the units from the returned
   --  list has been changed.
   --
   --  Main_Unit should be a main unit from some compilation which
   --  has created a tree file from the set of tree files making up
   --  the enclosing Context of Main_Unit.
   --
   --  ASIS_Inappropriate_Compilation_Unit is raised if Main_Unit
   --  does not satisfy this restriction.
   --
   --  Note, that this query is supposed to be used for ASIS Contexts
   --  representing complete Ada partitions, otherwise it may return
   --  formally correct, but meaningless results.
   --
   --  The interface of this query is still subject to design discussions???
   --  In particular, some limitations may be imposed on appropriate unit
   --  kinds, or a  special parameter may be added to filter out some parts
   --  of the result
   --
   --  Appropriate Unit_Kinds:
   --     A_Procedure
   --     A_Function
   --     A_Package
   --     A_Generic_Procedure
   --     A_Generic_Function
   --     A_Generic_Package
   --
   --     A_Procedure_Instance
   --     A_Function_Instance
   --     A_Package_Instance
   --
   --     A_Procedure_Renaming
   --     A_Function_Renaming
   --     A_Package_Renaming
   --
   --     A_Generic_Procedure_Renaming
   --     A_Generic_Function_Renaming
   --     A_Generic_Package_Renaming
   --
   --     A_Procedure_Body
   --     A_Function_Body
   --     A_Package_Body
   --
   --     A_Procedure_Body_Subunit
   --     A_Function_Body_Subunit
   --     A_Package_Body_Subunit
   --     A_Task_Body_Subunit
   --     A_Protected_Body_Subunit
   --
   --  Returns Unit_Kinds:
   --     A_Procedure
   --     A_Function
   --     A_Package
   --     A_Generic_Procedure
   --     A_Generic_Function
   --     A_Generic_Package
   --
   --     A_Procedure_Instance
   --     A_Function_Instance
   --     A_Package_Instance
   --
   --     A_Procedure_Renaming
   --     A_Function_Renaming
   --     A_Package_Renaming
   --
   --     A_Generic_Procedure_Renaming
   --     A_Generic_Function_Renaming
   --     A_Generic_Package_Renaming
   --
   --     A_Procedure_Body
   --     A_Function_Body
   --     A_Package_Body
   --
   --     A_Procedure_Body_Subunit
   --     A_Function_Body_Subunit
   --     A_Package_Body_Subunit
   --     A_Task_Body_Subunit
   --     A_Protected_Body_Subunit

   function Original_Text_Name
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String;
   --  In case if the source of the Compilation_Unit contains a
   --  Source_Reference pragma, returns the file name from this pragma,
   --  otherwise returns the same result as Asis.Compilation_Units.Text_Name
   --
   --  All Unit_Kinds are appropriate.

   function Is_Sub_Package_Implemented_As_Child_Unit
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Boolean;
   --  Internally, the GNAT implementation treats subpackages of Ada.Text_IO,
   --  such as Integer_IO, as implicit child units rather local packages
   --  defined in the specification of Ada.Text_IO. They would have undoubtedly
   --  been designed this way if child units had been available in the original
   --  design of Ada. This has the advantage that code for these units is only
   --  processed and loaded if used. At the compiler level, the difference is
   --  made entirely invisible to the Ada program, so this transformation is
   --  completely conforming to the RM. The situation with Ada.Wide_Text_IO and
   --  Ada.Wide_Wide_Text_IO is the same, see the documentation of these
   --  packages for full details.
   --
   --  But at the ASIS level, it is not practical to hide this transformation,
   --  so an ASIS application can see that this is happening, and the
   --  application itself must handle this as necessary. This query can be used
   --  to identify ASIS Compilation Units that represents specs and bodies
   --  of private children of predefined Ada text input-output packages that
   --  implements the optimization described above.
   --
   --  Note that reverting this GNAT transformation (if needed) is not obvious.
   --  You may have an ASIS Context that contains a spec and a body of a
   --  private child of a Standard text IO package, but only a spec of this
   --  Standard text IO package. So, if an application wants to mimic the code
   --  of the Standard text input-output package, it is possible to get from
   --  the private child spec into the spec of the input-output package, but
   --  not from the private child body into the body of the package because
   --  there is no such a body in the Context. So this is completely up to
   --  an ASIS application how to deal with such private children.

   -------------------------------------
   -- Extensions to Asis.Declarations --
   -------------------------------------

   function First_Name (Dcl : Asis.Element) return Asis.Element;
   --  Returns the first defining name from an argument declaration. Is
   --  equivalent to
   --
   --    Names (Dcl) (Names (Dcl)'First)
   --
   --  Appropriate Element_Kinds:
   --     A_Declaration
   --
   --  Returns Element_Kinds
   --     A_Defining_Name

   function Corresponding_Overridden_Operation
     (Declaration : Asis.Declaration)
      return        Asis.Declaration;
   --  In case if Is_Overriding_Operation (Declaration) is True, returns the
   --  declaration of the subprogram that is overridden by Declaration (it may
   --  be explicit or implicit declaration). Otherwise returns Nil_Element.
   --  Note, that this query knows nothing about multiple inheritance!
   --
   --  Appropriate Declaration_Kinds:
   --     A_Procedure_Declaration
   --     A_Function_Declaration
   --     A_Procedure_Instantiation
   --     A_Function_Instantiation
   --     A_Procedure_Body_Declaration
   --     A_Function_Body_Declaration
   --     A_Null_Procedure_Declaration
   --     A_Procedure_Renaming_Declaration
   --     A_Function_Renaming_Declaration
   --
   --  Returns Declaration_Kinds:
   --     A_Procedure_Declaration
   --     A_Function_Declaration
   --     A_Procedure_Instantiation
   --     A_Function_Instantiation
   --     A_Procedure_Body_Declaration
   --     A_Function_Body_Declaration
   --     A_Null_Procedure_Declaration
   --     A_Procedure_Renaming_Declaration
   --     A_Function_Renaming_Declaration

   function Corresponding_Overridden_Operations
     (Declaration : Asis.Declaration)
      return        Asis.Element_List;
   --  The difference with the previous Corresponding_Overridden_Operation
   --  in case of multiple inheritance this query returns all the operations
   --  of the parent type and all the interface types from which the owner
   --  of the argument primitive is derived. Returns Nil_Element_List if
   --  not Is_Overriding_Operation (Declaration).
   --
   --  Not implemented yet!!!
   --
   --  Appropriate Declaration_Kinds:
   --     A_Procedure_Declaration
   --     A_Function_Declaration
   --     A_Procedure_Instantiation
   --     A_Function_Instantiation
   --     A_Procedure_Body_Declaration
   --     A_Function_Body_Declaration
   --     A_Procedure_Renaming_Declaration
   --     A_Function_Renaming_Declaration
   --
   --  Returns Declaration_Kinds:
   --     A_Procedure_Declaration
   --     A_Function_Declaration
   --     A_Procedure_Instantiation
   --     A_Function_Instantiation
   --     A_Procedure_Body_Declaration
   --     A_Function_Body_Declaration
   --     A_Procedure_Renaming_Declaration
   --     A_Function_Renaming_Declaration

   function Has_Controlling_Result
     (Declaration : Asis.Declaration)
      return        Boolean;
   --  In case if argument function declaration tests True with
   --  Is_Dispatching_Operation, checks if the argument function has a
   --  dispatching result. Returns true for any unexpected argument and for any
   --  declaration that tests False with Is_Dispatching_Operation. Also returns
   --  False if an argument declaration is a completion of another declaration.
   --
   --  Expected Element_Kinds:
   --       A_Function_Declaration
   --       An_Expression_Function_Declaration
   --       A_Function_Renaming_Declaration
   --       A_Function_Body_Declaration
   --       A_Function_Body_Stub

   function Controlling_Parameters
     (Declaration : Asis.Declaration)
      return        Asis.Element_List;
   --  In case if argument subprogram declaration tests True with
   --  Is_Dispatching_Operation, returns the list of controlling parameter
   --  specifications (this list can be empty in case of a function having
   --  controlling result). Returns Nil_Element_List if the argument
   --  declaration tests False with Is_Dispatching_Operation. Also returns
   --  Nil_Element_List if an argument declaration is a completion of another
   --  declaration.
   --
   --  Appropriate Declaration_Kinds:
   --       A_Procedure_Declaration
   --       A_Function_Declaration
   --       An_Expression_Function_Declaration
   --       A_Procedure_Renaming_Declaration
   --       A_Function_Renaming_Declaration
   --       A_Null_Procedure_Declaration
   --       A_Procedure_Body_Declaration
   --       A_Function_Body_Declaration
   --       A_Procedure_Body_Stub
   --       A_Function_Body_Stub
   --
   --
   --  Returns Declaration_Kinds:
   --       A_Parameter_Specification

   ------------------------------------
   -- Extensions to Asis.Definitions --
   ------------------------------------

   function Inherited_Type_Operators
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Declaration_List;
   --  Returns a list of user-defined operator functions inherited by this
   --  type. (Each operator function in the result list has parameter (s) or
   --  (and) result of the argument type.
   --  This function is used in the implementation of
   --  Asis.Definitions.Corresponding_Type_Operators, that's why the list of
   --  appropriate kinds include type definitions that can not have any
   --  inherited declarations associated with them. For these arguments
   --  Nil_Element_List is returned. For non-null result each component of the
   --  result list Is_Part_Of_Implicit and Is_part_Of_Inherited
   --
   --  Appropriate Definition_Kinds:
   --       A_Type_Definition
   --       A_Formal_Type_Declaration
   --       A_Private_Type_Definition
   --       A_Tagged_Private_Type_Definition
   --       A_Private_Extension_Definition
   --       A_Task_Definition
   --       A_Protected_Definition
   --
   --  Returns Declaration_Kinds:
   --       A_Function_Declaration

   function Explicit_Type_Operators
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Declaration_List;
   --  If the argument is of A_Formal_Type_Definition kind, returns a list of
   --  formal operator function from the same formal part that have a parameter
   --  or return the result of this formal type. Otherwise returns a list of
   --  explicitly declared operator functions that are primitive operations
   --  of the argument type
   --
   --  Appropriate Definition_Kinds:
   --       A_Type_Definition
   --       A_Formal_Type_Declaration
   --       A_Private_Type_Definition
   --       A_Tagged_Private_Type_Definition
   --       A_Private_Extension_Definition
   --       A_Task_Definition
   --       A_Protected_Definition
   --
   --  Returns Declaration_Kinds:
   --       A_Function_Declaration
   --       A_Function_Body_Declaration
   --       A_Function_Body_Stub
   --       A_Function_Renaming_Declaration
   --       A_Function_Instantiation
   --       A_Formal_Function_Declaration

   function Corresponding_Parent_Subtype_Unwind_Base
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Declaration;
   --  This query differs from Asis.Definitions,Corresponding_Parent_Subtype
   --  in the following. If the argument type definition contains the ('Base)
   --  attribute reference as the parent subtype mark, it gets to the prefix
   --  of this attribute and applies
   --  Asis.Declarations.Corresponding_First_Subtype to it.

   ------------------------------------
   -- Extensions to Asis.Expressions --
   ------------------------------------

   function Full_Name_Image
     (Expression : Asis.Expression)
      return       Program_Text;
   --  Similar to Asis.Expressions.Name_Image, but also works on full expanded
   --  names

   function Denotation
     (Reference : Asis.Expression)
      return      Asis.Defining_Name;
   --  Returns the defining name denoted by Reference. Same as
   --  Corresponding_Name_Definition, except it works for A_Selected_Component.

   function Normalize_Reference (Ref : Asis.Element) return Asis.Element;
   --  This function is supposed to be called for the ASIS Elements
   --  representing a subtype mark. A subtype mark can be represented by
   --  an Element of one of the tree following kinds:
   --
   --       An_Identifier
   --       A_Selected_Component
   --       An_Attribute_Reference
   --
   --  This function "prepares" its argument for applying the ASIS
   --  Corresponding_Name_Definition and Corresponding_Name_Declaration
   --  queries, that is, returns its argument if it is of An_Identifier kind,
   --  returns the selector of the argument if it is of A_Selected_Component
   --  kind, and applies itself to the attribute prefix in case of
   --  An_Attribute_Reference

   function Corresponding_First_Definition
     (Defining_Name : Asis.Defining_Name)
      return          Asis.Defining_Name;
   --  In case there is more than one defining occurrence of an argument
   --  Defining_Name representing the same view of the same entity (such as a
   --  defining unit name for a program unit for which separate spec and body
   --  are present and a formal parameter name for a generic subprogram or
   --  subprogram having a separate spec) this function returns the first
   --  defining occurrence which actually introduces the corresponding entity.
   --  If there are only one defining occurrence of the argument Name, or if
   --  for some reason the first defining occurrence cannot be returned, the
   --  argument name is returned.
   --
   --  Appropriate Element kinds:
   --      A_Defining_Name
   --
   --  Returns Element kinds:
   --      A_Defining_Name

   function Corresponding_Body_Parameter_Definition
     (Defining_Name : Asis.Defining_Name)
      return          Asis.Defining_Name;
   --  When applying to a defining name which is a name of a formal parameter
   --  of a subprogram, this function returns the defining name of this
   --  parameter from a subprogram body. If there is no body for this
   --  subprogram, Nil_Element is returned. If Defining_Name is not a
   --  defining name of a formal subprogram parameter, Nil_Element is
   --  returned.
   --
   --  Appropriate Element kinds:
   --      A_Defining_Identifier
   --
   --  Returns Element kinds:
   --      A_Defining_Identifier
   --      Not_An_Element

   function Static_Expression_Value_Image
     (Expression : Asis.Expression)
      return       Wide_String;
   --  PARTIALLY IMPLEMENTED!!!
   --  Computes the value of Expression (which should be a static expression!)
   --  and represents it as a (wide) string. For enumeration expressions, the
   --  image of the Pos value of the defining enumeration or character literal
   --  corresponding to the  value of the expression is returned (see
   --  Asis.Declarations.Position_Number_Image query).
   --
   --  For ASIS Expression Elements for which Is_True_Expression yields False
   --  and empty string is returned
   --
   --  For non-static expressions, an empty string is returned.
   --
   --  Currently this function is implemented only for discrete and string
   --  types. For other types an empty string is returned.
   --
   --  Appropriate Element_Kinds:
   --     An_Expression

   function Static_Range_Low_Bound_Value_Image
     (Range_Element : Asis.Range_Constraint)
      return          Wide_String;
   --  PARTIALLY IMPLEMENTED!!!
   --  For A_Range_Attribute_Reference constraint defining by a static range,
   --  this function computes the value of the corresponding low bound and
   --  represents it as a (wide) string. For enumeration ranges, the
   --  image of the Pos value of the defining enumeration or character literal
   --  corresponding to the  value of the low bound is returned (see
   --  Asis.Extensions.Static_Expression_Value_Image and
   --  Asis.Declarations.Position_Number_Image queries).
   --
   --  For non-static expressions ranges, an empty string is returned.
   --
   --  Currently this function is implemented only for discrete types. For
   --  other types an empty string is returned.
   --
   --  Appropriate Constraint_Kinds:
   --     A_Range_Attribute_Reference

   function Static_Range_High_Bound_Value_Image
     (Range_Element : Asis.Range_Constraint)
      return          Wide_String;
   --  PARTIALLY IMPLEMENTED!!!
   --  For A_Range_Attribute_Reference constraint defining by a static range,
   --  this function computes the value of the corresponding high bound and
   --  represents it as a (wide) string. For enumeration ranges, the
   --  image of the Pos value of the defining enumeration or character literal
   --  corresponding to the  value of the high bound is returned (see
   --  Asis.Extensions.Static_Expression_Value_Image and
   --  Asis.Declarations.Position_Number_Image queries).
   --
   --  For non-static expressions ranges, an empty string is returned.
   --
   --  Currently this function is implemented only for discrete types. For
   --  other types an empty string is returned.
   --
   --  Appropriate Constraint_Kinds:
   --     A_Range_Attribute_Reference
   --
   --  Appropriate Discrete_Range_Kinds:
   --     A_Discrete_Range_Attribute_Reference

   -----------------------------
   -- Extensions to Asis.Text --
   -----------------------------

   function Element_Span_In_Template
     (Element : Asis.Element)
      return    Asis.Text.Span;
   --  If Is_Part_Of_Instance is True for the argument Element, then this
   --  function returns the span of the corresponding piece of code in the
   --  generic template. Otherwise a Nil_Span is returned. Nil_Span is also
   --  returned if Is_Part_Of_Implicit Element is True for Element.

   function Element_Image_In_Template
     (Element : Asis.Element)
      return    Program_Text;
   --  If Is_Part_Of_Instancce is True for the argument Element, then this
   --  function returns the image of the corresponding piece of code in the
   --  generic template. Otherwise a null string is returned. A null string
   --  is also returned if Is_Part_Of_Implicit_ELement is true for Element

   function Original_Line_Number
     (Element       : Asis.Element;
      Compiled_Line : Line_Number_Positive)
      return Line_Number;
   --  If the enclosing compilation unit of the argument Element contains a
   --  Source_Reference pragma, this function converts the line number of
   --  the file which actually was compiled ("physical" file) into the
   --  corresponding line number in the original file. For the line containing
   --  a Source_Reference pragma zero is returned.
   --
   --  Returns 0 if not Is_Text_Available(Element).
   --
   --  Raises ASIS_Inappropriate_Line_Number if Is_Text_Available(Element) and
   --  Compiled_Line is greater than the maximum line number of the compiled
   --  file

   --------------------------------
   -- General_Purpose Extensions --
   --------------------------------

   function Get_Last_Component (E : Asis.Element) return Asis.Element;
   --  Returns the right-most direct component of its argument. Returns
   --  Nil_Element if its argument has no components. It is an error to
   --  call this function for Nil_Element

   function Components (E : Asis.Element) return Asis.Element_List;
   --  Returns the list of all the first-level components of its argument.
   --  Nil_Element is returned for a terminal component.
   --  The implementation
   --  of this function is not very effective - we do not use any dynamic
   --  element lists, we simply compute the components twice - first time
   --  to get to know the overall number of components, and second
   --  time to fill in the result Element_List

   function Pos_In_List
     (E       : Asis.Element;
      In_List : Asis.Element_List)
      return    ASIS_Natural;
   --  Returns the index of of E in In_List. If In_List contins more than one
   --  element that Is_Equal to E, returns the index of the leftmost one.
   --  Returns 0 is E is not an element of In_List.

   generic
      type From is (<>);
      type To is (<>);
      Default : To;
   package Generic_Enum_Conversion is

      --  Elaboration of an instance body creates a table, which is efficient
      --  only if this is instantiated at library level.

      function Convert (X : From) return To;
      --  Converts X to a same-named value of type To. If there is no such
      --  value, returns Default. This is essentially doing
      --  "To'Value (From'Image (X))", except more efficiently.
      --  For example if "type From is (Red, Orange, Yellow);"
      --  and "type To is (Green, Amber, Red);" then
      --  Convert (Red) returns Red, and Convert (Orange) returns Default.

   end Generic_Enum_Conversion;

end Asis.Extensions;
