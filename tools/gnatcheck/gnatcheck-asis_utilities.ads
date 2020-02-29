------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . A S I S _ U T I L I T I E S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2004-2019, AdaCore                     --
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

--  This package defines various ASIS secondary and extension queries for
--  gnatcheck

with Asis;                       use Asis;
with Asis.Extensions.Strings;    use Asis.Extensions.Strings;

package Gnatcheck.ASIS_Utilities is

   --------------------------------------
   -- General-purpose utility routines --
   --------------------------------------

   function Get_Associations (El : Asis.Element) return Asis.Element_List;
   --  Returns a list of associations from El. This function expects that
   --  El has a list of associations as its first-level components. If it is
   --  not the case, Nil_Element_List is returned
   --
   --  Expected Expression_Kinds:
   --     A_Record_Aggregate
   --     An_Extension_Aggregate
   --     A_Positional_Array_Aggregate
   --     A_Named_Array_Aggregate
   --
   --  List of expected argument kinds may be extended!!!
   --
   --  Returned Element_Kinds:
   --     An_Association

   function Get_Choices (El : Asis.Element) return Asis.Element_List;
   --  Returns a list of choices from El. This function expects that
   --  El has a list of choices as its first-level components. If it is
   --  not the case, Nil_Element_List is returned
   --
   --  Expected Association_Kinds:
   --     A_Record_Component_Association
   --     An_Array_Component_Association
   --
   --  List of expected Association_Kinds may be extended!!!
   --
   --  Returns Element_Kinds:
   --     A_Definition
   --     An_Expression
   --
   --  Returns Definition_Kinds:
   --     A_Discrete_Range
   --     An_Others_Choice

   function Get_Name_Definition (Ref : Asis.Element) return Asis.Element;
   --  Returns the name definition for Ref, unwinding all the renamings, if
   --  any. Note, that this function should be used only if a client is sure
   --  that all the possible renamings indeed can be unwound (that is, it is
   --  safe to apply it to exception or package names, but it may blow up or
   --  return a junk result on variable name)
   --
   --  ??? Should be moved to Asis.Extensions???
   --
   --  Appropriate Expression_Kinds:
   --       An_Identifier
   --       A_Selected_Component
   --
   --  Returns Element_Kinds:
   --       Not_An_Element
   --       A_Defining_Name

   function Is_Frame (El : Asis.Element) return Boolean;
   --  Checks if El contains a handled sequence of statement as its first-level
   --  component ("frame" is an old Ada-83 term for such a construct). All the
   --  Element kinds are expected.

   function Is_Positional (El : Asis.Element) return Boolean;
   --  Checks if the argument association element is a positional association.
   --  Returns False for any unexpected element
   --
   --  Expected Element_Kinds:
   --
   --     An_Association

   function Has_Positional_Association (El : Asis.Element) return Boolean;
   --  Checks if this argument contains a positional association. At the moment
   --  this function is implemented only for aggregates, but it may be
   --  generalized for other elements that have associations as their
   --  first-level components (subprogram and entry calls, discriminant
   --  constraints, generic instantiations)
   --
   --  Returns False for any unexpected Element
   --
   --  Expected Expression_Kinds
   --
   --     A_Record_Aggregate
   --     An_Extension_Aggregate

   function Get_Handlers
     (El              : Asis.Element;
      Include_Pragmas : Boolean := False)
      return            Asis.Element_List;
   --  If Is_Frame (El), returns a list of the exception handlers from El,
   --  otherwise returns Nil_Element List

   function Is_Handled
     (Exc  : Asis.Element;
      By   : Asis.Element_List)
      return Boolean;
   --  Supposing that Exc is a defining name of an exception from an exception
   --  declaration (or from exception renaming declaration), and By is a list
   --  of AN_Exception_Handler elements, checks if this exception is handled by
   --  the given list of handlers. The function does not check that its
   --  arguments are appropriate for this check, a caller is completely
   --  responsible for providing correct arguments. This function assumes that
   --  any actual for By parameter is a list or exception handlers obtained
   --  from some construct, that is, if it contains a handler with OTHERS
   --  choice, then this is the last handler in the argument list, and in this
   --  case the result is TRUE.

   function Raises_Exception (El : Asis.Element) return Boolean;
   --  Checks if El contains a statement sequence, and this statement sequence
   --  raises an exception. The current implementation is very simple-minded -
   --  it just check that the statement sequence contains a raise statement, no
   --  control-flow analysis is performed.
   --
   --  This function always returns False if the argument is a package body
   --  declaration (even it contains a statement sequence with raise
   --  statement). For a task body also False is always returned.
   --
   --  If El is not an exception handler, then no exception handlers are
   --  checked. Nested handlers are never checked.

   function Is_Address_Specification (El : Asis.Element) return Boolean;
   --  Checks if the argument is either an address clause or the definition of
   --  address aspect.

   function Enclosing_List return Asis.Element_List;
   --  This function returns a list of all the first-order components of the
   --  Element that is wisited (that's why it does not have an argument - it
   --  works on a traversal stack). If the argument is a top argument in the
   --  Compilation Unit structure, Nil_Element_List is returned

   ---------------------------------------
   -- Routines used by individual rules --
   ---------------------------------------

   function Can_Be_Replaced_With_Function
     (Decl : Asis.Element)
      return Boolean;
   --  Checks if its argument element is a procedure that can be rewritten as
   --  a function. A procedure is considered as being replaced with an
   --  equivalent function if it has exactly one non-limited parameter of OUT
   --  mode, and no parameter of IN OUT mode. Returns False for any unexpected
   --  element
   --
   --  Expected Declaration_Kinds:
   --     A_Procedure_Declaration
   --     A_Procedure_Body_Declaration
   --     A_Procedure_Body_Stub
   --     A_Generic_Procedure_Declaration
   --     A_Formal_Function_Declaration

   function Denotes_Access_Subtype (N : Asis.Element) return Boolean;
   --  For Misnamed_Identifiers
   --  Checks if the argument is an defining identifier that denotes an access
   --  type or subtype.

   function Denotes_Class_Wide_Subtype (N : Asis.Element) return Boolean;
   --  For Misnamed_Identifiers
   --  Checks if the argument is an identifier or an expanded name that
   --  denotes a class-wide subtype

   function Full_View_Visible
     (Type_Decl : Asis.Declaration;
      At_Place  : Asis.Element)
      return      Boolean;
   --  Provided that Type_Decl represents a declaration of a private type or
   --  private extension declaration, checks if the corresponding full
   --  declaration is visible at the place where actual for At_Place is
   --  located. Returns False if the actual for Type_Decl does not represent
   --  a private type declaration or a private extension declaration, or if
   --  any of the abuttals is either Is_Part_Of_Implicit or
   --  Is_Part_Of_Instance.
   --
   --  This function assumes that Type_Decl is visible At_place.
   --
   --  The function is originally developed for Direct_Calls_To_Primitives
   --  rule.

   function Get_Corresponding_Definition
     (El   : Asis.Element)
      return Asis.Element;
   --  Similar to the ASIS Corresponding_Name_Definition, but in case if in is
   --  not possible to get a single defining name for the argument, returns
   --  Nil_Element instead of raising Asis_Inappropriate_Element

   function Is_Prefix_Notation_Exception
     (El                 : Asis.Element;
      Exclude_Second_Par : Boolean)
      return Boolean;
   --  This function checks if its argument is one of the exception cases for
   --  Positional_Parameter_Associations rule, related to prefix notation
   --  for primitive operations of tagged types. The first exception case
   --  corresponds to the first parameter of a call that uses prefixed
   --  notation - it is always has a positional association. The second
   --  exception is a second parameter of the call, if the called subprogram
   --  has exactly two parameters (that is, to Parameter in
   --
   --    Object.Operation (Parameter)
   --
   --  If Exclude_Second_Par is ON, the function returns False for the second
   --  exception case.
   --
   --  This function assumes that Is_Prefix_Notation is True for
   --  Enclosing_Element of his argument.

   function Has_One_Parameter (El : Asis.Element) return Boolean;
   --  Checks that:
   --
   --  - its argument element is a subprogram/entry call and the called entity
   --    has exactly one formal parameter.
   --  or
   --  - its argument is a generic instantiation and the instantiated generic
   --    has exactly one formal generic parameter.
   --
   --  Returns False for any unexpected argument.
   --
   --  Expected Expression_Kinds:
   --     A_Function_Call
   --
   --  Expected Statement_Kinds:
   --     A_Procedure_Call_Statement
   --     An_Entry_Call_Statement

   function Is_Boolean_Logical_Op (Op : Asis.Element) return Boolean;
   --  Checks if Op is a reference to a logical operation of a Boolean (that
   --  is, Standard.Boolean or derived thereof) type. returns False for any
   --  unexpected Element
   --
   --  Expected Operator_Kinds:
   --     An_And_Operator
   --     An_Or_Operator
   --     An_Xor_Operator

   function Is_Control_Structure (Stmt : Asis.Element) return Boolean;
   --  Checks if its argument is a control structure (according to the
   --  definition of the control structure in Control_Structure_Nesting rule).
   --  Returns False for any unexpected element
   --
   --  Expected Element_Kinds:
   --     A_Statement

   function Is_Constraint_Error (Ref : Asis.Element) return Boolean;
   --  Supposing that Checks Ref is an exception name, checks that it denotes
   --  the predefined Constraint_Error exception (unwinding all the renamings)
   --  Returns False if it denotes the obsolete predefined renaming of
   --  Constraint_Error as Numeric_Error.

   function Is_Interrupt_Handler (Proc : Asis.Element) return Boolean;
   --  Checks if the argument is an interrupt handler (a protected
   --  parameterless procedure for which a pragma or an aspect Attach_Handler
   --  or Interrupt_Handler is applied. Returns False for any unexpected
   --  Element.
   --
   --  Expected Declaration_Kinds
   --     A_Procedure_Declaration

   function Is_Numeric_Error (Ref : Asis.Element) return Boolean;
   --  Supposing that Checks Ref is an exception name, checks that it denotes
   --  the obsolete predefined renaming of Constraint_Error as Numeric_Error.

   function Is_Predefined (Operation : Asis.Element) return Boolean;
   --  Check if Operation denotes a predefined operation. Returns False for
   --  any unexpected element. Also returns False if Operation is an ambiguous
   --  reference to more than one entity.
   --
   --  Expected Expression_Kinds:
   --     An_Operator_Symbol

   function Is_Float (Expr : Asis.Element) return Boolean;
   --  Check if Expr is of float type.
   --  Returns False for any unexpected element
   --
   --  Expected Element_Kinds:
   --     An_Expression (should be Asis.Extensions.Is_True_Expression)

   function Is_Fixed (Expr : Asis.Element) return Boolean;
   --  Check if Expr is of fixed type.
   --  Returns False for any unexpected element
   --
   --  Expected Element_Kinds:
   --     An_Expression (should be Asis.Extensions.Is_True_Expression)

   function Is_From_Standard (El : Asis.Element) return Boolean;
   --  Checks if its argument belongs to the predefined Standard package. Any
   --  non-null Element is expected

   function Is_Public (Def_Name : Asis.Element) return Boolean;
   --  Checks if Def_Name can be referenced from another compilation unit.
   --  Returns False for any unexpected element,
   --
   --  Expected Element_Kinds:
   --     A_Defining_Name

   function Is_Ref_To_Standard_Num_Subtype
     (Ref  : Asis.Element)
      return Boolean;
   --  Supposing that Ref is of An_Identifier kind, checks if it is a reference
   --  to a predefined numeric (sub)type defined in Standard.

   function Is_Standard_Boolean (Expr : Asis.Element) return Boolean;
   --  Check if Expr is of the type Standard.Boolean (or of a subtype thereof).
   --  Returns False for any unexpected element
   --
   --  Expected Element_Kinds:
   --     An_Expression (should be Asis.Extensions.Is_True_Expression)

   function Is_Predefined_String (Type_Decl : Asis.Element) return Boolean;
   --  Check if Type_Decl is the declaration of a predefined String type or a
   --  subtype thereof. A type derived from a predefined String type is also
   --  considered as predefined String type.
   --  Returns False for any unexpected element
   --
   --  Expected Declaration_Kinds:
   --     An_Ordinary_Type_Declaration
   --     A_Subtype_Declaration

   function Is_Unconstrained_Array (Type_Decl : Asis.Element) return Boolean;
   --  Check if Type_Decl is the declaration of an unconstrained array type or
   --  subtype. A type derived from an unconstrained array (sub)type is also
   --  considered as an unconstrained array type.
   --  Returns False for any unexpected element
   --
   --  Expected Declaration_Kinds:
   --     An_Ordinary_Type_Declaration
   --     A_Subtype_Declaration

   function Contains_Loop (El : Asis.Element) return Boolean;
   --  Supposing that El is of A_Statement or A_Path kind, checks if it
   --  contains a loop statement. Declarations (that may be a part of enclosed
   --  block statements) are not considered. Returns False for any unexpected
   --  argument
   --
   --  Expected Element_Kinds:
   --
   --    A_Statement
   --    A_Path

   function Has_Address_Clause (Def_Name : Asis.Element) return Boolean;
   --  Checks if the argument is a defining name to which the address clause
   --  is applied. Returns False for any unexpected element
   --
   --  Expected Defining_Name_Kinds:
   --     A_Defining_Identifier

   function Has_Statements_And_Decls (Decl : Asis.Element) return Boolean;
   --  Checks that the argument package body declarations has both non-empty
   --  declaration and statement sequences. Returns False for any unexpected
   --  Element
   --
   --  Expected Declaration_Kinds:
   --     A_Package_Body_Declaration

   function Get_Call_Element return Asis.Element;
   --  Supposing that the top of the traversal stack is a function name, this
   --  function tries to locate the corresponding function call. Note, that
   --  this function uses the traversal stack to go up the syntax structure, so
   --  it is safe to use it only in the Pre- and Post-operations to get the
   --  call element for the currently visited element. Note also, that the
   --  result may be not a function call (for example, if the argument is an
   --  operation symbol that is a pragma argument), so the caller is
   --  responsible for checking the result.

   function Defines_Components (Decl : Asis.Element) return Boolean;
   --  Checks if the argument is a type declaration that can define a
   --  component. Type is considered as a type that can define a component if
   --  it contains record_definition (by its own or as a part of a record
   --  extension). Even if the record definition actually defines no
   --  components, for the type containing it as a part of its definition this
   --  function returns True. Returns False for any unexpected Element.
   --
   --  Expected Declaration_Kinds:
   --     An_Ordinary_Type_Declaration

   function Constraint_Depends_On_Discriminant
     (Constr : Asis.Element)
      return   Boolean;
   --  Checks if the argument is a constraint from a component definition and
   --  this constraint depends of discriminant. Returns False for any
   --  unexpected Element. Note, that this function can return True only index
   --  or discriminant constraint.
   --
   --  Expected Constraint_Kinds:
   --     An_Index_Constraint
   --     A_Discriminant_Constraint

   function Self_Ref_Discr_Constraint
     (Constr : Asis.Element)
      return   Boolean;
   --  Checks if the argument is a discriminant constraint from a record
   --  component definition and if at least one value provided for a
   --  discriminant is an access attribute (including 'Access,
   --  'Unchecked_Access and 'Unrestricted_Access) and the prefix of the
   --  attribute denotes the type this component belongs to. Returns False in
   --  any other case.
   --
   --  Expected Constraint_Kinds:
   --     An_Index_Constraint
   --     A_Discriminant_Constraint

   function Is_Constant (E : Asis.Element) return Boolean;
   --  For Identifier_Casing, Identifier_Prefixes.
   --  Checks if E is a defining name that defines a constant object. This
   --  function just check Ekind of the corresponding tree node and does not
   --  perform any smart analysis. Returns False for any unexpected element.
   --
   --  Expected Defining_Name_Kinds:
   --     A_Defining_Identifier

   function Storage_Order_Defined_By_Pragma (E : Asis.Element) return Boolean;
   --  For No_Scalar_Storage_Order_Specified
   --  Assumes that the argument is of An_Ordinary_Type_Declaration kind and
   --  the defined type is a record type (including derived types), otherwise
   --  the result is undefined (and most probably the function just blows up).
   --  Checks if the type has the Attribute_Definition pragma applied to it and
   --  this pragma defines the Scalar_Storage_Order attribute for this type.

   function Denotes_Subtype_With_Predicate
     (E    : Asis.Element)
      return Boolean;
   --  Expects that E is either An_Identifier or A_Selected_Component Element
   --  and returns False for any unexpected Element. For expected Element,
   --  checks if the argument is a subtype mark, and if it is, if the
   --  denoted subtype is defined with a dynamic predicate.

   function From_Subtype_With_Predicate
     (E    : Asis.Element)
      return Boolean;
   --  Assumes that E is a prefix of 'Valid attribute. Checks if E has type,
   --  and if it does, if the nominal subtype of E is defined with a dynamic
   --  predicate.

   function Is_Limited (SM : Asis.Element) return Boolean;
   --  Supposing that SM represent a subtype mark, checks if the denoted type
   --  is limited. Returns False for any unexpected element.
   --
   --  Expected Expression_Kinds:
   --       An_Identifier
   --       A_Selected_Component
   --       An_Attribute_Reference

   procedure Check_Classwide_Pre_Vioaltion
     (Op       : Asis.Element;
      Detected : out Boolean;
      At_SLOC  : out String_Loc);
   --  Assuming that Op is a declaration of overriding dispatching operation
   --  this procedure checks if all the operations that are
   --  overridden/implemented by this declaration have class-wide precondition
   --  (either explicitly specified or inherited). If this is the case,
   --  the OUT parameter Detected is set True, and At_SLOC points to the string
   --  that is a concatenation of "%1%" and standard GNAT reference to the
   --  (explicit) declaration of implemented/inherited operation that does not
   --  have a class-wide Pre-condition. If there is more than one
   --  implemented/inherited with no Class-Wide precondition, there is
   --  non-determinated which one is indicated by At_SLOC parameter. Otherwise
   --  Detected is set to False, and At_SLOCK to Nil_String_Loc.
   --
   --  Unfortunately ASIS has not been properly updated for multiple
   --  inheritance introduced in Ada 2012, so the functionality described above
   --  cannot be implemented in ASIS and we have to use direct tree traversing.

   function Contains_Modular_Component
     (Type_Decl : Asis.Element)
      return      Boolean;
   --  Assuming that Type_Decl represents a record type or record extension
   --  declaration checks if the type contains a component of a modular type.
   --  Returns False for any unexpected element.
   --
   --  Expects Declaration_Kinds:
   --     An_Ordinary_Type_Declaration

   function Is_Representation_Item (El : Asis.Element) return Boolean;
   --  Defines if the argument is a representation item for
   --  Misplaced_Representation_Items rule.

   function Entity_From_Rep_Item
     (Rep_Item : Asis.Element)
      return     Asis.Element;
   --  Assuming that Rep_Cl represents a representation item (as defined for
   --  Misplaced_Representation_Items rule, that is by Is_Representation_Item
   --  query) computes the declaartion of the entity this representation item
   --  is applied to.
   --  Raises ASIS_Inappropriate_Element for any other argument
   --
   --  Appropriate Clause_Kinds:
   --     A_Representation_Clause
   --
   --  Appropriate Element_Kinds
   --     A_Pragma (The pragma name is one of the following:
   --                 Atomic
   --                 Atomic_Components
   --                 Independent
   --                 Independent_Components
   --                 Pack
   --                 Unchecked_Union
   --                 Volatile
   --                 Volatile_Components)

   function Needs_Real_Range_Definition (El : Asis.Element) return Boolean;
   --  For No_Explicit_Real_Range rule. Checks if the argument is a declaration
   --  of a floating point type or a decimal fixed point type, or a derived
   --  type declaration where the ancestor type is of these kinds.

   function Has_Range_Specification (El : Asis.Element) return Boolean;
   --  For No_Explicit_Real_Range rule, Assumes that for the outer call (this
   --  is a recursive function) Needs_Real_Range_Definition (El) is True.
   --  Checks if the argument type has a range specification. In case if the
   --  argument is a derived type, step-by-step unwinds the chain of
   --  derivations and subtyping and stops when finds a range constraint.

   function Is_Object_Address_Specification (El : Asis.Element) return Boolean;
   --  Checks if the argument is either an address clause or the definition of
   --  address aspect, and it is applied to a data object.

   --  Routines for Outbound_Protected_Assignment rule --

   function Get_Encl_Protected_Body return Asis.Element;
   --  Checks if El is an element located in protected body. If it is, returns
   --  the corresponding protected body, otherwise returns Nil_Element
   --  This function uses traversal stack, that's why it does not have any
   --  argument.

   function Get_Obj_Dcl (El : Asis.Element) return Asis.Element;
   --  Assuming that El is an element representing the variable name from an
   --  assignment statement, tries to compute the declaration of the data
   --  object that is the source of this assignment (or which component is the
   --  source of this assignment). If this is not possible because of any
   --  reason Nil_Element is returned.

   function Is_Local
     (Dcl            : Asis.Element;
      Protected_Body : Asis.Element)
      return Boolean;
   --  Assuming that Dcl represents a data object declaration, and
   --  Protected_Body represents a protected body, checks if Dcl is local to
   --  Protected_Body (that is, declared either inside it or inside the
   --  corresponding protected specification).

   ----------------------------------------------------------------------
   -- Routines used for creating and analyzing of the global structure --
   ----------------------------------------------------------------------

   function Is_Call_To_Operator_Function (El : Asis.Element) return Boolean;
   --  Checks if the argument element is a call to an operator function, if the
   --  argument represents a call to operator function in prefix form, such as
   --  "+" (I, J), the result is also True. Returns False for any unexpected
   --  argument.
   --
   --  Expected Expression_Kinds:
   --     A_Function_Call

   -------------------------------------------------
   --  Utilities used to form diagnostic messages --
   -------------------------------------------------

   function Scope_Name (El : Asis.Element) return String;
   --  If not Is_Nil (El) and Debug_Flag_JJ is ON (-dJ debug option is
   --  specified) returns the full Ada name of the innermost scope that
   --  encloses El.

   --------------------------------------------------------------
   -- Utilities that are not used at the moment (see G523-016) --
   --------------------------------------------------------------

   function Get_Root_Type (Decl : Asis.Element) return Asis.Element;
   --  Returns the root type for the object(s). All the subtypings and
   --  derivations are unwound.
   --
   --  What about private types???
   --
   --  Appropriate Declaration_Kinds:
   --       A_Variable_Declaration
   --       A_Constant_Declaration
   --       ????
   --
   --  Returns Element_Kinds:
   --       An_Ordinary_Type_Declaration
   --       A_Task_Type_Declaration
   --       ????

   function Get_Type_Components
     (El                    : Asis.Element;
      Include_Discriminants : Boolean)
      return                  Asis.Element_List;
   --  Supposing that El is either a A_Protected_Type_Declaration or
   --  (non-private!) An_Ordinary_Type_Declaration element, returns the
   --  component definitions from the type definition, if any. Returns
   --  Nil_Element_List if the type does not have components. Discriminant
   --  components are included in the result only if Include_Discriminants is
   --  set ON.
   --
   --  For a derived type or for a type extension, the result also includes
   --  implicit inherited components (so we relay on the fact that processing
   --  of implicit components is reliable enough!). Inherited discriminants are
   --  NOT collected!

   function Get_Type_Decl_From_Subtype_Mark
     (SM   : Asis.Element)
      return Asis.Element;
   --  Supposing that SM represents subtype mark that is not an attribute
   --  reference, returns the full declaration of the corresponding type,
   --  unwinding subtyping and getting the full type declaration in case of a
   --  private type. Raises ASIS_Inappropriate_Element otherwise. (Note that
   --  this function does not expect to get any incomplete type declaration
   --  as a part of processing)
   --
   --  Appropriate Expression_Kinds:
   --       An_Identifier
   --       A_Selected_Component
   --
   --  Returns Declaration_Kinds:
   --       An_Ordinary_Type_Declaration
   --       A_Task_Type_Declaration
   --       A_Protected_Type_Declaration

   function Declaration_Of_Renamed_Entity
     (R    : Asis.Element)
      return Asis.Element;
   --  Supposing that R is a renaming declaration element, this function tries
   --  to find the declaration of the entity that is renamed (it can be another
   --  renaming declaration). If this is impossible because of any reason (the
   --  renaming entity is a component that does not have  a separate
   --  declaration, a object, subprogram or a value pointed by an access value,
   --  etc.), Nil_Elemet is returned

   function Is_Function_Declaration (El : Asis.Element) return Boolean;
   --  Checks if El is any kind of a declaration that declares a function
   --  (actually this function checks that the declaration kind of El is in
   --  the list of returned kinds of Corresponding_Called_Function)

   function Changed_Element (El : Asis.Element) return Asis.Element;
   --  Supposing that El represents a name of a data object (???), tries
   --  to locate the Element representing the defining name of the (whole)
   --  object (that is, returns the defining name of a variable in case if
   --  El a reference to a component, and so on)

   function Corresponding_Protected_Object
     (Pref : Asis.Element)
      return Asis.Element;
   --  Provided that Pref is a prefix name from a protected operation call,
   --  defines the declaration of the protected object the called operation
   --  belongs to.
   --  For a protected object declared by a single protected declaration, this
   --  function returns this declaration, otherwise the defining name of a
   --  protected object is returned.
   --
   --  ??? Protected objects that are parts of other data structures???

   function Is_Task_Entry_Call (Call : Asis.Element) return Boolean;
   --  Provided that Call is of An_Entry_Call_Statement kind, check that the
   --  called entry is a task entry.

   function Is_Protected_Operation_Call (Call : Asis.Element) return Boolean;
   --  Provided that Call is of A_Function_Call, A_Procedure_Call_Statement or
   --  An_Entry_Call_Statement kind, check that the called entity is a
   --  protected operation.

   function Can_Cause_Side_Effect (El : Asis.Element) return Boolean;
   --  Checks if the argument can cause a side effect. (That is, the
   --  execution/evaluation/elaboration of a construct can change any data
   --  object).
   --
   --  This function partly duplicates duplicates the routine that computes a
   --  side effect level (???), we introduce it at the development stage
   --  to get some structurable and maintainable design of the side effect
   --  problem

   function Is_Renaming (El : Asis.Element) return Boolean;
   --  Checks if El is a renaming that is of interest for a call graph.

   function Is_Template_Caller (El : Asis.Element) return Boolean;
   --  Checks if El is a "template" for entities that can call another entity
   --  and/or called by another entity. For example, a task type or a protected
   --  operation is such a template.

   function Is_Task_Creation (El : Asis.Element) return Boolean;
   --  Check if a new task object can be created as a result of elaboration of
   --  the argument object. Note that we are considering a task as a callable
   --  entity, and creation a task means calling this entity

   function Is_Dynamic_Call (Call : Asis.Element) return Boolean;
   --  Checks if the argument is a dynamic (but not dispatching!) call.
   --  Dynamic means that the calling entity cannot be statically determined
   --  (what else can we have here except the call through the access to
   --  a procedure or to a function?)

   function Is_Body (El : Asis.Element) return Boolean;
   --  Checks if the argument element is a body element from the point of view
   --  of the call graph

   function Is_Constructor (Element : Asis.Element) return Boolean;
   --  Checks if El is a declaration of a constructor function (that is, a
   --  primitive function of a tagged type that has a controlling result and no
   --  controlling parameter). Returns False if El is a completion of another
   --  declaration.

   function Is_Downward_View_Conversion
     (Element : Asis.Element)
      return    Boolean;
   --  Checks if El is a downward vew conversion.

   function Is_Enum_Literal_Renaming (El : Asis.Element) return Boolean;
   --  Checks if the argument element is a renaming declaration and the renamed
   --  entity is an enumeration literal (the renamed entity does not have
   --  to be statically determined!)

   function Needs_Completion (El : Asis.Element) return Boolean;
   --  Checks if its argument needs a completion

   function Get_Whole_Object (El : Asis.Element) return Asis.Element;
   --  Supposing that El is A_Selected_Component Element (and it indeed
   --  represents a true expression), returns the part of this expanded name
   --  that represents the whole data object (the argument can be either a
   --  subcomponent or an expanded name of some data object). The result should
   --  be of An_Identifier kind.
   --  Returns Nil_Element if returning An_Identifier Element representing the
   --  whole data object is impossible because of any reason (???)
   --  (For the rule about side effects)

   function Used_To_Pass_Actual_Subpr (El : Asis.Element) return Boolean;
   --  Checks if the argument is an artificial subprogram renaming used to pass
   --  the actual subprogram into expanded generic instantiation.

   function Call_To_Complicated_Cuncurrent_Structure
     (Call : Asis.Element)
      return Boolean;
   --  This function actually defines the limitations imposed on the complexity
   --  of the calls to protected operations and task entries. If it returns
   --  True, that means that the given call can not be represented in the call
   --  graph and used for checking the global rules.
   --
   --  The current limitations are rather strong - the task or protected object
   --  the called entry or protected operation belongs to should be declared
   --  either by single task/protected declarations or by an object declaration
   --  with a subtype mark denoting a task or protected type or a type derived
   --  from a task or protected type.
   --
   --  This function does not check that its argument is a call to a protected
   --  operation or a task entry.

   function Get_Underlying_Type
     (SM              : Asis.Element;
      Stop_At_Private : Boolean := False)
      return            Asis.Element;
   --  Assumes that SM is a subtype mark. Tries to get from this subtype mark
   --  to the type declaration that defines the underlying properties of the
   --  type. That is, from private type/extension declaration it gets to the
   --  corresponding full declaration, from subtype - to the corresponding
   --  first named subtype, from derived type (including record extension) - to
   --  the corresponding root type. If SM denotes an incomplete type, this
   --  incomplete type is returned as the result.
   --
   --  Returns Nil_Element if this function cannot do this because of any
   --  reason. This function does not raise any exception.

   function Unwind_Type
     (D               : Asis.Element;
      Stop_At_Private : Boolean := False)
      return            Asis.Element;
   --  Assumes that D is a subtype declaration or a type declaration (including
   --  task, protected, formal and incomplete type declarations, otherwise
   --  returns the argument unchanged or raises Assert_Error in debug mode).
   --  Unwinds subtyping, private declarations and derivation.

end Gnatcheck.ASIS_Utilities;
