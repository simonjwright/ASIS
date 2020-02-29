------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                    A S I S _ U L . U T I L I T I E S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains ASIS utilities (both ASIS secondary queries and ASIS
--  extensions that directly operates on the GNAT tree) that are used more
--  then in one place (either in two or more tools or in some tool and in the
--  ASIS UL itself). This package can be considered as a starting point for
--  adding this or that utility to Asis.Extensions.

with Snames;

with Asis;                       use Asis;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;
with Asis.Text;                  use Asis.Text;

package ASIS_UL.Utilities is

   function Dispatching_Operations
     (Type_Def : Asis.Element)
      return     Asis.Element_List;
   --  Provided that Can_Have_Dispatching_Operations (Type_Def), returns the
   --  list of dispatching operations of the corresponding type. Otherwise
   --  raises Asis_Inappropriate_Element

   function Get_Called_Element (Call : Asis.Element) return Asis.Element;
   --  Supposing that the argument Is_Call, tries to compute the called
   --  element, depending on the kind of the call. If the called element
   --  can be detected, returns Corresponding_Element for it, otherwise
   --  returns Nil_Element.
   --
   --  What about calls to attribute subprograms in case when the corresponding
   --  attribute is predefined???

   function Call_Parameters (Call : Asis.Element) return Asis.Element_List;
   pragma Obsolescent (Call_Parameters);
   --  Supposing that Call is a procedure, function or entry call, returns
   --  a (non-normalized) list of parameter associations from the call. This
   --  function checks if its argument is an appropriate element for getting
   --  the association list only by applying the corresponding assertion, if
   --  assertions are OFF, Nil_Element_List is returned for an inappropriate
   --  argument element.

   function Unwind_Derivations_And_Subtyping
     (Decl : Asis.Element)
      return Asis.Element;
   --  Provided that Decl is of An_Ordinary_Type_Declaration or
   --  A_Subtype_Declaration kind (a caller is responsible for providing the
   --  appropriate Element), unwinds all derivations and subtypings, returning
   --  the type declaration that defines all the type properties of the
   --  argument type. Type extensions are also unwound.

   function Get_Called_Ref (Call : Asis.Element) return Asis.Element;
   --  If Element is a function, procedure or entry call - returns the name
   --  of the called entity from this call. Otherwise this function raises
   --  ASIS_INAPPROPRIATE_ELEMENT, but does it without providing any proper
   --  diagnosis.

   function Get_Statements (El : Asis.Element) return Asis.Element_List;
   pragma Obsolescent (Get_Statements);
   --  Returns the list of statements (excluding pragmas, if any) from the
   --  argument Element. List of appropriate Elements may be extended...

   function Called_Profile (Call : Asis.Element) return Asis.Element_List;
   pragma Obsolescent (Called_Profile);
   --  Supposing that Call is a procedure, function or entry call, returns
   --  a list of parameter specifications that statically correspond to the
   --  called entity (in case of a dispatching call this list is taken from the
   --  declaration of the corresponding subprogram, in case of dynamic call
   --  through the access-to-subprogram value - from the corresponding access
   --  definition. This function checks if its argument is an appropriate
   --  element for getting the called profile only by applying the
   --  corresponding assertion, if assertions are OFF, Nil_Element_List is
   --  returned for an inappropriate argument element.
   --
   --  Returns Nil_Element_List in case of a call to a predefined operator.
   --
   --  ??? Needs more testing!!!
   --  ??? Can we remove this at all???

   function Get_Parameter_Declaration (El : Asis.Element) return Asis.Element;
   --  Supposing that El is of A_Parameter_Association kind and the called
   --  entity is not an attribute subprogram (that is we have a possibility
   --  to get the profile of the called entity), gets the declaration of the
   --  corresponding formal parameter.

   function Get_Type_Structure (Decl : Asis.Element) return Asis.Element;
   --  This function gets to the type declaration that defines the actual
   --  structure of the type represented by argument type or subtype
   --  declaration. It unwinds subtyping and derivations and goes from private
   --  to full type declarations till it stops either at formal type
   --  declaration or at derived type with non-empty extension part (this
   --  gives the possibility to analyze extension components, this is the
   --  difference of this query from the standard
   --  Asis.Definitions.Corresponding_Type_Structure query) or at the
   --  declaration of non-private non-derived type. (If the argument is
   --  An_Incomplete_Type_Declaration kind, it is returned unchanged.
   --
   --  Appropriate Declaration_Kinds:
   --    An_Ordinary_Type_Declaration
   --    A_Task_Type_Declaration
   --    A_Protected_Type_Declaration
   --    An_Incomplete_Type_Declaration
   --    A_Tagged_Incomplete_Type_Declaration
   --    A_Private_Type_Declaration
   --    A_Private_Extension_Declaration
   --    A_Subtype_Declaration
   --    A_Formal_Type_Declaration
   --
   --  Returns Declaration_Kinds:
   --    An_Ordinary_Type_Declaration
   --    A_Task_Type_Declaration
   --    A_Protected_Type_Declaration
   --    An_Incomplete_Type_Declaration
   --    A_Tagged_Incomplete_Type_Declaration
   --    A_Formal_Type_Declaration

   function Get_Subtype_Structure (Def : Asis.Element) return Asis.Element;
   --  Similar to Get_Type_Structure, but works on A_Subtype_Indication
   --  and An_Access_Definition (An_Anonymous_Access_To_Variable and
   --  An_Anonymous_Access_To_Constant) Elements.
   --
   --- Note, that applying this function to analyze the structure of the
   --  record type may result in infinite loop for the cases like
   --
   --   type T_Root is abstract tagged record
   --      Next : access T_Root'Class;
   --   end record;
   --
   --  If the argument is the definition of the component Next, the result
   --  will be the declaration of enclosed type.

   function Inheritance_Depth (Type_Def : Asis.Element) return Natural;
   --  Assuming that Type_Def is A_Type_Definition element, computes the
   --  inheritance depth for the type. For tagged derived type, private
   --  extension and interface type the inheritance depth is the length of the
   --  longest path in the inheritance tree from the root to a leaf, for any
   --  other type the inheritance depth is 0. The exception is a formal
   --  private type with no progenitor and a formal interface type with exactly
   --  one progenitor - for such types the inheritance depth is equal to the
   --  depth of their parent/progenitor type, because this is the minimal
   --  possible length of an actual type that may be used in instantiation.

   function Interface_List (Decl : Asis.Element) return Asis.Element_List;
   --  If May_Have_Interface_List (Decl), returns the list of names from the
   --  interface list. Otherwise raises Asis_Inappropriate_Element.

   function Primitive_Of_Type (Op : Asis.Element) return Asis.Element;
   pragma Obsolescent (Primitive_Of_Type);
   --  Providing that Op is a declaration of a dispatching operation of some
   --  tagged type, returns the declaration of this type. This function is
   --  equivalent to Asis.Extensions.Primitive_Owner (except that it goes one
   --  step up the ASIS tree - from type definition to type declaration) in
   --  case if the operand is an explicit declaration, but differs in case of
   --  implicit declarations. This function does not check its argument, the
   --  caller is responsible for providing only the subprogram declaration
   --  Elements that denote the dispatching primitive operations

   function Unwind_Exception_Renamings
     (Exc  : Asis.Element)
      return Asis.Element;
   --  Provided that Exc is a defining name denoting an exception, checks if
   --  this name is defined by exception renaming declaration, and if it is,
   --  unwinds the renamings and returns the corresponding defining name from
   --  the exception declaration. Otherwise returns the argument unchanged.

   function Ada_Attribute_Designator
     (Attr : Asis.Attribute_Kinds)
      return String;
   --  Returns the attribute designator corresponding to the ASIS
   --  Attribute_Kinds value. Raises Constraint_Error for
   --  An_Implementation_Defined_Attribute, An_Unknown_Attribute, and
   --  Not_An_Attribute argument. The result string is returned in proper case.

   function GNAT_Attribute_Designator
     (Attr : Snames.Attribute_Id)
      return String;
   --  Returns the attribute designator corresponding to the attribute
   --  corresponding to the actual parameter. The result string is returned in
   --  proper case.

   function Ada_Pragma_Identifier
     (Attr : Asis.Pragma_Kinds)
      return String;
   --  Returns the pragma name corresponding to the ASIS Pragma_Kinds value.
   --  Raises Constraint_Error for An_Implementation_Defined_Pragma,
   --  An_Unknown_Pragma, and Not_A_Pragma argument. The result string is
   --  returned in proper case.

   function GNAT_Pragma_Identifier
     (Attr : Snames.Pragma_Id)
      return String;
   --  Returns the pragma identifier corresponding to the pragma corresponding
   --  to the actual parameter. The result string is returned in proper case.

   function Full_Expanded_Name_Image (Name : Asis.Element) return Program_Text;
   --  Provided that Name is of A_Defining_Name kind (but not of
   --  A_Defining_Expanded_Name kind), returns it full expanded Ada name that
   --  starts from the defining name of an enclosing library unit. In case if
   --  the argument entity is declared in a nameless block statement the result
   --  contains an empty string as the name of this block (A.B..My_Name)

   --------------------
   -- Test functions --
   --------------------

   function Is_Access_Attribute (Attr : Asis.Element) return Boolean;
   --  Checks if the argument is an attribute reference that denotes an access
   --  attribute (that is, 'Access, 'Unchecked_Access or 'Unrestricted_Access).
   --
   --  Returns False for any unexpected element
   --
   --  Expected Expression_Kinds:
   --     An_Attribute_Reference

   function Is_Volatile (Def_Name : Asis.Element) return Boolean;
   --  Checks if Def_Name denotes a volatile data object. This function is
   --  supposed to be used for the gnatcheck rule Volatile_Requires_Addr_Clause
   --  only. At the moment it does not analyze the Ada code but checks the
   --  Treat_As_Volatile flag in the tree.
   --
   --  Returns False for any unexpected element
   --
   --  Expected Defining_Name_Kinds:
   --     A_Defining_Identifier

   function Is_Tagged (Dcl : Asis.Element) return Boolean;
   --  Checks if Dcl declares a tagged type. Returns False for any unexpected
   --  element.
   --
   --  Expected Element_Kinds:
   --     A_Declaration

   function Is_Modular_Type (Subtype_Ref : Asis.Element) return Boolean;
   --  Provided that Subtype_Ref is a subtype mark, check if it denotes a
   --  modular type. Returns False for any unexpected element
   --
   --  Expected Expression_Kinds:
   --       An_Identifier
   --       A_Selected_Component
   --       An_Attribute_Reference

   function Is_Volatile_Type (Subtype_Ref : Asis.Element) return Boolean;
   --  Provided that Subtype_Ref is a subtype mark, check if it denotes a
   --  volatile type. The notion of a volatile type depends on our
   --  interpretation of the Volatile_Requires_Addr_Clause rule. At the moment
   --  we do not consider atomic types as volatile types, we also do not
   --  take into account the effect of the Atomic_Components and
   --  Volatile_Components pragmas.
   --
   --  When iterating through the derivations chain looking for Volatile
   --  pragma, we do not consider that record extensions can be volatile
   --
   --  This function returns False for any unexpected element
   --
   --  Expected Expression_Kinds:
   --       An_Identifier
   --       A_Selected_Component
   --       An_Attribute_Reference

   function Is_Component (El : Asis.Element) return Boolean;
   --  Checks if El is a component (but not an expanded name of a whole)
   --  object. Returns False for any unexpected element,
   --
   --  Expected Expression_Kinds
   --     A_Selected_Component.

   function Is_Constructor_Function (El : Asis.Element) return Boolean;
   --  Checks if the argument is a declaration of a function that can be
   --  considered as f constructor function in a traditional OO model - that
   --  is, it is a primitive operation for some tagged type and it has a
   --  dispatching result but no dispatching operand. Returns False for any
   --  unexpected Element.
   --
   --  Expected  Declaration_Kinds:
   --     A_Function_Declaration
   --     A_Function_Body_Declaration
   --     An_Expression_Function_Declaration
   --     A_Function_Renaming_Declaration
   --     A_Function_Instantiation

   function Adds_New_Nesting_Level
     (El_Kind : Flat_Element_Kinds)
      return    Boolean;
   --  Checks if the element of the given kind adds a new level of construct
   --  nesting, according to the GNAT -gnatyLnnn option.

   function Can_Create_Return_Object (SM : Asis.Element) return Boolean;
   --  Provided that SM is a subtype mark represented by a simple name, checks
   --  if in a function body it is possible to create a local object of the
   --  denoted subtype and to return it as a function result. Currently we
   --  check that the type denoted by SM is not limited, and that SM does not
   --  denote an unconstrained array subtype or unconstrained subtype with
   --  no default discriminant values.

   function Can_Have_Dispatching_Operations
     (El   : Asis.Element)
      return Boolean;
   --  Checks if El is a type definition for that the corresponding type can
   --  have dispatching operations.
   --  The definition from the full declaration of a private type is never
   --  test True for this query, because private and full type declarations
   --  in the tree point to the same list of primitives.

   function Can_Have_Elaboration_Calls (El : Asis.Element) return Boolean;
   --  Checks if the argument ELement can issue a call as a part of its
   --  evaluation or elaboration. (For example, as a part of computing a
   --  default parameter or component value).

   function Does_Not_Add_New_Components (El : Asis.Element) return Boolean;
   --  Checks if the argument is a A_Derived_Record_Extension_Definition that
   --  does not add any new component to a type. Returns False for any
   --  unexpected element.
   --
   --  Expected Type_Kinds:
   --     A_Derived_Record_Extension_Definition

   function Contains_Raise_Stmt (C : Asis.Element) return Boolean;
   --  Checks if the argument construct contains a raise statement.

   function Defines_Predicate (A : Asis.Element) return Boolean;
   --  Checks if the argument is an aspect definition that defines a predicate
   --  (that is, Pre-condition, Post-condition or a type invariant). Returns
   --  False for any unexpected argument.
   --
   --  Expected Definition_Kinds:
   --     An_Aspect_Specification

   function Has_Pragma_Inline (Subpr : Element) return Boolean;
   --  Checks if Subpr is a (generic) subprogram declaration or a subprogram
   --  body to which the Inline pragma is applied, or a subprogram
   --  instantiation that is inlined because pragma Inline is applied to the
   --  corresponding --  template. Returns False for any unexpected element.
   --
   --  Expected Declaration_Kinds:
   --
   --    A_Procedure_Declaration
   --    A_Function_Declaration
   --    A_Generic_Procedure_Declaration
   --    A_Generic_Function_Declaration
   --    A_Procedure_Body_Declaration
   --    A_Function_Body_Declaration
   --    A_Procedure_Instantiation
   --    A_Function_Instantiation
   --
   --  ??? WHAT ABOUT SUBPROGRAM RENAMINGS???

   function Is_Call_To_Attribute_Subprogram
     (El   : Asis.Element)
      return Boolean;
   --  Checks if the argument element is a call to an attribute subprogram,
   --  this check is used if ASIS returns Nil_Element as the corresponding
   --  called subprogram

   function Is_Controlled (Type_Name : Asis.Element) return Boolean;
   --  Provided that  Type_Name is A_Defining_Identifier Element, checks if
   --  this is a defining name of a controlled type. This function is
   --  implemented as an extension query, it extracts the needed information
   --  directly from the tree.

   function Is_Controlling_Type_Operation (El : Asis.Element) return Boolean;
   --  Checks if El is an explicit declaration of a procedure that overrides
   --  Initialize, Adjust (for non-limited type) or Finalize operation of a
   --  controlled type.

   function Is_Derived_From
     (Descendant : Element;
      Ancestor   : Element)
      return       Boolean;
   pragma Obsolescent (Is_Derived_From);
   --  Checks if Descendant is the declaration of a type that is derived
   --  (directly or indirectly) from Ancestor (that is also supposed to be
   --  a type declaration Element). Returns False if any of arguments are not
   --  type declaration Elements or if they represent the same type.

   function Is_Executable_Body (El : Element) return Boolean;
   --  Checks if its argument represents the executable body, that is, the
   --  body for which it makes sense to compute the complexity metrics. At
   --  the moment, we compute complexity for subprogram, task and entry bodies.
   --  We do not compute the complexity for protected bodies (they just do not
   --  contain statements on their own). For a package body this function
   --  returns true only if the body contains statements of its own. An
   --  expression function is also considered as executable body, because it
   --  can be defined by a complex expression for that a complexity value can
   --  be computed.

   function Is_Handled
     (Exc  : Asis.Element;
      H    : Asis.Element)
      return Boolean;
   --  Provided that Exc is a defining name from an exception declaration, and
   --  H is of An_Exception_Handler type, checks if the exception is handled
   --  by the handler in such a way that no exception is raised as the result
   --  of handling

   function Is_Imported_Subprogram (El : Asis.Element) return Boolean;
   --  Checks if its argument is a subprogram declaration completed by the
   --  pragma Import (or by the obsolete pragma Interface) or an instantiation
   --  of a generic subprogram that is completed by the pragma Import (or
   --  Interface).
   --
   --  Returns False for any unexpected Element.
   --
   --  Expected Declaration_Kinds:
   --
   --    A_Procedure_Declaration
   --    A_Function_Declaration
   --    A_Procedure_Instantiation
   --    A_Function_Instantiation,

   function Is_In_Visible_Part
     (Decl  : Element;
      Scope : Element)
      return  Boolean;
   function Is_In_Private_Part
     (Decl  : Element;
      Scope : Element)
      return  Boolean;
   --  Check if Decl is declared in the visible (private) part of Scope.
   --  Currently this function is implemented only for the case when Scope is
   --  either a package or a generic package, it may be extended for task and
   --  protected definitions as scopes. Returns False for any unexpected
   --  elements. This function supposes that for both Decl and Scope
   --  Is_Part_Of_Instance and Is_Part_Of_Implicit is False, but it does not
   --  check this itself.
   --
   --  Expected Element_Kinds for Decl:
   --     A_Declaration
   --
   --  Expected Declaration_Kinds for Scope:
   --     A_Package_Declaration
   --     A_Generic_Package_Declaration

   function Is_Indefinite_Subtype (SM : Asis.Element) return Boolean;
   --  If SM is a subtype mark represented by a simple or expanded name, check
   --  that the denoted subtype is indefinite. Returns False for any unexpected
   --  element (that is, returns False for an attribute reference used as a
   --  subtype mark). A caller is responsible for the fact that SN indeed is a
   --  subtype mark

   function Is_Non_Structural_Statement
     (Stmt         : Element;
      Exit_Is_Goto : Boolean := True)
      return         Boolean;
   --  Check if this statement argument is a non-structural control statement.
   --  If Stmt is not a control statement (see the list of expected statement
   --  kinds), the result is always False.
   --
   --  A control statement is considered as non-structural, if it contains some
   --  statement which transfers the control outside this control statement,
   --  and this does not allow to treat this control statement as one-entry and
   --  one-exit control structure.
   --
   --  The following ways of transferring the control are possible:
   --  - goto statement (if it transfers the control outside the control
   --    statement in question)
   --  - return statement
   --  - raise statement
   --  - terminate alternative in a selective accept
   --  - exit statement is considered as (non-structural) control transfer if
   --    Exit_Is_Goto is set ON)
   --
   --  Different forms of a select statement are considered as non-structural
   --  only if they contain any of the transfer control statements listed above
   --  (that is, asynchronous select is not considered as non-structural
   --  statement on its own)
   --
   --  Expected statement kinds:
   --    An_If_Statement
   --    A_Case_Statement
   --    A_Loop_Statement
   --    A_While_Loop_Statement
   --    A_For_Loop_Statement
   --    A_Selective_Accept_Statement
   --    A_Timed_Entry_Call_Statement
   --    A_Conditional_Entry_Call_Statement
   --    An_Asynchronous_Select_Statement

   function Is_Program_Unit (El : Element) return Boolean;
   --  Checks if its argument represents a program unit for which it makes
   --  makes sense to compute some metrics. See the body for the details.
   --  Note, that renamings, instantiations and (generic) subprogram
   --  declarations are considered as program units only if they are library
   --  items.

   function Is_Publically_Accessible (Decl : Element) return Boolean;
   --  Checks if Decl is A_Declaration Element that can be accessed from
   --  another (non-private) compilation unit. That is, returns True a
   --  declaration located in the visible part of a library package or a
   --  library generic package, except for the case when the declaration is
   --  located in the private part of a local package. Returns False for any
   --  declaration located in a private library unit.
   --
   --  Returns False for any unexpected element.
   --
   --  Expected Element_Kinds:
   --     A_Declaration

   function Is_RM_Program_Unit (El : Element) return Boolean;
   --  Checks if the argument corresponds to the notion of a program unit as
   --  it is defined in RM95 10.1(1) ("A program unit is either a package,
   --  a task unit, a protected unit, a protected entry, a generic unit, or
   --  an explicitly declared subprogram other than an enumeration literal.")

   function Is_Static_Loop (Loop_Stmt : Element) return Boolean;
   --  Checks if its argument is a static for-loop (that is, the discrete
   --  subtype indication in the loop parameter specification is static).
   --  Returns False for any unexpected element

   function Is_Unchecked_Convertion_Instance (Decl : Element) return Boolean;
   --  Checks if the argument is the instantiation of Ada.Unchecked_Conversion.

   function May_Have_Interface_List (Decl : Element) return Boolean;
   --  Checks if Decl represents a declaration that may have an interface list
   --  as a component thereof. Returns False for any unexpected element.
   --
   --  Expected Declaration_Kinds:
   --     An_Ordinary_Type_Declaration
   --     A_Formal_Type_Declaration
   --     A_Private_Extension_Declaration
   --     A_Task_Type_Declaration
   --     A_Protected_Type_Declaration
   --     A_Single_Task_Declaration
   --     A_Single_Protected_Declaration

   function Belongs_To_Multiple_Inheritance (Decl : Element) return Boolean;
   pragma Obsolescent (Belongs_To_Multiple_Inheritance);
   --  Is needed for a temporary workaround in processing of dispatching
   --  operations in gnatelim. Assuming that Is_Dispatching_Operation (Decl),
   --  this function tests if the type for that Decl is a dispatching operation
   --  inherits from interfaces.

   function Is_Recursive_Component_Definition
     (Def  : Asis.Element)
      return Boolean;
   pragma Obsolescent (Is_Recursive_Component_Definition);
   --  Assuming that Def is of An_Anonymous_Access_To_Variable or
   --  An_Anonymous_Access_To_Constant kind, checks if it is a definition
   --  of a "recursive" record component, such as
   --
   --     type Rec is ... record
   --        ...
   --        Comp : access Rec;
   --        ...
   --     end record.
   --
   --  Returns False for any unexpected element.

   procedure Print_Tree_Sources;
   --  Prints out the list of Ada source files used to create the currently
   --  accessed tree. This procedure can only be called when an ASIS Context
   --  is open. The reason why this procedure is placed in this package is that
   --  this package has access to many GNAT and internal ASIS units.

   -----------------------------------------------------------------
   -- Detecting of mutual arrangement of elements and their spans --
   -----------------------------------------------------------------

   function In_Private_Part
     (Pack    : Asis.Element;
      Element : Asis.Element)
      return    Boolean;
   --  If for both arguments Is_Text_Available, if they are from the same
   --  compilation unit, if Pack is a (generic) package specification, returns
   --  True if is located in the private part of Pack. Returns False
   --  otherwise.

   function Before
     (First  : Asis.Element;
      Second : Asis.Element)
   return Boolean;
   --  Checks that for both arguments Is_Text_Available is True, that they are
   --  both from the same compilation unit, and First element precedes the
   --  Second element (without any overlap).

   function Before (First : Span; Second : Span) return Boolean;
   --  Checks that the First span precedes the Second span (with no overlap).
   --  Returns False if any of the actuals is Nil_Span

   function Inclides
     (Whole : Asis.Element;
      Part  : Asis.Element)
      return  Boolean;
   --  Checks that for both arguments Is_Text_Available is True, that they are
   --  both from the same compilation unit, and that Part is completely located
   --  inside of Whole.

   function Inclides (Whole : Span; Part : Span) return Boolean;
   --  Checks that the Part span is located completely inside the Whole span.
   --  Returns False if any of the actuals is Nil_Span

   Main_Done : Boolean := False;
   --  This is set True at the (successful) end of each main procedure. The
   --  purpose is so assertions in Finalize operations can tell whether the
   --  main procedure exited normally. See, for example,
   --  Generic_Formatted_Output.Finalize, which insists that when we reach the
   --  end of the main procedure, the indentation level should be zero. But if
   --  an exception propagates out of the main procedure, that's just a bug
   --  which should be reported normally.

end ASIS_UL.Utilities;
