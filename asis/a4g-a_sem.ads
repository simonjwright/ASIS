------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                             A 4 G . A _ S E M                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--            Copyright (C) 1995-2018, Free Software Foundation, Inc.       --
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
-- Sciences.  ASIS-for-GNAT  is  now  maintained  by  AdaCore               --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines needed for semantic queries from
--  more than one Asis package

with Asis;         use Asis;

with A4G.Int_Knds; use A4G.Int_Knds;

with Einfo;        use Einfo;
with Namet;        use Namet;
with Types;        use Types;

package A4G.A_Sem is

   --  All the routines defined in this package do not check their
   --  arguments - a caller is responsible for the proper use of these
   --  routines

   ---------------------------------------
   -- Routines working on ASIS Elements --
   ---------------------------------------

   function Belongs_To_Limited_View (Decl : Asis.Element) return Boolean;
   --  Checks if the argument (declaration) Element may belong to a limited
   --  view of some package, see RM 05 10.1.1 (12.1/2 ..12.5/2))

   function Limited_View_Kind
     (Decl : Asis.Element)
      return Internal_Element_Kinds;
   --  Provided that Belongs_To_Limited_View (Decl), returns the rind that
   --  Decl should have in limited view (actually, the result is the kind of
   --  the argument except in case of type declarations, when the type is
   --  converted to An_Incomplete_Type_Declaration or
   --  A_Tagged_Incomplete_Type_Declaration

   function Get_Corr_Called_Entity
     (Call : Asis.Element)
      return Asis.Declaration;
   --  This function incapsulates the common code from
   --  Asis.Expressions.Corresponding_Called_Function and
   --  Asis.Statements.Corresponding_Called_Entity.
   --  It gets the Ada construction which is either a procedure (entry)
   --  or a function call or a generalized indexing (Ada 2012) and returns the
   --  declaration of the called entity. This function does not check its
   --  argument to be an appropriate Element for any of these ASIS queries.

   function Is_Range_Memberchip_Test (E : Asis.Element) return Boolean;
   function Is_Type_Memberchip_Test  (E : Asis.Element) return Boolean;
   --  These two functions are used as a check for appropriate Element in two
   --  obsolescent queries from Asis.Expressions - Membership_Test_Range and
   --  Membership_Test_Subtype_Mark respectively. They assume that an argument
   --  Element represents a membreship check Element. They check if the
   --  argument represents the membership test that in old ASIS was classified
   --  as An_In_Range_Membership_Test .. A_Not_In_Range_Membership_Test or
   --  An_In_Type_Membership_Test .. A_Not_In_Type_Membership_Test
   --  respectively.

   function Is_Based_On_Same_Node (E : Asis.Element) return Boolean;
   --  Assuming that Is_Based_On_Same_Node (E) checks if E is based on the same
   --  tree node as the Element representing the corresponding equality
   --  operation. Returns False if Is_Based_On_Same_Node (E) is False.

   procedure Reset_For_Body
     (El        : in out Asis.Element;
      Body_Unit : Asis.Compilation_Unit);
   --  Provided that El is a declaration from the spec of a library package
   --  or a library generic package, this procedure resets El to Is_Identical
   --  Element, but obtained from the tree contained the body for this package.
   --  This body is represented by the Body_Unit parameter, we use it to avoid
   --  call to Asis.Compilation_Units.Corresponding_Body in the implementation
   --  of this function.

   ------------------------------------
   -- Routines working on tree nodes --
   ------------------------------------

   function Entity_Present (N : Node_Id) return Boolean;
   --  Differs from 'Present (Entity (N))' that in case if the check
   --  'Present (Entity (N))' does not point to an expression node as it
   --  happens for identifiers that identify aspects in aspect specifications.

   function Defined_In_Standard (N : Node_Id) return Boolean;
   --  checks if its argument is an identifier or an enumeration literal
   --  defined in the predefined Standard package

   function Char_Defined_In_Standard (N : Node_Id) return Boolean;
   --  Checks if its argument is a character literal defined in the
   --  predefined Standard package. Can be applied to reference nodes and
   --  entity nodes.

   function Char_Needs_Charcode (N : Node_Id) return Boolean;
   --  Checks if its argument is a character literal defined in the
   --  predefined Standard package or belonging to a type derived from a
   --  Standard character type. The corresponding ASIS element needs
   --  Character_Code value in its representation. The function can be applied
   --  to reference nodes and entity nodes.

   function Unwind_Renaming (Def_Name : Node_Id) return Node_Id;
   --  Supposing that Def_Name is the node representing some defining
   --  occurrence of some name, this function unwinds all the renamings
   --  (if any) and returns the node representing the defining
   --  name of the entity referenced by this name. If there is no
   --  declaration for a given entity (this is the case, when a name
   --  renames a subprogram-attribute) an Empty node is returned.
   --
   --  Note, that the node for renaming declaration may be rewritten,
   --  in particular, renaming of a subprogram-attribute is rewritten
   --  into a subprogram body

   procedure Set_Stub_For_Subunit_If_Any (Def_Name : in out Node_Id);
   --  If Def_Name is N_Defining_Identifier node which represents the
   --  subprogram defining identifier from the proper body of a subunit,
   --  it is reset to point to the corresponding N_Defining_Identifier
   --  node from the corresponding body stub, if this stub acts as spec,
   --  or to the N_Defining_Identifier node from the corresponding
   --  subprogram declaration. Otherwise the argument remains unchanged.

   function Corr_Decl_For_Stub (Stub_Node : Node_Id) return Node_Id;
   --  This function should be called only for N_Subprogram_Body_Stub
   --  nodes. If the corresponding subprogram body stub is a completion
   --  of some subprogram declaration, the functions returns the node
   --  representing this subprogram declaration, otherwise it returns
   --  the Empty node.

   function Is_Anonymous (E : Entity_Kind) return Boolean;
   --  Check if E corresponds to an anonymous access type or subtype.

   function Is_Predefined (Def_Op : Node_Id) return Boolean;
   --  Returns True if Def_Op is N_Defining_Operator_Symbol representing
   --  a predefined operation. Returns False otherwise.
   --  ??? May be, there is something like this in GNAT???

   function Is_Impl_Neq (Def_Op : Entity_Id) return Boolean;
   --  Checks if the argument if the entity of implicit "/=" that is defined
   --  for explicit user-defined "="

   function Is_From_Instance (Node : Node_Id) return Boolean;
   --  Checks if Node is from expanded generic template

   function Is_From_Rewritten_Aggregate (Node : Node_Id) return Boolean;
   --  Checks if Node is an N_Component_Association node belonging to a
   --  rewritten tree structure corresponding to some aggregate. Returns False
   --  if Node is not of N_Component_Association kind.

   function Is_Name_Of_Expanded_Subprogram (Node : Node_Id) return Boolean;
   --  Detects if the argument is a defining name from an expanded subprogram
   --  instantiation, In this case the front-end creates an artificial
   --  defining identifier node that is not Comes_From_Source, but that also
   --  does not have an instantiation chain in Sloc, so ASIS can get confused
   --  with this node and treat is as an implicit node if apply the usual
   --  tests to it. (See G312-006).

   function Is_From_Unknown_Pragma (Node : Node_Id) return Boolean;
   --  Checks if Node belongs to a subtree rooted  by unknown pragma. The tree
   --  structures for unknown pragmas are very poorly decorated, so semantic
   --  queries may just blow up when applied to elements representing
   --  components of such pragmas.

   function Get_Actual_Type_Name (Type_Mark_Node : Node_Id) return Node_Id;
   --  This function supposes, that its argument is of N_Identifier kind.
   --  When applied to a reference to an implicit subtype created in
   --  expanded generic instantiation as a way to pass the actual type,
   --  this function "unwinds" this implicit subtyping and returns the
   --  reference to the actual type. Otherwise it returns its argument
   --  unchanged.
   --  The case when the actual type is a derived type is treated specially -
   --  in this case "unwinding" could bring the internal type created by the
   --  front-end, so we break this unwinding and return the entity (!!!) node
   --  of the corresponding actual type.

   function Get_Instance_Name (Int_Name : Node_Id) return Node_Id;
   --  For Int_Node which should be Is_Generic_Instance (otherwise it is an
   --  error to use this function) and which denotes the entity declared in
   --  an artificial package created by the compiler for a generic
   --  instantiation, it finds an entity defined in a generic instantiation

   function Get_Derived_Type
     (Type_Entity     : Entity_Id;
      Inherited_Subpr : Entity_Id)
      return            Entity_Id;
   --  This function supposes that Type_Entity is a type entity,
   --  and Inherited_Subpr is the defining name of implicit inherited
   --  subprogram. It checks if Type_Entity is an ancestor type for the
   --  derived type which inherits Inherited_Subpr, and if it is, returns
   --  the entity of the derived type, otherwise returns Type_Entity

   function Is_Derived_Rep_Item
     (Type_Entity : Entity_Id;
      Rep_Item :    Node_Id)
      return        Boolean;
   --  Supposing that Type_Entity is an entity of some type, and Rep_Item
   --  represents  some representation item from the chain of representation
   --  items associated with this type, this function checks it Type_Entity
   --  derives this Rep_Item from some of its parent types.

   function Is_Artificial_Protected_Op_Item_Spec
     (E :    Entity_Id)
      return Boolean;
   --  Checks if E represents the entity from the artificial subprogram spec
   --  created by the compiler for some protected_operation_item which does not
   --  have a separate spec in the source code.
   --  Note that this function check protected operation entity and entities of
   --  its formal parameters (At some point we should rename it as
   --  Is_FROM_Artificial_Protected_Op_Item_Spec)

   function Represents_Class_Wide_Type_In_Instance
     (N    : Node_Id)
      return Boolean;
   --  Test function used to check if from the ASIS viewpoint the argument may
   --  represent the 'Class attribute reference corresponding to the actual
   --  class-wide type in the instantiation (see F410-011 for full details)

   function Represents_Base_Type_In_Instance (N : Node_Id) return Boolean;
   --  Test function used to check if from the ASIS viewpoint the argument may
   --  represent the 'Base attribute reference corresponding to the actual
   --  type in the instantiation.

   function Pass_Generic_Actual (N : Node_Id) return Boolean;
   --  Checks if N represents an artificial (created by the front-end)
   --  declaration used to pass the actual in the instantiation. The problem
   --  here is that for such declarations the Sloc field not always (and not
   --  for all of their subcomponents) points to the instantiation chain.

   function Part_Of_Pass_Generic_Actual (N : Node_Id) return Boolean;
   --  This function checks if its argument is a subcomponent of the construct
   --  for that Pass_Generic_Actual returns True. The only reason to have these
   --  two function instead of just this one is performance.

   function Explicit_Parent_Subprogram (E : Entity_Id) return Entity_Id;
   --  Provided that E points to an inherited subprogram, this function
   --  computes the entity of the corresponding explicitly defined parent
   --  subprogram.

   function Get_Importing_Pragma (E : Entity_Id) return Node_Id;
   --  Supposing that E is an Is_Imported Entity node, compute the
   --  corresponding aspect or pragma that imports it (it can be Import or
   --- Interface or CPP_Constructor pragma). The name of the subprogram has
   --  been chosen before we get aspects in Ada 2012.

   function Is_Applied_To
     (Pragma_Node : Node_Id;
      Entity_Node : Entity_Id)
      return        Boolean;
   --  Supposing that Pragma_Node denotes a pragma, and Entity_Node is an
   --  entity node (the caller is responsible for this), checks if the pragma
   --  is applied to the entity.

   function Is_Implicit_Null_Procedure (N : Node_Id) return Boolean;
   --  Checks if N tepresents a declaration of an implicit null procedure used
   --  as an actual for a formal subprogram with null default.

   function Not_Overriden_By_Explicit (E : Entity_Id) return Boolean;
   --  Assumes that E is an entity node that corresponds to an implicit
   --  inherited operation. Checks if it is not overridden by an explicit
   --  one. This function is needed because the list of entities that follows
   --  the entity of a derived type may contain both implicit inherited entity
   --  and explicit entity that overrides it.

--   function Is_Redefined_For_Full_View
--     (E       : Entity_Id;
--      Alias_E : Entity_Id)
--      return    Boolean;
   --  E is supposed to be an entity denoting an implicit subprogram inherited
   --  by a type extension or a derived type, Alias_E is supposed to be a
   --  subprogram entity that ocerrides E (the caller is respobsible for
   --  ensuring that this is the case). This function checks if we have the
   --  following situation:
   --
   --  * E is inherited by a private extension;
   --
   --  * Alias_E is defiibed for/inherited by a full view

   function Patched_Comes_From_Source (N : Node_Id) return Boolean;
   --  Temporary solution for NA29-045. Differs from Atree.Comes_From_Source
   --  in that it returns True for defining names of formal subprograms from
   --  expanded instantiations that correspond to formal packages

   function Is_Rewritten_SPARK_Construct (N : Node_Id) return Boolean;
   --  Checks if N represents a SPARK-specific construct that is rewritten and
   --  for that the original structure is not fully attributed, so for this
   --  construct the corresponding ASIS Element should use the rewritten tree.

   function Type_Def_in_Standard (E : Entity_Id) return Boolean;
   --  Checks if the argument is an entity type node that defines a standard
   --  type in package Standard, this node has Parent set to
   --  N_Full_Type_Declaration node. The problem here is that Etype chain for
   --  some standard types ends up with a type entity node that has no Parent
   --  node and this function allows to stop at the right entity.

   function Is_Root_Standard_Type (E : Entity_Id) return Boolean;
   --  Checks if the argument is an entity type node that defines a standard
   --  type in package Standard and this node has Parent set to Empty.
   --  The problem here is that some type Entities may have Etype field set
   --  to such a type entity and this function allows to stop at the right
   --  entity.

   function Is_From_Universal_Expression (N : Node_Id) return Boolean;
   --  Assuming that N represents an operation, checks if this operation is
   --  from a universal expression (and, therefore, the corresponding tree
   --  structures are not attributed). At the moment the check is somewhat too
   --  simple-minded, this may need revision.

   function Is_From_SPARK_Aspect (N : Node_Id) return Boolean;
   --  Checks if the argument is from the aspect specification that is specific
   --  for SPARK 2014. The code in such aspect specifications is an Ada
   --  extension, it does not follow the language visibility rules, so it may
   --  contain names that have no definition and other things that make
   --  problems for ASIS.

   function Is_Class_Wide_Clone (E : Entity_Id) return Boolean;
   --  Checks if E is a subprogram entity that has been artificially built
   --  for internal compiler needs. We need this function here because it
   --  seems that Einfo.Is_Class_Wide_Clone does not work properly in all
   --  cases needed for ASIS.

   function Get_Original_For_Class_Wide_Clone
     (E    : Entity_Id)
      return Entity_Id;
   --  If Is_Class_Wide_Clone (E), tries to locate the subprogram entity for
   --  that E has been created as class-wide clone. Returns Empty if this
   --  attempt is not successful. If not Is_Class_Wide_Clone (E) then returns
   --  E.

   function Get_Orig_Body_For_Class_Wide_Clode
     (N    : Node_Id)
      return Node_Id;
   --  N is of N_Subprogram_Body kind and this is the body of class-wide
   --  clone, returns the N_Subprogram_Body node representing the body of the
   --  subprogram for that the class-wide clone has been created. Otherwise
   --  returns the argument unchanged.

   --------------------------------------------------------------------
   -- Queies for processing arguments of SPARK-specific pragmas that --
   -- crerate artificial aggregates as their arguments               --
   --------------------------------------------------------------------

   function Is_SPARK_Pragma (Pragma_Name : Name_Id) return Boolean;
   --  Checks if the argument denotes a SPARK-specific pragma for that the
   --  compiler can do "artificial upgrade" of its single argument to
   --  aggregate structure.

   function Artificial_Aggregate_For_SPARK_Pragma (N : Node_Id) return Boolean;
   --  Assuming that N is of N_Pragma_Argument_Association kind and that is
   --  from a pragma that Is_SPARK_Pragma, checks if this association actually
   --  is an artificial aggregate created for a single identifier pragma
   --  parameter.

end A4G.A_Sem;
