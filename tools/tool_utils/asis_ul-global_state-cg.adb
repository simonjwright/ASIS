------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              A S I S _ U L . G L O B A L _ S T A T E . C G               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2007-2018, AdaCore                      --
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

with GNAT.Directory_Operations;      use GNAT.Directory_Operations;
with GNAT.OS_Lib;                    use GNAT.OS_Lib;

with Asis;                           use Asis;
with Asis.Clauses;                   use Asis.Clauses;
with Asis.Compilation_Units;         use Asis.Compilation_Units;
with Asis.Declarations;              use Asis.Declarations;
with Asis.Definitions;               use Asis.Definitions;
with Asis.Elements;                  use Asis.Elements;
with Asis.Expressions;               use Asis.Expressions;
with Asis.Extensions;                use Asis.Extensions;
with Asis.Extensions.Flat_Kinds;     use Asis.Extensions.Flat_Kinds;
with Asis.Iterator;                  use Asis.Iterator;
with Asis.Statements;                use Asis.Statements;

with Asis.Set_Get;                   use Asis.Set_Get;

with ASIS_UL.Common;
with ASIS_UL.Global_State.CG.Conditions;
use ASIS_UL.Global_State.CG.Conditions;
with ASIS_UL.Options;                use ASIS_UL.Options;
with ASIS_UL.Output;                 use ASIS_UL.Output;
with ASIS_UL.Utilities;              use ASIS_UL.Utilities;

with ASIS_UL.Global_State.Utilities; use ASIS_UL.Global_State.Utilities;
with ASIS_UL.Global_State.Data;      use ASIS_UL.Global_State.Data;

package body ASIS_UL.Global_State.CG is

   ----------------------------------------------------------------
   -- Processing of dispatching operations and dispatching calls --
   ----------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Existing approach is not 100% correct and does not work for the case --
   --  of multiple inheritance:    ???                                      --
   ---------------------------------------------------------------------------

   --  For a dispatching call, the link to the corresponding dispatching
   --  operation (RM05 3.9.2 (1/2)) is stored as an ordinary call.
   --
   --  For each dispatching operation, if the operation overrides some other
   --  (dispatching) operation, a call from overriden operation to the
   --  overriding one is stored. The call graph stores only explicetely (???)
   --  declared entities, so if P(1) is an eplicitely declared dispatching
   --  operation, P(2) is the corresponding implicetely declared inherited
   --  operation, and P(3) is overriding dispatching operation that actually
   --  overrides what could be inherited from P(2), then the link (the call)
   --  from P(1) to P(3) will be stored.

   -------------------------
   --  To be implemented: --
   -------------------------

   --  ?????

   --  Dispatching operations and dispatching calls make the following problem
   --  for the call graph:
   --
   --  * At the place of a dispatching call, any of the operations that
   --    overrides the given operation can be called, BUT:
   --
   --  * when processing a dispatching call, we do not have a full set of
   --    operations to that the call can be dispatched
   --
   --  * when processing a dispatching operation that overrides some other
   --    dispatching operation, we do not know if the overridden operation is
   --    a root of some dispatching call. An implicit inherited operation is
   --    not stored in the call graph if it is not a root of some dispatching
   --    call (???);
   --
   --  The following way of representing dispatching operations in the call
   --  graph is suggested
   --
   --  * all the dispatching operations are stored in the call graph (including
   --    implicit inherited operations and abstract operations);
   --
   --  * for each dispatching operations, a list of corresponding operations
   --    of the types directly derived from the type that "owns" this operation
   --    is stored (corresponding operation here is either the inherited
   --    operation corresponding to this operation, or an explicitly declared
   --    operatation that overrides it. For this list we will be using the
   --    term "operations implementing the given dispatching operation". Term
   --    is not really good, but we try to express the following: if
   --    implemented operation is a dispatching root, then implementing
   --    operation can be called as the result of dispatching.
   --
   --  * for multiple inheritance,one operation can be implementing operation
   --    for more than one "parent" implemented operations;
   --
   --  * dispatching calls are stored as separate lists. For each dispatching
   --    call, the corresponding dispatching operation is stored in the list
   --    of dispatching calls. If dispatching operation is not abstract
   --    operation, the dispatching call is stored as an ordinary
   --    non-dispatching call in the list of direct calls (that is, the result
   --    of node representing the result of Corresponding_Element applied to
   --    the dispatching operation is stored;
   --
   --  * call graph transitive closure is performed in two steps:
   --
   --   1. for each dispatching operation, the list of all the operations that
   --      implement the given dispatching operation, directly or indirectly,
   --      is created;
   --
   --   2. for each node N, for each node M from the list of dispatching calls
   --      issued by N, the list of
   --      operations implementing the corresponding dispatching operation M
   --      (that is, the list of operations to that the call can be dispatched)
   --      is added to the list of direct calls issued by N
   --
   --   3. Normal transitive closure of the call graph is performed.
   --
   --  ???

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Process_Call
     (Element : Asis.Element;
      At_SLOC : String_Loc := Nil_String_Loc);
   --  Analyzes a subprogram call. If the call cannot be statically analyzed,
   --  ???????????????????????
   --  generates the corresponding diagnostic message in case if ??? is ON.
   --  IF At_SLOC is equal to Nil_String_Loc, the SLOC of the call is the SLOC
   --  of the argument Element, otherwise At_SLOC is used as the SLOC of the
   --  call (see the documentation for Add_CG_Info).

   procedure Process_Callable_Entity (El : Asis.Element);
   --  Stores (if needed) in the call graph the information about the
   --  callable entity. In case of a single task declaration, this procedure
   --  also stores the call link from the current scope to the task entity

   procedure Process_Elaboration_Calls (Element : Asis.Element);
   --  For the argument Element that should be
   --  ASIS_UL.Utilities.May_Contain_Elaboration_Calls, tries to find implicit
   --  calls that are made during the elaboration and for each of these calls
   --  processes this call as a regular call.

   procedure Process_Type_Default_Expressions
     (Type_To_Analyze : Asis.Element;
      Call_At_SLOC    : String_Loc);
   --  Implements a part of the functionality of Process_Elaboration_Calls
   --  Recursively traverses the type structure of the type represented by
   --  Type_To_Analyze argument (note that this type should not be private or
   --  derived type!) and adds all the fucntion calls from the component
   --  expressions in the call graph. At_SLOC parameter represents the location
   --  of the calls to store (because these calls are issued as a part of
   --  object declaration elaboration declaration or allocator evaluation).

   procedure Process_Init_Expressions_In_Record_Components
     (Component_List : Asis.Element_List;
      Call_At_SLOC   : String_Loc);
   --  Implements a part of the functionality of
   --  Process_Type_Default_Expressions. Traverses the argument list and do
   --  the following:
   --
   --  - if a list element is a component definition and it contains an
   --    initialization expression, traverses this expression to locate
   --    function calls;
   --
   --  - if a list element is a component definition and it does not contain an
   --    initialization expression, analyzes the component type to get
   --    initialization expressions for suncomponents and to extract function
   --    calls from them;
   --
   --  - if a list elemen is a variant part, recursively gets into the variant
   --    part strcture to get and to analyze the variant components;
   --
   --  For errey compoenets, the component type is analyzed for possible
   --  default initialization expressions.

   procedure Process_Renaming_As_Body (El : Asis.Element);
   --  If we have renaming-as-body, this means that we have the corresponding
   --  subprigram declaration, so - the corresponding node in the call graph.
   --  This subprogram detects (and creates, if needed) the corresponding node
   --  in the call graph and sets for this node Is_Renaming ON. Then in tries
   --  to unwind the renaming, and if the renamed entity can be statically
   --  defined, stores the ID of this entity in the Calls_Chain for the node.
   --  (That is, if we have a subprogram that has renaming-as-body as its
   --  completion, we represent this in the call graph as if this subprogram
   --  calls the renamed subprogram. The case of renaming a task entry as a
   --  subprogram is not implemented yet.)

   procedure Process_Task_Components
     (Type_Decl      : Asis.Element;
      Call_At_SLOC   : String_Loc;
      Recursive_Call : Boolean := False);
   --  Analyze the argument type declaration and defines the tasks that are
   --  created when creating the value of this type. It is supposed that
   --  Get_Type_Structure function has already been applied to the argument
   --  type declaration. The actual for Call_At_SLOC should indicate the source
   --  location of the construct that initiate task creations (e.g. SLOC of an
   --  object declaration that contains task components).
   --
   --  The problem of this procedure is that it can get into cycles in case of
   --  recursive record types. Two cases of recursion are possible:
   --
   --     type Rec_1 is record
   --        ...
   --        C : access Rec_1;
   --        ...
   --     end record
   --
   --  and
   --
   --     type Rec_2;
   --     type Rec_3;
   --
   --     type Access_Rec_2 is access Rec_2
   --     type Access_Rec_3 is access Rec_3
   --
   --     type Rec_2 is record
   --        C : Access_Rec_3;
   --        ....
   --     end record;
   --
   --     type Rec_3 is record
   --        C : Access_Rec_2;
   --        ....
   --     end record;
   --
   --  The break this cycling, Recursive_Call parameter is used. When this
   --  parameter is False, the query starts from cleaning the set of processed
   --  types, otherwise it does not do this. Before strarting processing a type
   --  declaration, the qery checks if it already stored in the set of
   --  processed types, and if it is, skips this type.
   --
   --  All the calls to this query from the code that builds the call graph
   --  should be with Recursive_Call => False to avoid cycling

   Processed_Types : Asis.Extensions.Element_Containers.Set;

   procedure Process_Record_Task_Components
     (Component_List : Asis.Element_List;
      Call_At_SLOC   : String_Loc);
   --  Similar to the Process_Task_Components procedure, but works on a list of
   --  record components (more exactly, on the list returned by the
   --  Asis.Definitions.Record_Components query.

   procedure Process_Task_Creation (El : Asis.Element);
   --  Supposing that Can_Create_Tasks (El), recursively traverse the type
   --  declaration of the object or value representing by El and stores the
   --  information about all the tasks (if any) that are created when the
   --  object/value is cretaed.
   --  (Suppose we have:
   --
   --     task type T is ,.. end T;
   --     type Rec is record
   --         Comp_I : Integer;
   --         Comp_T : T;
   --     end record;
   --
   --     Var : Rec; --  here a task of type T is created,
   --
   --  This procedure should get from the declaration of Var the information
   --  that a task of the type T is created as a result of elaboration this
   --  declaration.

   procedure Process_Stream_Attribute_Redefinition
     (Element : Asis.Element;
      At_SLOC : String_Loc);
   --  Assuming that El is an attribute definition clause that redefines a
   --  stream attribute, tries to define the procedure used for the
   --  redefinition and if it is possible, creates a link that represents the
   --  call to this procedure from the current scope. (The redefined attribute
   --  can be used only within the current scope).

   procedure Process_Reference_To_Subprogram
     (Element : Asis.Element;
      At_SLOC : String_Loc);
   --  Assuming that El is a construct that can create a reference to a
   --  subprogram that can be used for indirect subprogram call, tries to
   --  define the refered subprogram and if it is indeed a subprogram, creates
   --  a link that represents the call to this subprogram from its enclosing
   --  scope.
   --
   --  !!! Note, that there are also references to tasks and to entries, and we
   --  do not process these cases at the moment!

   procedure Process_Discr_Init_Proc (El : Asis.Element);
   --  Provided that Has_Discr_Init_Proc (El) is True, creates the
   --  representation of the discriminant initialization procedure for this
   --  type. This includes storing the information about all the (direct) calls
   --  issuing by this initialization procedure.

   procedure Process_Type_Init_Proc (El : Asis.Element);
   --  Provided that Has_Type_Init_Proc (El) is True, creates the
   --  representation of the component initialization procedure for this type.
   --  This includes storing the information about all the (direct) calls
   --  issuing by this initialization procedure.

   procedure Process_Scope (El : Asis.Element);
   --  Stores in the call graph the information about the scope (that is -
   --  about the body of a callable entity) and updates Current_Scope and
   --  the scope stack.

   procedure Store_Dispatching_Operations (El : Asis.Element);
   --  Provided that El is a type definition that may have dispatching
   --  operations, stores all the dispatching operations in the call graph.

   procedure Store_Arc
     (Called_Entity  : Asis.Element;
      At_SLOC        : String_Loc;
      Calling_Entity : Asis.Element := Nil_Element);
   --  Supposing that Called_Entity is an Element that can be stored as a node
   --  of the Call Graph (that is, Corresponding_Element has already been
   --  applied to it), stores the call arc from Calling_Entity (or from the
   --  current scope if Calling_Entity is Nil_Element) to the node
   --  corresponding to this element using At_SLOC as the SLOC of the place
   --  where the call takes place. Only one (the first) call from the scope to
   --  the given Element is stored.

   procedure Store_Dispatching_Call
     (Called_Entity  : Asis.Element;
      At_SLOC        : String_Loc;
      Calling_Entity : Asis.Element := Nil_Element);
   --  Similar to Store_Arc, but stores the arc not to the list of direct
   --  calls, but to the list of dispatching calls.

   procedure Set_Is_Renaming (N : GS_Node_Id; Val : Boolean := True);
   --  Set the flag indicating if the callable entity is a renaming of another
   --  callable entity (only renamings-as-bodies are represented in the call
   --  graph),

   function First_Direct_Call (N : GS_Node_Id) return GS_Node_Id;
   --  Returns the first node from the direct call list of N. Returns
   --  No_GS_Node if the list of direcr calls for N is empty

   procedure Traverse_Construct_For_CG_Info is new Traverse_Element
     (State_Information => String_Loc,
      Pre_Operation     => Add_CG_Info_Pre_Op,
      Post_Operation    => Complete_CG_Info_Post_Op);
   --  Traverses the argument Element in ordrer to collect call graph
   --  information. Usded as internal traversal routine for the implementation

   procedure Unconditionally_Traverse_Construct_For_CG_Info is new
     Traverse_Element (State_Information => String_Loc,
                       Pre_Operation     => Unconditionally_Add_CG_Info_Pre_Op,
                       Post_Operation    => Complete_CG_Info_Post_Op);
   --  Traverses the argument Element in ordrer to collect call graph
   --  information. Usded as internal traversal routine for the implementation
   --  of Collect_CG_Info_From_Construct.
   --  of Collect_CG_Info_From_Construct.

   ---------------------------------------------------
   --  Dispatching calls and dispatching operations --
   ---------------------------------------------------

   procedure Add_Possible_Calls
     (Calling_Node   : GS_Node_Id;
      Disp_Operation : GS_Node_Id);
   --  This procedure assumes that Calling_Node issues a dispatching call and
   --  this call is dispatched to Disp_Operation. It adds all the subprograms
   --  that can be called as the result of dispatching call to Disp_Operation
   --  to the list of direct calls of Calling_Node (using placeholder SLOC
   --  (0, 0))

   ------------------------------------------------------------
   -- Data structures used for call graph transitive closure --
   ------------------------------------------------------------

   --  The following variables are used by Close_Node procedure, we define them
   --  as global to avoid elaboration expances for each call of Close_Node.

   New_Set   : Node_Lists.Set;
   --  A set of nodes that are added to All_Call. For each of the nodes from
   --  this set we should analyse its direct calls and then remove the node
   --  fron this set. We stop the loop for the next node when this set is
   --  empty,

   Newer_Set : Node_Lists.Set;
   --  Nodes that are added for All_Call at the last iteration of the
   --  processing of New_Set for the given node. They should be added to
   --  New_Set to process their direct calls.

   Next_Direct_Call : Node_Lists.Cursor;
   Next_Call        : SLOC_Node_Lists.Cursor;
   Next_All_Call    : Node_Lists.Cursor;
   Next_Ref         : SLOC_Node_Lists.Cursor;
   Link_Tmp         : SLOC_Link;

   Traverse_Renamings_Done_Flag : Boolean := False;
   Transitive_Closure_Done_Flag : Boolean := False;
   --  Flags that indicates that the corresponding operation has been done

   --  !!!! Start of the junc patch code to be removed as soon as possible!
   --  See I106-005
   procedure Patch_For_Default_Parameter_Initialization
     (Element : Asis.Element);
   pragma Unreferenced (Patch_For_Default_Parameter_Initialization);
   --  This is a temporary patch for the compiler problem described in
   --  I106-005: if Element is a parameter specification from a subprogram
   --  or an entry, then all the function called in the default initialization
   --  expressions (if any) are unconditionally marked as used.

   procedure Mark_Called_Function_Used
     (Element : Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State);
   --  If Element is a function call, tries to define the called function and
   --  mark it as used.

   procedure Mark_All_Called_Functions_Used is new Traverse_Element
      (Pre_Operation     => Mark_Called_Function_Used,
       Post_Operation    => No_Op,
       State_Information => No_State);
   --  !!!! End of the junc patch code to be removed as soon as possible!

   -----------------
   -- Add_CG_Info --
   -----------------

   procedure Add_CG_Info
     (Element : Asis.Element;
      At_SLOC : String_Loc := Nil_String_Loc)
   is
   begin

      if Can_Have_Elaboration_Calls (Element) then
         --  Is_Call and Can_Create_Tasks Elements can have elaboration calls,
         --  so we have to process elaboration calls in a separate IF
         --  statement.
         Process_Elaboration_Calls (Element);
      end if;

      if Is_Scope (Element) then
         Process_Scope (Element);
      elsif Is_Declaration_Of_Callable_Entity (Element) then
         Process_Callable_Entity (Element);

      elsif Asis.Extensions.Is_Renaming_As_Body (Element) then
         Process_Renaming_As_Body (Element);
         --  At the moment, we just unwind renamings to the called subprogram

      elsif Is_Call (Element) then
         Process_Call (Element, At_SLOC => At_SLOC);
      elsif Can_Create_Tasks (Element) then
         Process_Task_Creation (Element);
      elsif Is_Stream_Attribute_Redefinition (Element) then
         Process_Stream_Attribute_Redefinition (Element, At_SLOC);
      elsif Can_Create_Reference_To_Subprogram (Element) then
         Process_Reference_To_Subprogram (Element, At_SLOC);
      elsif Represent_Dispatching_Calls
        and then
           Can_Have_Dispatching_Operations (Element)
      then
         Store_Dispatching_Operations (Element);
      end if;

      if Has_Type_Init_Proc (Element) then
         Process_Type_Init_Proc (Element);
      end if;

      if Has_Discr_Init_Proc (Element) then
         Process_Discr_Init_Proc (Element);
      end if;
   end Add_CG_Info;

   ------------------------
   -- Add_CG_Info_Pre_Op --
   ------------------------

   Definition                    : Asis.Element;
   Is_Global_Reference           : Boolean;
   Can_Be_Accessed_By_Local_Task : Boolean;
   Reference_Kind                : Reference_Kinds;
   --  We define these variables as global for Pre_Operation because of
   --  performance reasons (to awoind their allocation for each identifier
   --  element being visited during traversal)

   procedure Add_CG_Info_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out String_Loc)
   is
      Expanded_Code : Asis.Element;

      procedure Treat_Element (Element : Asis.Element);

      procedure Treat_Element (Element : Asis.Element) is
      begin
         if (Flat_Element_Kind (Element) = A_Defining_Identifier and then
               (Flat_Element_Kind (Enclosing_Element (Element)) =
                  A_Variable_Declaration or else
                Flat_Element_Kind (Enclosing_Element (Element)) =
                  A_Formal_Object_Declaration))
         --  Possible initialization in the declaration of a package-level
         --  global variable, which counts as a write
           or else
           Flat_Element_Kind (Element) = An_Identifier
         --  Possible read or write to a variable
         then

            Check_If_Global_Reference
              (Element                       => Element,
               Definition                    => Definition,
               Is_Global_Reference           => Is_Global_Reference,
               Can_Be_Accessed_By_Local_Task =>
                 Can_Be_Accessed_By_Local_Task,
               Reference_Kind                => Reference_Kind,
               Compute_Reference_Kind        => True);

            if Is_Global_Reference and then
              Reference_Kind /= Not_A_Reference
            then
               Process_Global_Reference
                 (Element,
                  Definition,
                  Reference_Kind);
            end if;

         end if;
      end Treat_Element;
   begin
      --  !!!! To be removed as soon as possible! See I106-005
      --  Patch_For_Default_Parameter_Initialization (Element);

      if not Compute_Global_Objects_Accessed and then
        Is_Non_Executable_Construct (Element)
      then
         Control := Abandon_Children;
         return;
      end if;

      Add_CG_Info (Element, State);

      if Compute_Global_Objects_Accessed then
         if Flat_Element_Kind (Element) = A_Parameter_Association then
            Traverse_Construct_For_CG_Info
              (Element => Actual_Parameter (Element),
               Control => Control,
               State   => State);
            --  Avoid traversing the formal parameter of an association
            Control := Abandon_Children;
         end if;

         Treat_Element (Element);
      end if;

      if Declaration_Kind (Element) in
           A_Package_Instantiation .. A_Function_Instantiation
      then
         Expanded_Code := Corresponding_Declaration (Element);

         Traverse_Construct_For_CG_Info
           (Element => Expanded_Code,
            Control => Control,
            State   => State);

         Expanded_Code := Corresponding_Body (Element);

         if not Is_Nil (Expanded_Code) then
            Traverse_Construct_For_CG_Info
              (Element => Expanded_Code,
               Control => Control,
               State   => State);
         end if;

      end if;

   exception
      when Ex : others =>
         ASIS_UL.Common.Tool_Failures := ASIS_UL.Common.Tool_Failures + 1;

         ASIS_UL.Output.Error ("call graph info collection failed");
         ASIS_UL.Output.Error (Build_GNAT_Location (Element));
         ASIS_UL.Output.Report_Unhandled_Exception (Ex);

   end Add_CG_Info_Pre_Op;

   ------------------------
   -- Add_Possible_Calls --
   ------------------------

   procedure Add_Possible_Calls
     (Calling_Node   : GS_Node_Id;
      Disp_Operation : GS_Node_Id)
   is
      Next_Impl_Subpr : Node_Lists.Cursor;
      Next_Impl_Node  : GS_Node_Id;
   begin
      Next_Impl_Subpr := Node_Lists.First (Table (Disp_Operation).Node_List_3);

      while Node_Lists.Has_Element (Next_Impl_Subpr) loop
         Next_Impl_Node := Node_Lists.Element (Next_Impl_Subpr);

         Add_Link_To_SLOC_List
           (To_Node     => Calling_Node,
            Link_To_Add => (Next_Impl_Node, Nil_String_Loc));

         Next_Impl_Subpr := Node_Lists.Next  (Next_Impl_Subpr);
      end loop;

   end Add_Possible_Calls;

   -------------------
   -- Body_Analyzed --
   -------------------

   function Body_Analyzed (N : GS_Node_Id) return Boolean is
   begin
      pragma Assert (GS_Node_Kind (N) in  Callable_Nodes);
      return Table (N).Bool_Flag_1;
   end Body_Analyzed;

   -----------------------------------
   -- Check_Call_Graph_Completeness --
   -----------------------------------

   procedure Check_Call_Graph_Completeness is
   begin

      for Node in First_GS_Node .. Last_Node loop

         if Is_Callable_Node (Node)
          and then
            not Is_Of_No_Interest (Node)
          and then
            not Body_Analyzed (Node)
          and then
            not Missing_Body_Reported (Node)
         then
            ASIS_UL.Output.Warning
              ("body is not analyzed for " &
               GS_Node_Name (Node)  & " (" &
               Get_String (GS_Node_SLOC (Node)) & ")");

            Set_Missing_Body_Reported (Node);
         end if;

      end loop;

   end Check_Call_Graph_Completeness;

   -------------------------
   -- Check_For_Main_Unit --
   -------------------------

   Main_Unit_Already_Processed : Boolean := False;
   --  As soon as the source file with the name coresponding to
   --  ASIS_UL.Options.Main_Subprogram_Name is processed, we do not need to
   --  check anything in Check_For_Main_Unit any more

   procedure Check_For_Main_Unit
     (SF   : SF_Id;
      CU   : Asis.Compilation_Unit;
      Unit : Asis.Element)
   is
      Main_Unit_Node    : GS_Node_Id;
   begin

      if not Main_Unit_Already_Processed
        and then
         ASIS_UL.Options.Main_Subprogram_Name /= null
        and then
         Base_Name (ASIS_UL.Options.Main_Subprogram_Name.all) =
         Base_Name (Source_Name (SF))
      then

         Main_Unit_Already_Processed := True;

         if not Can_Be_Main_Program (CU) then
            ASIS_UL.Output.Error
              ("file specified as main unit cannot be main subprogram");
            ASIS_UL.Common.Tool_Failures := ASIS_UL.Common.Tool_Failures + 1;

            return;
         end if;

         Main_Unit_Node    := Corresponding_Node (Unit);
         pragma Assert (Present (Main_Unit_Node));

         Add_Link_To_SLOC_List
           (To_Node => Environment_Task_Node,
            Link_To_Add => (Main_Unit_Node, Build_GNAT_Location (Unit)));

      end if;

   end Check_For_Main_Unit;

   -----------------------------
   -- Check_Node_Completeness --
   -----------------------------

   procedure Check_Node_Completeness (N : GS_Node_Id) is
      Next_Call   : Node_Lists.Cursor;
      Next_Call_N : GS_Node_Id;
   begin
      Next_Call := Node_Lists.First (All_Calls (N).all);

      while Node_Lists.Has_Element (Next_Call) loop
         Next_Call_N := Node_Lists.Element (Next_Call);
         if not Body_Analyzed (Next_Call_N)
           and then
            not Missing_Body_Reported (Next_Call_N)
         then
            ASIS_UL.Output.Warning
              ("body is not analyzed for " &
               GS_Node_Name (Next_Call_N)  & " (" &
               Get_String (GS_Node_SLOC (Next_Call_N)) & ")");

            Set_Missing_Body_Reported (Next_Call_N);
         end if;

         Next_Call := Node_Lists.Next (Next_Call);
      end loop;
   end Check_Node_Completeness;

   ----------------
   -- Close_Node --
   ----------------

   procedure Close_Node (Node : GS_Node_Id) is
   begin

      --  SLOC_Node_List_1 <--> Direct calls
      --  Node_List_1      <--> All calls

      Node_Lists.Clear (New_Set);
      Node_Lists.Clear (Newer_Set);

      Add_SLOC_Node_List_To_Node_List
        (Table (Node).Node_List_1,
         Table (Node).SLOC_Node_List_1);

      Add_SLOC_Node_List_To_Node_List
        (New_Set,
         Table (Node).SLOC_Node_List_1);

      while not Node_Lists.Is_Empty (New_Set) loop
         Next_Direct_Call := Node_Lists.First (New_Set);

         Next_Call :=
           SLOC_Node_Lists.First
             (Table (Node_Lists.Element (Next_Direct_Call)).
                SLOC_Node_List_1);

         while SLOC_Node_Lists.Has_Element (Next_Call) loop

            if not Node_Lists.Contains
              (Table (Node).Node_List_1,
               SLOC_Node_Lists.Element (Next_Call).Node)
            then
               Node_Lists.Insert
                 (Newer_Set, SLOC_Node_Lists.Element (Next_Call).Node);
            end if;

            Next_Call := SLOC_Node_Lists.Next (Next_Call);
         end loop;

         Node_Lists.Delete_First (New_Set);

         if not Node_Lists.Is_Empty (Newer_Set) then
            Node_Lists.Union (Table (Node).Node_List_1,
                              Newer_Set);
            Node_Lists.Union (New_Set,   Newer_Set);
            Node_Lists.Clear (Newer_Set);
         end if;

      end loop;

      --  SLOC_Node_List_2 <--> Direct reads
      --  SLOC_Node_List_3 <--> Direct writes
      --  Node_List_1      <--> All calls

      if Compute_Global_Objects_Accessed then

         for Node in First_GS_Node .. Last_Node loop

            --  Output_Node (Node);

            --  Traverse the set of all calls:

            Next_All_Call :=
              Node_Lists.First (Table (Node).Node_List_1);

            while Node_Lists.Has_Element (Next_All_Call) loop

               if not Is_Of_No_Interest
                        (Node_Lists.Element (Next_All_Call))
               then

                  --  Read references
                  Next_Ref :=
                    SLOC_Node_Lists.First
                      (Table (Node_Lists.Element (Next_All_Call)).
                          SLOC_Node_List_2);

                  while SLOC_Node_Lists.Has_Element (Next_Ref) loop

                     if not SLOC_Node_Lists.Contains
                              (Table (Node).SLOC_Node_List_2,
                               SLOC_Node_Lists.Element (Next_Ref))
                       and then
                         Is_Global_For
                           (Node  => SLOC_Node_Lists.Element (Next_Ref).Node,
                            Scope => Node)
--                           or else
--                            GS_Is_Local_Var_Accessed_By_Local_Tasks
--                              (SLOC_Node_Lists.Element (Next_Ref).Node))
                     then
                        Link_Tmp := SLOC_Node_Lists.Element (Next_Ref);

                        Add_Link_To_SLOC_List
                          (To_Node     => Node,
                           To_List     => Indirect_Read_References,
                           Link_To_Add => Link_Tmp);

                        Add_Link_To_SLOC_List
                          (To_Node     => Link_Tmp.Node,
                           To_List     => Indirect_Read_References,
                           Link_To_Add => (Node => Node,
                                           SLOC => Nil_String_Loc));
                     end if;

                     Next_Ref := SLOC_Node_Lists.Next (Next_Ref);
                  end loop;

                  --  Write references
                  Next_Ref :=
                    SLOC_Node_Lists.First
                      (Table (Node_Lists.Element (Next_All_Call)).
                          SLOC_Node_List_3);

                  while SLOC_Node_Lists.Has_Element (Next_Ref) loop

                     if not SLOC_Node_Lists.Contains
                              (Table (Node).SLOC_Node_List_3,
                               SLOC_Node_Lists.Element (Next_Ref))
                       and then
                         Is_Global_For
                           (Node  => SLOC_Node_Lists.Element (Next_Ref).Node,
                            Scope => Node)
--                           or else
--                            GS_Is_Local_Var_Accessed_By_Local_Tasks
--                              (SLOC_Node_Lists.Element (Next_Ref).Node))
                     then
                        Link_Tmp := SLOC_Node_Lists.Element (Next_Ref);

                        Add_Link_To_SLOC_List
                          (To_Node     => Node,
                           To_List     => Indirect_Write_References,
                           Link_To_Add => Link_Tmp);

                        Add_Link_To_SLOC_List
                          (To_Node     => Link_Tmp.Node,
                           To_List     => Indirect_Write_References,
                           Link_To_Add => (Node => Node,
                                           SLOC => Nil_String_Loc));
                     end if;

                     Next_Ref := SLOC_Node_Lists.Next (Next_Ref);
                  end loop;

               end if;

               Next_All_Call := Node_Lists.Next (Next_All_Call);
            end loop;

         end loop;

      end if;

   end Close_Node;

   ------------------------------------
   -- Collect_CG_Info_From_Construct --
   ------------------------------------

   procedure Collect_CG_Info_From_Construct
     (Element : Asis.Element;
      At_SLOC : String_Loc := Nil_String_Loc)
   is
      State   : String_Loc       := At_SLOC;
      Control : Traverse_Control := Continue;
   begin
      Traverse_Construct_For_CG_Info (Element, Control, State);
   end Collect_CG_Info_From_Construct;

   ----------------------
   -- Complete_CG_Info --
   ----------------------

   procedure Complete_CG_Info (El : Asis.Element) is
   begin

      if Is_Scope (El)
        and then
         Should_Be_In_CG (El)
      then
         Remove_Current_Scope;
      end if;

   end Complete_CG_Info;

   ------------------------------
   -- Complete_CG_Info_Post_Op --
   ------------------------------

   procedure Complete_CG_Info_Post_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out String_Loc)
   is
      pragma Unreferenced (Control, State);
   begin
      Complete_CG_Info (Element);
   end Complete_CG_Info_Post_Op;

   ------------------------------
   -- Expand_Dispatching_Calls --
   ------------------------------

   procedure Expand_Dispatching_Calls is
      Next_Disp_Call : Node_Lists.Cursor;
      Next_Call_Node : GS_Node_Id;
   begin

      for Node in First_GS_Node .. Last_Node loop
         Next_Disp_Call := Node_Lists.First (Table (Node).Node_List_2);

         while Node_Lists.Has_Element (Next_Disp_Call) loop
            Next_Call_Node := Node_Lists.Element (Next_Disp_Call);

            Add_Possible_Calls
              (Calling_Node   => Node,
               Disp_Operation => Next_Call_Node);

            Next_Disp_Call := Node_Lists.Next (Next_Disp_Call);
         end loop;

      end loop;

   end Expand_Dispatching_Calls;

   -----------------------
   -- First_Direct_Call --
   -----------------------

   function First_Direct_Call (N : GS_Node_Id) return GS_Node_Id is
      Result : GS_Node_Id := No_GS_Node;
   begin

      pragma Assert (Is_Callable_Node (N));

      if not SLOC_Node_Lists.Is_Empty (Table (N).SLOC_Node_List_1) then
         Result :=
           SLOC_Node_Lists.First_Element (Table (N).SLOC_Node_List_1).Node;
      end if;

      return Result;
   end First_Direct_Call;

   --------------------
   -- GS_Is_Renaming --
   --------------------

   function GS_Is_Renaming (N : GS_Node_Id) return Boolean is
   begin
      pragma Assert (GS_Node_Kind (N) in  Callable_Nodes);
      return Table (N).Bool_Flag_2;
   end GS_Is_Renaming;

   ---------------------
   -- GS_Is_Task_Type --
   ---------------------

   function GS_Is_Task_Type (N : GS_Node_Id) return Boolean is
   begin
      pragma Assert (GS_Node_Kind (N) in  Callable_Nodes);

      return GS_Node_Kind (N) = A_Task
            and then
             Table (N).Bool_Flag_3;
   end GS_Is_Task_Type;

   -----------------------------------
   -- Is_Called_By_Environment_Task --
   -----------------------------------

   function Is_Called_By_Environment_Task (N : GS_Node_Id) return Boolean is
      Result : Boolean := False;
   begin
      if Present (N) then
         Result :=
           Node_Lists.Contains
             (Container => Table (Environment_Task_Node).Node_List_1,
              Item      => N);
      end if;

      return Result;
   end Is_Called_By_Environment_Task;

   ---------------------------------
   -- Is_Library_Level_Subprogram --
   ---------------------------------

   function Is_Library_Level_Subprogram (N : GS_Node_Id) return Boolean is
      Result : Boolean := False;
   begin
      if Present (N)
        and then
         GS_Node_Kind (N) in Subprogram_Nodes
        and then
         GS_Node_Enclosing_Scope (N) = Environment_Task_Node
      then
         --  The only possibility that we have at the moment is to compare
         --  the name of the subprogram
         Result := GS_Node_Name (N) = GS_Enclosed_CU_Name (N);
      end if;

      return Result;
   end Is_Library_Level_Subprogram;

   -----------------------
   -- Is_Recursive_Node --
   -----------------------

   function Is_Recursive_Node (N : GS_Node_Id) return Boolean is
   begin

      return Node_Lists.Contains
               (Container => Table (N).Node_List_1, -- all calls
                Item      => N);
   end Is_Recursive_Node;

   -------------------------------
   -- Mark_Called_Function_Used --
   -------------------------------

   procedure Mark_Called_Function_Used
     (Element : Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State)
   is
      pragma Unreferenced (Control, State);
      Called_El   : Asis.Element;
      Called_Node : GS_Node_Id;
   begin

      if Expression_Kind (Element) = A_Function_Call then
         Called_El := Get_Called_Element (Element);

         if Declaration_Kind (Called_El) = An_Enumeration_Literal_Specification
           or else
            Is_Predefined_Operation_Renaming (Called_El)
         then
            return;
         end if;

         Called_El := Corresponding_Element (Called_El);

         if Is_Nil (Called_El)
           or else
            Expression_Kind (Called_El) = An_Attribute_Reference
           or else
            Expression_Kind (Called_El) = An_Enumeration_Literal
         then
            return;
         end if;

         Called_Node := Corresponding_Node (Called_El);

         if Present (Called_Node) then
            Set_Application_Flag_1 (Called_Node, True);
         end if;

      end if;

   end Mark_Called_Function_Used;

   ---------------------------
   -- Missing_Body_Reported --
   ---------------------------

   function Missing_Body_Reported (N : GS_Node_Id) return Boolean is
   begin
      pragma Assert (GS_Node_Kind (N) in  Callable_Nodes);
      return Table (N).Bool_Flag_7;
   end Missing_Body_Reported;

   ------------------------------------------------
   -- Patch_For_Default_Parameter_Initialization --
   ------------------------------------------------

   procedure Patch_For_Default_Parameter_Initialization
     (Element : Asis.Element)
   is
      Tmp     : Asis.Element;
      Control : Traverse_Control := Continue;
      State   : No_State         := Not_Used;
   begin

      if Declaration_Kind (Element) = A_Parameter_Specification then
         Tmp := Enclosing_Element (Element);

         if Is_Declaration_Of_Callable_Entity (Tmp) or else
            Declaration_Kind (Tmp) in
              An_Entry_Declaration .. An_Entry_Body_Declaration
         then
            Tmp := Initialization_Expression (Element);

            if not Is_Nil (Tmp) then
               Mark_All_Called_Functions_Used (Tmp, Control, State);
            end if;

         end if;
      end if;

   end Patch_For_Default_Parameter_Initialization;

   ------------------
   -- Process_Call --
   ------------------

   procedure Process_Call
     (Element : Asis.Element;
      At_SLOC : String_Loc := Nil_String_Loc)
   is
      Called_El   : Asis.Element := Get_Called_Element (Element);
      Called_Node : GS_Node_Id;

      Tmp_Cursor  : Node_Lists.Cursor;
      Tmp_Success : Boolean;
   begin

      if Is_Nil (Called_El) then

         if Is_Call_To_Predefined_Operation (Element)
           or else
             Is_Call_To_Attribute_Subprogram (Element)
           or else
             Is_Call_To_Default_Null_Procedure (Element)
         then
            --  We do not consider such calls at all
            return;
         elsif Generate_Global_Structure_Warnings then
            ASIS_UL.Output.Error (Build_GNAT_Location (Element) &
                   ": call can not be resolved statically");
         end if;

      elsif Declaration_Kind (Called_El) =
            An_Enumeration_Literal_Specification
      then
         --  This may happen in instantiation if an enumeration literal is
         --  used as an actual for a formal function.
         return;
      else

         if Is_Predefined_Operation_Renaming (Called_El) then
            --  We do not consider such calls at all
            return;
         end if;

         if Is_Renaming_Of_Null_Proc_Default (Called_El) then
            --  May take place in nested generic when formal subprogram with
            --  null default is used to instantiate another generic inside the
            --  template code.
            return;
         end if;

         Called_El := Corresponding_Element (Called_El);

         if Is_Nil (Called_El) then
            --  Subprogram renaming cannot be resolved statically. We do not
            --  generate any diagnstic here, because the cubprogram to be
            --  called here shall be marked as used anyway (if we have a
            --  explicit dereference here, then the renamed subprogram is
            --  marked as used when 'Access attribute is applied to it
            return;
         elsif Expression_Kind (Called_El) = An_Attribute_Reference
              or else
               Expression_Kind (Called_El) = An_Enumeration_Literal
         then
            --  These calls are of no interest
            return;
         end if;

         if not Should_Be_In_CG (Called_El) then
            return;
         end if;

         pragma Assert
           (Is_Declaration_Of_Callable_Entity (Called_El)
           or else
            Is_Scope (Called_El));

         if ASIS_UL.Options.Represent_Dispatching_Calls
           and then
            Is_Dispatching_Call (Element)
         then
            Called_Node := Corresponding_Node (Called_El);

            Node_Lists.Insert
              (Container => Table (Current_Scope).Node_List_2,
               New_Item  => Called_Node,
               Position  => Tmp_Cursor,
               Inserted  => Tmp_Success);
         end if;

         if Is_Part_Of_Inherited (Called_El) then
            Called_El := Corresponding_Declaration (Called_El);
         end if;

         if Skip_Dispatching_Calls then
            if At_SLOC = Nil_String_Loc then
               Store_Dispatching_Call
                 (Called_Entity => Called_El,
                  At_SLOC       => Build_GNAT_Location (Element));
            else
               Store_Dispatching_Call
                 (Called_Entity => Called_El,
                  At_SLOC       => At_SLOC);
            end if;
         else
            if At_SLOC = Nil_String_Loc then
               Store_Arc
                 (Called_Entity => Called_El,
                  At_SLOC       => Build_GNAT_Location (Element));
            else
               Store_Arc
                 (Called_Entity => Called_El,
                  At_SLOC       => At_SLOC);
            end if;
         end if;

      end if;

   end Process_Call;

   -----------------------------
   -- Process_Callable_Entity --
   -----------------------------

   procedure Process_Callable_Entity (El : Asis.Element) is
      Tmp : GS_Node_Id;
   begin
      Tmp := Corresponding_Node (El, Current_Scope);

      if Present (Tmp)
       and then
         Declaration_Kind (El) = A_Single_Task_Declaration
      then
         Store_Arc (Called_Entity => El, At_SLOC => Build_GNAT_Location (El));
      end if;

   end Process_Callable_Entity;

   -----------------------------
   -- Process_Discr_Init_Proc --
   -----------------------------

   procedure Process_Discr_Init_Proc (El : Asis.Element) is
      Proc_Node : constant GS_Node_Id :=
        Corresponding_Node (El, Expected_Kind => A_Type_Discr_Init_Procedure);
      pragma Unreferenced (Proc_Node);
   begin
      null;
   end Process_Discr_Init_Proc;

   -------------------------------
   -- Process_Elaboration_Calls --
   -------------------------------

   procedure Process_Elaboration_Calls (Element : Asis.Element) is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
      Call_AT_SLOC : constant String_Loc     := Build_GNAT_Location (Element);

      Type_To_Analyze : Asis.Element := Nil_Element;
      --  To be set to point to the (full) type declaration of the type
      --  for that we have to process default (sub)component initialization
      --  expressions

      Tmp_El : Asis.Element;

      Process_Discriminants : Boolean := False;
      --  In case if the discriminant constraint is present, we do not have to
      --  process default expressions for discriminants

   begin

      case Arg_Kind is
         when A_Variable_Declaration |
              An_Allocation_From_Subtype =>

            if Arg_Kind = A_Variable_Declaration then
               Type_To_Analyze := Object_Declaration_View (Element);
            else
               Type_To_Analyze := Allocator_Subtype_Indication (Element);
            end if;

            if Type_Kind (Type_To_Analyze) in
                 An_Unconstrained_Array_Definition ..
                  A_Constrained_Array_Definition
            then
               Type_To_Analyze := Array_Component_Definition (Type_To_Analyze);
               Type_To_Analyze :=
                 Component_Definition_View (Type_To_Analyze);
            end if;

            case Flat_Element_Kind (Type_To_Analyze) is
               when A_Subtype_Indication =>
                  Tmp_El := Asis.Definitions.Subtype_Mark (Type_To_Analyze);
               when An_Anonymous_Access_To_Procedure           |
                    An_Anonymous_Access_To_Protected_Procedure |
                    An_Anonymous_Access_To_Function            |
                    An_Anonymous_Access_To_Protected_Function  =>
                  return;
               when others =>
                  Tmp_El :=
                    Anonymous_Access_To_Object_Subtype_Mark (Type_To_Analyze);
            end case;

            if Expression_Kind (Tmp_El) /= An_Attribute_Reference then
               --  In case of a attribute reference as a subtype mark the
               --  only possible case is 'Base, so we have a scalar type
               --  here, therefore it can be no default initialization

               Process_Discriminants :=
                  Definition_Kind (Type_To_Analyze) = A_Subtype_Indication
                 and then
                  Is_Nil (Subtype_Constraint (Type_To_Analyze))
                 and then
                  Is_Indefinite_Subtype (Tmp_El);
            else
               Process_Discriminants := False;
            end if;

            Type_To_Analyze := Get_Subtype_Structure (Type_To_Analyze);

            --  First, check discriminants:

            if Process_Discriminants then

               Add_Link_To_SLOC_List
                 (To_Node     => Current_Scope,
                  To_List     => Calls,
                  Link_To_Add =>
                    (Node => Corresponding_Node
                               (El            => Type_To_Analyze,
                                Expected_Kind => A_Type_Discr_Init_Procedure),
                     SLOC => Build_GNAT_Location (Element)));

            end if;

            --  Now, check if we have record components with defaul
            --  initialization expressions

            if Has_Type_Init_Proc (Type_To_Analyze) then
               Add_Link_To_SLOC_List
                 (To_Node     => Current_Scope,
                  To_List     => Calls,
                  Link_To_Add =>
                    (Node => Corresponding_Node
                               (El            => Type_To_Analyze,
                                Expected_Kind => A_Type_Init_Procedure),
                     SLOC => Build_GNAT_Location (Element)));

            end if;

         when An_Entry_Call_Statement    |
              A_Procedure_Call_Statement |
              A_Function_Call            =>

            declare
               Call_Parameters : constant Asis.Element_List :=
                 Get_Call_Parameters (Element, Normalized => True);
               --  Note that if Elemnent is a dispatching or dynamic call,
               --  Call_Parameters are Nil_Element_List!
            begin

               for J in Call_Parameters'Range loop

                  if Is_Defaulted_Association (Call_Parameters (J)) then
                     Tmp_El := Actual_Parameter (Call_Parameters (J));

                     Unconditionally_Collect_CG_Info_From_Construct
                       (Element => Tmp_El,
                        At_SLOC => Call_AT_SLOC);
                  end if;

               end loop;

            end;

         when A_Procedure_Instantiation |
              A_Function_Instantiation  =>

            declare
               Inst_Parameters : constant Asis.Element_List :=
                 Generic_Actual_Part (Element, Normalized => True);
            begin

               for J in Inst_Parameters'Range loop

                  if Is_Defaulted_Association (Inst_Parameters (J))
                    and then
                     Declaration_Kind (Enclosing_Element
                       (Formal_Parameter (Inst_Parameters (J)))) =
                          A_Formal_Object_Declaration
                  then
                     --  Note the condition expression: we check that we have
                     --  an association corresponding to formal object by
                     --  querying the kind of Enclosing_Element of a formal,
                     --  but not actual parameter of the association, because
                     --  the ASIS Standard does not define exactly the effect
                     --  of Enclosing_Element for an actual parameter from a
                     --  normalized association

                     Tmp_El := Actual_Parameter (Inst_Parameters (J));

                     Unconditionally_Collect_CG_Info_From_Construct
                       (Element => Tmp_El,
                        At_SLOC => Call_AT_SLOC);
                  end if;

               end loop;

            end;

         when others =>
            null;
            --  Not implemented yet
      end case;

   end Process_Elaboration_Calls;

   ---------------------------------------------------
   -- Process_Init_Expressions_In_Record_Components --
   ---------------------------------------------------

   procedure Process_Init_Expressions_In_Record_Components
     (Component_List : Asis.Element_List;
      Call_At_SLOC   : String_Loc)
   is
      Comp_Def : Asis.Element;
   begin

      for J in Component_List'Range loop

         case Flat_Element_Kind (Component_List (J)) is
            when Flat_Clause_Kinds |
                 A_Null_Component  =>
               null;
            when A_Variant_Part =>

               Process_Init_Expressions_In_Record_Components
                 (Component_List =>
                    Asis.Definitions.Variants (Component_List (J)),
                  Call_At_SLOC => Call_At_SLOC);

            when A_Variant =>

               Process_Init_Expressions_In_Record_Components
                 (Component_List =>
                    Asis.Definitions.Record_Components (Component_List (J)),
                  Call_At_SLOC => Call_At_SLOC);

            when A_Component_Declaration =>

               Comp_Def := Initialization_Expression (Component_List (J));

               if Is_Nil (Comp_Def) then
                  --  No initialization here, but we have to go down the
                  --  component structure:

                  Comp_Def := Object_Declaration_View (Component_List (J));
                  Comp_Def := Component_Definition_View (Comp_Def);

                  if Definition_Kind (Comp_Def) = An_Access_Definition then
                     return;
                  end if;

                  Comp_Def := Get_Subtype_Structure (Comp_Def);

                  Process_Type_Default_Expressions
                    (Type_To_Analyze => Comp_Def,
                     Call_At_SLOC    => Call_At_SLOC);
               else
                  Collect_CG_Info_From_Construct
                    (Element => Comp_Def,
                     At_SLOC => Call_At_SLOC);
               end if;

            when others =>
               --  Just in case...
               pragma Assert (False);
               null;
         end case;
      end loop;

   end Process_Init_Expressions_In_Record_Components;

   ----------------------------
   -- Process_Type_Init_Proc --
   ----------------------------

   procedure Process_Type_Init_Proc (El : Asis.Element) is
      Proc_Node : constant GS_Node_Id :=
        Corresponding_Node (El, Expected_Kind => A_Type_Init_Procedure);
      pragma Unreferenced (Proc_Node);
   begin
      null;
   end Process_Type_Init_Proc;

   ------------------------------------
   -- Process_Record_Task_Components --
   ------------------------------------

   procedure Process_Record_Task_Components
     (Component_List : Asis.Element_List;
      Call_At_SLOC   : String_Loc)
   is
      Comp_Def : Asis.Element;
   begin

      for J in Component_List'Range loop

         case Flat_Element_Kind (Component_List (J)) is
            when Flat_Clause_Kinds |
                 A_Null_Component  =>
               null;
            when A_Variant_Part =>

               Process_Record_Task_Components
                 (Component_List =>
                    Asis.Definitions.Variants (Component_List (J)),
                  Call_At_SLOC => Call_At_SLOC);

            when A_Variant =>

               Process_Record_Task_Components
                 (Component_List =>
                    Asis.Definitions.Record_Components (Component_List (J)),
                  Call_At_SLOC => Call_At_SLOC);

            when A_Component_Declaration =>
               Comp_Def := Object_Declaration_View (Component_List (J));
               Comp_Def := Component_Definition_View (Comp_Def);

               if Definition_Kind (Comp_Def) = A_Subtype_Indication
                or else
                  Access_Definition_Kind (Comp_Def) in
                    An_Anonymous_Access_To_Variable ..
                      An_Anonymous_Access_To_Constant
               then
                  Comp_Def := Get_Subtype_Structure (Comp_Def);

                  Process_Task_Components
                    (Type_Decl    => Comp_Def,
                     Call_At_SLOC => Call_At_SLOC);
               end if;
            when others =>
               --  Just in case...
               pragma Assert (False);
               null;
         end case;
      end loop;

   end Process_Record_Task_Components;

   -------------------------------------
   -- Process_Reference_To_Subprogram --
   -------------------------------------

   procedure Process_Reference_To_Subprogram
     (Element : Asis.Element;
      At_SLOC : String_Loc)
   is
      Subpr     : Asis.Element := Prefix (Element);
      Call_Sloc : String_Loc   := At_SLOC;
   begin
      Subpr := Normalize_Reference (Subpr);

      if Expression_Kind (Subpr) not in
           An_Identifier .. An_Operator_Symbol
      then
         --  No interest for call graph, so
         return;
      end if;

      Subpr := Corresponding_Name_Declaration (Subpr);

      case Declaration_Kind (Subpr) is
         when A_Procedure_Instantiation    |
              A_Function_Instantiation     |
              A_Procedure_Declaration      |
              A_Function_Declaration       |
              A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  =>
            --  Continue processing...
            null;
         when A_Procedure_Body_Stub |
              A_Function_Body_Stub  =>

            if Declaration_Kind (Corresponding_Declaration (Subpr)) in
                 A_Generic_Declaration
            then
               --  No interest for a call graph
               return;
            end if;
         when others =>
            --  Nothing interesting for a call graph
            return;
      end case;

      if Should_Be_In_CG (Subpr) then

         if Call_Sloc = Nil_String_Loc then
            Call_Sloc := Build_GNAT_Location (Element);
         end if;

         Store_Arc
           (Called_Entity  => Subpr,
            At_SLOC        => At_SLOC,
            Calling_Entity => Enclosing_Scope (Subpr));

      end if;

   end Process_Reference_To_Subprogram;

   ------------------------------
   -- Process_Renaming_As_Body --
   ------------------------------

   procedure Process_Renaming_As_Body (El : Asis.Element) is
      Subprogram_Node : constant GS_Node_Id :=
        Corresponding_Node (Corresponding_Declaration (El));

      Renamed_Subprogram : Asis.Element := Get_Renamed_Subprogram (El);

      Renamed_Subprogram_Node : GS_Node_Id;

      Is_Of_No_Interest : Boolean := True;
   begin
      if not (Should_Be_In_CG (El)
             and then
              Should_Be_In_CG (Renamed_Subprogram))
      then
         return;
      end if;

      Set_Is_Renaming (Subprogram_Node);

      case Declaration_Kind (Renamed_Subprogram) is

         when A_Procedure_Declaration      |
              A_Function_Declaration       |
              A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Procedure_Body_Stub        |
              A_Function_Body_Stub         |
              A_Procedure_Instantiation    |
              A_Function_Instantiation     =>
            Is_Of_No_Interest := False;

         when An_Entry_Declaration    =>
            --  Task entry is renamed as a subprogram - we cannot process
            --  this case yet:
            Set_Is_Of_No_Interest (Subprogram_Node);
            raise ASIS_UL.Common.Non_Implemented_Error;

         when others =>
            --  Is_Of_No_Interest remains ON. Here we have all the cases of
            --  attrubute subprogram renamings
            null;
      end case;

      if Is_Of_No_Interest then
         Set_Is_Of_No_Interest (Subprogram_Node);
      else
         if Is_Part_Of_Inherited (Renamed_Subprogram) then
            Renamed_Subprogram :=
              Corresponding_Declaration (Renamed_Subprogram);
         end if;

         Renamed_Subprogram_Node := Corresponding_Node (Renamed_Subprogram);

         --  Add the "call" from a renaming to the renamed subprogram
         Add_Link_To_SLOC_List
           (To_Node     => Subprogram_Node,
            To_List     => Calls,
            Link_To_Add => (Node => Renamed_Subprogram_Node,
                            SLOC => Build_GNAT_Location (El)));
      end if;

   end Process_Renaming_As_Body;

   -------------------
   -- Process_Scope --
   -------------------

   procedure Process_Scope (El : Asis.Element) is
      Tmp      : GS_Node_Id;
      Scope_El : Asis.Element;
   begin

      Scope_El := Corresponding_Element (El);

      if not Should_Be_In_CG (Scope_El) then
         --  Is it OK? What about the enclosing scope references?
         return;
      end if;

      if Is_Subunit (El) then
         Tmp := Corresponding_Node (Scope_El);
      else
         Tmp := Corresponding_Node (Scope_El, Current_Scope);
      end if;

      if Declaration_Kind (El) = A_Task_Body_Declaration
        and then
          Declaration_Kind (Corresponding_Declaration (Scope_El)) =
            A_Task_Type_Declaration
      then
         --  Task type differs from a single anonymously typed task object in
         --  respect of the scope node. For a task object, the front-end
         --  creates an inplicit task type using the defining identifier node
         --  from the task body as the defining identifier node for this type,
         --  so the defining identifier from the body works as a top of the
         --  scope for bodies corresponding to single task declarations. But
         --  for a body that corresponds to a task type we have to go to the
         --  task type declaration to get the scope node.

         Scope_El := Corresponding_Declaration (Scope_El);
      end if;

      Scope_El := First_Name (Scope_El);

      Set_Current_Scope (Tmp, Node (Scope_El));
      Set_Body_Analyzed (Tmp);

      if Represent_Dispatching_Calls
        and then
         Is_Dispatching_Operation (Scope_El)
        and then
         Is_Overriding_Operation (Scope_El)
      then
         Set_Implementing_Node (Implemented_Operations (Scope_El), Tmp);
      end if;

      --  If we have a body of a user-defined "=" operation that can be used
      --  as a part of the implementation of some other predefined "="
      --  according to RM 2012 4.5.2 (14/3 .. 15/3) and 3.4 (17/2), then we
      --  mark it as used by creating the call link from environment task node
      --  to the corresponding function declaration.

      if Can_Be_Embedded_In_Equiality (Enclosing_Element (Scope_El)) then
         Add_Link_To_SLOC_List
           (To_Node => Environment_Task_Node,
            Link_To_Add => (Tmp, Build_GNAT_Location (Scope_El)));
      end if;

   end Process_Scope;

   -------------------------------------------
   -- Process_Stream_Attribute_Redefinition --
   -------------------------------------------

   procedure Process_Stream_Attribute_Redefinition
     (Element : Asis.Element;
      At_SLOC : String_Loc)
   is
      Subpr : Asis.Element := Representation_Clause_Expression (Element);
   begin
      if Expression_Kind (Subpr) = An_Explicit_Dereference then
         return;
      else
         Subpr := Normalize_Reference (Subpr);
      end if;

      Subpr := Corresponding_Name_Definition (Subpr);
      Subpr := Enclosing_Element (Subpr);

      pragma Assert
        (Is_Declaration_Of_Callable_Entity (Subpr)
        or else
         Is_Scope (Subpr));

      if Should_Be_In_CG (Subpr) then

         if At_SLOC = Nil_String_Loc then
            Store_Arc
              (Called_Entity => Subpr,
               At_SLOC       => Build_GNAT_Location (Element));
         else
            Store_Arc
              (Called_Entity => Subpr,
               At_SLOC       => At_SLOC);
         end if;

      end if;

   end Process_Stream_Attribute_Redefinition;

   -----------------------------
   -- Process_Task_Components --
   -----------------------------

   procedure Process_Task_Components
     (Type_Decl      : Asis.Element;
      Call_At_SLOC   : String_Loc;
      Recursive_Call : Boolean := False)
   is
      T_Def : Asis.Element;
      Tmp   : Asis.Element;

      Unused_Cursor : Element_Containers.Cursor;
      Inserted       : Boolean;
   begin

      if Recursive_Call then
         Element_Containers.Clear (Processed_Types);
      end if;

      Element_Containers.Insert
        (Container => Processed_Types,
         New_Item  => Type_Decl,
         Position  => Unused_Cursor,
         Inserted  => Inserted);

      if not Inserted then
         --  To avoid recursion
         return;
      end if;

      case Declaration_Kind (Type_Decl) is
         when A_Task_Type_Declaration =>

            if Should_Be_In_CG (Type_Decl) then
               Store_Arc
                 (Called_Entity => Type_Decl,
                  At_SLOC       => Call_At_SLOC);
            end if;

         when A_Protected_Type_Declaration |
              A_Formal_Type_Declaration =>
            null;
         when An_Ordinary_Type_Declaration =>
            T_Def := Type_Declaration_View (Type_Decl);

            case Type_Kind (T_Def) is
               when A_Derived_Record_Extension_Definition =>

                  Tmp := Asis.Definitions.Record_Definition (T_Def);

                  if Definition_Kind (Tmp) /= A_Null_Record_Definition then
                     Process_Record_Task_Components
                       (Component_List => Record_Components (Tmp),
                        Call_At_SLOC   => Call_At_SLOC);
                  end if;

                  Tmp := Parent_Subtype_Indication (T_Def);
                  Tmp := Get_Subtype_Structure (Tmp);
                  Process_Task_Components (Tmp, Call_At_SLOC => Call_At_SLOC);

               when An_Unconstrained_Array_Definition |
                    A_Constrained_Array_Definition    =>

                  Tmp := Array_Component_Definition (T_Def);
                  Tmp := Component_Definition_View  (Tmp);

                  if Definition_Kind (Tmp) = A_Subtype_Indication then
                     --  we are not interested in components that are defined
                     --  by An_Access_Definition
                     Tmp := Get_Subtype_Structure (Tmp);

                     Process_Task_Components
                       (Tmp,
                        Call_At_SLOC => Call_At_SLOC);
                  end if;

               when A_Record_Type_Definition |
                    A_Tagged_Record_Type_Definition =>

                  --  Note: we do not process discriminant components!

                  Tmp := Asis.Definitions.Record_Definition (T_Def);

                  if Definition_Kind (Tmp) /= A_Null_Record_Definition then
                     Process_Record_Task_Components
                       (Component_List => Record_Components (Tmp),
                        Call_At_SLOC   => Call_At_SLOC);
                  end if;

               when A_Derived_Type_Definition =>
                  --  Just in case...
                  pragma Assert (False);
                  null;

               when others =>
                  null;
            end case;

         when An_Incomplete_Type_Declaration       |
              A_Tagged_Incomplete_Type_Declaration =>
            Process_Task_Components
              (Type_Decl    => Corresponding_Type_Declaration (Type_Decl),
               Call_At_SLOC => Call_At_SLOC);

         when others =>
            pragma Assert (False);
            null;
      end case;

   end Process_Task_Components;

   ---------------------------
   -- Process_Task_Creation --
   ---------------------------

   procedure Process_Task_Creation (El : Asis.Element) is
      Type_To_Analyze : Asis.Element;
   begin

      case Flat_Element_Kind (El) is
         when A_Variable_Declaration |
              A_Constant_Declaration =>
            Type_To_Analyze := Object_Declaration_View (El);

            if Type_Kind (Type_To_Analyze) in
                 An_Unconstrained_Array_Definition ..
                  A_Constrained_Array_Definition
            then
               Type_To_Analyze := Array_Component_Definition (Type_To_Analyze);
               Type_To_Analyze :=
                 Component_Definition_View (Type_To_Analyze);
            end if;

         when An_Allocation_From_Subtype =>
            Type_To_Analyze := Allocator_Subtype_Indication (El);
         when others =>
            pragma Assert (False);
            null;
      end case;

      if Definition_Kind (Type_To_Analyze) = An_Access_Definition
        and then
         Access_Definition_Kind (Type_To_Analyze) not in
            An_Anonymous_Access_To_Variable .. An_Anonymous_Access_To_Constant
      then
         return;
      end if;

      Type_To_Analyze := Get_Subtype_Structure (Type_To_Analyze);

      Process_Task_Components
        (Type_To_Analyze,
         Call_At_SLOC   => Build_GNAT_Location (El),
         Recursive_Call => True);
   end Process_Task_Creation;

   --------------------------------------
   -- Process_Type_Default_Expressions --
   --------------------------------------

   procedure Process_Type_Default_Expressions
     (Type_To_Analyze : Asis.Element;
      Call_At_SLOC    : String_Loc)
   is
      Type_Def : constant Asis.Element :=
        Type_Declaration_View (Type_To_Analyze);

      Tmp : Asis.Element;
   begin

      --  Note: we do not process discriminant components!

      case Definition_Kind (Type_Def) is
         when A_Protected_Definition =>
            Process_Init_Expressions_In_Record_Components
              (Component_List => Private_Part_Items (Type_Def),
               Call_At_SLOC   => Call_At_SLOC);

         when A_Type_Definition =>

            case Type_Kind (Type_Def) is

               when A_Derived_Record_Extension_Definition =>

                  Tmp := Asis.Definitions.Record_Definition (Type_Def);

                  if Definition_Kind (Tmp) = A_Null_Record_Definition then
                     Process_Init_Expressions_In_Record_Components
                       (Component_List => Record_Components (Tmp),
                        Call_At_SLOC   => Call_At_SLOC);
                  end if;

                  Tmp := Parent_Subtype_Indication (Type_Def);
                  Tmp := Get_Subtype_Structure (Tmp);

                  Process_Type_Default_Expressions
                    (Type_To_Analyze => Tmp,
                     Call_At_SLOC    => Call_At_SLOC);

               when An_Unconstrained_Array_Definition |
                    A_Constrained_Array_Definition    =>

                  Tmp := Array_Component_Definition (Type_Def);
                  Tmp := Component_Definition_View  (Tmp);

                  if Definition_Kind (Tmp) = A_Subtype_Indication then
                     --  we are not interested in components that are defined
                     --  by An_Access_Definition
                     Tmp := Get_Subtype_Structure (Tmp);

                     Process_Type_Default_Expressions
                       (Type_To_Analyze => Tmp,
                        Call_At_SLOC    => Call_At_SLOC);
                  end if;

               when A_Record_Type_Definition |
                    A_Tagged_Record_Type_Definition =>

                  Tmp := Asis.Definitions.Record_Definition (Type_Def);

                  if Definition_Kind (Tmp) /= A_Null_Record_Definition then
                     Process_Init_Expressions_In_Record_Components
                       (Component_List => Record_Components (Tmp),
                        Call_At_SLOC   => Call_At_SLOC);
                  end if;

               when others =>
                  --  No default initialization expression in this case!
                  null;
            end case;

         when others =>
            --  No default initialization expression in this case!
            null;
      end case;

   end Process_Type_Default_Expressions;

   -----------------------
   -- Set_Body_Analyzed --
   -----------------------

   procedure Set_Body_Analyzed (N : GS_Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (GS_Node_Kind (N) in  Callable_Nodes);
      Set_Bool_Flag_1 (N, Val);
   end Set_Body_Analyzed;

   ---------------------------
   -- Set_Implementing_Node --
   ---------------------------

   procedure Set_Implementing_Node
     (Implementred_Operations : Asis.Element_List;
      Implemeting_Node        : GS_Node_Id)
   is
      Next_Implemented_Op : GS_Node_Id;
   begin

      for Op in Implementred_Operations'Range loop
         Next_Implemented_Op :=
           Corresponding_Node
             (Corresponding_Element (Implementred_Operations (Op)),
              Unconditionally => True);

         Node_Lists.Insert
           (Table (Next_Implemented_Op).Node_List_3,
            Implemeting_Node);
      end loop;

   end Set_Implementing_Node;

   ---------------------
   -- Set_Is_Renaming --
   ---------------------

   procedure Set_Is_Renaming (N : GS_Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (GS_Node_Kind (N) in  Callable_Nodes);
      Set_Bool_Flag_2 (N, Val);
   end Set_Is_Renaming;

   ----------------------
   -- Set_Is_Task_Type --
   ----------------------

   procedure Set_Is_Task_Type (N : GS_Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (GS_Node_Kind (N) = A_Task);
      Set_Bool_Flag_3 (N, Val);
   end Set_Is_Task_Type;

   -------------------------------
   -- Set_Missing_Body_Reported --
   -------------------------------

   procedure Set_Missing_Body_Reported
     (N  : GS_Node_Id;
      Val : Boolean := True)
   is
   begin
      pragma Assert (GS_Node_Kind (N) in  Callable_Nodes);
      Set_Bool_Flag_7 (N, Val);
   end Set_Missing_Body_Reported;

   ---------------
   -- Store_Arc --
   ---------------

   procedure Store_Arc
     (Called_Entity  : Asis.Element;
      At_SLOC        : String_Loc;
      Calling_Entity : Asis.Element := Nil_Element)
   is
      Called_Node  : constant GS_Node_Id := Corresponding_Node (Called_Entity);
      Calling_Node :          GS_Node_Id := Current_Scope;
   begin

      if not Is_Nil (Calling_Entity) then
         Calling_Node := Corresponding_Node
           (Corresponding_Element (Calling_Entity), Unconditionally => True);
         pragma Assert (Present (Calling_Node));
      end if;

      pragma Assert
        (First_GS_Node < Called_Node
       and then
         Called_Node <= Last_Node);

      Add_Link_To_SLOC_List
        (To_Node     => Calling_Node,
         To_List     => Calls,
         Link_To_Add => (Node => Called_Node, SLOC => At_SLOC));

   end Store_Arc;

   ----------------------------
   -- Store_Dispatching_Call --
   ----------------------------

   procedure Store_Dispatching_Call
     (Called_Entity  : Asis.Element;
      At_SLOC        : String_Loc;
      Calling_Entity : Asis.Element := Nil_Element)
   is
      Called_Node  : constant GS_Node_Id := Corresponding_Node (Called_Entity);
      Calling_Node :          GS_Node_Id := Current_Scope;
   begin

      if not Is_Nil (Calling_Entity) then
         Calling_Node := Corresponding_Node
           (Corresponding_Element (Calling_Entity), Unconditionally => True);
         pragma Assert (Present (Calling_Node));
      end if;

      pragma Assert
        (First_GS_Node < Called_Node
       and then
         Called_Node <= Last_Node);

      Add_Link_To_SLOC_List
        (To_Node     => Calling_Node,
         To_List     => Dispatching_Calls,
         Link_To_Add => (Node => Called_Node, SLOC => At_SLOC));

   end Store_Dispatching_Call;

   ----------------------------------
   -- Store_Dispatching_Operations --
   ----------------------------------

   procedure Store_Dispatching_Operations (El : Asis.Element) is
      Disp_Ops : constant Asis.Element_List :=
        Dispatching_Operations (El);

      Tmp_Node : GS_Node_Id;
      pragma Unreferenced (Tmp_Node);
   begin

      for Op in Disp_Ops'Range loop
         if not Is_Predefined_Operation_Renaming (Disp_Ops (Op)) then
            Tmp_Node := Corresponding_Node (Disp_Ops (Op));
         end if;
      end loop;

   end Store_Dispatching_Operations;

   ------------------------
   -- Transitive_Closure --
   ------------------------

   procedure Transitive_Closure is
   begin

      if not Traverse_Renamings_Done then
         Traverse_Renamings;
      end if;

      if Represent_Dispatching_Calls
        and then
         not Skip_Dispatching_Calls
      then
         Expand_Dispatching_Calls;
      end if;

      Check_Call_Graph_Completeness;

      for Node in First_GS_Node .. Last_Node loop

         if Is_Callable_Node (Node)
           and then
            not Is_Of_No_Interest (Node)
         then
            Close_Node (Node);
         end if;

      end loop;

      Transitive_Closure_Done_Flag := True;

   end Transitive_Closure;

   -----------------------------
   -- Transitive_Closure_Done --
   -----------------------------

   function Transitive_Closure_Done return Boolean is
   begin
      return Transitive_Closure_Done_Flag;
   end Transitive_Closure_Done;

   ------------------------
   -- Traverse_Renamings --
   ------------------------

   procedure Traverse_Renamings is
      Already_Processed_Renamings : Node_Lists.Set;

      procedure Process_Renaming (Node : GS_Node_Id);
      --  Processes one renaming node and after that add node to
      --  Already_Processed_Renamings set. This procedure recursively traverses
      --  renaming chains (we suppose that these chains do not contain loops,
      --  any loop definitely means an elaboration problem!).

      procedure Process_Renaming (Node : GS_Node_Id) is
         Renamed_Node : constant GS_Node_Id := First_Direct_Call (Node);
      begin

         Node_Lists.Insert (Already_Processed_Renamings, Node);

         if Is_Of_No_Interest (Renamed_Node) then
            Set_Is_Of_No_Interest (Node);
            return;
         end if;

         if GS_Is_Renaming (Renamed_Node)
           and then
             not Node_Lists.Contains
                   (Already_Processed_Renamings, Renamed_Node)
         then
            Process_Renaming (Renamed_Node);
            --  This may define that Renamed_Node is of no interest, so:

            if Is_Of_No_Interest (Renamed_Node) then
               Set_Is_Of_No_Interest (Node);
               return;
            end if;

         end if;

         Set_Body_Analyzed (Node, Body_Analyzed (Renamed_Node));

      end Process_Renaming;

   begin
      Node_Lists.Clear (Already_Processed_Renamings);

      for Node in First_GS_Node .. Last_Node loop

         if Is_Callable_Node (Node)
           and then
             GS_Is_Renaming (Node)
           and then
            not Is_Of_No_Interest (Node)
           and then
             not Node_Lists.Contains (Already_Processed_Renamings, Node)
         then
            Process_Renaming (Node);
         end if;

      end loop;

      Traverse_Renamings_Done_Flag := True;
   end Traverse_Renamings;

   -----------------------------
   -- Traverse_Renamings_Done --
   -----------------------------

   function Traverse_Renamings_Done return Boolean is
   begin
      return Traverse_Renamings_Done_Flag;
   end Traverse_Renamings_Done;

   ----------------------------------------
   -- Unconditionally_Add_CG_Info_Pre_Op --
   ----------------------------------------

   procedure Unconditionally_Add_CG_Info_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out String_Loc)
   is
      Expanded_Code : Asis.Element;

   begin

      Add_CG_Info (Element, State);

      if Declaration_Kind (Element) in
           A_Package_Instantiation .. A_Function_Instantiation
      then
         Expanded_Code := Corresponding_Declaration (Element);

         Traverse_Construct_For_CG_Info
           (Element => Expanded_Code,
            Control => Control,
            State   => State);

         Expanded_Code := Corresponding_Body (Element);

         if not Is_Nil (Expanded_Code) then
            Traverse_Construct_For_CG_Info
              (Element => Expanded_Code,
               Control => Control,
               State   => State);
         end if;

      end if;

   exception
      when Ex : others =>
         ASIS_UL.Common.Tool_Failures := ASIS_UL.Common.Tool_Failures + 1;

         ASIS_UL.Output.Error ("call graph info collection failed");
         ASIS_UL.Output.Error (Build_GNAT_Location (Element));
         ASIS_UL.Output.Report_Unhandled_Exception (Ex);

   end Unconditionally_Add_CG_Info_Pre_Op;

   ----------------------------------------------------
   -- Unconditionally_Collect_CG_Info_From_Construct --
   ----------------------------------------------------

   procedure Unconditionally_Collect_CG_Info_From_Construct
     (Element : Asis.Element;
      At_SLOC : String_Loc := Nil_String_Loc)
   is
      State   : String_Loc       := At_SLOC;
      Control : Traverse_Control := Continue;
   begin
      Unconditionally_Traverse_Construct_For_CG_Info (Element, Control, State);
   end Unconditionally_Collect_CG_Info_From_Construct;

end ASIS_UL.Global_State.CG;
