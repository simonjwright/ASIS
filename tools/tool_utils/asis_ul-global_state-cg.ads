------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              A S I S _ U L . G L O B A L _ S T A T E . C G               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2007-2017, AdaCore                     --
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

--  This package defines the interface routines for building and analyzing
--  the call graph

--  ??? To much of the implementation details in the spec???

with Asis;

package ASIS_UL.Global_State.CG is

   procedure Check_For_Main_Unit
     (SF   : SF_Id;
      CU   : Asis.Compilation_Unit;
      Unit : Asis.Element);
   --  If the name of the file containing the main subprogram
   --  (ASIS_UL.Options.Main_Subprogram_Name) is specified, checks if SF is
   --  the file with the main subprogram, and if it is, checks that CU indeed
   --  can be a main subprogram. If it can, creates the link that represents
   --  the call from the Environment Tast to the (node representing) the
   --  corresponding subprogram, otherwise generates the error message.
   --  This routine assumes that CU is the compilation unit containing in SF,
   --  and Unit is the top unit declaration element from this CU.

   procedure Add_CG_Info
     (Element : Asis.Element;
      At_SLOC : String_Loc := Nil_String_Loc);
   --  This procedure is supposed to be called during the element traversal
   --  as a part of pre-operation for each element being visited. It adds to
   --  the global structure the information corresponding to the given Element.
   --  It also updates the auxiliary data structures used for the call graph
   --  construction (scope stack)
   --
   --  For each piece of the stored call graph information this procedure
   --  should store the full "trace" of the information needed to build a full
   --  cal graph. That is, if we process a subprogram call, we store the
   --  declaration of the called subprogram, and we also check for the source
   --  containing the body, and if it is not in the set of sources to be
   --  processed by the tool that uses this call graph engine, we try to locate
   --  this source and add to the source table (???)
   --
   --  If At_SLOC is not equal Nil_String_Loc, it is used as the SLOC of a
   --  construct that is a reason to add some information in the call graph.
   --  (For a function call that is a consequence of computing the default
   --  expression for a record componet as a part of elaboration of an object
   --  declaration, the SLOC of the call is the SLOC of the object declaration,
   --  but not the SLOC of the call in the default component expression in the
   --  corresponding record definition). Otehrwise the SLOC of the
   --  corresponding information item in the call graph is the SLOC of the
   --  argument Element.

   procedure Complete_CG_Info (El : Asis.Element);
   --  This procedure is supposed to be called during the element traversal
   --  as a part of post-operation for each element being visited. It completes
   --  the information stored in the global structure for the given Element.
   --  It also updates the auxiliary data structures used for the call graph
   --  construction (scope stack)

   procedure Add_CG_Info_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out String_Loc);

   procedure Unconditionally_Add_CG_Info_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out String_Loc);

   procedure Complete_CG_Info_Post_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out String_Loc);
   --  Wrapper procedures for Add_CG_Info and Add_CG_Info_Pre_Op, add State and
   --  control parameters, this makes it possible to use these procedures as
   --  actuals in Traverse_Element instantiation. The State parameter is used
   --  to pass the information of the SLOC of the information items stored
   --  in the call graph. If it is Nil_String_Loc, the SLOC of an information
   --  item is computed from the ELmenet from that this item is derived.
   --  Otherwise the State is used as SLOC of all the information items.
   --  (Consider getting the function calls from the default component
   --  expressions that are computed as a part of an object declaration
   --  elaboration)
   --
   --  The difference between Add_CG_Info_Pre_Op and
   --  Unconditionally_Add_CG_Info_Pre_Op is that Add_CG_Info_Pre_Op skips the
   --  pieces of the code that are not executed (default initialization
   --  expressions, generics, what else) by changing the Control parameter
   --  whereas Unconditionally_Add_CG_Info_Pre_Op does not modify the Control
   --  parameter and as the result all the code is traversed.

   procedure Collect_CG_Info_From_Construct
     (Element : Asis.Element;
      At_SLOC : String_Loc := Nil_String_Loc);
   --  Collects the call graph information from an argument element by
   --  tarversing the Element structure and calling Add_CG_Info as
   --  pre-operation and Complete_CG_Info as post-operation.
   --  If At_SLOC is not nil, it is used as a SLOC for the information items
   --  stored in the call graph as the result of calling this procredure.
   --  When collecting the call graph information, this procedure skips the
   --  parts of the code that are not executed - generic templates, default
   --  initialization expressions (what else???).

   procedure Unconditionally_Collect_CG_Info_From_Construct
     (Element : Asis.Element;
      At_SLOC : String_Loc := Nil_String_Loc);
   --  Similar to Collect_CG_Info_From_Construct, but this procedure collects
   --  the call graph information from all the components of the argument
   --  Element

   function Is_Recursive_Node (N : GS_Node_Id) return Boolean;
   --  Check if N calls itself (directly or indirectly). It is an error to call
   --  this function if No (N), or if the transitive clousure of the global
   --  structure has not been performed yet.

   function Is_Called_By_Environment_Task (N : GS_Node_Id) return Boolean;
   --  Checks if a given node is called (directly or indirectly) by an
   --  environment task

   ---------------------------------------------------
   --  Dispatching calls and dispatching operations --
   ---------------------------------------------------

   procedure Set_Implementing_Node
     (Implementred_Operations : Asis.Element_List;
      Implemeting_Node        : GS_Node_Id);
   --  Implemeting_Node is supposed to a be node corresponding to some
   --  dispatching operation, and Implementred_Operations is supposed to be the
   --  list of operations, directly "implemented" by this dispatching operation
   --  (that is, this operation can be called as a result of a dispatching call
   --  rooted by an operation from the list). This subprogram adds
   --  Implemeting_Node to the list of directly implementing subprograms for
   --  nodes corresponding to subprograms from Implementred_Operations list,
   --  if the node corresponding to some implemented operation does not exist,
   --  it is created as a part of processing of the call to this procedure.

   --------------------------------------------
   -- Routines related to transitive closure --
   --------------------------------------------

   procedure Expand_Dispatching_Calls;
   --  For each dispatching call, adds to the list of direct calls of the
   --  calling node all the operations that may be called at the place of this
   --  call. The artificial (0, 0) SLOC is used as SLOC for added nodes. (Can
   --  we do this or should we use the real sloc of the call here???)

   procedure Traverse_Renamings;
   --  This procedure goes trough all the Call Graph renamings nodes and
   --  decides for which renaming nodes we can say that the corresponding
   --  body is processed.

   function Traverse_Renamings_Done return Boolean;
   --  Checks if Traverse_Renamings has been called at least once.

   procedure Close_Node (Node : GS_Node_Id);
   --  Creates a list of all the nodes called by the given node using
   --  workpile algorithm.

   procedure Transitive_Closure;
   --  This procedure should be called when all the "local" call information is
   --  already stored in the global data structures. It performs the transitive
   --  closure of the call graph. After the call to this procedure, for
   --  each entity (call graph node) we have a full set of all calls.
   --
   --  If ASIS_UL.Options.Represent_Dispatching_Calls is ON, before performomg
   --  transitive closure, for each the dispatching call stored in the call
   --  graph (Node_List_2), the list of all the operations that "implement"
   --  the called dispatching operation (Node_List_3) are added to the list of
   --  direct calls of the node that issues this dispatching call. (That is, a
   --  dispatching call is considered as if all possible operations that
   --  inherit/override the dispatching operation are called at the place
   --  of the call. For added to the list of direct calls operations, the
   --  paceholder (empty) SLOC is used (???).
   --
   --  Call graph transitive closure is a time-consuming operation, At the
   --  moment we use the workpile algorithm to compute a set of all the nodes
   --  called by the given node.

   function Transitive_Closure_Done return Boolean;
   --  Checks if Transitive_Closure has been called at least once.

   --  Note that it is possible to call Transitive_Closure and
   --  Traverse_Renamings even of the corresponding test functions return True,
   --  and these procedures will try to do the job they are for.

   procedure Check_Call_Graph_Completeness;
   --  Checks if the call information stored in the global data structure is
   --  complete and allows to construct the full Call Graph. Generates a
   --  diagnostic message each time when any incompleteness is detected.

   procedure Check_Node_Completeness (N : GS_Node_Id);
   --  Checks if the call information stored in the global data structure for
   --  the argument node is complete and allows to construct the full Call
   --  Graph. Generates a diagnostic message each time when any
   --  incompleteness is detected.

   ----------------------------------------------------------------------
   --  Access and update routines for callable node general properties --
   ----------------------------------------------------------------------

   --  All the routines declared in this section should be allied to callable
   --  entities only.

   function Body_Analyzed (N : GS_Node_Id) return Boolean;
   procedure Set_Body_Analyzed (N : GS_Node_Id; Val : Boolean := True);
   --  Reports and sets the flag indicating if the body of the callable entity
   --  has been analyzed. Raise Constraint_Error if No (N).

   function Missing_Body_Reported (N : GS_Node_Id) return Boolean;
   procedure Set_Missing_Body_Reported (N : GS_Node_Id; Val : Boolean := True);
   --  Reports and sets the flag indicating if for the callable entity the
   --  warning about missing body has been issued. Raise Constraint_Error if
   --  No (N).

   function GS_Is_Renaming (N : GS_Node_Id) return Boolean;
   --  Tells if the node is a renaming of another node. Raises Constraint_Error
   --  is No (N).

   function Is_Library_Level_Subprogram (N : GS_Node_Id) return Boolean;
   --  Checks if the argument node represents a library-level subprogram.
   --  !!! The implementation is wrong!!! The function actually detects if the
   --  argument is a global subprogram. That is, it returns True also for
   --  all subprograms that are not nested in other scopes.

   function GS_Is_Task_Type (N : GS_Node_Id) return Boolean;
   --  Tells if N represents a task node correspondong to a task type. Always
   --  returns False if N is a callable, but non-task node. Raises
   --  Constraint_Error is No (N).

   procedure Set_Is_Task_Type (N : GS_Node_Id; Val : Boolean := True);
   --  Sets the flag indicating that N represents a task type entity. Assumes
   --  that N is of A_Task kind. Raises Constraint_Error is No (N).

end ASIS_UL.Global_State.CG;
