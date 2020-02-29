------------------------------------------------------------------------------
--                                                                          --
--                  COMMON ASIS TOOLS COMPONENTS LIBRARY                    --
--                                                                          --
--                 A S I S _ U L . G L O B A L _ S T A T E
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not,  write  to the  Free Software Foundation,  51 Franklin  --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the top of the subhierarchy describing the global
--  state of the set of sources being analyzed.

pragma Ada_2012;

with Ada.Containers.Ordered_Sets;

with Asis;                    use Asis;
with Asis.Extensions.Strings; use Asis.Extensions.Strings;

with Types;                   use Types;

with ASIS_UL.Source_Table;    use ASIS_UL.Source_Table;

package ASIS_UL.Global_State is

   --  The global state is represented as a single set of nodes representing
   --  entities  of different kinds. The global structure represents various
   --  relations between the corresponding entities. Depending on the following
   --  flags, it may contain this or that information.

   ---------------------
   --  Global options --
   ---------------------

   function Compute_Global_Objects_Accessed return Boolean;

   procedure Do_Compute_Global_Objects_Accessed;
   --  Set an internal flag to compute global objects accessed directly or
   --  indirectly by subprograms

   ----------------------------------
   --  Global structure node kinds --
   ----------------------------------

   type GS_Node_Kinds is
     (Not_A_Node,
      --  A null (absent or non-inialized) node

      --  Callable nodes
      Environment_Task,
      A_Package,
      A_Procedure,
      A_Null_Procedure,
      A_Type_Discr_Init_Procedure,
      A_Type_Init_Procedure,
      A_Function,
      A_Task,
      A_Task_Entry,
      A_Protected_Procedure,
      A_Protected_Function,
      A_Protected_Entry,

      --  Data nodes
      A_Data_Object

      --  To be continued???
      );

   --  Nodes for which there all variable declarations should be considered
   --  as global
   subtype Global_Nodes is GS_Node_Kinds range
     Environment_Task .. A_Package;
   subtype Callable_Nodes is GS_Node_Kinds range
     Environment_Task .. A_Protected_Entry;

   subtype Subprogram_Nodes is Callable_Nodes range A_Procedure .. A_Function;
   subtype Protected_Subprogram_Nodes is Callable_Nodes range
     A_Protected_Procedure .. A_Protected_Function;

   ------------------------------------------
   -- Callable entities and the call graph --
   ------------------------------------------

   --  The call graph consists of callable entities and caller-to-callee
   --  relations among them. The call graph gives a static and "flat" picture,
   --  it is not suitable for analysing properties specific for asynchronous
   --  processes. The call graph contains the following nodes:
   --
   --    Environment_Task
   --       This node represents an environment task, the call graph contains
   --       exactly one node of this kind. Environment task calls all the
   --       library level tasks, and it also calls the main subprogram if the
   --       main subprogram is specified. It also calls all the subprograms
   --       that that are called when from the elaboration of library packages.
   --       Nobody can call this node. This node represents the most global
   --       (library-level) scope.
   --
   --    A_Procedure
   --    A_Function
   --       Represent subprograms and subprogram instantiations. A subprogram
   --       is an entity declared by a subprogram declaration, subprogram body
   --       declaration or subprogram body stub in case there is no separate
   --       spec provided for the given subprogram. Subprogram renamings that
   --       are renamings as declarations are not counted for the call graph.
   --       In case of renaming as a body, if renaming can be resolved
   --       statically, this situation is considered as if the given subprogram
   --       calls the entity being renamed.
   --
   --    A_Null_Procedure
   --       Represents a null procedure (Ada 2005). The reason to define a
   --       separate kind for null procedures is that for these procedures it
   --       is known in advance that they do not have any code and they cannot
   --       call, access or update anything.
   --
   --    A_Task
   --       Represents a task that is viewed not as an asynchronous process,
   --       but as a procedure. That is, creation of a task is considered as
   --       a call to a procedure, where the task body is viewed as the body
   --       of the called procedure (in other words, we do not make the
   --       difference between "to call a process" and "to start a process").
   --       ??? Needs better documentation
   --
   --    A_Task_Entry
   --       The call graph considers a task entry call as a procedure call. The
   --       body of this "procedure" is a code of all the accept statements
   --       corresponding to this entry. Enclosing scope for a task entry is
   --       the task entity the entry belongs to.
   --
   --    A_Protected_Procedure
   --    A_Protected_Function
   --       We make the difference between "normal" subprograms and protected
   --       subprograms
   --
   --    A_Protected_Entry
   --       Similar to A_Task_Entry, but for protected entry we have the
   --       entry body instead of a set of accept statements code, and there
   --       is no "parent" reference here (the call graph does not contain
   --       any information about protected types and objects as whole
   --       entities)

   -------------------
   -- Data entities --
   -------------------

   --  To be documented...

   type GS_Node_Id is new Integer range 0 .. Integer'Last;
   --  Index of the nodes representing the global state

   No_GS_Node    : constant GS_Node_Id := GS_Node_Id'First;
   First_GS_Node : constant GS_Node_Id := No_GS_Node + 1;

   Environment_Task_Node : GS_Node_Id;
   --  Node representing the environment task

   subtype Existing_GS_Node_Id is GS_Node_Id
     range First_GS_Node .. GS_Node_Id'Last;

   type Reference_Kinds is
   --  Classifies the references from callable entities to data entities
     (Not_A_Reference,
      --  Either not applicable or non-defined
      Read,
      --  Read reference
      Write,
      --  Write reference:
      --  * variable in an assignment statement
      --  * actual for a OUT parameter
      Read_Write);
      --  Reference that can be both read and write:
      --  * actual for IN OUT parameter
      --  * prefix of 'Access and 'Unchecked_Access attribute, we are
      --    over-pessimistic in this case;

   ------------
   -- Scopes --
   ------------

   --  Scopes are statically enclosed bodies of callable entities,
   --  Environment_Task_Node represents the outermost (library-level) scope.
   --  Scopes are stored in the stack according to their nesting

   subtype Scope_Id is GS_Node_Id;
   No_Scope  : constant Scope_Id := Scope_Id'First;

   procedure Set_Current_Scope (Scope : GS_Node_Id; Scope_Tree_Node : Node_Id);
   --  Puts the argument on the top of the scope set. We need the corresponding
   --  tree node to check if an entity is global for the current scope.

   procedure Remove_Current_Scope;
   --  Pops the top scope from the stack. Raises Scope_Stack_Error if the scope
   --  stack is empty

   function Current_Scope return Scope_Id;
   --  Returns the top entity from the scope stack. Returns No_Scope_Ind if the
   --  stack is empty

   function Current_Scope_Tree_Node return Node_Id;
   --  Returns the tree node for the current scope. This node always belongs
   --  to the currently accessed tree.

   Scope_Stack_Error : exception;

   -----------
   -- Links --
   -----------

   --  The global data structure keeps links between nodes. All links are
   --  ordered, that is, a link goes from node A to node B, each link is stored
   --  for the node it goes from (that is, for A). There are two kinds of
   --  links - links that keep SLOCs of the place in the code that is a reason
   --  to store this link as a part of the global structure (such as a location
   --  of a subprogram call or a location of the reference to a data object),
   --  and links that keep only the nodes to which the link goes to, such
   --  links are used to represent such information as a list of all the
   --  entities called by a given subprogram, directly or indirectly, or a list
   --  of all the (global) data objects referenced by a given subprograms,
   --  directly or indirectly. If a link represent some indirect relation,
   --  there is no sense to keep a SLOC information for it. Keeping SLOCs for
   --  direct links allows to generate useful (back)trace information.

   --  The data structure keeps only one link for each event such as a call
   --  or a reference (that is, if a procedure A calls the procedure B many
   --  times (there are many procedure call statements targeted to B in the
   --  code of A), the node that represents A keeps only one link from A to B).
   --  Usually the SLOC stored as a part of this link corresponds to the
   --  (textually) first occurence of this event in the code.

   type SLOC_Link is record
      Node : GS_Node_Id;
      SLOC : String_Loc;
   end record;

   subtype Link is GS_Node_Id;

   ----------------------------
   -- Storage for node links --
   ----------------------------

   function "<" (Left, Right : SLOC_Link) return Boolean;
   function "=" (Left, Right : SLOC_Link) return Boolean;
   --  These functions compare only node Ids and ignore SLOCs.

   package SLOC_Node_Lists is new Ada.Containers.Ordered_Sets
     (Element_Type => SLOC_Link);
   --  Represents ordered sets of node links. Each link from this set contains
   --  a SLOC of the place from which this link originates

   package Node_Lists is new Ada.Containers.Ordered_Sets
     (Element_Type => Link);
   --  Represents ordered sets of node links (with no SLOC information)

   --  We need links to nodes with SLOCs in case if we have to generated
   --  useful call (back)traces (that say not only who is called, but also
   --  where it is called). But it is too expansive to use the link lists with
   --  SLOCs for big lists, such as list of all the calls (moreover, for an
   --  indirect call SLOC does not make very much sense)

   type SLOC_Node_List_Access is access SLOC_Node_Lists.Set;
   type Node_List_Access is access Node_Lists.Set;
   --  We need these access types to get node lists that represents call chains
   --  or other similar information for nodes in global structure

   --------------------------------------------------------
   -- General global structure entities/nodes properties --
   --------------------------------------------------------

   function Present (N : GS_Node_Id) return Boolean;
   function No      (N : GS_Node_Id) return Boolean;
   --  Check if the argument represents a nonexistent node

   function Last_Node return GS_Node_Id;
   --  Returtns the last node stored in the global state.

   function GS_Node_Kind (N : GS_Node_Id) return GS_Node_Kinds;
   --  Returns the kind of the argument node. Returns Not_A_Node if No (N).

   function Is_Callable_Node (N : GS_Node_Id) return Boolean;
   --  Checks if N represents a callable entity

   function Is_Subprogram_Node (N : GS_Node_Id) return Boolean;
   --  Checks if N represents a subprogram or a protected subprogram.

   function Is_Dispatching_Operation_Node (N : GS_Node_Id) return Boolean;
   --  Checks if N represents a dispatching operation. Accepts nodes that do
   --  not represent subprograms and returns False for them

   function Is_Abstract_Subprogram_Node (N : GS_Node_Id) return Boolean;
   --  Checks if N represents an abstract subprogram. Accepts nodes that do
   --  not represent subprograms and returns False for them

   function Is_Implicit_Subprogram_Node (N : GS_Node_Id) return Boolean;
   --  Checks if N represents an impilictly defined inherited subprogram.
   --  Accepts nodes that do not represent subprograms and returns False for
   --  them

   function GS_Node_SLOC (N : GS_Node_Id) return String_Loc;
   --  Returns the Source LOCation of the Ada construct the Node originated
   --  from. Returns Nil_String_Loc for Environment_Task node and in case when
   --  No (N)

   function GS_Node_Name (N : GS_Node_Id) return String;
   --  Retirns the name of the entity denoted by N. In case of expanded
   --  defining name the full expandsed name is returned
   --  ??? Should this function return Wide_String???

   function GS_Enclosed_CU_Name (N : GS_Node_Id) return String;
   --  Returns the name of the Compilation Unit that encloses the entity
   --  denoted by N;
   --  ??? Should this function return Wide_String???

   function GS_Node_Enclosing_Scope (N : GS_Node_Id) return Scope_Id;
   --  Returns the node that is a scope for the argument node. Returns
   --  No_GS_Node for Environment_Task node. Returns No_Scope if No (N).

   function GS_Node_Scope_Level (N : GS_Node_Id) return Natural;
   --  Returns the scope level. Node scope level is the nesting level of the
   --  scope the entity represented by the node belongs to (if the node itself
   --  is a scope, it is considered as belonging to itself). Environment_Task
   --  node has a scope level 1. Raises Constraint_Error is No (N)
   --  ??? See the documentation of Scope_Level field of the GS_Node_Record
   --  type. Needs to be cleaned up.

   function Is_RTL_Node (N : GS_Node_Id) return Boolean;
   --  Checks if the argument node represents an entity from some RTL unit.
   --  Raises Constraint_Error is No (N).

   function Is_Of_No_Interest (N : GS_Node_Id) return Boolean;
   --  Returns True if we are 100% sure that the given node cannot be of any
   --  interest for any analysis that can be performed on the global program
   --  structure. Raises Constraint_Error is No (N).

   function Enclosing_Source (N : GS_Node_Id) return SF_Id;
   --  Returns the ID of the source file the node has been extracted from.
   --  Returns No_SF_Id for Environment_Task. Raises Constraint_Error if
   --  No (N).

   function  Get_Application_Flag_1 (N : GS_Node_Id) return Boolean;
   procedure Set_Application_Flag_1 (N : GS_Node_Id; Val : Boolean);

   function Direct_Calls (N : GS_Node_Id) return SLOC_Node_List_Access;
   function All_Calls (N : GS_Node_Id) return Node_List_Access;
   --  Assuming that Is_Callable_Node (N), return (pointer to) the list of
   --  direct or all calls

   function Direct_Reads (N : GS_Node_Id) return SLOC_Node_List_Access;
   function Direct_Writes (N : GS_Node_Id) return SLOC_Node_List_Access;
   function Indirect_Reads (N : GS_Node_Id) return Node_List_Access;
   function Indirect_Writes (N : GS_Node_Id) return Node_List_Access;
   --  Assuming that Is_Callable_Node (N), return (pointer to) the list of
   --  direct or indirect reads or writes

   ---------------------------------------
   -- General global structure routines --
   ---------------------------------------

   procedure Initialize;
   --  Initializes the data structures needed to represent the global state.

   function Corresponding_Node
     (El              : Element;
      Enclosing_Scope : Scope_Id      := No_Scope;
      Expected_Kind   : GS_Node_Kinds := Not_A_Node;
      Unconditionally : Boolean       := False)
      return            GS_Node_Id;
   --  Returns the Id of the global structure node corresponding to El. If this
   --  El has not been added to the global structure yet, creates the
   --  corresponding node and returns it as the result. If set to non-empty
   --  value, Enclosing_Scope parameter is used to specify the enclosing scope
   --  for the node to be created.
   --
   --  If Expected_Kind is set to some value different from Not_A_Node, then
   --  this procedure looks for/creates the node of the specified kind.
   --
   --  Call to this function may result in creating more than one node in the
   --  global structure. For example, in the call graph, when creating a node
   --  for a callable entity, this function needs to set its scope link, and if
   --  the scope node does not exist, it is created, and the scope's scope
   --  node, and so on. For a type initialization routine it computes all the
   --  calls issued by this routine and creates the corresponding nodes and
   --  links in the call graph.
   --
   --  Creation of the new node may result in adding a new source file in the
   --  source files table (as a needed source). It may be the case when a call
   --  to this function adds more than one needed source (in case we create
   --  a node for some callable entity defined in a proper body of a subunit,
   --  then creation of the corresponding node may result in adding as a needed
   --  source the source for the body where the stub is located and the source
   --  of the corresponding spec).

   --------------------
   -- Debug routines --
   --------------------

   procedure Print_Global_Structure;
   --  Generates into Stderr the debug output for global data structure
   --  if the corresponding debug flag is ON (or if ASIS_UL.Options.Debug_Mode
   --  is ON, but we have to get rid of this flag), otherwise does nothing.

   procedure Print_Node (N : GS_Node_Id);
   --  Outputs into Stderr the debug information about the argument node N.
   --  format of the output

   procedure Print_List      (Node_List : Node_Lists.Set);
   procedure Print_SLOC_List (Node_List : SLOC_Node_Lists.Set);
   --  Debug routines, print into Stderr the debug image of the argument link
   --  list of nodes (without or with SLOC info).

private

   --  The entities below are needed only for the implementation of the
   --  global data structure.

   procedure Add_SLOC_Node_List_To_Node_List
     (Target : in out Node_Lists.Set;
      Source :        SLOC_Node_Lists.Set);
   --  This procedure is similar to the Union set container operation, the
   --  only difference is that Source is a link list with SLOCs, but Target
   --  does not have SLOCs (SLOCs parts from the elements of SOURCE are
   --  abandoned)

   -----------------------------------------------------
   --  General structure of the global structure node --
   -----------------------------------------------------

   type GS_Node_Record is record

      -------------------
      -- Common fields --
      -------------------

      --  Fields that exist for all entities. Should we use a discriminanted
      --  record here???

      Node_Kind : GS_Node_Kinds;

      SLOC : String_Loc;
      --  The full string location of the node (in case of generic
      --  instantiations includes the full istantiation chain)

      Name : String_Loc;
      --  Name of the entity represented by the node

      Source_File : SF_Id;
      --  Source file the given node belongs to.

      Enclosing_Scope : Scope_Id;

      Scope_Level : Natural;
      --  For a scope node, represents the nesting level of the scope.
      --  Is needed for analyzing if a data object is global for a scope, The
      --  scope level of an environment task is 1. If the node is not a scope,
      --  or if it corresponds to a subprogram for that the body has not been
      --  analyzed yet, the scope level is 0.

      Hash_Link : GS_Node_Id;
      --  Link to the next entry in the node table for the same hash code.

      Is_RTL_Node : Boolean;
      --  Indicates if the given node represents an entity defined in RTL.

      Is_Of_No_Interest : Boolean;
      --  Indicates if the node is of no interest for further analysis because
      --  of any reason. For example, a node represents a function that is an
      --  enumeration literal renaming - such function cannot call anything and
      --  it cannot refer to any data object

      --------------------------------------------------------------
      -- The meaning of the following fields depends on node kind --
      --------------------------------------------------------------

      Bool_Flag_1 : Boolean;
      --  Callable_Node -> Is_Body_Analyzed;
      --  Data_Node     -> ???

      Bool_Flag_2 : Boolean;
      --  Callable_Node -> Is_Renaming;
      --  Data_Node     -> ???

      Bool_Flag_3 : Boolean;
      --  Callable_Node ->
      --     A_Task              A_Task -> Is_Task_Type
      --     other callable nodes -> ???
      --  Data_Node     -> ???

      Bool_Flag_4 : Boolean;
      --  Callable_Node -> Is_Dispatching_Operation_Node;
      --  Data_Node     -> ???

      Bool_Flag_5 : Boolean;
      --  Callable_Node -> Is_Abstract_Subprogram_Node;
      --  Data_Node     -> ???

      Bool_Flag_6 : Boolean;
      --  Callable_Node -> Is_Implicit_Subprogram_Node;
      --  Data_Node     -> ???

      Bool_Flag_7 : Boolean;
      --  Callable_Node -> Is the missing body reported;
      --  Data_Node     -> ???

      Application_Flag_1 : Boolean;
      --  The usage of this flag is up to an application implemented on top of
      --  this call graph structure.

      SLOC_Node_List_1 : SLOC_Node_Lists.Set;
      --  Callable_Node -> Direct_Calls;
      --  Data_Node     -> ???

      SLOC_Node_List_2 : SLOC_Node_Lists.Set;
      --  For a callable node - references to global objects directly read by
      --  the callable entity.
      --  For a data node - list of all the callable entities that directly
      --  read the data entity.

      SLOC_Node_List_3 : SLOC_Node_Lists.Set;
      --  For a callable node - references to global objects directly written
      --  by the callable entity.
      --  For a data node - list of all the callable entities that directly
      --  write the data entity.

      Dispatching_Calls : SLOC_Node_Lists.Set;
      --  For a callable node - if Skip_Dispatching_Calls is ON, dispatching
      --  calls are stored here instead of SLOC_Node_List_1, then a tool
      --  can do transitive closure without taking into account dispatching
      --  calls, analyze the result, then copy stored dispatching calls
      --  into SLOC_Node_List_1 and do transitive closure again.
      --
      --  For a data node - not used

      Node_List_1 : Node_Lists.Set;
      --  Callable_Node -> All_Calls;
      --  Data_Node     -> ???

      Node_List_2 : Node_Lists.Set;
      --  Callable_Node ->
      --    Direct dispatching calls
      --  Data_Node     -> ???

      Node_List_3 : Node_Lists.Set;
      --  Callable_Node ->
      --    Is_Dispatching_Operation_Node ->
      --       Directly implementing subprograms
      --  Data_Node     -> ???

      Node_List_4 : Node_Lists.Set;
      --  Callable_Node ->
      --    Is_Dispatching_Operation_Node ->
      --       All implementing subprograms  ???
      --  Data_Node     -> ???

      Node_List_5 : Node_Lists.Set;
      --  For a callable node - references to global objects indirectly read by
      --  the callable entity.
      --  For a data node - list of all the callable entities that indirectly
      --  read the data entity.

      Node_List_6 : Node_Lists.Set;
      --  For a callable node - references to global objects indirectly written
      --  by the callable entity.
      --  For a data node - list of all the callable entities that indirectly
      --  write the data entity.

   end record;

   --------------------------------
   -- Access and update routines --
   --------------------------------

   type SLOC_Link_List_Types is
     (Calls,
      Dispatching_Calls,
      Direct_Read_References,
      Direct_Write_References,
      Indirect_Read_References,
      Indirect_Write_References
      --  To be continued...
     );
   --  Used to identify a list to operate with

   procedure Add_Link_To_SLOC_List
     (To_Node     : GS_Node_Id;
      Link_To_Add : SLOC_Link;
      To_List     : SLOC_Link_List_Types := Calls);
   --  Adds new link to the list pointed by To_List parameter of To_Node. If a
   --  link with the node from the argument link is already in the list,
   --  does nothing.

   type GS_Node_Record_Access is access GS_Node_Record;

   function Table (N : GS_Node_Id) return GS_Node_Record_Access;
   --  Mimics the notation Instantce_Name.Table (N) in the instantiation of the
   --  GNAT Table package. Returns the (pointer to the )Node with the index N
   --  from GS_Nodes_Table (see the body of the package). Raises
   --  Constraint_Error if a node with this index does not exsist.

   procedure Set_Is_Of_No_Interest (N : GS_Node_Id; Val : Boolean := True);
   --  Set the flag indicating if the callable entity is of no interest.

   --  Low-level procedures for setting fields tha are specific for
   --  node kind:
   procedure Set_Bool_Flag_1 (N : GS_Node_Id; Val : Boolean);
   procedure Set_Bool_Flag_2 (N : GS_Node_Id; Val : Boolean);
   procedure Set_Bool_Flag_3 (N : GS_Node_Id; Val : Boolean);
   procedure Set_Bool_Flag_4 (N : GS_Node_Id; Val : Boolean);
   procedure Set_Bool_Flag_5 (N : GS_Node_Id; Val : Boolean);
   procedure Set_Bool_Flag_6 (N : GS_Node_Id; Val : Boolean);
   procedure Set_Bool_Flag_7 (N : GS_Node_Id; Val : Boolean);

end ASIS_UL.Global_State;
