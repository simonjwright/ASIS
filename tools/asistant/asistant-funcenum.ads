------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                     A S I S T A N T . F U N C E N U M                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1997-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- ASIStant  is  free  software;  you can redistribute  it and/or modify it --
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
-- ASIStant  is an evolution of  ASIStint tool that was created by  Vasiliy --
-- Fofanov  as  part  of  a  collaboration  between  Software   Engineering --
-- Laboratory  of the  Swiss  Federal Institute of Technology in  Lausanne, --
-- Switzerland,  and the Scientific Research Computer Center of the  Moscow --
-- University, Russia,  supported by the  Swiss National Science Foundation --
-- grant #7SUPJ048247, "Development of ASIS for GNAT with industry quality" --
--                                                                          --
-- ASIStant  is  distributed as a part of the  ASIS implementation for GNAT --
-- (ASIS-for-GNAT) and is maintained by AdaCore (http://www.adacore.com).   --
------------------------------------------------------------------------------

with Asis;
with Asis.Compilation_Units.Relations;
with Asis.Text;
with Asis.Data_Decomposition;

package ASIStant.FuncEnum is

------------------------------------------------------------------------------
--  ASIS queries enumeration and template information
------------------------------------------------------------------------------

------------------------------------------------------------------------------
--    All ASIS 95 queries must be supported, except a generic query
--    Traverse_Element. Failure to recognize any query which is not in the
--    Open Problems List is a bug, and the corresponding bug report will
--    be greatly appreciated.
------------------------------------------------------------------------------

------------------------------------------------------------------------------
--    OPEN PROBLEMS LIST (not implemented queries)
--      Traverse_Element not supported due to major conceptual limitations
--      Asis.Implementation.Set_Status
--      package Asis.Ada_Environments.Containers (all queries & types)
--      Asis.Compilation_Units.Enclosing_Container
--      package Asis.Compilation_Units.Times
--      package Asis.Ids
--      package Asis.Data_Decomposition and <...>.Portable_Transfer
------------------------------------------------------------------------------

   type Var_Type is
   (
      Par_Absent,

      Par_ATime,
      Par_Boolean,
      Par_Context,
      Par_CUnit,
      Par_CUnitList,
      Par_ElemList,
      Par_Element,
      Par_Integer,
      Par_Line,
      Par_Line_List,
      Par_Relationship,
      Par_String,
      Par_Span,

      Par_DDA_Array_Component,
      Par_DDA_Array_Component_List,
      Par_DDA_Record_Component,
      Par_DDA_Record_Component_List,

      Par_XXXXX --  for not implemented types
   );
   --  Type to represent variable types

   package DDA renames Asis.Data_Decomposition;

   type ElemList_Ptr     is access all Asis.Element_List;
   type CUnitList_Ptr    is access all Asis.Compilation_Unit_List;
   type Relship_Ptr      is access all
     Asis.Compilation_Units.Relations.Relationship;
   type LineList_Ptr     is access all Asis.Text.Line_List;
   type String_Ptr       is access all Wide_String;
   type DDA_ArrCList_Ptr is access all DDA.Array_Component_List;
   type DDA_RecCList_Ptr is access all DDA.Record_Component_List;

   type Query_Result (RType : Var_Type := Par_Absent) is
      record
         case RType is
            when Par_Absent    => null;
            when Par_String    => S  : String_Ptr;
            when Par_Boolean   => B  : Boolean;
            when Par_CUnit     => C  : Asis.Compilation_Unit;
            when Par_CUnitList => CL : CUnitList_Ptr;
            when Par_Element   => E  : Asis.Element;
            when Par_ElemList  => EL : ElemList_Ptr;
            when Par_Context | Par_Integer
                               => I  : Integer;
            when Par_Line      => L  : Asis.Text.Line;
            when Par_Line_List => LL : LineList_Ptr;
            when Par_Relationship
                               => R  : Relship_Ptr;
            when Par_Span      => Sp : Asis.Text.Span;
            when Par_DDA_Array_Component
                               => AC : DDA.Array_Component;

            when Par_DDA_Array_Component_List
                               => ACL : DDA_ArrCList_Ptr;
            when Par_DDA_Record_Component
                               => RC : DDA.Record_Component;
            when Par_DDA_Record_Component_List
                               => RCL : DDA_RecCList_Ptr;

            when others        => null;
         end case;
      end record;
   --  Universal ASIStant expression representation

   function "=" (Left, Right : Query_Result) return Boolean;
   --  We need to define this "=" becayse Query_Result has fields with abstract
   --  "=". Moreover, access fields need more smart comparison then just
   --  predefined "="

------------------------------------------------------------------------------
--    For brevity sake, the following convention is used to identify a query
--    profile: the types of query parameters are written one after another in
--    an abbreviated form, then the portion Ret<Return Type Abbreviation> is
--    appended. Below is a list of abbreviations:
--        Bool        - Boolean
--        Ctx         - Asis.Context
--        CUnit       - Asis.Compilation_Unit
--        CUnitList   - Asis.Compilation_Unit_List
--        DDA_ArrC     - DDA.Array_Component
--        DDA_ArrCList - DDA.Array_Component_List
--        DDA_RecC     - DDA.Record_Component
--        DDA_RecCList - DDA.Record_Component_List
--        Elem        - Asis.Element
--        ElemList    - Asis.Element_List
--        Int         - Integer/Asis.Asis_Integer
--        Line        - Asis.Text.Line
--        LineList    - Asis.Text.Line_List
--        Null        - (RetNull) Procedure
--        Relship     - Asis.Compilation_Units.Relationship
--        Span        - Asis.Text.Span
--
--    FOR EXAMPLE, the profile
--        (C : Asis.Compilation_Unit; I : Integer) return Asis.Element_List
--    will be represented as
--        CUnitIntRetElemList
------------------------------------------------------------------------------

--  Enumeration of all parameter and result type profiles of all queries
   type Func_Syntax_Enum is
     (CtxRetBool_Syn,
      CtxRetCUnitList_Syn,
      CtxRetElemList_Syn,
      CtxRetNull_Syn,
      CtxRetString_Syn,
      CtxStringStringRetNull_Syn,
      CUnitBoolRetElemList_Syn,
      CUnitCtxRetCUnit_Syn,
      CUnitCtxRetCUnitList_Syn,
      CUnitCUnitRetBool_Syn,
      CUnitIntIntRetElem_Syn,
      CUnitListCtxRetRelship_Syn,
      CUnitListCUnitListCtxStringRetRelship_Syn,
      CUnitListRetBool_Syn,
      CUnitRetBool_Syn,
      CUnitRetCtx_Syn,
      CUnitRetCUnit_Syn,
      CUnitRetCUnitList_Syn,
      CUnitRetElem_Syn,
      CUnitRetElemList_Syn,
      CUnitRetString_Syn,
      CUnitStringRetATime_Syn,
      CUnitStringRetBool_Syn,
      CUnitStringRetString_Syn,
      DDA_ArrCRetDDA_ArrC_Syn,
      DDA_ArrCRetDDA_RecCList_Syn,
      DDA_ArrCRetElem_Syn,
      DDA_RecCRetDDA_ArrC_Syn,
      DDA_RecCRetDDA_RecCList_Syn,
      DDA_RecCRetElem_Syn,
      ElemBoolRetElemList_Syn,
      ElemCtxRetElem_Syn,
      ElemElemBoolRetBool_Syn,
      ElemElemBoolRetElemList_Syn,
      ElemElemRetBool_Syn,
      ElemElemRetElem_Syn,
      ElemIntIntRetLineList_Syn,
      ElemListRetBool_Syn,
      ElemRetBool_Syn,
      ElemRetCUnit_Syn,
      ElemRetDDA_ArrC_Syn,
      ElemRetDDA_RecCList_Syn,
      ElemRetElem_Syn,
      ElemRetElemList_Syn,
      ElemRetInt_Syn,
      ElemRetLineList_Syn,
      ElemRetSpan_Syn,
      ElemRetString_Syn,
      ElemSpanRetLineList_Syn,
      IntIntRetBool_Syn,
      IntIntRetInt_Syn,
      LineRetString_Syn,
      RelshipRetCUnitList_Syn,
      RetBool_Syn,
      RetCUnit_Syn,
      RetCUnitList_Syn,
      RetElem_Syn,
      RetElemList_Syn,
      RetLine_Syn,
      RetRelship_Syn,
      RetSpan_Syn,
      RetString_Syn,
      SpanRetBool_Syn,
      SpanRetInt_Syn,
      StringCtxRetCUnit_Syn,
      StringRetNull_Syn,
      StringStringRetBool_Syn,
      StringStringRetString_Syn);

--  Enumeration of all queries
   type Switch_Index is (
   --  Placeholder
      Invalid_Index,
   --  CtxRetBool
      Exists,
      Is_Open,
      Has_Associations,
   --  CtxRetCUnitList
      Compilation_Unit_Bodies,
      Compilation_Units,
      Library_Unit_Declarations,
   --  CtxRetElemList
      Configuration_Pragmas,
   --  CtxRetNull
      Close,
      Dissociate,
      Open,
   --  CtxRetString
      Debug_Image_Ctx,
      Name,
      Parameters,
   --  CtxStringStringRetNull
      Associate,
   --  CUnitBoolRetElemList
      Context_Clause_Elements,
   --  CUnitCtxRetCUnit
      Corresponding_Body_CU_Ctx,
      Corresponding_Declaration_CU_Ctx,
   Corresponding_Parent_Declaration_Ctx,
   Corresponding_Subunit_Parent_Body_Ctx,
   --  CUnitCtxRetCUnitList
      Corresponding_Children_Ctx,
      Subunits_Ctx,
   --  CUnitCUnitRetBool
      Is_Equal_CU,
      Is_Identical_CU,
   --  CUnitIntIntRetElem
      Find_Element,
   --  CUnitListCtxRetRelship
      Elaboration_Order,
   --  CUnitListCUnitListCtxStringRetRelship
      Semantic_Dependence_Order,
   --  CUnitListRetBool
      Is_Nil_CUL,
   --  CUnitRetBool
      Can_Be_Main_Program,
      Exists_CU,
      Has_Limited_View_Only, --  ASOS 2005
      Is_Body_Required,
      Is_Nil_CU,
   --  CUnitRetCtx
      Enclosing_Context,
   --  CUnitRetCUnit
      Corresponding_Body_CU,
      Corresponding_Declaration_CU,
      Corresponding_Parent_Declaration,
      Corresponding_Subunit_Parent_Body,
   --  CUnitRetCUnitList
      Corresponding_Children,
      Subunits,
   --  CUnitRetElem
      Browse_CU,
      Unit_Declaration,
   --  CUnitRetElemList
      Compilation_Pragmas,
   --  CUnitRetString
      Compilation_Command_Line_Options,
      Debug_Image_CU,
      Object_Form,
      Object_Name,
      Text_Form,
      Text_Name,
      Unit_Class,
      Unit_Full_Name,
      Unit_Kind,
      Unit_Origin,
      Unique_Name,
   --  CUnitStringRetATime
      Attribute_Time,
   --  CUnitStringRetBool
      Has_Attribute,
   --  CUnitStringRetString
      Attribute_Values,
   --  DDA_ArrCRetDDA_ArrC
      DDA_Array_Components_2,
   --  DDA_ArrCRetDDA_RecCList
      DDA_Discriminant_Components_2,
      DDA_Record_Components_2,
   --  DDA_ArrCRetElem
      DDA_Component_Indication,
   --  DDA_RecCRetDDA_ArrC
      DDA_Array_Components_1,
   --  DDA_RecCRetDDA_RecCList
      DDA_Discriminant_Components_1,
      DDA_Record_Components_1,
   --  DDA_RecCRetElem
      DDA_Component_Declaration,
   --  ElemBoolRetElemList
      Accept_Body_Exception_Handlers,
      Accept_Body_Statements,
      Block_Declarative_Items,
      Block_Exception_Handlers,
      Block_Statements,
      Body_Declarative_Items,
      Body_Exception_Handlers,
      Body_Statements,
      Call_Statement_Parameters,
      Component_Clauses,
      Discriminant_Associations,
      Extended_Return_Exception_Handlers,  --  ASIS 2005
      Extended_Return_Statements,  --  ASIS 2005
      Function_Call_Parameters,
      Generic_Actual_Part,
      Generic_Formal_Part,
      Handler_Statements,
      Loop_Statements,
      Private_Part_Declarative_Items,
      Private_Part_Items,
      Protected_Operation_Items,
      Record_Component_Associations,
      Record_Components,
      Sequence_Of_Statements,
      Statement_Paths,
      Variants,
      Visible_Part_Declarative_Items,
      Visible_Part_Items,
   --  ElemCtxRetElem
      Corresponding_Body_Ctx,
      Corresponding_Body_Stub_Ctx,
      Corresponding_Declaration_Ctx,
      Corresponding_Subunit_Ctx,
      Corresponding_Type_Declaration_Ctx,
   --  ElemElemBoolRetBool
      Is_Referenced,
   --  ElemElemBoolRetElemList
      References,
   --  ElemElemRetBool
      Is_Equal,
      Is_Identical,
   --  ElemElemRetElem
      Enclosing_Element_EEE,
   --  ElemIntIntRetLineList
      Lines_2,
   --  ElemListRetBool
      Is_Nil_EL,
   --  ElemRetBool
      Declarations_Is_Private_Present,
      Definitions_Is_Private_Present,
      Has_Abstract,          --  ASIS 2005
      Has_Aliased,           --  ASIS 2005
      Has_Limited,           --  ASIS 2005
      Has_Null_Exclusion,    --  ASIS 2005
      Has_Private,           --  ASIS 2005
      Has_Protected,         --  ASIS 2005
      Has_Reverse,           --  ASIS 2005
      Has_Synchronized,      --  ASIS 2005
      Has_Tagged,            --  ASIS 2005
      Has_Task,              --  ASIS 2005
      Is_Declare_Block,
      Is_Dispatching_Call,
      Is_Defaulted_Association,
      Is_From_Limited_View,     --  ASIS 2005
      Is_Generalized_Indexing,  -- ASIS 2012
      Is_Generalized_Reference, -- ASIS 2012
      Declarations_Is_Name_Repeated,
      Statements_Is_Name_Repeated,
      Is_Nil,
      Is_Not_Null_Return,  --  ASIS 2005
      Is_Not_Overriding_Declaration,  --  ASIS 2005
      Is_Normalized,
      Is_Overriding_Declaration,  --  ASIS 2005
      Is_Part_Of_Implicit,
      Is_Part_Of_Inherited,
      Is_Part_Of_Instance,
      Is_Prefix_Call,
      Is_Prefix_Notation,  --  ASIS 2005
      Is_Private_Present,
      Is_Subunit,
      Is_Dispatching_Operation,
      Is_Text_Available,
   --  ElemRetCUnit
      Enclosing_Compilation_Unit,
   --  ElemRetDDA_ArrC
      DDA_Array_Components,
   --  ElemRetDDA_RecCList
      DDA_Discriminant_Components,
      DDA_Record_Components,
   --  ElemRetElem
      Accept_Entry_Direct_Name,
      Accept_Entry_Index,
      Access_To_Function_Result_Profile,
      Access_To_Object_Definition,
      Actual_Parameter,
      Allocator_Qualified_Expression,
      Allocator_Subtype_Indication,
      Ancestor_Subtype_Indication,
      Anonymous_Access_To_Object_Subtype_Mark,  -- ASIS 2005
      Array_Component_Definition,
      Aspect_Definition,                        -- ASIS 2012
      Aspect_Mark,                              -- ASIS 2012
      Assignment_Expression,
      Assignment_Variable_Name,
      Associated_Message,  --  ASIS 2005
      Attribute_Designator_Identifier,
      Body_Block_Statement,
      Browse,
      Called_Name,
      Case_Expression,
      Choice_Parameter_Specification,
      Component_Clause_Position,
      Component_Clause_Range,
      Component_Definition_View,  --  ASIS 2005
      Component_Expression,
      Component_Subtype_Indication,
      Condition_Expression,
      Converted_Or_Qualified_Expression,
      Converted_Or_Qualified_Subtype_Mark,
      Corresponding_Base_Entity,
      Corresponding_Body,
      Corresponding_Body_Stub,
      Corresponding_Called_Entity,
      Corresponding_Called_Function,
      Corresponding_Constant_Declaration,
      Corresponding_Declaration,
      Corresponding_Destination_Statement,
      Corresponding_End_Name,
      Corresponding_Entry,
      Corresponding_Equality_Operator,
      Corresponding_Expression_Type,
      Corresponding_Expression_Type_Definition,  --  ASIS 2005
      Corresponding_First_Subtype,
      Corresponding_Generic_Element,
      Corresponding_Last_Constraint,
      Corresponding_Last_Subtype,
      Corresponding_Loop_Exited,
      Corresponding_Name_Declaration,
      Corresponding_Name_Definition,
      Corresponding_Parent_Subtype,
      Corresponding_Root_Type,
      Corresponding_Subprogram_Derivation,
      Corresponding_Subunit,
      Corresponding_Type,
      Corresponding_Type_Completion,
      Corresponding_Type_Declaration,
      Corresponding_Type_Partial_View,
      Corresponding_Type_Structure,
      Declaration_Subtype_Mark,
      Defining_Prefix,
      Defining_Selector,
      Delay_Expression,
      Delta_Expression,
      Digits_Expression,
      Discriminant_Direct_Name,
      Discriminant_Expression,
      Discriminant_Part,
      Enclosing_Element,
      Entry_Barrier,
      Entry_Family_Definition,
      Entry_Index_Specification,
      Exit_Condition,
      Exit_Loop_Name,
      Expression_Parenthesized,
      Extension_Aggregate_Expression,
      For_Loop_Parameter_Specification,
      Formal_Parameter,
      Formal_Subprogram_Default,
      Generic_Unit_Name,
      Get_Nonlimited_View,         --  ASIS 2005
      Goto_Label,
      Guard,
      Initialization_Expression,
      Integer_Constraint,
      Iteration_Scheme_Name,           --  Ada 2012 query name - ???
      Iterator_Specification,          --  Ada 2012 query name - ???
      Lower_Bound,
      Membership_Test_Expression,
      Membership_Test_Range,
      Membership_Test_Subtype_Mark,
      Mod_Clause_Expression,
      Mod_Static_Expression,
      Object_Declaration_View,
      Parent_Subtype_Indication,
      Predicate,                       --  Ada 2012
      Dependent_Expression,            --  Ada 2012
      Prefix,
      Qualified_Expression,
      Raised_Exception,
      Range_Attribute,
      Real_Range_Constraint,
      Record_Definition,
      Renamed_Entity,
      Representation_Clause_Expression,
      Representation_Clause_Name,
      Requeue_Entry_Name,
      Result_Expression,         --  Ada 2012
      Result_Profile,
      Return_Expression,
      Return_Object_Declaration,  --  ASIS 2005
      Selector,
      Short_Circuit_Operation_Left_Expression,
      Short_Circuit_Operation_Right_Expression,
      Slice_Range,
      Specification_Subtype_Definition,
      Statement_Identifier,
      Subpool_Name,                    --  Ada 2012 query name - ??
      Subtype_Constraint,
      Subtype_Indication,              --  Ada 2012 query name - ???
      Subtype_Mark,
      Type_Declaration_View,
      Upper_Bound,
      While_Condition,
   --  ElemRetElemList
      Aborted_Tasks,
      Accept_Parameters,
      Access_To_Subprogram_Parameter_Profile,
      Array_Component_Associations,
      Array_Component_Choices,
      Aspect_Specifications,  -- ASIS 2012
      Attribute_Designator_Expressions,
      Case_Path_Alternative_Choices,
      Case_Statement_Alternative_Choices,
      Clause_Names,
      Corresponding_Name_Definition_List,
      Corresponding_Pragmas,
      Corresponding_Representation_Clauses,
      Corresponding_Type_Operators,
      DDA_All_Named_Components,
      Declaration_Interface_List,  --  ASIS 2005
      Definition_Interface_List,   --  ASIS 2005
      Discrete_Ranges,
      Discrete_Subtype_Definitions,
      Discriminant_Selector_Names,
      Discriminants,
      Enumeration_Literal_Declarations,
      Exception_Choices,
      Expression_Paths,
      Implicit_Components,
      Implicit_Inherited_Declarations,
      Implicit_Inherited_Subprograms,
      Index_Expressions,
      Index_Subtype_Definitions,
      Label_Names,
      Membership_Test_Choices,
      Names,
      Parameter_Profile,
      Pragma_Argument_Associations,
      Pragmas,
      Record_Component_Choices,
      Variant_Choices,
   --  ElemRetInt
      First_Line_Number,
      Hash,
      Last_Line_Number,
   --  ElemRetLineList
      Lines,
   --  ElemRetSpan
      Compilation_Span,
      Compilation_Unit_Span,
      Element_Span,
   --  ElemRetString (mainly additional queries to cover enum results)
      Access_Type_Kind,
      Access_Definition_Kind,
      Association_Kind,
      Attribute_Kind,
      Clause_Kind,
      Constraint_Kind,
      Debug_Image,
      Declaration_Kind,
      Declaration_Origin,
      Default_Kind,
      Defining_Name_Image,
      Defining_Name_Kind,
      Definition_Kind,
      Discrete_Range_Kind,
      Element_Image,
      Element_Kind,
      Expression_Kind,
      Formal_Type_Kind,
      Interface_Kind,  --  ASIS 2005
      Mode_Kind,
      Name_Image,
      Operator_Kind,
      Path_Kind,
      Position_Number_Image,
      Pragma_Kind,
      Pragma_Name_Image,
      Representation_Clause_Kind,
      Representation_Value_Image,
      Root_Type_Kind,
      Statement_Kind,
      Trait_Kind,
      Type_Kind,
      Value_Image,
   --  ElemSpanRetLineList
      Lines_1,
   --  IntIntRetBool
      Eq,
      Gt,
      Lt,
   --  IntIntRetInt
      Add,
      Sub,
   --  LineRetString
      Comment_Image,
      Debug_Image_L,
      Line_Image,
      Non_Comment_Image,
   --  RelshipRetCUnitList
      Consistent,
      Inconsistent,
      Missing,
      Circular,
   --  RetBool
      Attributes_Are_Supported,
      Default_In_Mode_Supported,
      Discriminant_Associations_Normalized,
      Function_Call_Parameters_Normalized,
      Generic_Actual_Part_Normalized,
      Generic_Macro_Expansion_Supported,
      Implicit_Components_Supported,
      Inherited_Declarations_Supported,
      Inherited_Subprograms_Supported,
      Is_Commentary_Supported,
      Is_Finalized,
      Is_Formal_Parameter_Named_Notation_Supported,
      Is_Initialized,
      Is_Line_Number_Supported,
      Is_Prefix_Call_Supported,
      Is_Span_Column_Position_Supported,
      Object_Declarations_Normalized,
      Predefined_Operations_Supported,
      Record_Component_Associations_Normalized,
   --  RetCUnit
      Nil_Compilation_Unit,
   --  RetCUnitList
      Nil_Compilation_Unit_List,
   --  RetElem
      Nil_Element,
   --  RetElemList
      Nil_Element_List,
   --  RetLine
      Nil_Line,
   --  RetRelship
      Nil_Relationship,
   --  RetSpan
      Nil_Span,
   --  RetString
      Asis_Implementor,
      Asis_Implementor_Information,
      Asis_Implementor_Version,
      Asis_Version,
      Attribute_Value_Delimiter,
      Default_Name,
      Default_Parameters,
      Delimiter_Image,
   Diagnosis,
   Status,
   --  SpanRetBool
      Is_Nil_Sp,
   --  SpanRetInt
      First_Column,
      First_Line,
      Last_Column,
      Last_Line,
   --  StringCtxRetCUnit
      Compilation_Unit_Body,
      Library_Unit_Declaration,
   --  StringRetNull
      Finalize,
      Initialize,
   --  StringStringRetBool
      Eq_SS,
      Gt_SS,
      Lt_SS,
   --  StringStringRetString
      Concat
   );

   pragma Ordered (Switch_Index);

   subtype Func_Param is Var_Type;

   type Profile_Range is new Integer range 0 .. 4;

   type Func_Syntax is array (Profile_Range) of Func_Param;
   --  0 is the type of return value, 1..4 - of params 1..4

   type Switch_Node is record
      From, To : Switch_Index;
      SelectID : Func_Syntax_Enum;
      Synt     : Func_Syntax;
   end record;

   type Switch_Info_Array is array (Positive range <>) of Switch_Node;

   Switch_Info : constant Switch_Info_Array := (
--  CtxRetBool
   (Exists, Has_Associations,
    CtxRetBool_Syn,
      (Par_Boolean, Par_Context, Par_Absent, Par_Absent, Par_Absent)),
--  CtxRetCUnitList
   (Compilation_Unit_Bodies, Library_Unit_Declarations,
    CtxRetCUnitList_Syn,
      (Par_CUnitList, Par_Context, Par_Absent, Par_Absent, Par_Absent)),
--  CtxRetElemList
   (Configuration_Pragmas, Configuration_Pragmas,
    CtxRetElemList_Syn,
   (Par_ElemList, Par_Context, Par_Absent, Par_Absent, Par_Absent)),
--  CtxRetNull
   (Close, Open,
    CtxRetNull_Syn,
      (Par_Absent, Par_Context, Par_Absent, Par_Absent, Par_Absent)),
--  CtxRetString
   (Debug_Image_Ctx, Parameters,
    CtxRetString_Syn,
      (Par_String, Par_Context, Par_Absent, Par_Absent, Par_Absent)),
--  CtxStringStringRetNull
   (Associate, Associate,
    CtxStringStringRetNull_Syn,
      (Par_Absent, Par_Context, Par_String, Par_String, Par_Absent)),
--  CUnitBoolRetElemList
   (Context_Clause_Elements, Context_Clause_Elements,
    CUnitBoolRetElemList_Syn,
      (Par_ElemList, Par_CUnit, Par_Boolean, Par_Absent, Par_Absent)),
--  CUnitCtxRetCUnit
   (Corresponding_Body_CU_Ctx, Corresponding_Subunit_Parent_Body_Ctx,
    CUnitCtxRetCUnit_Syn,
      (Par_CUnit, Par_CUnit, Par_Context, Par_Absent, Par_Absent)),
--  CUnitCtxRetCUnitList
   (Corresponding_Children_Ctx, Subunits_Ctx,
    CUnitCtxRetCUnitList_Syn,
      (Par_CUnitList, Par_CUnit, Par_Context, Par_Absent, Par_Absent)),
--  CUnitCUnitRetBool
   (Is_Equal_CU, Is_Identical_CU,
    CUnitCUnitRetBool_Syn,
      (Par_Boolean, Par_CUnit, Par_CUnit, Par_Absent, Par_Absent)),
--  CUnitIntIntRetElem
   (Find_Element, Find_Element,
    CUnitIntIntRetElem_Syn,
      (Par_Element, Par_CUnit, Par_Integer, Par_Integer, Par_Absent)),
--  CUnitListCtxRetRelship
   (Elaboration_Order, Elaboration_Order,
    CUnitListCtxRetRelship_Syn,
      (Par_Relationship, Par_CUnitList, Par_Context, Par_Absent, Par_Absent)),
--  CUnitListCUnitListCtxStringRetRelship
   (Semantic_Dependence_Order, Semantic_Dependence_Order,
    CUnitListCUnitListCtxStringRetRelship_Syn,
      (Par_Relationship, Par_CUnitList, Par_CUnitList, Par_Context, Par_String)
   ),
--  CUnitListRetBool
   (Is_Nil_CUL, Is_Nil_CUL,
    CUnitListRetBool_Syn,
      (Par_Boolean, Par_CUnitList, Par_Absent, Par_Absent, Par_Absent)),
--  CUnitRetBool
   (Can_Be_Main_Program, Is_Nil_CU,
    CUnitRetBool_Syn,
      (Par_Boolean, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
--  CUnitRetCtx
   (Enclosing_Context, Enclosing_Context,
    CUnitRetCtx_Syn,
      (Par_Context, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
--  CUnitRetCUnit
   (Corresponding_Body_CU, Corresponding_Subunit_Parent_Body,
    CUnitRetCUnit_Syn,
      (Par_CUnit, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
--  CUnitRetCUnitList
   (Corresponding_Children, Subunits,
    CUnitRetCUnitList_Syn,
      (Par_CUnitList, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
--  CUnitRetElem
   (Browse_CU, Unit_Declaration,
    CUnitRetElem_Syn,
      (Par_Element, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
--  CUnitRetElemList
   (Compilation_Pragmas, Compilation_Pragmas,
    CUnitRetElemList_Syn,
      (Par_ElemList, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
--  CUnitRetString
   (Compilation_Command_Line_Options, Unique_Name,
    CUnitRetString_Syn,
      (Par_String, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
--  CUnitStringRetATime
   (Attribute_Time, Attribute_Time,
    CUnitStringRetATime_Syn,
      (Par_ATime, Par_CUnit, Par_String, Par_Absent, Par_Absent)),
--  CUnitStringRetBool
   (Has_Attribute, Has_Attribute,
    CUnitStringRetBool_Syn,
      (Par_Boolean, Par_CUnit, Par_String, Par_Absent, Par_Absent)),
--  CUnitStringRetString
   (Attribute_Values, Attribute_Values,
    CUnitStringRetString_Syn,
      (Par_String, Par_CUnit, Par_String, Par_Absent, Par_Absent)),
--  DDA_ArrCRetDDA_ArrC
   (DDA_Array_Components_2, DDA_Array_Components_2,
    DDA_ArrCRetDDA_ArrC_Syn,
      (Par_DDA_Array_Component, Par_DDA_Array_Component, Par_Absent,
       Par_Absent, Par_Absent)),
--  DDA_ArrCRetDDA_RecCList
   (DDA_Discriminant_Components_2, DDA_Record_Components_2,
    DDA_ArrCRetDDA_RecCList_Syn,
      (Par_DDA_Record_Component_List, Par_DDA_Array_Component, Par_Absent,
       Par_Absent, Par_Absent)),
--  DDA_ArrCRetElem
   (DDA_Component_Indication, DDA_Component_Indication,
    DDA_ArrCRetElem_Syn,
      (Par_Element, Par_DDA_Array_Component, Par_Absent, Par_Absent,
       Par_Absent)),
--  DDA_RecCRetDDA_ArrC
   (DDA_Array_Components_1, DDA_Array_Components_1,
    DDA_RecCRetDDA_ArrC_Syn,
      (Par_DDA_Array_Component, Par_DDA_Record_Component, Par_Absent,
       Par_Absent, Par_Absent)),
--  DDA_RecCRetDDA_RecCList
   (DDA_Discriminant_Components_1, DDA_Record_Components_1,
    DDA_RecCRetDDA_RecCList_Syn,
      (Par_DDA_Record_Component_List, Par_DDA_Record_Component, Par_Absent,
       Par_Absent, Par_Absent)),
--  DDA_RecCRetElem
   (DDA_Component_Declaration, DDA_Component_Declaration,
    DDA_RecCRetElem_Syn,
      (Par_Element, Par_DDA_Record_Component, Par_Absent, Par_Absent,
       Par_Absent)),
--  ElemBoolRetElemList
   (Accept_Body_Exception_Handlers, Visible_Part_Items,
    ElemBoolRetElemList_Syn,
      (Par_ElemList, Par_Element, Par_Boolean, Par_Absent, Par_Absent)),
--  ElemCtxRetElem
   (Corresponding_Body, Corresponding_Type_Declaration_Ctx,
    ElemCtxRetElem_Syn,
      (Par_Element, Par_Element, Par_Context, Par_Absent, Par_Absent)),
--  ElemElemBoolRetBool
   (Is_Referenced, Is_Referenced,
    ElemElemBoolRetBool_Syn,
      (Par_Boolean, Par_Element, Par_Element, Par_Boolean, Par_Absent)),
--  ElemElemBoolRetElemList
   (References, References,
    ElemElemBoolRetElemList_Syn,
      (Par_ElemList, Par_Element, Par_Element, Par_Boolean, Par_Absent)),
--  ElemElemRetBool
   (Is_Equal, Is_Identical,
    ElemElemRetBool_Syn,
      (Par_Boolean, Par_Element, Par_Element, Par_Absent, Par_Absent)),
--  ElemElemRetElem
   (Enclosing_Element_EEE, Enclosing_Element_EEE,
    ElemElemRetElem_Syn,
      (Par_Element, Par_Element, Par_Element, Par_Absent, Par_Absent)),
--  ElemIntIntRetLineList
   (Lines_2, Lines_2,
    ElemIntIntRetLineList_Syn,
      (Par_Line_List, Par_Element, Par_Integer, Par_Integer, Par_Absent)),
--  ElemListRetBool
   (Is_Nil_EL, Is_Nil_EL,
    ElemListRetBool_Syn,
      (Par_Boolean, Par_ElemList, Par_Absent, Par_Absent, Par_Absent)),
--  ElemRetBool
   (Declarations_Is_Private_Present, Is_Text_Available,
    ElemRetBool_Syn,
      (Par_Boolean, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
--  ElemRetCUnit
   (Enclosing_Compilation_Unit, Enclosing_Compilation_Unit,
    ElemRetCUnit_Syn,
      (Par_CUnit, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
--  ElemRetDDA_ArrC
   (DDA_Array_Components, DDA_Array_Components,
    ElemRetDDA_ArrC_Syn,
      (Par_DDA_Array_Component, Par_Element, Par_Absent, Par_Absent,
       Par_Absent)),
--  ElemRetDDA_RecCList
   (DDA_Discriminant_Components, DDA_Record_Components,
    ElemRetDDA_RecCList_Syn,
      (Par_DDA_Record_Component_List, Par_Element, Par_Absent, Par_Absent,
       Par_Absent)),
--  ElemRetElem
   (Accept_Entry_Direct_Name, While_Condition,
    ElemRetElem_Syn,
      (Par_Element, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
--  ElemRetElemList
   (Aborted_Tasks, Variant_Choices,
    ElemRetElemList_Syn,
      (Par_ElemList, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
--  ElemRetInt
   (First_Line_Number, Last_Line_Number,
    ElemRetInt_Syn,
      (Par_Integer, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
--  ElemRetLineList
   (Lines, Lines,
    ElemRetLineList_Syn,
      (Par_Line_List, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
--  ElemRetSpan
   (Compilation_Span, Element_Span,
    ElemRetSpan_Syn,
      (Par_Span, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
--  ElemRetString
   (Access_Type_Kind, Value_Image,
    ElemRetString_Syn,
      (Par_String, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
--  ElemSpanRetLineList
   (Lines_1, Lines_1,
    ElemSpanRetLineList_Syn,
      (Par_Line_List, Par_Element, Par_Span, Par_Absent, Par_Absent)),
--  IntIntRetBool
   (Eq, Lt,
    IntIntRetBool_Syn,
      (Par_Boolean, Par_Integer, Par_Integer, Par_Absent, Par_Absent)),
--  IntIntRetInt
   (Add, Sub,
    IntIntRetInt_Syn,
      (Par_Integer, Par_Integer, Par_Integer, Par_Absent, Par_Absent)),
--  LineRetString
   (Comment_Image, Non_Comment_Image,
    LineRetString_Syn,
      (Par_String, Par_Line, Par_Absent, Par_Absent, Par_Absent)),
--  RelshipRetCUnitList
   (Consistent, Circular,
    RelshipRetCUnitList_Syn,
      (Par_CUnitList, Par_Relationship, Par_Absent, Par_Absent, Par_Absent)),
--  RetBool
   (Attributes_Are_Supported, Record_Component_Associations_Normalized,
    RetBool_Syn,
      (Par_Boolean, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
--  RetCUnit
   (Nil_Compilation_Unit, Nil_Compilation_Unit,
    RetCUnit_Syn,
      (Par_CUnit, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
--  RetCUnitList
   (Nil_Compilation_Unit_List, Nil_Compilation_Unit_List,
    RetCUnitList_Syn,
      (Par_CUnitList, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
--  RetElem
   (Nil_Element, Nil_Element,
    RetElem_Syn,
      (Par_Element, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
--  RetElemList
   (Nil_Element_List, Nil_Element_List,
    RetElemList_Syn,
      (Par_ElemList, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
--  RetLine
   (Nil_Line, Nil_Line,
    RetLine_Syn,
      (Par_Line, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
--  RetRelship
   (Nil_Relationship, Nil_Relationship,
    RetRelship_Syn,
      (Par_Relationship, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
--  RetSpan
   (Nil_Span, Nil_Span,
    RetSpan_Syn,
      (Par_Span, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
--  RetString
   (Asis_Implementor, Status,
    RetString_Syn,
      (Par_String, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
--  SpanRetBool
   (Is_Nil_Sp, Is_Nil_Sp,
    SpanRetBool_Syn,
      (Par_Boolean, Par_Span, Par_Absent, Par_Absent, Par_Absent)),
--  SpanRetInt
   (First_Column, Last_Line,
    SpanRetInt_Syn,
      (Par_Integer, Par_Span, Par_Absent, Par_Absent, Par_Absent)),
--  StringCtxRetCUnit
   (Compilation_Unit_Body, Library_Unit_Declaration,
    StringCtxRetCUnit_Syn,
      (Par_CUnit, Par_String, Par_Context, Par_Absent, Par_Absent)),
--  StringRetNull
   (Finalize, Initialize,
    StringRetNull_Syn,
      (Par_Absent, Par_String, Par_Absent, Par_Absent, Par_Absent)),
--  StringStringRetBool
   (Eq_SS, Lt_SS,
    StringStringRetBool_Syn,
      (Par_Boolean, Par_String, Par_String, Par_Absent, Par_Absent)),
--  StringStringRetString
   (Concat, Concat,
    StringStringRetString_Syn,
      (Par_String, Par_String, Par_String, Par_Absent, Par_Absent))
   );

   SI_LENGTH : constant Positive := Switch_Info'Length;

end ASIStant.FuncEnum;
