------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                      A S I S T A N T . F U N C A R R                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2016, Free Software Foundation, Inc.         --
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
-------------------------- ----------------------------------------------------

with Asis;  use Asis;

with Asis.Ada_Environments;
with Asis.Clauses;
with Asis.Compilation_Units;
with Asis.Compilation_Units.Relations;
with Asis.Data_Decomposition;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Elements;
with Asis.Expressions;
with Asis.Implementation;
with Asis.Implementation.Permissions;
with Asis.Limited_Views;
with Asis.Statements;
with Asis.Text;

with ASIStant.Enum_Mapping;

with ASIStant.Add_Ons; use ASIStant.Add_Ons;
with ASIStant.Browser;
with ASIStant.Find_Element;

package body ASIStant.FuncArr is

   procedure Initialize_Query_Arrays is
      package DDA renames Asis.Data_Decomposition;
   begin
      FCtxRetBool :=
        (
         Asis.Ada_Environments.Exists'Access,
         Asis.Ada_Environments.Is_Open'Access,
         Asis.Ada_Environments.Has_Associations'Access
        );

      FCtxRetCUnitList :=
        (
         Asis.Compilation_Units.Compilation_Unit_Bodies'Access,
         Asis.Compilation_Units.Compilation_Units'Access,
         Asis.Compilation_Units.Library_Unit_Declarations'Access
        );

      FCtxRetElemList :=
        (others =>
           Asis.Elements.Configuration_Pragmas'Access
        );

      FCtxRetNull :=
        (
         Asis.Ada_Environments.Close'Access,
         Asis.Ada_Environments.Dissociate'Access,
         Asis.Ada_Environments.Open'Access
        );

      FCtxRetString :=
        (
         Asis.Ada_Environments.Debug_Image'Access,
         Asis.Ada_Environments.Name'Access,
         Asis.Ada_Environments.Parameters'Access
        );

      FCtxStringStringRetNull :=
        (others =>
           Asis.Ada_Environments.Associate'Access
        );

      FCUnitBoolRetElemList :=
        (others =>
           Asis.Elements.Context_Clause_Elements'Access
        );

      FCUnitCtxRetCUnit :=
        (
         Asis.Compilation_Units.Corresponding_Body'Access,
         Asis.Compilation_Units.Corresponding_Declaration'Access,
         Asis.Compilation_Units.Corresponding_Parent_Declaration'Access,
         Asis.Compilation_Units.Corresponding_Subunit_Parent_Body'Access
        );

      FCUnitCtxRetCUnitList :=
        (
         Asis.Compilation_Units.Corresponding_Children'Access,
         Asis.Compilation_Units.Subunits'Access
        );

      FCUnitCUnitRetBool :=
        (
         Asis.Compilation_Units.Is_Equal'Access,
         Asis.Compilation_Units.Is_Identical'Access
        );

      FCUnitIntIntRetElem :=
        (others =>
           ASIStant.Find_Element'Access
        );

      FCUnitListRetBool :=
        (others =>
           Asis.Compilation_Units.Is_Nil'Access
        );

      FCUnitListCtxRetRelship :=
        (others =>
           Asis.Compilation_Units.Relations.Elaboration_Order'Access
        );

      FCUnitListCUnitListCtxStringRetRelship :=
        (others =>
           ASIStant.Enum_Mapping.Semantic_Dependence_Order'Access
        );

      FCUnitRetBool :=
        (
         Asis.Compilation_Units.Can_Be_Main_Program'Access,
         Asis.Compilation_Units.Exists'Access,
         Asis.Limited_Views.Has_Limited_View_Only'Access,
         Asis.Compilation_Units.Is_Body_Required'Access,
         Asis.Compilation_Units.Is_Nil'Access
        );

      FCUnitRetCtx :=
        (others =>
           Asis.Compilation_Units.Enclosing_Context'Access
        );

      FCUnitRetCUnit :=
        (
         Asis.Compilation_Units.Corresponding_Body'Access,
         Asis.Compilation_Units.Corresponding_Declaration'Access,
         Asis.Compilation_Units.Corresponding_Parent_Declaration'Access,
         Asis.Compilation_Units.Corresponding_Subunit_Parent_Body'Access
        );

      FCUnitRetCUnitList :=
        (
         Asis.Compilation_Units.Corresponding_Children'Access,
         Asis.Compilation_Units.Subunits'Access
        );

      FCUnitRetElem :=
        (
         ASIStant.Browser.Browse'Access,
         Asis.Elements.Unit_Declaration'Access
        );

      FCUnitRetElemList :=
        (others =>
           Asis.Elements.Compilation_Pragmas'Access
        );

      FCUnitRetString :=
        (
         Asis.Compilation_Units.Compilation_Command_Line_Options'Access,
         Asis.Compilation_Units.Debug_Image'Access,
         Asis.Compilation_Units.Object_Form'Access,
         Asis.Compilation_Units.Object_Name'Access,
         Asis.Compilation_Units.Text_Form'Access,
         Asis.Compilation_Units.Text_Name'Access,
         ASIStant.Enum_Mapping.Unit_Class'Access,
         Asis.Compilation_Units.Unit_Full_Name'Access,
         ASIStant.Enum_Mapping.Unit_Kind'Access,
         ASIStant.Enum_Mapping.Unit_Origin'Access,
         Asis.Compilation_Units.Unique_Name'Access
        );

      FCUnitStringRetBool :=
        (others =>
           Asis.Compilation_Units.Has_Attribute'Access
        );

      FCUnitStringRetString :=
        (others =>
           Asis.Compilation_Units.Attribute_Values'Access
        );

      FDDA_ArrCRetDDA_ArrC :=
        (others =>
           DDA.Array_Components'Access
        );

      FDDA_ArrCRetDDA_RecCList :=
        (
         DDA.Discriminant_Components'Access,
         DDA.Record_Components'Access
        );

      FDDA_ArrCRetElem :=
        (others =>
           DDA.Component_Indication'Access
        );

      FDDA_RecCRetDDA_ArrC :=
        (others =>
           DDA.Array_Components'Access
        );

      FDDA_RecCRetDDA_RecCList :=
        (
         DDA.Discriminant_Components'Access,
         DDA.Record_Components'Access
        );

      FDDA_RecCRetElem :=
        (others =>
           DDA.Component_Declaration'Access
        );

      FElemBoolRetElemList :=
        (
         Asis.Statements.Accept_Body_Exception_Handlers'Access,
         Asis.Statements.Accept_Body_Statements'Access,
         Asis.Statements.Block_Declarative_Items'Access,
         Asis.Statements.Block_Exception_Handlers'Access,
         Asis.Statements.Block_Statements'Access,
         Asis.Declarations.Body_Declarative_Items'Access,
         Asis.Declarations.Body_Exception_Handlers'Access,
         Asis.Declarations.Body_Statements'Access,
         Asis.Statements.Call_Statement_Parameters'Access,
         Asis.Clauses.Component_Clauses'Access,
         Asis.Definitions.Discriminant_Associations'Access,
         Asis.Statements.Extended_Return_Exception_Handlers'Access,
         Asis.Statements.Extended_Return_Statements'Access,
         Asis.Expressions.Function_Call_Parameters'Access,
         Asis.Declarations.Generic_Actual_Part'Access,
         Asis.Declarations.Generic_Formal_Part'Access,
         Asis.Statements.Handler_Statements'Access,
         Asis.Statements.Loop_Statements'Access,
         Asis.Declarations.Private_Part_Declarative_Items'Access,
         Asis.Definitions.Private_Part_Items'Access,
         Asis.Declarations.Protected_Operation_Items'Access,
         Asis.Expressions.Record_Component_Associations'Access,
         Asis.Definitions.Record_Components'Access,
         Asis.Statements.Sequence_Of_Statements'Access,
         Asis.Statements.Statement_Paths'Access,
         Asis.Definitions.Variants'Access,
         Asis.Declarations.Visible_Part_Declarative_Items'Access,
         Asis.Definitions.Visible_Part_Items'Access
        );

      FElemCtxRetElem :=
        (
         Asis.Declarations.Corresponding_Body'Access,
         Asis.Declarations.Corresponding_Body_Stub'Access,
         Asis.Declarations.Corresponding_Declaration'Access,
         Asis.Declarations.Corresponding_Subunit'Access,
         Asis.Declarations.Corresponding_Type_Declaration'Access
        );

      FElemElemBoolRetBool :=
        (others =>
           Asis.Expressions.Is_Referenced'Access
        );

      FElemElemBoolRetElemList :=
        (others =>
           Asis.Expressions.References'Access
        );

      FElemElemRetBool :=
        (
         Asis.Elements.Is_Equal'Access,
         Asis.Elements.Is_Identical'Access
        );

      FElemElemRetElem :=
        (others =>
           Asis.Elements.Enclosing_Element'Access
        );

      FElemIntIntRetLineList :=
        (others =>
           Asis.Text.Lines'Access
        );

      FElemListRetBool :=
        (others =>
           Asis.Elements.Is_Nil'Access
        );

      FElemRetBool :=
        (
         Asis.Declarations.Is_Private_Present'Access,
         Asis.Definitions.Is_Private_Present'Access,
         Asis.Elements.Has_Abstract'Access,
         Asis.Elements.Has_Aliased'Access,
         Asis.Elements.Has_Limited'Access,
         Asis.Elements.Has_Null_Exclusion'Access,
         Asis.Elements.Has_Private'Access,
         Asis.Elements.Has_Protected'Access,
         Asis.Elements.Has_Reverse'Access,
         Asis.Elements.Has_Synchronized'Access,
         Asis.Elements.Has_Tagged'Access,
         Asis.Elements.Has_Task'Access,
         Asis.Statements.Is_Declare_Block'Access,
         Asis.Statements.Is_Dispatching_Call'Access,
         Asis.Expressions.Is_Defaulted_Association'Access,
         Asis.Limited_Views.Is_From_Limited_View'Access,
         Asis.Expressions.Is_Generalized_Indexing'Access,
         Asis.Expressions.Is_Generalized_Reference'Access,
         Asis.Declarations.Is_Name_Repeated'Access,
         Asis.Statements.Is_Name_Repeated'Access,
         Asis.Elements.Is_Nil'Access,
         Asis.Elements.Is_Not_Null_Return'Access,
         Asis.Declarations.Is_Not_Overriding_Declaration'Access,
         Asis.Expressions.Is_Normalized'Access,
         Asis.Declarations.Is_Overriding_Declaration'Access,
         Asis.Elements.Is_Part_Of_Implicit'Access,
         Asis.Elements.Is_Part_Of_Inherited'Access,
         Asis.Elements.Is_Part_Of_Instance'Access,
         Asis.Expressions.Is_Prefix_Call'Access,
         Asis.Elements.Is_Prefix_Notation'Access,
         Asis.Declarations.Is_Private_Present'Access,
         Asis.Declarations.Is_Subunit'Access,
         Asis.Declarations.Is_Dispatching_Operation'Access,
         Text.Is_Text_Available'Access
        );

      FElemRetCUnit :=
        (others =>
           Asis.Elements.Enclosing_Compilation_Unit'Access
        );

      FElemRetDDA_ArrC :=
        (others =>
           DDA.Array_Components'Access
        );

      FElemRetDDA_RecCList :=
        (others =>
           DDA.Record_Components'Access
        );

      pragma Warnings (Off, "*obsolescent entity ""Membership_Test_Range""*");
      pragma Warnings (Off, "*obs* ""Membership_Test_Subtype_Mark""*");

      FElemRetElem  :=
        (
         Asis.Statements.Accept_Entry_Direct_Name'Access,
         Asis.Statements.Accept_Entry_Index'Access,
         Asis.Definitions.Access_To_Function_Result_Profile'Access,
         Asis.Definitions.Access_To_Object_Definition'Access,
         Asis.Expressions.Actual_Parameter'Access,
         Asis.Expressions.Allocator_Qualified_Expression'Access,
         Asis.Expressions.Allocator_Subtype_Indication'Access,
         Asis.Definitions.Ancestor_Subtype_Indication'Access,
         Asis.Definitions.Anonymous_Access_To_Object_Subtype_Mark'Access,
         Asis.Definitions.Array_Component_Definition'Access,
         Asis.Definitions.Aspect_Definition'Access,
         Asis.Definitions.Aspect_Mark'Access,
         Asis.Statements.Assignment_Expression'Access,
         Asis.Statements.Assignment_Variable_Name'Access,
         Asis.Statements.Associated_Message'Access,
         Asis.Expressions.Attribute_Designator_Identifier'Access,
         Asis.Declarations.Body_Block_Statement'Access,
         ASIStant.Browser.Browse'Access,
         Asis.Statements.Called_Name'Access,
         Asis.Statements.Case_Expression'Access,
         Asis.Statements.Choice_Parameter_Specification'Access,
         Asis.Clauses.Component_Clause_Position'Access,
         Asis.Clauses.Component_Clause_Range'Access,
         Asis.Definitions.Component_Definition_View'Access,
         Asis.Expressions.Component_Expression'Access,
         Asis.Definitions.Component_Subtype_Indication'Access,
         Asis.Statements.Condition_Expression'Access,
         Asis.Expressions.Converted_Or_Qualified_Expression'Access,
         Asis.Expressions.Converted_Or_Qualified_Subtype_Mark'Access,
         Asis.Declarations.Corresponding_Base_Entity'Access,
         Asis.Declarations.Corresponding_Body'Access,
         Asis.Declarations.Corresponding_Body_Stub'Access,
         Asis.Statements.Corresponding_Called_Entity'Access,
         Asis.Expressions.Corresponding_Called_Function'Access,
         Asis.Declarations.Corresponding_Constant_Declaration'Access,
         Asis.Declarations.Corresponding_Declaration'Access,
         Asis.Statements.Corresponding_Destination_Statement'Access,
         Asis.Elements.Corresponding_End_Name'Access,
         Asis.Statements.Corresponding_Entry'Access,
         Asis.Declarations.Corresponding_Equality_Operator'Access,
         Asis.Expressions.Corresponding_Expression_Type'Access,
         Asis.Expressions.Corresponding_Expression_Type_Definition'Access,
         Asis.Declarations.Corresponding_First_Subtype'Access,
         Asis.Declarations.Corresponding_Generic_Element'Access,
         Asis.Declarations.Corresponding_Last_Constraint'Access,
         Asis.Declarations.Corresponding_Last_Subtype'Access,
         Asis.Statements.Corresponding_Loop_Exited'Access,
         Asis.Expressions.Corresponding_Name_Declaration'Access,
         Asis.Expressions.Corresponding_Name_Definition'Access,
         Asis.Definitions.Corresponding_Parent_Subtype'Access,
         Asis.Definitions.Corresponding_Root_Type'Access,
         Asis.Declarations.Corresponding_Subprogram_Derivation'Access,
         Asis.Declarations.Corresponding_Subunit'Access,
         Asis.Declarations.Corresponding_Type'Access,
         Asis.Declarations.Corresponding_Type_Completion'Access,
         Asis.Declarations.Corresponding_Type_Declaration'Access,
         Asis.Declarations.Corresponding_Type_Partial_View'Access,
         Asis.Definitions.Corresponding_Type_Structure'Access,
         Asis.Declarations.Declaration_Subtype_Mark'Access,
         Asis.Declarations.Defining_Prefix'Access,
         Asis.Declarations.Defining_Selector'Access,
         Asis.Statements.Delay_Expression'Access,
         Asis.Definitions.Delta_Expression'Access,
         Asis.Definitions.Digits_Expression'Access,
         Asis.Definitions.Discriminant_Direct_Name'Access,
         Asis.Expressions.Discriminant_Expression'Access,
         Asis.Declarations.Discriminant_Part'Access,
         Asis.Elements.Enclosing_Element'Access,
         Asis.Declarations.Entry_Barrier'Access,
         Asis.Declarations.Entry_Family_Definition'Access,
         Asis.Declarations.Entry_Index_Specification'Access,
         Asis.Statements.Exit_Condition'Access,
         Asis.Statements.Exit_Loop_Name'Access,
         Asis.Expressions.Expression_Parenthesized'Access,
         Asis.Expressions.Extension_Aggregate_Expression'Access,
         Asis.Statements.For_Loop_Parameter_Specification'Access,
         Asis.Expressions.Formal_Parameter'Access,
         Asis.Declarations.Formal_Subprogram_Default'Access,
         Asis.Declarations.Generic_Unit_Name'Access,
         Asis.Limited_Views.Get_Nonlimited_View'Access,
         Asis.Statements.Goto_Label'Access,
         Asis.Statements.Guard'Access,
         Asis.Declarations.Initialization_Expression'Access,
         Asis.Definitions.Integer_Constraint'Access,
         Asis.Declarations.Iteration_Scheme_Name'Access, --  Ada 2012 name -???
         Asis.Expressions.Iterator_Specification'Access, --  Ada 2012 name -???
         Asis.Definitions.Lower_Bound'Access,
         Asis.Expressions.Membership_Test_Expression'Access,
         Asis.Expressions.Membership_Test_Range'Access,
         Asis.Expressions.Membership_Test_Subtype_Mark'Access,
         Asis.Clauses.Mod_Clause_Expression'Access,
         Asis.Definitions.Mod_Static_Expression'Access,
         Asis.Declarations.Object_Declaration_View'Access,
         Asis.Definitions.Parent_Subtype_Indication'Access,
         Asis.Expressions.Predicate'Access,
         Asis.Expressions.Dependent_Expression'Access,
         Asis.Expressions.Prefix'Access,
         Asis.Statements.Qualified_Expression'Access,
         Asis.Statements.Raised_Exception'Access,
         Asis.Definitions.Range_Attribute'Access,
         Asis.Definitions.Real_Range_Constraint'Access,
         Asis.Definitions.Record_Definition'Access,
         Asis.Declarations.Renamed_Entity'Access,
         Asis.Clauses.Representation_Clause_Expression'Access,
         Asis.Clauses.Representation_Clause_Name'Access,
         Asis.Statements.Requeue_Entry_Name'Access,
         Asis.Declarations.Result_Expression'Access,
         Asis.Declarations.Result_Profile'Access,
         Asis.Statements.Return_Expression'Access,
         Asis.Statements.Return_Object_Declaration'Access,
         Asis.Expressions.Selector'Access,
         Asis.Expressions.Short_Circuit_Operation_Left_Expression'Access,
         Asis.Expressions.Short_Circuit_Operation_Right_Expression'Access,
         Asis.Expressions.Slice_Range'Access,
         Asis.Declarations.Specification_Subtype_Definition'Access,
         Asis.Statements.Statement_Identifier'Access,
         Asis.Expressions.Subpool_Name'Access,
         Asis.Definitions.Subtype_Constraint'Access,
         Asis.Declarations.Subtype_Indication'Access, --  Ada 2012 name - ???
         Asis.Definitions.Subtype_Mark'Access,
         Asis.Declarations.Type_Declaration_View'Access,
         Asis.Definitions.Upper_Bound'Access,
         Asis.Statements.While_Condition'Access
        );

      pragma Warnings (On, "*obsolescent entity ""Membership_Test_Range""*");
      pragma Warnings (On, "*obs* ""Membership_Test_Subtype_Mark""*");

      FElemRetElemList :=
        (
         Asis.Statements.Aborted_Tasks'Access,
         Asis.Statements.Accept_Parameters'Access,
         Asis.Definitions.Access_To_Subprogram_Parameter_Profile'Access,
         Asis.Expressions.Array_Component_Associations'Access,
         Asis.Expressions.Array_Component_Choices'Access,
         Asis.Declarations.Aspect_Specifications'Access,
         Asis.Expressions.Attribute_Designator_Expressions'Access,
         Asis.Statements.Case_Path_Alternative_Choices'Access,
         Asis.Statements.Case_Statement_Alternative_Choices'Access,
         Asis.Clauses.Clause_Names'Access,
         Asis.Expressions.Corresponding_Name_Definition_List'Access,
         Asis.Elements.Corresponding_Pragmas'Access,
         Asis.Declarations.Corresponding_Representation_Clauses'Access,
         Asis.Definitions.Corresponding_Type_Operators'Access,
         DDA.All_Named_Components'Access,
         Asis.Declarations.Declaration_Interface_List'Access,
         Asis.Definitions.Definition_Interface_List'Access,
         Asis.Definitions.Discrete_Ranges'Access,
         Asis.Definitions.Discrete_Subtype_Definitions'Access,
         Asis.Expressions.Discriminant_Selector_Names'Access,
         Asis.Definitions.Discriminants'Access,
         Asis.Definitions.Enumeration_Literal_Declarations'Access,
         Asis.Statements.Exception_Choices'Access,
         Asis.Expressions.Expression_Paths'Access,
         Asis.Definitions.Implicit_Components'Access,
         Asis.Definitions.Implicit_Inherited_Declarations'Access,
         Asis.Definitions.Implicit_Inherited_Subprograms'Access,
         Asis.Expressions.Index_Expressions'Access,
         Asis.Definitions.Index_Subtype_Definitions'Access,
         Asis.Statements.Label_Names'Access,
         Asis.Expressions.Membership_Test_Choices'Access,
         Asis.Declarations.Names'Access,
         Asis.Declarations.Parameter_Profile'Access,
         Asis.Elements.Pragma_Argument_Associations'Access,
         Asis.Elements.Pragmas'Access,
         Asis.Expressions.Record_Component_Choices'Access,
         Asis.Definitions.Variant_Choices'Access
        );

      FElemRetInt :=
        (others =>
         --  !!!    Text.First_Line_Number'Access,
           Asis.Elements.Hash'Access
         --  !!!    Asis.Text.Last_Line_Number'Access
        );

      FElemRetLineList :=
        (others =>
           Asis.Text.Lines'Access
        );

      FElemRetSpan :=
        (
         Text.Compilation_Span'Access,
         Text.Compilation_Unit_Span'Access,
         Text.Element_Span'Access
        );

      FElemRetString :=
        (
         ASIStant.Enum_Mapping.Access_Type_Kind'Access,
         ASIStant.Enum_Mapping.Access_Definition_Kind'Access,
         ASIStant.Enum_Mapping.Association_Kind'Access,
         ASIStant.Enum_Mapping.Attribute_Kind'Access,
         ASIStant.Enum_Mapping.Clause_Kind'Access,
         ASIStant.Enum_Mapping.Constraint_Kind'Access,
         Asis.Elements.Debug_Image'Access,
         ASIStant.Enum_Mapping.Declaration_Kind'Access,
         ASIStant.Enum_Mapping.Declaration_Origin'Access,
         ASIStant.Enum_Mapping.Default_Kind'Access,
         Asis.Declarations.Defining_Name_Image'Access,
         ASIStant.Enum_Mapping.Defining_Name_Kind'Access,
         ASIStant.Enum_Mapping.Definition_Kind'Access,
         ASIStant.Enum_Mapping.Discrete_Range_Kind'Access,
         Text.Element_Image'Access,
         ASIStant.Enum_Mapping.Element_Kind'Access,
         ASIStant.Enum_Mapping.Expression_Kind'Access,
         ASIStant.Enum_Mapping.Formal_Type_Kind'Access,
         ASIStant.Enum_Mapping.Interface_Kind'Access,
         ASIStant.Enum_Mapping.Mode_Kind'Access,
         Asis.Expressions.Name_Image'Access,
         ASIStant.Enum_Mapping.Operator_Kind'Access,
         ASIStant.Enum_Mapping.Path_Kind'Access,
         Asis.Declarations.Position_Number_Image'Access,
         ASIStant.Enum_Mapping.Pragma_Kind'Access,
         Asis.Elements.Pragma_Name_Image'Access,
         ASIStant.Enum_Mapping.Representation_Clause_Kind'Access,
         Asis.Declarations.Representation_Value_Image'Access,
         ASIStant.Enum_Mapping.Root_Type_Kind'Access,
         ASIStant.Enum_Mapping.Statement_Kind'Access,
         ASIStant.Enum_Mapping.Trait_Kind'Access,
         ASIStant.Enum_Mapping.Type_Kind'Access,
         Asis.Expressions.Value_Image'Access
        );

      FElemSpanRetLineList :=
        (others =>
           Asis.Text.Lines'Access
        );

      FIntIntRetBool :=
        (
         Eq'Access,
         Gt'Access,
         Lt'Access
        );

      FIntIntRetInt :=
        (
         Add'Access,
         Sub'Access
        );

      FLineRetString :=
        (
         Asis.Text.Comment_Image'Access,
         Asis.Text.Debug_Image'Access,
         Asis.Text.Line_Image'Access,
         Asis.Text.Non_Comment_Image'Access
        );

      FRelshipRetCUnitList :=
        (
         Consistent'Access,
         Inconsistent'Access,
         Missing'Access,
         Circular'Access
        );

      FRetBool :=
        (
         Asis.Implementation.Permissions.Attributes_Are_Supported'Access,
         Asis.Implementation.Permissions.Default_In_Mode_Supported'Access,

         Asis.Implementation.Permissions.
           Discriminant_Associations_Normalized'Access,

         Asis.Implementation.Permissions.
           Function_Call_Parameters_Normalized'Access,

         Asis.Implementation.Permissions.Generic_Actual_Part_Normalized'Access,

         Asis.Implementation.Permissions.
           Generic_Macro_Expansion_Supported'Access,
         Asis.Implementation.Permissions.Implicit_Components_Supported'Access,

         Asis.Implementation.Permissions.
           Inherited_Declarations_Supported'Access,

         Asis.Implementation.Permissions.
           Inherited_Subprograms_Supported'Access,
         Asis.Implementation.Permissions.Is_Commentary_Supported'Access,
         Asis.Implementation.Is_Finalized'Access,

         Asis.Implementation.Permissions.
           Is_Formal_Parameter_Named_Notation_Supported'Access,

         Asis.Implementation.Is_Initialized'Access,
         Asis.Implementation.Permissions.Is_Line_Number_Supported'Access,
         Asis.Implementation.Permissions.Is_Prefix_Call_Supported'Access,

         Asis.Implementation.Permissions.
           Is_Span_Column_Position_Supported'Access,
         Asis.Implementation.Permissions.Object_Declarations_Normalized'Access,

         Asis.Implementation.Permissions.
           Predefined_Operations_Supported'Access,
         Asis.Implementation.Permissions.
           Record_Component_Associations_Normalized'Access
        );

      FRetCUnit :=
        (others =>
           ASIStant.Add_Ons.Nil_Compilation_Unit'Access
        );

      FRetCUnitList :=
        (others =>
           ASIStant.Add_Ons.Nil_Compilation_Unit_List'Access
        );

      FRetElem :=
        (others =>
           ASIStant.Add_Ons.Nil_Element'Access
        );

      FRetElemList :=
        (others =>
           ASIStant.Add_Ons.Nil_Element_List'Access
        );

      FRetRelship :=
        (others =>
           ASIStant.Add_Ons.Nil_Relationship'Access
        );

      FRetString :=
        (
         Implementation.ASIS_Implementor'Access,
         Implementation.ASIS_Implementor_Information'Access,
         Implementation.ASIS_Implementor_Version'Access,
         Implementation.ASIS_Version'Access,
         Asis.Compilation_Units.Attribute_Value_Delimiter'Access,
         Asis.Ada_Environments.Default_Name'Access,
         Asis.Ada_Environments.Default_Parameters'Access,
         Text.Delimiter_Image'Access,
         Implementation.Diagnosis'Access,
         ASIStant.Enum_Mapping.Status'Access
        );

      FSpanRetBool :=
        (others =>
           Text.Is_Nil'Access
        );

      FSpanRetInt :=
        (
         First_Column'Access,
         First_Line'Access,
         Last_Column'Access,
         Last_Line'Access
        );

      FStringCtxRetCUnit :=
        (
         Asis.Compilation_Units.Compilation_Unit_Body'Access,
         Asis.Compilation_Units.Library_Unit_Declaration'Access
        );

      FStringRetNull :=
        (
         Asis.Implementation.Finalize'Access,
         Asis.Implementation.Initialize'Access
        );

      FStringStringRetBool :=
        (
         Eq'Access,
         Gt'Access,
         Lt'Access
        );

      FStringStringRetString :=
        (others =>
           Concat'Access
        );

   end Initialize_Query_Arrays;

end ASIStant.FuncArr;
