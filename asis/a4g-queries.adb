------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . Q U E R I E S                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2016, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR  PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with ASIS-for-GNAT;  see file --
-- COPYING.  If not,  write  to the  Free Software Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
-- The   original   version  of  this  component  has  been  developed   by --
-- Jean-Charles Marteau  (Jean-Charles.Marteau@ensimag.imag.fr)  and  Serge --
-- Reboul  (Serge.Reboul@ensimag.imag.fr),  ENSIMAG  High  School Graduates --
-- (Computer sciences) Grenoble, France in Sema Group Grenoble, France. Now --
-- this component is maintained by the ASIS team at AdaCore                 --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Asis.Clauses; use Asis.Clauses;
with Asis.Declarations; use Asis.Declarations;
with Asis.Definitions; use Asis.Definitions;
with Asis.Elements; use Asis.Elements;
pragma Elaborate (Asis.Elements);
--  Elaborate needed to avoid elaboration cycle
with Asis.Expressions; use Asis.Expressions;
with Asis.Extensions; use Asis.Extensions;
pragma Elaborate (Asis.Extensions);
--  Elaborate needed to avoid elaboration cycle
with Asis.Statements; use Asis.Statements;

package body A4G.Queries is

   ------------
   -- Tables --
   ------------

   --  There are two tables below, initialized by giant aggregates. These drive
   --  the Appropriate_Queries functions. The first maps element kinds to
   --  sequences of queries, and the second maps queries to Func_Elems.

   -----------------
   -- Query_Table --
   -----------------

   --  Query_Table is a mapping from element kinds (flat) to the sequence of
   --  structural queries that are appropriate for that kind.

   --  Note that some of the bodies are given out of order from the usual
   --  style; this is necessary to avoid calls before elaboration.

   type Query_Table_Type is array (Flat_Element_Kinds'Base) of Query_List_Ptr;

   function "+" (Qs : Query_List) return Query_List_Ptr;
   function "+" (Q : Structural_Queries) return Query_List_Ptr;
   --  "+" is used in the aggregate below that initializes the Query_Table, as
   --  a shorthand for allocating the array. The second version is just a
   --  workaround to the fact that Ada doesn't allow singleton positional
   --  arrays.

   function "+" (Qs : Query_List) return Query_List_Ptr is
   begin
      return new Query_List'(Qs);
   end "+";

   function "+" (Q : Structural_Queries) return Query_List_Ptr is
   begin
      return +(1 => Q);
   end "+";

   Query_Table : constant Query_Table_Type :=
     (Not_An_Element =>
        No_Queries'Access,
      Flat_Pragma_Kinds =>
        +Pragma_Argument_Associations,
      Flat_Defining_Simple_Name_Kinds =>
        No_Queries'Access,
      A_Defining_Expanded_Name =>
        +(Defining_Prefix,
          Defining_Selector),
      An_Ordinary_Type_Declaration =>
        +(Names,
          Discriminant_Part,
          Type_Declaration_View,
          Aspect_Specifications),
      A_Task_Type_Declaration =>
        +(Names,
          Discriminant_Part,
          Aspect_Specifications,
          Declaration_Interface_List,
          Type_Declaration_View),
      A_Protected_Type_Declaration =>
        +(Names,
          Discriminant_Part,
          Aspect_Specifications,
          Declaration_Interface_List,
          Type_Declaration_View),
      An_Incomplete_Type_Declaration =>
        +(Names,
          Discriminant_Part,
          Aspect_Specifications),
      A_Tagged_Incomplete_Type_Declaration =>
        +(Names,
          Discriminant_Part,
          Aspect_Specifications),
      A_Private_Type_Declaration =>
        +(Names,
          Discriminant_Part,
          Type_Declaration_View,
          Aspect_Specifications),
      A_Private_Extension_Declaration =>
        +(Names,
          Discriminant_Part,
          Type_Declaration_View,
          Aspect_Specifications),
      A_Subtype_Declaration =>
        +(Names,
          Type_Declaration_View,
          Aspect_Specifications),
      A_Variable_Declaration =>
        +(Names,
          Has_Aliased,
          Object_Declaration_View,
          Initialization_Expression,
          Aspect_Specifications),
      A_Constant_Declaration =>
        +(Names,
          Has_Aliased,
          Object_Declaration_View,
          Initialization_Expression,
          Aspect_Specifications),
      A_Deferred_Constant_Declaration =>
        +(Names,
          Has_Aliased,
          Object_Declaration_View,
          Aspect_Specifications),
      A_Single_Task_Declaration =>
        +(Names,
          Aspect_Specifications,
          Declaration_Interface_List,
          Object_Declaration_View),
      A_Single_Protected_Declaration =>
        +(Names,
          Aspect_Specifications,
          Declaration_Interface_List,
          Object_Declaration_View),
      A_Flat_Number_Declaration =>
        +(Names,
          Initialization_Expression),
      An_Enumeration_Literal_Specification =>
        +Names,
      A_Discriminant_Specification =>
        +(Names,
          Has_Null_Exclusion,
          Object_Declaration_View,
          Initialization_Expression),
      A_Component_Declaration =>
        +(Names,
          Has_Aliased, -- Why needed???
          Object_Declaration_View,
          Initialization_Expression,
          Aspect_Specifications),
      A_Loop_Parameter_Specification =>
        +(Names,
          Has_Reverse,
          Specification_Subtype_Definition),
      A_Generalized_Iterator_Specification =>
        +(Names,
          Has_Reverse,
          Iteration_Scheme_Name),
      An_Element_Iterator_Specification =>
        +(Names,
          Subtype_Indication,
          Has_Reverse,
          Iteration_Scheme_Name),
      A_Procedure_Declaration =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Parameter_Profile,
          Has_Abstract,
          Aspect_Specifications),
      A_Function_Declaration =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Parameter_Profile,
          Is_Not_Null_Return,
          Result_Profile,
          Has_Abstract,
          Aspect_Specifications),
      A_Parameter_Specification =>
        +(Names,
          Has_Aliased,
          Has_Null_Exclusion,
          Object_Declaration_View,
          Initialization_Expression),
      A_Procedure_Body_Declaration =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Parameter_Profile,
          Aspect_Specifications,
          Body_Declarative_Items,
          Body_Statements,
          Body_Exception_Handlers),
      A_Function_Body_Declaration =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Parameter_Profile,
          Is_Not_Null_Return,
          Result_Profile,
          Aspect_Specifications,
          Body_Declarative_Items,
          Body_Statements,
          Body_Exception_Handlers),
      A_Return_Variable_Specification =>
        +(Names,
          Has_Aliased,
          Object_Declaration_View,
          Initialization_Expression),
      A_Return_Constant_Specification =>
        +(Names,
          Has_Aliased,
          Object_Declaration_View,
          Initialization_Expression),
      A_Null_Procedure_Declaration =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Parameter_Profile,
          Aspect_Specifications),
      An_Expression_Function_Declaration =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Parameter_Profile,
          Is_Not_Null_Return,
          Result_Profile,
          Result_Expression,
          Aspect_Specifications),
      A_Package_Declaration =>
        +(Names,
          Aspect_Specifications,
          Visible_Part_Declarative_Items,
          Private_Part_Declarative_Items),
      A_Package_Body_Declaration =>
        +(Names,
          Aspect_Specifications,
          Body_Declarative_Items,
          Body_Statements,
          Body_Exception_Handlers),
      An_Object_Renaming_Declaration =>
        +(Names,
          Has_Null_Exclusion,
          Object_Declaration_View,
          Renamed_Entity,
          Aspect_Specifications),
      An_Exception_Renaming_Declaration =>
        +(Names,
          Renamed_Entity,
          Aspect_Specifications),
      A_Package_Renaming_Declaration =>
        +(Names,
          Renamed_Entity,
          Aspect_Specifications),
      A_Procedure_Renaming_Declaration =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Parameter_Profile,
          Renamed_Entity,
          Aspect_Specifications),
      A_Function_Renaming_Declaration =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Parameter_Profile,
          Is_Not_Null_Return,
          Result_Profile,
          Renamed_Entity,
          Aspect_Specifications),
      A_Generic_Package_Renaming_Declaration =>
        +(Names,
          Renamed_Entity,
          Aspect_Specifications),
      A_Generic_Procedure_Renaming_Declaration =>
        +(Names,
          Renamed_Entity,
          Aspect_Specifications),
      A_Generic_Function_Renaming_Declaration =>
        +(Names,
          Renamed_Entity,
          Aspect_Specifications),
      A_Task_Body_Declaration =>
        +(Names,
          Aspect_Specifications,
          Body_Declarative_Items,
          Body_Statements,
          Body_Exception_Handlers),
      A_Protected_Body_Declaration =>
        +(Names,
          Aspect_Specifications,
          Protected_Operation_Items),
      An_Entry_Declaration =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Entry_Family_Definition,
          Parameter_Profile,
          Aspect_Specifications),
      An_Entry_Body_Declaration =>
        +(Names,
          Entry_Index_Specification,
          Parameter_Profile,
          Entry_Barrier,
          Body_Declarative_Items,
          Body_Statements,
          Body_Exception_Handlers),
      An_Entry_Index_Specification =>
        +(Names,
          Specification_Subtype_Definition),
      A_Procedure_Body_Stub =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Parameter_Profile,
          Aspect_Specifications),
      A_Function_Body_Stub =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Parameter_Profile,
          Is_Not_Null_Return,
          Result_Profile,
          Aspect_Specifications),
      A_Package_Body_Stub =>
        +(Names,
          Aspect_Specifications),
      A_Task_Body_Stub =>
        +(Names,
          Aspect_Specifications),
      A_Protected_Body_Stub =>
        +(Names,
          Aspect_Specifications),
      An_Exception_Declaration =>
        +(Names,
          Aspect_Specifications),
      A_Choice_Parameter_Specification =>
        +Names,
      A_Generic_Procedure_Declaration =>
        +(Generic_Formal_Part,
          Names,
          Parameter_Profile,
          Aspect_Specifications),
      A_Generic_Function_Declaration =>
        +(Generic_Formal_Part,
          Names,
          Parameter_Profile,
          Is_Not_Null_Return,
          Result_Profile,
          Aspect_Specifications),
      A_Generic_Package_Declaration =>
        +(Generic_Formal_Part,
          Names,
          Aspect_Specifications,
          Visible_Part_Declarative_Items,
          Private_Part_Declarative_Items),
      A_Package_Instantiation =>
        +(Names,
          Generic_Unit_Name,
          Generic_Actual_Part,
          Aspect_Specifications),
      A_Procedure_Instantiation | A_Function_Instantiation =>
        +(Is_Overriding_Declaration,
          Is_Not_Overriding_Declaration,
          Names,
          Generic_Unit_Name,
          Generic_Actual_Part,
          Aspect_Specifications),
      A_Formal_Object_Declaration =>
        +(Names,
          Has_Null_Exclusion,
          Object_Declaration_View,
          Initialization_Expression,
          Aspect_Specifications),
      A_Formal_Type_Declaration =>
        +(Names,
          Discriminant_Part,
          Type_Declaration_View,
          Aspect_Specifications),
      A_Formal_Incomplete_Type_Declaration =>
        +(Names,
          Discriminant_Part,
          Has_Tagged,
          Aspect_Specifications),
      A_Formal_Procedure_Declaration =>
        +(Names,
          Parameter_Profile,
          Formal_Subprogram_Default,
          Has_Abstract,
          Aspect_Specifications),
      A_Formal_Function_Declaration =>
        +(Names,
          Parameter_Profile,
          Is_Not_Null_Return,
          Result_Profile,
          Formal_Subprogram_Default,
          Has_Abstract,
          Aspect_Specifications),
      A_Formal_Package_Declaration =>
        +(Names,
          Generic_Unit_Name,
          Generic_Actual_Part,
          Aspect_Specifications),
      A_Formal_Package_Declaration_With_Box =>
        +(Names,
          Generic_Unit_Name,
          Aspect_Specifications),
      A_Derived_Type_Definition =>
        +(Has_Abstract,
          Has_Limited,
          Parent_Subtype_Indication),
      A_Derived_Record_Extension_Definition =>
        +(Has_Abstract,
          Has_Limited,
          Parent_Subtype_Indication,
          Definition_Interface_List,
          Record_Definition),
      An_Enumeration_Type_Definition =>
        +Enumeration_Literal_Declarations,
      A_Signed_Integer_Type_Definition =>
        +Integer_Constraint,
      A_Modular_Type_Definition =>
        +Mod_Static_Expression,
      Flat_Root_Type_Kinds =>
        No_Queries'Access,
      A_Floating_Point_Definition =>
        +(Digits_Expression,
          Real_Range_Constraint),
      An_Ordinary_Fixed_Point_Definition =>
        +(Delta_Expression,
          Real_Range_Constraint),
      A_Decimal_Fixed_Point_Definition =>
        +(Delta_Expression,
          Digits_Expression,
          Real_Range_Constraint),
      An_Unconstrained_Array_Definition =>
        +(Index_Subtype_Definitions,
          Array_Component_Definition),
      A_Constrained_Array_Definition =>
        +(Discrete_Subtype_Definitions,
          Array_Component_Definition),
      A_Record_Type_Definition =>
        +(Has_Abstract,
          Has_Limited,
          Record_Definition),
      A_Tagged_Record_Type_Definition =>
        +(Has_Abstract,
          Has_Limited,
          Record_Definition),
      Flat_Interface_Kinds =>
        +Definition_Interface_List,
      Flat_Access_To_Object_Definition =>
        +(Has_Null_Exclusion,
          Access_To_Object_Definition),
      An_Access_To_Procedure =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile),
      An_Access_To_Protected_Procedure =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile),
      An_Access_To_Function =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile,
          Is_Not_Null_Return,
          Access_To_Function_Result_Profile),
      An_Access_To_Protected_Function =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile,
          Is_Not_Null_Return,
          Access_To_Function_Result_Profile),
      A_Subtype_Indication =>
        +(Has_Aliased, -- Why needed???
          Has_Null_Exclusion,
          Subtype_Mark,
          Subtype_Constraint),
      A_Range_Attribute_Reference =>
        +Range_Attribute,
      A_Simple_Expression_Range =>
        +(Lower_Bound,
          Upper_Bound),
      A_Digits_Constraint =>
        +(Digits_Expression,
          Real_Range_Constraint),
      A_Delta_Constraint =>
        +(Delta_Expression,
          Real_Range_Constraint),
      An_Index_Constraint =>
        +Discrete_Ranges,
      A_Discriminant_Constraint =>
        +Discriminant_Associations,
      A_Component_Definition =>
        +(Has_Aliased,
          Component_Definition_View),
      A_Discrete_Subtype_Indication_As_Subtype_Definition =>
        +(Subtype_Mark,
          Subtype_Constraint),
      A_Discrete_Range_Attribute_Reference_As_Subtype_Definition =>
        +Range_Attribute,
      A_Discrete_Simple_Expression_Range_As_Subtype_Definition =>
        +(Lower_Bound,
          Upper_Bound),
      A_Discrete_Subtype_Indication =>
        +(Subtype_Mark,
          Subtype_Constraint),
      A_Discrete_Range_Attribute_Reference =>
        +Range_Attribute,
      A_Discrete_Simple_Expression_Range =>
        +(Lower_Bound,
          Upper_Bound),
      An_Unknown_Discriminant_Part =>
        No_Queries'Access,
      A_Known_Discriminant_Part =>
        +Discriminants,
      A_Record_Definition =>
        +(Has_Limited,
          Record_Components),
      A_Null_Record_Definition =>
        No_Queries'Access,
      A_Null_Component =>
        No_Queries'Access,
      A_Variant_Part =>
        +(Discriminant_Direct_Name,
          Variants),
      A_Variant =>
        +(Variant_Choices,
          Record_Components),
      An_Others_Choice =>
        No_Queries'Access,
      An_Anonymous_Access_To_Variable =>
        +(Has_Null_Exclusion,
          Anonymous_Access_To_Object_Subtype_Mark),
      An_Anonymous_Access_To_Constant =>
        +(Has_Null_Exclusion,
          Anonymous_Access_To_Object_Subtype_Mark),
      An_Anonymous_Access_To_Procedure =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile),
      An_Anonymous_Access_To_Protected_Procedure =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile),
      An_Anonymous_Access_To_Function =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile,
          Is_Not_Null_Return,
          Access_To_Function_Result_Profile),
      An_Anonymous_Access_To_Protected_Function =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile,
          Is_Not_Null_Return,
          Access_To_Function_Result_Profile),
      A_Private_Type_Definition =>
        +(Has_Abstract,
          Has_Limited),
      A_Tagged_Private_Type_Definition =>
        +(Has_Abstract,
          Has_Limited),
      A_Private_Extension_Definition =>
        +(Has_Abstract,
          Has_Limited,
          Has_Synchronized,
          Ancestor_Subtype_Indication,
          Definition_Interface_List),
      A_Task_Definition =>
        +(Visible_Part_Items,
          Private_Part_Items),
      A_Protected_Definition =>
        +(Visible_Part_Items,
          Private_Part_Items),
      A_Formal_Private_Type_Definition =>
        +(Has_Abstract,
          Has_Limited),
      A_Formal_Tagged_Private_Type_Definition =>
        +(Has_Abstract,
          Has_Limited),
      A_Formal_Derived_Type_Definition =>
        +(Has_Abstract,
          Has_Limited,
          Has_Synchronized,
          Subtype_Mark,
          Definition_Interface_List,
          Has_Private),
      A_Formal_Discrete_Type_Definition =>
        No_Queries'Access,
      A_Formal_Signed_Integer_Type_Definition =>
        No_Queries'Access,
      A_Formal_Modular_Type_Definition =>
        No_Queries'Access,
      A_Formal_Floating_Point_Definition =>
        No_Queries'Access,
      A_Formal_Ordinary_Fixed_Point_Definition =>
        No_Queries'Access,
      A_Formal_Decimal_Fixed_Point_Definition =>
        No_Queries'Access,
      Flat_Formal_Interface_Kinds =>
        +Definition_Interface_List,
      A_Formal_Unconstrained_Array_Definition =>
        +(Index_Subtype_Definitions,
          Array_Component_Definition),
      A_Formal_Constrained_Array_Definition =>
        +(Discrete_Subtype_Definitions,
          Array_Component_Definition),
      A_Formal_Pool_Specific_Access_To_Variable =>
        +(Has_Null_Exclusion,
          Access_To_Object_Definition),
      A_Formal_Access_To_Variable =>
        +(Has_Null_Exclusion,
          Access_To_Object_Definition),
      A_Formal_Access_To_Constant =>
        +(Has_Null_Exclusion,
          Access_To_Object_Definition),
      A_Formal_Access_To_Procedure =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile),
      A_Formal_Access_To_Protected_Procedure =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile),
      A_Formal_Access_To_Function =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile,
          Is_Not_Null_Return,
          Access_To_Function_Result_Profile),
      A_Formal_Access_To_Protected_Function =>
        +(Has_Null_Exclusion,
          Access_To_Subprogram_Parameter_Profile,
          Is_Not_Null_Return,
          Access_To_Function_Result_Profile),
      An_Aspect_Specification =>
        +(Aspect_Mark,
          Aspect_Definition),
      A_Box_Expression =>
        No_Queries'Access,
      An_Integer_Literal =>
        No_Queries'Access,
      A_Real_Literal =>
        No_Queries'Access,
      A_String_Literal =>
        No_Queries'Access,
      Flat_Usage_Name_Kinds =>
        No_Queries'Access,
      An_Explicit_Dereference =>
        +Prefix,
      A_Function_Call =>
        +(Prefix,
          Function_Call_Parameters,
          Is_Prefix_Call,
          Is_Prefix_Notation),
      An_Indexed_Component =>
        +(Prefix,
          Index_Expressions),
      A_Slice =>
        +(Prefix,
          Slice_Range),
      A_Selected_Component =>
        +(Prefix,
          Selector),
      Flat_Attr_Ref_Without_Exp_Kinds =>
        +(Prefix,
          Attribute_Designator_Identifier),
      Flat_Attr_Ref_With_Exp_Kinds =>
        +(Prefix,
          Attribute_Designator_Identifier,
          Attribute_Designator_Expressions),
      A_Record_Aggregate =>
        +Record_Component_Associations,
      An_Extension_Aggregate =>
        +(Extension_Aggregate_Expression,
          Record_Component_Associations),
      A_Positional_Array_Aggregate =>
        +Array_Component_Associations,
      A_Named_Array_Aggregate =>
        +Array_Component_Associations,
      An_And_Then_Short_Circuit =>
        +(Short_Circuit_Operation_Left_Expression,
          Short_Circuit_Operation_Right_Expression),
      An_Or_Else_Short_Circuit =>
        +(Short_Circuit_Operation_Left_Expression,
          Short_Circuit_Operation_Right_Expression),
      An_In_Membership_Test =>
        +(Membership_Test_Expression,
          Membership_Test_Choices),
      A_Not_In_Membership_Test =>
        +(Membership_Test_Expression,
          Membership_Test_Choices),
      A_Null_Literal =>
        No_Queries'Access,
      A_Parenthesized_Expression =>
        +Expression_Parenthesized,
      A_Raise_Expression =>
        +(Raised_Exception,
          Associated_Message),
      A_Type_Conversion =>
        +(Converted_Or_Qualified_Subtype_Mark,
          Converted_Or_Qualified_Expression),
      A_Qualified_Expression =>
        +(Converted_Or_Qualified_Subtype_Mark,
          Converted_Or_Qualified_Expression),
      An_Allocation_From_Subtype =>
        +(Subpool_Name,
          Allocator_Subtype_Indication),
      An_Allocation_From_Qualified_Expression =>
        +(Subpool_Name,
          Allocator_Qualified_Expression),
      A_Case_Expression =>
        +(Case_Expression,
          Expression_Paths),
      An_If_Expression =>
        +Expression_Paths,
      A_For_All_Quantified_Expression =>
        +(Iterator_Specification,
          Predicate),
      A_For_Some_Quantified_Expression =>
        +(Iterator_Specification,
          Predicate),
      A_Pragma_Argument_Association =>
        +(Formal_Parameter,
          Actual_Parameter),
      A_Discriminant_Association =>
        +(Discriminant_Selector_Names,
          Discriminant_Expression),
      A_Record_Component_Association =>
        +(Record_Component_Choices,
          Component_Expression),
      An_Array_Component_Association =>
        +(Array_Component_Choices,
          Component_Expression),
      A_Parameter_Association =>
        +(Formal_Parameter,
          Actual_Parameter),
      A_Generic_Association =>
        +(Formal_Parameter,
          Actual_Parameter),
      A_Null_Statement =>
        +Label_Names,
      An_Assignment_Statement =>
        +(Label_Names,
          Assignment_Variable_Name,
          Assignment_Expression),
      An_If_Statement =>
        +(Label_Names,
          Statement_Paths),
      A_Case_Statement =>
        +(Label_Names,
          Case_Expression,
          Statement_Paths),
      A_Loop_Statement =>
        +(Label_Names,
          Statement_Identifier,
          Loop_Statements),
      A_While_Loop_Statement =>
        +(Label_Names,
          Statement_Identifier,
          While_Condition,
          Loop_Statements),
      A_For_Loop_Statement =>
        +(Label_Names,
          Statement_Identifier,
          For_Loop_Parameter_Specification,
          Loop_Statements),
      A_Block_Statement =>
        +(Label_Names,
          Statement_Identifier,
          Block_Declarative_Items,
          Block_Statements,
          Block_Exception_Handlers),
      An_Exit_Statement =>
        +(Label_Names,
          Exit_Loop_Name,
          Exit_Condition),
      A_Goto_Statement =>
        +(Label_Names,
          Goto_Label),
      A_Procedure_Call_Statement =>
        +(Label_Names,
          Called_Name,
          Call_Statement_Parameters,
          Is_Prefix_Notation),
      A_Return_Statement =>
        +(Label_Names,
          Return_Expression),
      An_Extended_Return_Statement =>
        +(Label_Names,
          Return_Object_Declaration,
          Extended_Return_Statements,
          Extended_Return_Exception_Handlers),
      An_Accept_Statement =>
        +(Label_Names,
          Accept_Entry_Direct_Name,
          Accept_Entry_Index,
          Accept_Parameters,
          Accept_Body_Statements,
          Accept_Body_Exception_Handlers),
      An_Entry_Call_Statement =>
        +(Label_Names,
          Called_Name,
          Call_Statement_Parameters),
      A_Requeue_Statement =>
        +(Label_Names,
          Requeue_Entry_Name),
      A_Requeue_Statement_With_Abort =>
        +(Label_Names,
          Requeue_Entry_Name),
      A_Delay_Until_Statement =>
        +(Label_Names,
          Delay_Expression),
      A_Delay_Relative_Statement =>
        +(Label_Names,
          Delay_Expression),
      A_Terminate_Alternative_Statement =>
        No_Queries'Access,
      A_Selective_Accept_Statement =>
        +(Label_Names,
          Statement_Paths),
      A_Timed_Entry_Call_Statement =>
        +(Label_Names,
          Statement_Paths),
      A_Conditional_Entry_Call_Statement =>
        +(Label_Names,
          Statement_Paths),
      An_Asynchronous_Select_Statement =>
        +(Label_Names,
          Statement_Paths),
      An_Abort_Statement =>
        +(Label_Names,
          Aborted_Tasks),
      A_Raise_Statement =>
        +(Label_Names,
          Raised_Exception,
          Associated_Message),
      A_Code_Statement =>
        +(Label_Names,
          Qualified_Expression),
      An_If_Path =>
        +(Condition_Expression,
          Sequence_Of_Statements),
      An_Elsif_Path =>
        +(Condition_Expression,
          Sequence_Of_Statements),
      An_Else_Path =>
        +Sequence_Of_Statements,
      A_Case_Path =>
        +(Case_Path_Alternative_Choices,
          Sequence_Of_Statements),
      A_Select_Path =>
        +(Guard,
          Sequence_Of_Statements),
      An_Or_Path =>
        +(Guard,
          Sequence_Of_Statements),
      A_Then_Abort_Path =>
        +Sequence_Of_Statements,
      A_Case_Expression_Path =>
        +(Case_Path_Alternative_Choices,
          Dependent_Expression),
      An_If_Expression_Path =>
        +(Condition_Expression,
          Dependent_Expression),
      An_Elsif_Expression_Path =>
        +(Condition_Expression,
          Dependent_Expression),
      An_Else_Expression_Path =>
        +Dependent_Expression,
      A_Use_Package_Clause |
      A_Use_Type_Clause |
      A_Use_All_Type_Clause =>
        +Clause_Names,
      A_With_Clause =>
        +(Has_Limited,
          Has_Private,
          Clause_Names),
      An_Attribute_Definition_Clause =>
        +(Representation_Clause_Name,
          Representation_Clause_Expression),
      An_Enumeration_Representation_Clause =>
        +(Representation_Clause_Name,
          Representation_Clause_Expression),
      A_Record_Representation_Clause =>
        +(Representation_Clause_Name,
          Mod_Clause_Expression,
          Component_Clauses),
      An_At_Clause =>
        +(Representation_Clause_Name,
          Representation_Clause_Expression),
      A_Component_Clause =>
        +(Representation_Clause_Name,
          Component_Clause_Position,
          Component_Clause_Range),
      An_Exception_Handler =>
        +(Choice_Parameter_Specification,
          Exception_Choices,
          Handler_Statements),

      A_Compilation_Unit =>
        +(Context_Clause_Elements,
          Unit_Declaration,
          Pragmas_After),

      A_Comment =>
        No_Queries'Access,

      An_Aliased |
      A_Null_Exclusion |
      A_Not_Null_Return |
      A_Reverse |
      A_Limited |
      A_Synchronized |
      A_Private |
      An_Abstract |
      A_Tagged |
      An_Overriding |
      A_Not_Overriding |
      An_Is_Prefix_Call |
      An_Is_Prefix_Notation =>
        No_Queries'Access,

      Flat_List_Kinds |
      Flat_Abstract_Classes |
      Non_Trivial_Mapping |
      Not_Implemented_Mapping |
      Trivial_Mapping |
      No_Mapping =>
        null -- should never look at these

     ); -- end Query_Table

   -----------------------
   -- Local subprograms --
   -----------------------

   function Maybe_Reorder
     (Element : Asis.Element;
      Queries : Query_List_Ptr;
      Syntactic : Boolean) return Query_List_Ptr;
   --  If a subprogram call needs reordering, and Syntactic is true, this
   --  returns the reordered query list. Otherwise, returns Queries unchanged.

   function Subprogram_Call_Needs_Reordering
     (El   : Asis.Element)
      return Boolean;
   --  Checks if for a subprogram call element the sequence of its components
   --  obtained as
   --
   --     'Name_Of_Called_Subprogram' -> 'Parameter_Associations'
   --
   --  should be reordered because of one of the following reasons:
   --
   --  1. For 'A + B' the right order of traversing is
   --
   --        'A' -> '+' -> 'B'
   --
   --  2. For Obj.Operation (Par1, Par2) where Obj is the dispatching
   --     operand for subprogram Operation the right order of traversing is
   --
   --        'Obj' -> 'Fun' -> 'Par1' ->'Par2'

   function First_Parameter_Association
     (Call : Asis.Element)
      return Asis.Element;
   --  Returns the first parameter association from the Call that is supposed
   --  to be either A_Function_Call or A_Procedure_Call_Statement Element, and
   --  the corresponding call has at least one parameter association.

   function All_But_First_Associations
     (Call : Asis.Element)
      return Asis.Element_List;
   --  Returns the parameter association list from the Call that contains all
   --  but first associations. The Call that is supposed to be either
   --  A_Function_Call or A_Procedure_Call_Statement Element, and the
   --  corresponding call has at least one parameter association. If Call
   --  contains exactly one parameter association, the result is
   --  Nil_Element_List

   --  Subprograms declared below implement first-depth-level parsing of
   --  Elements of specific kinds - they return a list of queries needed to
   --  get all the first-depth-level components of their argument in
   --  from-left-to-right order

   --  Following are the queries to use when reordered:

   Reordered_Func_Call_Func_Elems : aliased constant Query_List :=
     (First_Parameter_Association,
      Prefix,
      All_But_First_Associations);

   Reordered_Proc_Call_Func_Elems : aliased constant Query_List :=
     (Label_Names,
      First_Parameter_Association,
      Called_Name,
      All_But_First_Associations);

   Reordered_Func_Call_Queries : constant Query_List_Ptr :=
     Reordered_Func_Call_Func_Elems'Access;
   Reordered_Proc_Call_Queries : constant Query_List_Ptr :=
     Reordered_Proc_Call_Func_Elems'Access;

   --------------------------
   -- Function_Table --
   --------------------------

   --  Function_Table is a mapping from query to Func_Elem -- that is,
   --  information about which Ada subprogram does the query, and what its
   --  parameter and result type profile is.

   type Function_Table_Type is array (Structural_Queries) of Func_Elem;

   function FE
     (Q : Structural_Queries;
      Func : A_Single_Element_CU_Query) return Func_Elem;
   function FE
     (Q : Structural_Queries;
      Func : A_Element_List_CU_Query) return Func_Elem;
   function FE
     (Q : Structural_Queries;
      Func : A_Single_Element_Query) return Func_Elem;
   function FE
     (Q : Structural_Queries;
      Func : A_Element_List_Query) return Func_Elem;
   function FE
     (Q : Structural_Queries;
      Func : A_Element_List_Query_With_Boolean;
      Bool : Boolean) return Func_Elem;
   function FE
     (Q : Structural_Queries;
      Func : A_Boolean_Query) return Func_Elem;
   --  Constructor for Func_Elems. One version for each parameter and result
   --  type profile. Note that the Func will resolve the overloading.

   function FE
     (Q : Structural_Queries;
      Func : A_Single_Element_CU_Query) return Func_Elem
   is
   begin
      return
        (Q => Q,
         Query_Kind => Single_Element_CU_Query,
         Func_Simple_CU => Func);
   end FE;

   function FE
     (Q : Structural_Queries;
      Func : A_Element_List_CU_Query) return Func_Elem is
   begin
      return
        (Q => Q,
         Query_Kind => Element_List_CU_Query,
         Func_List_CU => Func);
   end FE;

   function FE
     (Q : Structural_Queries;
      Func : A_Single_Element_Query) return Func_Elem
   is
   begin
      return
        (Q => Q,
         Query_Kind => Single_Element_Query,
         Func_Simple => Func);
   end FE;

   function FE
     (Q : Structural_Queries;
      Func : A_Element_List_Query) return Func_Elem is
   begin
      return
        (Q => Q,
         Query_Kind => Element_List_Query,
         Func_List => Func);
   end FE;

   function FE
     (Q : Structural_Queries;
      Func : A_Element_List_Query_With_Boolean;
      Bool : Boolean) return Func_Elem is
   begin
      return
        (Q => Q,
         Query_Kind => Element_List_Query_With_Boolean,
         Func_List_Boolean => Func,
         Bool => Bool);
   end FE;

   function FE
     (Q : Structural_Queries;
      Func : A_Boolean_Query) return Func_Elem is
   begin
      return
        (Q => Q,
         Query_Kind => Boolean_Query,
         Func_Boolean => Func);
   end FE;

   --  All of the entries in the following table are intialized with an
   --  expression of the form "X => FE (X, X'Access[, True|False])", except for
   --  the first few, and except for Formal_Subprogram_Default (see below).

   Function_Table : constant Function_Table_Type :=
     (No_Query =>
        (No_Query, Bug),

      --  The following two are queries of Compilation_Unit. The rest below are
      --  queries of Elements.
      Context_Clause_Elements =>
        FE (Context_Clause_Elements,
            Context_Clause_Elements'Access),
      Unit_Declaration =>
        FE (Unit_Declaration,
            Unit_Declaration'Access),
      Pragmas_After =>
        FE (Pragmas_After,
            Pragmas_After'Access),

      Has_Aliased =>
        FE (Has_Aliased,
            Has_Aliased'Access),
      Has_Null_Exclusion =>
        FE (Has_Null_Exclusion,
            Has_Null_Exclusion'Access),
      Is_Not_Null_Return =>
        FE (Is_Not_Null_Return,
            Is_Not_Null_Return'Access),
      Has_Reverse =>
        FE (Has_Reverse,
            Has_Reverse'Access),
      Has_Limited =>
        FE (Has_Limited,
            Has_Limited'Access),
      Has_Synchronized =>
        FE (Has_Synchronized,
            Has_Synchronized'Access),
      Has_Private =>
        FE (Has_Private,
            Has_Private'Access),
      Has_Abstract =>
        FE (Has_Abstract,
            Has_Abstract'Access),
      Has_Tagged =>
        FE (Has_Tagged,
            Has_Tagged'Access),
      Is_Overriding_Declaration =>
        FE (Is_Overriding_Declaration,
            Is_Overriding_Declaration'Access),
      Is_Not_Overriding_Declaration =>
        FE (Is_Not_Overriding_Declaration,
            Is_Not_Overriding_Declaration'Access),
      Is_Prefix_Call =>
        FE (Is_Prefix_Call,
            Is_Prefix_Call'Access),
      Is_Prefix_Notation =>
        FE (Is_Prefix_Notation,
            Is_Prefix_Notation'Access),

      All_But_First_Associations =>
        FE (All_But_First_Associations,
            All_But_First_Associations'Access),
      First_Parameter_Association =>
        FE (First_Parameter_Association,
            First_Parameter_Association'Access),
      Clause_Names =>
        FE (Clause_Names,
            Clause_Names'Access),
      Component_Clause_Position =>
        FE (Component_Clause_Position,
            Component_Clause_Position'Access),
      Component_Clause_Range =>
        FE (Component_Clause_Range,
            Component_Clause_Range'Access),
      Component_Clauses =>
        FE (Component_Clauses,
            Component_Clauses'Access,
            True),
      Mod_Clause_Expression =>
        FE (Mod_Clause_Expression,
            Mod_Clause_Expression'Access),
      Representation_Clause_Expression =>
        FE (Representation_Clause_Expression,
            Representation_Clause_Expression'Access),
      Representation_Clause_Name =>
        FE (Representation_Clause_Name,
            Representation_Clause_Name'Access),
      Aspect_Specifications =>
        FE (Aspect_Specifications,
            Aspect_Specifications'Access),
      Body_Declarative_Items =>
        FE (Body_Declarative_Items,
            Body_Declarative_Items'Access,
            True),
      Body_Exception_Handlers =>
        FE (Body_Exception_Handlers,
            Body_Exception_Handlers'Access,
            True),
      Body_Statements =>
        FE (Body_Statements,
            Body_Statements'Access,
            True),
      Declaration_Interface_List =>
        FE (Declaration_Interface_List,
            Declaration_Interface_List'Access),
      Defining_Prefix =>
        FE (Defining_Prefix,
            Defining_Prefix'Access),
      Defining_Selector =>
        FE (Defining_Selector,
            Defining_Selector'Access),
      Discriminant_Part =>
        FE (Discriminant_Part,
            Discriminant_Part'Access),
      Entry_Barrier =>
        FE (Entry_Barrier,
            Entry_Barrier'Access),
      Entry_Family_Definition =>
        FE (Entry_Family_Definition,
            Entry_Family_Definition'Access),
      Entry_Index_Specification =>
        FE (Entry_Index_Specification,
            Entry_Index_Specification'Access),
      Generic_Actual_Part =>
        FE (Generic_Actual_Part,
            Generic_Actual_Part'Access,
            False),
      Generic_Formal_Part =>
        FE (Generic_Formal_Part,
            Generic_Formal_Part'Access,
            True),
      Generic_Unit_Name =>
        FE (Generic_Unit_Name,
            Generic_Unit_Name'Access),
      Initialization_Expression =>
        FE (Initialization_Expression,
            Initialization_Expression'Access),
      Iteration_Scheme_Name =>
        FE (Iteration_Scheme_Name,
            Iteration_Scheme_Name'Access),
      Names =>
        FE (Names,
            Names'Access),
      Object_Declaration_View =>
        FE (Object_Declaration_View,
            Object_Declaration_View'Access),
      Parameter_Profile =>
        FE (Parameter_Profile,
            Parameter_Profile'Access),
      Private_Part_Declarative_Items =>
        FE (Private_Part_Declarative_Items,
            Private_Part_Declarative_Items'Access,
            True),
      Protected_Operation_Items =>
        FE (Protected_Operation_Items,
            Protected_Operation_Items'Access,
            True),
      Renamed_Entity =>
        FE (Renamed_Entity,
            Renamed_Entity'Access),
      Result_Expression =>
        FE (Result_Expression,
            Result_Expression'Access),
      Result_Profile =>
        FE (Result_Profile,
            Result_Profile'Access),
      Specification_Subtype_Definition =>
        FE (Specification_Subtype_Definition,
            Specification_Subtype_Definition'Access),
      Subtype_Indication =>
        FE (Subtype_Indication,
            Subtype_Indication'Access),
      Type_Declaration_View =>
        FE (Type_Declaration_View,
            Type_Declaration_View'Access),
      Visible_Part_Declarative_Items =>
        FE (Visible_Part_Declarative_Items,
            Visible_Part_Declarative_Items'Access,
            True),
      Access_To_Function_Result_Profile =>
        FE (Access_To_Function_Result_Profile,
            Access_To_Function_Result_Profile'Access),
      Access_To_Object_Definition =>
        FE (Access_To_Object_Definition,
            Access_To_Object_Definition'Access),
      Access_To_Subprogram_Parameter_Profile =>
        FE (Access_To_Subprogram_Parameter_Profile,
            Access_To_Subprogram_Parameter_Profile'Access),
      Ancestor_Subtype_Indication =>
        FE (Ancestor_Subtype_Indication,
            Ancestor_Subtype_Indication'Access),
      Anonymous_Access_To_Object_Subtype_Mark =>
        FE (Anonymous_Access_To_Object_Subtype_Mark,
            Anonymous_Access_To_Object_Subtype_Mark'Access),
      Array_Component_Definition =>
        FE (Array_Component_Definition,
            Array_Component_Definition'Access),
      Aspect_Definition =>
        FE (Aspect_Definition,
            Aspect_Definition'Access),
      Aspect_Mark =>
        FE (Aspect_Mark,
            Aspect_Mark'Access),
      Component_Definition_View =>
        FE (Component_Definition_View,
            Component_Definition_View'Access),
      Definition_Interface_List =>
        FE (Definition_Interface_List,
            Definition_Interface_List'Access),
      Delta_Expression =>
        FE (Delta_Expression,
            Delta_Expression'Access),
      Digits_Expression =>
        FE (Digits_Expression,
            Digits_Expression'Access),
      Discrete_Ranges =>
        FE (Discrete_Ranges,
            Discrete_Ranges'Access),
      Discrete_Subtype_Definitions =>
        FE (Discrete_Subtype_Definitions,
            Discrete_Subtype_Definitions'Access),
      Discriminant_Associations =>
        FE (Discriminant_Associations,
            Discriminant_Associations'Access,
            False),
      Discriminant_Direct_Name =>
        FE (Discriminant_Direct_Name,
            Discriminant_Direct_Name'Access),
      Discriminants =>
        FE (Discriminants,
            Discriminants'Access),
      Enumeration_Literal_Declarations =>
        FE (Enumeration_Literal_Declarations,
            Enumeration_Literal_Declarations'Access),
      Index_Subtype_Definitions =>
        FE (Index_Subtype_Definitions,
            Index_Subtype_Definitions'Access),
      Integer_Constraint =>
        FE (Integer_Constraint,
            Integer_Constraint'Access),
      Lower_Bound =>
        FE (Lower_Bound,
            Lower_Bound'Access),
      Mod_Static_Expression =>
        FE (Mod_Static_Expression,
            Mod_Static_Expression'Access),
      Parent_Subtype_Indication =>
        FE (Parent_Subtype_Indication,
            Parent_Subtype_Indication'Access),
      Private_Part_Items =>
        FE (Private_Part_Items,
            Private_Part_Items'Access,
            True),
      Range_Attribute =>
        FE (Range_Attribute,
            Range_Attribute'Access),
      Real_Range_Constraint =>
        FE (Real_Range_Constraint,
            Real_Range_Constraint'Access),
      Record_Components =>
        FE (Record_Components,
            Record_Components'Access,
            True),
      Record_Definition =>
        FE (Record_Definition,
            Record_Definition'Access),
      Subtype_Constraint =>
        FE (Subtype_Constraint,
            Subtype_Constraint'Access),
      Subtype_Mark =>
        FE (Subtype_Mark,
            Subtype_Mark'Access),
      Upper_Bound =>
        FE (Upper_Bound,
            Upper_Bound'Access),
      Variant_Choices =>
        FE (Variant_Choices,
            Variant_Choices'Access),
      Variants =>
        FE (Variants,
            Variants'Access,
            True),
      Visible_Part_Items =>
        FE (Visible_Part_Items,
            Visible_Part_Items'Access,
            True),
      Pragma_Argument_Associations =>
        FE (Pragma_Argument_Associations,
            Pragma_Argument_Associations'Access),
      Actual_Parameter =>
        FE (Actual_Parameter,
            Actual_Parameter'Access),
      Allocator_Qualified_Expression =>
        FE (Allocator_Qualified_Expression,
            Allocator_Qualified_Expression'Access),
      Allocator_Subtype_Indication =>
        FE (Allocator_Subtype_Indication,
            Allocator_Subtype_Indication'Access),
      Array_Component_Associations =>
        FE (Array_Component_Associations,
            Array_Component_Associations'Access),
      Array_Component_Choices =>
        FE (Array_Component_Choices,
            Array_Component_Choices'Access),
      Attribute_Designator_Expressions =>
        FE (Attribute_Designator_Expressions,
            Attribute_Designator_Expressions'Access),
      Attribute_Designator_Identifier =>
        FE (Attribute_Designator_Identifier,
            Attribute_Designator_Identifier'Access),
      Component_Expression =>
        FE (Component_Expression,
            Component_Expression'Access),
      Converted_Or_Qualified_Expression =>
        FE (Converted_Or_Qualified_Expression,
            Converted_Or_Qualified_Expression'Access),
      Converted_Or_Qualified_Subtype_Mark =>
        FE (Converted_Or_Qualified_Subtype_Mark,
            Converted_Or_Qualified_Subtype_Mark'Access),
      Dependent_Expression =>
        FE (Dependent_Expression,
            Dependent_Expression'Access),
      Discriminant_Expression =>
        FE (Discriminant_Expression,
            Discriminant_Expression'Access),
      Discriminant_Selector_Names =>
        FE (Discriminant_Selector_Names,
            Discriminant_Selector_Names'Access),
      Expression_Parenthesized =>
        FE (Expression_Parenthesized,
            Expression_Parenthesized'Access),
      Expression_Paths =>
        FE (Expression_Paths,
            Expression_Paths'Access),
      Extension_Aggregate_Expression =>
        FE (Extension_Aggregate_Expression,
            Extension_Aggregate_Expression'Access),
      Formal_Parameter =>
        FE (Formal_Parameter,
            Formal_Parameter'Access),
      Function_Call_Parameters =>
        FE (Function_Call_Parameters,
            Function_Call_Parameters'Access,
            False),
      Index_Expressions =>
        FE (Index_Expressions,
            Index_Expressions'Access),
      Iterator_Specification =>
        FE (Iterator_Specification,
            Iterator_Specification'Access),
      Membership_Test_Choices =>
        FE (Membership_Test_Choices,
            Membership_Test_Choices'Access),
      Membership_Test_Expression =>
        FE (Membership_Test_Expression,
            Membership_Test_Expression'Access),
      Predicate =>
        FE (Predicate,
            Predicate'Access),
      Prefix =>
        FE (Prefix,
            Prefix'Access),
      Record_Component_Associations =>
        FE (Record_Component_Associations,
            Record_Component_Associations'Access,
            False),
      Record_Component_Choices =>
        FE (Record_Component_Choices,
            Record_Component_Choices'Access),
      Selector =>
        FE (Selector,
            Selector'Access),
      Short_Circuit_Operation_Left_Expression =>
        FE (Short_Circuit_Operation_Left_Expression,
            Short_Circuit_Operation_Left_Expression'Access),
      Short_Circuit_Operation_Right_Expression =>
        FE (Short_Circuit_Operation_Right_Expression,
            Short_Circuit_Operation_Right_Expression'Access),
      Slice_Range =>
        FE (Slice_Range,
            Slice_Range'Access),
      Subpool_Name =>
        FE (Subpool_Name,
            Subpool_Name'Access),
      Formal_Subprogram_Default =>
        FE (Formal_Subprogram_Default,
            Extensions.Formal_Subprogram_Default'Access),
      --  Note "Extensions." is needed to disambiguate from the one in
      --  Asis.Declarations.
      Aborted_Tasks =>
        FE (Aborted_Tasks,
            Aborted_Tasks'Access),
      Accept_Body_Exception_Handlers =>
        FE (Accept_Body_Exception_Handlers,
            Accept_Body_Exception_Handlers'Access,
            True),
      Accept_Body_Statements =>
        FE (Accept_Body_Statements,
            Accept_Body_Statements'Access,
            True),
      Accept_Entry_Direct_Name =>
        FE (Accept_Entry_Direct_Name,
            Accept_Entry_Direct_Name'Access),
      Accept_Entry_Index =>
        FE (Accept_Entry_Index,
            Accept_Entry_Index'Access),
      Accept_Parameters =>
        FE (Accept_Parameters,
            Accept_Parameters'Access),
      Assignment_Expression =>
        FE (Assignment_Expression,
            Assignment_Expression'Access),
      Assignment_Variable_Name =>
        FE (Assignment_Variable_Name,
            Assignment_Variable_Name'Access),
      Associated_Message =>
        FE (Associated_Message,
            Associated_Message'Access),
      Block_Declarative_Items =>
        FE (Block_Declarative_Items,
            Block_Declarative_Items'Access,
            True),
      Block_Exception_Handlers =>
        FE (Block_Exception_Handlers,
            Block_Exception_Handlers'Access,
            True),
      Block_Statements =>
        FE (Block_Statements,
            Block_Statements'Access,
            True),
      Called_Name =>
        FE (Called_Name,
            Called_Name'Access),
      Call_Statement_Parameters =>
        FE (Call_Statement_Parameters,
            Call_Statement_Parameters'Access,
            False),
      Case_Expression =>
        FE (Case_Expression,
            Case_Expression'Access),
      Case_Path_Alternative_Choices =>
        FE (Case_Path_Alternative_Choices,
            Case_Path_Alternative_Choices'Access),
      Choice_Parameter_Specification =>
        FE (Choice_Parameter_Specification,
            Choice_Parameter_Specification'Access),
      Condition_Expression =>
        FE (Condition_Expression,
            Condition_Expression'Access),
      Delay_Expression =>
        FE (Delay_Expression,
            Delay_Expression'Access),
      Exception_Choices =>
        FE (Exception_Choices,
            Exception_Choices'Access),
      Exit_Condition =>
        FE (Exit_Condition,
            Exit_Condition'Access),
      Exit_Loop_Name =>
        FE (Exit_Loop_Name,
            Exit_Loop_Name'Access),
      Extended_Return_Exception_Handlers =>
        FE (Extended_Return_Exception_Handlers,
            Extended_Return_Exception_Handlers'Access,
            True),
      Extended_Return_Statements =>
        FE (Extended_Return_Statements,
            Extended_Return_Statements'Access,
            True),
      For_Loop_Parameter_Specification =>
        FE (For_Loop_Parameter_Specification,
            For_Loop_Parameter_Specification'Access),
      Goto_Label =>
        FE (Goto_Label,
            Goto_Label'Access),
      Guard =>
        FE (Guard,
            Guard'Access),
      Handler_Statements =>
        FE (Handler_Statements,
            Handler_Statements'Access,
            True),
      Label_Names =>
        FE (Label_Names,
            Label_Names'Access),
      Loop_Statements =>
        FE (Loop_Statements,
            Loop_Statements'Access,
            True),
      Qualified_Expression =>
        FE (Qualified_Expression,
            Qualified_Expression'Access),
      Raised_Exception =>
        FE (Raised_Exception,
            Raised_Exception'Access),
      Requeue_Entry_Name =>
        FE (Requeue_Entry_Name,
            Requeue_Entry_Name'Access),
      Return_Expression =>
        FE (Return_Expression,
            Return_Expression'Access),
      Return_Object_Declaration =>
        FE (Return_Object_Declaration,
            Return_Object_Declaration'Access),
      Sequence_Of_Statements =>
        FE (Sequence_Of_Statements,
            Sequence_Of_Statements'Access,
            True),
      Statement_Identifier =>
        FE (Statement_Identifier,
            Statement_Identifier'Access),
      Statement_Paths =>
        FE (Statement_Paths,
            Statement_Paths'Access,
            True),
      While_Condition =>
        FE (While_Condition,
            While_Condition'Access)
     ); -- end Function_Table

   --------------------------------
   -- All_But_First_Associations --
   --------------------------------

   function All_But_First_Associations
     (Call : Asis.Element)
      return Asis.Element_List
   is
      Result : constant Asis.Element_List :=
        Asis.Extensions.Get_Call_Parameters (Call);
   begin
      return Result (Result'First + 1 .. Result'Last);
   end All_But_First_Associations;

   -------------------------
   -- Appropriate_Queries --
   -------------------------

   function Appropriate_Queries
     (Kind : Flat_Element_Kinds'Base) return Query_List_Ptr
   is
   begin
      return Query_Table (Kind);
   end Appropriate_Queries;

   function Appropriate_Queries
     (Element : Asis.Element; Syntactic : Boolean)
     return Query_List_Ptr
   is

      Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
      Result : constant Query_List_Ptr :=
        Maybe_Reorder (Element,
                       Appropriate_Queries (Kind),
                       Syntactic);

   begin
      return Result;
   end Appropriate_Queries;

   function Appropriate_Queries
     (Element : Asis.Element; Syntactic : Boolean := True)
     return Func_Elem_Array
   is
      Queries : constant Query_List_Ptr :=
        Appropriate_Queries (Element, Syntactic);

      Result : Func_Elem_Array (Queries'Range);

      --  Start of processing for Appropriate_Queries
   begin
      for J in Result'Range loop
         Result (J) := Function_Table (Queries (J));
      end loop;

      return Result;
   end Appropriate_Queries;

   ---------------------------------
   -- First_Parameter_Association --
   ---------------------------------

   function First_Parameter_Association
     (Call : Asis.Element)
      return Asis.Element
   is
      Result : constant Asis.Element_List :=
        Asis.Extensions.Get_Call_Parameters (Call);
   begin
      return Result (Result'First);
   end First_Parameter_Association;

   -------------------
   -- Get_Func_Elem --
   -------------------

   function Get_Func_Elem (Q : Structural_Queries) return Func_Elem is
   begin
      return Function_Table (Q);
   end Get_Func_Elem;

   -------------------
   -- Maybe_Reorder --
   -------------------

   function Maybe_Reorder
     (Element : Asis.Element;
      Queries : Query_List_Ptr;
      Syntactic : Boolean) return Query_List_Ptr
   is
      Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
   begin
      if Syntactic then
         case Kind is
            when A_Function_Call =>
               if Subprogram_Call_Needs_Reordering (Element) then
                  return Reordered_Func_Call_Queries;
               end if;
            when A_Procedure_Call_Statement |
                 An_Entry_Call_Statement     =>
               if Subprogram_Call_Needs_Reordering (Element) then
                  return Reordered_Proc_Call_Queries;
               end if;
            when others => null;
         end case;
      end if;

      return Queries;
   end Maybe_Reorder;

   -----------------
   -- Num_Queries --
   -----------------

   function Num_Queries
     (Kind : Flat_Element_Kinds'Base) return A4G.Queries.Query_Count
   is
   begin
      return Appropriate_Queries (Kind)'Length;
   end Num_Queries;

   --------------------------------------
   -- Subprogram_Call_Needs_Reordering --
   --------------------------------------

   function Subprogram_Call_Needs_Reordering
     (El   : Asis.Element)
      return Boolean
   is
      Result : Boolean := False;
   begin
      if Asis.Elements.Is_Prefix_Notation (El)
        or else
        (Asis.Elements.Expression_Kind (El) = A_Function_Call
         and then
          not Asis.Expressions.Is_Prefix_Call (El)
         and then
          Asis.Expressions.Function_Call_Parameters (El)'Length = 2)
      then
         Result := True;
      end if;

      return Result;
   end Subprogram_Call_Needs_Reordering;

end A4G.Queries;
