------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                 A S I S T A N T . H E L P . Q U E R I E S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2012, Free Software Foundation, Inc.         --
--                                                                          --
-- ASIStant  is  free  software;  you can  redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version.  ASIStant is  distributed  in the hope  that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;  without  even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License  distributed with GNAT;  see file COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
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
--                                                                          --
------------------------------------------------------------------------------

with ASIStant.FuncEnum; use ASIStant.FuncEnum;

package ASIStant.Help.Queries is

--  This package contains lists of queries that are applicable to each
--  extended Element kind. This information is used by the help system.

   type Query_List is array (1 .. 10) of Switch_Index;

   Appropriate_Queries : array (Flat_Element_Kinds'Range) of Query_List :=
     (
      An_All_Calls_Remote_Pragma .. An_Unknown_Pragma =>
        (
         Pragma_Argument_Associations,
         others => Invalid_Index
         ),

      A_Defining_Identifier .. A_Defining_Not_Operator =>
        (
         others => Invalid_Index
         ),

      A_Defining_Expanded_Name =>
        (
         Defining_Prefix,
         Defining_Selector,
         others => Invalid_Index
         ),

      An_Ordinary_Type_Declaration =>
        (
         Names,
         Discriminant_Part,
         Type_Declaration_View,
         others => Invalid_Index
         ),

      A_Task_Type_Declaration |
        A_Protected_Type_Declaration =>
        (
         Names,
         Discriminant_Part,
         Declaration_Interface_List,
         Type_Declaration_View,
         others => Invalid_Index
         ),

      An_Incomplete_Type_Declaration |
        A_Tagged_Incomplete_Type_Declaration =>
        (
         Names,
         Discriminant_Part,
         others => Invalid_Index
         ),

      A_Private_Type_Declaration =>
        (
         Names,
         Discriminant_Part,
         Type_Declaration_View,
         others => Invalid_Index
         ),

      A_Private_Extension_Declaration =>
        (
         Names,
         Discriminant_Part,
         Type_Declaration_View,
         others => Invalid_Index
         ),

      A_Subtype_Declaration =>
        (
         Names,
         Type_Declaration_View,
         others => Invalid_Index
         ),

      A_Variable_Declaration =>
        (
         Names,
         Object_Declaration_View,
         Initialization_Expression,
         others => Invalid_Index
         ),

      A_Constant_Declaration =>
        (
         Names,
         Object_Declaration_View,
         Initialization_Expression,
         others => Invalid_Index
         ),

      A_Deferred_Constant_Declaration =>
        (
         Names,
         Object_Declaration_View,
         others => Invalid_Index
         ),

      A_Single_Task_Declaration |
        A_Single_Protected_Declaration =>
        (
         Names,
         Declaration_Interface_List,
         Object_Declaration_View,
         others => Invalid_Index
         ),

      An_Integer_Number_Declaration .. A_Real_Number_Declaration =>
        (
         Names,
         Initialization_Expression,
         others => Invalid_Index
         ),

      An_Enumeration_Literal_Specification =>
        (
         Names,
         others => Invalid_Index
         ),

      A_Discriminant_Specification =>
        (
         Names,
         Object_Declaration_View,
         Initialization_Expression,
         others => Invalid_Index
         ),

      A_Component_Declaration =>
        (
         Names,
         Object_Declaration_View,
         Initialization_Expression,
         others => Invalid_Index
         ),

      A_Loop_Parameter_Specification =>
        (
         Names,
         Specification_Subtype_Definition,
         others => Invalid_Index
         ),

      A_Generalized_Iterator_Specification =>
        (
         Names,
         Iteration_Scheme_Name,
         others => Invalid_Index
         ),

      An_Element_Iterator_Specification =>
        (
         Names,
         Subtype_Indication,
         Iteration_Scheme_Name,
         others => Invalid_Index
         ),

      A_Procedure_Declaration =>
        (
         Names,
         Parameter_Profile,
         Aspect_Specifications,
         others => Invalid_Index
         ),

      A_Function_Declaration =>
        (
         Names,
         Parameter_Profile,
         Result_Profile,
         Aspect_Specifications,
         others => Invalid_Index
         ),

      A_Parameter_Specification =>
        (
         Names,
         Object_Declaration_View,
         Initialization_Expression,
         others => Invalid_Index
         ),

      A_Procedure_Body_Declaration =>
        (
         Names,
         Parameter_Profile,
         Body_Declarative_Items,
         Body_Statements,
         Body_Exception_Handlers,
         others => Invalid_Index
         ),

      A_Function_Body_Declaration =>
        (
         Names,
         Parameter_Profile,
         Result_Profile,
         Body_Declarative_Items,
         Body_Statements,
         Body_Exception_Handlers,
         others => Invalid_Index
         ),

      A_Return_Variable_Specification |
        A_Return_Constant_Specification =>
        (
         Names,
         Object_Declaration_View,
         Initialization_Expression,
         others => Invalid_Index
         ),

      A_Null_Procedure_Declaration =>
        (
         Names,
         Parameter_Profile,
         Aspect_Specifications,
         others => Invalid_Index
         ),

      An_Expression_Function_Declaration =>
        (
         Names,
         Parameter_Profile,
         Result_Profile,
         Result_Expression,
         Aspect_Specifications,
         others => Invalid_Index
         ),

      A_Package_Declaration =>
        (
         Names,
         Visible_Part_Declarative_Items,
         Private_Part_Declarative_Items,
         others => Invalid_Index
         ),

      A_Package_Body_Declaration =>
        (
         Names,
         Body_Declarative_Items,
         Body_Statements,
         Body_Exception_Handlers,
         others => Invalid_Index
         ),

      An_Object_Renaming_Declaration =>
        (
         Names,
         Object_Declaration_View,
         Renamed_Entity,
         others => Invalid_Index
         ),

      An_Exception_Renaming_Declaration .. A_Package_Renaming_Declaration =>
        (
         Names,
         Renamed_Entity,
         others => Invalid_Index
         ),

      A_Procedure_Renaming_Declaration =>
        (
         Names,
         Parameter_Profile,
         Renamed_Entity,
         others => Invalid_Index
         ),

      A_Function_Renaming_Declaration =>
        (
         Names,
         Parameter_Profile,
         Result_Profile,
         Renamed_Entity,
         others => Invalid_Index
         ),

      A_Generic_Package_Renaming_Declaration ..
      A_Generic_Function_Renaming_Declaration =>
        (
         Names,
         Renamed_Entity,
         others => Invalid_Index
         ),

      A_Task_Body_Declaration =>
        (
         Names,
         Body_Declarative_Items,
         Body_Statements,
         Body_Exception_Handlers,
         others => Invalid_Index
         ),

      A_Protected_Body_Declaration =>
        (
         Names,
         Protected_Operation_Items,
         others => Invalid_Index
         ),

      An_Entry_Declaration =>
        (
         Names,
         Entry_Family_Definition,
         Parameter_Profile,
         others => Invalid_Index
         ),

      An_Entry_Body_Declaration =>
        (
         Names,
         Entry_Index_Specification,
         Parameter_Profile,
         Entry_Barrier,
         Body_Declarative_Items,
         Body_Statements,
         Body_Exception_Handlers,
         others => Invalid_Index
         ),

      An_Entry_Index_Specification =>
        (
         Names,
         Specification_Subtype_Definition,
         others => Invalid_Index
         ),

      A_Procedure_Body_Stub =>
        (
         Names,
         Parameter_Profile,
         others => Invalid_Index
         ),

      A_Function_Body_Stub =>
        (
         Names,
         Parameter_Profile,
         Result_Profile,
         others => Invalid_Index
         ),

      A_Package_Body_Stub =>
        (
         Names,
         others => Invalid_Index
         ),

      A_Task_Body_Stub =>
        (
         Names,
         others => Invalid_Index
         ),

      A_Protected_Body_Stub =>
        (
         Names,
         others => Invalid_Index
         ),

      An_Exception_Declaration =>
        (
         Names,
         others => Invalid_Index
         ),

      A_Choice_Parameter_Specification =>
        (
         Names,
         others => Invalid_Index
         ),

      A_Generic_Procedure_Declaration =>
        (
         Generic_Formal_Part,
         Names,
         Parameter_Profile,
         others => Invalid_Index
         ),

      A_Generic_Function_Declaration =>
        (
         Generic_Formal_Part,
         Names,
         Parameter_Profile,
         Result_Profile,
         others => Invalid_Index
         ),

      A_Generic_Package_Declaration =>
        (
         Generic_Formal_Part,
         Names,
         Visible_Part_Declarative_Items,
         Private_Part_Declarative_Items,
         others => Invalid_Index
         ),

      A_Package_Instantiation =>
        (
         Names,
         Generic_Unit_Name,
         Generic_Actual_Part,
         others => Invalid_Index
         ),

      A_Procedure_Instantiation =>
        (
         Names,
         Generic_Unit_Name,
         Generic_Actual_Part,
         others => Invalid_Index
         ),

      A_Function_Instantiation =>
        (
         Names,
         Generic_Unit_Name,
         Generic_Actual_Part,
         others => Invalid_Index
         ),

      A_Formal_Object_Declaration =>
        (
         Names,
         Object_Declaration_View,
         Initialization_Expression,
         others => Invalid_Index
         ),

      A_Formal_Type_Declaration =>
        (
         Names,
         Discriminant_Part,
         Type_Declaration_View,
         others => Invalid_Index
         ),

      A_Formal_Incomplete_Type_Declaration =>
        (
         Names,
         Discriminant_Part,
         others => Invalid_Index
         ),

      A_Formal_Procedure_Declaration =>
        (
         Names,
         Parameter_Profile,
         Formal_Subprogram_Default,
         others => Invalid_Index
         ),

      A_Formal_Function_Declaration =>
        (
         Names,
         Parameter_Profile,
         Result_Profile,
         Formal_Subprogram_Default,
         others => Invalid_Index
         ),

      A_Formal_Package_Declaration =>
        (
         Names,
         Generic_Unit_Name,
         Generic_Actual_Part,
         others => Invalid_Index
         ),

      A_Formal_Package_Declaration_With_Box =>
        (
         Names,
         Generic_Unit_Name,
         others => Invalid_Index
         ),

      A_Derived_Type_Definition =>
        (
         Parent_Subtype_Indication,
         others => Invalid_Index
         ),

      A_Derived_Record_Extension_Definition =>
        (
         Parent_Subtype_Indication,
         Definition_Interface_List,
         Record_Definition,
         others => Invalid_Index
         ),

      An_Enumeration_Type_Definition =>
        (
         Enumeration_Literal_Declarations,
         others => Invalid_Index
         ),

      A_Signed_Integer_Type_Definition =>
        (
         Integer_Constraint,
         others => Invalid_Index
         ),

      A_Modular_Type_Definition =>
        (
         Mod_Static_Expression,
         others => Invalid_Index
         ),

      A_Root_Integer_Definition =>
        (
         others => Invalid_Index
         ),

      A_Root_Real_Definition =>
        (
         others => Invalid_Index
         ),

      A_Universal_Integer_Definition =>
        (
         others => Invalid_Index
         ),

      A_Universal_Real_Definition =>
        (
         others => Invalid_Index
         ),

      A_Universal_Fixed_Definition =>
        (
         others => Invalid_Index
         ),

      A_Floating_Point_Definition =>
        (
         Digits_Expression,
         Real_Range_Constraint,
         others => Invalid_Index
         ),

      An_Ordinary_Fixed_Point_Definition =>
        (
         Delta_Expression,
         Real_Range_Constraint,
         others => Invalid_Index
         ),

      A_Decimal_Fixed_Point_Definition =>
        (
         Delta_Expression,
         Digits_Expression,
         Real_Range_Constraint,
         others => Invalid_Index
         ),

      An_Unconstrained_Array_Definition =>
        (
         Index_Subtype_Definitions,
         Array_Component_Definition,
         others => Invalid_Index
         ),

      A_Constrained_Array_Definition =>
        (
         Discrete_Subtype_Definitions,
         Array_Component_Definition,
         others => Invalid_Index
         ),

      A_Record_Type_Definition =>
        (
         Record_Definition,
         others => Invalid_Index
         ),

      A_Tagged_Record_Type_Definition =>
        (
         Record_Definition,
         others => Invalid_Index
         ),

      An_Ordinary_Interface |
        A_Limited_Interface |
        A_Task_Interface |
        A_Protected_Interface |
        A_Synchronized_Interface =>
        (
         Definition_Interface_List,
         others => Invalid_Index
         ),

      A_Pool_Specific_Access_To_Variable =>
        (
         Access_To_Object_Definition,
         others => Invalid_Index
         ),

      An_Access_To_Variable =>
        (
         Access_To_Object_Definition,
         others => Invalid_Index
         ),

      An_Access_To_Constant =>
        (
         Access_To_Object_Definition,
         others => Invalid_Index
         ),

      An_Access_To_Procedure =>
        (
         Access_To_Subprogram_Parameter_Profile,
         others => Invalid_Index
         ),

      An_Access_To_Protected_Procedure =>
        (
         Access_To_Subprogram_Parameter_Profile,
         others => Invalid_Index
         ),

      An_Access_To_Function =>
        (
         Access_To_Subprogram_Parameter_Profile,
         Access_To_Function_Result_Profile,
         others => Invalid_Index
         ),

      An_Access_To_Protected_Function =>
        (
         Access_To_Subprogram_Parameter_Profile,
         Access_To_Function_Result_Profile,
         others => Invalid_Index
         ),

      A_Subtype_Indication =>
        (
         Subtype_Mark,
         Subtype_Constraint,
         others => Invalid_Index
         ),

      A_Range_Attribute_Reference =>
        (
         Range_Attribute,
         others => Invalid_Index
         ),

      A_Simple_Expression_Range =>
        (
         Lower_Bound,
         Upper_Bound,
         others => Invalid_Index
         ),

      A_Digits_Constraint =>
        (
         Digits_Expression,
         Real_Range_Constraint,
         others => Invalid_Index
         ),

      A_Delta_Constraint =>
        (
         Delta_Expression,
         Real_Range_Constraint,
         others => Invalid_Index
         ),

      An_Index_Constraint =>
        (
         Discrete_Ranges,
         others => Invalid_Index
         ),

      A_Discriminant_Constraint =>
        (
         Discriminant_Associations,
         others => Invalid_Index
         ),

      A_Component_Definition =>
        (
         Component_Definition_View,
         others => Invalid_Index
         ),

      A_Discrete_Subtype_Indication_As_Subtype_Definition =>
        (
         Subtype_Mark,
         Subtype_Constraint,
         others => Invalid_Index
         ),

      A_Discrete_Range_Attribute_Reference_As_Subtype_Definition =>
        (
         Range_Attribute,
         others => Invalid_Index
         ),

      A_Discrete_Simple_Expression_Range_As_Subtype_Definition =>
        (
         Lower_Bound,
         Upper_Bound,
         others => Invalid_Index
         ),

      A_Discrete_Subtype_Indication =>
        (
         Subtype_Mark,
         Subtype_Constraint,
         others => Invalid_Index
         ),

      A_Discrete_Range_Attribute_Reference =>
        (
         Range_Attribute,
         others => Invalid_Index
         ),

      A_Discrete_Simple_Expression_Range =>
        (
         Lower_Bound,
         Upper_Bound,
         others => Invalid_Index
         ),

      An_Unknown_Discriminant_Part =>
        (
         others => Invalid_Index
         ),

      A_Known_Discriminant_Part =>
        (
         Discriminants,
         others => Invalid_Index
         ),

      A_Record_Definition =>
        (
         Record_Components,
         others => Invalid_Index
         ),

      A_Null_Record_Definition =>
        (
         others => Invalid_Index
         ),

      A_Null_Component =>
        (
         others => Invalid_Index
         ),

      A_Variant_Part =>
        (
         Discriminant_Direct_Name,
         Variants,
         others => Invalid_Index
         ),

      A_Variant =>
        (
         Variant_Choices,
         Record_Components,
         others => Invalid_Index
         ),

      An_Others_Choice =>
        (
         others => Invalid_Index
         ),

      An_Anonymous_Access_To_Variable |
        An_Anonymous_Access_To_Constant =>
        (
         Anonymous_Access_To_Object_Subtype_Mark,
         others => Invalid_Index
         ),

      An_Anonymous_Access_To_Procedure |
        An_Anonymous_Access_To_Protected_Procedure =>
        (
         Access_To_Subprogram_Parameter_Profile,
         others => Invalid_Index
         ),

      An_Anonymous_Access_To_Function |
        An_Anonymous_Access_To_Protected_Function =>
        (
         Access_To_Subprogram_Parameter_Profile,
         Access_To_Function_Result_Profile,
         others => Invalid_Index
         ),

      A_Private_Type_Definition =>
        (
         others => Invalid_Index
         ),

      A_Tagged_Private_Type_Definition =>
        (
         others => Invalid_Index
         ),

      A_Private_Extension_Definition =>
        (
         Ancestor_Subtype_Indication,
         Definition_Interface_List,
         others => Invalid_Index
         ),

      A_Task_Definition =>
        (
         Visible_Part_Items,
         Private_Part_Items,
         others => Invalid_Index
         ),

      A_Protected_Definition =>
        (
         Visible_Part_Items,
         Private_Part_Items,
         others => Invalid_Index
         ),

      A_Formal_Private_Type_Definition =>
        (
         others => Invalid_Index
         ),

      A_Formal_Tagged_Private_Type_Definition =>
        (
         others => Invalid_Index
         ),

      A_Formal_Derived_Type_Definition =>
        (
         Subtype_Mark,
         Definition_Interface_List,
         others => Invalid_Index
         ),

      A_Formal_Discrete_Type_Definition =>
        (
         others => Invalid_Index
         ),

      A_Formal_Signed_Integer_Type_Definition =>
        (
         others => Invalid_Index
         ),

      A_Formal_Modular_Type_Definition =>
        (
         others => Invalid_Index
         ),

      A_Formal_Floating_Point_Definition =>
        (
         others => Invalid_Index
         ),

      A_Formal_Ordinary_Fixed_Point_Definition =>
        (
         others => Invalid_Index
         ),

      A_Formal_Decimal_Fixed_Point_Definition =>
        (
         others => Invalid_Index
         ),

      A_Formal_Ordinary_Interface |
        A_Formal_Limited_Interface |
        A_Formal_Task_Interface |
        A_Formal_Protected_Interface |
        A_Formal_Synchronized_Interface =>
        (
         Definition_Interface_List,
         others => Invalid_Index
         ),

      A_Formal_Unconstrained_Array_Definition =>
        (
         Index_Subtype_Definitions,
         Array_Component_Definition,
         others => Invalid_Index
         ),

      A_Formal_Constrained_Array_Definition =>
        (
         Discrete_Subtype_Definitions,
         Array_Component_Definition,
         others => Invalid_Index
         ),

      A_Formal_Pool_Specific_Access_To_Variable =>
        (
         Access_To_Object_Definition,
         others => Invalid_Index
         ),

      A_Formal_Access_To_Variable =>
        (
         Access_To_Object_Definition,
         others => Invalid_Index
         ),

      A_Formal_Access_To_Constant =>
        (
         Access_To_Object_Definition,
         others => Invalid_Index
         ),

      A_Formal_Access_To_Procedure =>
        (
         Access_To_Subprogram_Parameter_Profile,
         others => Invalid_Index
         ),

      A_Formal_Access_To_Protected_Procedure =>
        (
         Access_To_Subprogram_Parameter_Profile,
         others => Invalid_Index
         ),

      A_Formal_Access_To_Function =>
        (
         Access_To_Subprogram_Parameter_Profile,
         Access_To_Function_Result_Profile,
         others => Invalid_Index
         ),

      A_Formal_Access_To_Protected_Function =>
        (
         Access_To_Subprogram_Parameter_Profile,
         Access_To_Function_Result_Profile,
         others => Invalid_Index
         ),

      An_Aspect_Specification =>
        (
         Aspect_Mark,
         Aspect_Definition,
         others => Invalid_Index
         ),

      An_Integer_Literal =>
        (
         others => Invalid_Index
         ),

      A_Real_Literal =>
        (
         others => Invalid_Index
         ),

      A_String_Literal =>
        (
         others => Invalid_Index
         ),

      An_Identifier =>
        (
         others => Invalid_Index
         ),

      An_And_Operator =>
        (
         others => Invalid_Index
         ),

      An_Or_Operator =>
        (
         others => Invalid_Index
         ),

      An_Xor_Operator =>
        (
         others => Invalid_Index
         ),

      An_Equal_Operator =>
        (
         others => Invalid_Index
         ),

      A_Not_Equal_Operator =>
        (
         others => Invalid_Index
         ),

      A_Less_Than_Operator =>
        (
         others => Invalid_Index
         ),

      A_Less_Than_Or_Equal_Operator =>
        (
         others => Invalid_Index
         ),

      A_Greater_Than_Operator =>
        (
         others => Invalid_Index
         ),

      A_Greater_Than_Or_Equal_Operator =>
        (
         others => Invalid_Index
         ),

      A_Plus_Operator =>
        (
         others => Invalid_Index
         ),

      A_Minus_Operator =>
        (
         others => Invalid_Index
         ),

      A_Concatenate_Operator =>
        (
         others => Invalid_Index
         ),

      A_Unary_Plus_Operator =>
        (
         others => Invalid_Index
         ),

      A_Unary_Minus_Operator =>
        (
         others => Invalid_Index
         ),

      A_Multiply_Operator =>
        (
         others => Invalid_Index
         ),

      A_Divide_Operator =>
        (
         others => Invalid_Index
         ),

      A_Mod_Operator =>
        (
         others => Invalid_Index
         ),

      A_Rem_Operator =>
        (
         others => Invalid_Index
         ),

      An_Exponentiate_Operator =>
        (
         others => Invalid_Index
         ),

      An_Abs_Operator =>
        (
         others => Invalid_Index
         ),

      A_Not_Operator =>
        (
         others => Invalid_Index
         ),

      A_Character_Literal =>
        (
         others => Invalid_Index
         ),

      An_Enumeration_Literal =>
        (
         others => Invalid_Index
         ),

      An_Explicit_Dereference =>
        (
         Prefix,
         others => Invalid_Index
         ),

      A_Function_Call =>
        (
         Prefix,
         Function_Call_Parameters,
         others => Invalid_Index
         ),

      An_Indexed_Component =>
        (
         Prefix,
         Index_Expressions,
         others => Invalid_Index
         ),

      A_Slice =>
        (
         Prefix,
         Slice_Range,
         others => Invalid_Index
         ),

      A_Selected_Component =>
        (
         Prefix,
         Selector,
         others => Invalid_Index
         ),

      An_Access_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Address_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Adjacent_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Aft_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Alignment_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Base_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Bit_Order_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Body_Version_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Callable_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Caller_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Ceiling_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Class_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Component_Size_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Compose_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Constrained_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Copy_Sign_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Count_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Definite_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Delta_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Denorm_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Digits_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Exponent_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_External_Tag_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_First_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         Attribute_Designator_Expressions,
         others => Invalid_Index
         ),

      A_First_Bit_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Floor_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Fore_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Fraction_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Identity_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Image_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Input_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Last_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         Attribute_Designator_Expressions,
         others => Invalid_Index
         ),

      A_Last_Bit_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Leading_Part_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Length_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         Attribute_Designator_Expressions,
         others => Invalid_Index
         ),

      A_Machine_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Machine_Emax_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Machine_Emin_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Machine_Mantissa_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Machine_Overflows_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Machine_Radix_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Machine_Rounds_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Max_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Max_Size_In_Storage_Elements_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Min_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Model_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Model_Emin_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Model_Epsilon_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Model_Mantissa_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Model_Small_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Modulus_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Output_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Partition_ID_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Pos_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Position_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Pred_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Range_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         Attribute_Designator_Expressions,
         others => Invalid_Index
         ),

      A_Read_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Remainder_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Round_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Rounding_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Safe_First_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Safe_Last_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Scale_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Scaling_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Signed_Zeros_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Size_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Small_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Storage_Pool_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Storage_Size_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Succ_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Tag_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Terminated_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Truncation_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Unbiased_Rounding_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Unchecked_Access_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Val_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Valid_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Value_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Version_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Wide_Image_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Wide_Value_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Wide_Width_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Width_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      A_Write_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      --  ???Couldn't we say "An_Attr_Ref =>"?
      A_Machine_Rounding_Attribute |
        A_Mod_Attribute |
        A_Priority_Attribute |
        A_Stream_Size_Attribute |
        A_Wide_Wide_Image_Attribute |
        A_Wide_Wide_Value_Attribute |
        A_Wide_Wide_Width_Attribute |
        A_Max_Alignment_For_Allocation_Attribute |
        An_Overlaps_Storage_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         others => Invalid_Index
         ),

      An_Implementation_Defined_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         Attribute_Designator_Expressions,
         others => Invalid_Index
         ),

      An_Unknown_Attribute =>
        (
         Prefix,
         Attribute_Designator_Identifier,
         Attribute_Designator_Expressions,
         others => Invalid_Index
         ),

      A_Record_Aggregate =>
        (
         Record_Component_Associations,
         others => Invalid_Index
         ),

      An_Extension_Aggregate =>
        (
         Extension_Aggregate_Expression,
         Record_Component_Associations,
         others => Invalid_Index
         ),

      A_Positional_Array_Aggregate =>
        (
         Array_Component_Associations,
         others => Invalid_Index
         ),

      A_Named_Array_Aggregate =>
        (
         Array_Component_Associations,
         others => Invalid_Index
         ),

      An_And_Then_Short_Circuit =>
        (
         Short_Circuit_Operation_Left_Expression,
         Short_Circuit_Operation_Right_Expression,
         others => Invalid_Index
         ),

      An_Or_Else_Short_Circuit =>
        (
         Short_Circuit_Operation_Left_Expression,
         Short_Circuit_Operation_Right_Expression,
         others => Invalid_Index
         ),

      An_In_Membership_Test .. A_Not_In_Membership_Test =>
        (
         Membership_Test_Expression,
         Membership_Test_Choices,
         others => Invalid_Index
         ),

      A_Null_Literal =>
        (
         others => Invalid_Index
         ),

      A_Parenthesized_Expression =>
        (
         Expression_Parenthesized,
         others => Invalid_Index
         ),

      A_Type_Conversion =>
        (
         Converted_Or_Qualified_Subtype_Mark,
         Converted_Or_Qualified_Expression,
         others => Invalid_Index
         ),

      A_Qualified_Expression =>
        (
         Converted_Or_Qualified_Subtype_Mark,
         Converted_Or_Qualified_Expression,
         others => Invalid_Index
         ),

      An_Allocation_From_Subtype =>
        (
         Subpool_Name,
         Allocator_Subtype_Indication,
         others => Invalid_Index
         ),

      An_Allocation_From_Qualified_Expression =>
        (
         Subpool_Name,
         Allocator_Qualified_Expression,
         others => Invalid_Index
         ),
      A_Case_Expression =>
        (
         Case_Expression,
         Expression_Paths,
         others => Invalid_Index
         ),

      An_If_Expression =>
        (
         Expression_Paths,
         others => Invalid_Index
         ),

      A_For_All_Quantified_Expression =>
        (
         Iterator_Specification,
         Predicate,
         others => Invalid_Index
         ),

      A_For_Some_Quantified_Expression =>
        (
         Iterator_Specification,
         Predicate,
         others => Invalid_Index
         ),

      A_Pragma_Argument_Association =>
        (
         Formal_Parameter,
         Actual_Parameter,
         others => Invalid_Index
         ),

      A_Discriminant_Association =>
        (
         Discriminant_Selector_Names,
         Discriminant_Expression,
         others => Invalid_Index
         ),

      A_Record_Component_Association =>
        (
         Record_Component_Choices,
         Component_Expression,
         others => Invalid_Index
         ),

      An_Array_Component_Association =>
        (
         Array_Component_Choices,
         Component_Expression,
         others => Invalid_Index
         ),

      A_Parameter_Association =>
        (
         Formal_Parameter,
         Actual_Parameter,
         others => Invalid_Index
         ),

      A_Generic_Association =>
        (
         Formal_Parameter,
         Actual_Parameter,
         others => Invalid_Index
         ),

      A_Null_Statement =>
        (
         Label_Names,
         others => Invalid_Index
         ),

      An_Assignment_Statement =>
        (
         Label_Names,
         Assignment_Variable_Name,
         Assignment_Expression,
         others => Invalid_Index
         ),

      An_If_Statement =>
        (
         Label_Names,
         Statement_Paths,
         others => Invalid_Index
         ),

      A_Case_Statement =>
        (
         Label_Names,
         Case_Expression,
         Statement_Paths,
         others => Invalid_Index
         ),

      A_Loop_Statement =>
        (
         Label_Names,
         Statement_Identifier,
         Loop_Statements,
         others => Invalid_Index
         ),

      A_While_Loop_Statement =>
        (
         Label_Names,
         Statement_Identifier,
         While_Condition,
         Loop_Statements,
         others => Invalid_Index
         ),

      A_For_Loop_Statement =>
        (
         Label_Names,
         Statement_Identifier,
         For_Loop_Parameter_Specification,
         Loop_Statements,
         others => Invalid_Index
         ),

      A_Block_Statement =>
        (
         Label_Names,
         Statement_Identifier,
         Block_Declarative_Items,
         Block_Statements,
         Block_Exception_Handlers,
         others => Invalid_Index
         ),

      An_Exit_Statement =>
        (
         Label_Names,
         Exit_Loop_Name,
         Exit_Condition,
         others => Invalid_Index
         ),

      A_Goto_Statement =>
        (
         Label_Names,
         Goto_Label,
         others => Invalid_Index
         ),

      A_Procedure_Call_Statement =>
        (
         Label_Names,
         Called_Name,
         Call_Statement_Parameters,
         others => Invalid_Index
         ),

      A_Return_Statement =>
        (
         Label_Names,
         Return_Expression,
         others => Invalid_Index
         ),

      An_Extended_Return_Statement =>
        (
         Return_Object_Declaration,
         Extended_Return_Statements,
         Extended_Return_Exception_Handlers,
         others => Invalid_Index
         ),

      An_Accept_Statement =>
        (
         Label_Names,
         Accept_Entry_Direct_Name,
         Accept_Entry_Index,
         Accept_Parameters,
         Accept_Body_Statements,
         Accept_Body_Exception_Handlers,
         others => Invalid_Index
         ),

      An_Entry_Call_Statement =>
        (
         Label_Names,
         Called_Name,
         Call_Statement_Parameters,
         others => Invalid_Index
         ),

      A_Requeue_Statement =>
        (
         Label_Names,
         Requeue_Entry_Name,
         others => Invalid_Index
         ),

      A_Requeue_Statement_With_Abort =>
        (
         Label_Names,
         Requeue_Entry_Name,
         others => Invalid_Index
         ),

      A_Delay_Until_Statement =>
        (
         Label_Names,
         Delay_Expression,
         others => Invalid_Index
         ),

      A_Delay_Relative_Statement =>
        (
         Label_Names,
         Delay_Expression,
         others => Invalid_Index
         ),

      A_Terminate_Alternative_Statement =>
        (
         others => Invalid_Index
         ),

      A_Selective_Accept_Statement =>
        (
         Label_Names,
         Statement_Paths,
         others => Invalid_Index
         ),

      A_Timed_Entry_Call_Statement =>
        (
         Label_Names,
         Statement_Paths,
         others => Invalid_Index
         ),

      A_Conditional_Entry_Call_Statement =>
        (
         Label_Names,
         Statement_Paths,
         others => Invalid_Index
         ),

      An_Asynchronous_Select_Statement =>
        (
         Label_Names,
         Statement_Paths,
         others => Invalid_Index
         ),

      An_Abort_Statement =>
        (
         Label_Names,
         Aborted_Tasks,
         others => Invalid_Index
         ),

      A_Raise_Statement =>
        (
         Label_Names,
         Raised_Exception,
         Associated_Message,
         others => Invalid_Index
         ),

      A_Code_Statement =>
        (
         Label_Names,
         Qualified_Expression,
         others => Invalid_Index
         ),

      An_If_Path =>
        (
         Condition_Expression,
         Sequence_Of_Statements,
         others => Invalid_Index
         ),

      An_Elsif_Path =>
        (
         Condition_Expression,
         Sequence_Of_Statements,
         others => Invalid_Index
         ),

      An_Else_Path =>
        (
         Sequence_Of_Statements,
         others => Invalid_Index
         ),

      A_Case_Path =>
        (
         Case_Path_Alternative_Choices,
         Sequence_Of_Statements,
         others => Invalid_Index
         ),

      A_Select_Path =>
        (
         Guard,
         Sequence_Of_Statements,
         others => Invalid_Index
         ),

      An_Or_Path =>
        (
         Guard,
         Sequence_Of_Statements,
         others => Invalid_Index
         ),

      A_Then_Abort_Path =>
        (
         Sequence_Of_Statements,
         others => Invalid_Index
         ),

      A_Case_Expression_Path =>
        (
         Case_Path_Alternative_Choices,
         Dependent_Expression,
         others => Invalid_Index
         ),

      An_If_Expression_Path =>
        (
         Condition_Expression,
         Dependent_Expression,
         others => Invalid_Index
         ),

      An_Elsif_Expression_Path =>
        (
         Condition_Expression,
         Dependent_Expression,
         others => Invalid_Index
         ),

      An_Else_Expression_Path =>
        (
         Dependent_Expression,
         others => Invalid_Index
         ),

      A_Use_Package_Clause =>
        (
         Clause_Names,
         others => Invalid_Index
         ),

      A_Use_Type_Clause =>
        (
         Clause_Names,
         others => Invalid_Index
         ),

      A_Use_All_Type_Clause =>
        (
         Clause_Names,
         others => Invalid_Index
         ),

      A_With_Clause =>
        (
         Clause_Names,
         others => Invalid_Index
         ),

      An_Attribute_Definition_Clause =>
        (
         Representation_Clause_Name,
         Representation_Clause_Expression,
         others => Invalid_Index
         ),

      An_Enumeration_Representation_Clause =>
        (
         Representation_Clause_Name,
         Representation_Clause_Expression,
         others => Invalid_Index
         ),

      A_Record_Representation_Clause =>
        (
         Representation_Clause_Name,
         Mod_Clause_Expression,
         Component_Clauses,
         others => Invalid_Index
         ),

      An_At_Clause =>
        (
         Representation_Clause_Name,
         Representation_Clause_Expression,
         others => Invalid_Index
         ),

      A_Component_Clause =>
        (
         Representation_Clause_Name,
         Component_Clause_Position,
         Component_Clause_Range,
         others => Invalid_Index
         ),

      An_Exception_Handler =>
        (
         Choice_Parameter_Specification,
         Exception_Choices,
         Handler_Statements,
         others => Invalid_Index
         ),

      others =>
        (others => Invalid_Index)
      );

end ASIStant.Help.Queries;
