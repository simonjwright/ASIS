------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . Q U E R I E S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2013, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY  or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License  distributed with ASIS-for-GNAT; see   file --
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
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adaccore.com).                                               --
--                                                                          --
-- The original version of this component has been developed by Jean-Charles--
-- Marteau (Jean-Charles.Marteau@ensimag.imag.fr) and Serge Reboul          --
-- (Serge.Reboul@ensimag.imag.fr), ENSIMAG High School Graduates (Computer  --
-- sciences) Grenoble, France in Sema Group Grenoble, France. Now this      --
-- component is maintained by the ASIS team                                 --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Asis; use Asis;

----------------------------------------------------------
-- The goal of this package is, when we have an element --
-- to let us have ALL the possible queries for that     --
-- element that return its children.                    --
----------------------------------------------------------

with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;

package A4G.Queries is

   --  Structural_Queries is an enumeration of all the structural (syntactic)
   --  queries. Note that these enumeration literals are overloaded with the
   --  names of the query functions themselves. So for example, there is a
   --  function Clause_Names in Asis.Clauses, and there is a corresponding
   --  enumeration literal Clause_Names below.

   type Structural_Queries is
     (No_Query,

      --  The following are queries of Compilation_Unit

      Context_Clause_Elements,
      Unit_Declaration,
      Pragmas_After,

      --  The following are Boolean queries, used only by gnat2xml. These are
      --  not used in ASIS itself. Subtype Boolean_Queries below needs to be
      --  kept in synch with this.

      Has_Aliased,
      Has_Null_Exclusion,
      Is_Not_Null_Return,
      Has_Reverse,
      Has_Limited,
      Has_Synchronized,
      Has_Private,
      Has_Abstract,
      Has_Tagged,
      Is_Overriding_Declaration,
      Is_Not_Overriding_Declaration,
      Is_Prefix_Call,
      Is_Prefix_Notation,

      --  The rest below are the normal queries of Elements

      --  Queries in A4G.Queries:
      All_But_First_Associations,
      First_Parameter_Association,

      --  Queries in Asis.Clauses:
      Clause_Names,
      Component_Clause_Position,
      Component_Clause_Range,
      Component_Clauses,
      Mod_Clause_Expression,
      Representation_Clause_Expression,
      Representation_Clause_Name,

      --  Queries in Asis.Declarations:
      Aspect_Specifications,
      Body_Declarative_Items,
      Body_Exception_Handlers,
      Body_Statements,
      Declaration_Interface_List,
      Defining_Prefix,
      Defining_Selector,
      Discriminant_Part,
      Entry_Barrier,
      Entry_Family_Definition,
      Entry_Index_Specification,
      Generic_Actual_Part,
      Generic_Formal_Part,
      Generic_Unit_Name,
      Initialization_Expression,
      Iteration_Scheme_Name,
      Names,
      Object_Declaration_View,
      Parameter_Profile,
      Private_Part_Declarative_Items,
      Protected_Operation_Items,
      Renamed_Entity,
      Result_Expression,
      Result_Profile,
      Specification_Subtype_Definition,
      Subtype_Indication,
      Type_Declaration_View,
      Visible_Part_Declarative_Items,

      --  Queries in Asis.Definitions:
      Access_To_Function_Result_Profile,
      Access_To_Object_Definition,
      Access_To_Subprogram_Parameter_Profile,
      Ancestor_Subtype_Indication,
      Anonymous_Access_To_Object_Subtype_Mark,
      Array_Component_Definition,
      Aspect_Definition,
      Aspect_Mark,
      Component_Definition_View,
      Definition_Interface_List,
      Delta_Expression,
      Digits_Expression,
      Discrete_Ranges,
      Discrete_Subtype_Definitions,
      Discriminant_Associations,
      Discriminant_Direct_Name,
      Discriminants,
      Enumeration_Literal_Declarations,
      Index_Subtype_Definitions,
      Integer_Constraint,
      Lower_Bound,
      Mod_Static_Expression,
      Parent_Subtype_Indication,
      Private_Part_Items,
      Range_Attribute,
      Real_Range_Constraint,
      Record_Components,
      Record_Definition,
      Subtype_Constraint,
      Subtype_Mark,
      Upper_Bound,
      Variant_Choices,
      Variants,
      Visible_Part_Items,

      --  Queries in Asis.Elements:
      Pragma_Argument_Associations,

      --  Queries in Asis.Expressions:
      Actual_Parameter,
      Allocator_Qualified_Expression,
      Allocator_Subtype_Indication,
      Array_Component_Associations,
      Array_Component_Choices,
      Attribute_Designator_Expressions,
      Attribute_Designator_Identifier,
      Component_Expression,
      Converted_Or_Qualified_Expression,
      Converted_Or_Qualified_Subtype_Mark,
      Dependent_Expression,
      Discriminant_Expression,
      Discriminant_Selector_Names,
      Expression_Parenthesized,
      Expression_Paths,
      Extension_Aggregate_Expression,
      Formal_Parameter,
      Function_Call_Parameters,
      Index_Expressions,
      Iterator_Specification,
      Membership_Test_Choices,
      Membership_Test_Expression,
      Predicate,
      Prefix,
      Record_Component_Associations,
      Record_Component_Choices,
      Selector,
      Short_Circuit_Operation_Left_Expression,
      Short_Circuit_Operation_Right_Expression,
      Slice_Range,
      Subpool_Name,

      --  Queries in Asis.Extensions:
      Formal_Subprogram_Default,

      --  Queries in Asis.Statements:
      Aborted_Tasks,
      Accept_Body_Exception_Handlers,
      Accept_Body_Statements,
      Accept_Entry_Direct_Name,
      Accept_Entry_Index,
      Accept_Parameters,
      Assignment_Expression,
      Assignment_Variable_Name,
      Associated_Message,
      Block_Declarative_Items,
      Block_Exception_Handlers,
      Block_Statements,
      Called_Name,
      Call_Statement_Parameters,
      Case_Expression,
      Case_Path_Alternative_Choices,
      Choice_Parameter_Specification,
      Condition_Expression,
      Delay_Expression,
      Exception_Choices,
      Exit_Condition,
      Exit_Loop_Name,
      Extended_Return_Exception_Handlers,
      Extended_Return_Statements,
      For_Loop_Parameter_Specification,
      Goto_Label,
      Guard,
      Handler_Statements,
      Label_Names,
      Loop_Statements,
      Qualified_Expression,
      Raised_Exception,
      Requeue_Entry_Name,
      Return_Expression,
      Return_Object_Declaration,
      Sequence_Of_Statements,
      Statement_Identifier,
      Statement_Paths,
      While_Condition
     ); -- end Structural_Queries

   subtype Boolean_Queries is Structural_Queries
     range Has_Aliased .. Is_Prefix_Notation;

   type Query_Kinds is
     (Bug,
      --  just for the discriminant default expression
      Single_Element_Query,
      --  Queries taking an element and returning an element.
      Element_List_Query,
      --  Queries taking an element and returning a list of elements.
      Element_List_Query_With_Boolean,
      --  Queries taking an element and a boolean and returning a list
      --  of elements.
      Boolean_Query,
      --  Queries taking an element and returning a Boolean. These are used in
      --  gnat2xml.
      Single_Element_CU_Query,
      --  Queries taking Compilation_Unit and returning an element.
      Element_List_CU_Query
      --  Queries taking a Compilation_Unit and returning a list of elements.
    );

   subtype CU_Query_Kinds is Query_Kinds range
     Single_Element_CU_Query .. Element_List_CU_Query;

   type A_Single_Element_CU_Query is access
      function (Elem : Asis.Compilation_Unit) return Asis.Element;

   type A_Element_List_CU_Query is access
      function
        (Elem : Asis.Compilation_Unit;
         Bool : Boolean)
         return Asis.Element_List;

   type A_Single_Element_Query is access
      function (Elem : Asis.Element) return Asis.Element;

   type A_Element_List_Query is access
      function (Elem : Asis.Element) return Asis.Element_List;

   type A_Element_List_Query_With_Boolean is access
      function
        (Elem : Asis.Element;
         Bool : Boolean)
         return Asis.Element_List;

   type A_Boolean_Query is access
      function
        (Elem : Asis.Element)
         return Boolean;

   --  Discriminated record that can access any type of query.
   type Func_Elem
     (Q : Structural_Queries := Structural_Queries'First;
      Query_Kind : Query_Kinds := Bug)
   is
      record
         case Query_Kind is
            when Bug =>
               null;
            when Single_Element_CU_Query =>
               Func_Simple_CU : A_Single_Element_CU_Query;
            when Element_List_CU_Query =>
               Func_List_CU : A_Element_List_CU_Query;
            when Single_Element_Query =>
               Func_Simple : A_Single_Element_Query;
            when Element_List_Query =>
               Func_List : A_Element_List_Query;
            when Element_List_Query_With_Boolean =>
               Func_List_Boolean : A_Element_List_Query_With_Boolean;
               Bool : Boolean;
            when Boolean_Query =>
               Func_Boolean : A_Boolean_Query;
         end case;
      end record;

   type Query_Index is new Positive;
   subtype Query_Count is Query_Index'Base range 0 .. Query_Index'Last;
   type Func_Elem_Array is array (Query_Index range <>) of Func_Elem;

   type Query_List is array (Query_Index range <>) of Structural_Queries;
   type Query_List_Ptr is access constant Query_List;

   No_Queries : aliased constant Query_List := (1 .. 0 => <>);

   --  Appropriate_Queries returns the sequence of syntactic queries that
   --  applies to a given element or element kind. Syntactic determines the
   --  handling of prefix-notation calls; see Asis.Extensions.Iterator for
   --  details.

   function Appropriate_Queries
     (Kind : Flat_Element_Kinds'Base) return Query_List_Ptr;
   --  This version operates on an element kind, and behaves as if Syntactic is
   --  False.

   function Appropriate_Queries
     (Element : Asis.Element; Syntactic : Boolean)
     return Query_List_Ptr;
   --  This version operates on an element, and takes the Syntactic parameter
   --  into account.

   function Appropriate_Queries
     (Element : Asis.Element; Syntactic : Boolean := True)
     return Func_Elem_Array;
   --  This version is similar to the previous one, but returns the information
   --  in a different form (a Func_Elem_Array instead of a Query_List_Ptr).
   --  ???We can probably get rid of this last one at some point.

   function Num_Queries
     (Kind : Flat_Element_Kinds'Base) return A4G.Queries.Query_Count;
   --  Number of queries (i.e. subelements), assuming Syntactic = False.

   function Get_Func_Elem (Q : Structural_Queries) return Func_Elem;
   --  Get the Func_Elem corresponding to the given query, showing the
   --  parameter and result type profile, and the actual Ada function to call.

   --  The following table is a mapping from queries to the type returned by
   --  that query. Queries can also return a Nil_Element. Currently used in
   --  gnat2xml.

   type Query_Result_Type_Table is
     array (Structural_Queries) of Flat_Element_Kinds'Base;

   Query_Result_Types : constant Query_Result_Type_Table :=
     (No_Query =>
        Not_An_Element,

      --  The following are queries of Compilation_Unit

      Context_Clause_Elements =>
        A_Context_Clause_List,
      Unit_Declaration =>
        A_Declaration_Class,
      Pragmas_After =>
        An_Element_List,

      --  For the Boolean queries, the values below show what is returned to
      --  mean True. These queries can also return Not_An_Element, which means
      --  False. So for example Has_Aliased returns An_Aliased if "aliased" is
      --  present in the source code, and Not_An_Element otherwise.

      Has_Aliased =>
        An_Aliased,
      Has_Null_Exclusion =>
        A_Null_Exclusion,
      Is_Not_Null_Return =>
        A_Not_Null_Return,
      Has_Reverse =>
        A_Reverse,
      Has_Limited =>
        A_Limited,
      Has_Synchronized =>
        A_Synchronized,
      Has_Private =>
        A_Private,
      Has_Abstract =>
        An_Abstract,
      Has_Tagged =>
        A_Tagged,
      Is_Overriding_Declaration =>
        An_Overriding,
      Is_Not_Overriding_Declaration =>
        A_Not_Overriding,
      Is_Prefix_Call =>
        An_Is_Prefix_Call,
      Is_Prefix_Notation =>
        An_Is_Prefix_Notation,

      --  Queries in A4G.Queries:
      All_But_First_Associations =>
        An_Association_List,
      First_Parameter_Association =>
        An_Association_Class,

      --  Queries in Asis.Clauses:
      Clause_Names =>
        A_Name_List,
      Component_Clause_Position =>
        An_Expression_Class,
      Component_Clause_Range =>
        A_Discrete_Range_Class,
      Component_Clauses =>
        A_Component_Clause_List,
      Mod_Clause_Expression =>
        An_Expression_Class,
      Representation_Clause_Expression =>
        An_Expression_Class,
      Representation_Clause_Name =>
        A_Name_Class,

      --  Queries in Asis.Declarations:
      Aspect_Specifications =>
        An_Element_List,
      Body_Declarative_Items =>
        An_Element_List,
      Body_Exception_Handlers =>
        An_Exception_Handler_List,
      Body_Statements =>
        A_Statement_List,
      Declaration_Interface_List =>
        An_Expression_List,
      Defining_Prefix =>
        A_Name_Class,
      Defining_Selector =>
        A_Defining_Name_Class,
      Discriminant_Part =>
        A_Definition_Class,
      Entry_Barrier =>
        An_Expression_Class,
      Entry_Family_Definition =>
        A_Discrete_Subtype_Definition_Class,
      Entry_Index_Specification =>
        A_Declaration_Class,
      Generic_Actual_Part =>
        An_Association_List,
      Generic_Formal_Part =>
        An_Element_List,
      Generic_Unit_Name =>
        An_Expression_Class,
      Initialization_Expression =>
        An_Expression_Class,
      Iteration_Scheme_Name =>
        An_Element_Class,
      Names =>
        A_Defining_Name_List,
      Object_Declaration_View =>
        A_Definition_Class,
      Parameter_Profile =>
        A_Parameter_Specification_List,
      Private_Part_Declarative_Items =>
        A_Declarative_Item_List,
      Protected_Operation_Items =>
        A_Declaration_List,
      Renamed_Entity =>
        An_Expression_Class,
      Result_Expression =>
        An_Expression_Class,
      Result_Profile =>
        An_Element_Class,
      Specification_Subtype_Definition =>
        A_Discrete_Subtype_Definition_Class,
      Subtype_Indication =>
        An_Element_Class,
      Type_Declaration_View =>
        A_Definition_Class,
      Visible_Part_Declarative_Items =>
        A_Declarative_Item_List,

      --  Queries in Asis.Definitions:
      Access_To_Function_Result_Profile =>
        An_Element_Class,
      Access_To_Object_Definition =>
        A_Subtype_Indication,
      Access_To_Subprogram_Parameter_Profile =>
        A_Parameter_Specification_List,
      Ancestor_Subtype_Indication =>
        A_Subtype_Indication,
      Anonymous_Access_To_Object_Subtype_Mark =>
        An_Expression_Class,
      Array_Component_Definition =>
        A_Component_Definition,
      Aspect_Definition =>
        An_Element_Class,
      Aspect_Mark =>
        An_Element_Class,
      Component_Definition_View =>
        A_Definition_Class,
      Definition_Interface_List =>
        An_Expression_List,
      Delta_Expression =>
        An_Expression_Class,
      Digits_Expression =>
        An_Expression_Class,
      Discrete_Ranges =>
        A_Discrete_Range_List,
      Discrete_Subtype_Definitions =>
        A_Definition_List,
      Discriminant_Associations =>
        A_Discriminant_Association_List,
      Discriminant_Direct_Name =>
        A_Name_Class,
      Discriminants =>
        A_Discriminant_Specification_List,
      Enumeration_Literal_Declarations =>
        A_Declaration_List,
      Index_Subtype_Definitions =>
        An_Expression_List,
      Integer_Constraint =>
        A_Range_Constraint_Class,
      Lower_Bound =>
        An_Expression_Class,
      Mod_Static_Expression =>
        An_Expression_Class,
      Parent_Subtype_Indication =>
        A_Subtype_Indication,
      Private_Part_Items =>
        A_Declarative_Item_List,
      Range_Attribute =>
        An_Expression_Class,
      Real_Range_Constraint =>
        A_Range_Constraint_Class,
      Record_Components =>
        A_Record_Component_List,
      Record_Definition =>
        A_Definition_Class,
      Subtype_Constraint =>
        A_Constraint_Class,
      Subtype_Mark =>
        An_Expression_Class,
      Upper_Bound =>
        An_Expression_Class,
      Variant_Choices =>
        An_Element_List,
      Variants =>
        A_Variant_List,
      Visible_Part_Items =>
        A_Declarative_Item_List,

      --  Queries in Asis.Elements:
      Pragma_Argument_Associations =>
        An_Association_List,

      --  Queries in Asis.Expressions:
      Actual_Parameter =>
        An_Expression_Class,
      Allocator_Qualified_Expression =>
        An_Expression_Class,
      Allocator_Subtype_Indication =>
        A_Subtype_Indication,
      Array_Component_Associations =>
        An_Association_List,
      Array_Component_Choices =>
        An_Element_List,
      Attribute_Designator_Expressions =>
        An_Expression_List,
      Attribute_Designator_Identifier =>
        An_Expression_Class,
      Component_Expression =>
        An_Expression_Class,
      Converted_Or_Qualified_Expression =>
        An_Expression_Class,
      Converted_Or_Qualified_Subtype_Mark =>
        An_Expression_Class,
      Dependent_Expression =>
        An_Expression_Class,
      Discriminant_Expression =>
        An_Expression_Class,
      Discriminant_Selector_Names =>
        An_Expression_List,
      Expression_Parenthesized =>
        An_Expression_Class,
      Expression_Paths =>
        An_Element_List,
      Extension_Aggregate_Expression =>
        An_Expression_Class,
      Formal_Parameter =>
        An_Element_Class,
      Function_Call_Parameters =>
        An_Association_List,
      Index_Expressions =>
        An_Expression_List,
      Iterator_Specification =>
        A_Declaration_Class,
      Membership_Test_Choices =>
        An_Element_List,
      Membership_Test_Expression =>
        An_Expression_Class,
      Predicate =>
        An_Expression_Class,
      Prefix =>
        An_Expression_Class,
      Record_Component_Associations =>
        An_Association_List,
      Record_Component_Choices =>
        An_Expression_List,
      Selector =>
        An_Expression_Class,
      Short_Circuit_Operation_Left_Expression =>
        An_Expression_Class,
      Short_Circuit_Operation_Right_Expression =>
        An_Expression_Class,
      Slice_Range =>
        A_Discrete_Range_Class,
      Subpool_Name =>
        An_Expression_Class,

      --  Queries in Asis.Extensions:
      Formal_Subprogram_Default =>
        An_Expression_Class,

      --  Queries in Asis.Statements:
      Aborted_Tasks =>
        An_Expression_List,
      Accept_Body_Exception_Handlers =>
        A_Statement_List,
      Accept_Body_Statements =>
        A_Statement_List,
      Accept_Entry_Direct_Name =>
        A_Name_Class,
      Accept_Entry_Index =>
        An_Expression_Class,
      Accept_Parameters =>
        A_Parameter_Specification_List,
      Assignment_Expression =>
        An_Expression_Class,
      Assignment_Variable_Name =>
        An_Expression_Class,
      Associated_Message =>
        An_Expression_Class,
      Block_Declarative_Items =>
        A_Declarative_Item_List,
      Block_Exception_Handlers =>
        An_Exception_Handler_List,
      Block_Statements =>
        A_Statement_List,
      Called_Name =>
        An_Expression_Class,
      Call_Statement_Parameters =>
        An_Association_List,
      Case_Expression =>
        An_Expression_Class,
      Case_Path_Alternative_Choices =>
        An_Element_List,
      Choice_Parameter_Specification =>
        A_Declaration_Class,
      Condition_Expression =>
        An_Expression_Class,
      Delay_Expression =>
        An_Expression_Class,
      Exception_Choices =>
        An_Element_List,
      Exit_Condition =>
        An_Expression_Class,
      Exit_Loop_Name =>
        An_Expression_Class,
      Extended_Return_Exception_Handlers =>
        An_Exception_Handler_List,
      Extended_Return_Statements =>
        A_Statement_List,
      For_Loop_Parameter_Specification =>
        A_Declaration_Class,
      Goto_Label =>
        An_Expression_Class,
      Guard =>
        An_Expression_Class,
      Handler_Statements =>
        A_Statement_List,
      Label_Names =>
        A_Defining_Name_List,
      Loop_Statements =>
        A_Statement_List,
      Qualified_Expression =>
        An_Expression_Class,
      Raised_Exception =>
        An_Expression_Class,
      Requeue_Entry_Name =>
        A_Name_Class,
      Return_Expression =>
        An_Expression_Class,
      Return_Object_Declaration =>
        A_Declaration_Class,
      Sequence_Of_Statements =>
        A_Statement_List,
      Statement_Identifier =>
        A_Defining_Name_Class,
      Statement_Paths =>
        A_Path_List,
      While_Condition =>
        An_Expression_Class
     ); -- end Query_Result_Types

end A4G.Queries;
