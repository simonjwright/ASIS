------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . R U L E S . C U S T O M _ 4              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2018-2019, AdaCore                      --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
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
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the a set of gnatcheck rules for gnatcheck developed
--  to satisfy some specific requests from gnatcheck users. There is not any
--  specific idea that used to group all these rules together or to make
--  a separate package for user-driven rules when we already have
--  Gnatcheck.Rules.Custom_1 and Gnatcheck.Rules.Custom_2, the only reason is
--  to keep the packages for the rules that comes out from user requests under
--  some reasonable size limit.
--
--  The rules in this packages are ordered alphabetically

pragma Ada_2012;

package Gnatcheck.Rules.Custom_4 is

   -----------------------
   --  Abort_Statements --
   -----------------------

   --  Flag abort statements.
   --
   --  This rule has no parameters.

   type Abort_Statements_Rule_Type is new
     Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Abort_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule (Rule : in out Abort_Statements_Rule_Type);

   Abort_Statements_Rule : aliased Abort_Statements_Rule_Type;

   ----------------------------------------------------
   -- Address_Specifications_For_Initialized_Objects --
   ----------------------------------------------------

   --  Flag an object declaration if the declaration contains an initialization
   --  expression and there is an address specification (via address
   --  specification clause or address aspect definition) applied to this
   --  object.
   --
   --  This rule has no parameters.

   type Address_Specifications_For_Initialized_Objects_Rule_Type is new
     Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out
        Address_Specifications_For_Initialized_Objects_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Address_Specifications_For_Initialized_Objects_Rule_Type);

   Address_Specifications_For_Initialized_Objects_Rule :
     aliased Address_Specifications_For_Initialized_Objects_Rule_Type;

   ---------------------------------------------
   -- Address_Specifications_For_Local_Objects --
   ---------------------------------------------

   --  Flag address clauses and address aspect definitions if they are applied
   --  to data objects declared in local subprogram bodies. Data objects
   --  declared in library subprogram bodies are not flagged.
   --
   --  This rule has no parameters.

   type Address_Specifications_For_Local_Objects_Rule_Type is new
     Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Address_Specifications_For_Local_Objects_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Address_Specifications_For_Local_Objects_Rule_Type);

   Address_Specifications_For_Local_Objects_Rule :
     aliased Address_Specifications_For_Local_Objects_Rule_Type;

   -------------------------------------------
   -- Bit_Records_Without_Layout_Definition --
   -------------------------------------------

   --  Flag record type declarations if a record has a component of a modular
   --  type and the record type does not have a record representation clause
   --  applied to it.
   --
   --  This rule has no parameters.

   type Bit_Records_Without_Layout_Definition_Rule_Type is new
     Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Bit_Records_Without_Layout_Definition_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Bit_Records_Without_Layout_Definition_Rule_Type);

   Bit_Records_Without_Layout_Definition_Rule :
     aliased Bit_Records_Without_Layout_Definition_Rule_Type;

   ----------------------------------------------
   -- Incomplete_Representation_Specifications --
   ----------------------------------------------

   --  Flag all record types that have a layout representation specification
   --  but without Size and Pack representation specifications.
   --
   --  This rule has no parameters.

   type Incomplete_Representation_Specifications_Rule_Type is new
     Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Incomplete_Representation_Specifications_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Incomplete_Representation_Specifications_Rule_Type);

   Incomplete_Representation_Specifications_Rule :
     aliased Incomplete_Representation_Specifications_Rule_Type;

   -----------------------
   -- Local_USE_Clauses --
   -----------------------

   --  Use clauses that are not parts of compilation unit context clause are
   --  flagged. The rule has an optional parameter for +R option:
   --
   --  Except_USE_TYPE_Clauses - do not flag local use type clauses.

   type Local_USE_Clauses_Rule_Type is new
     Rule_With_Exceptions_Template (1) with null record;

   overriding function Exception_Name
     (Rule      : Local_USE_Clauses_Rule_Type;
      Exc_Index : Exception_Index)
      return      String;

   overriding function Exception_Number
     (Rule     : Local_USE_Clauses_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Local_USE_Clauses_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule (Rule : in out Local_USE_Clauses_Rule_Type);

   Local_USE_Clauses_Rule : aliased Local_USE_Clauses_Rule_Type;

   ----------------------------
   --  Max_Identifier_Length --
   ----------------------------

   --  Flag any defining identifier that has length longer than specified by
   --  the rule parameter. The rule has a mandatory parameter for +R option:
   --
   --    N - the maximal allowed identifier length specification

   type Max_Identifier_Length_Rule_Type is new
     One_Integer_Parameter_Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Max_Identifier_Length_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Max_Identifier_Length_Rule_Type);

   Max_Identifier_Length_Rule : aliased Max_Identifier_Length_Rule_Type;

   ------------------------------------
   -- Misplaced_Representation_Items --
   ------------------------------------

   --  Flag a representation item if there is any Ada construct except
   --  another representation item for the same entity between this clause
   --  and the declaration of the entity it applies to. A representation item
   --  in the context of this rule is either a representation clause or one of
   --  the following representation pragmas:
   --
   --  Atomic   J.15.8(9/3)
   --  Atomic_Components   J.15.8(9/3)
   --  Independent   J.15.8(9/3)
   --  Independent_Components   J.15.8(9/3)
   --  Pack   J.15.3(1/3)
   --  Unchecked_Union   J.15.6(1/3)
   --  Volatile   J.15.8(9/3)
   --  Volatile_Components   J.15.8(9/3)
   --
   --  This rule has no parameters.

   type Misplaced_Representation_Items_Rule_Type is new
     Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Misplaced_Representation_Items_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Misplaced_Representation_Items_Rule_Type);

   Misplaced_Representation_Items_Rule :
     aliased Misplaced_Representation_Items_Rule_Type;

   -----------------------------
   --  No_Explicit_Real_Range --
   -----------------------------

   --  Flag a declaration of a floating point type or a decimal fixed point
   --  type, including types derived from them if no explicit range
   --  specification is provided for the type.
   --
   --  This rule has no parameters.

   type No_Explicit_Real_Range_Rule_Type is new Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out No_Explicit_Real_Range_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out No_Explicit_Real_Range_Rule_Type);

   No_Explicit_Real_Range_Rule : aliased No_Explicit_Real_Range_Rule_Type;

   --------------------------
   --  Number_Declarations --
   --------------------------

   --  Number declarations are flagged.
   --
   --  This rule has no parameters.

   type Number_Declarations_Rule_Type is new Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Number_Declarations_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Number_Declarations_Rule_Type);

   Number_Declarations_Rule : aliased Number_Declarations_Rule_Type;

   --------------------------------------
   -- Object_Declarations_Out_Of_Order --
   --------------------------------------

   --  Flag any object declaration that is located in a library unit body if
   --  this is preceding by a declaration of a program unit spec, stub or body.
   --
   --  This rule has no parameters.

   type Object_Declarations_Out_Of_Order_Rule_Type is new
     Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Object_Declarations_Out_Of_Order_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Object_Declarations_Out_Of_Order_Rule_Type);

   Object_Declarations_Out_Of_Order_Rule :
     aliased Object_Declarations_Out_Of_Order_Rule_Type;

   -----------------------------
   --  One_Construct_Per_Line --
   -----------------------------

   --  Flag any statement, declaration or representation clause if the code
   --  line where this construct starts contains some other Ada code symbols
   --  preceding or following this construct. The following constructs are not
   --  flagged:
   --    - enumeration literal specification;
   --    - parameter specifications;
   --    - discriminant specifications;
   --    - mod clauses;
   --    - loop parameter specification;
   --    - entry index specification;
   --    - choice parameter specification;
   --
   --  In case if we have two or more declarations/statements/clauses on a
   --  line and if there is no Ada code preceding the first construct, the
   --  first construct
   --
   --  This rule has no parameters.

   type One_Construct_Per_Line_Rule_Type is new
     Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out One_Construct_Per_Line_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out One_Construct_Per_Line_Rule_Type);

   One_Construct_Per_Line_Rule : aliased One_Construct_Per_Line_Rule_Type;

   -------------------------------------
   --  Outbound_Protected_Assignments --
   -------------------------------------

   --  Flag an assignment statement located in a protected body if the
   --  variable name in the left part of the statement denotes an object
   --  declared outssided ourside this protected type or object.
   --
   --  This rule has no parameters.

   type Outbound_Protected_Assignments_Rule_Type is new
     Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Outbound_Protected_Assignments_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Outbound_Protected_Assignments_Rule_Type);

   Outbound_Protected_Assignments_Rule :
     aliased Outbound_Protected_Assignments_Rule_Type;

   --------------------------------
   --  Relative_Delay_Statements --
   --------------------------------

   --  Relative delay statements are flagged. Delay until statements are not
   --  flagged.
   --
   --  This rule has no parameters.

   type Relative_Delay_Statements_Rule_Type is new
     Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Relative_Delay_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Relative_Delay_Statements_Rule_Type);

   Relative_Delay_Statements_Rule :
     aliased Relative_Delay_Statements_Rule_Type;

   -------------------------------------
   --  Single_Value_Enumeration_Types --
   -------------------------------------

   --  Flag an enumeration type definition if it contains a single enumeration
   --  literal specification
   --
   --  This rule has no parameters.

   type Single_Value_Enumeration_Types_Rule_Type is new
     Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Single_Value_Enumeration_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Single_Value_Enumeration_Types_Rule_Type);

   Single_Value_Enumeration_Types_Rule :
     aliased Single_Value_Enumeration_Types_Rule_Type;

   --------------------------
   -- Unconstrained_Arrays --
   --------------------------

   --  Unconstrained array definitions are flagged.
   --
   --  This rule has no parameters.

   type Unconstrained_Arrays_Rule_Type is new Rule_Template with null record;

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Unconstrained_Arrays_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   overriding procedure Init_Rule
     (Rule : in out Unconstrained_Arrays_Rule_Type);

   Unconstrained_Arrays_Rule : aliased Unconstrained_Arrays_Rule_Type;

end Gnatcheck.Rules.Custom_4;
