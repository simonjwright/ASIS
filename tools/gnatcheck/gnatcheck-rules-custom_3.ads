------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . R U L E S . C U S T O M _ 3              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

package Gnatcheck.Rules.Custom_3 is

   ----------------------------
   -- Binary_Case_Statements --
   ----------------------------

   --  Flag a case statement if this statement has only two alternatives, one
   --  containing exactly one choice, the other containing exactly one choice
   --  or the “OTHERS” choice
   --
   --  This rule has no parameters.

   type Binary_Case_Statements_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Binary_Case_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule (Rule : in out Binary_Case_Statements_Rule_Type);

   Binary_Case_Statements_Rule : aliased Binary_Case_Statements_Rule_Type;

   ------------------------------------------
   -- Default_Values_For_Record_Components --
   ------------------------------------------

   --  Flag a record component declaration if it contains a default expression.
   --  Do not flag record component declarations in protected definitions.
   --  Do not flag discriminant specifications)
   --
   --  This rule has no parameters.

   type Default_Values_For_Record_Components_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Default_Values_For_Record_Components_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure
     Init_Rule (Rule : in out Default_Values_For_Record_Components_Rule_Type);

   Default_Values_For_Record_Components_Rule :
     aliased Default_Values_For_Record_Components_Rule_Type;

   -----------------------------------
   -- Deriving_From_Predefined_Type --
   -----------------------------------

   --  Flag derived type declaration if the ultimate ancestor type is a
   --  predefined Ada type. Do not flag record extensions and private
   --  extensions. The rule is checked inside expanded generics.

   --
   --  This rule has no parameters.

   type Deriving_From_Predefined_Type_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Deriving_From_Predefined_Type_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule (Rule : in out Deriving_From_Predefined_Type_Rule_Type);

   Deriving_From_Predefined_Type_Rule :
     aliased Deriving_From_Predefined_Type_Rule_Type;

   ----------------------------------------
   -- Enumeration_Representation_Clauses --
   ----------------------------------------

   --  Flag enumeration representation clauses.
   --
   --  This rule has no parameters.

   type Enumeration_Representation_Clauses_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Enumeration_Representation_Clauses_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule
     (Rule : in out Enumeration_Representation_Clauses_Rule_Type);

   Enumeration_Representation_Clauses_Rule :
     aliased Enumeration_Representation_Clauses_Rule_Type;

   --------------------------
   -- Expression_Functions --
   --------------------------

   --  Flag each function expression declared in a package specification
   --  (including specification of local packages and generic package
   --  specifications).
   --
   --  This rule has no parameters.

   type Expression_Functions_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Expression_Functions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule (Rule : in out Expression_Functions_Rule_Type);

   Expression_Functions_Rule : aliased Expression_Functions_Rule_Type;

   ---------------------------
   -- Fixed_Equality_Checks --
   ---------------------------

   --  Calls to the predefined equality operations for Fixed point types are
   --  detected

   --  Both "=" and "/=" operations are checked.

   --  A fixed point type here is an ordinary fixed type, a decimal fixed point
   --  type or a type derived from any of such types

   --  User-defined equality operations are not detected.

   --  "=" and "/=" operations for floating point types are not detected
   --
   --  This rule has no parameters.

   type Fixed_Equality_Checks_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Fixed_Equality_Checks_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Checks if Element is a name of a predefined "=" or "/=" operator from
   --  a (function) call with fixed arguments

   procedure Init_Rule (Rule : in out Fixed_Equality_Checks_Rule_Type);

   Fixed_Equality_Checks_Rule : aliased Fixed_Equality_Checks_Rule_Type;

   ------------------------
   -- Nested_Subprograms --
   ------------------------

   --  Flag any subprogram declaration, subprogram body declaration, subprogram
   --  instantiation, expression function declaration or subprogram body stub
   --  that is not a completion of another subprogram declaration and that is
   --  declared within subprogram body (including bodies of generic
   --  subprograms), task body or entry body directly or indirectly (that is -
   --  inside a local nested package). Protected subprograms are not flagged.
   --  Null procedure declarations are not flagged. Procedure declarations
   --  completed by null procedure declarations are not flagged.
   --
   --  This rule has no parameters.

   type Nested_Subprograms_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Nested_Subprograms_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule (Rule : in out Nested_Subprograms_Rule_Type);

   Nested_Subprograms_Rule : aliased Nested_Subprograms_Rule_Type;

   ----------------
   -- Null_Paths --
   ----------------

   --  Flag a statement sequence that is a component of an IF, CASE or LOOP
   --  statement if this sequences consists of NULL statements only
   --
   --  This rule has no parameters.

   type Null_Paths_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Null_Paths_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule (Rule : in out Null_Paths_Rule_Type);

   Null_Paths_Rule : aliased Null_Paths_Rule_Type;

   --------------------------------
   -- Objects_Of_Anonymous_Types --
   --------------------------------

   --  Flag any object declaration located immediately within a package
   --  declaration or a package body (including generic packages) if it uses
   --  anonymous access or array type definition. Record component definitions
   --  and parameter specifications are not flagged. Formal object declarations
   --  defined with anonymous access definitions are flagged.
   --
   --  This rule has no parameters.

   type Objects_Of_Anonymous_Types_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Objects_Of_Anonymous_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule (Rule : in out Objects_Of_Anonymous_Types_Rule_Type);

   Objects_Of_Anonymous_Types_Rule :
     aliased Objects_Of_Anonymous_Types_Rule_Type;

   ------------------------------
   -- POS_On_Enumeration_Types --
   ------------------------------

   --  Flag 'Pos attribute in case if the attribute prefix has an enumeration
   --  type (including types derived from enumeration types).
   --
   --  This rule has no parameters.

   type POS_On_Enumeration_Types_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out POS_On_Enumeration_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule (Rule : in out POS_On_Enumeration_Types_Rule_Type);

   POS_On_Enumeration_Types_Rule : aliased POS_On_Enumeration_Types_Rule_Type;

   -----------------------------------
   -- Representation_Specifications --
   -----------------------------------

   --  Flag each record representation clause, enumeration representation
   --  clause and representation attribute clause. Flag each aspect definition
   --  that defines a representation aspect.
   --
   --  This rule has no parameters.

   type Representation_Specifications_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Representation_Specifications_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule (Rule : in out Representation_Specifications_Rule_Type);

   Representation_Specifications_Rule :
     aliased Representation_Specifications_Rule_Type;

   -----------------------
   -- Subprogram_Access --
   -----------------------

   --  Flag all construct that belongs to access_to_subprogram_definition
   --  syntax category, and all access definitions that define access to
   --  subprogram.
   --
   --  This rule has no parameters.

   type Subprogram_Access_Rule_Type is new Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Subprogram_Access_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule (Rule : in out Subprogram_Access_Rule_Type);

   Subprogram_Access_Rule : aliased Subprogram_Access_Rule_Type;

   --------------------------------------
   -- Unchecked_Conversions_As_Actuals --
   --------------------------------------

   --  Flag call to instantiation of Unchecked_Conversion if it is an actual in
   --  procedure or entry call or if it is a default value in a subprogram or
   --  entry parameter specification.
   --
   --  This rule has no parameters.

   type Unchecked_Conversions_As_Actuals_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unchecked_Conversions_As_Actuals_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule
     (Rule : in out Unchecked_Conversions_As_Actuals_Rule_Type);

   Unchecked_Conversions_As_Actuals_Rule :
     aliased Unchecked_Conversions_As_Actuals_Rule_Type;

   -----------------------------------
   -- Unchecked_Address_Conversions --
   -----------------------------------

   --  Flag instantiations of Ada.Unchecked_Conversion if the actual for the
   --  formal type Source is the System.Address type (or a type derived from
   --  (it, and the actual for the formaltype Target is an access type
   --  (including types derived from access types). This include cases when the
   --  actual for Source is a private type and its full declaration is a type
   --  derived from System.Address, and cases when the actual for Target is
   --  a private type and its full declaration is an access type. The rule is
   --  checked inside expanded generics.
   --
   --  This rule has no parameters.

   type Unchecked_Address_Conversions_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unchecked_Address_Conversions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule (Rule : in out Unchecked_Address_Conversions_Rule_Type);

   Unchecked_Address_Conversions_Rule :
     aliased Unchecked_Address_Conversions_Rule_Type;

   ------------------------------------
   -- Uninitialized_Global_Variables --
   ------------------------------------

   --  Flag an object declaration located immediately within a package
   --  declaration, a generic package declaration or a package body, if it does
   --  not have an explicit initialization. Do not flag deferred constant
   --  declarations and declarations of objects of limited types.
   --
   --  This rule has no parameters.

   type Uninitialized_Global_Variables_Rule_Type is new
     Rule_Template with null record;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Uninitialized_Global_Variables_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Implements the check described above

   procedure Init_Rule
     (Rule : in out Uninitialized_Global_Variables_Rule_Type);

   Uninitialized_Global_Variables_Rule :
     aliased Uninitialized_Global_Variables_Rule_Type;

end Gnatcheck.Rules.Custom_3;
