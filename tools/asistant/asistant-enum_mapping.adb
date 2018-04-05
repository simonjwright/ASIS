------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                 A S I S T A N T . E N U M _ M A P P I N G                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2011, Free Software Foundation, Inc.         --
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
------------------------------------------------------------------------------

with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Errors;
with Asis.Implementation;

package body ASIStant.Enum_Mapping is

------------------------------------------------------------------------------
--  Mapping of ASIS queries that return enumeration types
------------------------------------------------------------------------------

   function Status return Wide_String is
   begin
      return Asis.Errors.Error_Kinds'Wide_Image (Asis.Implementation.Status);
   end Status;

   function Unit_Kind (Compilation_Unit : Asis.Compilation_Unit)
     return Wide_String is
   begin
      return Asis.Unit_Kinds'Wide_Image (
              Asis.Compilation_Units.Unit_Kind (Compilation_Unit));
   end Unit_Kind;

   function Unit_Class (Compilation_Unit : Asis.Compilation_Unit)
     return Wide_String is
   begin
      return Asis.Unit_Classes'Wide_Image (
         Asis.Compilation_Units.Unit_Class (Compilation_Unit));
   end Unit_Class;

   function Unit_Origin (Compilation_Unit : Asis.Compilation_Unit)
     return Wide_String is
   begin
      return Asis.Unit_Origins'Wide_Image (
         Asis.Compilation_Units.Unit_Origin (Compilation_Unit));
   end Unit_Origin;

   function Element_Kind (Element : Asis.Element)
     return Wide_String is
   begin
      return Asis.Element_Kinds'Wide_Image (
         Asis.Elements.Element_Kind (Element));
   end Element_Kind;

   function Pragma_Kind (Pragma_Element : Asis.Pragma_Element)
     return Wide_String is
   begin
      return Asis.Pragma_Kinds'Wide_Image (
         Asis.Elements.Pragma_Kind (Pragma_Element));
   end Pragma_Kind;

   function Defining_Name_Kind (Defining_Name : Asis.Defining_Name)
     return Wide_String is
   begin
      return Asis.Defining_Name_Kinds'Wide_Image (
         Asis.Elements.Defining_Name_Kind (Defining_Name));
   end Defining_Name_Kind;

   function Declaration_Kind (Declaration : Asis.Declaration)
     return Wide_String is
   begin
      return Asis.Declaration_Kinds'Wide_Image (
         Asis.Elements.Declaration_Kind (Declaration));
   end Declaration_Kind;

   function Trait_Kind (Element : Asis.Element)
     return Wide_String is
   begin
      return Asis.Trait_Kinds'Wide_Image (
         Asis.Elements.Trait_Kind (Element));
   end Trait_Kind;

   function Declaration_Origin (Declaration : Asis.Declaration)
     return Wide_String is
   begin
      return Asis.Declaration_Origins'Wide_Image (
         Asis.Elements.Declaration_Origin (Declaration));
   end Declaration_Origin;

   function Mode_Kind (Declaration : Asis.Declaration)
     return Wide_String is
   begin
      return Asis.Mode_Kinds'Wide_Image (
         Asis.Elements.Mode_Kind (Declaration));
   end Mode_Kind;

   function Default_Kind (Declaration : Asis.Generic_Formal_Parameter)
     return Wide_String is
   begin
      return Asis.Subprogram_Default_Kinds'Wide_Image (
         Asis.Elements.Default_Kind (Declaration));
   end Default_Kind;

   function Definition_Kind (Definition : Asis.Definition)
     return Wide_String is
   begin
      return Asis.Definition_Kinds'Wide_Image (
         Asis.Elements.Definition_Kind (Definition));
   end Definition_Kind;

   function Type_Kind (Definition : Asis.Type_Definition)
     return Wide_String is
   begin
      return Asis.Type_Kinds'Wide_Image (
         Asis.Elements.Type_Kind (Definition));
   end Type_Kind;

   function Formal_Type_Kind (Definition : Asis.Formal_Type_Definition)
     return Wide_String is
   begin
      return Asis.Formal_Type_Kinds'Wide_Image (
          Asis.Elements.Formal_Type_Kind (Definition));
   end Formal_Type_Kind;

   function Access_Type_Kind (Definition : Asis.Access_Type_Definition)
     return Wide_String is
   begin
      return Asis.Access_Type_Kinds'Wide_Image (
         Asis.Elements.Access_Type_Kind (Definition));
   end Access_Type_Kind;

   function Access_Definition_Kind
     (Definition : Asis.Definition)
      return       Wide_String
   is
   begin
      return Asis.Access_Definition_Kinds'Wide_Image (
         Asis.Elements.Access_Definition_Kind (Definition));
   end Access_Definition_Kind;

   function Root_Type_Kind (Definition : Asis.Root_Type_Definition)
     return Wide_String is
   begin
      return Asis.Root_Type_Kinds'Wide_Image (
         Asis.Elements.Root_Type_Kind (Definition));
   end Root_Type_Kind;

   function Constraint_Kind (Definition : Asis.Constraint)
     return Wide_String is
   begin
      return Asis.Constraint_Kinds'Wide_Image (
         Asis.Elements.Constraint_Kind (Definition));
   end Constraint_Kind;

   function Discrete_Range_Kind (Definition : Asis.Discrete_Range)
     return Wide_String is
   begin
      return Asis.Discrete_Range_Kinds'Wide_Image (
         Asis.Elements.Discrete_Range_Kind (Definition));
   end Discrete_Range_Kind;

   function Expression_Kind (Expression : Asis.Expression)
     return Wide_String is
   begin
      return Asis.Expression_Kinds'Wide_Image (
         Asis.Elements.Expression_Kind (Expression));
   end Expression_Kind;

   function Operator_Kind (Element : Asis.Element)
     return Wide_String is
   begin
      return Asis.Operator_Kinds'Wide_Image (
         Asis.Elements.Operator_Kind (Element));
   end Operator_Kind;

   function Attribute_Kind (Expression : Asis.Expression)
     return Wide_String is
   begin
      return Asis.Attribute_Kinds'Wide_Image (
         Asis.Elements.Attribute_Kind (Expression));
   end Attribute_Kind;

   function Association_Kind (Association : Asis.Association)
     return Wide_String is
   begin
      return Asis.Association_Kinds'Wide_Image (
         Asis.Elements.Association_Kind (Association));
   end Association_Kind;

   function Statement_Kind (Statement : Asis.Statement)
     return Wide_String is
   begin
      return Asis.Statement_Kinds'Wide_Image (
         Asis.Elements.Statement_Kind (Statement));
   end Statement_Kind;

   function Path_Kind (Path : Asis.Path) return Wide_String is
   begin
      return Asis.Path_Kinds'Wide_Image (
         Asis.Elements.Path_Kind (Path));
   end Path_Kind;

   function Clause_Kind (Clause : Asis.Clause) return Wide_String is
   begin
      return Asis.Clause_Kinds'Wide_Image (
         Asis.Elements.Clause_Kind (Clause));
   end Clause_Kind;

   function Representation_Clause_Kind (Clause : Asis.Representation_Clause)
     return Wide_String is
   begin
      return Asis.Representation_Clause_Kinds'Wide_Image (
         Asis.Elements.Representation_Clause_Kind (Clause));
   end Representation_Clause_Kind;

   function Interface_Kind (Definition : Asis.Definition)
     return Wide_String is
   begin
      return Asis.Interface_Kinds'Wide_Image (
         Asis.Elements.Interface_Kind (Definition));
   end Interface_Kind;

   function Semantic_Dependence_Order (
               CUnit1, CUnit2 : Asis.Compilation_Unit_List;
               Context        : Asis.Context;
               Str            : Wide_String)
      return Asis.Compilation_Units.Relations.Relationship is
   begin
      return Asis.Compilation_Units.Relations.Semantic_Dependence_Order
                (CUnit1, CUnit2, Context,
                 Asis.Relation_Kinds'Wide_Value (Str));
   end Semantic_Dependence_Order;

end ASIStant.Enum_Mapping;
