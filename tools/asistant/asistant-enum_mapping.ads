------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                 A S I S T A N T . E N U M _ M A P P I N G                --
--                                                                          --
--                                 S p e c                                  --
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

with Asis;
with Asis.Compilation_Units.Relations;

package ASIStant.Enum_Mapping is

------------------------------------------------------------------------------
--  Mapping of ASIS queries that return enumeration types
------------------------------------------------------------------------------

   function Status
     return Wide_String;

   function Unit_Kind
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String;

   function Unit_Class
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String;

   function Unit_Origin
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String;

   function Element_Kind (Element : Asis.Element) return Wide_String;

   function Pragma_Kind
     (Pragma_Element : Asis.Pragma_Element)
      return           Wide_String;

   function Defining_Name_Kind
     (Defining_Name : Asis.Defining_Name)
      return          Wide_String;

   function Declaration_Kind
     (Declaration : Asis.Declaration)
      return        Wide_String;

   function Trait_Kind (Element : Asis.Element) return Wide_String;

   function Declaration_Origin
     (Declaration : Asis.Declaration)
      return        Wide_String;

   function Mode_Kind (Declaration : Asis.Declaration) return Wide_String;

   function Default_Kind
     (Declaration : Asis.Generic_Formal_Parameter)
      return        Wide_String;

   function Definition_Kind
     (Definition : Asis.Definition)
      return       Wide_String;

   function Type_Kind
     (Definition : Asis.Type_Definition)
      return       Wide_String;

   function Formal_Type_Kind
     (Definition : Asis.Formal_Type_Definition)
      return       Wide_String;

   function Access_Type_Kind
     (Definition : Asis.Access_Type_Definition)
      return       Wide_String;

   function Access_Definition_Kind
     (Definition : Asis.Definition)
      return       Wide_String;

   function Root_Type_Kind
     (Definition : Asis.Root_Type_Definition)
      return       Wide_String;

   function Constraint_Kind
     (Definition : Asis.Constraint)
      return       Wide_String;

   function Discrete_Range_Kind
     (Definition : Asis.Discrete_Range)
      return       Wide_String;

   function Expression_Kind
     (Expression : Asis.Expression)
      return       Wide_String;

   function Operator_Kind (Element : Asis.Element) return Wide_String;

   function Attribute_Kind (Expression : Asis.Expression) return Wide_String;

   function Association_Kind
     (Association : Asis.Association)
      return        Wide_String;

   function Statement_Kind (Statement : Asis.Statement) return Wide_String;

   function Path_Kind (Path : Asis.Path) return Wide_String;

   function Clause_Kind (Clause : Asis.Clause) return Wide_String;

   function Representation_Clause_Kind
     (Clause : Asis.Representation_Clause)
      return   Wide_String;

   function Interface_Kind (Definition : Asis.Definition) return Wide_String;
   --  ASIS 2005

------------------------------------------------------------------------------
--  Mapping of Semantic_Dependence_Order query that uses enumeration parameter
------------------------------------------------------------------------------

   function Semantic_Dependence_Order
     (CUnit1, CUnit2 : Asis.Compilation_Unit_List;
      Context        : Asis.Context;
      Str            : Wide_String)
      return           Asis.Compilation_Units.Relations.Relationship;

end ASIStant.Enum_Mapping;
