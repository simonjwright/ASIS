------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--            A S I S T A N T . A M B I G U O U S _ M A P P I N G           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1997-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- ASIStant is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIStant is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- ASIStant is an evolution of ASIStint tool that was created by            --
-- Vasiliy Fofanov as part of a collaboration between Software Engineering  --
-- Laboratory of the Swiss Federal Institute of Technology in Lausanne,     --
-- Switzerland, and the Scientific Research Computer Center of the Moscow   --
-- University, Russia, supported by the Swiss National Science Foundation   --
-- grant #7SUPJ048247, "Development of ASIS for GNAT with industry quality" --
--                                                                          --
-- ASIStant is distributed as a part of the ASIS implementation for GNAT    --
-- (ASIS-for-GNAT) and is maintained by AdaCore (http://www.adacore.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Asis.Extensions;

with ASIStant.FuncEnum; use ASIStant.FuncEnum;

package ASIStant.Ambiguous_Mapping is

------------------------------------------------------------------------------
--  Mapping of ambigious ASIS queries
------------------------------------------------------------------------------

   type Amb_Index is (
      Not_Ambiguous,
      Browse,
      Corresponding_Body,
      Corresponding_Body_Stub,
      Corresponding_Children,
      Corresponding_Declaration,
      Corresponding_Parent_Declaration,
      Corresponding_Subunit,
      Corresponding_Subunit_Parent_Body,
      Corresponding_Type_Declaration,
      DDA_Array_Components,
      DDA_Record_Components,
      Debug_Image,
      Enclosing_Element,
      Eq,
      Exists,
      Is_Equal,
      Is_Identical,
      Is_Nil,
      Lines,
      Subunits
      );
      --  All supported functions

   package Switch_Index_To_Amb_Index_Conversions is new
     Asis.Extensions.Generic_Enum_Conversion
     (From => Switch_Index, To => Amb_Index, Default => Not_Ambiguous);
   function To_Amb_Index (X : Switch_Index) return Amb_Index renames
     Switch_Index_To_Amb_Index_Conversions.Convert;
   --  Convert by name from Switch_Index to Amb_Index

   type Amb_Node is record
      New_Index : Switch_Index;
      Synt      : Func_Syntax;
   end record;

   AI_LENGTH : constant Natural := 5;

   Amb_Info : array (Amb_Index, 1 .. AI_LENGTH) of Amb_Node := (
      (
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Browse,
            (Par_Element, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
         (Browse_CU,
            (Par_Element, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Corresponding_Body_CU_Ctx,
            (Par_CUnit, Par_CUnit, Par_Context, Par_Absent, Par_Absent)),
         (Corresponding_Body_CU,
            (Par_CUnit, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
         (Corresponding_Body_Ctx,
            (Par_Element, Par_Element, Par_Context, Par_Absent, Par_Absent)),
         (Corresponding_Body,
            (Par_Element, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Corresponding_Body_Stub_Ctx,
            (Par_Element, Par_Element, Par_Context, Par_Absent, Par_Absent)),
         (Corresponding_Body_Stub,
            (Par_Element, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Corresponding_Children_Ctx,
            (Par_CUnitList, Par_CUnit, Par_Context, Par_Absent, Par_Absent)),
         (Corresponding_Children,
            (Par_CUnitList, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Corresponding_Declaration_CU_Ctx,
            (Par_CUnit, Par_CUnit, Par_Context, Par_Absent, Par_Absent)),
         (Corresponding_Declaration_CU,
            (Par_CUnit, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
         (Corresponding_Declaration_Ctx,
            (Par_Element, Par_Element, Par_Context, Par_Absent, Par_Absent)),
         (Corresponding_Declaration,
            (Par_Element, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Corresponding_Parent_Declaration_Ctx,
            (Par_CUnit, Par_CUnit, Par_Context, Par_Absent, Par_Absent)),
         (Corresponding_Parent_Declaration,
            (Par_CUnit, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Corresponding_Subunit_Ctx,
            (Par_Element, Par_Element, Par_Context, Par_Absent, Par_Absent)),
         (Corresponding_Subunit,
            (Par_Element, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Corresponding_Subunit_Parent_Body_Ctx,
            (Par_CUnit, Par_CUnit, Par_Context, Par_Absent, Par_Absent)),
         (Corresponding_Subunit_Parent_Body,
            (Par_CUnit, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Corresponding_Type_Declaration_Ctx,
            (Par_Element, Par_Element, Par_Context, Par_Absent, Par_Absent)),
         (Corresponding_Type_Declaration,
            (Par_Element, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (DDA_Array_Components,
            (Par_DDA_Array_Component, Par_Element, Par_Absent,
             Par_Absent, Par_Absent)),
         (DDA_Array_Components_1,
            (Par_DDA_Array_Component, Par_DDA_Record_Component, Par_Absent,
             Par_Absent, Par_Absent)),
         (DDA_Array_Components_2,
            (Par_DDA_Array_Component, Par_DDA_Array_Component, Par_Absent,
             Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (DDA_Record_Components,
            (Par_DDA_Record_Component_List, Par_Element, Par_Absent,
             Par_Absent, Par_Absent)),
         (DDA_Record_Components_1,
            (Par_DDA_Record_Component_List, Par_DDA_Record_Component,
             Par_Absent, Par_Absent, Par_Absent)),
         (DDA_Record_Components_2,
            (Par_DDA_Record_Component_List, Par_DDA_Record_Component,
             Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Debug_Image_Ctx,
            (Par_String, Par_Context, Par_Absent, Par_Absent, Par_Absent)),
         (Debug_Image_CU,
            (Par_String, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
         (Debug_Image,
            (Par_String, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
         (Debug_Image,
            (Par_String, Par_Line, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Enclosing_Element_EEE,
            (Par_Element, Par_Element, Par_Element, Par_Absent, Par_Absent)),
         (Enclosing_Element,
            (Par_Element, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Eq,
            (Par_Boolean, Par_Integer, Par_Integer, Par_Absent, Par_Absent)),
         (Eq_SS,
            (Par_Boolean, Par_String, Par_String, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Exists,
            (Par_Boolean, Par_Context, Par_Absent, Par_Absent, Par_Absent)),
         (Exists_CU,
            (Par_Boolean, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Is_Equal_CU,
            (Par_Boolean, Par_CUnit, Par_CUnit, Par_Absent, Par_Absent)),
         (Is_Equal,
            (Par_Boolean, Par_Element, Par_Element, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Is_Identical_CU,
            (Par_Boolean, Par_CUnit, Par_CUnit, Par_Absent, Par_Absent)),
         (Is_Identical,
            (Par_Boolean, Par_Element, Par_Element, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Is_Nil_CUL,
            (Par_Boolean, Par_CUnitList, Par_Absent, Par_Absent, Par_Absent)),
         (Is_Nil_CU,
            (Par_Boolean, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
         (Is_Nil_EL,
            (Par_Boolean, Par_ElemList, Par_Absent, Par_Absent, Par_Absent)),
         (Is_Nil,
            (Par_Boolean, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
         (Is_Nil_Sp,
            (Par_Boolean, Par_Span, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Lines,
            (Par_Line_List, Par_Element, Par_Absent, Par_Absent, Par_Absent)),
         (Lines_1,
            (Par_Line_List, Par_Element, Par_Span, Par_Absent, Par_Absent)),
         (Lines_2,
            (Par_Line_List, Par_Element, Par_Integer, Par_Integer,
             Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      ),

      (
         (Subunits_Ctx,
            (Par_CUnitList, Par_CUnit, Par_Context, Par_Absent, Par_Absent)),
         (Subunits,
            (Par_CUnitList, Par_CUnit, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent)),
         (Invalid_Index,
            (Par_Absent, Par_Absent, Par_Absent, Par_Absent, Par_Absent))
      )

   );

end ASIStant.Ambiguous_Mapping;
