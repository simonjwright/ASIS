------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                      A S I S T A N T . F U N C A R R                     --
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

with ASIStant.FuncEnum, ASIStant.FuncTypes;
use  ASIStant.FuncEnum, ASIStant.FuncTypes;

package ASIStant.FuncArr is

------------------------------------------------------------------------------
--  Arrays of accesses to ASIS queries
------------------------------------------------------------------------------

   FCtxRetBool : array
     (Exists .. Has_Associations)
     of CtxRetBool;

   FCtxRetCUnitList : array
     (Compilation_Unit_Bodies .. Library_Unit_Declarations)
     of CtxRetCUnitList;

   FCtxRetElemList : array
     (Configuration_Pragmas .. Configuration_Pragmas)
     of CtxRetElemList;

   FCtxRetNull : array
     (Close .. Open)
     of CtxRetNull;

   FCtxRetString : array
     (Debug_Image_Ctx .. Parameters)
     of CtxRetString;

   FCtxStringStringRetNull : array
     (Associate .. Associate)
     of CtxStringStringRetNull;

   FCUnitBoolRetElemList : array
     (Context_Clause_Elements .. Context_Clause_Elements)
     of CUnitBoolRetElemList;

   FCUnitCtxRetCUnit : array
     (Corresponding_Body_CU_Ctx .. Corresponding_Subunit_Parent_Body_Ctx)
     of CUnitCtxRetCUnit;

   FCUnitCtxRetCUnitList : array
     (Corresponding_Children_Ctx .. Subunits_Ctx)
     of CUnitCtxRetCUnitList;

   FCUnitCUnitRetBool : array
     (Is_Equal_CU .. Is_Identical_CU)
     of CUnitCUnitRetBool;

   FCUnitIntIntRetElem : array
     (Find_Element .. Find_Element)
     of CUnitIntIntRetElem;

   FCUnitListRetBool : array
     (Is_Nil_CUL .. Is_Nil_CUL)
     of CUnitListRetBool;

   FCUnitListCtxRetRelship : array
     (Elaboration_Order .. Elaboration_Order)
     of CUnitListCtxRetRelship;

   FCUnitListCUnitListCtxStringRetRelship : array
     (Semantic_Dependence_Order .. Semantic_Dependence_Order)
     of CUnitListCUnitListCtxStringRetRelship;

   FCUnitRetBool : array
     (Can_Be_Main_Program .. Is_Nil_CU)
     of CUnitRetBool;

   FCUnitRetCtx : array
     (Enclosing_Context .. Enclosing_Context)
     of CUnitRetCtx;

   FCUnitRetCUnit : array
     (Corresponding_Body_CU .. Corresponding_Subunit_Parent_Body)
     of CUnitRetCUnit;

   FCUnitRetCUnitList : array
     (Corresponding_Children .. Subunits)
     of CUnitRetCUnitList;

   FCUnitRetElem : array
     (Browse_CU .. Unit_Declaration)
     of CUnitRetElem;

   FCUnitRetElemList : array
     (Compilation_Pragmas .. Compilation_Pragmas)
     of CUnitRetElemList;

   FCUnitRetString : array
     (Compilation_Command_Line_Options .. Unique_Name)
     of CUnitRetString;

   FCUnitStringRetBool : array
     (Has_Attribute .. Has_Attribute)
     of CUnitStringRetBool;

   FCUnitStringRetString : array
     (Attribute_Values .. Attribute_Values)
     of CUnitStringRetString;

   FDDA_ArrCRetDDA_ArrC : array
      (DDA_Array_Components_2 .. DDA_Array_Components_2)
     of DDA_ArrCRetDDA_ArrC;

   FDDA_ArrCRetDDA_RecCList : array
      (DDA_Discriminant_Components_2 .. DDA_Record_Components_2)
     of DDA_ArrCRetDDA_RecCList;

   FDDA_ArrCRetElem : array
      (DDA_Component_Indication .. DDA_Component_Indication)
     of DDA_ArrCRetElem;

   FDDA_RecCRetDDA_ArrC : array
      (DDA_Array_Components_1 .. DDA_Array_Components_1)
     of DDA_RecCRetDDA_ArrC;

   FDDA_RecCRetDDA_RecCList : array
      (DDA_Discriminant_Components_1 .. DDA_Record_Components_1)
     of DDA_RecCRetDDA_RecCList;

   FDDA_RecCRetElem : array
      (DDA_Component_Declaration .. DDA_Component_Declaration)
     of DDA_RecCRetElem;

   FElemBoolRetElemList : array
     (Accept_Body_Exception_Handlers .. Visible_Part_Items)
     of ElemBoolRetElemList;

   FElemCtxRetElem : array
     (Corresponding_Body_Ctx .. Corresponding_Type_Declaration_Ctx)
     of ElemCtxRetElem;

   FElemElemBoolRetBool : array
     (Is_Referenced .. Is_Referenced)
     of ElemElemBoolRetBool;

   FElemElemBoolRetElemList : array
     (References .. References)
     of ElemElemBoolRetElemList;

   FElemElemRetBool : array
     (Is_Equal .. Is_Identical)
     of ElemElemRetBool;

   FElemElemRetElem : array
     (Enclosing_Element_EEE .. Enclosing_Element_EEE)
     of ElemElemRetElem;

   FElemIntIntRetLineList : array
     (Lines_2 .. Lines_2)
     of ElemIntIntRetLineList;

   FElemListRetBool : array
     (Is_Nil_EL .. Is_Nil_EL)
     of ElemListRetBool;

   FElemRetBool : array
     (Declarations_Is_Private_Present .. Is_Text_Available)
     of ElemRetBool;

   FElemRetCUnit : array
     (Enclosing_Compilation_Unit .. Enclosing_Compilation_Unit)
     of ElemRetCUnit;

   FElemRetDDA_ArrC : array
     (DDA_Array_Components .. DDA_Array_Components)
     of ElemRetDDA_ArrC;

   FElemRetDDA_RecCList : array
     (DDA_Record_Components .. DDA_Record_Components)
     of ElemRetDDA_RecCList;

   FElemRetElem  : array
     (Accept_Entry_Direct_Name .. While_Condition)
     of ElemRetElem;

   FElemRetElemList : array
     (Aborted_Tasks .. Variant_Choices)
     of ElemRetElemList;

   FElemRetInt : array
     (Hash .. Hash)
     of ElemRetInt;

   FElemRetLineList : array
     (Lines .. Lines)
     of ElemRetLineList;

   FElemRetSpan : array
     (Compilation_Span .. Element_Span)
     of ElemRetSpan;

   FElemRetString : array
     (Access_Type_Kind .. Value_Image)
     of ElemRetString;

   FElemSpanRetLineList : array
     (Lines_1 .. Lines_1)
     of ElemSpanRetLineList;

   FIntIntRetBool : array
     (Eq .. Lt)
     of IntIntRetBool;

   FIntIntRetInt : array
     (Add .. Sub)
     of IntIntRetInt;

   FLineRetString : array
     (Comment_Image .. Non_Comment_Image)
     of LineRetString;

   FRelshipRetCUnitList : array
     (Consistent .. Circular)
     of RelshipRetCUnitList;

   FRetBool : array
     (Attributes_Are_Supported .. Record_Component_Associations_Normalized)
     of RetBool;

   FRetCUnit : array
     (FuncEnum.Nil_Compilation_Unit .. FuncEnum.Nil_Compilation_Unit)
     of RetCUnit;

   FRetCUnitList : array
     (FuncEnum.Nil_Compilation_Unit_List .. FuncEnum.Nil_Compilation_Unit_List)
     of RetCUnitList;

   FRetElem : array
     (FuncEnum.Nil_Element .. FuncEnum.Nil_Element)
     of RetElem;

   FRetElemList : array
     (FuncEnum.Nil_Element_List .. FuncEnum.Nil_Element_List)
     of RetElemList;

   FRetRelship : array
     (Nil_Relationship .. Nil_Relationship)
     of RetRelship;

   FRetString : array
     (Asis_Implementor .. Status)
     of RetString;

   FSpanRetBool : array
     (Is_Nil_Sp .. Is_Nil_Sp)
     of SpanRetBool;

   FSpanRetInt : array
     (First_Column .. Last_Line)
     of SpanRetInt;

   FStringCtxRetCUnit : array
     (Compilation_Unit_Body .. Library_Unit_Declaration)
     of StringCtxRetCUnit;

   FStringRetNull : array
     (Finalize .. Initialize)
     of StringRetNull;

   FStringStringRetBool : array
     (Eq_SS .. Lt_SS)
     of StringStringRetBool;

   FStringStringRetString : array
     (Concat .. Concat)
     of StringStringRetString;

   procedure Initialize_Query_Arrays;
   --  Initializes all above arrays with accesses to corresponding ASIS and
   --  ASIStant queries. This can not be done at elaboration time because of
   --  the resulting elaboration circularity.

end ASIStant.FuncArr;
