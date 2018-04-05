------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                      A S I S T A N T . X T A B L E                       --
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

with Asis;
with Asis.Data_Decomposition;
with Asis.Text;
with ASIStant.Common; use ASIStant.Common;
with ASIStant.FuncEnum; use ASIStant.FuncEnum;
with ASIStant.DeAlloc; use ASIStant.DeAlloc;
with ASIStant.Grow_Table;

package ASIStant.XTable is

------------------------------------------------------------------------------
--  This package adds ASIS types support to ASIStant variable tables
------------------------------------------------------------------------------

   package DDA renames Asis.Data_Decomposition;

   ATIElem      : ElemArray_Ptr :=
                  new ElemArray (0 .. MAX_ATIELEMENTS);
   ATIElemList  : ElemListArray_Ptr :=
                  new ElemListArray (0 .. MAX_ATIELEMLISTS);
   ATICUnit     : CUnitArray_Ptr :=
                  new CUnitArray (0 .. MAX_ATICUNITS);
   ATICUnitList : CUnitListArray_Ptr :=
                  new CUnitListArray (0 .. MAX_ATICUNITLISTS);
   ATIRelship   : RelshipArray_Ptr :=
                  new RelshipArray (0 .. MAX_ATIRELATIONSHIPS);
   ATILine      : LineArray_Ptr :=
                  new LineArray (0 .. MAX_ATILINES);
   ATILineList  : LineListArray_Ptr :=
                  new LineListArray (0 .. MAX_ATILINELISTS);
   DDA_ArrC     : DDA_ArrCArray_Ptr :=
                  new DDA_ArrCArray (0 .. MAX_DDA_ARRCOMPS);
   DDA_ArrCList : DDA_ArrCListArray_Ptr :=
                  new DDA_ArrCListArray (0 .. MAX_DDA_ARRCOMPLISTS);
   DDA_RecC     : DDA_RecCArray_Ptr :=
                  new DDA_RecCArray (0 .. MAX_DDA_RECCOMPS);
   DDA_RecCList : DDA_RecCListArray_Ptr :=
                  new DDA_RecCListArray (0 .. MAX_DDA_RECCOMPLISTS);

   procedure Grow_Elem_Table is new Grow_Table
      (Table_Item => Asis.Element,
       Table_Type => ElemArray,
       Table_Ptr  => ElemArray_Ptr,
       Table      => ATIElem,
       Table_Size => MAX_ATIELEMENTS,
       Free_Proc  => Free);

   procedure Grow_ElemList_Table is new Grow_Table
      (Table_Item => ElemList_Ptr,
       Table_Type => ElemListArray,
       Table_Ptr  => ElemListArray_Ptr,
       Table      => ATIElemList,
       Table_Size => MAX_ATIELEMLISTS,
       Free_Proc  => Free);

   procedure Grow_CUnit_Table is new Grow_Table
      (Table_Item => Asis.Compilation_Unit,
       Table_Type => CUnitArray,
       Table_Ptr  => CUnitArray_Ptr,
       Table      => ATICUnit,
       Table_Size => MAX_ATICUNITS,
       Free_Proc  => Free);

   procedure Grow_CUnitList_Table is new Grow_Table
      (Table_Item => CUnitList_Ptr,
       Table_Type => CUnitListArray,
       Table_Ptr  => CUnitListArray_Ptr,
       Table      => ATICUnitList,
       Table_Size => MAX_ATICUNITLISTS,
       Free_Proc  => Free);

   procedure Grow_Relship_Table is new Grow_Table
      (Table_Item => Relship_Ptr,
       Table_Type => RelshipArray,
       Table_Ptr  => RelshipArray_Ptr,
       Table      => ATIRelship,
       Table_Size => MAX_ATIRELATIONSHIPS,
       Free_Proc  => Free);

   procedure Grow_Line_Table is new Grow_Table
      (Table_Item => Asis.Text.Line,
       Table_Type => LineArray,
       Table_Ptr  => LineArray_Ptr,
       Table      => ATILine,
       Table_Size => MAX_ATILINES,
       Free_Proc  => Free);

   procedure Grow_LineList_Table is new Grow_Table
      (Table_Item => LineList_Ptr,
       Table_Type => LineListArray,
       Table_Ptr  => LineListArray_Ptr,
       Table      => ATILineList,
       Table_Size => MAX_ATILINELISTS,
       Free_Proc  => Free);

   procedure Grow_DDA_ArrC_Table is new Grow_Table
      (Table_Item => DDA.Array_Component,
       Table_Type => DDA_ArrCArray,
       Table_Ptr  => DDA_ArrCArray_Ptr,
       Table      => DDA_ArrC,
       Table_Size => MAX_DDA_ARRCOMPS,
       Free_Proc  => Free);

   procedure Grow_DDA_ArrCList_Table is new Grow_Table
      (Table_Item => DDA_ArrCList_Ptr,
       Table_Type => DDA_ArrCListArray,
       Table_Ptr  => DDA_ArrCListArray_Ptr,
       Table      => DDA_ArrCList,
       Table_Size => MAX_DDA_ARRCOMPLISTS,
       Free_Proc  => Free);

   procedure Grow_DDA_RecC_Table is new Grow_Table
      (Table_Item => DDA.Record_Component,
       Table_Type => DDA_RecCArray,
       Table_Ptr  => DDA_RecCArray_Ptr,
       Table      => DDA_RecC,
       Table_Size => MAX_DDA_RECCOMPS,
       Free_Proc  => Free);

   procedure Grow_DDA_RecCList_Table is new Grow_Table
      (Table_Item => DDA_RecCList_Ptr,
       Table_Type => DDA_RecCListArray,
       Table_Ptr  => DDA_RecCListArray_Ptr,
       Table      => DDA_RecCList,
       Table_Size => MAX_DDA_RECCOMPLISTS,
       Free_Proc  => Free);

   ATIContext   : array (0 .. MAX_ATICONTEXTS) of Asis.Context;
   ATISpan      : array (0 .. MAX_ATISPANS) of Asis.Text.Span;
   --  These arrays will contain the user defined variables of ASIS types
   --  See package ASIStant.Common for the definitions of the above constants

   ATIElemFree      : Natural := 1;
   ATIElemListFree  : Natural := 1;
   ATICUnitFree     : Natural := 1;
   ATICUnitListFree : Natural := 1;
   ATIRelshipFree   : Natural := 1;
   ATIContextFree   : Natural := 1;
   ATISpanFree      : Natural := 1;
   ATILineFree      : Natural := 1;
   ATILineListFree  : Natural := 1;
   DDA_ArrCFree     : Natural := 1;
   DDA_ArrCListFree : Natural := 1;
   DDA_RecCFree     : Natural := 1;
   DDA_RecCListFree : Natural := 1;
   --  Positions of the first free element in arrays.
   --  Note that the 0th position is used for internal purposes.

end ASIStant.XTable;
