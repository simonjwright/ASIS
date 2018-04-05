------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                     A S I S T A N T . D E A L L O C                      --
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

with Asis; use Asis;
with Unchecked_Deallocation;
with ASIStant.Common; use ASIStant.Common;
with ASIStant.FuncEnum; use ASIStant.FuncEnum;

package ASIStant.DeAlloc is

------------------------------------------------------------------------------
--  Dynamic storage deallocation
------------------------------------------------------------------------------

   procedure Free is
     new Unchecked_Deallocation (Wide_String, String_Ptr);

   procedure Free is
     new Unchecked_Deallocation (Asis.Compilation_Unit_List, CUnitList_Ptr);

   procedure Free is
     new Unchecked_Deallocation (Asis.Element_List, ElemList_Ptr);

   procedure Free is
     new Unchecked_Deallocation (ElemArray, ElemArray_Ptr);

   procedure Free is
     new Unchecked_Deallocation (ElemListArray, ElemListArray_Ptr);

   procedure Free is
     new Unchecked_Deallocation (CUnitArray, CUnitArray_Ptr);

   procedure Free is
     new Unchecked_Deallocation (CUnitListArray, CUnitListArray_Ptr);

   procedure Free is
     new Unchecked_Deallocation (LineArray, LineArray_Ptr);

   procedure Free is
     new Unchecked_Deallocation (LineListArray, LineListArray_Ptr);

   procedure Free is
     new Unchecked_Deallocation (RelshipArray, RelshipArray_Ptr);

   procedure Free is
     new Unchecked_Deallocation (DDA_ArrCArray, DDA_ArrCArray_Ptr);

   procedure Free is
     new Unchecked_Deallocation (DDA_ArrCListArray, DDA_ArrCListArray_Ptr);

   procedure Free is
     new Unchecked_Deallocation (DDA_RecCArray, DDA_RecCArray_Ptr);

   procedure Free is
     new Unchecked_Deallocation (DDA_RecCListArray, DDA_RecCListArray_Ptr);

end ASIStant.DeAlloc;
