------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                         A S I S T A N T . S E T                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2012, Free Software Foundation, Inc.         --
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

------------------------------------------------------------------------------
--  Package for declaring and changing of ASIStant variables
------------------------------------------------------------------------------

with Ada.Strings;            use Ada.Strings;
with Ada.Strings.Wide_Fixed; use Ada.Strings.Wide_Fixed;

with ASIStant.Common;        use ASIStant.Common;
with ASIStant.Evaluate;      use ASIStant.Evaluate;
with ASIStant.FuncEnum;      use ASIStant.FuncEnum;
with ASIStant.Table;         use ASIStant.Table;
with ASIStant.XTable;        use ASIStant.XTable;

package body ASIStant.Set is

   ------------------------
   --  Local subprograms --
   ------------------------

   function Build_Var (Name : Wide_String; VI : Var_Info) return Var_Info;
   --  Generates a data structure for a variable called Name with a value VI

   function Build_Var (Name : Wide_String; VI : Var_Info) return Var_Info is

      VI1 : Var_Info;

   begin

      VI1 := VI;
      Move (Name, VI1.Name, Right);

      case VI.VType is

         when Par_Absent   =>
            Error (ERR_BADPARAM);

         when Par_CUnit   =>
            if ATICUnitFree = MAX_ATICUNITS then
               Grow_CUnit_Table;
            end if;
            ATICUnit (ATICUnitFree) := ATICUnit (VI.IValue);
            VI1.IValue := ATICUnitFree;
            ATICUnitFree := ATICUnitFree + 1;

         when Par_CUnitList =>
            if ATICUnitListFree = MAX_ATICUNITLISTS then
               Grow_CUnitList_Table;
            end if;
            ATICUnitList (ATICUnitListFree) := ATICUnitList (VI.IValue);
            ATICUnitList (0) := null;
            VI1.IValue := ATICUnitListFree;
            ATICUnitListFree := ATICUnitListFree + 1;

         when Par_Element =>
            if ATIElemFree = MAX_ATIELEMENTS then
               Grow_Elem_Table;
            end if;
            ATIElem (ATIElemFree) := ATIElem (VI.IValue);
            VI1.IValue := ATIElemFree;
            ATIElemFree := ATIElemFree + 1;

         when Par_ElemList =>
            if ATIElemListFree = MAX_ATIELEMLISTS then
               Grow_ElemList_Table;
            end if;
            ATIElemList (ATIElemListFree) := ATIElemList (VI.IValue);
            ATIElemList (0) := null;
            VI1.IValue := ATIElemListFree;
            ATIElemListFree := ATIElemListFree + 1;

         when Par_Relationship =>
            if ATIRelshipFree = MAX_ATIRELATIONSHIPS then
               Grow_Relship_Table;
            end if;
            ATIRelship (ATIRelshipFree) := ATIRelship (VI.IValue);
            ATIRelship (0) := null;
            VI1.IValue := ATIRelshipFree;
            ATIRelshipFree := ATIRelshipFree + 1;

         when Par_Span    =>
            ATISpan (ATISpanFree) := ATISpan (VI.IValue);
            VI1.IValue := ATISpanFree;
            ATISpanFree := ATISpanFree + 1;

         when Par_DDA_Array_Component =>
            if DDA_ArrCFree = MAX_DDA_ARRCOMPS then
               Grow_DDA_ArrC_Table;
            end if;
            DDA_ArrC (DDA_ArrCFree) := DDA_ArrC (VI.IValue);
            VI1.IValue := DDA_ArrCFree;
            DDA_ArrCFree := DDA_ArrCFree + 1;

         when Par_DDA_Array_Component_List =>
            if DDA_ArrCListFree = MAX_DDA_ARRCOMPLISTS then
               Grow_DDA_ArrCList_Table;
            end if;
            DDA_ArrCList (DDA_ArrCListFree) := DDA_ArrCList (VI.IValue);
            DDA_ArrCList (0) := null;
            VI1.IValue := DDA_ArrCListFree;
            DDA_ArrCListFree := DDA_ArrCListFree + 1;

         when Par_DDA_Record_Component =>
            if DDA_RecCFree = MAX_DDA_RECCOMPS then
               Grow_DDA_RecC_Table;
            end if;
            DDA_RecC (DDA_RecCFree) := DDA_RecC (VI.IValue);
            VI1.IValue := DDA_RecCFree;
            DDA_RecCFree := DDA_RecCFree + 1;

         when Par_DDA_Record_Component_List =>
            if DDA_RecCListFree = MAX_DDA_RECCOMPLISTS then
               Grow_DDA_RecCList_Table;
            end if;
            DDA_RecCList (DDA_RecCListFree) := DDA_RecCList (VI.IValue);
            DDA_RecCList (0) := null;
            VI1.IValue := DDA_RecCListFree;
            DDA_RecCListFree := DDA_RecCListFree + 1;

         when Par_Line =>
            if ATILineFree = MAX_ATILINES then
               Grow_Line_Table;
            end if;
            ATILine (ATILineFree) := ATILine (VI.IValue);
            VI1.IValue := ATILineFree;
            ATILineFree := ATILineFree + 1;

         when Par_Line_List =>
            if ATILineListFree = MAX_ATILINELISTS then
               Grow_LineList_Table;
            end if;
            ATILineList (ATILineListFree) := ATILineList (VI.IValue);
            ATILineList (0) := null;
            VI1.IValue := ATILineListFree;
            ATILineListFree := ATILineListFree + 1;

         when others => null;
      end case;

      return VI1;

      exception
         when Constraint_Error => --  ASIS types arrays overflow
            Error (ERR_TABLEFULL);

   end Build_Var;

   procedure Set (N : Node_Position) is

      NPtrV, NPtrE : Node_Position;
      VI : Var_Info;
      QR : Query_Result;

   begin

      if CurStat.Tree (N).NValue = 0 then
         Error (ERR_NEEDPARAM);
      end if;

      NPtrV := CurStat.Tree (N).NValue;

      --  Check that there is no built-in query with this name. We simply
      --  convert the name to Switch_Index, if successful query with this name
      --  exists
      declare
         NPtr  : constant Node_Position := CurStat.Tree (NPtrV).NValue;
         Name  : constant Wide_String   := CurStat.Tree (NPtr).SValue.all;
         Query : Switch_Index;
         pragma Warnings (Off, Query);
      begin
         Query := Switch_Index'Wide_Value (Name);
         Error (ERR_BADVARNAME, Name);
      exception
         when Constraint_Error =>
            --  CE raised is in fact an expected outcome!
            null;
      end;

      if CurStat.Tree (NPtrV).Next_Node = 0 then
      --  Create a Context variable
         NPtrV := CurStat.Tree (NPtrV).NValue;
         Move (CurStat.Tree (NPtrV).SValue.all, VI.Name, Right);
         VI.VType    := Par_Context;
         VI.IValue   := ATIContextFree;
         ATIContextFree := ATIContextFree + 1;
         Modify_Var (CurTable, VI);
         return;
      end if;

      NPtrE := CurStat.Tree (NPtrV).Next_Node;

      if CurStat.Tree (NPtrE).Next_Node /= 0 then
      --  Only 1 or 2 parameters allowed
         Error (ERR_TOOMANYPARAMS);
      end if;

      NPtrV := CurStat.Tree (NPtrV).NValue;   --  variable name
      NPtrE := CurStat.Tree (NPtrE).NValue;   --  expression

      if CurStat.Tree (NPtrV).NType /= NT_VARIABLE then
         Error (ERR_BADPARAM);
      end if;

      QR := Evaluate_Node (NPtrE);

      VI := Build_Var (CurStat.Tree (NPtrV).SValue.all, Store_Var_Value (QR));

      Modify_Var (CurTable, VI);

   end Set;

end ASIStant.Set;
