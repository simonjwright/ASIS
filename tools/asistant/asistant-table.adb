------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                       A S I S T A N T . T A B L E                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2010, Free Software Foundation, Inc.         --
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

with Ada.Strings;            use  Ada.Strings;
with Ada.Strings.Wide_Fixed; use Ada.Strings.Wide_Fixed;

with ASIStant.String_Handling;
with ASIStant.XTable;        use ASIStant.XTable;

package body ASIStant.Table is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Get_Var_Idx (T : Var_Table; N : Wide_String) return Natural;
   --  Scans for the variable index in table T. Returns 0 if fails.

------------------------------------------------------------------------------
--  This package provides handling of ASIStant language variable tables
------------------------------------------------------------------------------

   function Get_Var_Idx (T : Var_Table; N : Wide_String) return Natural is
   --  Scans for the variable index in table T. Returns 0 if fails.
      Name : Var_Name;
   begin
      Move (N, Name, Right);
      ASIStant.String_Handling.To_Upper (Name);
      for i in 1 .. T.Free - 1 loop
         if T.Table.all (i).Name = Name then
            return i;
         end if;
      end loop;
      return 0;
   end Get_Var_Idx;

   function Get_Var (T : Var_Table; N : Wide_String) return Var_Info is
   --  Scans for the variable in table T. Returns Var_Unknown if fails.
      Idx : constant Integer := Get_Var_Idx (T, N);
   begin
      if Idx = 0 then
         return Var_Unknown;
      else
         return T.Table (Idx);
      end if;
   end Get_Var;

   function Get_Var_Value (VI : Var_Info) return Query_Result is
      QR : Query_Result (VI.VType);
   begin
      case VI.VType is
         when Par_Absent    => null;
         when Par_String    => QR.S   := VI.SValue;
         when Par_Boolean   => QR.B   := Boolean'Val (VI.IValue);
         when Par_CUnit     => QR.C   := ATICUnit (VI.IValue);
         when Par_CUnitList => QR.CL  := ATICUnitList (VI.IValue);
         when Par_Element   => QR.E   := ATIElem (VI.IValue);
         when Par_ElemList  => QR.EL  := ATIElemList (VI.IValue);
         when Par_Context | Par_Integer
                            => QR.I   := VI.IValue;
         when Par_Line      => QR.L   := ATILine (VI.IValue);
         when Par_Line_List  => QR.LL  := ATILineList (VI.IValue);
         when Par_Relationship
                            => QR.R   := ATIRelship (VI.IValue);
         when Par_Span      => QR.Sp  := ATISpan (VI.IValue);
         when Par_DDA_Array_Component
                            => QR.AC  := DDA_ArrC (VI.IValue);
         when Par_DDA_Array_Component_List
                            => QR.ACL := DDA_ArrCList (VI.IValue);
         when Par_DDA_Record_Component
                            => QR.RC  := DDA_RecC (VI.IValue);
         when Par_DDA_Record_Component_List
                            => QR.RCL := DDA_RecCList (VI.IValue);

         when others => Error (ERR_BADPARAM);
      end case;
      return QR;
   exception
      when others =>
         Error (ERR_INTERNAL, "Get_Var_Value");
   end Get_Var_Value;

   function Store_Var_Value (QR : Query_Result) return Var_Info is
      VI : Var_Info;
   begin
      VI.VType := QR.RType;
      case QR.RType is
         when Par_String    =>
            VI.SValue := QR.S;
         when Par_Boolean   =>
            VI.IValue := Boolean'Pos (QR.B);
         when Par_CUnit     =>
            ATICUnit (0) := QR.C;
            VI.IValue := 0;
         when Par_CUnitList =>
            ATICUnitList (0) := QR.CL;
            VI.IValue := 0;
         when Par_Element   =>
            ATIElem (0) := QR.E;
            VI.IValue := 0;
         when Par_ElemList  =>
            ATIElemList (0) := QR.EL;
            VI.IValue := 0;
         when Par_Context | Par_Integer =>
            VI.IValue := QR.I;
         when Par_Line      =>
            ATILine (0) := QR.L;
            VI.IValue := 0;
         when Par_Line_List =>
            ATILineList (0) := QR.LL;
            VI.IValue := 0;
         when Par_Relationship =>
            ATIRelship (0) := QR.R;
            VI.IValue := 0;
         when Par_Span      =>
            ATISpan (0) := QR.Sp;
            VI.IValue := 0;
         when Par_DDA_Array_Component =>
            DDA_ArrC (0) := QR.AC;
            VI.IValue := 0;
         when Par_DDA_Array_Component_List =>
            DDA_ArrCList (0) := QR.ACL;
            VI.IValue := 0;
         when Par_DDA_Record_Component =>
            DDA_RecC (0) := QR.RC;
            VI.IValue := 0;
         when Par_DDA_Record_Component_List =>
            DDA_RecCList (0) := QR.RCL;
            VI.IValue := 0;

         when others =>
            Error (ERR_BADPARAM);
      end case;

      return VI;

   exception
      when others => Error (ERR_INTERNAL, "Store_Var_Value");
   end Store_Var_Value;

   procedure Modify_Var (T : in out Var_Table; V : Var_Info) is
   --  Adds/changes variable
      Idx   : constant Integer := Get_Var_Idx (T, V.Name);
      VT    : V_TablePtr;
      VName : Var_Name := V.Name;
   begin
      ASIStant.String_Handling.To_Upper (VName);

      if Idx = 0 then

         if T.Free > T.Max then
            --  Increase length of var. table
            T.Max := T.Max + MAX_VARIABLES;
            VT := new V_Table (1 .. T.Max);

            for i in 1 .. T.Free - 1 loop
               VT (i) := T.Table (i);
            end loop;

            T.Table := VT;
         end if;

         T.Table (T.Free) := V;
         T.Table (T.Free).Name := VName;
         T.Free := T.Free + 1;
      else
         T.Table (Idx) := V;
         T.Table (Idx).Name := VName;
      end if;

   end Modify_Var;

end ASIStant.Table;
