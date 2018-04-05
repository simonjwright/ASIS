------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                    A S I S T A N T . E V A L U A T E                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2013, Free Software Foundation, Inc.         --
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

with ASIStant.Call;     use ASIStant.Call;
with ASIStant.Common;   use ASIStant.Common;
with ASIStant.Table;    use ASIStant.Table;
with ASIStant.XTable;   use ASIStant.XTable;

package body ASIStant.Evaluate is

------------------------------------------------------------------------------
--  ASIStant expressions evaluation
------------------------------------------------------------------------------

   function Evaluate_Node (NPtr : Node_Position) return Query_Result is

      VI : Var_Info;
      NPtr1, NPtrP : Node_Position;

   begin

      case CurStat.Tree (NPtr).NType is

         when NT_BOOLEAN     =>
            return (Par_Boolean, Boolean'Val (CurStat.Tree (NPtr).IValue));

         when NT_INTEGER     =>
            return (Par_Integer,
                    Integer'Wide_Value (CurStat.Tree (NPtr).SValue.all));

         when NT_STRING      =>
            return (Par_String, Save_String (CurStat.Tree (NPtr).SValue.all));

         when NT_VARIABLE    =>
            VI := Get_Var (CurTable, CurStat.Tree (NPtr).SValue.all);
            if VI /= Var_Unknown then
               return Get_Var_Value (VI);
            else
               return Call_ASIStant_Function
                 (CurStat.Tree (NPtr).SValue.all, Empty_Parameter_Set);
            end if;

         when NT_FUNCTION =>
            VI := Get_Var (CurTable, CurStat.Tree (NPtr).SValue.all);

            if VI /= Var_Unknown then
               --  An indexing
               if VI.VType /= Par_ElemList and then
                  VI.VType /= Par_CUnitList and then
                  VI.VType /= Par_Line_List and then
                  VI.VType /= Par_DDA_Array_Component_List and then
                 VI.VType /= Par_DDA_Record_Component_List
               then
                  Error (ERR_BADVARTYPE);
               end if;

               NPtr1 := CurStat.Tree (NPtr).NValue;
               NPtr1 := CurStat.Tree (NPtr1).NValue;

               if CurStat.Tree (NPtr1).Next_Node /= 0 then
                  Error (ERR_BADLISTELEM);
               end if;

               declare
                  QR : constant Query_Result := Evaluate_Node (NPtr1);
               begin
                  if QR.RType /= Par_Integer then
                     Error (ERR_BADLISTELEM);
                  end if;

                  if VI.VType = Par_ElemList then
                     return (Par_Element, ATIElemList (VI.IValue).all (QR.I));
                  elsif VI.VType = Par_CUnitList then
                     return (Par_CUnit, ATICUnitList (VI.IValue).all (QR.I));
                  elsif VI.VType = Par_Line_List then
                     return (Par_Line, ATILineList (VI.IValue).all (QR.I));
                  elsif VI.VType = Par_DDA_Array_Component_List then
                     return (Par_DDA_Array_Component,
                             DDA_ArrCList (VI.IValue).all (QR.I));
                  elsif VI.VType = Par_DDA_Record_Component_List then
                     return (Par_DDA_Record_Component,
                             DDA_RecCList (VI.IValue).all (QR.I));
                  else
                     Error (ERR_INTERNAL);
                  end if;
               exception
                  when Constraint_Error =>
                     Error (ERR_NOTINRANGE);
               end;

            else
               --  A query call
               declare
                  PS  : Parameter_Set;
               begin
                  --  Determine syntax
                  NPtrP := CurStat.Tree (NPtr).NValue;

                  for i in Parameter_Range loop
                     NPtr1 := CurStat.Tree (NPtrP).NValue;
                     PS (i) := Evaluate_Node (NPtr1);
                     NPtrP := CurStat.Tree (NPtrP).Next_Node;
                     exit when NPtrP = 0;
                  end loop;

                  if NPtrP /= 0 then
                     Error (ERR_TOOMANYPARAMS);
                  end if;

                  return Call_ASIStant_Function
                           (CurStat.Tree (NPtr).SValue.all, PS);
               end;
            end if;

         when others     =>
            Error (ERR_INTERNAL, "Module : Evaluate. ST node : " &
                   Node_Type'Wide_Image (CurStat.Tree (NPtr).NType));
      end case;

   end Evaluate_Node;

end ASIStant.Evaluate;
