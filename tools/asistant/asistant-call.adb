------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                        A S I S T A N T . C A L L                         --
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
------------------------------------------------------------------------------

with Asis; use Asis;

with Asis.Compilation_Units.Relations; use  Asis.Compilation_Units.Relations;
with Asis.Errors;
with Asis.Exceptions;                  use Asis.Exceptions;
with Asis.Implementation;

with ASIStant.Add_Ons;                 use ASIStant.Add_Ons;
with ASIStant.Ambiguous_Mapping;       use ASIStant.Ambiguous_Mapping;
with ASIStant.Common;                  use ASIStant.Common;
with ASIStant.FuncArr;                 use ASIStant.FuncArr;
with ASIStant.String_Handling;
with ASIStant.Text_IO;                 use ASIStant.Text_IO;
with ASIStant.XTable;                  use ASIStant.XTable;

package body ASIStant.Call is

------------------------------------------------------------------------------
--  Package for calling of ASIS queries
------------------------------------------------------------------------------

   procedure Resolve_Ambiguous (sw : in out Switch_Index; PS : Parameter_Set);
   --  Local subprogram:
   --  Attempts to find a current name/profile in the ambiguous queries list
   --  (also see asistant.ambiguous_mapping)

   function Identify_Function (N : Node_Position)
     return Function_Type is
   begin
      return Function_Type'Wide_Value ("FT_" & CurStat.Tree (N).SValue.all);
   exception
      when Constraint_Error => return FT_CALL;
   end Identify_Function;

   procedure Resolve_Ambiguous
     (sw :  in out Switch_Index;
      PS : Parameter_Set)
   is
      Amb : Amb_Index;
      Match : Boolean;
   begin
      Amb := To_Amb_Index (sw);
      if Amb = Not_Ambiguous then
         null;
         --  it just means no overloading for a given query

      else
         for i in 1 .. AI_LENGTH loop
            exit when Amb_Info (Amb, i).New_Index = Invalid_Index;
            Match := True;
            for j in Parameter_Range loop
               if Amb_Info (Amb, i).Synt (j) /= PS (j).RType then
                  Match := False;
                  exit;
               end if;
            end loop;
            if Match then
               sw := Amb_Info (Amb, i).New_Index;
               return;
            end if;
         end loop;
         Error (ERR_UNKNOWNSYNTAX, Switch_Index'Wide_Image (sw));
      end if;
   end Resolve_Ambiguous;

   function Call_ASIStant_Function
     (N  : Wide_String;
      PS : Parameter_Set)
      return Query_Result
   is
      sw :    Switch_Index;
      Match : Boolean;
      Local : Boolean := True;
      Key : Func_Syntax_Enum;
      Name : Wide_String (N'Range) := N;

   begin
      ASIStant.String_Handling.To_Upper (Name);

      begin
         if Name = "AND" then
            return (Par_Boolean, PS (1).B and PS (2).B);
         elsif Name = "OR" then
            return (Par_Boolean, PS (1).B or PS (2).B);
         elsif Name = "NOT" then
            return (Par_Boolean, not PS (1).B);
         elsif Name = "LENGTH" then
            return (Par_Integer, Length (PS (1)));
         else
            Local := False;
            sw := Switch_Index'Wide_Value (Name);
         end if;
      exception
         when Constraint_Error =>
            if Local then
               Error (ERR_BADPARAM, "for query " & Name);
            else
               Error (ERR_UNKNOWNQUERY, Name);
            end if;
      end;

      ATIPut_Line ("Calling query " & Name, 0);

      Resolve_Ambiguous (sw, PS);

      --  Determine key and check syntax
      for i in Switch_Info'Range loop

         if sw in Switch_Info (i).From .. Switch_Info (i).To then
            Key := Switch_Info (i).SelectID;
            Match := True;

            for j in Parameter_Range loop

               if Switch_Info (i).Synt (j) /= PS (j).RType then

                  Match := False;
                  exit;
               end if;

            end loop;

            if not Match then
               Error (ERR_UNKNOWNSYNTAX, Name);
            end if;
            exit;
         end if;

      end loop;

      case Key is

         when CtxRetBool_Syn =>
            return (Par_Boolean,
                    FCtxRetBool (sw) (ATIContext (PS (1).I))
                   );

         when CtxRetCUnitList_Syn =>
            return
              (Par_CUnitList,
               Save_CUnitList (FCtxRetCUnitList (sw) (ATIContext (PS (1).I))));

         when CtxRetElemList_Syn =>
            return
              (Par_ElemList,
               Save_ElemList (FCtxRetElemList (sw) (ATIContext (PS (1).I))));

         when CtxRetNull_Syn =>
            FCtxRetNull (sw) (ATIContext (PS (1).I));
            return (RType => Par_Absent);

         when CtxRetString_Syn =>
            return (Par_String,
                    Save_String (FCtxRetString (sw) (ATIContext (PS (1).I))));

         when CtxStringStringRetNull_Syn =>
            FCtxStringStringRetNull (sw)
             (ATIContext (PS (1).I), PS (2).S.all, PS (3).S.all);

            return (RType => Par_Absent);

         when CUnitBoolRetElemList_Syn =>
            return (Par_ElemList,
                    Save_ElemList (FCUnitBoolRetElemList (sw)
                      (PS (1).C, PS (2).B)));

         when CUnitCtxRetCUnit_Syn =>
            return (Par_CUnit,
                    FCUnitCtxRetCUnit (sw) (PS (1).C, ATIContext (PS (2).I)));

         when CUnitCtxRetCUnitList_Syn =>
            return (Par_CUnitList,
                    Save_CUnitList (FCUnitCtxRetCUnitList (sw)
                      (PS (1).C, ATIContext (PS (2).I))));

         when CUnitCUnitRetBool_Syn =>
            return (Par_Boolean,
                    FCUnitCUnitRetBool (sw) (PS (1).C, PS (2).C));

         when CUnitIntIntRetElem_Syn =>
            return (Par_Element,
                    FCUnitIntIntRetElem (sw) (PS (1).C, PS (2).I, PS (3).I));

         when CUnitListCtxRetRelship_Syn =>
            return (Par_Relationship,
                    Save_Relship (FCUnitListCtxRetRelship (sw)
                      (PS (1).CL.all, ATIContext (PS (2).I))));
         when CUnitListCUnitListCtxStringRetRelship_Syn =>
            return (Par_Relationship,
                    Save_Relship (FCUnitListCUnitListCtxStringRetRelship (sw)
                      (PS (1).CL.all, PS (2).CL.all, ATIContext (PS (3).I),
                       PS (4).S.all)));
         when CUnitListRetBool_Syn =>
            return (Par_Boolean,
                    FCUnitListRetBool (sw) (PS (1).CL.all));

--        when CUnitListRetInt_Syn =>
--            return (Par_Integer,
--                    FCUnitListRetInt (sw) (PS (1).CL.all));

         when CUnitRetBool_Syn =>
            return (Par_Boolean,
                    FCUnitRetBool (sw) (PS (1).C));

         when CUnitRetCUnit_Syn =>
            return (Par_CUnit,
                    FCUnitRetCUnit (sw) (PS (1).C));

         when CUnitRetCUnitList_Syn =>
            return (Par_CUnitList,
                    Save_CUnitList (FCUnitRetCUnitList (sw) (PS (1).C)));

         when CUnitRetElem_Syn =>
            return (Par_Element,
                    FCUnitRetElem (sw) (PS (1).C));

         when CUnitRetElemList_Syn =>
            return (Par_ElemList,
                    Save_ElemList (FCUnitRetElemList (sw) (PS (1).C)));

         when CUnitRetString_Syn =>
            return (Par_String,
                    Save_String (FCUnitRetString (sw) (PS (1).C)));

         when CUnitStringRetBool_Syn =>
            return (Par_Boolean,
                    FCUnitStringRetBool (sw) (PS (1).C, PS (2).S.all)
                  );

         when CUnitStringRetString_Syn =>
            return (Par_String,
                    Save_String (FCUnitStringRetString (sw)
                      (PS (1).C, PS (2).S.all)));

         when DDA_ArrCRetDDA_ArrC_Syn =>
            return (Par_DDA_Array_Component,
                    FDDA_ArrCRetDDA_ArrC (sw) (PS (1).AC));
         when DDA_ArrCRetDDA_RecCList_Syn =>
            return (Par_DDA_Record_Component_List,
                    Save_DDA_RecCList
                      (FDDA_ArrCRetDDA_RecCList (sw) (PS (1).AC)));
         when DDA_ArrCRetElem_Syn =>
            return (Par_Element,
                    FDDA_ArrCRetElem (sw) (PS (1).AC));
         when DDA_RecCRetDDA_ArrC_Syn =>
            return (Par_DDA_Array_Component,
                    FDDA_RecCRetDDA_ArrC (sw) (PS (1).RC));
         when DDA_RecCRetDDA_RecCList_Syn =>
            return (Par_DDA_Record_Component_List,
                    Save_DDA_RecCList
                      (FDDA_RecCRetDDA_RecCList (sw) (PS (1).RC)));
         when DDA_RecCRetElem_Syn =>
            return (Par_Element,
                    FDDA_RecCRetElem (sw) (PS (1).RC));
         when ElemBoolRetElemList_Syn =>
            return (Par_ElemList,
                    Save_ElemList (FElemBoolRetElemList (sw)
                       (PS (1).E, PS (2).B)));

         when ElemCtxRetElem_Syn =>
            return (Par_Element,
                    FElemCtxRetElem (sw) (PS (1).E, ATIContext (PS (2).I)));

         when ElemElemBoolRetBool_Syn =>
            return (Par_Boolean,
                    FElemElemBoolRetBool (sw) (PS (1).E, PS (2).E, PS (3).B));

         when ElemElemBoolRetElemList_Syn =>
            return (Par_ElemList,
                    Save_ElemList (FElemElemBoolRetElemList (sw)
                      (PS (1).E, PS (2).E, PS (3).B)));

         when ElemElemRetBool_Syn =>
            return (Par_Boolean,
                    FElemElemRetBool (sw) (PS (1).E, PS (2).E));

         when ElemElemRetElem_Syn =>
            return (Par_Element,
                    FElemElemRetElem (sw) (PS (1).E, PS (2).E));

         when ElemIntIntRetLineList_Syn =>
            return (Par_Line_List,
                    Save_LineList (FElemIntIntRetLineList (sw)
                      (PS (1).E, PS (2).I, PS (3).I)));

         when ElemListRetBool_Syn =>
            return (Par_Boolean,
                    FElemListRetBool (sw) (PS (1).EL.all));

--        when ElemListRetInt_Syn =>
--            return (Par_Integer,
--                    FElemListRetInt (sw) (PS (1).EL.all));

         when ElemRetBool_Syn =>
            return (Par_Boolean,
                    FElemRetBool (sw) (PS (1).E));

         when ElemRetCUnit_Syn =>
            return (Par_CUnit,
                    FElemRetCUnit (sw) (PS (1).E));

         when ElemRetDDA_ArrC_Syn =>
            return (Par_DDA_Array_Component,
                    FElemRetDDA_ArrC (sw) (PS (1).E));

         when ElemRetDDA_RecCList_Syn =>
            return (Par_DDA_Record_Component_List,
                    Save_DDA_RecCList (FElemRetDDA_RecCList (sw) (PS (1).E)));

         when ElemRetElem_Syn =>
            return (Par_Element,
                    FElemRetElem (sw) (PS (1).E));

         when ElemRetElemList_Syn =>
            return (Par_ElemList,
                    Save_ElemList (FElemRetElemList (sw) (PS (1).E)));

         when ElemRetInt_Syn =>
            return (Par_Integer,
                    FElemRetInt (sw) (PS (1).E));

         when ElemRetLineList_Syn =>
            return (Par_Line_List,
                    Save_LineList (FElemRetLineList (sw) (PS (1).E)));

         when ElemRetSpan_Syn =>
            return (Par_Span,
                    FElemRetSpan (sw) (PS (1).E));

         when ElemRetString_Syn =>
            return (Par_String,
                    Save_String (FElemRetString (sw) (PS (1).E)));

         when IntIntRetBool_Syn =>
            return (Par_Boolean,
                    FIntIntRetBool (sw) (PS (1).I, PS (2).I));

         when IntIntRetInt_Syn =>
            return (Par_Integer,
                    FIntIntRetInt (sw) (PS (1).I, PS (2).I));

         when LineRetString_Syn =>
            return (Par_String,
                    Save_String (FLineRetString (sw) (PS (1).L)));

         when RelshipRetCUnitList_Syn =>
            return (Par_CUnitList,
                    Save_CUnitList (
                      FRelshipRetCUnitList (sw) (PS (1).R.all)));

         when RetBool_Syn =>
            return (Par_Boolean,
                    FRetBool (sw).all);

         when RetCUnit_Syn =>
            return (Par_CUnit,
                    FRetCUnit (sw).all);

         when RetCUnitList_Syn =>
            return (Par_CUnitList,
                    Save_CUnitList (Compilation_Unit_List
                      (FRetCUnitList (sw).all)));

         when RetElem_Syn =>
            return (Par_Element,
                    FRetElem (sw).all);

         when RetElemList_Syn =>
            return (Par_ElemList,
                    Save_ElemList (Element_List (FRetElemList (sw).all)));

         when RetRelship_Syn =>
            return (Par_Relationship,
                    Save_Relship (Relationship (FRetRelship (sw).all)));

         when RetString_Syn =>
            return (Par_String,
                    Save_String (Wide_String (FRetString (sw).all)));

         when SpanRetBool_Syn =>
            return (Par_Boolean,
                    FSpanRetBool (sw) (PS (1).Sp));

         when SpanRetInt_Syn =>
            return (Par_Integer,
                    FSpanRetInt (sw) (PS (1).Sp));

         when StringCtxRetCUnit_Syn =>
            return (Par_CUnit,
                    FStringCtxRetCUnit (sw)
                      (PS (1).S.all, ATIContext (PS (2).I)));

         when StringRetNull_Syn =>
            FStringRetNull (sw) (PS (1).S.all);
            return (RType => Par_Absent);

         when StringStringRetBool_Syn =>
            return (Par_Boolean,
                    FStringStringRetBool (sw) (PS (1).S.all, PS (2).S.all));

         when StringStringRetString_Syn =>
            return (Par_String,
                    Save_String (FStringStringRetString (sw)
                      (PS (1).S.all, PS (2).S.all)));

         when others => Error (ERR_NOTSUPPORTED, Switch_Index'Wide_Image (sw));

      end case;

   exception
      when ASIStant_ERROR =>
         raise ASIStant_ERROR;
      when ASIS_Inappropriate_Context          |
           ASIS_Inappropriate_Container        |
           ASIS_Inappropriate_Compilation_Unit |
           ASIS_Inappropriate_Element          |
           ASIS_Inappropriate_Line             |
           ASIS_Inappropriate_Line_Number      |
           ASIS_Failed                         =>

         ATIPut_Line ("Exception is raised by ASIS query "
                                  & Name & ".", 5);
         ATIPut_Line ("Status : " &
           Asis.Errors.Error_Kinds'Wide_Image (Asis.Implementation.Status), 5);
         ATIPut_Line ("Diagnosis : ", 5);
         ATIPut_Line (Asis.Implementation.Diagnosis, 5);
         raise ASIStant_ASIS_ERROR;
   end Call_ASIStant_Function;

   function Save_CUnitList (C : Asis.Compilation_Unit_List)
      return CUnitList_Ptr
   is
   begin
      return new Asis.Compilation_Unit_List'(C);
   end Save_CUnitList;

   function Save_ElemList  (E : Asis.Element_List) return ElemList_Ptr is
   begin
      return new Asis.Element_List'(E);
   end Save_ElemList;

   function Save_LineList  (L : Asis.Text.Line_List) return LineList_Ptr is
   begin
      return new Asis.Text.Line_List'(L);
   end Save_LineList;

   function Save_DDA_ArrCList  (A : DDA.Array_Component_List)
      return DDA_ArrCList_Ptr is
   begin
      return new DDA.Array_Component_List'(A);
   end Save_DDA_ArrCList;

   function Save_DDA_RecCList  (R : DDA.Record_Component_List)
      return DDA_RecCList_Ptr is
   begin
      return new DDA.Record_Component_List'(R);
   end Save_DDA_RecCList;

   function Save_String (S : Wide_String) return String_Ptr is
   begin
      return new Wide_String'(S);
   end Save_String;

   function Save_Relship (R : Asis.Compilation_Units.Relations.Relationship)
      return Relship_Ptr is
   begin
      return new Asis.Compilation_Units.Relations.Relationship'(R);
   end Save_Relship;

end ASIStant.Call;
