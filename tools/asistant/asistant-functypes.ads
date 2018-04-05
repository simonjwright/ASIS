------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                            F U N C T Y P E S                             --
--                                                                          --
--                                 S p e c                                  --
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
------------------------------------------------------------------------------

with Asis;
with Asis.Compilation_Units.Relations;
with Asis.Data_Decomposition;
with Asis.Text;

package ASIStant.FuncTypes is

------------------------------------------------------------------------------
--  Definitions of accesses to various ASIS queries
------------------------------------------------------------------------------

   package DDA renames Asis.Data_Decomposition;

   type CtxRetBool is access
      function (Context : Asis.Context)
         return           Boolean;

   type CtxRetCUnitList is access
      function (Context : Asis.Context)
         return           Asis.Compilation_Unit_List;

   type CtxRetElemList is access
      function (Context : Asis.Context)
         return           Asis.Element_List;

   type CtxRetNull is access procedure (Context : in out Asis.Context);

   type CtxRetString is access
      function (Context : Asis.Context)
         return           Wide_String;

   type CtxStringStringRetNull is access
      procedure (Context : in out Asis.Context; Str1, Str2 : Wide_String);

   type CUnitBoolRetElemList is access
      function (CUnit : Asis.Compilation_Unit;
                Bool  : Boolean)
         return         Asis.Element_List;

   type CUnitCtxRetCUnit is access
      function (CUnit   : Asis.Compilation_Unit;
                Context : Asis.Context)
         return           Asis.Compilation_Unit;

   type CUnitCtxRetCUnitList is access
      function (CUnit   : Asis.Compilation_Unit;
                Context : Asis.Context)
         return           Asis.Compilation_Unit_List;

   type CUnitCUnitRetBool is access
      function (CUnit1, CUnit2 : Asis.Compilation_Unit)
         return                  Boolean;

   type CUnitIntIntRetElem is access
      function (CUnit      : Asis.Compilation_Unit;
                Int1, Int2 : Integer)
         return              Asis.Element;

   type CUnitListCtxRetRelship is access
      function (CUnit   : Asis.Compilation_Unit_List;
                Context : Asis.Context)
         return           Asis.Compilation_Units.Relations.Relationship;

   type CUnitListCUnitListCtxStringRetRelship is access
      function (CUnit1, CUnit2 : Asis.Compilation_Unit_List;
                Context        : Asis.Context;
                Str            : Wide_String)
         return                  Asis.Compilation_Units.Relations.Relationship;

   type CUnitListRetBool is access
      function (CUnit : Asis.Compilation_Unit_List)
         return         Boolean;

   type CUnitListRetInt is access
      function (CUnit : Asis.Compilation_Unit_List)
         return         Integer;

   type CUnitRetBool is access
      function (CUnit : Asis.Compilation_Unit)
         return         Boolean;

   type CUnitRetCtx is access
      function (CUnit : Asis.Compilation_Unit)
         return         Asis.Context;

   type CUnitRetCUnit is access
      function (CUnit : Asis.Compilation_Unit)
         return         Asis.Compilation_Unit;

   type CUnitRetCUnitList is access
      function (CUnit : Asis.Compilation_Unit)
         return         Asis.Compilation_Unit_List;

   type CUnitRetElem is access
      function (CUnit : Asis.Compilation_Unit)
         return         Asis.Element;

   type CUnitRetElemList is access
      function (CUnit : Asis.Compilation_Unit)
         return         Asis.Element_List;

   type CUnitRetString is access
      function (CUnit : Asis.Compilation_Unit)
         return         Wide_String;

   type CUnitStringRetBool is access
      function
        (CUnit : Asis.Compilation_Unit;
         Str   : Wide_String)
         return  Boolean;

   type CUnitStringRetString is access
      function
        (CUnit : Asis.Compilation_Unit;
         Str   : Wide_String)
         return  Wide_String;

   type DDA_ArrCRetDDA_ArrC is access
      function (ArrC : DDA.Array_Component) return DDA.Array_Component;

   type DDA_ArrCRetDDA_RecCList is access
      function (ArrC : DDA.Array_Component) return DDA.Record_Component_List;

   type DDA_ArrCRetElem is access
      function (ArrC : DDA.Array_Component) return Asis.Element;

   type DDA_RecCRetDDA_ArrC is access
      function (RecC : DDA.Record_Component) return DDA.Array_Component;

   type DDA_RecCRetDDA_RecCList is access
      function (RecC : DDA.Record_Component) return DDA.Record_Component_List;

   type DDA_RecCRetElem is access
      function (RecC : DDA.Record_Component) return Asis.Element;

   type ElemBoolRetElemList is access
      function
        (Element : Asis.Element;
         Bool    : Boolean)
         return    Asis.Element_List;

   type ElemCtxRetElem is access
      function
        (Element : Asis.Element;
         Context : Asis.Context)
         return    Asis.Element;

   type ElemElemBoolRetBool is access
      function
        (Element1, Element2 : Asis.Element;
         Bool               : Boolean)
         return               Boolean;

   type ElemElemBoolRetElemList is access
      function
        (Element1, Element2 : Asis.Element;
         Bool               : Boolean)
         return               Asis.Element_List;

   type ElemElemRetBool is access
      function (Element1, Element2 : Asis.Element) return Boolean;

   type ElemElemRetElem is access
      function (Element1, Element2 : Asis.Element) return Asis.Element;

   type ElemIntIntRetLineList is access
      function
        (Elem : Asis.Element;
         Int1 : Positive;
         Int2 : Natural)
         return Asis.Text.Line_List;

   type ElemListRetBool is access
      function (ElemList : Asis.Element_List) return Boolean;

   type ElemListRetInt is access
      function (ElemList : Asis.Element_List) return Integer;

   type ElemRetBool is access
      function (Element : Asis.Element) return Boolean;

   type ElemRetCUnit is access
      function (Element : Asis.Element) return Asis.Compilation_Unit;

   type ElemRetDDA_ArrC is access
      function (Element : Asis.Element) return DDA.Array_Component;

   type ElemRetDDA_RecCList is access
      function (Element : Asis.Element) return DDA.Record_Component_List;

   type ElemRetElem is access
      function (Element  : Asis.Element) return Asis.Element;

   type ElemRetElemList is access
      function (Element : Asis.Element) return Asis.Element_List;

   type ElemRetInt is access
      function (Element : Asis.Element) return Integer;

   type ElemRetLineList is access
      function (Element : Asis.Element) return Asis.Text.Line_List;

   type ElemRetSpan is access
      function (Element : Asis.Element) return Asis.Text.Span;

   type ElemRetString is access
      function (Element : Asis.Element) return Wide_String;

   type ElemSpanRetLineList is access
      function (Elem : Asis.Element; Span : Asis.Text.Span)
         return Asis.Text.Line_List;

   type IntIntRetBool is access
      function (I1, I2 : Integer) return Boolean;

   type IntIntRetInt is access
      function (I1, I2 : Integer) return Integer;

   type LineRetString is access
      function (L : Asis.Text.Line) return Wide_String;

   type RelshipRetCUnitList is access
      function
        (R    : Asis.Compilation_Units.Relations.Relationship)
         return Asis.Compilation_Unit_List;

   type RetBool is access
      function return Boolean;

   type RetCUnit is access
      function return Asis.Compilation_Unit;

   type RetCUnitList is access
      function return Asis.Compilation_Unit_List;

   type RetElem is access
      function return Asis.Element;

   type RetElemList is access
      function return Asis.Element_List;

   type RetRelship is access
      function return Asis.Compilation_Units.Relations.Relationship;

   type RetSpan is access
      function return Asis.Text.Span;

   type RetString is access
      function return Wide_String;

   type SpanRetBool is access
      function (Span : Asis.Text.Span) return Boolean;

   type SpanRetInt is access
      function (Span : Asis.Text.Span) return Integer;

   type StringCtxRetCUnit is access
      function
        (Str     : Wide_String;
         Context : Asis.Context)
         return    Asis.Compilation_Unit;

   type StringRetNull is access
      procedure (Str  : Wide_String);

   type StringStringRetBool is access
      function (S1, S2 : Wide_String) return Boolean;

   type StringStringRetString is access
      function (S1, S2 : Wide_String) return Wide_String;

end ASIStant.FuncTypes;
