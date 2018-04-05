------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                      A S I S T A N T . A D D _ O N S                     --
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

with ASIStant.Common; use ASIStant.Common;
with ASIStant.FuncEnum; use ASIStant.FuncEnum;

package body ASIStant.Add_Ons is

------------------------------------------------------------------------------
--  Additional queries
------------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --   Nil_*** queries
   ---------------------------------------------------------------------------

   function Nil_Compilation_Unit return Asis.Compilation_Unit is
   begin
      return Asis.Nil_Compilation_Unit;
   end Nil_Compilation_Unit;

   function Nil_Compilation_Unit_List return Asis.Compilation_Unit_List is
   begin
      return Asis.Nil_Compilation_Unit_List;
   end Nil_Compilation_Unit_List;

   function Nil_Element return Asis.Element is
   begin
      return Asis.Nil_Element;
   end Nil_Element;

   function Nil_Element_List return Asis.Element_List is
   begin
      return Asis.Nil_Element_List;
   end Nil_Element_List;

   function Nil_Relationship
      return Asis.Compilation_Units.Relations.Relationship is
   begin
      return Asis.Compilation_Units.Relations.Nil_Relationship;
   end Nil_Relationship;

   ---------------------------------------------------------------------------
   --  Queries to access Span fields
   ---------------------------------------------------------------------------

   function First_Line   (Sp : Asis.Text.Span) return Integer is
   begin
      return Sp.First_Line;
   end First_Line;

   function First_Column (Sp : Asis.Text.Span) return Integer is
   begin
      return Sp.First_Column;
   end First_Column;

   function Last_Line    (Sp : Asis.Text.Span) return Integer is
   begin
      return Sp.Last_Line;
   end Last_Line;

   function Last_Column  (Sp : Asis.Text.Span) return Integer is
   begin
      return Sp.Last_Column;
   end Last_Column;

   ---------------------------------------------------------------------------
   --  Queries to access Relationship fields
   ---------------------------------------------------------------------------

   function Consistent   (R : Asis.Compilation_Units.Relations.Relationship)
      return Asis.Compilation_Unit_List is
   begin
      return R.Consistent;
   end Consistent;

   function Inconsistent (R : Asis.Compilation_Units.Relations.Relationship)
      return Asis.Compilation_Unit_List is
   begin
      return R.Inconsistent;
   end Inconsistent;

   function Missing      (R : Asis.Compilation_Units.Relations.Relationship)
      return Asis.Compilation_Unit_List is
   begin
      return R.Missing;
   end Missing;

   function Circular     (R : Asis.Compilation_Units.Relations.Relationship)
      return Asis.Compilation_Unit_List is
   begin
      return R.Circular;
   end Circular;

------------------------------------------------------------------------------
--  Service queries
------------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  List handling
   ---------------------------------------------------------------------------

   function Length (Q : FuncEnum.Query_Result) return Integer is
      I : Integer;
   begin
      case Q.RType is
         when Par_CUnitList =>
            I := Q.CL.all'Length;
         when Par_ElemList =>
            I := Q.EL.all'Length;
         when Par_Line =>
            I := Asis.Text.Length (Q.L);
         when Par_Line_List =>
            I := Q.LL.all'Length;
         when Par_String =>
            I := Q.S.all'Length;
         when Par_DDA_Array_Component_List =>
            I := Q.ACL.all'Length;
         when Par_DDA_Record_Component_List =>
            I := Q.RCL.all'Length;
         when others =>
            Error (ERR_UNKNOWNSYNTAX, "Length");
      end case;
      return I;
   end Length;

   ---------------------------------------------------------------------------
   --  Wide_String handling
   ---------------------------------------------------------------------------

   function Concat (S1, S2 : Wide_String) return Wide_String is
   begin
      return S1 & S2;
   end Concat;

   function Eq     (S1, S2 : Wide_String) return Boolean is
   begin
      return S1 = S2;
   end Eq;

   function Lt     (S1, S2 : Wide_String) return Boolean is
   begin
      return S1 < S2;
   end Lt;

   function Gt     (S1, S2 : Wide_String) return Boolean is
   begin
      return S1 > S2;
   end Gt;

   ---------------------------------------------------------------------------
   --  Integer handling
   ---------------------------------------------------------------------------

   function Add (I1, I2 : Integer) return Integer is
   begin
      return I1 + I2;
   end Add;

   function Sub (I1, I2 : Integer) return Integer is
   begin
      return I1 - I2;
   end Sub;

   function Eq  (I1, I2 : Integer) return Boolean is
   begin
      return I1 = I2;
   end Eq;

   function Lt  (I1, I2 : Integer) return Boolean is
   begin
      return I1 < I2;
   end Lt;

   function Gt  (I1, I2 : Integer) return Boolean is
   begin
      return I1 > I2;
   end Gt;

end ASIStant.Add_Ons;
