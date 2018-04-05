------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                      A S I S T A N T . A D D _ O N S                     --
--                                                                          --
--                                 S p e c                                  --
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

with Asis;
with Asis.Compilation_Units.Relations;
with Asis.Text;

with ASIStant.FuncEnum;

package ASIStant.Add_Ons is

------------------------------------------------------------------------------
--  Additional queries
------------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --   Nil_*** queries
   ---------------------------------------------------------------------------

   function Nil_Compilation_Unit      return Asis.Compilation_Unit;
   function Nil_Compilation_Unit_List return Asis.Compilation_Unit_List;
   function Nil_Element               return Asis.Element;
   function Nil_Element_List          return Asis.Element_List;
   function Nil_Relationship
      return Asis.Compilation_Units.Relations.Relationship;

   ---------------------------------------------------------------------------
   --  Queries to access Span fields
   ---------------------------------------------------------------------------

   function First_Line   (Sp : Asis.Text.Span) return Integer;
   function First_Column (Sp : Asis.Text.Span) return Integer;
   function Last_Line    (Sp : Asis.Text.Span) return Integer;
   function Last_Column  (Sp : Asis.Text.Span) return Integer;

   ---------------------------------------------------------------------------
   --  Queries to access Relationship fields
   ---------------------------------------------------------------------------

   function Consistent   (R : Asis.Compilation_Units.Relations.Relationship)
      return Asis.Compilation_Unit_List;
   function Inconsistent (R : Asis.Compilation_Units.Relations.Relationship)
      return Asis.Compilation_Unit_List;
   function Missing      (R : Asis.Compilation_Units.Relations.Relationship)
      return Asis.Compilation_Unit_List;
   function Circular     (R : Asis.Compilation_Units.Relations.Relationship)
      return Asis.Compilation_Unit_List;

------------------------------------------------------------------------------
--  Service queries
------------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  List handling
   ---------------------------------------------------------------------------

   function Length (Q : ASIStant.FuncEnum.Query_Result) return Integer;

   ---------------------------------------------------------------------------
   --  Wide_String handling
   ---------------------------------------------------------------------------

   function Concat (S1, S2 : Wide_String) return Wide_String;
   function Eq     (S1, S2 : Wide_String) return Boolean;
   function Lt     (S1, S2 : Wide_String) return Boolean;
   function Gt     (S1, S2 : Wide_String) return Boolean;

   ---------------------------------------------------------------------------
   --  Integer handling
   ---------------------------------------------------------------------------

   function Add (I1, I2 : Integer) return Integer;
   function Sub (I1, I2 : Integer) return Integer;
   function Eq  (I1, I2 : Integer) return Boolean;
   function Lt  (I1, I2 : Integer) return Boolean;
   function Gt  (I1, I2 : Integer) return Boolean;

end ASIStant.Add_Ons;
