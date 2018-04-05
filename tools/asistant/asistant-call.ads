------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                        A S I S T A N T . C A L L                         --
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
with Asis.Text;
with Asis.Compilation_Units.Relations;
with Asis.Data_Decomposition;

with ASIStant.FuncEnum; use ASIStant.FuncEnum;
with ASIStant.S_Parser; use ASIStant.S_Parser;

package ASIStant.Call is

------------------------------------------------------------------------------
--  Package for calling of ASIS queries; also provides a set of ASIStant
--  commands
------------------------------------------------------------------------------

   package DDA renames Asis.Data_Decomposition;

   type Function_Type is ( --  set of ASIStant commands
      FT_CALL,              --  (implicit) Call ASIS queries
      FT_EXECUTE,           --  Start/resume script
      FT_EXIT,              --  exit script
      FT_HELP,              --  General and query-specific help
      FT_IF,                --  Conditional operation
      FT_INFO,              --  ASIS and ASIStant tech info
      FT_IRUN,              --  Start/resume script interactively
      FT_LOG,               --  Start/end log
      FT_LOGLEVEL,          --  Set output level
      FT_PAUSE,             --  Pause script
      FT_PRINT,             --  Print variable value
      FT_PRINTDETAIL,       --  Set PRINT detail
      FT_QUIT,              --  Quit
      FT_RUN,               --  Start/resume script
      FT_SET                --  Set variable
   );

   subtype Parameter_Range is Profile_Range range 1 .. 4;

   type Parameter_Set is array (Parameter_Range) of Query_Result;

   Empty_Parameter_Set : constant Parameter_Set :=
     (others => Query_Result'(RType => Par_Absent));

   function  Identify_Function (N : Node_Position) return Function_Type;
   --  Identifies ASIStant request

   function Call_ASIStant_Function (N : Wide_String; PS : Parameter_Set)
      return Query_Result;
   --  Calls ASIStant function Name with a parameter set PS

   function Save_ElemList  (E : Asis.Element_List)
      return ElemList_Ptr;
   function Save_CUnitList (C : Asis.Compilation_Unit_List)
      return CUnitList_Ptr;
   function Save_LineList  (L : Asis.Text.Line_List)
      return LineList_Ptr;
   function Save_String    (S : Wide_String)
      return String_Ptr;
   function Save_Relship (R : Asis.Compilation_Units.Relations.Relationship)
      return Relship_Ptr;
   function Save_DDA_ArrCList  (A : DDA.Array_Component_List)
      return DDA_ArrCList_Ptr;
   function Save_DDA_RecCList  (R : DDA.Record_Component_List)
      return DDA_RecCList_Ptr;

end ASIStant.Call;
