------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                      A S I S T A N T . C O M M O N                       --
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
with Asis.Data_Decomposition;

with ASIStant.FuncEnum; use ASIStant.FuncEnum;

package ASIStant.Common is

------------------------------------------------------------------------------
--  This package contains definitions common to ASIStant packages. By changing
--  some of the constants here, ASIStant flexibility can be adjusted.
------------------------------------------------------------------------------

   MAX_ID_LENGTH     : constant Positive := 32;
   --  The length of ASIStant identifier (may not be less than the longest
   --  name of ASIS query).

   MAX_VARIABLES     : constant Positive := 100;
   --  Size of initial ASIStant variable table; see ASIStant.Table

   MAX_TREENODES     : constant Positive := 500;
   --  Size of ASIStant statement trees; see ASIStant.S_Parser

   MAX_SCRIPTDEPTH   : constant Positive := 5;
   --  Depth of script enclosing; see ASIStant.Text_IO

   MAX_ATIELEMENTS      : Natural := 200;
   MAX_ATIELEMLISTS     : Natural := 100;
   MAX_ATICUNITS        : Natural := 100;
   MAX_ATICUNITLISTS    : Natural := 10;
   MAX_ATICONTEXTS      : Natural := 10;
   MAX_ATISPANS         : Natural := 100;
   MAX_ATILINES         : Natural := 100;
   MAX_ATILINELISTS     : Natural := 30;
   MAX_ATIRELATIONSHIPS : Natural := 10;
   MAX_DDA_ARRCOMPS     : Natural := 100;
   MAX_DDA_ARRCOMPLISTS : Natural := 10;
   MAX_DDA_RECCOMPS     : Natural := 100;
   MAX_DDA_RECCOMPLISTS : Natural := 10;
   --  These constants define the size of ASIS support arrays declared
   --  in ASIStant.XTable

   ASIStant_ERROR : exception;
   --  This exception is raised any time ASIStant encounters a non-ASIS error;
   --  diagnosis of the type Error_Type is stored in LastErr

   ASIStant_ASIS_ERROR : exception;
   --  This exception is raised any time ASIStant encounters an ASIS error

   subtype Name_String is Wide_String (1 .. MAX_ID_LENGTH);
   --  Type to represent ASIStant IDs

   package DDA renames Asis.Data_Decomposition;

   type ElemArray         is array (Natural range <>) of Asis.Element;
   type ElemListArray     is array (Natural range <>) of ElemList_Ptr;
   type CUnitArray        is array (Natural range <>) of Asis.Compilation_Unit;
   type CUnitListArray    is array (Natural range <>) of CUnitList_Ptr;
   type RelshipArray      is array (Natural range <>) of Relship_Ptr;
   type LineArray         is array (Natural range <>) of Asis.Text.Line;
   type LineListArray     is array (Natural range <>) of LineList_Ptr;
   type DDA_ArrCArray     is array (Natural range <>) of DDA.Array_Component;
   type DDA_ArrCListArray is array (Natural range <>) of DDA_ArrCList_Ptr;
   type DDA_RecCArray     is array (Natural range <>) of DDA.Record_Component;
   type DDA_RecCListArray is array (Natural range <>) of DDA_RecCList_Ptr;

   type ElemArray_Ptr         is access all ElemArray;
   type ElemListArray_Ptr     is access all ElemListArray;
   type CUnitArray_Ptr        is access all CUnitArray;
   type CUnitListArray_Ptr    is access all CUnitListArray;
   type RelshipArray_Ptr      is access all RelshipArray;
   type LineArray_Ptr         is access all LineArray;
   type LineListArray_Ptr     is access all LineListArray;
   type DDA_ArrCArray_Ptr     is access all DDA_ArrCArray;
   type DDA_ArrCListArray_Ptr is access all DDA_ArrCListArray;
   type DDA_RecCArray_Ptr     is access all DDA_RecCArray;
   type DDA_RecCListArray_Ptr is access all DDA_RecCListArray;

   type Error_Type is
     (
      ERR_INTERNAL,
      ERR_NOTIMPLEMENTED,
      ERR_NOTSUPPORTED,

      ERR_NOSCRIPT,
      ERR_NEEDFILENAME,

      ERR_BADBOOLEAN,
      ERR_BADEXPR,
      ERR_BADID,
      ERR_BADINTEGER,
      ERR_BADLISTELEM,
      ERR_BADPARAM,
      ERR_BADPARAMLIST,
      ERR_BADSTRING,
      ERR_BADVARTYPE,
      ERR_BADVARNAME,
      ERR_NEEDCHAR,
      ERR_NEEDFUNCTION,
      ERR_NEEDPARAM,
      ERR_NOTINRANGE,
      ERR_TOOMANYPARAMS,
      ERR_UNKNOWNVAR,

      ERR_TABLEFULL,

      ERR_UNKNOWNSYNTAX,
      ERR_UNKNOWNQUERY
      );
   --  All possible errors

   LastErr : Error_Type;

   DebugPrint : Boolean := True;

   procedure Print_ASIStant_Header;
   --  Prints the ASIStant copyright header

   procedure Error (ErrNo  : Error_Type;
                    ErrStr : Wide_String := "";
                    Level  : Natural := 5);
   --  Debug output function
   pragma No_Return (Error);

end ASIStant.Common;
