------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS                            --
--                                                                          --
--     A S I S _ U L . G L O B A L _ S T A T E . C G . G N A T E L I M      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2009, AdaCore                     --
--                                                                          --
-- GNATELIM  is  free software;  you can  redistribute it and/or  modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 2 or (at your option) any later --
-- version. GNATELIM is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING.  If not, --
-- write to the  Free Software Foundation, 51 Franklin Street, Fifth Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- The original version  of  Gnatelim  was developed by  Alain  Le  Guennec --
-- It is now maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines gnatelim-specific call graph routines

package ASIS_UL.Global_State.CG.Gnatelim is

   procedure Mark_Used_Subprograms;
   --  Ananlyzes the call graph (after performing the transitive closure on it)
   --  and marks used subprograms. This procedure takes care about such special
   --  cases as marking as used dispatching  subprograms in case if gnatelim
   --  options say that dispatching subprograms should not be eliminated,
   --  marking as used any overriding  dispatching subprogram as used if its
   --  parent is used (the front-end requirement), marking as used any type
   --  initialization procedure (front-end temporary limitation, should be
   --  removed at some point.

   function Is_Used (N : GS_Node_Id) return Boolean;
   --  Checks if the argument node is marked as used. (It does not check that
   --  the argument node is a subprogram node)

end ASIS_UL.Global_State.CG.Gnatelim;
