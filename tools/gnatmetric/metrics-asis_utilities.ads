------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--               M E T R I C S . A S I S _ U T I L I T I E S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2002-2008, AdaCore                     --
--                                                                          --
-- GNAT Metrics Toolset  is free software;  you can  redistribute it and/or --
-- modify it under terms of the  GNU General Public License as published by --
-- the Free Software Foundation;  either version 2, or (at your option) any --
-- later version.  GNAT Metrics Toolset is  distributed in the hope that it --
-- will be useful, but  WITHOUT ANY WARRANTY; without even the implied war- --
-- ranty of  MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the --
-- GNU General Public License for more details.  You should have received a --
-- copy of the  GNU General Public License distributed with  GNAT; see file --
-- COPYING.  If not,  write to the  Free Software  Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- GNAT Metrics Toolset is maintained by AdaCore (http://www.adacore.com).  --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines different ASIS secondary queries needed in metrics
--  tools

with Asis;                       use Asis;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;

package METRICS.ASIS_Utilities is

   function May_Contain_Program_Units
     (El_Kind : Flat_Element_Kinds)
      return    Boolean;
   --  Checks if the element of the given kind may contain program units as its
   --  components.

   function Is_Process (El_Kind : Flat_Element_Kinds) return Boolean;
   --  Checks if the element of the given kind should be counted for '-lav'
   --  metric.

   function CU_Profile (CU : Compilation_Unit) return String;
   --  Return the general "profile" of the argument unit which integrates the
   --  general unit attributes (such as unit kind, class etc.) and which
   --  ends with the unit name (e.g. "private generic package body Foo"

end METRICS.ASIS_Utilities;
