------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--              A S I S _ U L . M E T R I C S . C O M P U T E               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not,  write  to the  Free Software Foundation,  51 Franklin  --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines for computing metrics.

with Asis;                        use Asis;

with ASIS_UL.Metrics.Definitions; use ASIS_UL.Metrics.Definitions;

package ASIS_UL.Metrics.Compute is

   procedure Compute_Complexity_Metrics
     (Body_Element :     Asis.Element;
      Counter      : out Complexity_Metric_Counter);
   --  If Body_Element represents an executable body, computes the complexity
   --  metrics for the element (see the definition of Complexity_Metric_Counter
   --  type in ASIS_UL.Metrics.Definitions for more details.) Otherwise the
   --  value of Counter is indefinite.

   procedure Compute_Syntaxt_Metrics
     (Unit_Element :     Asis.Element;
      Counter      : out Syntax_Metric_Counter);
   --  If Unit_Element is a program unit for that element metrics should be
   --  computed, computes and stores in Counter all the syntax metrics.
   --  Otherwise the value of Counter is indefinite

end ASIS_UL.Metrics.Compute;
