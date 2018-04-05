------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                   M E T R I C S . C O N T R A C T S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2015-2016, AdaCore                      --
--                                                                          --
-- GNATMETRIC  is  free software; you can  redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either version 3, or (at your option)  any  later --
-- version. GNATMETRIC  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATMETRIC is maintained by AdaCore (http://www.adacore.com).            --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines routines for computing contract-specific metrics.

with Asis;                       use Asis;
with METRICS.Metric_Definitions; use METRICS.Metric_Definitions;

package METRICS.Contracts is

   procedure Count_Contract_Info
     (Subprogram :        Asis.Element;
      Info       : in out Contract_Details);
   --  This procedure assumes that Subprogram is either a procedure or a
   --  function declaration. It parces the Elements that define a contract for
   --  this subprogram (expression functions???!!!) and updates
   --  Public_Subprogram_Contract_Count accordingly.

end METRICS.Contracts;
