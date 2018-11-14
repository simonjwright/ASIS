------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS                            --
--                                                                          --
--                     G N A T E L I M . O P T I O N S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2009-2017, AdaCore                      --
--                                                                          --
-- GNATELIM  is  free software;  you can  redistribute it and/or  modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 3 or (at your option) any later --
-- version. GNATELIM is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING3. If not, --
-- go to http://www.gnu.org/licenses for a complete copy of the license.    --
--                                                                          --
-- The original version  of  Gnatelim  was developed by  Alain  Le  Guennec --
-- It is now maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains definitions of gnatelim-specific options

with ASIS_UL.Options;

with Gnatelim.Projects;

package Gnatelim.Options is

   Eliminate_Dispatching_Operations : Boolean renames
     ASIS_UL.Options.Represent_Dispatching_Calls;
   --  --elim-dispatching
   --  If this flag is ON, gnatelim tries to eliminate dispatching operations.

   Compute_Closure : Boolean := False;
   --  If this flag is ON, gnatelim itself computes the set of units that
   --   make up a closure of the main unit. This flag is computed by gnatelim
   --  itself by analyzing its parameters.

   Long_Pragma_Format : Boolean := True;
   --  If this flag is ON, gnatelim adds the UNIT_NAME parameter to the
   --  generated pragmas. The initialization should be changed to False when
   --  K108-003 is complete).

   Gnatelim_Prj : Gnatelim.Projects.Gnatelim_Project_Type;

end Gnatelim.Options;
