------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS                            --
--                                                                          --
--                     G N A T E L I M .C L O S U R E                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

--  This package contains routines that compute (and process, if needed)
--  closures of the main unit.

package Gnatelim.Closure is

   procedure Try_Get_Sources_From_Build (Success : in out Boolean);
   --  Tries to get the set of sources making up the main unit closure using
   --  the results of the main unit build (if any). Uses routines from
   --  GNATCOLL.Projects if gnatelim is called with a project file or calls to
   --  the binder with '-R' option otherwise. Success is set True if this
   --  attempt is successful and False otherwise.

   procedure Process_Closure;
   --  Computes the closure of the main unit step-by-step. If no argument
   --  project file is specified, issues an error message and raises
   --  Fatal_Error.

end Gnatelim.Closure;
