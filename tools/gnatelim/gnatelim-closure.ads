------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS                            --
--                                                                          --
--                     G N A T E L I M .C L O S U R E                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

--  This package contains routines that compute (and process, if needed)
--  closures of the main unit.

package Gnatelim.Closure is

   procedure Try_Get_Sources_From_Binder (Success : in out Boolean);
   --  Tryes to get the set of sources making up the main unit closure using
   --  the call to the binder with '-R' option. This can be possible only if
   --  the main unit has already been built. Success is set True if this
   --  attempt is successful and False otherwise.

   procedure Process_Closure;
   --  Computes the closure of the main unit step-by-step (using gnatmake '-n'
   --  option) and analyses it also in step-by-step manner. Even though this
   --  approach does not need a source table, this procedure stores sources in
   --  the table for debugging purposes.

end Gnatelim.Closure;
