------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--          G N A T T E S T  . S K E L E T O N . G E N E R A T O R          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2011-2016, AdaCore                     --
--                                                                          --
-- GNATTEST  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either  version  2, or (at your option) any later --
-- version.  GNATTEST  is  distributed  in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License distributed with GNAT; see file COPYING. If --
-- not, write to the  Free  Software  Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.,                                      --
--                                                                          --
-- GNATTEST is maintained by AdaCore (http://www.adacore.com).              --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines different routines for generating skeleton test files.

with Asis;

package GNATtest.Skeleton.Generator is

   procedure Process_Sources;
   --  Generates tests and gathers information needed to generate harness
   --  Iterates trough elements of the source table trying to minimize the
   --  number times the tree file is created.

   function Mangle_Hash
     (Subp       : Asis.Declaration) return String;
   --  Returns the name of a given procedure or function with a hash code made
   --  of full ada names of all its parameters and result profile in case of
   --  a function.

end GNATtest.Skeleton.Generator;
