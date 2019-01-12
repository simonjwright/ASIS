------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                 G N A T T E S T . E N V I R O N M E N T                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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

--  This package defines data structures and routines for setting up and
--  controlling the GNATtest, such as argument tables, routines for
--  initialization, cleaning up etc.

package GNATtest.Environment is

   procedure Initialize;
   --  This procedure scans the tool command-line parameters and set all the
   --  tool control parameters. It also fills in the source file table
   --  Raises Fatal_Error if the results of initialization make meaningless
   --  any further processing. It also creates a temporary directory and
   --  moves into it.

   procedure Clean_Up;
   --  Performs the final clean-up actions, including closing and deleting of
   --  all files that should be closed or deleted.

   procedure Context_Clean_Up;
   --  Closes and dissociates the context, if needed

   Source_Compilation_Failed : Boolean := False;
   --  Indicates when at least one of sources could not be compiled.
   --  This should affect exit code.

end GNATtest.Environment;
