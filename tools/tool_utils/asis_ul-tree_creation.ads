------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                A S I S _ U L . T R E E _ C R E A T I O N                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not,  write  to the  Free Software Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines for asynchronous tree creation.

with ASIS_UL.Source_Table; use ASIS_UL.Source_Table;

package ASIS_UL.Tree_Creation is

   function Use_Parallel_Tree_Creation return Boolean;
   --  ???

   procedure Check_Tree_Creations;
   --  Checks for the compilation of the asynchronous tree creation processes.
   --  For a completed process, sets the status of the corresponding source
   --  according to the compilation results. Stops when the first source for
   --  that the tree has been successfully created is detected (that is, this
   --  source is a legal Ada source).

   procedure Set_Max_Processes;
   --  Sets the upper bound of the counter of the parallel tree creations. We
   --  have always the main process running, so the maximal number of tree
   --  creations we can start in parallel is one less. And we count processes
   --  starting from zero - that is why we subtracts another 1 from
   --  process_Num. This upper bound should be set after processing the tool
   --  parameters (-jN option). This procedure also creates the data structure
   --  for storing the information
   --  function Use_Parallel_Tree_Creation;
   --  ???

   procedure Start_Tree_Creations
     (SF                   : SF_Id;
      Only_Bodies          : Boolean;
      Need_Compiler_Output : Boolean := False;
      All_Warnings_Off     : Boolean := True);
   --  Starts asynchronous processes for compiling sources for the trees. The
   --  number of process is up to the parameter specified by '-j' option
   --  (counting the main tool process, so the number of asynchronous gcc calls
   --  for the trees is at most one less then the value of the parameter of
   --  -j). The parameters have the following meaning:
   --  SF                   - the source that is the last one for which the
   --                         tree creation process have been started; this
   --                         procedure does not call gcc to create the tree
   --                         for SF;
   --  Only_Bodies          - flag used to optimize the search for the next
   --                         source to compile (??? needs revision and better
   --                         documentation!!!)
   --  Need_Compiler_Output - if this flag is ON, the call to gcc redirects the
   --                         compiler output into the file located in the same
   --                         directory as the tree file. The name of this file
   --                         is returned by
   --                         Gnatcheck.Compiler.Get_Compiler_Out_File_Name
   --                         function;
   --  All_Warnings_Off     - if this flag is ON, the call to gcc contains an
   --                         option that suppresses all compiler warnings.

end ASIS_UL.Tree_Creation;
