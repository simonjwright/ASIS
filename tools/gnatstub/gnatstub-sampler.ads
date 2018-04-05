------------------------------------------------------------------------------
--                                                                          --
--                           GNATSTUB COMPONENTS                            --
--                                                                          --
--                      G N A T S T U B . S A M P L E R                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1997-2016, Free Software Foundation, Inc.        --
--                                                                          --
-- Gnatstub  is  free  software;  you can  redistribute it and/or modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 3 or (at your option) any later --
-- version. Gnatstub is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING3. If not, --
-- go to http://www.gnu.org/licenses for a complete copy of the license.    --
--                                                                          --
-- Gnatstub  was  originally  developed  by  Alexei Kuchumov  as  a part of --
-- collaboration  between  Software  Engineering  Laboratory of  the  Swiss --
-- Federal  Institute  of  Technology  in  Lausanne,  Switzerland, and  the --
-- Scientific  Research  Computer  Center  of the  Moscow State University, --
-- Russia.  This  work  was  supported  by  a grant from the Swiss National --
-- Science Foundation,  no 7SUPJ048247,  funding a project  "Development of --
-- ASIS for GNAT with industry quality".                                    --
--                                                                          --
-- Gnatstub is now maintained by AdaCore (http://www.adacore.com).          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

--  This package defines routines providing the main functionality of
--  Gnatstub.

with Asis; use Asis;

package Gnatstub.Sampler is

   procedure Brief_Help;
   --  Prints brief help information to stdout.

   procedure Print_Gnatstub_Usage;
   --  Similar to Brief_Help, but corresponds to the general format generated
   --  by other GNAT tools for '--help' option, and sends the output into
   --  Stdout.

   procedure Initialize;
   --  Reads and checks the command line parameters and initializes the
   --  Gnatstub options. Checks the existence of the files to be processed
   --  by Gnatstub and applicability of the gnatstub options with these files.
   --  Tries to create the tree file, if necessary.
   --  If everything is OK, sets the global Is_Initialized variable True.
   --  This procedure does not use anything from ASIS

   procedure Create_Sample;
   --  If Is_Initialized, generates the sample body. This procedure is an
   --  ASIS application

   procedure Clean_Up;
   --  Beacuse of historical reasons, the clean-up procedure for gnatstub
   --  differs from the standard clean-up procedure for othet AdaCore ASIS
   --  tools that are based on ASIS UL. gnatstub creates the tree file in the
   --  current directory and it may reuse the existing tree file and it may
   --  keep the tree file it has created. So this procedure first removes the
   --  tree file in the current directory (if the option that keeps the tree
   --  is not specified), and then it calls the standard clean-up procedure to
   --  clean the temporarty directory (if it has been created as a part of
   --  project file processing).

end Gnatstub.Sampler;
