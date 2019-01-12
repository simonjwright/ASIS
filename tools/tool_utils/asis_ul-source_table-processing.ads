------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--      A S I S _ U L . S O U R C E _ T A B L E . P R O C E S S I N G       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2004-2018, AdaCore                     --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the basic processing routines for the source file
--  table. We define them in a child package in case if some tools need only
--  the source table but not any stubs or templates for the processing of
--  the files from this table.

with ASIS_UL.Options; use ASIS_UL.Options;

package ASIS_UL.Source_Table.Processing is

   procedure Initialize;
   --  This procedure is supposed to be called after the general initialization
   --  routine (ASIS_UL.Environment.Initialize), when all the tool command-line
   --  parameters are processed and all the argument files are stored in the
   --  file table.
   --
   --  This procedure is supposed to make some additional settings in the
   --  source file table based on some information other than the tool command
   --  line parameters. For example, it may read some configuration file(s) and
   --  as a result mark some for the argument files as not to be processed.
   --
   --  This routine is supposed to be provided by the tool developer (if any
   --  additional initialization is needed for the source table). At the
   --  moment, the ASIS Utility Library contains a placeholder for it, at some
   --  point it may be replaced with the code doing some processing of XML
   --  configuration file.

   procedure Process_Sources
     (Need_Semantic_Info : Boolean := True;
      Add_Needed_Sources : Boolean := Mimic_gcc;
      Keep_ALI           : Boolean := False);
   --  Iterates through the source file table and calls the processing routine
   --  for each source. This procedure implements only iteration through the
   --  table, the specific processing routine(s) should be provided by the tool
   --  developer. Need_Semantic_Info controls the optimization when processing
   --  units on the base of the same tree. If your tool does need the detailed
   --  semantic information, set this parameter ON, otherwise the information
   --  from expanded generics and program unit bodies may be lost.  If your
   --  tool is purely syntactical, you may set this parameter OFF to speed up
   --  the processing.
   --  If Add_Needed_Sources is set ON, this procedure, when processing a body
   --  adds to the source table a spec for this unit and its subunit, if they
   --  are not in the table, and processes these added units. Otherwise only
   --  those units that are in the source table are processed. Setting
   --  Add_Needed_Sources ON may be useful when processing closures computed on
   --  the fly. Note that in Mimic_gcc, Add_Needed_Sources is ON.

   procedure Process_Source
     (SF                 : SF_Id;
      Only_Bodies        : Boolean;
      Need_Semantic_Info : Boolean;
      Add_Needed_Sources : Boolean := Mimic_gcc;
      Keep_ALI_Files     : Boolean := Mimic_gcc);
   --  Processes the source file stored under SF index in the source file
   --  table.  The caller is responsible for keeping the actual parameter
   --  inside the range of the existing table entries. The processing consists
   --  of creating the tree file for this source, and if the tree is
   --  successfully created, then the ASIS Compilation Unit corresponding to
   --  this source is processed. Then this routine tries to locate in the set
   --  of ASIS Compilation Units represented by this tree units corresponding
   --  to some other sources stored in the source table, and to process all
   --  these units. When the processing is complete, the tree file and the
   --  corresponding ALI file are deleted from the temporary directory except
   --  if Keep_ALI_Files is set ON, in this case only a tree file is deleted
   --  (this may be useful when using 'gnatmake -n' to process the closure of
   --  some unit).
   --
   --  Only_Bodies parameter is used to optimize tree creation process. If the
   --  calling context wants to process all the bodies first, it should set
   --  this flag ON in the corresponding loop through the sources stored in the
   --  Source Table.
   --
   --  Processing of other units that are represented in the tree created as a
   --  part of processing the call to this subprogram is controlled by
   --  Need_Semantic_Info parameter. If it is set ON, we do not process withed
   --  spec units because it may result in losing information about expanded
   --  package bodies.
   --
   --  Add_Needed_Sources is used to specify if the needed sources (spec for
   --  a body and subunits for stubs) should be processed even if they are not
   --  in the source table, see the documentation for the Process_Sources
   --  routine.
   --
   --  Note that in Mimic_gcc mode, Add_Needed_Sources and Keep_ALI_Files are
   --  both ON.

   procedure Finalize;
   --  This procedure is supposed to be called after completing the processing
   --  of all the sources stored in the source table that have to be and can be
   --  processed. It is supposed to summarize and to analyze some global
   --  information (if any) that has been generated during source processing.
   --
   --  This routine is supposed to be provided by the tool developer. At the
   --  moment, the ASIS Utility Library contains a placeholder for it.

   function All_Files_Successfully_Processed return Boolean;
   --  Checks if all the argument files have the status Processed, and returns
   --  False if it is not the case.

end ASIS_UL.Source_Table.Processing;
