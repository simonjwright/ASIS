------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                            A 4 G . A _ O P T                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2019, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your option)  any later --
-- version.  ASIS-for-GNAT  is  distributed  in  the  hope  that it will be --
-- useful,  but  WITHOUT ANY WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have  received  a copy of the  GNU General Public License and --
-- a copy of the  GCC Runtime Library Exception  distributed with GNAT; see --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains global switches set by the
--  Asis_Environment.Initialize routine from the Parameters siting and
--  referenced throughout the ASIS-for-GNAT
--
--  This package may be considered as an ASIS analog of the GNAT Opt
--  package

package A4G.A_Opt is

   Is_Initialized  : Boolean := False;
   --  flag indicating if the environment has already been initialized.

   Was_Initialized_At_Least_Once : Boolean := False;
   --  flag indicating if the environment was initialized at least
   --  once during the current launch of an ASIS application

   type ASIS_Warning_Mode_Type is (Suppress, Normal, Treat_As_Error);
   ASIS_Warning_Mode : ASIS_Warning_Mode_Type := Normal;
   --  Controls treatment of warning messages. If set to Suppress, warning
   --  messages are not generated at all. In Normal mode, they are generated
   --  but do not count as errors. In Treat_As_Error mode, a warning is
   --  treated as an error: ASIS_Failed is raised and the warning message is
   --  sent to an ASIS Diagnosis string.

   Strong_Version_Check : Boolean := True;
   --  Strong version check means that version strings read from the tree and
   --  stored in Gnatvsn are compared. Weak check means comparing ASIS version
   --  numbers. See BA23-002

   Generate_Bug_Box : Boolean := True;
   --  Flag indicating if the ASIS bug box should be generated into Stderr
   --  when an ASIS implementation bug is detected.

   Keep_Going : Boolean := False;
   --  Flag indicating if the exit to OS should NOT be generated in case if
   --  ASIS internal implementation error. Set ON by Initialize '-k' parameter.

   ASIS_2005_Mode_Internal : constant Boolean := True;
   --  If this switch is ON, ASIS detects as predefined units also units listed
   --  as predefined in the 2005 revision of the Ada Standard. Now this flag is
   --  always ON, and we do not have any parameter to tell ASIS that only
   --  Ada 95 predefined units should be classified as predefined Compilation
   --  Units in ASIS.

   Cache_EE_Results : Boolean := False;
   --  If this flag is ON, the cache storage is used to optimize computation
   --  of Enclosing_Element for expressions (normally it requires traversing
   --  the subtree rooted by rough estimation of enclosing Element);
   --
   --  Theiretically this flag should be set individually for each Context,
   --  but the only practical ASIS operational mode is one Context at a time.
   --  This flag is set ON by '--cache_ee' and is set OFF by a call to
   --  Asis.Ada_Environments.Dissociate (for any Context argument).

   procedure Process_Initialization_Parameters (Parameters : String);
   --  Processes a Parameters string passed to the
   --  Asis.Implementation.Initialize query: check parameters and makes the
   --  corresponding settings for ASIS global switches and flags.

   procedure Process_Finalization_Parameters (Parameters : String);
   --  Processes a Parameters string passed to the
   --  Asis.Implementation.Finalize query.

   procedure Set_Off;
   --  Sets Is_Initialized flag OFF and then sets all the global switches
   --  except Was_Initialized_At_Least_Once in the initial (default) position.
   --  Is to be called by Asis_Environment.Finalize

   --  the type declarations below should probably be moved into A_Types???

   type  Context_Mode is
   --  different ways to define an ASIS Context:
     (One_Tree,
      --  a Context is made up by only one tree file
      N_Trees,
      --  a Context is made up by N tree files
      Partition,
      --  a partition Context
      All_Trees);
      --  all the tree files in tree search path are considered as making up a
      --  given Context

   type Tree_Mode is
   --  how ASIS deals with tree files
     (On_The_Fly,
      --  trees are created on the fly, created trees are reused as long as a
      --  Context remains opened
      Pre_Created,
      --  only those trees which have been created before a Context is opened
      --  are used
      Mixed,
      --  mixed approach - if ASIS cannot find a needed tree, it tries to
      --  create it on the fly
      Incremental,
      --  Similar to Mixed, but these mode goes beyond the ASIS standard and
      --  allows to change the environment when the Context remains open:
      --  -  when the Context is opened, all the existing trees are processed;
      --  -  if ASIS can not find a needed tree, it tries to create it on the
      --     fly, and it refreshes the information in the Context unit table
      --     using the data from this newly created tree;
      --  -  any access to a unit or to an element checks that a tree to be
      --     accessed is consistent with the sources
      --  ???? This documentation definitely needs revising???
      GNSA
      --  Any tree is created on the fly by calling GNSA. It is not written
      --  in a tree file and then read back by ASIS, but it is left in the
      --  same data structures where it has been created, and after that ASIS
      --  works on the same data structures.
      );

   type Source_Mode is
   --  how ASIS takes into account source files when checking the consistency
     (All_Sources,
      --  sources of all the units from a given Context (except the predefined
      --  Standard package) should be around, and they should be the same as
      --  the sources from which tree files making up the Context were created
      Existing_Sources,
      --  If for a given unit from the Context the corresponding source file
      --  exists, it should be the same as those used to create tree files
      --  making up the Context
      No_Sources);
      --  Existing  source files are not taken into account when checking the
      --  consistency of tree files

   Check_Temporary_Files : Boolean := False;
   --  If this flag is OFF, all the consistency checks (when ASIS reads in tree
   --  files) ignore GNAT temporary files. This is needed in case when tree
   --  files are created by gprbuild calls. gprbuild on the fly creates Ada
   --  configuration files (as GNAT temporary files), passes them as arguments
   --  of compiler calls that create trees and then deletes these files. So we
   --  may have more than one temporary configuration file used for creation
   --  different tree files with the same name and with different time stamps.
   --
   --  At the moment there is no option to change setting of this flag but we
   --  may want to add it later.

   type DDA_Modes is (Normal, gnatR);
   --  Normal - all the representation inforation is exytacted from the tree
   --           file being processed
   --
   --  gnatR  - all the representation inforation is exytacted from the json
   --           file that contains the output generated by -gnatR option

end A4G.A_Opt;
