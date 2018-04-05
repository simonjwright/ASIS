------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                A S I S _ U L . S O U R C E _ T A B L E .                 --
--                 G N A T C H E C K _ P R O C E S S I N G                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2004-2016, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the processing routines for the source file
--  table that are specific to the GNATCHECK tool.

package ASIS_UL.Source_Table.Gnatcheck_Processing is

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
   --  At the moment, this procedure calls initialization routine for the
   --  Diagnosis mapping table.

   procedure Process_Sources;
   --  Iterates though the source file table and calls the processing routine
   --  for each source. This procedure implement only iteration though the
   --  table, the specific processing routine(s) should be provided by the
   --  tool developer.

   procedure Delete_ALI_Files;
   --  Deletes all the .ali files listed in the deletion file.
   --  See also comments on Deletion_File_Name in Gnatcheck.Options.

   procedure Finalize;
   --  This procedure is supposed to be called after completing of the
   --  processing of all the sources stored in the source table that have
   --  to be and can be processed. It is supposed to summarize and to analyze
   --  some global information (if any) that has been generated during source
   --  processing.
   --
   --  This routine also generates various kinds of report.

   type Exit_Code_Type is (
      E_Success,     --  No tool failure, no rule violation detected       (0)
      E_Violation,   --  No fatal tool failure, rule violation(s) detected (1)
      E_Fatal,       --  Fatal tool failure                                (2)
      E_Non_Trusted, --  Non-fatal tool failures detected, and no rule     (2)
                     --  violation detected. In this case we cannot trust that
                     --  the argument code indeed contains no rule violation.
      No_Check);     --  No file has been checked                          (3)

   Exit_Code : Exit_Code_Type := No_Check;
   --  Gnatcheck exit code. If gnatcheck has done some real work, this
   --  default should be changed.

   procedure Define_Exit_Code;
   --  Defines the value of Exit_Code variable. This procedure should never be
   --  called if a fatal failure is detected.

   procedure Exit_Gnatcheck (Exit_Code : Exit_Code_Type);
   pragma No_Return (Exit_Gnatcheck);
   --  A call to Exit_Gnatcheck terminates execution with the given status. A
   --  status of zero indicates normal completion, a non-zero status indicates
   --  abnormal termination, the particular codes are given in brackets in the
   --  documentation of Exit_Code_Type values.

end ASIS_UL.Source_Table.Gnatcheck_Processing;
