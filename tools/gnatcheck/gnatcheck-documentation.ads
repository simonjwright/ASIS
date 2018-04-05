------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              G N A T C H E C K . D O C U M E N T A T I O N               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

--  This package defines routines for generating the gnatcheck documentation

with Gnatcheck.Options;

package Gnatcheck.Documentation is

   procedure Set_Doc_File;
   --  Creates and opens a file to put the documentation into (or only opens
   --  a file if the needed file already exists, in this case the existing
   --  file is overriden

   procedure Compose_Documentation;
   --  Compose the document describing gnatcheck rules. Assumes that the file
   --  to place the documentation into is already (created if needed) and
   --  opened.

   procedure Clean_Up;
   --  Closes the file with gnatcheck rule documentation (if needed) and
   --  returns to the directory from which the program has been called

   --------------------------------------
   -- Documentation generation options --
   --------------------------------------

   Add_Requirements : Boolean := False;
   --  '-req'
   --  If this flag is ON, the qualification requirements are added to the
   --  generated documentation

   Add_Questions : Boolean := False;
   --  '-qst'
   --  If this flag is ON, the questions concerning rule definition and/or
   --  documentation are added to the generated documentation

   Rule_Doc_Status : Gnatcheck.Options.Rule_Statuses :=
      Gnatcheck.Options.Fully_Implemented;
   --  '-rs=N, N in 1 .. 2'
   --  The generated documentation contains information about the rules that
   --  have status not less than Rule_Doc_Status

   procedure Process_Rule_Status_Par
     (Par     : String;
      Success : out Boolean);
   --  Analyzes parameter of '-rs' option and sets Rule_Doc_Status. Sets
   --  Success True if Par is a valid paraqmeter and False otherwise.
   --  At the moment '-rs' option has only two valuid parameters -
   --  one-character strings "1" (that meants Non_Documented) and "2" (that
   --  means Under_Construction)

   procedure Set_Max_Details;
   --  '-all'
   --  Sets maximal detalisation for the generated documentation

end Gnatcheck.Documentation;
