------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                     G N A T C H E C K . O U T P U T                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2004-2014, AdaCore                      --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston,                                                                  --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the output routines for gnatcheck

package Gnatcheck.Output is

   procedure Brief_Help;
   --  Prints the brief gnatcheck help info into Stderr

   procedure Print_Gnatcheck_Usage;
   --  Similar to Brief_Help, but corresponds to the general format generated
   --  by other GNAT tools for '--help' option, and sends the output into
   --  Stdout

   procedure Set_Coding_Standard_File_Name (Fname : String);
   --  Stores the name of the file to write the sample coding standard in.

   procedure Write_Coding_Standard;
   --  Writes down the coding standard file and closes the corresponding file.

end Gnatcheck.Output;
