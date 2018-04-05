------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--              G N A T T E S T  . S T U B . G E N E R A T O R              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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

--  This package defines different routines for generating stub files.

with Asis;

package GNATtest.Stub.Generator is

   procedure Process_Unit
     (CU                  : Asis.Compilation_Unit;
      Body_File_Name      : String;
      Stub_Data_File_Spec : String;
      Stub_Data_File_Body : String);
   --  Processes corresponding spec and body,
   --  (re)creates stub body and stub data package.

end GNATtest.Stub.Generator;
