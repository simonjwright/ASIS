------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                        G N A T C H E C K . I D S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2008-2010, AdaCore                     --
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

--  This package defines ID types for rule table and category table. We define
--  ID types in a separate package because both rule type and category type
--  needs both kinds of IDs

package Gnatcheck.Ids is

   -----------------
   -- Category ID --
   -----------------

   type Category_Id is new Natural;
   No_Category      : constant Category_Id := Category_Id'First;
   Root_Category_Id : constant Category_Id := No_Category + 1;

   -------------
   -- Rule ID --
   -------------

   type Rule_Id is new Natural;
   No_Rule    : constant Rule_Id := Rule_Id'First;

   --  Fake Ids for compiler checks:
   Restrictions_Id : constant Rule_Id := No_Rule + 1;
   Style_Checks_Id : constant Rule_Id := Restrictions_Id + 1;
   Warnings_Id     : constant Rule_Id := Style_Checks_Id + 1;

   subtype Compiler_Checks is Rule_Id range Restrictions_Id ..  Warnings_Id;

   First_Compiler_Check : constant := Restrictions_Id;
   First_Rule           : constant Rule_Id := Warnings_Id + 1;

end Gnatcheck.Ids;
