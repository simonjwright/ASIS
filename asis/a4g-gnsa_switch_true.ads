------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                       A 4 G . G N S A _ S W I T C H                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2016-2017, Free Software Foundation, Inc.       --
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

with System.OS_Lib; use System.OS_Lib;

package A4G.GNSA_Switch is

   Use_GNSA : constant Boolean := True;

   GNSA_Dir : String := "libexec" & Directory_Separator & "asis-gnsa";
   --  Name of the directory where GNSA to be used for ASIS tree creation is
   --  supposed to be located

end A4G.GNSA_Switch;
