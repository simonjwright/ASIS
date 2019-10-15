------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--        A S I S . D A T A _ D E C O M P O S I T I O N . G N A T R         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--              Copyright (C) 2019, Free Software Foundation, Inc.          --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License  distributed with ASIS-for-GNAT; see file     --
-- COPYING. If not, write to the Free Software Foundation,  59 Temple Place --
-- - Suite 330,  Boston, MA 02111-1307, USA.                                --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
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

--  This package contains resources needed to extract representation
--  information from -gnatR3js output and to use it for implementing ASIS DDA
--  queries.

private package Asis.Data_Decomposition.gnatRj is

   procedure Set_Repinfo_File
     (E       :     Asis.Element;
      Success : out Boolean);
   --  Detects the name of the JSON file that may contain the representation
   --  information for the source E is extracted from. E can be any non-nil
   --  Element. if Is_Nil (E), sets Success to False and exit. Note, that in
   --  case of a package spec the representation information for spec is
   --  duplicated in the JSON file created for the corresponding body (if any).
   --  If the needed file cannot be located, sets Success to False and exists.
   --
   --  Checks if the currently accesses JSON file is the file located. If
   --  it is, sets Success to True and exits. If it is not, tries to read
   --  the located JSON file into the buffer (by calling
   --  Repinfo.Input.Read_JSON_Stream). Sets Success to True if the attemt is
   --  successful and False otherwise.

   function Get_Entity_Name (E : Asis.Element) return String;
   --  Returns full qualified Ada name of the declaration that immediatelly
   --  encloses E (or of E if E itself is a declaration).

end Asis.Data_Decomposition.gnatRj;
