------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . E E _ C A C H E                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2014, Free Software Foundation, Inc.             --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines a cache storage that allows to speed up computation
--  of Enclosing_Element on expressions.

pragma Ada_2012;

with Asis; use Asis;

package A4G.EE_Cache is

   procedure Store_Enclosing_Element
     (Parent : Asis.Element;
      Child  : Asis.Element);
   --  If Child is already stored as enclosed element, does nothing. Otherwise
   --  stores Child (and Parent if it has not been stored yet) and sets for
   --  Child the link to parent as to its enclosing element.
   --  Does nothing f A4G.A_Opt.Cache_EE_Results is OFF.

   procedure Store_Enclosing_Element
     (Parent   : Asis.Element;
      Children : Asis.Element_List);
   --  If the first element of Children is already stored as enclosed element,
   --  does nothing. Otherwise stores all the components of Children (and
   --  Parent if it has not been stored yet) and sets for each component of
   --  Children the link to parent as to its enclosing element.
   --  Does nothing f A4G.A_Opt.Cache_EE_Results is OFF.

   function Get_Enclosing_Element
     (E       :     Asis.Element;
      Success : out Boolean)
      return        Asis.Element;
   --  Tries to get the enclosing element for E from the cache storage. If the
   --  attempt is successfull, sets Success ON and returns the enclosing
   --  element as the result, otherwise sets Sussess OFF, the result in this
   --  case is undefined. Note that in some cases the result can be Nil_Element
   --  and Success may be set ON (not sure that such cases will be stored, but
   --  theoretically this is possible).

   procedure Init_EE_Cache;
   --  Chould be called when opening a Context.

   procedure Print_EE_Cache;
   --  Prints the content of the cache storage into Stdout for debuging
   --  purposes.

   procedure Print_EE_Cache_Summary_Info;
   --  Debug procedure. Prints some general characteristics of the cache
   --  storage into Stdout.

end A4G.EE_Cache;
