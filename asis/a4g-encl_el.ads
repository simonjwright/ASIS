------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . E N C L _ E L                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2017, Free Software Foundation, Inc.       --
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

--  This package contains routines for computing the enclosing element
--  for the Asis.Elements.Enclosing_Element function

with Asis;

with Types;             use Types;

package A4G.Encl_El is

   function Corresponding_Instantiation
     (Element : Asis.Element)
      return Asis.Element;
   --  This function accepts an Element representing an expanded generic
   --  declaration as an argument and returns the generic instantiation
   --  which was expanded in the argument declaration. According to subclause
   --  15.26, this instantiation should be returned as the Enclosing_Element
   --  for the expanded generic declaration.
   --
   --  Should we move this function in Asis.Extensions?

   function Enclosing_For_Explicit_Instance_Component
     (Element : Asis.Element)
      return Asis.Element;
   --  Computes the Enclosing Element for an explicit component of an
   --  expanded generic declaration. The problem in this case is, that if
   --  the result represents the whole expanded declaration, the
   --  Special_Case field of the result should be properly set

   function Enclosing_Element_For_Explicit
     (Element : Asis.Element)
      return Asis.Element;
   --  This is the general constructor of enclosing element for explicit
   --  elements

   function Enclosing_Element_For_Implicit
     (Element : Asis.Element)
      return Asis.Element;
   --  This is the general constructor of enclosing element for implicit
   --  elements. It's only partially implemented for now.

   function Enclosing_Element_For_Limited_View
     (Element : Asis.Element)
      return Asis.Element;
   --  This is the general constructor of enclosing element for elements from
   --  limited view.

   function Parent_No_Normalization (Node : Node_Id) return Node_Id;
   --  this function is the modification of Atree.Parent. It is able
   --  to deal in the "ASIS mode" with the sequences of one-identifier
   --  declarations/with clauses resulting from the normalization of
   --  multi-name declarations/with clauses which is done by the
   --  compiler

   procedure Skip_Normalized_Declarations_Back (Node : in out Node_Id);
   --  This procedure is applied in case when the compiler may normalize a
   --  multi-identifier declaration (or multi-name with and use clauses) in a
   --  set of equivalent one-identifier (one-name) declarations (clauses). It
   --  is intended to be called for Node representing any  declaration (clause)
   --  in this normalized sequence, and it resets its parameter to point to the
   --  first declaration (clause) in this sequence
   --
   --  There is no harm to call this procedure for Node which does not
   --  represent a normalized declaration (or even which does not represent
   --  any declaration at all), or for Node which represents the first
   --  declaration in a normalized chain - the procedure simply leaves
   --  its parameter intact.
   --
   --  (In some sense this procedure may be considered as an "inversion
   --  of the local procedure Skip_Normalized_Declarations defined in
   --  the body of the A4G.Mapping package)

end A4G.Encl_El;
