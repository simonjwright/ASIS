------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--            G N A T C H E C K . T R A V E R S A L _ S T A C K             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2004-2007, AdaCore                      --
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

--  This package defines the data structure that represents the current state
--  of the traversal of the argument unit: starting from the top element of the
--  traversing down to the element that encloses the currently visited.
--  The structure is the combination of the unlimited stack and the table
--  that allows the direct access to the elements using.
--
--  This structure provides the effective (comparing with Enclosing_Element)
--  way of investigating the enclosing context of the Element being visited

with Asis;

package Gnatcheck.Traversal_Stack is

   --  The traversal stack (it can also be called a traversal table) contains
   --  for each ASIS Element some record containing this Element and some
   --  characteristics of this Element. We keep the corresponding structure
   --  invisible for the clients by defining it in the package body. If you
   --  would like to add a new rule check that needs some new information
   --  to be added and retrieved from the traversal stack, you should add
   --  the corresponding flag or field in the Element_Record in the body and
   --  to provide the corresponding interface routines.

   type Elmt_Idx is new Natural;
   --  Index of the Element in the traversal stack

   No_Element : constant Elmt_Idx := 0;

   Stack_Error : exception;

   function Present (Idx : Elmt_Idx) return Boolean;
   --  Checks that its argument is not equal to No_Element

   procedure Push (E : Asis.Element);
   procedure Pop;
   --  These two procedures implement the basic stack abstraction. The first
   --  procedure is called in pre-operation, it puts the record corresponding
   --  to the argument Element to the top of the stack. The last procedure is
   --  called in post-operation and removes the element record from the top of
   --  the stack (Pop raises Stack_Error if called for an empty stack)

   function Get_Element (From_Idx : Elmt_Idx) return Asis.Element;
   --  Returns the ASIS Element corresponding to the given index. Raises
   --  Stack_Error if the argument exceeds the number of the elements currently
   --  stored in the traversal stack.

   function Get_Enclosing_Element
     (Steps_Up : Elmt_Idx := 0)
      return     Asis.Element;
   --  Gets the enclosing element for the currently visited element, taking
   --  the Steps_Up steps up the structure being traversed. Raises Stack_Error
   --  if stepping up exceeds this structure. If Steps_Up is 0, this function
   --  is equivalent to Asis.Elements.Enclosing_Element called to the Element
   --  being visited during the traversal. The main reason to use this function
   --  instead of Asis.Elements.Enclosing_Element is to improve the
   --  performance.

   procedure Initialize;
   --  Initializes internal tables and puts Nil_Element to the bottom of the
   --  stack to make Get_Enclosing_Element working correctly on top-level
   --  Elements

   -------------------------------------------------------------------
   -- Functions that get specific elements from the traversal stack --
   -------------------------------------------------------------------

   --  Functions that defined in this section are looking for some specific
   --  element in the chain of elements that ends up with the element currently
   --  being visited during the traversal. Most of them start from the top of
   --  the stack and go to the bottom, looking for the element of interest.
   --  That's why these functions do not have any argument

   function Enclosing_Subprogram_Body return Elmt_Idx;
   --  Looks for the nearest enclosing subprogram body, entry body or accept
   --  statement. Returns No_Element if there is no such construct.

   --------------------------------------------------
   -- Retrieving and modifying elements properties --
   --------------------------------------------------

   function Has_Return (El : Elmt_Idx) return Boolean;
   --  Provided that El represents a function body, returns the flag that says
   --  that a return statement has already been encountered in this body

   procedure Set_Has_Return (El : Elmt_Idx);
   --  Provided that El represents a function body, sets ON the flag that says
   --  that a return statement has already been encountered in this body

end Gnatcheck.Traversal_Stack;
