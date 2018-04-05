------------------------------------------------------------------------------
--                                                                          --
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                A S I S _ U L . S I M P L E _ S T A C K S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2012-2014, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

--  Defines a simple unlimited stack abstraction.

with Ada.Containers.Vectors;

generic
   type Element_Type is private;
   No_Element : Element_Type;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package ASIS_UL.Simple_Stacks is

   --  Element_Type is the type of the stack elements.
   --
   --  No_Element is the special value to be returned in case if the state of
   --  the stack does not allow the given operation to return any meaningful
   --  result. That is, opposite to the classical stac abstraction, this one
   --  returns No_Element instead of raising an exception.
   --
   --  The main usage of this stack is to be used as a state of
   --  Traverse_Element - if we Push an element being visited in Pre_Operation
   --  and Pop it in Post_Operation, then the stack gives us the track to the
   --  top of our traversal (No_Element is set to Nil_Element).

   type Simple_Stack is private;

   procedure Push (S : in out Simple_Stack; Elem : Element_Type);
   --  Places Elem on the top of the stack

   function Pop (S : in out Simple_Stack) return Element_Type;
   --  Returns the top element of the stack and removes this element from
   --  the stack. Returns No_Element if the stack is empty

   procedure Pop (S : in out Simple_Stack);
   --  Removes the top element from the stack. Does nothing if the stack
   --  is empty

   function Top
     (S         : in out Simple_Stack;
      Step_Down :        Natural := 0)
      return             Element_Type;
   --  Returns the top element of the stack without removing this element from
   --  the stack. Returns No_Element if the stack is empty. If Step_Down is
   --  not 0, returns the Element which is Step_Down steps to the bottom of the
   --  stack, except if the stack depth is less than Step_Down, otherwise
   --  No_Element is returned

   function Size (S : in out Simple_Stack) return Natural;
   --  Returns the number of elements in the stack

   function Is_Empty_Stack (S : in out Simple_Stack) return Boolean;
   --  Same as Size = 0

private
   package Simple_Stacks_Package is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Element_Type,
      "="          => "=");

      type Simple_Stack is new Simple_Stacks_Package.Vector with null record;
end ASIS_UL.Simple_Stacks;
