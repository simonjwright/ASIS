------------------------------------------------------------------------------
--                                                                          --
--                        ASIS-for-GNAT COMPONENTS                          --
--                                                                          --
--             A S I S . E X T E N S I O N S . I T E R A T O R              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2014, Free Software Foundation, Inc.          --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR  PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with ASIS-for-GNAT;  see file --
-- COPYING.  If not,  write  to the  Free Software Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

--  This package contains some iterator generics that are more general than the
--  Asis.Iterator.Traverse_Element procedure:
--
--     Traverse_Element -- same as Asis.Iterator.Traverse_Element
--
--     Traverse_Element_Q -- same as Traverse_Element, but passes in the
--     relevant queries, and has pre/post operations for lists as well as for
--     elements. The Q parameter of each Pre/Post op tells how we got here. For
--     example, when we traverse A_Package_Declaration, we will traverse the
--     Names, then the Visible_Part_Declarative_Items, and finally the
--     Private_Part_Declarative_Items. This involves three calls to
--     Pre_List_Operation, passing Q = the corresponding enumeration literals
--     of type Structural_Queries (Names, Visible_Part_Declarative_Items,
--     Private_Part_Declarative_Items), which are overloaded with the names of
--     the query functions themselves. This allows clients to tell whether a
--     given declaration is in the visible or private part, for example. If
--     Element is the N'th element of an Element_List, the Index parameter is
--     passed N, and Is_List_Element is True. Otherwise (not a list element),
--     Element is the N'th child of its parent, and Index = N, and
--     Is_List_Element is False.
--
--     Traverse_Unit -- similar to Traverse_Element, but traverses a
--     Compilation_Unit. This is needed because a Compilation_Unit is not an
--     element.
--
--     Traverse_Unit_Q -- traverses a Compilation_Unit with queries, list
--     operations, and list index as for Traverse_Element_Q.
--
--  All of these procedures take a Traverse_Nils parameter, which controls
--  whether Nil_Elements are traversed -- False means Nil_Elements are ignored.
--
--  Also a Syntactic parameter, which controls how operator calls and
--  prefix-notation calls are traversed. Syntactic = True means the traversal
--  follows the syntax, so for "A+B", we traverse A, then +, then B, and for
--  "X.P(Y)", we traverse X, then P, then Y. Syntactic = True means the
--  traversal follows the semantics, so the above are treated as if they were
--  "+"(A,B) and P(X,Y).

with A4G.Queries;

package Asis.Extensions.Iterator is

   subtype Structural_Queries is A4G.Queries.Structural_Queries;
   --  Enumeration of all structural queries -- that is, queries that take an
   --  Element and return its syntactic subelements

   subtype Query_Index is A4G.Queries.Query_Index;

   function Is_Leaf (E : Asis.Element) return Boolean;
   --  True if E has no subelements

   --  Tree traversals:

   generic

      type State_Information is limited private;

      with procedure Pre_Operation
                       (Q       :        Structural_Queries;
                        Index   :        Query_Index;
                        Is_List_Element : Boolean;
                        Element :        Asis.Element;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

      with procedure Post_Operation
                       (Q       :        Structural_Queries;
                        Index   :        Query_Index;
                        Is_List_Element : Boolean;
                        Element :        Asis.Element;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

      with procedure Pre_List_Operation
                       (Q       :        Structural_Queries;
                        List    : Asis.Element_List;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

      with procedure Post_List_Operation
                       (Q       :        Structural_Queries;
                        List    : Asis.Element_List;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

   procedure Traverse_Unit_Q
     (Unit          : Asis.Compilation_Unit;
      Control       : in out Traverse_Control;
      State         : in out State_Information;
      Traverse_Nils : Boolean;
      Syntactic     : Boolean);
   --  Traverses all the syntactical structure of the argument Compilation
   --  Unit. In ASIS, a Compilation Unit consists of context clause Elements
   --  and of the Element representing the program unit, and these syntax
   --  elements does not have a common root. Traverse_Unit instantiates
   --  Asis.Iterator.Traverse_Element passing its own formal parameters as
   --  actuals for Traverse_Element. Then it goes into all the
   --  first-depth-level structural components of the argument Compilation
   --  Unit by applying this instance of Traverse_Element to it.
   --
   --  If the value of traverse Control becomes Terminate_Immediately,
   --  traversing of all the unit components is terminated (that is, if it
   --  happens in some context clause Element, the Unit declaration Element
   --  will not be traversed.
   --
   --  Appropriate Unit_Kinds:
   --     A_Procedure
   --     A_Function
   --     A_Package
   --
   --     A_Generic_Procedure
   --     A_Generic_Function
   --     A_Generic_Package
   --
   --     A_Procedure_Instance
   --     A_Function_Instance
   --     A_Package_Instance
   --
   --     A_Procedure_Renaming
   --     A_Function_Renaming
   --     A_Package_Renaming
   --
   --     A_Generic_Procedure_Renaming
   --     A_Generic_Function_Renaming
   --     A_Generic_Package_Renaming
   --
   --     A_Procedure_Body
   --     A_Procedure_Body
   --     A_Function_Body
   --     A_Package_Body
   --
   --     A_Procedure_Body_Subunit
   --     A_Function_Body_Subunit
   --     A_Package_Body_Subunit
   --     A_Task_Body_Subunit
   --     A_Protected_Body_Subunit

   generic

      type State_Information is limited private;

      with procedure Pre_Operation
                       (Element :        Asis.Element;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

      with procedure Post_Operation
                       (Element :        Asis.Element;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

   procedure Traverse_Unit
     (Unit          : Asis.Compilation_Unit;
      Control       : in out Traverse_Control;
      State         : in out State_Information;
      Traverse_Nils : Boolean := False;
      Syntactic     : Boolean := True);
   --  Same as Traverse_Unit_Q above, except the Q parameter and the List
   --  operations are not present.

   generic

      type State_Information is limited private;

      with procedure Pre_Operation
                       (Q       :        Structural_Queries;
                        Index   :        Query_Index;
                        Is_List_Element : Boolean;
                        Element :        Asis.Element;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

      with procedure Post_Operation
                       (Q       :        Structural_Queries;
                        Index   :        Query_Index;
                        Is_List_Element : Boolean;
                        Element :        Asis.Element;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

      with procedure Pre_List_Operation
                       (Q       :        Structural_Queries;
                        List    : Asis.Element_List;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

      with procedure Post_List_Operation
                       (Q       :        Structural_Queries;
                        List    : Asis.Element_List;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

   procedure Traverse_Element_Q
     (Root_Query           : Structural_Queries;
      Root_Index           : Query_Index;
      Root_Is_List_Element : Boolean;
      Element              : Asis.Element;
      Control              : in out Traverse_Control;
      State                : in out State_Information;
      Traverse_Nils        : Boolean;
      Syntactic            : Boolean);
   --  This is the same as Asis.Iterator.Traverse_Element, except that the
   --  query that reached Element is passed in Q. Root_Query is passed to the
   --  outermost calls to Pre/Post_Operation.

   generic

      type State_Information is limited private;

      with procedure Pre_Operation
                       (Element :        Asis.Element;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

      with procedure Post_Operation
                       (Element :        Asis.Element;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

   procedure Traverse_Element
     (Element       : Asis.Element;
      Control       : in out Traverse_Control;
      State         : in out State_Information;
      Traverse_Nils : Boolean;
      Syntactic     : Boolean);
   --  This is the same as Traverse_Element in Asis.Iterator. See that package
   --  for detailed documentation.

end Asis.Extensions.Iterator;
