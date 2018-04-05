------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--             A S I S . E X T E N S I O N S . I T E R A T O R              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2014, Free Software Foundation, Inc.          --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with  ASIS-for-GNAT; see file --
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
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Asis.Compilation_Units;     use Asis.Compilation_Units;
with Asis.Elements;              use Asis.Elements;
with Asis.Errors;                use Asis.Errors;
with Asis.Exceptions;            use Asis.Exceptions;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;

with A4G.Queries;             use A4G.Queries;
with A4G.Vcheck;              use A4G.Vcheck;

package body Asis.Extensions.Iterator is

   Package_Name : constant String := "Asis.Extensions.Iterator.";

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (E : Asis.Element) return Boolean is
   begin
      return Appropriate_Queries (Flat_Element_Kind (E))'Length = 0;
   end Is_Leaf;

   ---------------------
   -- Traverse_Unit_Q --
   ---------------------

   procedure Traverse_Unit_Q
     (Unit          : Asis.Compilation_Unit;
      Control       : in out Traverse_Control;
      State         : in out State_Information;
      Traverse_Nils : Boolean;
      Syntactic     : Boolean)
   is
      Arg_Kind : constant Unit_Kinds := Unit_Kind (Unit);

      procedure Process_Element is new Traverse_Element_Q
        (State_Information => State_Information);

   begin
      Check_Validity (Unit, Package_Name & "Control");

      if not (Arg_Kind in A_Procedure .. A_Protected_Body_Subunit) then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Package_Name & "Traverse_Unit_Q");
      end if;

      declare
         Cont_Clause_Elements : constant Element_List :=
            Asis.Elements.Context_Clause_Elements
              (Compilation_Unit => Unit,
               Include_Pragmas  => True);

         Unit_Element : constant Asis.Element :=
           Asis.Elements.Unit_Declaration (Unit);

      begin
         Pre_List_Operation
           (Context_Clause_Elements, Cont_Clause_Elements, Control, State);

         for I in Cont_Clause_Elements'Range loop
            Process_Element
              (Context_Clause_Elements,
               Query_Index (I),
               True,
               Cont_Clause_Elements (I),
               Control,
               State,
               Traverse_Nils,
               Syntactic);
         end loop;

         Post_List_Operation
           (Context_Clause_Elements, Cont_Clause_Elements, Control, State);

         Process_Element (Unit_Declaration, 2, False, Unit_Element, Control,
                          State, Traverse_Nils, Syntactic);
      end;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Inappropriate_Context     |
           ASIS_Inappropriate_Container   |
           ASIS_Inappropriate_Element     |
           ASIS_Inappropriate_Line        |
           ASIS_Inappropriate_Line_Number =>

         Add_Call_Information (Outer_Call => Package_Name & "Traverse_Unit_Q");
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Traverse_Unit_Q");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Traverse_Unit_Q",
            Ex            => Ex,
            Arg_CU        => Unit);
   end Traverse_Unit_Q;

   -------------------
   -- Traverse_Unit --
   -------------------

   procedure Traverse_Unit
     (Unit          : Asis.Compilation_Unit;
      Control       : in out Traverse_Control;
      State         : in out State_Information;
      Traverse_Nils : Boolean := False;
      Syntactic     : Boolean := True)
   is
      Arg_Kind : constant Unit_Kinds := Unit_Kind (Unit);

      procedure Process_Element is new Traverse_Element
        (State_Information => State_Information,
         Pre_Operation     => Pre_Operation,
         Post_Operation    => Post_Operation);

   begin
      Check_Validity (Unit, Package_Name & "Control");

      if not (Arg_Kind in A_Procedure .. A_Protected_Body_Subunit) then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Package_Name & "Traverse_Unit");
      end if;

      declare
         Cont_Clause_Elements : constant Element_List :=
            Asis.Elements.Context_Clause_Elements
              (Compilation_Unit => Unit,
               Include_Pragmas  => True);

         Unit_Element : constant Asis.Element :=
           Asis.Elements.Unit_Declaration (Unit);

      begin

         for I in Cont_Clause_Elements'Range loop
            Process_Element (Cont_Clause_Elements (I), Control, State,
                             Traverse_Nils, Syntactic);
         end loop;

         Process_Element (Unit_Element, Control, State,
                          Traverse_Nils, Syntactic);
      end;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Inappropriate_Context     |
           ASIS_Inappropriate_Container   |
           ASIS_Inappropriate_Element     |
           ASIS_Inappropriate_Line        |
           ASIS_Inappropriate_Line_Number =>

         Add_Call_Information (Outer_Call => Package_Name & "Traverse_Unit");
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Traverse_Unit");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Traverse_Unit",
            Ex            => Ex,
            Arg_CU        => Unit);
   end Traverse_Unit;

   ------------------------
   -- Traverse_Element_Q --
   ------------------------

   procedure Traverse_Element_Q
     (Root_Query           : Structural_Queries;
      Root_Index           : Query_Index;
      Root_Is_List_Element : Boolean;
      Element              : Asis.Element;
      Control              : in out Traverse_Control;
      State                : in out State_Information;
      Traverse_Nils        : Boolean;
      Syntactic            : Boolean)
   is

      procedure Recursive_Traversal
        (Q               : Structural_Queries;
         Index           : Query_Index;
         Is_List_Element : Boolean;
         Element         : Asis.Element;
         Control         : in out Traverse_Control);
      --  This procedure does the main job

      procedure Traverse_Children
        (Element : Asis.Element;
         Control : in out Traverse_Control);
      --  Traverses children of a given construct

      ------------------------------------------------------
      -- Pre-condition: any value of Control is possible  --
      ------------------------------------------------------

      procedure Traverse_Children
        (Element : Asis.Element;
         Control : in out Traverse_Control)
      is
         --  The value of Control has been set by Pre_Operation

         --  Child access is an array containing access to the functions
         --  needed to access element's children
         Child_Access : constant Func_Elem_Array :=
                          Appropriate_Queries (Element, Syntactic);

         function Do_Return return Boolean;
            --  Check and reset the Control value on return from the traverse.
            --  the boolean returned says wether or not the program should
            --  return

         function Do_Return return Boolean is
         begin
         --------------------------------------------------------
         -- Post-condition:   Control  = Continue              --
         --                or Control  = Abandon_Siblings      --
         --                or Control  = Terminate_Immediately --
         --------------------------------------------------------
            case Control is
               when Terminate_Immediately =>
                  return True;
               when Continue              =>
                  return False;
               when Abandon_Siblings      =>
                  Control := Continue;
                  --  to continue the traversal of the parent
                  --  of the Each_Child (that is, Element) with
                  --  its Post_Operation
                  return True;  -- to prevent traversal of Each_Child siblings
               when Abandon_Children =>
                  --  this choice could never been chosen!!!
                  return False;
            end case;
         ---------------------------------------------------------------
         -- Post-Condition : Control = Continue (True or False)     --
         --               or Control = Terminate_Immediately (True) --
         ---------------------------------------------------------------
         end Do_Return;

      begin  --  Traverse_Children

         --  Validity Check has already been done

         ------------------------------------------
         -- Pre-condition:   Control  = Continue --
         ------------------------------------------
         --  Classify the Element using the various kinds queries.
         --  Query for all children of the Element in left-to-right order.
         --  Perform a depth-first traversal on each child.

         --  The only possibility for Control is to be equal to Continue here!
         --  If the current Element has no children, Control remains to be
         --  equal to Continue

         for Each_Query in Child_Access'Range loop
            case Child_Access (Each_Query).Query_Kind is
               when Bug | CU_Query_Kinds =>
                  raise Internal_Implementation_Error;
               when Boolean_Query =>
                  null; -- Ignore Boolean queries
               when Single_Element_Query =>
                  declare
                     Q : constant Structural_Queries :=
                       Child_Access (Each_Query).Q;
                     Child : constant Asis.Element :=
                       Child_Access (Each_Query).Func_Simple (Element);
                  begin

                     if Traverse_Nils
                       or else
                         Asis.Elements.Element_Kind (Child) /= Not_An_Element
                     then
                        Recursive_Traversal
                          (Q, Each_Query, False, Child, Control);

                        if Do_Return then
                           return;
                        end if;

                     end if;

                  end;

               when Element_List_Query =>
                  declare
                     Q : constant Structural_Queries :=
                       Child_Access (Each_Query).Q;
                     Child_List : constant Asis.Element_List :=
                       Child_Access (Each_Query).Func_List (Element);
                  begin
                     begin
                        Pre_List_Operation (Q, Child_List, Control, State);
                     exception
                        when ASIS_Inappropriate_Context          |
                             ASIS_Inappropriate_Container        |
                             ASIS_Inappropriate_Compilation_Unit |
                             ASIS_Inappropriate_Element          |
                             ASIS_Inappropriate_Line             |
                             ASIS_Inappropriate_Line_Number      |
                             ASIS_Failed                         =>

                           Add_Call_Information (
                              Argument   => Element,
                              Outer_Call =>
                                "Actual procedure for Pre_List_Operation");

                           raise;
                     end;

                     if Do_Return then
                        return;
                     end if;

                     --  If the list is empty, it's ok ... nothing is processed
                     for Index in Child_List'Range loop

                        Recursive_Traversal
                          (Q, Query_Index (Index), True, Child_List (Index),
                           Control);

                        if Do_Return then
                           return;
                        end if;

                     end loop;

                     begin
                        Post_List_Operation (Q, Child_List, Control, State);
                     exception
                        when ASIS_Inappropriate_Context          |
                             ASIS_Inappropriate_Container        |
                             ASIS_Inappropriate_Compilation_Unit |
                             ASIS_Inappropriate_Element          |
                             ASIS_Inappropriate_Line             |
                             ASIS_Inappropriate_Line_Number      |
                             ASIS_Failed                         =>

                           Add_Call_Information (
                              Argument   => Element,
                              Outer_Call =>
                                "Actual procedure for Post_List_Operation");

                           raise;
                     end;
                  end;

               when Element_List_Query_With_Boolean =>
                  declare
                     Q : constant Structural_Queries :=
                       Child_Access (Each_Query).Q;
                     Child_List : constant Asis.Element_List :=
                       Child_Access (Each_Query).Func_List_Boolean
                         (Element, Child_Access (Each_Query).Bool);
                  begin
                     begin
                        Pre_List_Operation (Q, Child_List, Control, State);
                     exception
                        when ASIS_Inappropriate_Context          |
                             ASIS_Inappropriate_Container        |
                             ASIS_Inappropriate_Compilation_Unit |
                             ASIS_Inappropriate_Element          |
                             ASIS_Inappropriate_Line             |
                             ASIS_Inappropriate_Line_Number      |
                             ASIS_Failed                         =>

                           Add_Call_Information (
                              Argument   => Element,
                              Outer_Call =>
                                "Actual procedure for Pre_List_Operation");

                           raise;
                     end;

                     if Do_Return then
                        return;
                     end if;

                     --  If the list is empty, it's ok ... nothing is processed
                     for Index in Child_List'Range loop

                        Recursive_Traversal
                          (Q, Query_Index (Index), True, Child_List (Index),
                           Control);

                        if Do_Return then
                           return;
                        end if;

                     end loop;

                     begin
                        Post_List_Operation (Q, Child_List, Control, State);
                     exception
                        when ASIS_Inappropriate_Context          |
                             ASIS_Inappropriate_Container        |
                             ASIS_Inappropriate_Compilation_Unit |
                             ASIS_Inappropriate_Element          |
                             ASIS_Inappropriate_Line             |
                             ASIS_Inappropriate_Line_Number      |
                             ASIS_Failed                         =>

                           Add_Call_Information (
                              Argument   => Element,
                              Outer_Call =>
                                "Actual procedure for Post_List_Operation");

                           raise;
                     end;
                  end;

            end case;
         end loop;
         -------------------------------------------
         -- Post-condition:   Control  = Continue --
         -------------------------------------------
         -- if Terminate_Immediately was set, we  --
         -- just do not entry this procedure ...  --
         -------------------------------------------

      end Traverse_Children;
      --------------------------------------------------------
      -- Post-condition: any value of Control is possible,  --
      --------------------------------------------------------

      -------------------------
      -- Recursive_Traversal --
      -------------------------

      ----------------------------------------
      -- Pre-condition: Control = Continue  --
      ----------------------------------------
      procedure Recursive_Traversal
        (Q               : Structural_Queries;
         Index           : Query_Index;
         Is_List_Element : Boolean;
         Element         : Asis.Element;
         Control         : in out Traverse_Control) is
      begin

         ----------------------------------------
         -- Pre-condition: Control = Continue  --
         ----------------------------------------

         begin
            --  Visit the Element
            Pre_Operation (Q, Index, Is_List_Element, Element, Control, State);
         exception
            when ASIS_Inappropriate_Context          |
                 ASIS_Inappropriate_Container        |
                 ASIS_Inappropriate_Compilation_Unit |
                 ASIS_Inappropriate_Element          |
                 ASIS_Inappropriate_Line             |
                 ASIS_Inappropriate_Line_Number      |
                 ASIS_Failed                         =>

               Add_Call_Information (
                  Argument   => Element,
                  Outer_Call => "Actual procedure for Pre_Operation");

               raise;
         end;

         --------------------------------------------------------
         -- Post-condition: any value of Control is possible   --
         --------------------------------------------------------

         if Control = Continue then
            Traverse_Children (Element, Control);
         end if;

         --------------------------------------------------------
         -- Pre-condition: any value of Control is possible,  --
         --------------------------------------------------------

         case Control is
            when Terminate_Immediately =>
               return;
            when Continue =>

               begin
                  --  Revisit the Element
                  Post_Operation
                    (Q, Index, Is_List_Element, Element, Control, State);
               exception
                  when ASIS_Inappropriate_Context          |
                       ASIS_Inappropriate_Container        |
                       ASIS_Inappropriate_Compilation_Unit |
                       ASIS_Inappropriate_Element          |
                       ASIS_Inappropriate_Line             |
                       ASIS_Inappropriate_Line_Number      |
                       ASIS_Failed                         =>

                     Add_Call_Information (
                        Argument   => Element,
                        Outer_Call => "Actual procedure for Post_Operation");

                     raise;
               end;

               --  reset the Control set by Post_Operation:
               case Control is
                  when Terminate_Immediately =>
                     return;
                  when Continue         =>
                     null;
                  when Abandon_Children =>
                     Control := Continue;
                     --  the current Element has no children to traverse
                     --  anymore!
                  when Abandon_Siblings =>
                     null;
               end case;

            when Abandon_Children =>
               --  OK, we abandonned the children, now we go up and continue
               Control := Continue;
            when Abandon_Siblings =>
               null;
         end case;
         ---------------------------------------------------------
         -- Post-condition:   Control  = Continue               --
         --                or Control  = Abandon_Siblings       --
         --                or Control = Terminate_Immediately   --
         ---------------------------------------------------------
      end Recursive_Traversal;
      ---------------------------------------------------------
      -- Post-condition:   Control  = Continue               --
      --                or Control  = Abandon_Siblings       --
      --                or Control = Terminate_Immediately   --
      ---------------------------------------------------------

   ----------------------------------
   -- Traverse_Element_Q Main body --
   ----------------------------------

   begin
      Check_Validity (Element, "Asis.Elements.Traverse_Element");

      if Asis.Elements.Is_Nil (Element) then
         Raise_ASIS_Inappropriate_Element
           ("Asis.Iterator.Traverse_Element",
            Wrong_Kind => Not_An_Element);
      elsif Control /= Continue then
         return;
      end if;

      ----------------------------------------
      -- Pre-condition: Control = Continue  --
      ----------------------------------------
      Recursive_Traversal (Q       => Root_Query,
                           Index   => Root_Index,
                           Is_List_Element => Root_Is_List_Element,
                           Element => Element,
                           Control => Control);
   exception
      when ASIS_Inappropriate_Element          |
           ASIS_Inappropriate_Context          |
           ASIS_Inappropriate_Container        |
           ASIS_Inappropriate_Compilation_Unit |
           ASIS_Inappropriate_Line             |
           ASIS_Inappropriate_Line_Number      |
           ASIS_Failed                         =>

         Add_Call_Information
           (Argument   => Element,
            Outer_Call => "Asis.Iterator.Traverse_Element");

         raise;
      --  when others =>
      --  Actual Pre- and Postoperations can raise whatever they want, and
      --  at the level of Traverse_Element_Q we can (and should) do nothing
      --  with this. So we just let this exception go ahead
      --   raise;
   end Traverse_Element_Q;

   ----------------------
   -- Traverse_Element --
   ----------------------

   procedure Traverse_Element
     (Element       : Asis.Element;
      Control       : in out Traverse_Control;
      State         : in out State_Information;
      Traverse_Nils : Boolean;
      Syntactic     : Boolean)
   is

      --  We simply pass the buck to Traverse_Element_Q, ignoring the Q
      --  Index, and Is_List_Element parameters, and doing nothing for lists.

      procedure Pre
        (Q               : Structural_Queries;
         Index           : Query_Index;
         Is_List_Element : Boolean;
         Element         : Asis.Element;
         Control         : in out Traverse_Control;
         State           : in out State_Information);

      procedure Post
        (Q               : Structural_Queries;
         Index           : Query_Index;
         Is_List_Element : Boolean;
         Element         :        Asis.Element;
         Control         : in out Traverse_Control;
         State           : in out State_Information);

      procedure Pre_List
        (Q       : Structural_Queries;
         List    : Asis.Element_List;
         Control : in out Traverse_Control;
         State   : in out State_Information) is null;

      procedure Post_List
        (Q       : Structural_Queries;
         List    : Asis.Element_List;
         Control : in out Traverse_Control;
         State   : in out State_Information) is null;

      ---------
      -- Pre --
      ---------

      procedure Pre
        (Q               : Structural_Queries;
         Index           : Query_Index;
         Is_List_Element : Boolean;
         Element         : Asis.Element;
         Control         : in out Traverse_Control;
         State           : in out State_Information)
      is
         pragma Unreferenced (Q);
         pragma Unreferenced (Index);
         pragma Unreferenced (Is_List_Element);
      begin
         Pre_Operation (Element, Control, State);
      end Pre;

      ----------
      -- Post --
      ----------

      procedure Post
        (Q               : Structural_Queries;
         Index           : Query_Index;
         Is_List_Element : Boolean;
         Element         : Asis.Element;
         Control         : in out Traverse_Control;
         State           : in out State_Information)
      is
         pragma Unreferenced (Q);
         pragma Unreferenced (Index);
         pragma Unreferenced (Is_List_Element);
      begin
         Post_Operation (Element, Control, State);
      end Post;

      procedure Traverse is new Traverse_Element_Q
        (State_Information, Pre, Post, Pre_List, Post_List);

   begin
      Traverse (No_Query, 1, False, Element, Control, State,
                Traverse_Nils, Syntactic);
   end Traverse_Element;

end Asis.Extensions.Iterator;
