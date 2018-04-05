------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--              A S I S _ U L . M E T R I C S . C O M P U T E               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not,  write  to the  Free Software Foundation,  51 Franklin  --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Asis.Elements;              use Asis.Elements;
with Asis.Expressions;           use Asis.Expressions;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;
with Asis.Iterator;              use Asis.Iterator;
with Asis.Statements;            use Asis.Statements;

with ASIS_UL.Utilities;          use ASIS_UL.Utilities;

package body ASIS_UL.Metrics.Compute is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Statement_Complexity (Stmt : Element) return Metric_Count;
   --  Computes the complexity added by the argument statement to the McCabe
   --  cyclomatic complexity. See the body of
   --  METRICS.Compute.Compute_Complexity for the description of the complexity
   --  value added by different statements. If the argument is not a statement,
   --  returns 0.

   function Control_Form_Complexity (Expr : Element) return Metric_Count;
   --  Computes the complexity added by the argument short-circuit control
   --  form.  See the body of METRICS.Compute.Compute_Complexity for the
   --  description of the complexity value added by short-circuits. If the
   --  argument is not a short-circuits, returns 0.

   function Conditional_Expression_Complexity
    (Expr : Element)
     return Metric_Count;
   --  Computes the complexity added by the argument conditional expression.
   --  See the body of METRICS.Compute.Compute_Complexity for the description
   --  of the complexity value added by conditional expressions. If the
   --  argument is not a conditional expression, returns 0.

   function Handled_Locally
     (Raise_Stmt : Asis.Element;
      Body_El    : Asis.Element)
      return       Boolean;
   --  Provided that Body_Element is an subprogram body, and Raise_Stmt is a
   --  raise statement (with an exception name!) from the sequence of
   --  statements of this body, this function tries to detect the nearest
   --  exception handler that will catch this exception and not reraise (this
   --  or another) exception.

   --------------------------------
   -- Compute_Complexity_Metrics --
   --------------------------------

   procedure Compute_Complexity_Metrics
     (Body_Element :     Asis.Element;
      Counter      : out Complexity_Metric_Counter)
   is
      Loop_Nesting : Metric_Count := 0;
      --  Temporary counter for loop nesting, corresponds to the loop nesting
      --  level of the currently visited construct. Is equal to 0 if traversing
      --  is not in a loop.

      --  We define the traversal routine here to have the access to
      --  Body_Element and Counter in Pre_ and Post_Operation, and

      procedure Check_Complexity
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean);
      --  Checks if the element being visited add some value to the body
      --  complexity. Increases the corresponding complexity counter if it
      --  does. State indicates if extra exit point metric should be computed.

      procedure Complexity_Post_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean);
      --  Corrects the loop nesting counter back to zero.

      procedure Collect_Complexity is new Traverse_Element
        (Pre_Operation     => Check_Complexity,
         Post_Operation    => Complexity_Post_Op,
         State_Information => Boolean);
      --  Counts the complexity metrics for the argument element by traversing
      --  its structure. Stores the result in State parameter

      Control             :          Traverse_Control := Continue;
      Compute_Exit_Points : constant Boolean          :=
        Declaration_Kind (Body_Element) in
          A_Procedure_Body_Declaration .. A_Function_Body_Declaration;
      Tmp : Boolean := True;

      ----------------------
      -- Check_Complexity --
      ----------------------

      procedure Check_Complexity
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean)
      is
         pragma Unreferenced (State);
         Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);

         Is_Static_Loop_Stmt : Boolean;
         Tmp                 : Metric_Count;

      begin

         --  How we compute the cyclomatic complexity:
         --
         --  1. Control statements:
         --
         --     IF adds 1 + the number of ELSIF paths
         --
         --     CASE statement adds the number of alternatives minus 1
         --
         --     WHILE loop always adds 1
         --
         --     FOR loop adds 1 unless we can detect that in any case this
         --          loop will be executes at least once
         --
         --     LOOP (condition-less) adds nothing
         --
         --     EXIT statement adds 1 if contains the exit condition, otherwise
         --          adds nothing
         --
         --     GOTO statement adds nothing
         --
         --     RETURN statement adds nothing
         --
         --     SELECT STATEMENTS:
         --
         --        SELECTIVE_ACCEPT is treaded as a CASE statement (number of
         --           alternatives minus 1). Opposite to IF statement, ELSE
         --           path adds 1 to the complexity (that is, for IF,
         --           both IF ... END IF; and IF ... ELSE ... END IF; adds 1,
         --           whereas
         --              SELECT
         --                 ...
         --              OR
         --                 ...
         --              END SELECT;
         --           adds 1, but
         --
         --              SELECT
         --                 ...
         --              OR
         --                 ...
         --              ELSE
         --                 ...
         --              END SELECT;
         --           adds 2
         --
         --        TIMED_ENTRY_CALL, CONDITIONAL_ENTRY_CALL and
         --        ASYNCHRONOUS_SELECT add 1 (they are considered as an IF
         --           statement with no ELSIF parts
         --
         --  2. We do not check if some code or some path is dead (unreachable)
         --
         --  3. We do not take into account the code in the exception handlers
         --     (only the main statement sequence is analyzed). RAISE statement
         --     adds nothing
         --
         --  4. A short-circuit control form add to the complexity value the
         --     number of AND THEN or OR ELSE at the given level (that is, if
         --     we have
         --
         --       Bool := A and then (B and then C) and then E;
         --
         --     we consider this as two short-circuit control forms: the outer
         --     adds to the complexity 2 and the inner adds 1.
         --
         --     Any short-circuit control form is taken into account, including
         --     expressions being parts of type and object definitions.
         --
         --  5. Conditional expressions.
         --
         --  5.1 An IF expression is treated in the same way as an IF
         --      statement: it adds 1 + the number of ELSIF paths, but to the
         --      expression complexity.
         --
         --  5.2 A CASE expression is treated in the same way as an CASE
         --      statement: it adds the number of CASE paths minus 1, but to
         --      the expression complexity.
         --
         --  6. Quantified expressions are treated as the equivalent loop
         --     construct:
         --
         --        for some X in Y => Z (X)
         --
         --     is considered as a shortcut for
         --
         --        Result := False;
         --        Tmp := First (X);
         --
         --        while Present (Tmp) loop
         --           if Z (Tmp) then
         --              Result := True;
         --              exit;
         --           end if;
         --
         --           Tmp := Next (Tmp);
         --        end loop;
         --
         --     That is, it adds 2 (1 as WHILE loop and 1 as IF statement with
         --     no ELSIF parts.
         --
         --     'for all' expression is treated in a similar way.
         --
         --     For essential complexity, quantified expressions add 1 if
         --     Treat_Exit_As_Goto is set ON.
         --
         --  7. Any enclosed body is just skipped and is not taken into
         --     account. The only situation that is not completely clear is
         --     an enclosed package body with statement sequence part. When
         --     enclosing body is executed, this enclosed package body will
         --     also be executed inconditionally and exactly once - this is the
         --     reason to count it when computing the complexity of enclosing
         --     body. From the other side, the enclosed package body is similar
         --     to enclosed local procedures, and we for sure do not want to
         --     count enclosed procedures...

         case Arg_Kind is

            when An_Assert_Pragma =>
               if not Check_Predicates then
                  Control := Abandon_Children;
               end if;

            when An_Aspect_Specification =>
               if not Check_Predicates
                 and then
                  Defines_Predicate (Element)
               then
                  Control := Abandon_Children;
               end if;

            when Flat_Statement_Kinds =>

               Tmp := Statement_Complexity (Element);

               if Tmp > 0 and then not Count_Static_Loop then
                  Is_Static_Loop_Stmt := Is_Static_Loop (Element);
               end if;

               if Count_Static_Loop
                 or else
                  not Is_Static_Loop_Stmt
               then
                  Counter.Statement_Complexity :=
                    Counter.Statement_Complexity + Tmp;
               end if;

               if Arg_Kind = A_Loop_Statement then
                  --  We do not count unconditional loops when computing
                  --  cyclomatic complexity, so we have to add 1 for
                  --  essential complexity
                  Tmp := 1;
               end if;

               if Tmp > 0
                 and then
                  Is_Non_Structural_Statement (Element, Treat_Exit_As_Goto)
               then
                  Counter.Essential_Complexity :=
                    Counter.Essential_Complexity + Tmp;
               end if;

               if Arg_Kind in A_Loop_Statement .. A_For_Loop_Statement then
                  Loop_Nesting := Loop_Nesting + 1;

                  if Counter.Max_Loop_Nesting < Loop_Nesting then
                     Counter.Max_Loop_Nesting := Loop_Nesting;
                  end if;

               end if;

               if Compute_Exit_Points then

                  case Statement_Kind (Element) is
                     when A_Return_Statement           |
                          An_Extended_Return_Statement =>
                        Counter.Extra_Exit_Points :=
                          Counter.Extra_Exit_Points + 1;
                     when A_Raise_Statement =>

                        if not Handled_Locally (Element, Body_Element) then
                           Counter.Extra_Exit_Points :=
                             Counter.Extra_Exit_Points + 1;
                        end if;

                     when others =>
                        null;
                  end case;

               end if;

            when An_And_Then_Short_Circuit |
                 An_Or_Else_Short_Circuit  =>

               Counter.Expression_Complexity :=
                 Counter.Expression_Complexity +
                 Control_Form_Complexity (Element);

            when A_Case_Expression |
                 An_If_Expression  =>

               Counter.Expression_Complexity :=
                 Counter.Expression_Complexity +
                 Conditional_Expression_Complexity (Element);

            when A_For_All_Quantified_Expression  |
                 A_For_Some_Quantified_Expression =>

               Counter.Expression_Complexity :=
                 Counter.Expression_Complexity + 2;

               if Treat_Exit_As_Goto then
                  Counter.Essential_Complexity :=
                    Counter.Essential_Complexity + 1;
               end if;

            when An_Exception_Handler =>

               --  We just do not go inside
               Control := Abandon_Children;

            when A_Procedure_Body_Declaration |
                 A_Function_Body_Declaration  |
                 A_Package_Body_Declaration   |
                 A_Task_Body_Declaration      |
                 An_Entry_Body_Declaration    =>

               if not Is_Equal (Element, Body_Element) then
                  --  We do not go inside local bodies
                  Control := Abandon_Children;
               end if;

            when An_Expression_Function_Declaration =>

               if Is_Equal (Element, Body_Element) then
                  --  Implicit RETURN statement
                  Counter.Statement_Complexity := 1;
               else
                  Control := Abandon_Children;
               end if;

            when others =>
               null;
         end case;

--         pragma Assert
--           (Counter.Statement_Complexity >= Counter.Essential_Complexity);

      end Check_Complexity;

      ------------------------
      -- Complexity_Post_Op --
      ------------------------

      procedure Complexity_Post_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean)
      is
         pragma Unreferenced (Control, State);
         El_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
      begin

         if El_Kind in A_Loop_Statement .. A_For_Loop_Statement then
            Loop_Nesting := Loop_Nesting - 1;
         end if;

      end Complexity_Post_Op;

   begin

      if not Is_Executable_Body (Body_Element) then
         return;
      end if;

      Counter :=
        (Statement_Complexity     => 1,
         Expression_Complexity    => 0,
         Essential_Complexity     => 1,
         Essential_Complexity_New => 0,
         Max_Loop_Nesting         => 0,
         Extra_Exit_Points        => 0);
      --  Complexity values for an "empty" program.

      Collect_Complexity
        (Element => Body_Element,
         Control => Control,
         State   => Tmp);

      if Compute_Exit_Points
        and then
         Declaration_Kind (Body_Element) = A_Function_Body_Declaration
      then
         Counter.Extra_Exit_Points := Counter.Extra_Exit_Points - 1;
      end if;

      --  New stuff for McCabe essential complexity
      --  The corresponding project is postponed for the moment.

--      Counter.Essential_Complexity_New :=
--        Compute_Essential_Complexity (Body_Element);

   end Compute_Complexity_Metrics;

   ---------------------------------------
   -- Conditional_Expression_Complexity --
   ---------------------------------------

   function Conditional_Expression_Complexity
     (Expr : Element)
      return Metric_Count
   is
      Result   : Metric_Count       := 0;
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Expr);
   begin

      if Arg_Kind in A_Case_Expression | An_If_Expression then
         declare
            Expr_Paths : constant Element_List := Expression_Paths (Expr);
         begin
            Result := Expr_Paths'Length;

            if Arg_Kind = A_Case_Expression then
               Result := Result - 1;
            else
               if Path_Kind (Expr_Paths (Expr_Paths'Last)) =
                  An_Else_Expression_Path
               then
                  Result := Result - 1;
               end if;
            end if;
         end;
      end if;

      return Result;
   end Conditional_Expression_Complexity;

   -----------------------------
   -- Control_Form_Complexity --
   -----------------------------

   function Control_Form_Complexity (Expr : Element) return Metric_Count is
      Result   : Metric_Count       := 0;
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Expr);
      Tmp      : Asis.Element;
   begin

      if Flat_Element_Kind (Enclosing_Element (Expr)) /= Arg_Kind then
         --  that is, in case of A and then B and then C we compute the
         --  complexity only once and for the "whole" short-circuit.

         Result := 1;

         Tmp := Short_Circuit_Operation_Left_Expression (Expr);

         while Flat_Element_Kind (Tmp) = Arg_Kind loop
            Tmp := Short_Circuit_Operation_Left_Expression (Tmp);
            Result := Result + 1;
         end loop;

      end if;

      return Result;
   end Control_Form_Complexity;

   ---------------------------
   --  Statement_Complexity --
   ---------------------------

   function Statement_Complexity (Stmt : Element) return Metric_Count is
      Result     : Metric_Count                := 0;
      Arg_Kind   : constant Flat_Element_Kinds := Flat_Element_Kind (Stmt);
   begin

      case Arg_Kind is

         when An_If_Statement              |
              A_Case_Statement             |
              A_Selective_Accept_Statement =>

            declare
               Paths : constant Element_List := Statement_Paths (Stmt);
            begin

               if Arg_Kind = An_If_Statement then
                  Result := Paths'Length;

                  if Flat_Element_Kind (Paths (Paths'Last)) = An_Else_Path then
                     Result := Result - 1;
                  end if;

               else
                  Result := Paths'Length - 1;
               end if;

            end;

         when A_While_Loop_Statement             |
              A_Timed_Entry_Call_Statement       |
              A_Conditional_Entry_Call_Statement |
              An_Asynchronous_Select_Statement   |
              A_For_Loop_Statement =>

            Result := 1;

         when An_Exit_Statement =>

            if not Is_Nil (Exit_Condition (Stmt)) then
               Result := 1;
            end if;

         when others =>
            null;
      end case;

      return Result;
   end Statement_Complexity;

   -----------------------------
   -- Compute_Syntaxt_Metrics --
   -----------------------------

   procedure Compute_Syntaxt_Metrics
     (Unit_Element :     Asis.Element;
      Counter      : out Syntax_Metric_Counter)
   is

      type Syntax_Count_State is record
         Unit_Nesting      : Metric_Count;
         Construct_Nesting : Metric_Count;
      end record;

      procedure Syntax_Metrics_Count_Pre_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Syntax_Count_State);

      procedure Syntax_Metrics_Count_Post_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Syntax_Count_State);

      procedure Collect_Syntax_Metrics is new Traverse_Element
        (Pre_Operation     => Syntax_Metrics_Count_Pre_Op,
         Post_Operation    => Syntax_Metrics_Count_Post_Op,
         State_Information => Syntax_Count_State);

      Control : Traverse_Control   := Continue;
      State   : Syntax_Count_State := (0, 0);

      procedure Syntax_Metrics_Count_Pre_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Syntax_Count_State)
      is
         El_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
         pragma Unreferenced (Control);
      begin

         if State.Unit_Nesting = 0 then
            --  We are in the top unit for that the metric counter is called,
            --  so:
            Counter.All_Declarations := Counter.All_Declarations + 1;

            Counter.Max_Program_Unit_Nesting :=
               Counter.Max_Program_Unit_Nesting + 1;
            --  When we count unit nesting level, we also count the unit for
            --  that the metric counting routine is called, so we subtract 1
            --  before reporting the metric

            Counter.Max_Construct_Nesting :=
               Counter.Max_Construct_Nesting + 1; --  ???

            State.Unit_Nesting := State.Unit_Nesting + 1;
            State.Construct_Nesting := State.Construct_Nesting + 1; --  ???

            return;
         else
            --  We are inside the unit, the normal metric counting

               if Adds_New_Nesting_Level (El_Kind) then
                  State.Construct_Nesting := State.Construct_Nesting + 1;

                  if State.Construct_Nesting >
                       Counter.Max_Construct_Nesting
                  then
                     Counter.Max_Construct_Nesting :=
                       State.Construct_Nesting;
                  end if;

               end if;

               if Is_RM_Program_Unit (Element) then
                  State.Unit_Nesting := State.Unit_Nesting + 1;

                  if State.Unit_Nesting >
                        Counter.Max_Program_Unit_Nesting
                  then
                     Counter.Max_Program_Unit_Nesting := State.Unit_Nesting;
                  end if;

               end if;

         end if;

         if El_Kind in Flat_Statement_Kinds
           and then
            El_Kind /= A_Terminate_Alternative_Statement
         then
            Counter.All_Statements := Counter.All_Statements + 1;

            if State.Unit_Nesting = 1 then
               Counter.Own_Statements := Counter.Own_Statements + 1;
            end if;

         end if;

         if El_Kind in Flat_Declaration_Kinds then
            Counter.All_Declarations := Counter.All_Declarations + 1;

            if State.Unit_Nesting = 1 then
               Counter.Own_Declarations := Counter.Own_Declarations + 1;
            end if;

         end if;

      end Syntax_Metrics_Count_Pre_Op;

      procedure Syntax_Metrics_Count_Post_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Syntax_Count_State)
      is
         pragma Unreferenced (Control);
      begin
         if Is_RM_Program_Unit (Element) then
            State.Unit_Nesting := State.Unit_Nesting - 1;
         end if;

         if Adds_New_Nesting_Level (Flat_Element_Kind (Element)) then
            State.Construct_Nesting := State.Construct_Nesting - 1;
         end if;
      end Syntax_Metrics_Count_Post_Op;

   begin --  Compute_Syntaxt_Metrics

      if not Is_Program_Unit (Unit_Element) then
         return;
      end if;

      Counter := Null_Syntax_Metric_Counter;

      Collect_Syntax_Metrics (Unit_Element, Control, State);
   end Compute_Syntaxt_Metrics;

   ---------------------
   -- Handled_Locally --
   ---------------------

   function Handled_Locally
     (Raise_Stmt : Asis.Element;
      Body_El    : Asis.Element)
      return       Boolean
   is
      Next_Frame         : Asis.Element := Enclosing_Element (Raise_Stmt);
      Exception_To_Catch : Asis.Element;
   begin

      Exception_To_Catch := Raised_Exception (Raise_Stmt);
      Exception_To_Catch := Normalize_Reference (Exception_To_Catch);
      Exception_To_Catch := Corresponding_Name_Definition (Exception_To_Catch);
      Exception_To_Catch := Unwind_Exception_Renamings (Exception_To_Catch);

      Look_For_Handler : while not Is_Equal (Next_Frame, Body_El) loop

         if Statement_Kind (Next_Frame) = A_Block_Statement then
            declare
               Handlers : constant Asis.Element_List :=
                 Block_Exception_Handlers (Next_Frame);
            begin
               for J in Handlers'Range loop
                  if Is_Handled (Exception_To_Catch, Handlers (J)) then
                     return True;
                  end if;
               end loop;
            end;
         end if;

         Next_Frame := Enclosing_Element (Next_Frame);
      end loop Look_For_Handler;

      return False;
   end Handled_Locally;

end ASIS_UL.Metrics.Compute;
