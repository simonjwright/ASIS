------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--          A S I S _ U L . M E T R I C S . D E F I N I T I O N S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

--  This package contains various definitions used by metric computing
--  routines.

package ASIS_UL.Metrics.Definitions is

   -----------------------
   -- Basic definitions --
   -----------------------

   Metric_Count_Disabled : constant := -2;
   --  Used to indicate the situation that the given metric should not be
   --  computed and reported

   Metric_Count_Undefined : constant := -1;
   --  Used to indicate the situation that the given metric should be
   --  computed and reported but has not been computed

   Max_Metric_Count : constant := Integer'Last;

   type Metric_Count is range Metric_Count_Disabled .. Max_Metric_Count;

   ------------------------
   -- Complexity metrics --
   ------------------------

   --  Complexity metrics are computed for executable bodies. (See
   --  ASIS_UL.Utilities.Is_Executable_Body as the test if an Element
   --  represents an executable body). The following metric values are
   --  computed:
   --
   --  - statement cyclomatic complexity (that is, cyclomatic complexity
   --    computed without taking into account short circuit control forms;
   --
   --  - expression cyclomatic complexity (that is, value added to cyclomatic
   --    complexity only by expressions - short circuit control forms,
   --    conditional expressions, quantified expressions);
   --
   --  - essential complexity;
   --
   --  - maximum loop nesting level in the body (note, that as a loop nesting
   --    level we are counting all the loops in the loop nest, that is, a loop
   --    statement that does not contain any nested level makes up a loop nest
   --    with nesting level one);

   type Complexity_Metric_Counter is record
      Statement_Complexity     : Metric_Count;
      Expression_Complexity    : Metric_Count;
      Essential_Complexity     : Metric_Count;
      Essential_Complexity_New : Metric_Count;
      --  Not used at the moment, is supposed to be used for McCabe essential
      --  complexity
      Max_Loop_Nesting         : Metric_Count;
      Extra_Exit_Points        : Metric_Count;
   end record;

   Treat_Exit_As_Goto : Boolean := True;
   --  This flag defines if loop exit statements should be treated as
   --  (non-structural) goto statements when computing essential complexity.

   Count_Static_Loop : Boolean := True;
   --  This flag defines if static FOR loops should be counted when computing
   --  cyclomatic complexity. If this flag is OFF they are not counted.

   Check_Predicates : Boolean := False;
   --  If this flag is ON, the computation of cyclomatic complexity takes into
   --  account the code in Pre- and Post-conditions and assertions. Otherwise
   --  this code is ignored.
   --
   --  At the moment this flag is always OFF and we do not have an option or
   --  any other means to set in ON. This may be revised if and when we decide
   --  that complexity metrics should consider the code in predicates and
   --  assertions.

   ---------------------
   -- Element metrics --
   ---------------------

   type Syntax_Metric_Counter is record
      All_Statements : Metric_Count := 0;
      --  The total number of statements;

      All_Declarations : Metric_Count := 0;
      --  The total number of declarations;

      Own_Statements : Metric_Count := 0;
      --  The number of statements from the given unit only, not counting
      --  statements from nested units;

      Own_Declarations : Metric_Count;
      --  The number of declarations from the given unit only, not counting
      --  declarations from nested units and the unit declaration itself;

      Max_Program_Unit_Nesting : Metric_Count := 0;
      --  Maximal number of nesting level of program units nested into the
      --  given element.

      Max_Construct_Nesting : Metric_Count := 0;
      --  Maximal number of construct nesting.

   end record;

   Null_Syntax_Metric_Counter : constant Syntax_Metric_Counter :=
     (others => 0);

end ASIS_UL.Metrics.Definitions;
