------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                     M E T R I C S . C O M P U T E                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2002-2017, AdaCore                     --
--                                                                          --
-- GNATMETRIC  is  free software; you can  redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either version 3, or (at your option)  any  later --
-- version. GNATMETRIC  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATMETRIC is maintained by AdaCore (http://www.adacore.com).            --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Text_IO;                 use Ada.Text_IO;

with Asis.Compilation_Units;      use Asis.Compilation_Units;
with Asis.Declarations;           use Asis.Declarations;
with Asis.Elements;               use Asis.Elements;
with Asis.Exceptions;             use Asis.Exceptions;
with Asis.Extensions;             use Asis.Extensions;
with Asis.Extensions.Flat_Kinds;  use Asis.Extensions.Flat_Kinds;
with Asis.Implementation;
with Asis.Iterator;               use Asis.Iterator;
with Asis.Text;                   use Asis.Text;

with GNAT.Table;

with ASIS_UL.Metrics.Compute;     use ASIS_UL.Metrics.Compute;
with ASIS_UL.Metrics.Definitions; use ASIS_UL.Metrics.Definitions;
with ASIS_UL.Output;              --  use ASIS_UL.Output;
with ASIS_UL.Utilities;           use ASIS_UL.Utilities;

with METRICS.ASIS_Utilities;      use METRICS.ASIS_Utilities;
with METRICS.Common;              use METRICS.Common;
with METRICS.Contracts;           use METRICS.Contracts;
with METRICS.Coupling;            use METRICS.Coupling;
with METRICS.Metric_Definitions;  use METRICS.Metric_Definitions;
with METRICS.Options;             use METRICS.Options;
with METRICS.Output;              use METRICS.Output;
with METRICS.Source_Table;        use METRICS.Source_Table;

package body METRICS.Compute is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Compute_Line_Metrics
     (SF :   SF_Id   := No_SF_Id;
      El :   Element := Nil_Element)
      return Line_Metrics_Record;
   --  Computes line metrics for its argument. If SF is not No_SF_Id, then
   --  the line metrics are computed for the whole source file, otherwise
   --  they are computed for the Element argument (in case of Nil_Element or if
   --  not Is_Text_Available (El) zero is returned as a computed value for all
   --  the line metrics

   type Line_Kinds is (
     Blank_Line,        --  contains only space and HT (or nothing)
     Comment_Line,      --  contains comment text only
     Code_Line,         --  contains at least one character of non-comment code
     EOL_Comment_Line); --  End-of-line comment: contains both code and comment

   function Line_Kind (Line : Asis.Text.Line) return Line_Kinds;
   --  Scans the line image and detects an appropriate Line_Kinds value.

   function Line_Image_Kind (Line_Img : Program_Text) return Line_Kinds;
   --  Scans a line image and returns Blank_Line if this is a blank line,
   --  Comment_Line if the image is an Ada comment and Comment_Line otherwise.
   --  Never returns EOL_Comment_Line!

   package Line_Kinds_Table is new GNAT.Table (
      Table_Component_Type => Line_Kinds,
      Table_Index_Type     => Asis.Text.Line_Number_Positive,
      Table_Low_Bound      => 1,
      Table_Initial        => 1_000,
      Table_Increment      => 100);

   procedure File_Line_Kinds_Table (Line_List : Asis.Text.Line_List);
   --  Files Line_Kinds_Table assuming that the argument line list is a line
   --  list corresponding to some Compilation_Unit

   function Is_White_Space (W_Ch : Wide_Character) return Boolean;
   --  Checks if the argument id a white space.

   procedure Compute_And_Report_Element_Metrics
     (Program_Unit         : Element;
      Nesting_Level        : Natural := 0;
      SF                   : SF_Id   := No_SF_Id;
      Include_Nested_Units : Boolean := True);
   --  Computes all possible metrics for its argument Program_Unit.
   --  Nesting_Level sets the indentation level for the output. If SF is not
   --  equal to No_SF_Id (and if Program_Unit represents the library item from
   --  the corresponding source file) , this means that some element metrics
   --  should be stored in source file table as the data to be used to form the
   --  global statistics. If Include_Nested_Units is set ON, metrics are
   --  computed and reported for all the enclosing program units.
   --
   --  Metrics are computed for ASIS Elements representing program units, for
   --  more details see the body of Is_Program_Unit test function which makes
   --  the corresponding check

   procedure Compute_Complexity
     (Body_El : Element;
      Depth   : Natural);
   --  Computes and prints out the cyclomatic and loop nesting complexity for
   --  Body_El. This information is outputted into the out file which is
   --  specific for the source being analyzed. The Depth parameter is used to
   --  format the output.

   --  Identified complexity to report on file level
   type Complexity_Type is
     (Statement_Complexity,
      Expression_Complexity,
      Cyclomatic_Complexity,
      Essential_Complexity,
      Max_Loop_Nesting);

   type Complexity_Values is array (Complexity_Type) of Float;
   --  Type to contains sum of complexity of every unit in the file per
   --  complexity type. Maximum is kept for loop nesting
   type Complexity_To_Print is array (Complexity_Type) of Real_Val_To_Print;
   --  Type to contains printable average values of complexities.
   type Complexity_To_Report is array (Complexity_Type) of Boolean;
   --  Type that indicates if a complexity has to be processed and reported.
   --  Those values are set according to switches.

   type File_Level_Complexity_Metrics_Record is record
      Totals                 : Complexity_Values;
      --  Sum of all unit complexities, except for Max Loop Nesting where it
      --  is the maximum.
      Total_Executable_Units : Natural;
      --  Total number of executable units (unit for which complexity is
      --  processed), contained in the current processed file.
   end record;

   Is_Complexity_reported : Complexity_To_Report;
   --  Is used in Report_File_Procedure.
   File_Complexity_Metrics : File_Level_Complexity_Metrics_Record;

   procedure Initialize_File_Complexity_Metrics;
   --  Reset all values from File_Complexity_Metrics record to 0.
   --  Set values for Is_Complexity_reported according to the switches.
   --  Called for each file.

   procedure Increase_File_Complexity_Metrics
     (Complexity_Metrics : Complexity_Metric_Counter);
   --  Increase values from File_Complexity_Metrics:
   --  take the maximum for loop nesting,
   --  sum with the new values for the others,
   --  increase of 1 the Total_Executable_Units for average.

   procedure Report_File_Complexity;
   --  Process average for values in File_Complexity_Metrics,
   --  except for loop nesting, that will be reported.
   --  Then add information to text and XML report.
   --  This method is call only if Total_Executable_Units is
   --  not equal to 0.

   ----------------------------------------
   -- Compute_And_Report_Element_Metrics --
   ----------------------------------------

   procedure Compute_And_Report_Element_Metrics
     (Program_Unit         : Element;
      Nesting_Level        : Natural := 0;
      SF                   : SF_Id   := No_SF_Id;
      Include_Nested_Units : Boolean := True)
   is
      Syntax_Metrics  : Syntax_Metric_Counter;
      Line_Metrics    : Line_Metrics_Record    := Zero_Line_Metrics;

      Public_Subprogram_Count          : Metric_Count     := 0;
      Public_Subprogram_Contract_Count : Contract_Details :=
        Zero_Contract_Details;

      Subprogram_Body_Count   : Metric_Count := 0;

      Public_Types_Count          : Metric_Count := 0;
      Public_Types_Detailed_Count : Public_Types_Details
        := Zero_Public_Types_Details;
      All_Types_Count             : Metric_Count := 0;

      Body_Span   : Span;
      First_Line  : Line_Number_Positive;
      Last_Line   : Line_Number_Positive;
      Count_Lines : Boolean := False;
      --  Used to count average process line numbers

      LSLOC : Metric_Count := 0;
      --  Used to compute the logical SLOC (all declarations + all statement)

      Is_Library_Item       : Boolean := False;

      --  Now we define two traversals needed to collect metrics. The first
      --  traversal analyzes the argument unit only, without getting into any
      --  nested program unit. When this traversal is completed and the
      --  corresponding report is generated, we start the second traversal
      --  in order to collect the detailed metrics for nested program units.
      --  This second traversal looks for the elements being program units for
      --  which metrics should be collected and recursively calls this
      --  Compute_And_Report_Element_Metrics procedure for them. This is not
      --  very effective but this allows to avoid using complicated
      --  data structures for organizing the well-structured output. The
      --  goal is to output all the metrics for a program unit first, and only
      --  after that to output metrics for its enclosed units

      type Global_State_Type is record
         Program_Unit_Nesting : Metric_Count;
         Construct_Nesting    : Metric_Count;
         Process_Nesting      : Natural;
      end record;

      type Local_State_Type is record
         Top_Unit : Boolean;
         --  Is use as a flag to prevent a second processing of the same
         --  Program_Unit element

         Depth    : Natural;
         --  Sets the indentation for the output
      end record;

      Global_State   : Global_State_Type :=
        (Program_Unit_Nesting => 0,
         Construct_Nesting    => 0,
         Process_Nesting      => 0);

      Local_State    : Local_State_Type;
      Control        : Traverse_Control := Continue;

      function Needs_Param_Counting (Unit : Element) return Boolean;
      --  Assumes that Unit is a program unit checks if parameter count metric
      --  should be computed for it

      procedure Compute_And_Report_Params_Num
        (Unit  : Asis.Element;
         Depth : Natural);
      --  Computes and reports parameter number metris. Unit should be an
      --  appropriate element for this metric. Depth should indicate the
      --  nesting level.

      procedure First_Pre_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Global_State_Type);
      --  Computes and collects the following metrics:
      --
      --  - syntax metrics specific for a library unit
      --  - data for average lines in bodies metric

      procedure First_Post_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Global_State_Type);

      procedure Collect_Global_Metrics is new Traverse_Element
        (Pre_Operation     => First_Pre_Operation,
         Post_Operation    => First_Post_Operation,
         State_Information => Global_State_Type);
      --  This procedure computes metrics for the argument element only, it
      --  does not go into nested units, if any. It collects data for average
      --  lines in body metric, and in case if its argument corresponds to a
      --  library unit, it computes the library-unit-specific syntax metrics.

      procedure Second_Pre_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Local_State_Type);
      --  This procedure detects for which Elements being visited all the
      --  detailed metrics should be computed and then calls the
      --  Compute_And_Report_Element_Metrics procedure for them

      procedure Second_Post_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Local_State_Type);

      procedure Collect_Local_Metrics is new Traverse_Element
        (Pre_Operation     => Second_Pre_Operation,
         Post_Operation    => Second_Post_Operation,
         State_Information => Local_State_Type);
      --  This procedure makes the second traversal in order to compute and to
      --  print out metrics for all the enclosed program units

      --------------------------
      -- Needs_Param_Counting --
      --------------------------

      function Needs_Param_Counting (Unit : Element) return Boolean is
      begin
         return
           Compute_Spb_Pars_Num
         and then
          (Declaration_Kind (Unit) in A_Procedure_Declaration      |
                                      A_Null_Procedure_Declaration |
                                      A_Function_Declaration       |
                                      A_Procedure_Instantiation    |
                                      A_Function_Instantiation
         or else
           (Declaration_Kind (Unit) in
              A_Procedure_Body_Declaration       |
              A_Function_Body_Declaration        |
              An_Expression_Function_Declaration |
              A_Procedure_Body_Stub              |
              A_Function_Body_Stub
           and then
            Acts_As_Spec (Unit)));

      end Needs_Param_Counting;

      -----------------------------------
      -- Compute_And_Report_Params_Num --
      -----------------------------------

      procedure Compute_And_Report_Params_Num
        (Unit  : Asis.Element;
         Depth : Natural)
      is
         Arg : constant Asis.Element :=
           (if Declaration_Kind (Unit) in A_Procedure_Instantiation |
                                          A_Function_Instantiation
            then
               Corresponding_Declaration (Unit)
            else
               Unit);

         Param_Num_Count :          Param_Num := Zero_Param_Num;
         Params          : constant Asis.Element_List :=
           Parameter_Profile (Arg);

         N : Metric_Count;
      begin
         for J in Params'Range loop
            N := Metric_Count (Names (Params (J))'Length);

            case Mode_Kind (Params (J)) is
               when Not_A_Mode => null;
               when A_Default_In_Mode |
                    An_In_Mode        =>
                  Param_Num_Count.In_Params :=
                    Param_Num_Count.In_Params + N;
               when An_Out_Mode =>
                  Param_Num_Count.Out_Params :=
                    Param_Num_Count.Out_Params + N;
               when An_In_Out_Mode =>
                  Param_Num_Count.In_Out_Params :=
                    Param_Num_Count.In_Out_Params + N;
            end case;
         end loop;

         Param_Num_Count.All_Params := Param_Num_Count.In_Params +
           Param_Num_Count.Out_Params + Param_Num_Count.In_Out_Params;

         Report
           ("all parameters           :" & Param_Num_Count.All_Params'Img,
            Depth => Depth + 1);

         Output_XML_Metric
           ("all_parameters",
            Param_Num_Count.All_Params,
            Depth => Depth + 3);

         if Param_Num_Count.All_Params > 0 then
            Report
              ("IN parameters         :" & Param_Num_Count.In_Params'Img,
               Depth => Depth + 2);
            Output_XML_Metric
              ("in_parameters",
               Param_Num_Count.In_Params,
               Depth => Depth + 4);

            Report
              ("OUT parameters        :" & Param_Num_Count.Out_Params'Img,
               Depth => Depth + 2);
            Output_XML_Metric
              ("out_parameters",
               Param_Num_Count.Out_Params,
               Depth => Depth + 4);

            Report
              ("IN OUT parameters     :" & Param_Num_Count.In_Out_Params'Img,
               Depth => Depth + 2);
            Output_XML_Metric
              ("in_out_parameters",
               Param_Num_Count.In_Out_Params,
               Depth => Depth + 4);

         end if;
      end Compute_And_Report_Params_Num;

      -------------------------
      -- First_Pre_Operation --
      -------------------------

      procedure First_Pre_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Global_State_Type)
      is
         El_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
         pragma Unreferenced (Control);

      begin

         if Compute_Average_Lines_In_Bodies
           and then
            SF /= No_SF_Id
            --  This is needed to count this metric only once - for the library
            --  item or the proper body of subunit only
         then

            Count_Lines := False;

            if Is_Process (El_Kind) then
               State.Process_Nesting   := State.Process_Nesting + 1;
               Num_Of_Processes_Bodies := Num_Of_Processes_Bodies + 1;

               if State.Process_Nesting = 1 then

                  Count_Lines := True;
                  Body_Span   := Element_Span (Element);
                  First_Line  := Body_Span.First_Line;
                  Last_Line   := Body_Span.Last_Line;

               end if;

            elsif El_Kind = A_Package_Body_Declaration then
               --  For this metric, we count only package bodies that have
               --  non-empty statement sequences.

               --  We assume, that a statement sequence in a package body
               --  cannot contain a block statement that defines a program
               --  unit (if this is not the case, the code of a program unit
               --  defined in a block statement in a statement sequence of a
               --  package body will be counted twice, but this case really
               --  looks pathological!

               declare
                  Stmts : constant Asis.Element_List :=
                    Body_Statements (Element, True);

               begin

                  if not Is_Nil (Stmts) then
                     Count_Lines             := True;
                     Num_Of_Processes_Bodies := Num_Of_Processes_Bodies + 1;

                     First_Line :=
                       Element_Span (Stmts (Stmts'First)).First_Line;

                     Last_Line := Element_Span (Stmts (Stmts'Last)).Last_Line;
                  end if;
               end;

            end if;

            if Count_Lines then

               for J in First_Line .. Last_Line loop

                  if Line_Kinds_Table.Table (J) in
                       Code_Line .. EOL_Comment_Line
                  then
                     Lines_In_Process_Bodies :=
                       Lines_In_Process_Bodies + 1;
                  end if;

               end loop;

            end if;

         end if;

         case El_Kind is

            when Flat_Declaration_Kinds =>

               if Is_Library_Item            and then
                  May_Have_Subprogram_Bodies and then

                  (El_Kind = A_Procedure_Body_Declaration
                  or else
                   El_Kind = A_Function_Body_Declaration)

               then
                  Subprogram_Body_Count := Subprogram_Body_Count + 1;
               end if;

               if Is_Library_Item
                 and then
                  May_Have_Public_Subprograms
                 and then
                  (El_Kind = A_Procedure_Declaration         or else
                   El_Kind = A_Function_Declaration          or else
                   El_Kind = A_Generic_Procedure_Declaration or else
                   El_Kind = A_Generic_Function_Declaration)
                 and then
                   not Is_Private (Element)
               then
                  --  Note, that this condition does not allow to count the
                  --  library level subprogram body which acts as a spec
                  --  as an public subprogram. So we have to do this
                  --  outside the traversal.
                  Public_Subprogram_Count := Public_Subprogram_Count + 1;

                  if Contract_Metrics_Set then
                     Count_Contract_Info
                        (Element,
                         Public_Subprogram_Contract_Count);
                  end if;
               end if;

               if Is_Library_Item       and then
                  May_Have_Public_Types and then
                  not Is_Private (Element)
               then

                  Public_Types_Count := Public_Types_Count + 1;

                  case El_Kind is
                     when A_Task_Type_Declaration =>
                        Public_Types_Detailed_Count.Task_Types :=
                          Public_Types_Detailed_Count.Task_Types + 1;

                     when A_Protected_Type_Declaration =>
                        Public_Types_Detailed_Count.Protected_Types :=
                          Public_Types_Detailed_Count.Protected_Types + 1;

                     when A_Private_Type_Declaration |
                          A_Private_Extension_Declaration =>
                        Public_Types_Detailed_Count.Private_Types :=
                          Public_Types_Detailed_Count.Private_Types + 1;

                        if Trait_Kind (Element) in
                           An_Abstract_Trait ..
                           An_Abstract_Limited_Private_Trait
                        then
                           Public_Types_Detailed_Count.Abstract_Types :=
                             Public_Types_Detailed_Count.Abstract_Types + 1;
                        end if;

                        if El_Kind = A_Private_Type_Declaration and then
                           Definition_Kind (Type_Declaration_View (Element)) =
                           A_Tagged_Private_Type_Definition
                        then
                           Public_Types_Detailed_Count.Tagged_Types :=
                             Public_Types_Detailed_Count.Tagged_Types + 1;
                        end if;

                     when An_Ordinary_Type_Declaration =>

                        if Trait_Kind (Element) = An_Abstract_Trait then
                           Public_Types_Detailed_Count.Abstract_Types :=
                             Public_Types_Detailed_Count.Abstract_Types + 1;
                        end if;

                        if Type_Kind (Type_Declaration_View (Element)) =
                           A_Tagged_Record_Type_Definition
                        then
                           Public_Types_Detailed_Count.Tagged_Types :=
                             Public_Types_Detailed_Count.Tagged_Types + 1;
                        end if;

                     when others =>
                        --  It's not a type to count
                        Public_Types_Count := Public_Types_Count - 1;
                  end case;

               end if;

               if Is_Library_Item and then
                  May_Have_Type_Definitions
               then

                  case El_Kind is
                     when A_Task_Type_Declaration         |
                          A_Protected_Type_Declaration    |
                          A_Private_Type_Declaration      |
                          A_Private_Extension_Declaration =>

                        All_Types_Count := All_Types_Count + 1;

                     when An_Ordinary_Type_Declaration =>

                        --  We do not count the full declarations for private
                        --  types

                        if not (Is_Private (Element) and then
                           Declaration_Kind
                             (Corresponding_Type_Declaration (Element)) in
                           A_Private_Type_Declaration ..
                           A_Private_Extension_Declaration)
                        then
                           All_Types_Count := All_Types_Count + 1;
                        end if;

                     when others =>
                        null;
                  end case;
               end if;

            when others =>
               null;
         end case;

      end First_Pre_Operation;

      --------------------------
      -- First_Post_Operation --
      --------------------------

      procedure First_Post_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Global_State_Type)
      is
      begin

         pragma Unreferenced (Control);

         if Compute_Average_Lines_In_Bodies
           and then
            SF /= No_SF_Id
           and then
            Is_Process (Flat_Element_Kind (Element))
         then
            State.Process_Nesting := State.Process_Nesting - 1;
         end if;

      end First_Post_Operation;

      --------------------------
      -- Second_Pre_Operation --
      --------------------------

      procedure Second_Pre_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Local_State_Type)
      is
      begin

         if State.Top_Unit then
            State.Top_Unit := False;
            return;
         end if;

         if Is_Program_Unit (Element) then

            Compute_And_Report_Element_Metrics
              (Program_Unit => Element,
               Nesting_Level => State.Depth);

            Control := Abandon_Children;
         elsif Needs_Param_Counting (Element) then
            Report ("");
            Report_Program_Unit
              (Element,
               Depth => State.Depth,
               Library_Item => False);

            Compute_And_Report_Params_Num
              (Unit =>  Element,
               Depth => State.Depth);

            Close_Tag ("unit", State.Depth);
         end if;

      end Second_Pre_Operation;

      ---------------------------
      -- Second_Post_Operation --
      ---------------------------

      procedure Second_Post_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Local_State_Type)
      is
      begin
         pragma Unreferenced (Element);
         pragma Unreferenced (Control);
         pragma Unreferenced (State);

         null;

      end Second_Post_Operation;

   begin  --  Compute_And_Report_Element_Metrics

      if not Is_Program_Unit (Program_Unit) then
         return;
      end if;

      --  Starting the first traversal:

      Is_Library_Item :=
        SF /= No_SF_Id and then Is_Equal (Program_Unit, The_Unit);

      if Unit_Metrics_Set then
         Report ("");
         Report_Program_Unit (Program_Unit, Nesting_Level, Is_Library_Item);
      end if;

      --  Compute and report line metrics:

      Line_Metrics := Compute_Line_Metrics (El => Program_Unit);

      if Line_Metrics_Set then
         Report ("");
         Report ("=== Code line metrics ===", Depth => Nesting_Level);

         if Compute_All_Lines then
            Report
              ("all lines           :" & Line_Metrics.All_Lines'Img,
               Depth => Nesting_Level + 1);

            Output_XML_Metric
              ("all_lines",
               Line_Metrics.All_Lines,
               Depth => Nesting_Level + 3);
         end if;

         if Compute_Code_Lines then
            Report
              ("code lines          :" & Line_Metrics.Code_Lines'Img,
               Depth => Nesting_Level + 1);

            Output_XML_Metric
              ("code_lines",
               Line_Metrics.Code_Lines,
               Depth => Nesting_Level + 3);
         end if;

         if Compute_Comment_Lines then
            Report
              ("comment lines       :" & Line_Metrics.Comment_Lines'Img,
               Depth => Nesting_Level + 1);

            Output_XML_Metric
              ("comment_lines",
               Line_Metrics.Comment_Lines,
               Depth => Nesting_Level + 3);
         end if;

         if Compute_EOL_Comments then
            Report
              ("end-of-line comments:" & Line_Metrics.EOL_Comments'Img,
               Depth => Nesting_Level + 1);

            Output_XML_Metric
              ("eol_comments",
               Line_Metrics.EOL_Comments,
               Depth => Nesting_Level + 3);
         end if;

         if Compute_Comment_Code_Ratio then
            Comment_Code_Ratio :=
                (Float (Line_Metrics.Comment_Lines) +
                   Float (Line_Metrics.EOL_Comments))
                /

                 (Float (Line_Metrics.Comment_Lines) +
                    Float (Line_Metrics.Code_Lines))
                *
                  100.0;

            Comment_Code_Ratio_To_Print :=
              Comment_Code_Percentage (Comment_Code_Ratio);

            Report
              ("comment percentage  :" & Comment_Code_Ratio_To_Print'Img,
               Depth => Nesting_Level + 1);

            Output_XML_Metric
              ("comment_percentage",
               Comment_Code_Ratio_To_Print'Img,
               Depth => Nesting_Level + 3);

         end if;

         if Compute_Blank_Lines then
            Report
              ("blank lines         :" & Line_Metrics.Blank_Lines'Img,
               Depth => Nesting_Level + 1);

            Output_XML_Metric
              ("blank_lines",
               Line_Metrics.Blank_Lines,
               Depth => Nesting_Level + 3);
         end if;

      end if;

      --  Compute and report element metrics:

      if Compute_All_Statements
        or else
         Compute_All_Declarations
        or else
         Compute_Progam_Unit_Nesting
        or else
         Compute_Construct_Nesting
        or else
         Compute_Average_Lines_In_Bodies
        or else
         (Is_Library_Item and then
          (Compute_Public_Subprograms or else
           Compute_All_Subprograms    or else
           Compute_Public_Types       or else
           Compute_All_Types))
        or else
         Needs_Param_Counting (Program_Unit)
      then

         Collect_Global_Metrics (Program_Unit, Control, Global_State);
         Compute_Syntaxt_Metrics (Program_Unit, Syntax_Metrics);

         if Is_Library_Item and then
            CU_Class = A_Public_Declaration_And_Body
         then
            Public_Subprogram_Count := 1;

            if Contract_Metrics_Set then
               Count_Contract_Info
                  (Program_Unit,
                   Public_Subprogram_Contract_Count);
            end if;
         end if;

         if Element_Metrics_Set then

            Report ("");
            Report ("=== Element metrics ===", Depth => Nesting_Level);

            if Is_Library_Item       and then
               May_Have_Public_Types and then
               Public_Types_Count > 0
            then
               Report
                 ("public types             :" &
                  Public_Types_Count'Img,
                  Depth => Nesting_Level + 1);

               Output_XML_Metric
                 ("public_types",
                  Public_Types_Count,
                  Depth => Nesting_Level + 3);

               if Details_Present (Public_Types_Detailed_Count) then

                  Report ("   including", Depth => Nesting_Level + 1);

                  if Public_Types_Detailed_Count.Abstract_Types > 0 then
                     Report
                       ("abstract types  :" &
                        Public_Types_Detailed_Count.Abstract_Types'Img,
                        Depth => Nesting_Level + 2);

                     Output_XML_Metric
                       ("abstract_types",
                        Public_Types_Detailed_Count.Abstract_Types,
                        Depth => Nesting_Level + 3);
                  end if;

                  if Public_Types_Detailed_Count.Tagged_Types > 0 then
                     Report
                       ("tagged types    :" &
                        Public_Types_Detailed_Count.Tagged_Types'Img,
                        Depth => Nesting_Level + 2);

                     Output_XML_Metric
                       ("tagged_types",
                        Public_Types_Detailed_Count.Tagged_Types,
                        Depth => Nesting_Level + 3);
                  end if;

                  if Public_Types_Detailed_Count.Private_Types > 0 then
                     Report
                       ("private types   :" &
                        Public_Types_Detailed_Count.Private_Types'Img,
                        Depth => Nesting_Level + 2);

                     Output_XML_Metric
                       ("private_types",
                        Public_Types_Detailed_Count.Private_Types,
                        Depth => Nesting_Level + 3);
                  end if;

                  if Public_Types_Detailed_Count.Task_Types > 0 then
                     Report
                       ("task types      :" &
                        Public_Types_Detailed_Count.Task_Types'Img,
                        Depth => Nesting_Level + 2);

                     Output_XML_Metric
                       ("task_types",
                        Public_Types_Detailed_Count.Task_Types,
                        Depth => Nesting_Level + 3);
                  end if;

                  if Public_Types_Detailed_Count.Protected_Types > 0 then
                     Report
                       ("protected types :" &
                        Public_Types_Detailed_Count.Protected_Types'Img,
                        Depth => Nesting_Level + 2);

                     Output_XML_Metric
                       ("protected_types",
                        Public_Types_Detailed_Count.Protected_Types,
                        Depth => Nesting_Level + 3);
                  end if;

               end if;

               Global_Statistics.Public_Types :=
                 Global_Statistics.Public_Types + Public_Types_Count;

               Add_Public_Types_Details (Public_Types_Detailed_Count);

               Global_Statistics.Computed_Public_Types :=
                 Global_Statistics.Computed_Public_Types + 1;

            end if;

         end if;

         if Is_Library_Item           and then
            May_Have_Type_Definitions and then
            All_Types_Count > 0
         then
            Report
              ("all type definitions     :" &
               All_Types_Count'Img,
               Depth => Nesting_Level + 1);

            Output_XML_Metric
              ("all_types",
               All_Types_Count,
               Depth => Nesting_Level + 3);

            Global_Statistics.All_Types :=
              Global_Statistics.All_Types + All_Types_Count;

            Global_Statistics.Computed_All_Types :=
              Global_Statistics.Computed_All_Types + 1;

         end if;

         if Is_Library_Item            and then
            Compute_Public_Subprograms and then
            May_Have_Public_Subprograms
         then
            Report
              ("public subprograms       :" &
               Public_Subprogram_Count'Img,
               Depth => Nesting_Level + 1);

               Output_XML_Metric
                 ("public_subprograms",
                  Public_Subprogram_Count,
                  Depth => Nesting_Level + 3);

            if Public_Subprogram_Count > 0 then
               Global_Statistics.Public_Subprograms :=
                 Global_Statistics.Public_Subprograms +
                 Public_Subprogram_Count;

               Global_Statistics.Computed_Public_Subprograms :=
                 Global_Statistics.Computed_Public_Subprograms + 1;
            end if;

         end if;

         if Is_Library_Item  and then
            May_Have_Subprogram_Bodies
         then
            Report
              ("all subprogram bodies    :" &
               Subprogram_Body_Count'Img,
               Depth => Nesting_Level + 1);

               Output_XML_Metric
                 ("all_subprograms",
                  Subprogram_Body_Count,
                  Depth => Nesting_Level + 3);

            if Subprogram_Body_Count > 0 then
               Global_Statistics.All_Subprograms :=
                 Global_Statistics.All_Subprograms +
                 Subprogram_Body_Count;

               Global_Statistics.Computed_All_Subprograms :=
                 Global_Statistics.Computed_All_Subprograms + 1;
            end if;
         end if;

         if Compute_All_Statements then
            Report
              ("all statements           :" &
               Syntax_Metrics.All_Statements'Img,
               Depth => Nesting_Level + 1);

               Output_XML_Metric
                 ("all_stmts",
                  Syntax_Metrics.All_Statements,
                  Depth => Nesting_Level + 3);

            if Is_Library_Item then
               --  Collect the global statistics for all the units being
               --  processed

               Global_Statistics.Syntax_Metrics.All_Statements :=
                 Global_Statistics.Syntax_Metrics.All_Statements +
                 Syntax_Metrics.All_Statements;
            end if;

         end if;

         if Compute_All_Declarations then
            Report
              ("all declarations         :" &
               Syntax_Metrics.All_Declarations'Img,
               Depth => Nesting_Level + 1);

               Output_XML_Metric
                 ("all_dcls",
                  Syntax_Metrics.All_Declarations,
                  Depth => Nesting_Level + 3);

            if Is_Library_Item then
               --  Collect the global statistics for all the units being
               --  processed

               Global_Statistics.Syntax_Metrics.All_Declarations :=
                 Global_Statistics.Syntax_Metrics.All_Declarations +
                 Syntax_Metrics.All_Declarations;
            end if;

         end if;

         if Compute_All_Statements and then Compute_All_Declarations then
            LSLOC :=
               Syntax_Metrics.All_Declarations +
               Syntax_Metrics.All_Statements;

            Report
              ("logical SLOC             :" & LSLOC'Img,
               Depth => Nesting_Level + 1);

               Output_XML_Metric
                 ("lsloc",
                  LSLOC,
                  Depth => Nesting_Level + 3);
         end if;

         if Compute_Progam_Unit_Nesting
           and then
            Syntax_Metrics.Max_Program_Unit_Nesting > 1
         then
            Syntax_Metrics.Max_Program_Unit_Nesting :=
              Syntax_Metrics.Max_Program_Unit_Nesting - 1;

            Report
              ("maximal unit nesting     :" &
               Syntax_Metrics.Max_Program_Unit_Nesting'Img,
               Depth => Nesting_Level + 1);

               Output_XML_Metric
                 ("unit_nesting",
                  Syntax_Metrics.Max_Program_Unit_Nesting,
                  Depth => Nesting_Level + 3);
         end if;

         if Compute_Construct_Nesting then

            Report
              ("maximal construct nesting:" &
               Syntax_Metrics.Max_Construct_Nesting'Img,
               Depth => Nesting_Level + 1);

               Output_XML_Metric
                 ("construct_nesting",
                  Syntax_Metrics.Max_Construct_Nesting,
                  Depth => Nesting_Level + 3);
         end if;

         if Is_Library_Item then
            Global_Statistics.Computed_Element_Metrics :=
              Global_Statistics.Computed_Element_Metrics + 1;
         end if;

         if Needs_Param_Counting (Program_Unit) then
            Compute_And_Report_Params_Num
              (Unit =>  Program_Unit,
               Depth => Nesting_Level);

         end if;

      end if;

      --  Compute and report complexity:

      if Is_Executable_Body (Program_Unit)
        and then
         (Complexity_Metrics_Set
         or else
          Compute_Average_Complexity)
      then
         Compute_Complexity (Program_Unit, Depth => Nesting_Level);
      end if;

      --  And now, if needed we go down in the argument structure

      if Include_Nested_Units then
         Local_State.Top_Unit := True;
         Local_State.Depth := Nesting_Level + 1;
         Collect_Local_Metrics (Program_Unit, Control, Local_State);
      end if;

      if Unit_Metrics_Set then
         Close_Tag ("unit", Nesting_Level + 2);
      end if;

   exception

   --  In case of an ASIS exception we report it and go ahead with computing
   --  other metrics for other sources. Otherwise we let the exception
   --  propagate out of this routine to raise Fatal_Error at upper level
      when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
                Asis.Exceptions.ASIS_Inappropriate_Container        |
                Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
                Asis.Exceptions.ASIS_Inappropriate_Element          |
                Asis.Exceptions.ASIS_Inappropriate_Line             |
                Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
                Asis.Exceptions.ASIS_Failed                         =>

         ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);

         if SF /= No_SF_Id then
            Set_Source_Status (SF, Error_Detected);
         end if;

         Asis.Implementation.Set_Status;
   end Compute_And_Report_Element_Metrics;

   ------------------------
   -- Compute_Complexity --
   ------------------------

   procedure Compute_Complexity
     (Body_El : Element;
      Depth   : Natural)
   is
      Cyclomatic_Complexity : Metric_Count := 0;
      --  Here we have to compute the McCabe cyclomatic complexity

      Complexity_Metrics : Complexity_Metric_Counter;
      --  Here we collect all the complexity metrics

   begin  --  Compute_Complexity

      Compute_Complexity_Metrics (Body_El, Complexity_Metrics);

      Cyclomatic_Complexity :=
        Complexity_Metrics.Statement_Complexity +
        Complexity_Metrics.Expression_Complexity;

      if Compute_Average_Complexity then
         Units_Compute_Average_Complexity_For :=
           Units_Compute_Average_Complexity_For + 1;
         Total_Cyclomatic_Complexity :=
           Total_Cyclomatic_Complexity + Cyclomatic_Complexity;
      end if;

      if Complexity_Metrics_Set then

         Increase_File_Complexity_Metrics (Complexity_Metrics);

         Report ("");
         Report ("=== Complexity metrics ===", Depth => Depth);

         if Compute_Cyclomatic_Complexity then

            Report ("statement complexity     :" &
                    Complexity_Metrics.Statement_Complexity'Img,
                    Depth => Depth + 1);

            Output_XML_Metric
              ("statement_complexity",
               Complexity_Metrics.Statement_Complexity,
               Depth => Depth + 3);

            Report ("expression complexity    :" &
                    Complexity_Metrics.Expression_Complexity'Img,
                    Depth => Depth + 1);

            Output_XML_Metric
              ("expression_complexity",
               Complexity_Metrics.Expression_Complexity,
               Depth => Depth + 3);

            Report ("cyclomatic complexity    :" & Cyclomatic_Complexity'Img,
                     Depth => Depth + 1);

            Output_XML_Metric
              ("cyclomatic_complexity",
               Cyclomatic_Complexity,
               Depth => Depth + 3);

         end if;

         if Compute_Essential_Complexity then
            Report ("essential complexity     :" &
                    Complexity_Metrics.Essential_Complexity'Img,
                    Depth => Depth + 1);

            Output_XML_Metric
              ("essential_complexity",
               Complexity_Metrics.Essential_Complexity,
               Depth => Depth + 3);
         end if;

         if Compute_Loop_Nesting then
            Report
              ("maximum loop nesting     :" &
               Complexity_Metrics.Max_Loop_Nesting'Img,
               Depth => Depth + 1);

            Output_XML_Metric
              ("max_loop_nesting",
               Complexity_Metrics.Max_Loop_Nesting,
               Depth => Depth + 3);
         end if;

         if Compute_Extra_Exit_Points then
            Report
              ("extra exit points        :" &
               Complexity_Metrics.Extra_Exit_Points'Img,
               Depth => Depth + 1);

            Output_XML_Metric
              ("extra_exit_points",
               Complexity_Metrics.Extra_Exit_Points,
               Depth => Depth + 3);
         end if;

      end if;

   exception
      when Ex : others =>
         Report
           ("Failed to compute complexity",
           Depth => Depth + 1);
         ASIS_UL.Output.Report_Unhandled_Exception (Ex);
   end Compute_Complexity;

   --------------------------
   -- Compute_Line_Metrics --
   --------------------------

   function Compute_Line_Metrics
     (SF :   SF_Id   := No_SF_Id;
      El :   Element := Nil_Element)
      return Line_Metrics_Record
   is
      Result   : Line_Metrics_Record := Zero_Line_Metrics;
      The_Span : Span;
   begin

      if not Line_Metrics_Set or else
         (SF = No_SF_Id
            and then
         (Is_Nil (El) or else not Is_Text_Available (El)))
      then
         return Zero_Line_Metrics;
      end if;

      if SF /= No_SF_Id then
         The_Span := Compilation_Span (The_Unit);
      else
         The_Span := Element_Span (El);
      end if;

      if Compute_All_Lines then
         Result.All_Lines :=
           Metric_Count (The_Span.Last_Line - The_Span.First_Line + 1);
      end if;

      if Selective_Line_Metrics_Set then

         declare
            Current_Line_Kind : Line_Kinds;
         begin

            for J in The_Span.First_Line .. The_Span.Last_Line loop

               Current_Line_Kind := Line_Kinds_Table.Table (J);

               case Current_Line_Kind is
                  when Blank_Line =>

                     if Compute_Blank_Lines then
                        Result.Blank_Lines := Result.Blank_Lines + 1;
                     end if;

                  when Code_Line =>

                     if Compute_Code_Lines
                       or else
                        Compute_Comment_Code_Ratio
                     then
                        Result.Code_Lines := Result.Code_Lines + 1;
                     end if;

                  when Comment_Line =>

                     if Compute_Comment_Lines
                       or else
                        Compute_Comment_Code_Ratio
                     then
                        Result.Comment_Lines := Result.Comment_Lines + 1;
                     end if;

                  when EOL_Comment_Line =>

                     if Compute_Code_Lines
                       or else
                        Compute_Comment_Code_Ratio
                     then
                        Result.Code_Lines := Result.Code_Lines + 1;
                     end if;

                     if Compute_EOL_Comments
                       or else
                        Compute_Comment_Code_Ratio
                     then
                        Result.EOL_Comments := Result.EOL_Comments + 1;
                     end if;

               end case;

            end loop;

         end;

      end if;

      return Result;

   exception
   --  In case of an ASIS exception we report it and go ahead with computing
   --  other metrics for other sources. Otherwise we let the exception
   --  propagate out of this routine to raise Fatal_Error at upper level
      when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
                Asis.Exceptions.ASIS_Inappropriate_Container        |
                Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
                Asis.Exceptions.ASIS_Inappropriate_Element          |
                Asis.Exceptions.ASIS_Inappropriate_Line             |
                Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
                Asis.Exceptions.ASIS_Failed                         =>

         ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);

         if SF /= No_SF_Id then
            Set_Source_Status (SF, Error_Detected);
         end if;

         Asis.Implementation.Set_Status;

         return Zero_Line_Metrics;
   end Compute_Line_Metrics;

   --------------------------
   -- Compute_Unit_Metrics --
   --------------------------

   procedure Compute_Unit_Metrics
     (The_CU : Asis.Compilation_Unit;
      SF     : SF_Id)
   is
      CU_Span           : Span;
      File_Line_Metrics : Line_Metrics_Record;
   begin
      The_Unit := Unit_Declaration (The_CU);

      CU_Kind  := Unit_Kind (The_CU);
      CU_Class := Unit_Class (The_CU);
      CU_Span  := Compilation_Span (The_Unit);

      Set_Global_Metrics_Flags;

      File_Line_Kinds_Table
        (Lines (The_Unit, CU_Span.First_Line, CU_Span.Last_Line));

      File_Line_Metrics := Compute_Line_Metrics (SF);

      Set_All_Lines     (SF, File_Line_Metrics.All_Lines);
      Set_Code_Lines    (SF, File_Line_Metrics.Code_Lines);
      Set_Comment_Lines (SF, File_Line_Metrics.Comment_Lines);
      Set_EOL_Comments  (SF, File_Line_Metrics.EOL_Comments);
      Set_Blank_Lines   (SF, File_Line_Metrics.Blank_Lines);

      if Unit_Metrics_Set then
         Set_Source_Out_File (SF);
         Generate_Header (SF, The_CU);
         Generate_Line_Output (SF);
      end if;

      --  Re-initialize File complexity record
      Initialize_File_Complexity_Metrics;

      Compute_And_Report_Element_Metrics
        (Program_Unit         => The_Unit,
         Nesting_Level        => 0,
         SF                   => SF,
         Include_Nested_Units => Compute_Local_Metrics);

      if Source_Status (SF) /=  Error_Detected then
         Set_Source_Status (SF, Processed);
      end if;

      if Unit_Metrics_Set then
         if File_Complexity_Metrics.Total_Executable_Units /= 0 then
            Report_File_Complexity;
         end if;
         Close_Tag ("file", Depth => 1);
      end if;

      if Is_Open (Source_Output_File) then
         Close (Source_Output_File);
      end if;
      if Compute_Coupling_Metric then
         Collect_Coupling_Dependencies (The_CU);
      end if;

      if Is_Open (Source_Output_File) then
         Close (Source_Output_File);
      end if;

   exception
      when others =>
         if Is_Open (Source_Output_File) then
            Close (Source_Output_File);
         end if;

         raise;
   end Compute_Unit_Metrics;

   ---------------------------
   -- File_Line_Kinds_Table --
   ---------------------------

   procedure File_Line_Kinds_Table (Line_List : Asis.Text.Line_List) is
   begin
      Line_Kinds_Table.Init;

      for J in Line_List'Range loop
         Line_Kinds_Table.Append (Line_Kind (Line_List (J)));
      end loop;

   end File_Line_Kinds_Table;

   --------------------
   -- Is_White_Space --
   --------------------

   function Is_White_Space (W_Ch : Wide_Character) return Boolean is
      Ch : constant Character := To_Character (W_Ch);
   begin
      return (False
         or else Ch = ' '
         or else Ch = ASCII.HT);
   end Is_White_Space;

   ---------------
   -- Line_Kind --
   ---------------

   function Line_Kind (Line : Asis.Text.Line) return Line_Kinds is
      Result : Line_Kinds;
   begin

      Result := Line_Image_Kind (Line_Image (Line));

      if Result = Code_Line
       and then
         Line_Image_Kind (Comment_Image (Line)) = Comment_Line
      then
         Result := EOL_Comment_Line;
      end if;

      return Result;

   end Line_Kind;

   ---------------------
   -- Line_Image_Kind --
   ---------------------

   function Line_Image_Kind (Line_Img : Program_Text) return Line_Kinds is
      Idx       : Natural    := 0;
      Result    : Line_Kinds := Blank_Line;
   begin

      for J in Line_Img'Range loop

         if not Is_White_Space (Line_Img (J)) then
            Idx := J;
            exit;
         end if;

      end loop;

      if Idx > 0 then

         if Line_Img (Idx) = '-' and then
            Idx < Line_Img'Last  and then
            Line_Img (Idx + 1) = '-'
         then
            Result := Comment_Line;
         else
            Result := Code_Line;
         end if;

      end if;

      return Result;

   end Line_Image_Kind;

   --------------------------------------
   -- Increase_File_Complexity_Metrics --
   --------------------------------------

   procedure Increase_File_Complexity_Metrics
    (Complexity_Metrics : Complexity_Metric_Counter) is
   begin
      --  Statement Complexity
      File_Complexity_Metrics.Totals (Statement_Complexity) :=
        File_Complexity_Metrics.Totals (Statement_Complexity) +
          Float (Complexity_Metrics.Statement_Complexity);

      --  Expression Complexity
      File_Complexity_Metrics.Totals (Expression_Complexity) :=
        File_Complexity_Metrics.Totals (Expression_Complexity) +
          Float (Complexity_Metrics.Expression_Complexity);

      --  Essential Complexity
      File_Complexity_Metrics.Totals (Essential_Complexity) :=
        File_Complexity_Metrics.Totals (Essential_Complexity) +
          Float (Complexity_Metrics.Essential_Complexity);

      --  Cyclomatic Complexity
      File_Complexity_Metrics.Totals (Cyclomatic_Complexity) :=
        File_Complexity_Metrics.Totals (Cyclomatic_Complexity) +
          Float (Complexity_Metrics.Statement_Complexity) +
          Float (Complexity_Metrics.Expression_Complexity);

      --  Max Loop Nesting
      if File_Complexity_Metrics.Totals (Max_Loop_Nesting) <
        Float (Complexity_Metrics.Max_Loop_Nesting)
      then

            File_Complexity_Metrics.Totals (Max_Loop_Nesting) :=
               Float (Complexity_Metrics.Max_Loop_Nesting);
      end if;

      --  File_Total_Executable_Units
      File_Complexity_Metrics.Total_Executable_Units :=
        File_Complexity_Metrics.Total_Executable_Units + 1;

   end Increase_File_Complexity_Metrics;

   ------------------------------------
   -- Report_File_Complexity_Average --
   ------------------------------------

   procedure Report_File_Complexity
   is
      Values_To_Print : Complexity_To_Print;
      Length : Natural;
      Max_Line_Length : constant := 25;
   begin
      --  Title for text report
      Report ("");
      Report ("=== Average complexity metrics ===");
      --  Process values
      for C in Complexity_Type loop
         if Is_Complexity_reported (C) then
            case C is
            --  Compute average
            when Statement_Complexity | Expression_Complexity |
                 Essential_Complexity | Cyclomatic_Complexity =>

                  Values_To_Print (C) :=
                    Real_Val_To_Print
                      (File_Complexity_Metrics.Totals (C) /
                         Float
                           (File_Complexity_Metrics.Total_Executable_Units));

            --  Just retreive maximum value
            when Max_Loop_Nesting =>
               Values_To_Print (Max_Loop_Nesting) :=
                 Real_Val_To_Print (File_Complexity_Metrics.Totals (C));

            end case;

            --  Add to XML Report
            Output_XML_Metric
              (To_Lower (C'Img),
               Values_To_Print (C)'Img,
               Depth => 2);

            --  Add to text Report
            --  Compute the spaces required before the semi-column in order to
            --  fit the report text pattern.
            Length := Natural (Max_Line_Length -
                Complexity_Type'Image (C)'Length);
            Report (To_Lower (C'Img) & String'(1 .. Length => ' ') & ": " &
                Values_To_Print (C)'Img,
                   Depth => 2);
         end if;

      end loop;
   end Report_File_Complexity;

   ----------------------------------------
   -- Initialize_File_Complexity_Metrics --
   ----------------------------------------

   procedure Initialize_File_Complexity_Metrics is
   begin
      File_Complexity_Metrics.Totals := (others => 0.0);
      File_Complexity_Metrics.Total_Executable_Units := 0;
      Is_Complexity_reported :=
     (Statement_Complexity  => Compute_Cyclomatic_Complexity,
      Expression_Complexity => Compute_Cyclomatic_Complexity,
      Essential_Complexity  => Compute_Essential_Complexity,
      Cyclomatic_Complexity => Compute_Cyclomatic_Complexity,
      Max_Loop_Nesting      => Compute_Loop_Nesting);

   end Initialize_File_Complexity_Metrics;

end METRICS.Compute;
