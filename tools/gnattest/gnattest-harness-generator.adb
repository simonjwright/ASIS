------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--           G N A T T E S T . H A R N E S S . G E N E R A T O R            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
--                                                                          --
-- GNATTEST  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either  version  2, or (at your option) any later --
-- version.  GNATTEST  is  distributed  in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License distributed with GNAT; see file COPYING. If --
-- not, write to the  Free  Software  Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.,                                      --
--                                                                          --
-- GNATTEST is maintained by AdaCore (http://www.adacore.com).              --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with Asis.Ada_Environments;      use Asis.Ada_Environments;
with Asis.Compilation_Units;     use Asis.Compilation_Units;
with Asis.Declarations;          use Asis.Declarations;
with Asis.Definitions;           use Asis.Definitions;
with Asis.Elements;              use Asis.Elements;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Errors;
with Asis.Exceptions;            use Asis.Exceptions;
with Asis.Expressions;           use Asis.Expressions;
with Asis.Implementation;
with Asis.Iterator;              use Asis.Iterator;
with Asis.Text;                  use Asis.Text;

with ASIS_UL.Common;
with ASIS_UL.Compiler_Options;   use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;

with GNATtest.Harness.Source_Table;  use GNATtest.Harness.Source_Table;
with GNATtest.Options;               use GNATtest.Options;
with GNATtest.Environment;           use GNATtest.Environment;
with GNATtest.Skeleton.Source_Table;

with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;          use GNATCOLL.VFS_Utils;
with GNATCOLL.Traces;             use GNATCOLL.Traces;

package body GNATtest.Harness.Generator is

   Me : constant Trace_Handle := Create ("Harness", Default => Off);

   use List_Of_Strings;
   use Asis_Element_List;

   Suit_List : List_Of_Strings.List;
   --  Storing the names of all suits

   Good_For_Substitution_Inst : List_Of_Strings.List;
   --  Storing the names of generic test packages, that should have
   --  a substitution suite instance.

   Last_Context_Name : String_Access;
   --  Suffixless name of the last tree file created

   type Separate_Project_Info is record
      Name_TD          : String_Access := null;
      Name_Extending   : String_Access := null;
      Path_TD          : String_Access := null;
      Path_Extending   : String_Access := null;
      Name_Of_Extended : String_Access := null;
      Path_Of_Extended : String_Access := null;
      Main_File_Name   : String_Access := null;
      Stub_Source_Dir  : String_Access := null;
      Test_Package     : String_Access := null;
      Test_Data        : String_Access := null;
      UUT_File_Name    : String_Access := null;
      Sources_List     : List_Of_Strings.List := List_Of_Strings.Empty_List;
   end record;

   package Separate_Project_Info_Vestors is new
     Ada.Containers.Vectors (Positive, Separate_Project_Info);
   use Separate_Project_Info_Vestors;

   Separate_Projects : Separate_Project_Info_Vestors.Vector;

   ------------------------
   --  String constants  --
   ------------------------

   --  Unit names:

   Common_Suite_Name          : constant String := "Suite";
   --  Suffixless name of the unit containing a common suite

   Generic_Suite_Name         : constant String := "Gen_Suite";
   --  Suffixless name of the unit containing a generic suite

   Substitution_Suite_Name    : constant String := "Substitution_Suite";
   --  Suffixless name of the unit containing substitution suite

   Generic_Substitution_Suite_Name  : constant String
     := "Gen_Substitution_Suite";
   --  Suffixless name of the unit containing a generic substitution suite

   Instant_Suite_Name         : constant String := "Suite_Inst";
   --  Suffixless name of the unit containing instantination suite

   Substitution_Instant_Suite_Name  : constant String
     := "Substitution_Suite_Inst";
   --  Suffixless name of the unit containing instantination suite

   --  Infrastructure elements:

   Test_Case_Prefix           : constant String := "Case_";
   --  Prefix to Test_Case variables' names

   Main_Suite_Name            : constant String := "Gnattest_Main_Suite";
   --  Suffixless name of the unit containing the main suite

   Test_Runner_Name           : constant String := "Test_Runner";
   --  Suffixless name of the unit containing the test runner

   -------------------------
   --  Inner Subprograms  --
   -------------------------

   function Initialize_Context (Source_Name : String) return Boolean;
   --  Creates a tree file and initializes the context.

   procedure Create_Tree (Full_Source_Name : String; Success : out Boolean);
   --  Tries to create the tree file for the given source file. The tree file
   --  and the corresponding ALI file are placed into a temporary directory.
   --  If the attempt is successful, Success is set ON, otherwise it is set
   --  OFF.

   procedure Process_Source (The_Unit : Asis.Compilation_Unit);
   --  Processes given compilation unit, gathers information that is needed
   --  for generating the testing unit and suite and calls their generators
   --  if the source is appropriate (contains one or less tagged recors test
   --  type declaration and at least one test routine).

   procedure Gather_Data
     (The_Unit          : Asis.Compilation_Unit;
      Data              : in out Data_Holder;
      Apropriate_Source : out Boolean);
   --  Iterates through the given unit and sets the values of Main_Type and
   --  Subp_List. All the iterations are done here.
   --  Checks if given unit is of the right kind and if it is appropriate.
   --  Marks unappropriate sources in the source table.

   procedure Gather_Liskiv_Data (Data : in out Data_Holder);
   --  Gathers data about overriden test routines.

   function Root_Type_Declaration
     (Type_Dec : Asis.Element) return Asis.Element;
   --  Unlike Corresponding_Root_Type unwinds all the tagged record type
   --  hierarchy disregart the privacy of intermidiate extensions.
   --  If the argument allready is a record type declaration, returns itself.
   --  If given not a tagged record declaration or extension declaration
   --  returns Nil_Element.

   function Parent_Type_Declaration
     (Type_Dec : Asis.Element) return Asis.Element;
   --  Returns a corresponding parent type declaration for a given tagged type
   --  extension declaration.

   function Corresponding_Generic_Package
     (Package_Instance : Asis.Element) return Asis.Element;
   --  Returns a corresponding generic package declaration for a
   --  formal package.

   function Is_AUnit_Part (Unit : Compilation_Unit) return Boolean;
   --  Checks if the unit under consideration is a part of AUnit library itself

   function Is_Test_Routine (Subp : Asis.Element) return Boolean;
   --  Indicates if the given Subprogram is a test routine, which means it
   --  has only one parameter whose type is a descendant of AUnit test type.
   --  Also returns False for Set_Up, Set_Up_Case, Tear_Down, Tear_Down_Case.

   function Is_Test_Fixture_Routine (Subp : Asis.Element) return Boolean;
   --  Same as above, but only returns true if the ancestor test type is
   --  Test_Fixture.

   function Is_Test_Related (Subp : Asis.Element) return Boolean;
   --  Indicates if the given Subprogram is of interest, that is a test routine
   --  or Set_Up/Tear_Down etc.

   function Is_Test_Case (Type_Decl : Asis.Element) return Boolean;
   --  Indicates if given type is a test_case type of AUnit framework.

   procedure Generate_Substitution_Suite_From_Tests (Data : Data_Holder);
   --  Creates a substitution test suite.

   procedure Generate_Suite_Instance (Data : Data_Holder);
   --  Creates a suite instantination.

   function Get_Subp_Name (Subp : Asis.Element) return String;
   --  Returns a name of the subprogram whose declaration is given as Subp.

   procedure Source_Clean_Up;
   --  Minimal clean-up needed for one source (deleting .ali & .adt)

   function Positive_Image (P : Positive) return String;
   --  Returns a trimmed image of the argument

   function Nesting_Difference
     (Nesting_1, Nesting_2 : String) return String;
   --  Returns difference in ending of two nestings without the first dot
   --  of the deeper nesting.

   procedure Generate_Common_Harness_Files;
   --  Generates aggregate project and a makefile for separate drivers
   --  and test_drivers.list.

   procedure Generate_Global_Config_Pragmas_File;
   --  Generates files containing pragmas suppressing pre and postconditions
   --  and possibly manipulating ghost policy.

   procedure Generate_Gnattest_Common_Prj;
   --  Generates abstract project file gnattest_common that contains different
   --  attributes relevant to the harness.

   function Gnattest_Common_Prj_Name return String is
      (Harness_Dir.all & Directory_Separator & "gnattest_common.gpr");

   function Type_Test_Package (Elem : Asis.Element) return String;
   --  Get name of test package where test type corresponding to Elem should be
   --  declared.

   function Type_Name (Elem : Asis.Element) return String is
     (To_String (Defining_Name_Image (First_Name (Elem))));

   -----------------
   -- Create_Tree --
   -----------------

   procedure Create_Tree (Full_Source_Name : String; Success : out Boolean) is
   begin
      Compile
       (new String'(Full_Source_Name),
        Arg_List.all,
        Success,
        GCC          => ASIS_UL.Common.Gcc_To_Call,
        Display_Call => ASIS_UL.Debug.Debug_Flag_C);
   end Create_Tree;

   -------------------------------------
   --  Corresponding_Generic_Package  --
   -------------------------------------
   function Corresponding_Generic_Package
     (Package_Instance : Asis.Element) return Asis.Element
   is
      Name : constant Asis.Element := First_Name (Package_Instance);
   begin
      return
        Unit_Declaration (Library_Unit_Declaration (Defining_Name_Image
          (Corresponding_Generic_Element (Name)), The_Context));
   end Corresponding_Generic_Package;

   -------------------
   --  Gather_Data  --
   -------------------

   procedure Gather_Data
     (The_Unit          : Asis.Compilation_Unit;
      Data              : in out Data_Holder;
      Apropriate_Source : out Boolean) is separate;

   --------------------------
   --  Gather_Liskiv_Data  --
   --------------------------

   procedure Gather_Liskiv_Data (Data : in out Data_Holder) is

      Test_Routine : Test_Routine_Info_Enhanced;

      Subp            : Asis.Element;
      Overridden_Subp : Asis.Element;
      Original_Type   : Asis.Element;
      Unit_Im         : String_Access;

      Type_Number : Positive;
      Depth       : Natural;

      Parent_Unit :  Asis.Compilation_Unit;
      Parent_File : String_Access;

      Tmp_Type_Info : Test_Type_Info;

   begin

      for K in 1 .. To_Index (Data.TR_List.Last) loop

         Subp        := Data.TR_List.Element (K).TR_Declaration;
         Type_Number := Data.TR_List.Element (K).Test_Type_Numb;

         if Is_Overriding_Operation (Subp) then
            Overridden_Subp := Corresponding_Overridden_Operation (Subp);
            Original_Type   := Enclosing_Element (Primitive_Owner (Subp));

            if Is_Part_Of_Inherited (Overridden_Subp) then
               Overridden_Subp :=
                 Corresponding_Declaration (Overridden_Subp);
            end if;

            Unit_Im := new String'(Base_Name (To_String (Text_Name
              (Enclosing_Compilation_Unit (Overridden_Subp)))));

            if
              Trait_Kind (Overridden_Subp) /= An_Abstract_Trait and
              Source_Present (Unit_Im.all)
            then

               Test_Routine.TR_Declaration      := Overridden_Subp;
               Test_Routine.TR_Text_Name        :=
                 new String'(Get_Subp_Name (Overridden_Subp));

               Parent_Unit :=
                 Enclosing_Compilation_Unit (Overridden_Subp);

               Parent_File := new String'
                 (Base_Name (To_String (Text_Name (Parent_Unit))));

               Test_Routine.TR_Parent_Unit_Decl := Parent_Unit;

               Test_Routine.TR_Rarent_Unit_Name :=
                 new String'(To_String (Unit_Full_Name (Parent_Unit)));

               Test_Routine.Test_Type_Numb := Type_Number;

               Test_Routine.Tested_Sloc := new String'
                 (Parent_File.all
                  & ":"
                  & Trim
                    (Integer'Image (First_Line_Number (Overridden_Subp)),
                     Both)
                  & ":"
                  & Trim
                    (Integer'Image (First_Column_Number (Overridden_Subp)),
                     Both)
                  & ": "
                  & Test_Routine.TR_Text_Name.all
                  & " overridden at "
                  & Base_Name (Data.Test_Unit_File_Name.all)
                  & ":"
                  & Trim
                    (Integer'Image (First_Line_Number (Original_Type)),
                     Both)
                  & ":"
                  & Trim
                    (Integer'Image (First_Column_Number (Original_Type)),
                     Both)
                  & ":");

               Data.LTR_List.Append (Test_Routine);

               Depth :=
                 Inheritance_Depth
                   (Data.Test_Types.Element (Type_Number).Test_Type,
                    Enclosing_Element (Primitive_Owner (Overridden_Subp)));

               if
                 Depth > Data.Test_Types.Element
                   (Type_Number).Max_Inheritance_Depth
               then
                  Tmp_Type_Info := Data.Test_Types.Element (Type_Number);
                  Tmp_Type_Info.Max_Inheritance_Depth := Depth;
                  Data.Test_Types.Replace_Element (Type_Number, Tmp_Type_Info);
               end if;

            end if;

         end if;

      end loop;

   end Gather_Liskiv_Data;

   ---------------------------------------------
   -- Generate_Substitution_Suite_From_Tested --
   ---------------------------------------------

   procedure Generate_Substitution_Suite_From_Tested
     (Data : Data_Holder; Path : String := "")
   is
      File_Destination : constant String :=
        (if Path = "" then Harness_Dir.all else Path);

      New_Unit_Name : constant String :=
        Data.Test_Unit_Full_Name.all & "." & Substitution_Suite_Name;

      Type_Ancestor : Asis.Element;

      package Include_Sets is
        new Ada.Containers.Indefinite_Ordered_Sets (String);
      use Include_Sets;

      type Duplication_Array is array
        (Data.Test_Types.First_Index .. Data.Test_Types.Last_Index) of Boolean;

      Duplication : Duplication_Array := (others => False);

      Include_Units : Include_Sets.Set;
      Include_Cur   : Include_Sets.Cursor;

      Type_Im  : String_Access;
      PUnit_Im : String_Access;
      Type_Ns  : String_Access;

      Current_TT : Test_Type_Info;
      Current_TT_Number : Natural;

      procedure Set_Current_TT (Type_Dec : Asis.Element);

      procedure Set_Current_TT (Type_Dec : Asis.Element)
      is
         Tmp_TT : Test_Type_Info;
      begin

         for
           I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index
         loop
            Tmp_TT := Data.Test_Types.Element (I);
            if Is_Equal (Tmp_TT.Tested_Type, Type_Dec) then
               Current_TT := Tmp_TT;
               Current_TT_Number := I;
               exit;
            end if;
         end loop;

      end Set_Current_TT;

   begin

      --  Creating overridden test suite spec
      Create (File_Destination
              & Unit_To_File_Name (New_Unit_Name)
              & ".ads");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      S_Put (0, "with AUnit.Test_Suites;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "package " & New_Unit_Name & " is");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "use AUnit.Test_Suites;");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "function Suite return AUnit.Test_Suites.Access_Test_Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "end " & New_Unit_Name & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;

      Close_File;

      --  Gathering additional information about the tests. We need to
      --  correctly address the test types for conversion from parent
      --  tests to actual tests. Thus we should know all the names of units
      --  containing predecessor types and distinguish them.

      for I in Data.LTR_List.First_Index .. Data.LTR_List.Last_Index loop

         Set_Current_TT (Data.LTR_List.Element (I).Tested_Type);

         Type_Ancestor := Current_TT.Tested_Type;

         for
           K in 1 .. Data.Test_Types.Element
             (Current_TT_Number).Max_Inheritance_Depth
         loop

            Type_Ancestor := Parent_Type_Declaration (Type_Ancestor);

            Type_Ns  := new String'(Get_Nesting (Type_Ancestor));
            PUnit_Im := new String'(To_String (Unit_Full_Name
              (Enclosing_Compilation_Unit (Type_Ancestor))));

            if Type_Ns.all = PUnit_Im.all then
               Include_Units.Include
                 (PUnit_Im.all              &
                  "."                       &
                  Type_Name (Type_Ancestor) &
                  Test_Data_Unit_Name_Suff  &
                  "."                       &
                  Type_Name (Type_Ancestor) &
                  Test_Unit_Name_Suff);
            else
               Include_Units.Include
                 (PUnit_Im.all                                  &
                  "."                                           &
                  Test_Data_Unit_Name                           &
                  "."                                           &
                  Test_Unit_Name                                &
                  "."                                           &
                  Nesting_Difference
                    (Type_Ns.all, PUnit_Im.all)                 &
                  "."                                           &
                  Type_Name (Type_Ancestor)                     &
                  Test_Data_Unit_Name_Suff                      &
                  "."                                           &
                  Type_Name (Type_Ancestor)                     &
                  Test_Unit_Name_Suff);
            end if;

         end loop;
      end loop;

      --  Creating overridden test suite body
      Create (File_Destination
              & Unit_To_File_Name (New_Unit_Name)
              & ".adb");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      S_Put (0, "with AUnit.Test_Caller;");
      Put_New_Line;
      S_Put (0, "with Ada.Unchecked_Conversion;");
      Put_New_Line;
      S_Put (0, "with Gnattest_Generated;");
      Put_New_Line;
      Put_New_Line;

      --  Adding dependancy units;
      Include_Cur := Include_Units.First;
      loop
         exit when Include_Cur = Include_Sets.No_Element;
         S_Put (0, "with " & Include_Sets.Element (Include_Cur) & ";");
         Put_New_Line;
         Include_Sets.Next (Include_Cur);
      end loop;

      Put_New_Line;
      S_Put (0,
             "package body " &
             New_Unit_Name   &
             " is");
      Put_New_Line;
      Put_New_Line;

      for I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index loop
         S_Put
           (3,
            "package Runner_"  &
            Positive_Image (I) &
            " is new AUnit.Test_Caller");
         Put_New_Line;

         S_Put (5,
                "(GNATtest_Generated.GNATtest_Standard."       &
                Data.Test_Unit_Full_Name.all                   &
                "."                                            &
                Data.Test_Types.Element (I).Test_Type_Name.all &
                ");");

         Put_New_Line;
      end loop;

      Put_New_Line;

      --  Declaring access to test routines types
      for L in Data.LTR_List.First_Index .. Data.LTR_List.Last_Index loop

         Set_Current_TT (Data.LTR_List.Element (L).Tested_Type);

         Type_Ancestor := Current_TT.Tested_Type;

         if not Duplication (Current_TT_Number) then

            for K in 1 .. Current_TT.Max_Inheritance_Depth loop

               Type_Ancestor := Parent_Type_Declaration (Type_Ancestor);
               Type_Im  := new String'
                 (Test_Routine_Prefix &
                  Type_Name (Type_Ancestor));
               PUnit_Im := new String'(Type_Test_Package (Type_Ancestor));

               S_Put (3,
                      "type Test_Method_"                &
                      Positive_Image (Current_TT_Number) &
                      "_"                                &
                      Trim (Integer'Image (K), Both)     &
                      " is access procedure");
               Put_New_Line;
               S_Put (5,
                      "(T : in out " &
                      PUnit_Im.all   &
                      "."            &
                      Type_Im.all    &
                      ");");
               Put_New_Line;

               Free (Type_Im);
               Free (PUnit_Im);
            end loop;
            Duplication (Current_TT_Number) := True;
         end if;
      end loop;

      Put_New_Line;
      S_Put (3, "Result : aliased AUnit.Test_Suites.Test_Suite;");
      Put_New_Line;
      Put_New_Line;

      --  Declaring test cases
      for K in Data.LTR_List.First_Index .. Data.LTR_List.Last_Index loop

         Set_Current_TT (Data.LTR_List.Element (K).Tested_Type);

         for Depth in 1 .. Data.LTR_List.Element (K).Inheritance_Depth loop
            S_Put
              (3,
               Test_Case_Prefix                           &
               Positive_Image (Current_TT_Number)         &
               "_"                                        &
               Data.LTR_List.Element (K).TR_Text_Name.all &
               "_"                                        &
               Trim (Integer'Image (Depth), Both)         &
               " : aliased Runner_"                       &
               Positive_Image (Current_TT_Number)         &
               ".Test_Case;");
            Put_New_Line;
         end loop;

      end loop;

      Put_New_Line;
      S_Put (3,
             "function Suite return AUnit.Test_Suites.Access_Test_Suite is");
      Put_New_Line;
      Put_New_Line;

      --  Instantinating test type converters
      for K in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index loop

         for
           I in 1 .. Data.Test_Types.Element (K).Max_Inheritance_Depth
         loop

            S_Put
              (6,
               "function Convert is new Gnattest_Generated." &
               "Gnattest_Standard.Ada.Unchecked_Conversion");
            Put_New_Line;
            S_Put (8,
                   "(Test_Method_"                &
                   Positive_Image (K)             &
                   "_"                            &
                   Trim (Integer'Image (I), Both) &
                   ", Runner_"                    &
                   Positive_Image (K)             &
                   ".Test_Method);");
            Put_New_Line;
         end loop;
      end loop;

      Put_New_Line;
      S_Put (3, "begin");
      Put_New_Line;
      Put_New_Line;

      --  Creating test cases
      for K in Data.LTR_List.First_Index .. Data.LTR_List.Last_Index loop

         Set_Current_TT (Data.LTR_List.Element (K).Tested_Type);

         Type_Ancestor := Current_TT.Tested_Type;

         for Depth in 1 .. Data.LTR_List.Element (K).Inheritance_Depth loop

            Type_Ancestor := Parent_Type_Declaration (Type_Ancestor);
            PUnit_Im := new String'(Type_Test_Package (Type_Ancestor));

            S_Put
              (6,
               "Runner_"                          &
               Positive_Image (Current_TT_Number) &
               ".Create");
            Put_New_Line;
            S_Put
              (8,
               "("                                        &
               Test_Case_Prefix                           &
               Positive_Image (Current_TT_Number)         &
               "_"                                        &
               Data.LTR_List.Element (K).TR_Text_Name.all &
               "_"                                        &
               Trim (Integer'Image (Depth), Both)         &
               ",");
            Put_New_Line;
            S_Put (9,
                   """"
                   & Data.LTR_List.Element (K).Tested_Sloc.all
                   & """,");
            Put_New_Line;
            S_Put (9,
                   "Convert ("                                &
                   PUnit_Im.all                               &
                   "."                                        &
                   Data.LTR_List.Element (K).TR_Text_Name.all &
                   "'Access));");
            Put_New_Line;

            Free (PUnit_Im);

         end loop;

      end loop;

      Put_New_Line;

      --  Adding test cases to the suite
      for K in Data.LTR_List.First_Index .. Data.LTR_List.Last_Index loop

         Set_Current_TT (Data.LTR_List.Element (K).Tested_Type);

         for Depth in 1 .. Data.LTR_List.Element (K).Inheritance_Depth loop
            S_Put
              (6,
               "Add_Test (Result'Access, "                      &
               Test_Case_Prefix                                 &
               Positive_Image (Current_TT_Number)               &
               "_"                                              &
               Data.LTR_List.Element (K).TR_Text_Name.all       &
               "_"                                              &
               Trim (Integer'Image (Depth), Both)               &
               "'Access);");
            Put_New_Line;

         end loop;

      end loop;

      Put_New_Line;
      S_Put (6, "return Result'Access;");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "end Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "end " & New_Unit_Name & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;
      Close_File;

      List_Of_Strings.Append (Suit_List, New_Unit_Name);

   end Generate_Substitution_Suite_From_Tested;

   ----------------------------------------------
   --  Generate_Substitution_Suite_From_Tests  --
   ----------------------------------------------

   procedure Generate_Substitution_Suite_From_Tests (Data : Data_Holder) is

      New_Unit_Name : constant String :=
        Data.Test_Unit_Full_Name.all & "." & Substitution_Suite_Name;

      Type_Im  : String_Access;
      PUnit_Im : String_Access;

      Type_Ancestor : Asis.Element;

      package Include_Sets is
        new Ada.Containers.Indefinite_Ordered_Sets (String);
      use Include_Sets;

      Include_Units : Include_Sets.Set;
      Include_Cur   : Include_Sets.Cursor;

      Type_Number : Positive;
   begin

      --  Creating overridden test suite spec
      Create (Harness_Dir.all                    &
              Unit_To_File_Name (New_Unit_Name) &
              ".ads");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      S_Put (0, "with AUnit.Test_Suites;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "package " & New_Unit_Name & " is");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "use AUnit.Test_Suites;");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "function Suite return AUnit.Test_Suites.Access_Test_Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "end " & New_Unit_Name & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;

      Close_File;

      --  Gathering additional information about the tests. We need to
      --  correctly address the test types for conversion from parent
      --  tests to actual tests. Thus we should know all the names of units
      --  containing predecessor types and distinguish them.

      for I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index loop
         Type_Ancestor := Data.Test_Types.Element (I).Test_Type;
         for K in 1 .. Data.Test_Types.Element (I).Max_Inheritance_Depth loop
            Type_Ancestor := Parent_Type_Declaration (Type_Ancestor);
            PUnit_Im := new String'(To_String (Unit_Full_Name
              (Enclosing_Compilation_Unit (Type_Ancestor))));
            Include_Units.Include (PUnit_Im.all);
            Free (PUnit_Im);
         end loop;
      end loop;

      --  Creating overridden test suite body
      Create (Harness_Dir.all                    &
              Unit_To_File_Name (New_Unit_Name) &
              ".adb");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      S_Put (0, "with AUnit.Test_Caller;");
      Put_New_Line;
      S_Put (0, "with Ada.Unchecked_Conversion;");
      Put_New_Line;
      S_Put (0, "with Gnattest_Generated;");
      Put_New_Line;
      Put_New_Line;

      --  Adding dependancy units;
      Include_Cur := Include_Units.First;
      loop
         exit when Include_Cur = Include_Sets.No_Element;
         S_Put (0, "with " & Include_Sets.Element (Include_Cur) & ";");
         Put_New_Line;
         Include_Sets.Next (Include_Cur);
      end loop;

      Put_New_Line;
      S_Put (0,
             "package body " &
             New_Unit_Name   &
             " is");
      Put_New_Line;
      Put_New_Line;

      for I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index loop
         S_Put
           (3,
            "package Runner_"  &
            Positive_Image (I) &
            " is new AUnit.Test_Caller");
         Put_New_Line;

         S_Put (5,
                "(GNATtest_Generated.GNATtest_Standard."       &
                Data.Test_Unit_Full_Name.all                   &
                "."                                            &
                Data.Test_Types.Element (I).Test_Type_Name.all &
                ");");

         Put_New_Line;
      end loop;

      Put_New_Line;

      --  Declaring access to test routines types
      for I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index loop

         Type_Ancestor := Data.Test_Types.Element (I).Test_Type;

         for K in 1 .. Data.Test_Types.Element (I).Max_Inheritance_Depth loop

            Type_Ancestor := Parent_Type_Declaration (Type_Ancestor);
            Type_Im  := new String'(To_String (Defining_Name_Image (First_Name
              (Type_Ancestor))));
            PUnit_Im := new String'(To_String (Unit_Full_Name
              (Enclosing_Compilation_Unit (Type_Ancestor))));

            S_Put (3,
                   "type Test_Method_"            &
                   Positive_Image (I)             &
                   "_"                            &
                   Trim (Integer'Image (K), Both) &
                   " is access procedure");
            Put_New_Line;
            S_Put (5,
                   "(T : in out " &
                   PUnit_Im.all   &
                   "."            &
                   Type_Im.all    &
                   ");");
            Put_New_Line;

            Free (Type_Im);
            Free (PUnit_Im);
         end loop;
      end loop;

      Put_New_Line;
      S_Put (3, "Result : aliased AUnit.Test_Suites.Test_Suite;");
      Put_New_Line;
      Put_New_Line;

      --  Declaring test cases
      for K in Data.LTR_List.First_Index .. Data.LTR_List.Last_Index loop

         Type_Number := Data.LTR_List.Element (K).Test_Type_Numb;

         for Depth in 1 .. Inheritance_Depth
           (Data.Test_Types.Element (Type_Number).Test_Type,
            Enclosing_Element (Primitive_Owner
              (Data.LTR_List.Element (K).TR_Declaration)))
         loop
            S_Put
              (3,
               Test_Case_Prefix                           &
               Positive_Image (Type_Number)               &
               "_"                                        &
               Data.LTR_List.Element (K).TR_Text_Name.all &
               "_"                                        &
               Trim (Integer'Image (Depth), Both)         &
               " : aliased Runner_"                       &
               Positive_Image (Type_Number)               &
               ".Test_Case;");
            Put_New_Line;
         end loop;

      end loop;

      Put_New_Line;
      S_Put (3,
             "function Suite return AUnit.Test_Suites.Access_Test_Suite is");
      Put_New_Line;
      Put_New_Line;

      --  Instantinating test type converters
      for K in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index loop

         for
           I in 1 .. Data.Test_Types.Element (K).Max_Inheritance_Depth
         loop

            S_Put
              (6,
               "function Convert is new Gnattest_Generated." &
               "Gnattest_Standard.Ada.Unchecked_Conversion");
            Put_New_Line;
            S_Put (8,
                   "(Test_Method_"                &
                   Positive_Image (K)             &
                   "_"                            &
                   Trim (Integer'Image (I), Both) &
                   ", Runner_"                    &
                   Positive_Image (K)             &
                   ".Test_Method);");
            Put_New_Line;
         end loop;
      end loop;

      Put_New_Line;
      S_Put (3, "begin");
      Put_New_Line;
      Put_New_Line;

      --  Creating test cases
      for K in Data.LTR_List.First_Index .. Data.LTR_List.Last_Index loop

         Type_Number := Data.LTR_List.Element (K).Test_Type_Numb;
         Type_Ancestor := Data.Test_Types.Element (Type_Number).Test_Type;

         for Depth in 1 .. Inheritance_Depth
           (Data.Test_Types.Element (Type_Number).Test_Type,
            Enclosing_Element (Primitive_Owner
              (Data.LTR_List.Element (K).TR_Declaration)))
         loop

            Type_Ancestor := Parent_Type_Declaration (Type_Ancestor);
            PUnit_Im := new String'(To_String (Unit_Full_Name
              (Enclosing_Compilation_Unit (Type_Ancestor))));

            S_Put
              (6,
               "Runner_"                    &
               Positive_Image (Type_Number) &
               ".Create");
            Put_New_Line;
            S_Put
              (8,
               "("                                        &
               Test_Case_Prefix                           &
               Positive_Image (Type_Number)               &
               "_"                                        &
               Data.LTR_List.Element (K).TR_Text_Name.all &
               "_"                                        &
               Trim (Integer'Image (Depth), Both)         &
               ",");
            Put_New_Line;
            S_Put (9,
                   """"
                   & Data.LTR_List.Element (K).Tested_Sloc.all
                   & """,");
            Put_New_Line;
            S_Put (9,
                   "Convert ("                                &
                   PUnit_Im.all                               &
                   "."                                        &
                   Data.LTR_List.Element (K).TR_Text_Name.all &
                   "'Access));");
            Put_New_Line;

            Free (PUnit_Im);

         end loop;

      end loop;

      Put_New_Line;

      --  Adding test cases to the suite
      for K in Data.LTR_List.First_Index .. Data.LTR_List.Last_Index loop

         Type_Number := Data.LTR_List.Element (K).Test_Type_Numb;

         for Depth in 1 .. Inheritance_Depth
           (Data.Test_Types.Element (Type_Number).Test_Type,
            Enclosing_Element (Primitive_Owner
              (Data.LTR_List.Element (K).TR_Declaration)))
         loop

            S_Put
              (6,
               "Add_Test (Result'Access, "                &
               Test_Case_Prefix                           &
               Positive_Image (Type_Number)               &
               "_"                                        &
               Data.LTR_List.Element (K).TR_Text_Name.all &
               "_"                                        &
               Trim (Integer'Image (Depth), Both)         &
               "'Access);");
            Put_New_Line;

         end loop;

      end loop;

      Put_New_Line;
      S_Put (6, "return Result'Access;");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "end Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "end " & New_Unit_Name & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;

      Close_File;

      List_Of_Strings.Append (Suit_List, New_Unit_Name);

   end Generate_Substitution_Suite_From_Tests;

   ---------------------------
   -- Generate_Test_Drivers --
   ---------------------------

   procedure Generate_Test_Drivers
     (Data      : Data_Holder;
      UUT       : String;
      Stub_List : Asis_Element_List.List)
   is

      procedure Process_Test_Routine (Current_TR : Test_Routine_Info'Class);
      procedure Process_Test_Package;

      function Recover_Test_Data_Unit_Name (S : String) return String;

      function Get_SPI
        (Current_TR    : Test_Routine_Info'Class;
         New_Unit_Dir  : String_Access;
         New_Unit_Name : String_Access)
         return Separate_Project_Info;

      Local_SPI         : Separate_Project_Info;
      Local_Data_Holder : Data_Holder := Data;

      function Get_SPI
        (Current_TR    : Test_Routine_Info'Class;
         New_Unit_Dir  : String_Access;
         New_Unit_Name : String_Access)
         return Separate_Project_Info
      is
         SPI : Separate_Project_Info;
      begin
         Trace (Me, "getting separate project info...");
         SPI.Name_TD         := new String'
           (TD_Prefix
            & Current_TR.TR_Text_Name.all);
         SPI.Path_TD         := new String'
           (New_Unit_Dir.all
            & Unit_To_File_Name
              (TD_Prefix
               & Current_TR.TR_Text_Name.all)
            & ".gpr");
         SPI.Main_File_Name   := new String'
           (Unit_To_File_Name (New_Unit_Name.all)
            & ".adb");

         if not Stub_Mode_ON then
            Trace (Me, "done");
            return SPI;
         end if;

         SPI.UUT_File_Name := new String'
           (GNATtest.Skeleton.Source_Table.Get_Current_Source_Spec);

         declare
            Corresponding_Body : constant String :=
              GNATtest.Skeleton.Source_Table.Get_Source_Body (UUT);
            Project_Name : constant String :=
              (if Corresponding_Body = "" then
                  GNATtest.Skeleton.Source_Table.Get_Source_Project_Name (UUT)
               else
                  GNATtest.Skeleton.Source_Table.Get_Source_Project_Name
                    (Corresponding_Body));
            Project_Path : constant String :=
              GNATtest.Skeleton.Source_Table.Get_Project_Path (Project_Name);

            Cur : Asis_Element_List.Cursor;
         begin
            SPI.Name_Extending := new String'
              (Current_TR.TR_Text_Name.all);
            SPI.Path_Extending := new String'
              (New_Unit_Dir.all
               & Unit_To_File_Name
                 (Current_TR.TR_Text_Name.all)
               & ".gpr");

            SPI.Name_Of_Extended := new String'
              (Project_Name);
            SPI.Path_Of_Extended := new String'
              (+Relative_Path
                 (Create (+Project_Path),
                  Create (+New_Unit_Dir.all)));

            SPI.Test_Package := new String'
              (Data.Test_Unit_Full_Name.all);
            SPI.Test_Data    := new String'
              (Recover_Test_Data_Unit_Name
                 (Data.Test_Unit_Full_Name.all));

            Cur := Stub_List.First;
            while Cur /= Asis_Element_List.No_Element loop
               declare
                  S : constant String :=
                    To_String
                      (Text_Name
                         (Enclosing_Compilation_Unit
                            (Asis_Element_List.Element (Cur))));
               begin
                  --  If any source meant to be stubbed is from same
                  --  project and is actually stubbed, then stub dir should
                  --  be set.
                  if
                    Project_Path =
                      GNATtest.Skeleton.Source_Table.Get_Project_Path
                        (GNATtest.Skeleton.Source_Table.
                           Get_Source_Project_Name (S)) and then
                        GNATtest.Skeleton.Source_Table.Source_Stubbed (S)
                  then

                     if S /= UUT then
                        declare
                           App : constant String :=
                             GNATtest.Skeleton.Source_Table.Get_Source_Body
                               (S);
                        begin
                           if App /= "" then
                              SPI.Sources_List.Append (App);
                              declare
                                 SD_Spec : constant String :=
                                   GNATtest.Skeleton.Source_Table.
                                     Get_Source_Stub_Data_Spec (S);
                                 SD_Body : constant String :=
                                   GNATtest.Skeleton.Source_Table.
                                     Get_Source_Stub_Data_Body (S);
                              begin
                                 if not
                                   Excluded_Test_Data_Files.Contains (SD_Spec)
                                 then
                                    SPI.Sources_List.Append (SD_Spec);
                                 end if;
                                 if not
                                   Excluded_Test_Data_Files.Contains (SD_Body)
                                 then
                                    SPI.Sources_List.Append (SD_Body);
                                 end if;
                              end;
                           end if;
                        end;
                     end if;

                     if SPI.Stub_Source_Dir = null then
                        SPI.Stub_Source_Dir := new String'
                          (GNATtest.Skeleton.Source_Table.
                             Get_Project_Stub_Dir (Project_Name));
                     end if;
                  end if;
               end;

               Next (Cur);
            end loop;
         end;

         Trace (Me, "done");
         return SPI;
      end Get_SPI;

      function Recover_Test_Data_Unit_Name (S : String) return String is
      begin
         for I in reverse S'Range loop
            if S (I) = '.' then
               return S (S'First .. I - 1);
            end if;
         end loop;

         return S;
      end Recover_Test_Data_Unit_Name;

      procedure Process_Test_Package is
         New_Unit_Dir : constant String :=
           Harness_Dir.all
           & Data.Test_Unit_Full_Name.all
           & Directory_Separator;

         New_Unit_Name : constant String :=
           (Data.Test_Unit_Full_Name.all
            & ".Suite.Test_Runner");
      begin
         Trace (Me, "processing package " & Data.Test_Unit_Full_Name.all);

         declare
            Dir : File_Array_Access;
         begin
            Append (Dir, GNATCOLL.VFS.Create (+New_Unit_Dir));
            Create_Dirs (Dir);
         exception
            when Directory_Error =>
               Report_Err
                 ("gnattest: cannot create directory " & New_Unit_Dir);
               raise Fatal_Error;
         end;

         --  Creating test driver procedure
         Create (New_Unit_Dir
                 & Unit_To_File_Name (New_Unit_Name)
                 & ".adb");

         Put_Harness_Header;
         S_Put (0, GT_Marker_Begin);
         Put_New_Line;

         S_Put (0, "with AUnit.Test_Suites; use AUnit.Test_Suites;");
         Put_New_Line;
         if not Stub_Mode_ON then
            S_Put (0, "with AUnit.Test_Caller;");
            Put_New_Line;
         end if;
         S_Put (0, "with Gnattest_Generated;");
         Put_New_Line;
         S_Put (0, "with AUnit.Reporter.GNATtest;");
         Put_New_Line;
         S_Put (0, "with AUnit.Run;");
         Put_New_Line;
         S_Put (0, "with AUnit.Options; use AUnit.Options;");
         Put_New_Line;
         if Data.Good_For_Substitution then
            S_Put
              (0,
               "with "
               & Data.Test_Unit_Full_Name.all
               & "."
               & Substitution_Suite_Name
               & "; use "
               & Data.Test_Unit_Full_Name.all
               & "."
               & Substitution_Suite_Name
               & ";");
            Put_New_Line;
         end if;

         S_Put (0, "procedure " & New_Unit_Name & " is");
         Put_New_Line;
         Put_New_Line;

         if Data.Good_For_Substitution then
            S_Put
              (3,
               "function Suite return AUnit.Test_Suites.Access_Test_Suite;");
            Put_New_Line;
            Put_New_Line;
            S_Put (3, "Result : aliased AUnit.Test_Suites.Test_Suite;");
            Put_New_Line;
            Put_New_Line;
            S_Put
              (3,
               "function Suite return AUnit.Test_Suites.Access_Test_Suite is");
            Put_New_Line;
            S_Put (3, "begin");
            Put_New_Line;
            Put_New_Line;
            S_Put
              (6,
               "Add_Test (Result'Access, "
               & Data.Test_Unit_Full_Name.all
               & "."
               & Common_Suite_Name
               & ".Suite);");
            Put_New_Line;
            S_Put
              (6,
               "Add_Test (Result'Access, "
               & Data.Test_Unit_Full_Name.all
               & "."
               & Substitution_Suite_Name
               & ".Suite);");
            Put_New_Line;
            Put_New_Line;
            S_Put (6, "return Result'Unchecked_Access;");
            Put_New_Line;
            Put_New_Line;
            S_Put (3, "end Suite;");
            Put_New_Line;
            Put_New_Line;

         end if;

         S_Put (3, "procedure Runner is new AUnit.Run.Test_Runner (Suite);");
         Put_New_Line;
         S_Put (3, "Reporter : AUnit.Reporter.GNATtest.GNATtest_Reporter;");
         Put_New_Line;
         S_Put (3, "GT_Options : AUnit_Options := Default_Options;");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "begin");
         Put_New_Line;
         if Show_Passed_Tests then
            S_Put (3, "GT_Options.Report_Successes := True;");
         else
            S_Put (3, "GT_Options.Report_Successes := False;");
         end if;
         Put_New_Line;
         if Show_Test_Duration then
            S_Put (3,
                   "GT_Options.Test_Case_Timer := True;");
            Put_New_Line;
         end if;
         Put_New_Line;
         S_Put (3, "Runner (Reporter, GT_Options);");
         Put_New_Line;

         S_Put (0, "end " & New_Unit_Name & ";");
         Put_New_Line;
         S_Put (0, GT_Marker_End);
         Put_New_Line;
         Close_File;

      end Process_Test_Package;

      procedure Process_Test_Routine (Current_TR : Test_Routine_Info'Class)
      is
         Current_Type : constant Test_Type_Info :=
          Data.Test_Types.Element (Current_TR.Test_Type_Numb);

         New_Unit_Name : String_Access;
         New_Unit_Dir  : String_Access;
      begin
         Trace (Me, "processing routine " & Current_TR.TR_Text_Name.all);

         New_Unit_Name := new String'
           (Data.Test_Unit_Full_Name.all
            & "."
            & TD_Prefix
            & Current_TR.TR_Text_Name.all);

         --  Distinguish different set of test drivers by putting them in dirs
         --  with names corresponding to UUTs.
         New_Unit_Dir := new String'
           (Harness_Dir.all
            & Data.Test_Unit_Full_Name.all
            & Directory_Separator);

         declare
            Dir : File_Array_Access;
         begin
            Append (Dir, GNATCOLL.VFS.Create (+New_Unit_Dir.all));
            Create_Dirs (Dir);
         exception
            when Directory_Error =>
               Report_Err
                 ("gnattest: cannot create directory " & New_Unit_Dir.all);
               raise Fatal_Error;
         end;

         --  Creating test driver procedure
         Create (New_Unit_Dir.all
                 & Unit_To_File_Name (New_Unit_Name.all)
                 & ".adb");

         Put_Harness_Header;
         S_Put (0, GT_Marker_Begin);
         Put_New_Line;

         S_Put (0, "pragma Ada_2005;");
         Put_New_Line;
         Put_New_Line;
         S_Put (0, "with AUnit.Test_Suites; use AUnit.Test_Suites;");
         Put_New_Line;
         if not Stub_Mode_ON then
            S_Put (0, "with AUnit.Test_Caller;");
            Put_New_Line;
         end if;
         S_Put (0, "with Gnattest_Generated;");
         Put_New_Line;
         S_Put (0, "with AUnit.Reporter.GNATtest;");
         Put_New_Line;
         S_Put (0, "with AUnit.Run;");
         Put_New_Line;
         S_Put (0, "with AUnit.Options; use AUnit.Options;");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "procedure " & New_Unit_Name.all & " is");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (3, "function Suite return AUnit.Test_Suites.Access_Test_Suite;");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "Result : aliased AUnit.Test_Suites.Test_Suite;");
         Put_New_Line;
         if not Stub_Mode_ON then
            S_Put
              (3,
               "package Caller is new AUnit.Test_Caller");
            Put_New_Line;
            S_Put
              (5,
               "(GNATtest_Generated.GNATtest_Standard."
               & Data.Test_Unit_Full_Name.all
               & "."
               & Current_Type.Test_Type_Name.all
               & ");");
            Put_New_Line;
         end if;
         S_Put (3, "Local_Test_Case : aliased Caller.Test_Case;");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (3, "function Suite return AUnit.Test_Suites.Access_Test_Suite is");
         Put_New_Line;
         S_Put (3, "begin");
         Put_New_Line;
         S_Put (6, "Caller.Create");
         Put_New_Line;
         S_Put (8, "(Local_Test_Case,");
         Put_New_Line;
         S_Put (9,
                """"
                & Current_TR.Tested_Sloc.all
                & """,");
         Put_New_Line;
         S_Put (9,
                Current_TR.TR_Text_Name.all
                & "'Access);");
         Put_New_Line;
         Put_New_Line;
         S_Put (6, "Add_Test (Result'Access, Local_Test_Case'Access);");
         Put_New_Line;
         Put_New_Line;
         S_Put (6, "return Result'Unchecked_Access;");
         Put_New_Line;
         S_Put (3, "end Suite;");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "procedure Runner is new AUnit.Run.Test_Runner (Suite);");
         Put_New_Line;
         S_Put (3, "Reporter : AUnit.Reporter.GNATtest.GNATtest_Reporter;");
         Put_New_Line;
         S_Put (3, "GT_Options : AUnit_Options := Default_Options;");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "begin");
         Put_New_Line;
         if Show_Passed_Tests then
            S_Put (3, "GT_Options.Report_Successes := True;");
         else
            S_Put (3, "GT_Options.Report_Successes := False;");
         end if;
         Put_New_Line;
         if Show_Test_Duration then
            S_Put (3,
                   "GT_Options.Test_Case_Timer := True;");
            Put_New_Line;
         end if;
         Put_New_Line;
         S_Put (3, "Runner (Reporter, GT_Options);");
         Put_New_Line;

         S_Put (0, "end " & New_Unit_Name.all & ";");
         Put_New_Line;
         S_Put (0, GT_Marker_End);
         Put_New_Line;
         Close_File;

         Separate_Projects.Append
           (Get_SPI
              (Current_TR,
               New_Unit_Dir,
               New_Unit_Name));

      end Process_Test_Routine;
   begin
      Trace (Me, "Generate_Test_Drivers");
      Increase_Indent (Me);

      if Driver_Per_Unit then
         Process_Test_Package;
         if Stub_Mode_ON then
            --  No test inheritance in stub mode. We need to clear the list
            --  of inherited test routines, otherwise corresponding test cases
            --  will be declared in the suite.
            Local_Data_Holder.ITR_List.Clear;
         end if;
         Generate_Suite
           (Local_Data_Holder,
            Harness_Dir.all
            & Data.Test_Unit_Full_Name.all
            & Directory_Separator);
         if not Stub_Mode_ON and then Data.Good_For_Substitution then
            Generate_Substitution_Suite_From_Tested
              (Local_Data_Holder,
               Harness_Dir.all
               & Data.Test_Unit_Full_Name.all
               & Directory_Separator);
         end if;

         declare
            S1 : constant String_Access := new String'
              (Harness_Dir.all
               & Data.Test_Unit_Full_Name.all
               & Directory_Separator);
            S2 : constant String_Access := new String'
              (Data.Test_Unit_Full_Name.all
               & ".Suite.Test_Runner");
         begin
            --  We may reuse the regular way of gathering data for separate
            --  drivers. Just need to override the names and paths for
            --  the generated projects.

            if Data.TR_List.Is_Empty then
               if Stub_Mode_ON then
                  --  No test inheritance in stub mode.
                  return;
               end if;
               Local_SPI := Get_SPI (Data.ITR_List.First_Element, S1, S2);
            else
               Local_SPI := Get_SPI (Data.TR_List.First_Element, S1, S2);
            end if;

            Free (Local_SPI.Name_TD);
            Local_SPI.Name_TD := new String'("Test_Driver");
            Free (Local_SPI.Name_Extending);
            Local_SPI.Name_Extending := new String'("Stubs");
            Free (Local_SPI.Path_TD);
            Local_SPI.Path_TD := new String'(S1.all & "test_driver.gpr");
            Free (Local_SPI.Path_Extending);
            Local_SPI.Path_Extending := new String'(S1.all & "stubs.gpr");
         end;

         Separate_Projects.Append (Local_SPI);
         Decrease_Indent (Me, "done");
         return;
      end if;

      for K in Data.TR_List.First_Index .. Data.TR_List.Last_Index loop
         Process_Test_Routine (Data.TR_List.Element (K));
      end loop;

      if Separate_Drivers and then not Stub_Mode_ON then
         for K in Data.ITR_List.First_Index .. Data.ITR_List.Last_Index loop
            Process_Test_Routine (Data.ITR_List.Element (K));
         end loop;
      end if;

      Decrease_Indent (Me, "done");
   end Generate_Test_Drivers;

   ----------------------------------------
   -- Generate_Substitution_Test_Drivers --
   ----------------------------------------

   procedure Generate_Substitution_Test_Drivers (Data : Data_Holder) is
      Current_Type : Test_Type_Info;
      Type_Ancestor : Asis.Element;

      Current_TR : Test_Routine_Info_Enhanced;

      New_Unit_Name : String_Access;
      New_Unit_Dir  : String_Access;

      function Get_SPI
        (Current_TR    : Test_Routine_Info'Class;
         New_Unit_Dir  : String_Access;
         New_Unit_Name : String_Access)
         return Separate_Project_Info;

      function Get_SPI
        (Current_TR    : Test_Routine_Info'Class;
         New_Unit_Dir  : String_Access;
         New_Unit_Name : String_Access)
         return Separate_Project_Info
      is
         SPI : Separate_Project_Info;
      begin
         SPI.Name_TD         := new String'
           (TD_Prefix_Overriden
            & Current_TR.TR_Text_Name.all);
         SPI.Path_TD         := new String'
           (New_Unit_Dir.all
            & Unit_To_File_Name
              (TD_Prefix_Overriden
               & Current_TR.TR_Text_Name.all)
            & ".gpr");
         SPI.Main_File_Name   := new String'
           (Unit_To_File_Name (New_Unit_Name.all)
            & ".adb");

         return SPI;
      end Get_SPI;

   begin

      for K in Data.LTR_List.First_Index .. Data.LTR_List.Last_Index loop

         Current_TR   := Data.LTR_List.Element (K);
         Current_Type := Data.Test_Types.Element (Current_TR.Test_Type_Numb);

         New_Unit_Name := new String'
           (Data.Test_Unit_Full_Name.all
            & "."
            & TD_Prefix_Overriden
            & Current_TR.TR_Text_Name.all);

         --  Distinguish different set of test drivers by putting them in dirs
         --  with names corresponding to UUTs.
         New_Unit_Dir := new String'
           (Harness_Dir.all
            & Data.Test_Unit_Full_Name.all
            & Directory_Separator);

         declare
            Dir : File_Array_Access;
         begin
            Append (Dir, GNATCOLL.VFS.Create (+New_Unit_Dir.all));
            Create_Dirs (Dir);
         exception
            when Directory_Error =>
               Report_Err
                 ("gnattest: cannot create directory " & New_Unit_Dir.all);
               raise Fatal_Error;
         end;

         --  Creating test driver procedure
         Create (New_Unit_Dir.all
                 & Unit_To_File_Name (New_Unit_Name.all)
                 & ".adb");

         Put_Harness_Header;
         S_Put (0, GT_Marker_Begin);
         Put_New_Line;

         S_Put (0, "pragma Ada_2005;");
         Put_New_Line;
         Put_New_Line;
         S_Put (0, "with AUnit.Test_Suites; use AUnit.Test_Suites;");
         Put_New_Line;
         S_Put (0, "with AUnit.Test_Caller;");
         Put_New_Line;
         S_Put (0, "with Gnattest_Generated;");
         Put_New_Line;
         S_Put (0, "with AUnit.Reporter.GNATtest;");
         Put_New_Line;
         S_Put (0, "with AUnit.Run;");
         Put_New_Line;
         S_Put (0, "with AUnit.Options; use AUnit.Options;");
         Put_New_Line;
         S_Put (0, "with Ada.Unchecked_Conversion;");
         Put_New_Line;
         Put_New_Line;

         Type_Ancestor := Current_Type.Tested_Type;

         for I in 1 .. Current_Type.Max_Inheritance_Depth loop
            Type_Ancestor := Parent_Type_Declaration (Type_Ancestor);
            S_Put (0, "with " & Type_Test_Package (Type_Ancestor) & ";");
            Put_New_Line;
         end loop;

         Put_New_Line;

         S_Put (0, "procedure " & New_Unit_Name.all & " is");
         Put_New_Line;
         Put_New_Line;

         Type_Ancestor := Current_Type.Tested_Type;

         for I in 1 .. Current_Type.Max_Inheritance_Depth loop
            Type_Ancestor := Parent_Type_Declaration (Type_Ancestor);
            S_Put
              (3,
               "type Test_Method_"
               & Trim (Integer'Image (I), Both)
               & " is access procedure");
            Put_New_Line;
            Put_New_Line;
            S_Put
              (5,
               "(T : in out "
               & Type_Test_Package (Type_Ancestor)
               & ".Test_"
               & Type_Name (Type_Ancestor)
               & ");");
            Put_New_Line;
         end loop;

         S_Put
           (3, "function Suite return AUnit.Test_Suites.Access_Test_Suite;");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "Result : aliased AUnit.Test_Suites.Test_Suite;");
         Put_New_Line;
         S_Put
           (3,
            "package Caller is new AUnit.Test_Caller");
         Put_New_Line;
         S_Put
           (5,
            "(GNATtest_Generated.GNATtest_Standard."
            & Data.Test_Unit_Full_Name.all
            & "."
            & Current_Type.Test_Type_Name.all
            & ");");
         Put_New_Line;

         for I in 1 .. Current_Type.Max_Inheritance_Depth loop
            S_Put
              (3,
               "Local_Test_Case_"
               & Trim (Integer'Image (I), Both)
               & " : aliased Caller.Test_Case;");
            Put_New_Line;
         end loop;

         Put_New_Line;

         S_Put
           (3, "function Suite return AUnit.Test_Suites.Access_Test_Suite is");
         Put_New_Line;
         Put_New_Line;

         for I in 1 .. Current_Type.Max_Inheritance_Depth loop
            S_Put
              (6,
               "function Convert is new Gnattest_Generated." &
               "Gnattest_Standard.Ada.Unchecked_Conversion");
            S_Put
              (8,
               "(Test_Method_"
               & Trim (Integer'Image (I), Both)
               & ", Caller.Test_Method);");
            Put_New_Line;
         end loop;
         Put_New_Line;

         S_Put (3, "begin");
         Put_New_Line;

         Type_Ancestor := Current_Type.Tested_Type;

         for I in 1 .. Current_Type.Max_Inheritance_Depth loop
            Type_Ancestor := Parent_Type_Declaration (Type_Ancestor);

            S_Put (6, "Caller.Create");
            Put_New_Line;
            S_Put
              (8,
               "(Local_Test_Case_"
               & Trim (Integer'Image (I), Both)
               & ",");
            Put_New_Line;
            S_Put (9,
                   """"
                   & Current_TR.Tested_Sloc.all
                   & """,");
            Put_New_Line;
            S_Put (9,
                   "Convert ("
                   & Type_Test_Package (Type_Ancestor)
                   & "."
                   & Current_TR.TR_Text_Name.all
                   & "'Access));");
            Put_New_Line;
         end loop;

         Put_New_Line;
         for I in 1 .. Current_Type.Max_Inheritance_Depth loop
            S_Put
              (6,
               "Add_Test (Result'Access, Local_Test_Case_"
               & Trim (Integer'Image (I), Both)
               & "'Access);");
            Put_New_Line;
         end loop;

         Put_New_Line;
         S_Put (6, "return Result'Unchecked_Access;");
         Put_New_Line;
         S_Put (3, "end Suite;");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "procedure Runner is new AUnit.Run.Test_Runner (Suite);");
         Put_New_Line;
         S_Put (3, "Reporter : AUnit.Reporter.GNATtest.GNATtest_Reporter;");
         Put_New_Line;
         S_Put (3, "GT_Options : AUnit_Options := Default_Options;");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "begin");
         Put_New_Line;
         if Show_Passed_Tests then
            S_Put (3, "GT_Options.Report_Successes := True;");
         else
            S_Put (3, "GT_Options.Report_Successes := False;");
         end if;
         Put_New_Line;
         if Show_Test_Duration then
            S_Put (3,
                   "GT_Options.Test_Case_Timer := True;");
            Put_New_Line;
         end if;
         Put_New_Line;
         S_Put (3, "Runner (Reporter, GT_Options);");
         Put_New_Line;

         S_Put (0, "end " & New_Unit_Name.all & ";");
         Put_New_Line;
         S_Put (0, GT_Marker_End);
         Put_New_Line;
         Close_File;

         Separate_Projects.Append
           (Get_SPI
              (Current_TR,
               New_Unit_Dir,
               New_Unit_Name));
      end loop;
   end Generate_Substitution_Test_Drivers;

   -----------------------------------
   -- Generate_Test_Driver_Projects --
   -----------------------------------

   procedure Generate_Test_Driver_Projects is
      P : Separate_Project_Info;
   begin
      if Separate_Projects.Is_Empty then
         Report_Std
           ("gnattest: no test skeletons generated because "
            & "no subprogram to test");
         Report_Std
           ("found in project " & Source_Prj.all, 10);
         Report_Std ("cannot create main suite and test runner", 10);
         raise Fatal_Error;
      end if;

      for
        K in Separate_Projects.First_Index .. Separate_Projects.Last_Index
      loop
         P := Separate_Projects.Element (K);

         declare
            Dir : File_Array_Access;
         begin
            Append
              (Dir,
               GNATCOLL.VFS.Create
                 (+(Dir_Name (P.Path_TD.all)
                  & Directory_Separator
                  & P.Name_TD.all
                  & "_obj")));
            Create_Dirs (Dir);
         exception
            when Directory_Error =>
               Report_Err
                 ("gnattest: cannot create obj directory for "
                  & P.Path_TD.all);
               raise Fatal_Error;
         end;

         Create (P.Path_TD.all);

         S_Put (0, "with ""aunit"";");
         Put_New_Line;
         S_Put
           (0,
            "with """
              & (+Relative_Path
                 (Create (+Tmp_Test_Prj.all),
                   Create (+Dir_Name (P.Path_TD.all))))
            & """;");
         Put_New_Line;
         S_Put
           (0,
            "with """
              & (+Relative_Path
                 (Create (+Gnattest_Common_Prj_Name),
                   Create (+Dir_Name (P.Path_TD.all))))
            & """;");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (0,
            "project "
            & P.Name_TD.all
            & " is");
         Put_New_Line;
         Put_New_Line;
         S_Put
           (3,
            "for Origin_Project use """
            & (+Relative_Path
                 (Create (+Source_Prj.all),
                   Create (+Normalize_Pathname (Dir_Name (P.Path_TD.all)))))
            & """;");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "for Target use Gnattest_Common'Target;");
         Put_New_Line;
         Put_New_Line;
         S_Put
           (3,
            "for Runtime (""Ada"") use Gnattest_Common'Runtime (""Ada"");");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "package Ide renames Gnattest_Common.Ide;");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package Make renames Gnattest_Common.Make;");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "for Languages use Gnattest_Common'Languages & (""Ada"");");
         Put_New_Line;

         S_Put
           (3,
            "for Main use ("""
            & P.Main_File_Name.all
            & """);");
         Put_New_Line;
         S_Put (3, "for Exec_Dir use ""."";");
         Put_New_Line;
         S_Put (3, "for Source_Dirs use (""."");");
         Put_New_Line;
         S_Put
           (3,
            "for Object_Dir use """
            & P.Name_TD.all
            & "_obj"";");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "package Builder renames Gnattest_Common.Builder;");
         Put_New_Line;
         S_Put (3, "package Linker renames Gnattest_Common.Linker;");
         Put_New_Line;
         S_Put (3, "package Binder renames Gnattest_Common.Binder;");
         Put_New_Line;
         S_Put (3, "package Compiler renames Gnattest_Common.Compiler;");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (0,
            "end "
            & P.Name_TD.all
            & ";");
         Close_File;
      end loop;

      Generate_Common_Harness_Files;

   end Generate_Test_Driver_Projects;

   -----------------------------------
   -- Generate_Common_Harness_Files --
   -----------------------------------

   procedure Generate_Common_Harness_Files is
      P : Separate_Project_Info;

      procedure Generate_Aggregate_Project;
      --  Create aggregate project that incorparates all test driver projects.

      function Path_To_Unix (S : String) return String;
      --  Replace all "\" with "/".

      procedure Generate_Aggregate_Project is
      begin
         Create (Harness_Dir.all & "test_drivers.gpr");

         S_Put (0, "with ""gnattest_common.gpr"";");
         Put_New_Line;
         Put_New_Line;
         S_Put (0, "aggregate project Test_Drivers is");
         Put_New_Line;
         S_Put (3, "for Project_Files use");
         Put_New_Line;

         if Separate_Projects.Length = Ada.Containers.Count_Type (1) then
            P := Separate_Projects.First_Element;

            declare
               Pth : constant String :=
                 +Relative_Path
                 (Create (+P.Path_TD.all),
                  Create (+Harness_Dir.all));
            begin
               S_Put (5, "(""" & Pth & """);");
            end;
            Put_New_Line;
         else
            for
              K in Separate_Projects.First_Index ..
                Separate_Projects.Last_Index
            loop
               P := Separate_Projects.Element (K);
               declare
                  Pth : constant String :=
                    +Relative_Path
                    (Create (+P.Path_TD.all),
                     Create (+Harness_Dir.all));
               begin
                  if K = Separate_Projects.First_Index then
                     S_Put (5, "(""" & Pth & """,");
                  elsif K = Separate_Projects.Last_Index then
                     S_Put (6, """" & Pth & """);");
                  else
                     S_Put (6, """" & Pth & """,");
                  end if;
               end;
               Put_New_Line;
            end loop;
         end if;

         Put_New_Line;
         S_Put
           (3,
            "for Origin_Project use """
            & (+Relative_Path
                 (Create (+Source_Prj.all),
                   Create (+Normalize_Pathname (Harness_Dir.all))))
            & """;");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "for Target use Gnattest_Common'Target;");
         Put_New_Line;
         S_Put
           (3,
            "for Runtime (""Ada"") use Gnattest_Common'Runtime (""Ada"");");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package Builder renames Gnattest_Common.Builder;");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package GNATtest is");
         Put_New_Line;
         S_Put (3, "for GNATTest_Mapping_File use ""gnattest.xml"";");
         Put_New_Line;
         S_Put (3, "end GNATtest;");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "end Test_Drivers;");

         Close_File;
      end Generate_Aggregate_Project;

      function Path_To_Unix (S : String) return String is
         S1 : String := (S);
      begin
         for I in S1'Range loop
            if S1 (I) = '\' then
               S1 (I) := '/';
            end if;
         end loop;

         return S1;
      end Path_To_Unix;

   begin
      Generate_Gnattest_Common_Prj;

      --  Makefile
      Create (Harness_Dir.all & "Makefile");
      S_Put (0, "# Check if we are running on Windows");
      Put_New_Line;
      S_Put (0, "ifeq ($(OS),Windows_NT)");
      Put_New_Line;
      S_Put (0,
             ASCII.HT
             & "EXE_EXT=.exe");
      Put_New_Line;
      S_Put (0, "else");
      Put_New_Line;
      S_Put (0,
             ASCII.HT
             & "EXE_EXT=");
      Put_New_Line;
      S_Put (0, "endif");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "# Executables");
      Put_New_Line;
      S_Put (0, "GPRBUILD=gprbuild");
      Put_New_Line;
      S_Put (0, "GPRCLEAN=gprclean");
      Put_New_Line;
      S_Put (0, "GNATCOV=gnatcov");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "# Number of processes used to build");
      Put_New_Line;
      S_Put (0, "# (default 0 is using maximum available cores)");
      Put_New_Line;
      S_Put (0, "NUMPROC=0");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "# Switches for gprbuild");
      Put_New_Line;
      S_Put
        (0,
         "# To be defined if there is a specific target and/or runtime, etc");
      Put_New_Line;
      S_Put (0, "BUILDERFLAGS=");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "# Project-specific switches");
      Put_New_Line;
      S_Put (0, "# To be defined to customize the build");
      Put_New_Line;
      S_Put (0, "GPRFLAGS=");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "# List of projects to build");
      Put_New_Line;
      S_Put (0, "PRJS = \");
      Put_New_Line;

      for
        K in Separate_Projects.First_Index .. Separate_Projects.Last_Index
      loop
         P := Separate_Projects.Element (K);
         declare
            Rel_Pth : constant Filesystem_String :=
              Relative_Path
                (Create (+P.Path_TD.all),
                 Create (+Harness_Dir.all));
            Pth : constant String :=
              (if Driver_Per_Unit then +Dir_Name (Rel_Pth) else +Rel_Pth);
         begin
            S_Put
                (0,
                 ASCII.HT
                 & Path_To_Unix (Pth)
                 & (if K = Separate_Projects.Last_Index then "" else " \"));
         end;
         Put_New_Line;
      end loop;

      Put_New_Line;
      S_Put (0, "CKPTS = $(patsubst %,%-gnatcov-cov,$(PRJS))");
      Put_New_Line;
      S_Put (0, "GNATCOV_LEVEL=stmt+decision");
      Put_New_Line;
      S_Put (0, "GNATCOV_OUTPUT_FMT=dhtml --output-dir=dhtml-report");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, ".PHONY: all");
      Put_New_Line;
      Put_New_Line;

      S_Put (0, "all: $(patsubst %,%-build,$(PRJS))");
      Put_New_Line;
      Put_New_Line;

      if Driver_Per_Unit then
         S_Put (0, "%-build: %/test_driver.gpr");
      else
         S_Put (0, "%-build: %");
      end if;
      Put_New_Line;
      S_Put
        (0,
         ASCII.HT
         & "$(GPRBUILD) $(BUILDERFLAGS) -P$< $(GPRFLAGS) -gargs -j$(NUMPROC)");
      Put_New_Line;
      Put_New_Line;

      if Driver_Per_Unit then
         S_Put (0, "%-build-cov: %/test_driver.gpr");
         Put_New_Line;
         S_Put
           (0,
            ASCII.HT
            & "$(GPRBUILD) $(BUILDERFLAGS) -P$< $(GPRFLAGS) -gargs "
            & "-j$(NUMPROC) -cargs -g -fdump-scos -fpreserve-control-flow");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "%-gnatcov-run: %-build-cov");
         Put_New_Line;
         S_Put
           (0,
            ASCII.HT
            & "$(GNATCOV) run --level=$(GNATCOV_LEVEL) -P$*/test_driver.gpr "
            & " $*/*-test_runner$(EXE_EXT) -o $*-gnattest.trace");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "%-gnatcov-cov: %-gnatcov-run");
         Put_New_Line;
         S_Put
           (0,
            ASCII.HT
            & "$(GNATCOV) coverage --save-checkpoint=$*-gnattest.ckpt "
            & "--level=$(GNATCOV_LEVEL) "
            & "-P$*/test_driver.gpr $*-gnattest.trace");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "gnatcov-consolidate: $(CKPTS)");
         Put_New_Line;
         declare
            Pth : constant String :=
              +Relative_Path
              (Create (+Source_Prj.all),
               Create (+Harness_Dir.all));
         begin
            S_Put
              (0,
               ASCII.HT
               & "$(GNATCOV) coverage -P"
               & Path_To_Unix (Pth)
               & " $(patsubst %,-C "
               & "%-gnattest.ckpt,$(PRJS)) -a $(GNATCOV_OUTPUT_FMT) "
               & "--level=$(GNATCOV_LEVEL)");
         end;
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "coverage: gnatcov-consolidate");
         Put_New_Line;
         Put_New_Line;
      end if;

      S_Put (0, "clean: $(patsubst %,%-clean,$(PRJS))");
      Put_New_Line;
      Put_New_Line;

      if Driver_Per_Unit then
         S_Put (0, "%-clean: %/test_driver.gpr");
      else
         S_Put (0, "%-clean: %");
      end if;
      Put_New_Line;
      S_Put
        (0,
         ASCII.HT
         & "$(GPRCLEAN) $(BUILDERFLAGS) -P$<");
      Put_New_Line;

      Close_File;

      --  suppress.adc & suppress_no_ghost.adc
      Generate_Global_Config_Pragmas_File;

      --  Executable list
      Create (Harness_Dir.all & "test_drivers.list");

      declare
         Exe_Suffix : String_Access := Get_Target_Executable_Suffix;
      begin
         for
           K in Separate_Projects.First_Index .. Separate_Projects.Last_Index
         loop
            P := Separate_Projects.Element (K);

            S_Put
              (0,
               Dir_Name (P.Path_TD.all)
               & Base_Name (P.Main_File_Name.all, ".adb")
               & Exe_Suffix.all);
            Put_New_Line;
         end loop;

         Free (Exe_Suffix);
      end;

      Close_File;

      Generate_Aggregate_Project;
   end Generate_Common_Harness_Files;

   -----------------------------------------
   -- Generate_Global_Config_Pragmas_File --
   -----------------------------------------

   procedure Generate_Global_Config_Pragmas_File is
   begin
      if not Is_Regular_File (Harness_Dir.all & "suppress.adc") then
         Create (Harness_Dir.all & "suppress.adc");
         S_Put (0, "pragma Assertion_Policy (Pre => Ignore);");
         Put_New_Line;
         S_Put (0, "pragma Assertion_Policy (Post => Ignore);");
         Put_New_Line;
         S_Put (0, "pragma Assertion_Policy (Ghost => Check);");
         Put_New_Line;
         Close_File;
      end if;

      if not Is_Regular_File (Harness_Dir.all & "suppress_no_ghost.adc") then
         Create (Harness_Dir.all & "suppress_no_ghost.adc");
         S_Put (0, "pragma Assertion_Policy (Pre => Ignore);");
         Put_New_Line;
         S_Put (0, "pragma Assertion_Policy (Post => Ignore);");
         Put_New_Line;
         Close_File;
      end if;
   end Generate_Global_Config_Pragmas_File;

   ----------------------------------
   -- Generate_Gnattest_Common_Prj --
   ----------------------------------

   procedure Generate_Gnattest_Common_Prj is
      Gnattest_Common_Prj : constant String := Gnattest_Common_Prj_Name;
   begin
      if Is_Regular_File (Gnattest_Common_Prj) then
         return;
      end if;
      Create (Gnattest_Common_Prj);

      S_Put (0, "abstract project Gnattest_Common is");
      Put_New_Line;
      S_Put (3, "for Languages use (""Ada"");");
      Put_New_Line;
      S_Put (3, "for Source_Files use ();");
      Put_New_Line;

      if GNATtest.Options.RTS_Path.all /= "" then
         if RTS_Attribute_Val = null then
            S_Put (3, "for Runtime (""Ada"") use """ &
                     GNATtest.Options.RTS_Path.all & """;");
         else
            S_Put
              (3,
               "for Runtime (""Ada"") use """ & RTS_Attribute_Val.all & """;");
         end if;
         Put_New_Line;
      end if;

      declare
         Firts_Idx : constant Natural := ASIS_UL.Common.Tool_Name'First;
         Last_Idx  : constant Natural :=
           Index (ASIS_UL.Common.Tool_Name.all, "-", Ada.Strings.Backward);
      begin
         if Last_Idx /= 0 then
            S_Put
              (3,
               "for Target use """
               & ASIS_UL.Common.Tool_Name (Firts_Idx .. Last_Idx - 1)
               & """;");
            Put_New_Line;
         end if;
      end;

      Put_New_Line;
      S_Put
        (3,
         "type TD_Compilation_Type is (""contract-checks"","
         & """no-contract-checks"", ""no-config-file"");");
      Put_New_Line;
      if Has_Test_Cases then
         S_Put
           (3,
            "TD_Compilation : TD_Compilation_Type := external "
            & "(""TEST_DRIVER_BUILD_MODE"", ""contract-checks"");");
      else
         S_Put
           (3,
            "TD_Compilation : TD_Compilation_Type := external "
            & "(""TEST_DRIVER_BUILD_MODE"", ""no-config-file"");");
      end if;
      Put_New_Line;

      Put_New_Line;
      S_Put (3, "package Builder is");
      Put_New_Line;
      S_Put (6, "case TD_Compilation is");
      Put_New_Line;
      S_Put (9, "when ""contract-checks"" =>");
      Put_New_Line;
      S_Put (12, "for Global_Configuration_Pragmas use ""suppress.adc"";");
      Put_New_Line;
      S_Put (9, "when ""no-contract-checks"" =>");
      Put_New_Line;
      S_Put
        (12,
         "for Global_Configuration_Pragmas use ""suppress_no_ghost.adc"";");
      Put_New_Line;
      S_Put (9, "when ""no-config-file"" =>");
      Put_New_Line;
      S_Put (12, "null;");
      Put_New_Line;
      S_Put (6, "end case;");
      Put_New_Line;
      S_Put (3, "end Builder;");
      Put_New_Line;
      Put_New_Line;

      S_Put (3, "package Linker is");
      Put_New_Line;
      S_Put (6, "for Default_Switches (""ada"") use (""-g"");");
      Put_New_Line;
      S_Put (3, "end Linker;");
      Put_New_Line;
      Put_New_Line;

      S_Put (3, "package Binder is");
      Put_New_Line;
      S_Put (6, "for Default_Switches (""ada"") use (""-E"", ""-static"");");
      Put_New_Line;
      S_Put (3, "end Binder;");
      Put_New_Line;
      Put_New_Line;

      S_Put (3, "Contract_Switches := ();");
      Put_New_Line;
      S_Put (3, "case TD_Compilation is");
      Put_New_Line;
      S_Put (6, "when ""contract-checks"" =>");
      Put_New_Line;
      S_Put (9, "Contract_Switches := (""-gnata"");");
      Put_New_Line;
      S_Put (6, "when others =>");
      Put_New_Line;
      S_Put (9, "null;");
      Put_New_Line;
      S_Put (3, "end case;");
      Put_New_Line;

      S_Put (3, "package Compiler is");
      Put_New_Line;
      S_Put (6, "for Default_Switches (""ada"") use");
      Put_New_Line;
      S_Put (8, "(""-g"", ""-gnatyM0""");
      declare
         Cur : List_Of_Strings.Cursor := Inherited_Switches.First;
      begin
         loop
            exit when Cur = List_Of_Strings.No_Element;
            S_Put (0, ", """ & List_Of_Strings.Element (Cur) & """");
            List_Of_Strings.Next (Cur);
         end loop;
         Inherited_Switches.Clear;
      end;
      S_Put (0, ") & Contract_Switches;");
      Put_New_Line;
      S_Put (3, "end Compiler;");
      Put_New_Line;
      Put_New_Line;

      if Stub_Mode_ON or else Separate_Drivers then
         S_Put (3, "package Ide is");
         Put_New_Line;
         S_Put (3, "end Ide;");
         Put_New_Line;
         S_Put (3, "package Make is");
         Put_New_Line;
         S_Put (3, "end Make;");
         Put_New_Line;
      end if;

      S_Put (0, "end Gnattest_Common;");
      Put_New_Line;
      Close_File;
   end Generate_Gnattest_Common_Prj;

   ----------------------------------------
   -- Generate_Stub_Test_Driver_Projects --
   ----------------------------------------

   procedure Generate_Stub_Test_Driver_Projects is
      P : Separate_Project_Info;

      Imported : List_Of_Strings.List;
      I_Cur    : List_Of_Strings.Cursor;
      S_Cur    : List_Of_Strings.Cursor;

      Tmp, Current_Infix : String_Access;

      package Srcs is new
        Ada.Containers.Indefinite_Ordered_Sets (String);
      use Srcs;

      Out_Dirs     : Srcs.Set;
      Out_Dirs_Cur : Srcs.Cursor;

      procedure Add_Nesting_Hierarchy_Dummies (S : String);
      --  For nested packages corresponding test packages are children to a
      --  dummy hierarchy replicating the original package nesting and
      --  descending from top-level test project. All those dummy packages
      --  should also be included as test driver source files.
      --  Analyzes test package name and acts accordingly.

      procedure Add_Nesting_Hierarchy_Dummies (S : String) is
         Idx, Idx2 : Integer;
      begin
         Idx := Index (S, Test_Data_Unit_Name);
         Idx2 := Index (S, Test_Data_Unit_Name, Idx + 1);

         if Idx2 = 0 then
            --  Not a nested test package.
            return;
         end if;

         Idx2 := Index (S, ".", Idx);
         if not
           Excluded_Test_Package_Bodies.Contains
             (Unit_To_File_Name
                (S (S'First .. Idx2 - 1)) & ".adb")
         then
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (S (S'First .. Idx2 - 1))
               & ".adb"",");
            Put_New_Line;
         end if;
         S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (S (S'First .. Idx2 - 1))
               & ".ads"",");
         Put_New_Line;

         Idx2 := Index (S, ".", Idx2 + 1);
         if not
           Excluded_Test_Package_Bodies.Contains
             (Unit_To_File_Name
                (S (S'First .. Idx2 - 1)) & ".adb")
         then
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (S (S'First .. Idx2 - 1))
               & ".adb"",");
            Put_New_Line;
         end if;
         S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (S (S'First .. Idx2 - 1))
               & ".ads"",");
         Put_New_Line;

         loop
            Idx2 := Index (S, ".", Idx2 + 1);
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (S (S'First .. Idx2 - 1))
               & ".ads"",");
            Put_New_Line;

            if
              Index (S, ".", Idx2 + 1) >
              Index (S, Test_Data_Unit_Name, Idx2 + 1)
            then
               --  Next package is the original test data one.
               exit;
            end if;
         end loop;
      end Add_Nesting_Hierarchy_Dummies;

   begin
      Trace (Me, "Generate_Stub_Test_Driver_Projects");
      Increase_Indent (Me);

      if Separate_Projects.Is_Empty then
         Report_Std
           ("gnattest: no test skeletons generated because "
            & "no subprogram to test");
         Report_Std
           ("found in project " & Source_Prj.all, 10);
         Report_Std ("cannot create main suite and test runner", 10);
         raise Fatal_Error;
      end if;

      if Stub_Mode_ON then
         GNATtest.Skeleton.Source_Table.Mark_Projects_With_Stubbed_Sources;
      end if;

      for
        K in Separate_Projects.First_Index .. Separate_Projects.Last_Index
      loop
         P := Separate_Projects.Element (K);

         Current_Infix := new String'(Get_Next_Infix);
         if Stub_Exclusion_Lists.Contains (Base_Name (P.UUT_File_Name.all))
         then
            GNATtest.Skeleton.Source_Table.Enforce_Custom_Project_Extention
              (P.UUT_File_Name.all, P.Path_Extending.all, Current_Infix.all);
         else
            GNATtest.Skeleton.Source_Table.Enforce_Project_Extention
              (P.Name_Of_Extended.all,
               P.Path_Extending.all,
               Current_Infix.all);
         end if;

         --  Extending project
         Create (P.Path_Extending.all);

         if Stub_Mode_ON then
            Imported :=
              GNATtest.Skeleton.Source_Table.Get_Imported_Projects
                (P.Name_Of_Extended.all);

            I_Cur := Imported.First;
            while I_Cur /= List_Of_Strings.No_Element loop
               if
                 GNATtest.Skeleton.Source_Table.Project_Extended
                   (List_Of_Strings.Element (I_Cur))
               then
                  Tmp := new String'(List_Of_Strings.Element (I_Cur));
                  declare
                     Imported_Stubbed_Path : constant String :=
                       GNATtest.Skeleton.Source_Table.Get_Project_Stub_Dir
                         (Tmp.all)
                       & Directory_Separator
                       & Unit_To_File_Name
                           (Stub_Project_Prefix & Current_Infix.all & Tmp.all)
                       & ".gpr";
                     Relative_P : constant String :=
                       +Relative_Path
                          (Create (+Imported_Stubbed_Path),
                           Create (+Dir_Name (P.Path_Extending.all)));
                  begin
                     S_Put
                       (0,
                        "with """
                        & Relative_P
                        & """;");
                     Put_New_Line;
                  end;
               end if;

               Free (Tmp);

               Next (I_Cur);
            end loop;
         end if;

         S_Put
           (0,
            "with """
              & (+Relative_Path
                 (Create (+Gnattest_Common_Prj_Name),
                   Create (+Dir_Name (P.Path_Extending.all))))
            & """;");
         Put_New_Line;
         Put_New_Line;
         S_Put
           (0,
            "project "
            & P.Name_Extending.all
            & " extends """
            & P.Path_Of_Extended.all
            & """ is");
         Put_New_Line;

         S_Put (3, "for Target use Gnattest_Common'Target;");
         Put_New_Line;
         Put_New_Line;
         S_Put
           (3,
            "for Runtime (""Ada"") use Gnattest_Common'Runtime (""Ada"");");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package Ide renames Gnattest_Common.Ide;");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package Make renames Gnattest_Common.Make;");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "for Languages use Gnattest_Common'Languages & (""Ada"");");
         Put_New_Line;

         declare
            Dir : File_Array_Access;
         begin
            Append
              (Dir,
               GNATCOLL.VFS.Create
                 (+(Dir_Name (P.Path_Extending.all)
                  & Directory_Separator
                  & P.Name_Extending.all
                  & "_lib")));
            Append
              (Dir,
               GNATCOLL.VFS.Create
                 (+(Dir_Name (P.Path_Extending.all)
                  & Directory_Separator
                  & P.Name_Extending.all
                  & "_obj")));
            Create_Dirs (Dir);
         exception
            when Directory_Error =>
               Report_Err
                 ("gnattest: cannot create obj/lib directory for "
                  & P.Path_Extending.all);
               raise Fatal_Error;
         end;

         S_Put
           (3,
            "for Library_Dir use """
            & P.Name_Extending.all
            & "_lib"";");
         Put_New_Line;
         S_Put
           (3,
            "for Object_Dir use """
            & P.Name_Extending.all
            & "_obj"";");
         Put_New_Line;
         Put_New_Line;
         if  P.Stub_Source_Dir = null then
            S_Put (3, "for Source_Dirs use ();");
         else

            S_Put
              (3,
               "for Source_Dirs use ("""
               & P.Stub_Source_Dir.all
               & """);");
         end if;
         Put_New_Line;
         Put_New_Line;

         S_Cur := P.Sources_List.First;
         if S_Cur /= List_Of_Strings.No_Element then
            S_Put (3, "for Source_Files use");
            Put_New_Line;

            while S_Cur /= List_Of_Strings.No_Element loop

               if S_Cur = P.Sources_List.First then
                  S_Put
                    (5,
                     "("""
                     & Base_Name (List_Of_Strings.Element (S_Cur))
                     & """");
               else
                  S_Put
                    (6,
                     """"
                     & Base_Name (List_Of_Strings.Element (S_Cur))
                     & """");
               end if;

               Next (S_Cur);

               if S_Cur = List_Of_Strings.No_Element then
                  S_Put (0, ");");
               else
                  S_Put (0, ",");
               end if;

               Put_New_Line;

            end loop;
         end if;

         S_Put (3, "package Compiler renames Gnattest_Common.Compiler;");

         Put_New_Line;
         Put_New_Line;

         S_Put
           (0,
            "end "
            & P.Name_Extending.all
            & ";");
         Close_File;

         --  Test driver project
         Create (P.Path_TD.all);

         S_Put (0, "with ""aunit"";");
         Put_New_Line;
         S_Put
           (0,
            "with """
              & (+Relative_Path
                 (Create (+Gnattest_Common_Prj_Name),
                   Create (+Dir_Name (P.Path_TD.all))))
            & """;");
         Put_New_Line;
         S_Put
           (0,
            "with """
            & Base_Name (P.Path_Extending.all)
            & """;");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (0,
            "project "
            & P.Name_TD.all
            & " is");
         Put_New_Line;
         S_Put
           (3,
            "for Origin_Project use """
            & (+Relative_Path
                 (Create (+P.Path_Of_Extended.all),
                   Create (+Normalize_Pathname (Dir_Name (P.Path_TD.all)))))
            & """;");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "for Languages use Gnattest_Common'Languages & (""Ada"");");
         Put_New_Line;

         S_Put (3, "for Target use Gnattest_Common'Target;");
         Put_New_Line;
         Put_New_Line;
         S_Put
           (3,
            "for Runtime (""Ada"") use Gnattest_Common'Runtime (""Ada"");");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package Ide renames Gnattest_Common.Ide;");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package Make renames Gnattest_Common.Make;");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (3,
            "for Main use ("""
            & P.Main_File_Name.all
            & """);");
         Put_New_Line;
         S_Put (3, "for Exec_Dir use ""."";");
         Put_New_Line;

         declare
            Dir : File_Array_Access;
         begin
            Append
              (Dir,
               GNATCOLL.VFS.Create
                 (+(Dir_Name (P.Path_TD.all)
                  & Directory_Separator
                  & P.Name_TD.all
                  & "_obj")));
            Create_Dirs (Dir);
         exception
            when Directory_Error =>
               Report_Err
                 ("gnattest: cannot create obj directory for "
                  & P.Path_TD.all);
               raise Fatal_Error;
         end;

         S_Put
           (3,
            "for Object_Dir use """
            & P.Name_TD.all
            & "_obj"";");
         Put_New_Line;

         GNATtest.Skeleton.Source_Table.Reset_Source_Iterator;
         loop
            Tmp := new String'
              (GNATtest.Skeleton.Source_Table.Next_Source_Name);
            exit when Tmp.all = "";

            if
              Is_Directory
                (GNATtest.Skeleton.Source_Table.Get_Source_Output_Dir
                   (Tmp.all))
            then
               Include
                 (Out_Dirs,
                  GNATtest.Skeleton.Source_Table.Get_Source_Output_Dir
                    (Tmp.all));
            end if;
            Free (Tmp);
         end loop;

         S_Put (3, "for Source_Dirs use");
         Put_New_Line;

         if Out_Dirs.Is_Empty then
            S_Put (5, "(""../common"", ""."");");

            Put_New_Line;
            Put_New_Line;
         else
            Out_Dirs_Cur := Out_Dirs.First;
            S_Put (5, "(""");
            S_Put
              (0,
               +Relative_Path
                 (Create (+Srcs.Element (Out_Dirs_Cur)),
                  Create (+Dir_Name (P.Path_TD.all))) &
                 """");
            loop
               Srcs.Next (Out_Dirs_Cur);
               exit when Out_Dirs_Cur = Srcs.No_Element;

               S_Put (0, ",");
               Put_New_Line;
               S_Put (6, """");
               S_Put
                 (0,
                  +Relative_Path
                    (Create (+Srcs.Element (Out_Dirs_Cur)),
                     Create (+Dir_Name (P.Path_TD.all))) &
                    """");

            end loop;
            S_Put (0, ",");
            Put_New_Line;
            S_Put (6, """../common"", ""."");");

            Put_New_Line;
            Put_New_Line;
         end if;

         if Stub_Mode_ON then
            S_Put
              (3, "for Source_Files use");
            Put_New_Line;
            S_Put
              (5, "(""gnattest_generated.ads"",");
            Put_New_Line;
            S_Put
              (6,
               """"
               & P.Main_File_Name.all
               & """,");
            Put_New_Line;
            Add_Nesting_Hierarchy_Dummies (P.Test_Package.all);
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (P.Test_Package.all)
               & ".adb"",");
            Put_New_Line;
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (P.Test_Package.all)
               & ".ads"",");
            Put_New_Line;
            if Driver_Per_Unit then
               S_Put
                 (6,
                  """"
                  & Unit_To_File_Name
                    (P.Test_Package.all & ".Suite")
                  & ".adb"",");
               Put_New_Line;
               S_Put
                 (6,
                  """"
                  & Unit_To_File_Name
                    (P.Test_Package.all & ".Suite")
                  & ".ads"",");
               Put_New_Line;
            end if;
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (P.Test_Data.all)
               & ".adb"",");
            Put_New_Line;
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (P.Test_Data.all)
               & ".ads"");");
            Put_New_Line;
            Put_New_Line;
         end if;

         S_Put (3, "package Builder renames Gnattest_Common.Builder;");
         Put_New_Line;
         S_Put (3, "package Linker renames Gnattest_Common.Linker;");
         Put_New_Line;
         S_Put (3, "package Binder renames Gnattest_Common.Binder;");
         Put_New_Line;
         S_Put (3, "package Compiler renames Gnattest_Common.Compiler;");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (0,
            "end "
            & P.Name_TD.all
            & ";");
         Close_File;
         Free (Current_Infix);
      end loop;

      Generate_Common_Harness_Files;

      Decrease_Indent (Me, "done");
   end Generate_Stub_Test_Driver_Projects;

   ----------------------
   --  Generate_Suite  --
   ----------------------

   procedure Generate_Suite (Data : Data_Holder; Path : String := "") is
      New_Unit_Name : String_Access;

      Current_Type : Test_Type_Info;

      File_Destination : constant String :=
        (if Path = "" then Harness_Dir.all else Path);
   begin

      if Data.Generic_Kind then

         New_Unit_Name := new String'(Data.Test_Unit_Full_Name.all &
                                      "."                          &
                                      Generic_Suite_Name);
      else

         New_Unit_Name := new String'(Data.Test_Unit_Full_Name.all &
                                      "."                          &
                                      Common_Suite_Name);
      end if;

      --  Creating test suite spec
      Create (File_Destination
              & Unit_To_File_Name (New_Unit_Name.all)
              & ".ads");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      S_Put (0, "with AUnit.Test_Suites;");
      if Data.Generic_Kind then
         S_Put (1, "use AUnit.Test_Suites;");
         Put_New_Line;
         S_Put (0, "with AUnit.Test_Caller;");
      end if;
      Put_New_Line;
      Put_New_Line;

      if Data.Generic_Kind then
         S_Put (0, "generic");
         Put_New_Line;
         S_Put (3, "Instance_Name : String;");
         Put_New_Line;
      end if;

      S_Put (0, "package " & New_Unit_Name.all & " is");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "function Suite return AUnit.Test_Suites.Access_Test_Suite;");
      Put_New_Line;
      Put_New_Line;

      if Data.Generic_Kind then

         for
           I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index
         loop
            S_Put (3, "package Runner_" & Positive_Image (I));
            S_Put (0, " is new AUnit.Test_Caller");
            Put_New_Line;

            S_Put (5,
                   "("                           &
                   Data.Test_Unit_Full_Name.all  &
                   "."                           &
                   Data.Test_Types.Element (I).Test_Type_Name.all &
                   ");");
            Put_New_Line;
            Put_New_Line;
         end loop;

         for K in Data.TR_List.First_Index .. Data.TR_List.Last_Index loop

            S_Put (3,
                   Data.TR_List.Element (K).TR_Text_Name.all &
                   "_" &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_Access : constant Runner_" &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   ".Test_Method :=");
            Put_New_Line;
            S_Put (5,
                   Data.TR_List.Element (K).TR_Text_Name.all &
                   "'Access;");
            Put_New_Line;

         end loop;

         for K in Data.ITR_List.First_Index .. Data.ITR_List.Last_Index loop

            S_Put
              (3,
               Data.ITR_List.Element (K).TR_Text_Name.all &
               "_" &
               Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
               "_Access : constant Runner_" &
               Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
               ".Test_Method :=");
            Put_New_Line;
            S_Put (5,
                   Data.ITR_List.Element (K).TR_Text_Name.all &
                   "'Access;");
            Put_New_Line;

         end loop;

         Put_New_Line;

      end if;

      S_Put (0, "end " & New_Unit_Name.all & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;
      Close_File;

      --  Creating test suite body
      Create (File_Destination                      &
              Unit_To_File_Name (New_Unit_Name.all) &
              ".adb");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      if not Data.Generic_Kind then
         S_Put (0, "with AUnit.Test_Caller;");
      end if;
      Put_New_Line;
      S_Put (0, "with Gnattest_Generated;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0,
             "package body "     &
             New_Unit_Name.all   &
             " is");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "use AUnit.Test_Suites;");
      Put_New_Line;
      Put_New_Line;

      if not Data.Generic_Kind then

         for
           I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index
         loop
            Current_Type := Data.Test_Types.Element (I);

            S_Put (3, "package Runner_" & Positive_Image (I));
            S_Put (0, " is new AUnit.Test_Caller");
            Put_New_Line;

            if
              Nesting_Difference
                (Current_Type.Nesting.all,
                 Data.Test_Unit_Full_Name.all) = ""
            then
               S_Put (5,
                      "(GNATtest_Generated.GNATtest_Standard." &
                      Data.Test_Unit_Full_Name.all    &
                      "."                             &
                      Current_Type.Test_Type_Name.all &
                      ");");
            else
               S_Put
                 (5,
                  "(GNATtest_Generated.GNATtest_Standard."     &
                  Data.Test_Unit_Full_Name.all     &
                  "."                              &
                  Nesting_Difference
                    (Current_Type.Nesting.all,
                     Data.Test_Unit_Full_Name.all) &
                  "."                              &
                  Current_Type.Test_Type_Name.all  &
                  ");");
            end if;

            Put_New_Line;
            Put_New_Line;
         end loop;

         S_Put (3, "Result : aliased AUnit.Test_Suites.Test_Suite;");

         Put_New_Line;
         Put_New_Line;

      end if;

      --  Declaring test cases for test routines

      --  Test case variables recieve unique numbers in order to
      --  escape name collisions for cases when test routines with
      --  same name and same test type are declared in different
      --  nested packages.

      for K in Data.TR_List.First_Index .. Data.TR_List.Last_Index loop

         if Data.Generic_Kind then

            S_Put (3,
                   Test_Case_Prefix                                         &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_"                                                      &
                   Data.TR_List.Element (K).TR_Text_Name.all                &
                   " : Runner_"                                             &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   ".Test_Case_Access;");
         else

            S_Put (3,
                   Test_Case_Prefix                                         &
                   Positive_Image (K)                                       &
                   "_"                                                      &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_"                                                      &
                   Data.TR_List.Element (K).TR_Text_Name.all                &
                   " : aliased Runner_"                                     &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   ".Test_Case;");
         end if;

         Put_New_Line;
      end loop;

      --  Declaring test cases for inherited test routines
      for K in Data.ITR_List.First_Index .. Data.ITR_List.Last_Index loop

         if Data.Generic_Kind then

            S_Put (3,
                   Test_Case_Prefix                                          &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "i_"                                                      &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   " : Runner_"                                              &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   ".Test_Case_Access;");
         else

            S_Put (3,
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "i_"                                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_"                                                       &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   " : aliased Runner_"                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   ".Test_Case;");
         end if;

         Put_New_Line;

      end loop;

      Put_New_Line;
      S_Put (3,
             "function Suite return AUnit.Test_Suites.Access_Test_Suite is");
      Put_New_Line;
      if Data.Generic_Kind then
         S_Put (6, "Result : constant Access_Test_Suite := new Test_Suite;");
         Put_New_Line;
      end if;
      S_Put (3, "begin");
      Put_New_Line;
      Put_New_Line;

      --  Creating test cases for test routines
      for K in Data.TR_List.First_Index .. Data.TR_List.Last_Index loop

         if Data.Generic_Kind then

            S_Put (6,
                   Test_Case_Prefix                                         &
                   Positive_Image (K)                                       &
                   "_"                                                      &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_"                                                      &
                   Data.TR_List.Element (K).TR_Text_Name.all                &
                   " :=");
            Put_New_Line;
            S_Put (8,
                   "Runner_"                                                &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   ".Create");
            Put_New_Line;
            S_Put (10,
                   " (Instance_Name &");
            Put_New_Line;
            S_Put (11,
                   " "" : "                                  &
                   Data.TR_List.Element (K).TR_Text_Name.all &
                   """,");
            Put_New_Line;
            S_Put (11,
                   Data.TR_List.Element (K).TR_Text_Name.all                &
                   "_"                                                      &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_Access);");

         else

            S_Put (6,
                   "Runner_"                                                &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   ".Create");
            Put_New_Line;
            S_Put (8,
                   "("                                                      &
                   Test_Case_Prefix                                         &
                   Positive_Image (K)                                       &
                   "_"                                                      &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_"                                                      &
                   Data.TR_List.Element (K).TR_Text_Name.all                &
                   ",");
            Put_New_Line;
            S_Put (9,
                   """"
                   & Data.TR_List.Element (K).Tested_Sloc.all
                   & """,");
            Put_New_Line;
            if
              Nesting_Difference
                (Data.TR_List.Element (K).Nesting.all,
                 Data.Test_Unit_Full_Name.all) /= ""
            then
               S_Put
                 (9,
                  Nesting_Difference
                    (Data.TR_List.Element (K).Nesting.all,
                     Data.Test_Unit_Full_Name.all)          &
                  "."                                       &
                  Data.TR_List.Element (K).TR_Text_Name.all &
                  "'Access);");
            else
               S_Put (9,
                      Data.TR_List.Element (K).TR_Text_Name.all &
                      "'Access);");
            end if;

         end if;

         Put_New_Line;

      end loop;

      --  Creating test cases for inherited test routines
      for K in Data.ITR_List.First_Index .. Data.ITR_List.Last_Index loop

         Current_Type := Data.Test_Types.Element
           (Data.ITR_List.Element (K).Test_Type_Numb);

         if Data.Generic_Kind then

            S_Put (6,
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "i_"                                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_"                                                       &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   " :=");
            Put_New_Line;
            S_Put (8,
                   "Runner_"                                                 &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   ".Create");
            Put_New_Line;
            S_Put (10, "(Instance_Name &");
            Put_New_Line;
            S_Put (11, """ (inherited from " &
                   Data.ITR_List.Element (K).TR_Rarent_Unit_Name.all    &
                   ") : "                                               &
                   Data.ITR_List.Element (K).TR_Text_Name.all           &
                   """,");
            Put_New_Line;
            S_Put (11,
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   "_"                                                       &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_Access);");
         else

            S_Put (6,
                   "Runner_"                                                 &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   ".Create");
            Put_New_Line;
            S_Put (8,
                   "("                                                       &
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "i_"                                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_"                                                       &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   ",");
            Put_New_Line;

            S_Put (9,
                   """"
                   & Data.ITR_List.Element (K).Tested_Sloc.all
                   & """,");
            Put_New_Line;
            if
              Nesting_Difference
                (Current_Type.Nesting.all, Data.Test_Unit_Full_Name.all) = ""
            then
               S_Put (9,
                      Data.ITR_List.Element (K).TR_Text_Name.all &
                      "'Access);");
            else
               S_Put
                 (9,
                  Nesting_Difference
                    (Current_Type.Nesting.all,
                     Data.Test_Unit_Full_Name.all)            &
                  "."                                         &
                   Data.ITR_List.Element (K).TR_Text_Name.all &
                  "'Access);");
            end if;

         end if;

         Put_New_Line;

      end loop;

      Put_New_Line;

      --  Adding test cases to the suite
      for K in Data.TR_List.First_Index .. Data.TR_List.Last_Index loop

         if Data.Generic_Kind then
            S_Put (6,
                   "Add_Test (Result'Access, "                               &
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "_"                                                       &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb)  &
                   "_"                                                       &
                   Data.TR_List.Element (K).TR_Text_Name.all                 &
                   ");");
         else
            S_Put (6,
                   "Add_Test (Result'Access, "                               &
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "_"                                                       &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb)  &
                   "_"                                                       &
                   Data.TR_List.Element (K).TR_Text_Name.all                 &
                   "'Access);");
         end if;

         Put_New_Line;

      end loop;

      --  Adding inherited test cases to the suite
      for K in Data.ITR_List.First_Index .. Data.ITR_List.Last_Index loop

         if Data.Generic_Kind then
            S_Put (6,
                   "Add_Test (Result'Access, "                               &
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "i_"                                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_"                                                       &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   ");");
         else
            S_Put (6,
                   "Add_Test (Result'Access, "                               &
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "i_"                                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_"                                                       &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   "'Access);");
         end if;

         Put_New_Line;

      end loop;

      Put_New_Line;

      for K in Data.TC_List.First_Index .. Data.TC_List.Last_Index loop
         S_Put
           (6,
            "Add_Test (Result'Access, new " &
            Data.TC_List.Element (K).Nesting.all &
            "." &
            Data.TC_List.Element (K).Name.all &
            ");");
         Put_New_Line;
      end loop;

      if Data.Generic_Kind then
         S_Put (6, "return Result;");
      else
         S_Put (6, "return Result'Access;");
      end if;
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "end Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "end " & New_Unit_Name.all & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;
      Close_File;

      if not Data.Generic_Kind then
         List_Of_Strings.Append (Suit_List, New_Unit_Name.all);
      end if;

   end Generate_Suite;

   -------------------------------
   --  Generate_Suite_Instance  --
   -------------------------------

   procedure Generate_Suite_Instance (Data : Data_Holder) is

      New_Unit_Name        : constant String :=
        Data.Test_Unit_Full_Name.all & "." & Instant_Suite_Name;
      New_Substitution_Unit_Name : constant String :=
        Data.Test_Unit_Full_Name.all & "." & Substitution_Instant_Suite_Name;

      Generate_Substitution : Boolean := False;

      Cur : List_Of_Strings.Cursor;
   begin

      Create (Harness_Dir.all                    &
              Unit_To_File_Name (New_Unit_Name) &
              ".ads");

      S_Put (0,
             "with "                     &
             Data.Gen_Unit_Full_Name.all &
             "."                         &
             Generic_Suite_Name          &
             ";");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "package " & New_Unit_Name & " is new");
      Put_New_Line;
      S_Put (2,
             Data.Test_Unit_Full_Name.all &
             "."                          &
             Generic_Suite_Name           &
             " ("""                       &
             Data.Test_Unit_Full_Name.all &
             """);");

      Close_File;

      List_Of_Strings.Append (Suit_List, New_Unit_Name);

      Cur := Good_For_Substitution_Inst.First;
      loop
         exit when Cur = List_Of_Strings.No_Element;

         if Data.Gen_Unit_Full_Name.all = List_Of_Strings.Element (Cur) then
            Generate_Substitution := True;
         end if;

         List_Of_Strings.Next (Cur);
      end loop;

      if not Generate_Substitution then
         return;
      end if;

      Create (Harness_Dir.all                           &
              Unit_To_File_Name (New_Substitution_Unit_Name) &
              ".ads");

      S_Put (0,
             "with "                     &
             Data.Gen_Unit_Full_Name.all &
             "."                         &
             Generic_Substitution_Suite_Name   &
             ";");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "package " & New_Substitution_Unit_Name & " is new");
      Put_New_Line;
      S_Put (2,
             Data.Test_Unit_Full_Name.all &
             "."                          &
             Generic_Substitution_Suite_Name    &
             " ("""                       &
             Data.Test_Unit_Full_Name.all &
             """);");

      List_Of_Strings.Append (Suit_List, New_Substitution_Unit_Name);

      Close_File;

   end Generate_Suite_Instance;

   ---------------------
   --  Get_Subp_Name  --
   ---------------------

   function Get_Subp_Name (Subp : Asis.Element) return String is
      Idx  : Integer;
   begin

      Idx := Names (Subp)'First;

      return Trim (To_String (Defining_Name_Image (Names (Subp) (Idx))),
                   Both);

   end Get_Subp_Name;

   -------------------------
   --  Inheritance_Depth  --
   -------------------------

   function Inheritance_Depth
     (Current_Type_Decl   : Asis.Element;
      Parent_Type_Decl    : Asis.Element)
      return Natural
   is

      Type_Decl : Asis.Element := Current_Type_Decl;
      Count : Natural := 0;

   begin

      loop

         Type_Decl := Parent_Type_Declaration (Type_Decl);

         Count := Count + 1;

         exit when
           Is_Equal (Parent_Type_Decl, Type_Decl) or else
           Is_Equal (Parent_Type_Decl,
                     Corresponding_Type_Declaration (Type_Decl));

      end loop;

      return Count;

   end Inheritance_Depth;

   --------------------------
   --  Initialize_Context  --
   --------------------------

   function Initialize_Context (Source_Name : String) return Boolean is
      Success : Boolean;

      use type Asis.Errors.Error_Kinds; --  for EC12-013
   begin
      Create_Tree (Get_Source_Full_Name (Source_Name), Success);

      if not Success then
         Set_Source_Status (Source_Name, Bad_Source);

         Report_Std ("gnattest: " & Source_Name &
                     " is not a legal Ada source");
         Source_Compilation_Failed := True;

         return False;

      end if;

      Last_Context_Name :=
        new String'(Get_Source_Suffixless_Name (Source_Name));

      Associate
       (The_Context => The_Context,
        Name        => "",
        Parameters  => "-C1 "
        & To_Wide_String (Get_Source_Suffixless_Name (Source_Name) & ".adt"));

      begin
         Open (The_Context);
         Success := True;
      exception
         when ASIS_Failed =>
            --  The only known situation when we can not open a C1 context for
            --  newly created tree is recompilation of System (see D617-017)

            if Asis.Implementation.Status = Asis.Errors.Use_Error
              and then
               Asis.Implementation.Diagnosis = "Internal implementation error:"
               & " Asis.Ada_Environments.Open - System is recompiled"
            then
               Report_Err
                 ("gnattest: can not process redefinition of System in " &
                    Source_Name);

               Set_Source_Status (Source_Name, Bad_Source);
               Success := False;
            else
               raise;
            end if;

      end;

      return Success;

   end Initialize_Context;

   ---------------------
   --  Is_AUnit_Part  --
   ---------------------

   function Is_AUnit_Part (Unit : Compilation_Unit) return Boolean is
      Full_Name : String_Access;
      J         : Natural;
   begin

      Full_Name := new
        String'(Trim (To_String (Unit_Full_Name (Unit)), Both));

      J := Full_Name.all'First;

      loop

         if Full_Name.all (J) = '.' then
            exit;
         end if;

         exit when J = Full_Name.all'Last;

         J := J + 1;

      end loop;

      if Full_Name.all (Full_Name.all'First .. J - 1) = "AUnit" then
         return True;
      else
         return False;
      end if;

   end Is_AUnit_Part;

   ------------------
   -- Is_Test_Case --
   ------------------

   function Is_Test_Case (Type_Decl : Asis.Element) return Boolean is
      Dec_Elem, Def_Elem : Asis.Element;
   begin

      Dec_Elem := Type_Decl;

      loop

         if
           To_String (Defining_Name_Image (First_Name (Dec_Elem))) =
           "Test_Case"
         then
            if Is_AUnit_Part (Enclosing_Compilation_Unit (Dec_Elem)) then
               return True;
            end if;
         end if;

         Def_Elem := Type_Declaration_View (Dec_Elem);

         if Definition_Kind (Def_Elem) = A_Private_Extension_Definition then

            Dec_Elem := Corresponding_Type_Declaration (Dec_Elem);
            Def_Elem := Type_Declaration_View (Dec_Elem);
         end if;

         if Type_Kind (Def_Elem) = A_Tagged_Record_Type_Definition then
            exit;
         end if;

         if Definition_Kind (Def_Elem) = A_Tagged_Private_Type_Definition then
            Dec_Elem := Corresponding_Type_Declaration (Dec_Elem);
            exit;
         end if;

         Dec_Elem := Corresponding_Parent_Subtype (Def_Elem);

      end loop;

      return False;
   end Is_Test_Case;

   -----------------------------
   -- Is_Test_Fixture_Routine --
   -----------------------------

   function Is_Test_Fixture_Routine (Subp : Asis.Element) return Boolean is
      Params     : constant Parameter_Specification_List :=
        Parameter_Profile (Subp);

      Param_Type : Asis.Element;

      Dec_Elem, Def_Elem : Asis.Element;

   begin
      if not Is_Test_Routine (Subp) then
         return False;
      end if;

      Param_Type := Object_Declaration_View (Params (Params'First));

      if Definition_Kind (Param_Type) = An_Access_Definition then
         return False;
      end if;

      Param_Type :=
        Corresponding_Name_Declaration (Normalize_Reference (Param_Type));

      Dec_Elem := Param_Type;
      loop

         if
           To_String (Defining_Name_Image (First_Name (Dec_Elem))) =
           "Test_Case"
         then
            if Is_AUnit_Part (Enclosing_Compilation_Unit (Dec_Elem)) then
               return False;
            end if;
         end if;

         Def_Elem := Type_Declaration_View (Dec_Elem);

         if Definition_Kind (Def_Elem) = A_Private_Extension_Definition then

            Dec_Elem := Corresponding_Type_Declaration (Dec_Elem);
            Def_Elem := Type_Declaration_View (Dec_Elem);
         end if;

         if Type_Kind (Def_Elem) = A_Tagged_Record_Type_Definition then
            exit;
         end if;

         if Definition_Kind (Def_Elem) = A_Tagged_Private_Type_Definition then
            Dec_Elem := Corresponding_Type_Declaration (Dec_Elem);
            exit;
         end if;

         if
           Type_Kind (Type_Declaration_View (Dec_Elem)) /=
           A_Derived_Record_Extension_Definition
         then
            return False;
         end if;

         Dec_Elem := Corresponding_Parent_Subtype (Def_Elem);

      end loop;

      return True;
   end Is_Test_Fixture_Routine;

   -----------------------
   --  Is_Test_Routine  --
   -----------------------

   function Is_Test_Routine (Subp : Asis.Element) return Boolean is
      Subp_Name  : constant String :=
        To_String (Defining_Name_Image (First_Name (Subp)));
   begin
      if not Is_Test_Related (Subp) then
         return False;
      end if;

      --  Checking for predefined AUnit set up and tear down routines.
      if Subp_Name = "Set_Up" then
         return False;
      end if;

      if Subp_Name = "Set_Up_Case" then
         return False;
      end if;

      if Subp_Name = "Tear_Down" then
         return False;
      end if;

      if Subp_Name = "Tear_Down_Case" then
         return False;
      end if;

      return True;
   end Is_Test_Routine;

   -----------------------
   --  Is_Test_Related  --
   -----------------------

   function Is_Test_Related (Subp : Asis.Element) return Boolean is

      Params     : constant Parameter_Specification_List :=
        Parameter_Profile (Subp);

      Param_Type : Asis.Element;

   begin

      if Params'Length /= 1 then
         return False;
      end if;

      if Is_AUnit_Part (Enclosing_Compilation_Unit (Subp)) then
         return False;
      end if;

      Param_Type := Object_Declaration_View (Params (Params'First));

      if Definition_Kind (Param_Type) = An_Access_Definition then
         return False;
      end if;

      Param_Type :=
        Corresponding_Name_Declaration (Normalize_Reference (Param_Type));

      Param_Type := Root_Type_Declaration (Param_Type);

      if Is_Nil (Param_Type) then
         return False;
      end if;

      if not Is_AUnit_Part (Enclosing_Compilation_Unit (Param_Type)) then
         return False;
      end if;

      return True;

   end Is_Test_Related;

   ------------------------
   -- Nesting_Difference --
   ------------------------

   function Nesting_Difference
     (Nesting_1, Nesting_2 : String) return String
   is
      L : constant Integer := Integer'Min (Nesting_1'Length, Nesting_2'Length);
   begin

      if Nesting_1'Length > Nesting_2'Length then
         return Nesting_1 (Nesting_1'First + L + 1 .. Nesting_1'Last);
      else
         return Nesting_2 (Nesting_2'First + L + 1 .. Nesting_2'Last);
      end if;

   end Nesting_Difference;

   -------------------------------
   --  Parent_Type_Declaration  --
   -------------------------------

   function Parent_Type_Declaration
     (Type_Dec : Asis.Element) return Asis.Element
   is
      Dec_Elem : Asis.Element := Type_Dec;
      Def_Elem : Asis.Element;
   begin

      Def_Elem := Type_Declaration_View (Dec_Elem);

      if Definition_Kind (Def_Elem) = A_Private_Extension_Definition then

         Dec_Elem := Corresponding_Type_Declaration (Dec_Elem);
         Def_Elem := Type_Declaration_View (Dec_Elem);
      end if;

      Dec_Elem := Corresponding_Parent_Subtype (Def_Elem);

      return Dec_Elem;

   exception

      when Asis.Exceptions.ASIS_Inappropriate_Element =>
         return Asis.Nil_Element;

   end Parent_Type_Declaration;

   ----------------------
   --  Positive_Image  --
   ----------------------

   function Positive_Image (P : Positive) return String is
   begin
      return Trim (Positive'Image (P), Both);
   end Positive_Image;

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source (The_Unit : Asis.Compilation_Unit) is
      Source_Name : String_Access;
      Apropriate_Source : Boolean := True;

      Data : Data_Holder;
   begin

      Source_Name :=
        new String'(Base_Name (To_String (Text_Name (The_Unit))));

      if Harness_Only then
         Report_Source (Source_Name.all);
      end if;

      Gather_Data (The_Unit, Data, Apropriate_Source);

      if Apropriate_Source then

         case Data.Data_Kind is

            when Instantination_Data =>

               Generate_Suite_Instance (Data);
               Set_Source_Status (Data.Test_Unit_File_Name.all, Processed);
               return;

            when others =>
               null;

         end case;

         if Data.Good_For_Suite then
            Generate_Suite (Data);
            null;
         end if;

         if not Data.Generic_Kind then
            if
              Substitution_Suite        and
              Data.Good_For_Suite and not
              Data.TR_List.Is_Empty
            then
               Gather_Liskiv_Data (Data);
               if not Data.LTR_List.Is_Empty then
                  Data.Good_For_Substitution  := True;

               else
                  Data.Good_For_Substitution  := False;
               end if;
            else
               Data.Good_For_Substitution  := False;
            end if;

            if Substitution_Suite and then Data.Good_For_Substitution  then
               Generate_Substitution_Suite_From_Tests (Data);
            end if;
         end if;

      end if;

      Data.Test_Types.Clear;
      Data.TR_List.Clear;
      Data.ITR_List.Clear;
      Data.LTR_List.Clear;
      Data.TC_List.Clear;

   end Process_Source;

   -----------------------
   --  Process_Sources  --
   -----------------------

   procedure Process_Sources is
      Source_Name : String_Access;
      Successful_Initialization : Boolean := True;
      The_Unit : Asis.Compilation_Unit;

      procedure Iterate_Sources (All_CU : Asis.Compilation_Unit_List);
      --  iterates through compilation units and checks if they are present in
      --  the source table, if so - processes them.

      procedure Iterate_Sources (All_CU : Asis.Compilation_Unit_List) is
         File_Name : String_Access;
      begin

         for J in All_CU'Range loop

            if Unit_Origin (All_CU (J)) = An_Application_Unit then
               File_Name :=
                 new String'(Base_Name (To_String (Text_Name (All_CU (J)))));

               if Source_Present (File_Name.all) and then
                 Get_Source_Status (File_Name.all) = Waiting
               then
                  Process_Source (All_CU (J));
               end if;

               Free (File_Name);
            end if;
         end loop;

      end Iterate_Sources;

   begin

      if SF_Table_Empty then
         if Harness_Only then
            Report_Err ("gnattest: no tests to generate suites for");
            raise Fatal_Error;
         else
            return;
         end if;
      end if;

      Asis.Implementation.Initialize ("-asis05 -ws");

      loop
         Source_Name := new String'(Next_Non_Processed_Source);
         exit when Source_Name.all = "";

         Successful_Initialization := Initialize_Context (Source_Name.all);

         if Successful_Initialization then

            The_Unit := Main_Unit_In_Current_Tree (The_Context);

            --  processing main unit
            Process_Source (The_Unit);

            --  processing others in same context
            Iterate_Sources
              (Asis.Compilation_Units.Compilation_Units (The_Context));

         end if;

         Source_Clean_Up;
         Context_Clean_Up;
         Free (Source_Name);
      end loop;

      Asis.Implementation.Finalize;

      Test_Runner_Generator;
      Project_Creator;
      if Harness_Only then
         if not Gnattest_Generated_Present then
            Generate_Common_File;
         end if;
      end if;

   end Process_Sources;

   -----------------------
   --  Project_Creator  --
   -----------------------

   procedure Project_Creator
   is
      Tmp : String_Access;
   begin
      Generate_Gnattest_Common_Prj;

      Create (Harness_Dir.all & "test_driver.gpr");

      if Tmp_Test_Prj /= null then
         S_Put (0, "with """                 &
                Base_Name (Tmp_Test_Prj.all) &
                """;");
         Put_New_Line;
      end if;

      if Harness_Only then
         S_Put (0, "with """     &
                  Source_Prj.all &
                  """;");
         Put_New_Line;
      end if;

      if Additional_Tests_Prj /= null then
         S_Put (0, "with """     &
                  Additional_Tests_Prj.all &
                  """;");
         Put_New_Line;
      end if;

      S_Put (0, "with ""gnattest_common.gpr"";");
      Put_New_Line;

      Put_New_Line;
      S_Put (0, "project Test_Driver is");
      Put_New_Line;
      Put_New_Line;
      S_Put
           (3,
            "for Origin_Project use """
            & (+Relative_Path
                 (Create (+Source_Prj.all),
                   Create (+Normalize_Pathname (Harness_Dir.all))))
            & """;");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "for Target use Gnattest_Common'Target;");
      Put_New_Line;
      Put_New_Line;
      S_Put
        (3,
         "for Runtime (""Ada"") use Gnattest_Common'Runtime (""Ada"");");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "for Languages use (""Ada"");");
      Put_New_Line;
      S_Put (3, "for Main use (""test_runner.adb"");");
      Put_New_Line;

      if Source_Prj.all = "" then

         Reset_Location_Iterator;
         S_Put (3, "for Source_Dirs use");
         Put_New_Line;
         S_Put (5, "(""" & Next_Source_Location & """");

         loop
            Tmp := new String'(Next_Source_Location);

            if Tmp.all = "" then
               if Harness_Only and then not Gnattest_Generated_Present then
                  S_Put (0, ",");
                  Put_New_Line;
                  S_Put (6, """common""");
               end if;
               S_Put (0, ");");
               Put_New_Line;
               exit;
            else
               S_Put (0, ",");
               Put_New_Line;
               S_Put (6, """" & Tmp.all & """");
               Put_New_Line;
            end if;

         end loop;

      else
         if Harness_Only and then not Gnattest_Generated_Present then
            S_Put (3, "for Source_Dirs use (""."", ""common"");");
            Put_New_Line;
         end if;

      end if;

      S_Put (3, "for Exec_Dir use ""."";");
      Put_New_Line;
      Put_New_Line;

      S_Put (3, "package Builder renames Gnattest_Common.Builder;");
      Put_New_Line;
      S_Put (3, "package Linker renames Gnattest_Common.Linker;");
      Put_New_Line;
      S_Put (3, "package Binder renames Gnattest_Common.Binder;");
      Put_New_Line;
      S_Put (3, "package Compiler renames Gnattest_Common.Compiler;");
      Put_New_Line;
      Put_New_Line;

      if IDE_Package_Present then
         S_Put
           (3,
            "package Ide renames " &
            Test_Prj_Prefix &
            Base_Name (Source_Prj.all, File_Extension (Source_Prj.all)) &
            ".Ide;");
         Put_New_Line;
         Put_New_Line;
      end if;

      if Make_Package_Present then
         S_Put
           (3,
            "package Make renames " &
            Test_Prj_Prefix &
            Base_Name (Source_Prj.all, File_Extension (Source_Prj.all)) &
            ".Make;");
         Put_New_Line;
         Put_New_Line;
      end if;

      if not Harness_Only then
         S_Put (3, "package GNATtest is");
         Put_New_Line;
         S_Put (6, "for GNATTest_Mapping_File use ""gnattest.xml"";");
         Put_New_Line;
         S_Put (3, "end GNATtest;");
         Put_New_Line;
         Put_New_Line;
      end if;

      S_Put (0, "end Test_Driver;");

      Close_File;

      Generate_Global_Config_Pragmas_File;
   end Project_Creator;

   -----------------------------
   --  Root_Type_Declaration  --
   -----------------------------

   function Root_Type_Declaration
     (Type_Dec : Asis.Element) return Asis.Element
   is
      Dec_Elem : Asis.Element := Type_Dec;
      Def_Elem : Asis.Element;
   begin

      loop

         Def_Elem := Type_Declaration_View (Dec_Elem);

         if Definition_Kind (Def_Elem) = A_Private_Extension_Definition then

            Dec_Elem := Corresponding_Type_Declaration (Dec_Elem);
            Def_Elem := Type_Declaration_View (Dec_Elem);
         end if;

         if Type_Kind (Def_Elem) = A_Tagged_Record_Type_Definition then
            return Dec_Elem;
         end if;

         if Definition_Kind (Def_Elem) = A_Tagged_Private_Type_Definition then
            Dec_Elem := Corresponding_Type_Declaration (Dec_Elem);
            return Dec_Elem;
         end if;

         Dec_Elem := Corresponding_Parent_Subtype (Def_Elem);

      end loop;

   exception
      when Asis.Exceptions.ASIS_Inappropriate_Element =>
         return Asis.Nil_Element;

   end Root_Type_Declaration;

   -----------------------
   --  Source_Clean_Up  --
   -----------------------

   procedure Source_Clean_Up is
      Success : Boolean;
   begin
      if Last_Context_Name = null then
         return;
      end if;

      Delete_File (Last_Context_Name.all & ".adt", Success);
      if not Success then
         Report_Std ("gnattest: cannot delete " &
                     Last_Context_Name.all & ".adt");
      end if;

      Delete_File (Last_Context_Name.all & ".ali", Success);
      if not Success then
         Report_Std ("gnattest: cannot delete " &
                     Last_Context_Name.all & ".ali");
      end if;

      Free (Last_Context_Name);
   end Source_Clean_Up;

   -----------------------------
   --  Test_Runner_Generator  --
   -----------------------------

   procedure Test_Runner_Generator is
      Iterator : List_Of_Strings.Cursor;
   begin
      if List_Of_Strings.Is_Empty (Suit_List) then
         Report_Std
           ("gnattest: no test skeletons generated because "
            & "no subprogram to test");
         Report_Std
           ("found in project " & Source_Prj.all, 10);
         Report_Std ("cannot create main suite and test runner", 10);
         raise Fatal_Error;
      end if;

      --  creating main suite spec
      Create (Harness_Dir.all & Unit_To_File_Name (Main_Suite_Name) & ".ads");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      S_Put (0, "with AUnit.Test_Suites; use AUnit.Test_Suites;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "package " & Main_Suite_Name & " is");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "function Suite return Access_Test_Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "end " & Main_Suite_Name & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;
      Close_File;

      --  creating main suite body
      Create
        (Harness_Dir.all & Unit_To_File_Name (Main_Suite_Name) & ".adb");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      Iterator := List_Of_Strings.First (Suit_List);
      loop
         exit when Iterator = List_Of_Strings.No_Element;

         S_Put
           (0,
            "with " & List_Of_Strings.Element (Iterator) & ";");
         Put_New_Line;

         List_Of_Strings.Next (Iterator);
      end loop;

      Put_New_Line;
      S_Put (0, "package body " & Main_Suite_Name & " is");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "Result : aliased AUnit.Test_Suites.Test_Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put
        (3,
         "function Suite return AUnit.Test_Suites." & "Access_Test_Suite is");
      Put_New_Line;
      S_Put (3, "begin");
      Put_New_Line;
      Put_New_Line;

      Iterator := List_Of_Strings.First (Suit_List);
      loop
         exit when Iterator = List_Of_Strings.No_Element;

         S_Put
           (6,
            "Add_Test (Result'Access, " &
            List_Of_Strings.Element (Iterator) &
            ".Suite);");
         Put_New_Line;

         List_Of_Strings.Next (Iterator);
      end loop;

      Put_New_Line;
      S_Put (6, "return Result'Access;");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "end Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "end " & Main_Suite_Name & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;
      Close_File;

      --  creating test runner body
      Create
        (Harness_Dir.all & Unit_To_File_Name (Test_Runner_Name) & ".adb");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      S_Put (0, "with AUnit.Reporter." & Reporter_Name.all & ";");
      Put_New_Line;
      S_Put (0, "with AUnit.Run;");
      Put_New_Line;
      S_Put (0, "with AUnit.Options; use AUnit.Options;");
      Put_New_Line;
      S_Put (0, "with " & Main_Suite_Name & "; use " & Main_Suite_Name & ";");
      Put_New_Line;
      Put_New_Line;
      if not No_Command_Line then
         S_Put (0, "with AUnit; use AUnit;");
         Put_New_Line;
         S_Put (0, "with Ada.Command_Line;");
         Put_New_Line;
      end if;
      if not No_Command_Line then
         S_Put (0, "with GNAT.Command_Line; use GNAT.Command_Line;");
         Put_New_Line;
         Put_New_Line;
         if not Harness_Only then
            S_Put (0, "with Gnattest_Generated;");
            Put_New_Line;
            Put_New_Line;
         end if;
      end if;
      S_Put (0, "procedure " & Test_Runner_Name & " is");
      Put_New_Line;
      if No_Command_Line then
         S_Put (3, "procedure Runner is new AUnit.Run.Test_Runner (Suite);");
      else
         S_Put
           (3,
            "function Runner is new "
            & "AUnit.Run.Test_Runner_With_Status (Suite);");
         Put_New_Line;
         S_Put (3, "Exit_Status : AUnit.Status;");
         Put_New_Line;
         if Add_Exit_Status then
            S_Put (3, "Use_Exit_Status : Boolean := True;");
         else
            S_Put (3, "Use_Exit_Status : Boolean := False;");
         end if;
      end if;
      Put_New_Line;
      S_Put
        (3,
         "Reporter : AUnit.Reporter."
         & Reporter_Name.all
         & "."
         & Reporter_Name.all
         & "_Reporter;");
      Put_New_Line;
      S_Put (3, "GT_Options : AUnit_Options := Default_Options;");
      Put_New_Line;
      S_Put (0, "begin");
      Put_New_Line;
      Put_New_Line;
      if Show_Passed_Tests then
         S_Put (3, "GT_Options.Report_Successes := True;");
      else
         S_Put (3, "GT_Options.Report_Successes := False;");
      end if;
      Put_New_Line;
      if Show_Test_Duration then
         S_Put (3, "GT_Options.Test_Case_Timer := True;");
      end if;
      Put_New_Line;
      if not No_Command_Line then
         S_Put (3, "begin");
         Put_New_Line;
         S_Put (6, "Initialize_Option_Scan;");
         Put_New_Line;
         S_Put (6, "loop");
         Put_New_Line;
         S_Put (9, "case GNAT.Command_Line.Getopt");
         Put_New_Line;
         if Harness_Only then
            --  No point in --skeleton-default in --harness-only mode.
            S_Put
              (11, "(""-passed-tests= -exit-status="")");
         else
            S_Put
              (11, "(""-skeleton-default= -passed-tests= -exit-status="")");
         end if;
         Put_New_Line;
         S_Put (9, "is");
         Put_New_Line;
         S_Put (12, "when ASCII.NUL =>");
         Put_New_Line;
         S_Put (15, "exit;");
         Put_New_Line;
         S_Put (12, "when '-' =>");
         Put_New_Line;
         --  --skeleton-default
         if not Harness_Only then
            S_Put (15, "if Full_Switch = ""-skeleton-default"" then");
            Put_New_Line;
            S_Put (18, "if Parameter = ""pass"" then");
            Put_New_Line;
            S_Put (21, "Gnattest_Generated.Default_Assert_Value := True;");
            Put_New_Line;
            S_Put (18, "elsif Parameter = ""fail"" then");
            Put_New_Line;
            S_Put (21, "Gnattest_Generated.Default_Assert_Value := False;");
            Put_New_Line;
            S_Put (18, "end if;");
            Put_New_Line;
            S_Put (15, "end if;");
            Put_New_Line;
         end if;
         --  --passed-tests
         S_Put (15, "if Full_Switch = ""-passed-tests"" then");
         Put_New_Line;
         S_Put (18, "if Parameter = ""show"" then");
         Put_New_Line;
         S_Put (21, "GT_Options.Report_Successes := True;");
         Put_New_Line;
         S_Put (18, "elsif Parameter = ""hide"" then");
         Put_New_Line;
         S_Put (21, "GT_Options.Report_Successes := False;");
         Put_New_Line;
         S_Put (18, "end if;");
         Put_New_Line;
         S_Put (15, "end if;");
         Put_New_Line;
         --  --exit-status
         S_Put (15, "if Full_Switch = ""-exit-status"" then");
         Put_New_Line;
         S_Put (18, "if Parameter = ""on"" then");
         Put_New_Line;
         S_Put (21, "Use_Exit_Status := True;");
         Put_New_Line;
         S_Put (18, "elsif Parameter = ""off"" then");
         Put_New_Line;
         S_Put (21, "Use_Exit_Status := False;");
         Put_New_Line;
         S_Put (18, "end if;");
         Put_New_Line;
         S_Put (15, "end if;");
         Put_New_Line;
         S_Put (12, "when others => null;");
         Put_New_Line;
         S_Put (9, "end case;");
         Put_New_Line;
         S_Put (6, "end loop;");
         Put_New_Line;
         S_Put (3, "exception");
         Put_New_Line;
         S_Put (6, "when GNAT.Command_Line.Invalid_Switch => null;");
         Put_New_Line;
         S_Put (3, "end;");
         Put_New_Line;
         Put_New_Line;
      end if;
      if No_Command_Line then
         S_Put (3, "Runner (Reporter, GT_Options);");
      else
         S_Put (3, "Exit_Status := Runner (Reporter, GT_Options);");
         Put_New_Line;
         S_Put
           (3,
            "if Use_Exit_Status and then Exit_Status = AUnit.Failure then");
         Put_New_Line;
         S_Put
           (6,
            "Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);");
         Put_New_Line;
         S_Put (3, "end if;");
      end if;
      Put_New_Line;
      S_Put (0, "end " & Test_Runner_Name & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;
      Close_File;

   end Test_Runner_Generator;

   -----------------------
   -- Type_Test_Package --
   -----------------------

   function Type_Test_Package (Elem : Asis.Element) return String
   is
      Type_Nesting : constant String := Get_Nesting (Elem);
      Package_Name : constant String :=
        To_String (Unit_Full_Name (Enclosing_Compilation_Unit (Elem)));
   begin
      if Type_Nesting = Package_Name then
         return
           Package_Name & "." & Type_Name (Elem) &
           Test_Data_Unit_Name_Suff & "." &
           Type_Name (Elem) & Test_Unit_Name_Suff;
      end if;

      return
        Package_Name & "." &
        Test_Data_Unit_Name & "." &
        Test_Unit_Name & "." &
        Nesting_Difference
        (Type_Nesting,
         Package_Name) &
        "." &
        Type_Name (Elem) &
        Test_Data_Unit_Name_Suff &
        "." &
        Type_Name (Elem) &
        Test_Unit_Name_Suff;
   end Type_Test_Package;

end GNATtest.Harness.Generator;
