------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                     G N A T T E S T . O P T I O N S                      --
--                                                                          --
--                                 S p e c                                  --
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

--  This package defines a set of options for the tool.

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNATtest.Common;            use GNATtest.Common;

package GNATtest.Options is

   --  Input options:
   Source_Prj : String_Access := new String'("");
   --  Source project file.

   Arg_File_Name : String_Access := new String'("");
   --  list of test drivers

   --  gnattest run options:
   type GNATtest_Modes is (Aggregation, Generation);

   GNATtest_Mode : GNATtest_Modes;

   --  Output options:
   type Output_Mode is (Separate_Root, Subdir, Direct);

   RTS_Path : String_Access := new String'("");
   RTS_Attribute_Val : String_Access;

   Target : String_Access := new String'("");

   Default_Output_Mode : constant Output_Mode := Direct;

   Test_Dir_Prefix   : String_Access := new String'("");
   --  Prefix added to the names of source dirs to get test dirs names.

   Test_Dir_Suffix   : String_Access := new String'("_test");
   --  Suffix added to the names of source dirs to get test dirs names.

   Test_Subdir_Name  : String_Access := new String'("tests");
   --  Name of subdirectory to place test files in case of --sudbir option.

   Separate_Root_Dir : String_Access;
   --  The root directory to place the test file hierarchy in case of
   --  --separate-root option.

   Test_Dir_Name     : String_Access := new String'
     ("gnattest" & Directory_Separator & "tests");
   --  Name of default directory to place test files.

   Stub_Dir_Name     : String_Access := new String'
     ("gnattest" & Directory_Separator & "stubs");

   Quiet : Boolean := False;
   --  When true, supresses non-critical output.

   Verbose : Boolean := False;
   --  Verbose mode.

   Harness_Only : Boolean := False;
   --  Indicates than argument files should be treated as tests but not as
   --  tested ones.

   Skeletons_Fail : Boolean := True;
   --  Distinguishes the default behavior of skeletons.

   Show_Passed_Tests : Boolean := True;
   --  Distinguishes the default output of passed tests.

   Add_Exit_Status : Boolean := False;
   --  When true, generated test driver will set exit status according to
   --  the outcome of tests.

   Generate_Separates : Boolean := False;
   --  Indicates the mode in which tests packages should be generated.

   Transition : Boolean := False;
   --  Indicates that previously generated separates should be taken into
   --  account when creating monolith packages.

   Stub_Mode_ON : Boolean := False;
   --  Indicates whenever stubs should be generated or not.

   Default_Stub_Exclusion_List_File : String_Access            := null;
   Stub_Exclusion_List_Files        : String_To_String_Map.Map :=
     String_To_String_Map.Empty_Map;

   Separate_Drivers : Boolean := False;
   --  When true, multiple test drivers willbe generated.
   Separate_Drivers_Set_By_Switch : Boolean := False;

   Driver_Per_Unit : Boolean := True;

   Substitution_Suite   : Boolean := False;
   --  Whenever or not to genretate suites for overrden tests applying them
   --  to fixture containing object of descendant type.

   Inheritance_To_Suite : Boolean := True;
   --  Whether or not to add inherited tests that correspond to inherited
   --  primitives to the test suite for descendant type.

   Harness_Dir : String_Access := new String'
     ("gnattest" & Directory_Separator & "harness");
   Harness_Set_By_Switch : Boolean := False;

   Additional_Tests_Prj : String_Access := null;
   Additional_Tests_Map : String_Access := null;

   Gnattest_Generated_Present : Boolean := False;
   --  Indicates if any of the source projects already have
   --  gnattest_generated.ads so that it won't be duplicated.

   No_Command_Line : Boolean;
   --  For several runtimes GNAT.Command_Line should not be added
   --  to test_runner.adb

   No_Command_Line_Externally_Set : Boolean := False;

   Omit_Sloc : Boolean := False;
   --  When true, slocs of subprograms under test are not put in test packages.

   Show_Test_Duration : Boolean := False;
   --  When true, AUnit_Options.Test_Case_Timer is set to True in test runner.

   IDE_Package_Present : Boolean;

   Make_Package_Present : Boolean;

   Queues_Number : Positive := 1;

   Reporter_Name : String_Access := new String'("gnattest");

   Test_Case_Only : Boolean := False;

   Has_Test_Cases : Boolean := False;

   Strict_Execution : Boolean := False;
   --  Return failure exit status if at least one of the sources
   --  could not be compiled.

   Main_Units : String_Set.Set := String_Set.Empty_Set;

   Environment_Dir : String_Access := null;

   Aggregate_Subdir_Name : String_Access := new String'("");
   --  Used to prepend the names of test driver executables in
   --  test_drivers.list

end GNATtest.Options;
