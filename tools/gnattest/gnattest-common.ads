------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                      G N A T T E S T . C O M M O N                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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

--  This package contains some general-purpose entities that are used by many
--  GNATtest components

pragma Ada_2012;

with Asis;
with Asis.Text;                   use Asis.Text;
with Asis.Elements;               use Asis.Elements;

with Ada.Exceptions;              use Ada.Exceptions;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;
with Ada.Sequential_IO;

with GNATCOLL.Projects;           use GNATCOLL.Projects;
with GNATCOLL.VFS;                use GNATCOLL.VFS;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;

package GNATtest.Common is

   procedure Report_Err (Message : String);
   --  Prints it's argument to the standard error output

   procedure Report_AUnit_Usage;
   --  Shows a message about AUnit being among the source files

   procedure Report_Std (Message : String; Offset : Integer := 0);
   --  Prints it's argument to the standard output with Offset spaces before.

   procedure Report_Unhandled_ASIS_Exception (Ex : Exception_Occurrence);
   --  Reports an unhandled ASIS exception

   procedure Report_Unhandled_Exception (Ex : Exception_Occurrence);
   --  Reports an unhandled non-ASIS exception

   procedure Generate_Common_File;
   --  Creates a file with package gnattest_generated which denotes the default
   --  skeletons behavior and declares renamins necessary to avoid name
   --  conflicts with tested sources.

   function Get_Subp_Name (Subp : Asis.Element) return String;
   --  if Subp is a subprigram declaration it will return subprogram's name;
   --  if Subp is an overloaded operator - it's text name

   Source_Project_Tree : GNATCOLL.Projects.Project_Tree;
   --  Source project file name. Used for extraction of source files and
   --  paths for compiler.

   The_Context : Asis.Context;
   --  The Context for all the processing. May be associated, opened, closed
   --  and dissociated several times during one tool run.

   Fatal_Error : exception;

   Tmp_Test_Prj : String_Access := null;

   Temp_Dir : String_Access;
   --  Contains the name of the temporary directory created by the metric tools
   --  for the tree files

   package Char_Sequential_IO is new Ada.Sequential_IO (Character);
   Output_File : Char_Sequential_IO.File_Type;

   procedure Create_Dirs (Target_Dirs : File_Array_Access);
   --  Creates given directories.

   procedure S_Put (Span : Natural; Text : String);
   --  Adds Span number spaces before the Text and prints it to Output_File

   procedure Create (Name : String);
   procedure Close_File;
   --  Wrappers for creating and closing output files.

   procedure Put_New_Line;
   --  Puts a unix-style terminator to the Output_File disregard from the
   --  current actual platform.

   function Unit_To_File_Name (Old : String) return String;
   --  Replaces dots with "-" and lowers the case of the letters.

   package List_Of_Strings is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Asis_Element_List is new
     Ada.Containers.Doubly_Linked_Lists (Asis.Element, Is_Equal);

   package String_Set is new
     Ada.Containers.Indefinite_Ordered_Sets (String);
   use String_Set;

   package String_To_String_Map is new
     Ada.Containers.Indefinite_Ordered_Maps (String, String);

   -------------
   -- Closure --
   -------------

   procedure Update_Closure;
   --  Update the source table with new entries based on closure recomputation.

   procedure Create_ALI (Source : String; Success : out Boolean);
   --  Invokes compiler on the given source like regular Create_Tree routines
   --  in harness and skeleton generators, but deletes the .adt file right
   --  away, leaving only the ALI behind for further closure updates.
   --  This is needed for i.e. library procedure declarations that import
   --  testable packages that are part of the closure.

   Excluded_Files : String_Set.Set;
   --  Due to dynamic nature of closure computation we may need to store
   --  the list of excluded sources till the very end.

   procedure Report_Exclusions_Not_Found;
   --  Report files from exclusion list that were not matched by any source
   --  and clears the list.

   --------------------
   -- Stub exclusion --
   --------------------

   Default_Stub_Exclusion_List : String_Set.Set :=
     String_Set.Empty_Set;
   package String_To_String_Set is new
     Ada.Containers.Indefinite_Ordered_Maps (String, String_Set.Set);
   use String_To_String_Set;
   Stub_Exclusion_Lists : String_To_String_Set.Map    :=
     String_To_String_Set.Empty_Map;

   procedure Store_Default_Excluded_Stub (Excluded : String);
   --  Store data on units that should not be stubbed for all UUTs.
   procedure Store_Excluded_Stub (Source : String; Excluded : String);
   --  Store data on units that should not be stubbed for given UUT.

   function Get_Next_Infix return String;
   --  Returns a numbered infix ("1_", "2_",..), increasing the number for
   --  each call.

   Inherited_Switches : List_Of_Strings.List;

   Excluded_Test_Package_Bodies : String_Set.Set;
   Excluded_Test_Data_Files     : String_Set.Set;

   function Get_Nesting (Elem : Asis.Element) return String;
   --  Returns the full package & protected prefix if the element.

   function Parent_Type_Declaration
     (Type_Dec : Asis.Element) return Asis.Element;
   --  Returns a corresponding parent type declaration for a given tagged type
   --  extension declaration.

   function First_Column_Number (Element : Asis.Element) return Line_Number;
   --  Returns the number on the first column of the first line of the element.

   procedure Put_Harness_Header;

   function Mangle_Hash_Full
     (Subp           : Asis.Declaration;
      Case_Sensitive : Boolean := False;
      N_Controlling  : Boolean := False;
      For_Stubs      : Boolean := False)
      return String;
   --  Returns full hash for given subprogram.

   function To_String_First_Name (Elem : Asis.Element) return String;

   function Substring_16 (S : String) return String is
     (S (S'First .. S'First + 15));
   function Substring_6 (S : String) return String is
     (S (S'First .. S'First + 5));

   function "<" (Left, Right : Asis.Element) return Boolean;

   ------------------------
   --  String constants  --
   ------------------------

   Test_Routine_Prefix      : constant String := "Test_";
   --  Prefix to each test routine

   Wrapper_Prefix           : constant String := "Wrap_";

   Stub_Type_Prefix         : constant String := "Stub_Data_Type_";

   Stub_Object_Prefix       : constant String := "Stub_Data_";

   Setter_Prefix            : constant String := "Set_Stub_";

   Stub_Result_Suffix       : constant String := "_Result";

   Stub_Counter_Var         : constant String := "Stub_Counter";

   Test_Unit_Name           : constant String := "Tests";
   --  Name of test child package for non-primitive tests.

   Test_Unit_Name_Suff      : constant String := "_Tests";
   --  Suffix for test packages that correspond to tagged record types.

   Gen_Test_Unit_Name       : constant String := "Gen_Tests";
   --  Name of generic test child package for non-primitive tests.

   Gen_Test_Unit_Name_Suff  : constant String := "_Gen_Tests";
   --  Suffix for generic test packages that correspond to tagged record types.

   Inst_Test_Unit_Name      : constant String := "Inst_Tests";
   --  Name of instatiation test child package.

   Test_Prj_Prefix          : constant String := "test_";
   --  Prefix of the output project file name.

   Test_Data_Unit_Name      : constant String := "Test_Data";

   Test_Data_Unit_Name_Suff : constant String := "_Test_Data";

   Stub_Data_Unit_Name      : constant String := "Stub_Data";

   Stub_Project_Prefix      : constant String := "Stub_";

   TD_Prefix                : constant String := "Driver_";
   TD_Prefix_Overriden      : constant String := "VTE_Driver_";

   Hash_Version             : constant String := "2.2";

   Closure_Subdir_Name      : constant String := "tmp_gnattest_closure";

   GT_Marker_Begin   : constant String := "--  begin read only";
   GT_Marker_End     : constant String := "--  end read only";

end GNATtest.Common;
