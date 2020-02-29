------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--          G N A T T E S T  . S K E L E T O N . G E N E R A T O R          --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.SHA1;

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Projects;          use GNATCOLL.Projects;

with Asis;                       use Asis;
with Asis.Ada_Environments;      use Asis.Ada_Environments;
with Asis.Clauses;               use Asis.Clauses;
with Asis.Compilation_Units;     use Asis.Compilation_Units;
with Asis.Declarations;          use Asis.Declarations;
with Asis.Definitions;           use Asis.Definitions;
with Asis.Elements;              use Asis.Elements;
with Asis.Expressions;           use Asis.Expressions;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Errors;
with Asis.Exceptions;            use Asis.Exceptions;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;
with Asis.Implementation;
with Asis.Iterator;              use Asis.Iterator;
with Asis.Limited_Views;         use Asis.Limited_Views;
with Asis.Text;                  use Asis.Text;

with ASIS_UL.Common;
with ASIS_UL.Compiler_Options;   use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;

with GNATtest.Skeleton.Source_Table; use GNATtest.Skeleton.Source_Table;

with GNATtest.Common;            use GNATtest.Common;
with GNATtest.Options;           use GNATtest.Options;
with GNATtest.Environment;       use GNATtest.Environment;
with GNATtest.Mapping;           use GNATtest.Mapping;

with GNATtest.Harness.Generator;
with GNATtest.Stub.Generator;
with Ada.Containers;

package body GNATtest.Skeleton.Generator is

   Me                : constant Trace_Handle :=
     Create ("Skeletons", Default => Off);
   Me_Direct_Callees : constant Trace_Handle :=
     Create ("Skeletons.Direct_Callees", Default => Off);

   -------------------
   --  Minded Data  --
   -------------------

   New_Tests_Counter : Natural := 0;
   All_Tests_Counter : Natural := 0;

   package Tests_Per_Unit is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Natural);
   use Tests_Per_Unit;

   Test_Info : Tests_Per_Unit.Map;

   type Data_Kind_Type is
     (Declaration_Data,
      Instantiation);

   type Base_Type_Info is tagged record
      Main_Type_Elem            : Asis.Element := Asis.Nil_Element;
      Main_Type_Abstract        : Boolean;
      Main_Type_Text_Name       : String_Access;

      Has_Argument_Father       : Boolean;
      Argument_Father_Unit_Name : String_Access;
      Argument_Father_Type_Name : String_Access;
      Argument_Father_Nesting   : String_Access;

      Nesting                   : String_Access;

      Type_Number               : Positive;

      No_Default_Discriminant   : Boolean;
   end record;

   package Type_Info_Vect is new
     Ada.Containers.Indefinite_Vectors (Positive, Base_Type_Info);
   use Type_Info_Vect;

   use String_Set;

   type Test_Case_Mode is (Normal, Robustness);

   type Test_Case_Info is record
      Pre  : Asis_Element_List.List;
      Post : Asis_Element_List.List;

      Elem : Asis.Element;
      Name : String_Access;
      Mode : Test_Case_Mode;
      Req  : Asis.Element;
      Ens  : Asis.Element;

      Req_Image : String_Access;
      Ens_Image : String_Access;

      Params_To_Temp : String_Set.Set;

      Req_Line : String_Access;
      Ens_Line : String_Access;

      TC_Hash : String_Access;
   end record;

   type Subp_Info is record
      Subp_Declaration : Asis.Declaration;
      Subp_Text_Name   : String_Access;
      Subp_Name_Image  : String_Access;
      Subp_Mangle_Name : String_Access;
      Subp_Full_Hash   : String_Access;

      --  Those versions of hash are stored for compatibility reasons.
      --  Transitions from older versions of hash should be performed
      --  automatically.

      Subp_Hash_V1    : String_Access;
      --  Case-sensitive hash.
      Subp_Hash_V2_1  : String_Access;
      --  Non-controlling parameters with same root type as controlling ones
      --  are replaced with root type before hashing.

      Is_Abstract      : Boolean;
      Corresp_Type     : Natural;
      Nesting          : String_Access;

      Has_TC_Info      : Boolean := False;
      TC_Info          : Test_Case_Info;

      Is_Overloaded    : Boolean;
   end record;

   package Subp_Data_List is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Subp_Info);
   use Subp_Data_List;

   type Package_Info is record
      Name       : String_Access;
      Is_Generic : Boolean;
      Data_Kind  : Data_Kind_Type;
      Element    : Asis.Element;

      --  only used for instantiations
      Generic_Containing_Package : String_Access;
   end record;

   package Package_Info_List is new
     Ada.Containers.Doubly_Linked_Lists (Package_Info);
   use Package_Info_List;

   --  Info on overloading subprograms
   package Name_Frequency is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Natural);
   use Name_Frequency;

   use Asis_Element_List;

   type Data_Holder (Data_Kind : Data_Kind_Type := Declaration_Data) is record

      Unit : Asis.Compilation_Unit;
      --  CU itself.

      Unit_Full_Name : String_Access;
      --  Fully expanded Ada name of the CU.

      Unit_File_Name : String_Access;
      --  Full name of the file, containing the CU.

      case Data_Kind is
         --  Indicates which data storing structures are used, determines the
         --  way of suite generation.

         when Declaration_Data =>

            Is_Generic       : Boolean;
            --  Indicates if given argument package declaration is generic.

            Has_Simple_Case  : Boolean := False;
            --  Indicates if we have routines that are not primitives of any
            --  tagged type.

            Needs_Fixtures   : Boolean := False;
            --  Indicates if we need to unclude AUnit.Fixtures in the test
            --  package.

            Needs_Set_Up     : Boolean := False;
            --  Indicates if we need the Set_Up routine for at least one test
            --  type;

            Needs_Assertions : Boolean := False;
            --  Indicates if we need to include AUnit.Assertions into the body
            --  of the test package.

            Subp_List : Subp_Data_List.List;
            --  List of subprograms declared in the argument package
            --  declaration.

            Type_Data_List : Type_Info_Vect.Vector;
            --  Stores info on tagged records in the argument package
            --  declaration.

            Package_Data_List : Package_Info_List.List;
            --  Stores info of nested packages.

            Units_To_Stub : Asis_Element_List.List;
            --  List of direct dependancies of current unit.

            Subp_Name_Frequency : Name_Frequency.Map;

         when Instantiation =>

            Gen_Unit : Asis.Compilation_Unit;
            --  Generic CU that is instatinated into the given one.

            Gen_Unit_Full_Name : String_Access;
            --  Fully expanded Ada name of the generic CU.

            Gen_Unit_File_Name : String_Access;
            --  Name of file containing the generic CU.

      end case;

   end record;

   ----------------
   -- Suite Data --
   ----------------

   type Test_Type_Info_Wrapper is record
      TT_Info       : GNATtest.Harness.Generator.Test_Type_Info;
      Test_Package  : String_Access;
      Original_Type : Asis.Element := Asis.Nil_Element;
   end record;

   package TT_Info is new
     Ada.Containers.Indefinite_Vectors (Positive, Test_Type_Info_Wrapper);
   use TT_Info;

   type Test_Routine_Info_Wrapper is record
      TR_Info       : GNATtest.Harness.Generator.Test_Routine_Info;
      Test_Package  : String_Access;
      Original_Type : Asis.Element := Asis.Nil_Element;
      Original_Subp : Asis.Element := Asis.Nil_Element;
   end record;

   package TR_Info is new
     Ada.Containers.Indefinite_Vectors (Positive, Test_Routine_Info_Wrapper);
   use TR_Info;

   type Test_Routine_Info_Enhanced_Wrapper is record
      TR_Info       : GNATtest.Harness.Generator.Test_Routine_Info_Enhanced;
      Test_Package  : String_Access;
      Original_Type : Asis.Element := Asis.Nil_Element;
   end record;

   package TR_Info_Enhanced is new
     Ada.Containers.Indefinite_Vectors (Positive,
                                        Test_Routine_Info_Enhanced_Wrapper);
   use TR_Info_Enhanced;

   type Suites_Data_Type is record
      Test_Types   : TT_Info.Vector;
      TR_List      : TR_Info.Vector;
      ITR_List     : TR_Info_Enhanced.Vector;
      LTR_List     : TR_Info_Enhanced.Vector;
   end record;

   ------------------
   -- Test Mapping --
   ------------------

   use TC_Mapping_List;
   use TR_Mapping_List;
   use DT_Mapping_List;
   use TP_Mapping_List;
   use SP_Mapping;

   procedure Add_TR
     (TP_List : in out TP_Mapping_List.List;
      TPtarg  : String;
      Test_F  : String;
      Test_T  : String;
      Subp    : Subp_Info;
      TR_Line : Natural := 1);

   procedure Add_DT
     (TP_List : in out TP_Mapping_List.List;
      TPtarg  : String;
      Test_F  : String;
      Line    : Natural;
      Column  : Natural);

   --------------
   -- Geberics --
   --------------
   package Element_List is new
     Ada.Containers.Doubly_Linked_Lists (Asis.Element, Is_Equal);

   package Name_Set is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Positive);

   use List_Of_Strings;

   type Generic_Tests is record
      Gen_Unit_Full_Name : String_Access;
      Tested_Type_Names  : List_Of_Strings.List;
      Has_Simple_Case    : Boolean := False;
   end record;
   --  Stores names of all tested type names, that produce names of generic
   --  test pachages, which should be instantiated
   --  if we have an instantiation of the tested package.

   package Generic_Tests_Storage is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Generic_Tests);
   use Generic_Tests_Storage;

   Gen_Tests_Storage : Generic_Tests_Storage.List;
   --  List of data on all the generic tests created during the processing of
   --  generic tested packages.

   type Generic_Package is record
      Name : String_Access;
      Sloc : String_Access := null;

      Has_Instantiation : Boolean := False;
   end record;

   package Generic_Package_Storage is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Generic_Package);
   use Generic_Package_Storage;

   Gen_Package_Storage : Generic_Package_Storage.List;
   --  Used to detect processed generic packages that do not have
   --  instantiations in the scope of argument sources and, therefore, won't be
   --  included into final harness.

   Last_Context_Name : String_Access;
   --  Suffixless name of the last tree file created

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
   --  for generating the testing unit and suite and generates them if the
   --  source is appropriate (contains one or less tagged type declaration).

   procedure Process_Stubs (List : Asis_Element_List.List);
   --  If ther are any units to stub, closes the context, generates .adt files
   --  for units to stub and passes compilation units to the Stub Generator.

   procedure Gather_Data
     (The_Unit          :     Asis.Compilation_Unit;
      Data              : out Data_Holder;
      Suite_Data_List   : out Suites_Data_Type;
      Apropriate_Source : out Boolean);
   --  Iterates through the given unit and gathers all the data needed for
   --  generation of test package. All the iterations are done here.
   --  Checks if given unit is of the right kind and if it is appropriate.
   --  Marks unappropriate sources in the source table.

   procedure Gather_Substitution_Data
     (Suite_Data_List : in out Suites_Data_Type);

   procedure Gather_Direct_Callees
     (Decl : Asis.Declaration; Set : in out String_Set.Set);

   procedure Source_Clean_Up;
   --  Minimal clean-up needed for one source (deleting .ali & .adt)

   function No_Inheritance_Through_Generics
     (Inheritance_Root_Type : Asis.Element;
      Inheritance_Final_Type : Asis.Element)
      return Boolean;
   --  Checks that all types between the root type and the final descendant
   --  are declared in regular packages.

   function Test_Types_Linked
     (Inheritance_Root_Type : Asis.Element;
      Inheritance_Final_Type : Asis.Element)
      return Boolean;
   --  Checks that there is no fully private types between the root type and
   --  the final descendant, so that corresponding test types are members of
   --  same hierarchy.

   function Is_Declared_In_Regular_Package
     (Elem : Asis.Element)
      return Boolean;
   --  Chechs that all enclosing elements for the given element are regular
   --  package declarations.

   function Is_Callable_Subprogram (Subp : Asis.Element) return Boolean;
   --  Checks that given subprogram is not abstract nor null procedure.

   function Is_Fully_Private
     (Arg : Asis.Declaration) return Boolean;
   --  Detects if Arg and it's incomplete declaration (if present)
   --  are both in private part.

   procedure Generate_Test_Package (Data : Data_Holder);
   --  Generates test package spec and body. Completely regeneratable.

   procedure Generate_Function_Wrapper
     (Current_Subp : Subp_Info; Declaration_Only : Boolean := False);
   --  Print a test-case specific wrapper for tested function.

   procedure Generate_Procedure_Wrapper
     (Current_Subp : Subp_Info; Declaration_Only : Boolean := False);
   --  Print a test-case specific wrapper for tested function.

   procedure Generate_Skeletons (Data : Data_Holder);
   --  Generates skeletons for those routines that do not have tests already.

   procedure Print_Comment_Declaration (Subp : Subp_Info; Span : Natural := 0);
   --  Prints the file containing the tested subprogram as well as the line
   --  coloumn numbers of the tested subprogram declaration.

   procedure Print_Comment_Separate (Subp : Subp_Info; Span : Natural := 0);
   --  Prints commented image of tested subprogram with given span.

   function Corresponding_Generic_Package
     (Package_Instance : Asis.Element) return Asis.Element;
   --  Returns a corresponding generic package declaration for a
   --  formal package.

   procedure Generate_Test_Package_Instantiation (Data : Data_Holder);
   --  Generates an instatiation of the corresponding generic test package

   procedure Generate_Project_File;
   --  Generates a project file that sets the value of Source_Dirs
   --  with the directories whe generated tests are placed and includes
   --  the argument project file.

   function Format_Time (Time : OS_Time) return String;
   --  Returns image of given time in 1901-01-01 00:00:00 format.

   procedure Put_Wrapper_Rename (Span : Natural; Current_Subp : Subp_Info);
   --  Puts subprogram renaming declaration, which renames generated wrapper
   --  into original tested subprogram's name.

   function Sanitize_TC_Name (TC_Name : String) return String;
   --  Processes the name of the test case in such a way that it could be used
   --  as a part of test routine name. the name is trimmed, then all sequences
   --  of whitespace characters are replaced with an underscore, all other
   --  illegal characters are omitted.

   ------------------------
   -- Nesting processing --
   ------------------------

   function Nesting_Common_Prefix
     (Nesting_1, Nesting_2 : String) return String;
   --  Returns the common prefix of two nestings.

   function Nesting_Difference
     (Nesting_1, Nesting_2 : String) return String;
   --  Returns difference in ending of two nestings without the first dot
   --  of the deeper nesting.

   procedure Generate_Nested_Hierarchy (Data : Data_Holder);
   --  Create dummy child packages copying nested packages from tested package.

   -----------------------
   -- Marker Processing --
   -----------------------

   package String_Vectors is new
     Ada.Containers.Indefinite_Vectors (Natural, String);

   type Markered_Data is record
      Commented_Out   : Boolean := False;
      Short_Name_Used : Boolean := False;
      Short_Name      : String_Access := new String'("");
      TR_Text         : String_Vectors.Vector;
      Issue_Warning   : Boolean := False;
   end record;

   type Unique_Hash is record
      Version : String_Access;
      Hash    : String_Access;
      TC_Hash : String_Access;
   end record;

   function "<" (L, R : Unique_Hash) return Boolean;

   package Markered_Data_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Unique_Hash, Markered_Data);
   use Markered_Data_Maps;

   Markered_Data_Map : Markered_Data_Maps.Map;

   procedure Put_Opening_Comment_Section
     (Subp           : Subp_Info;
      Overloading_N  : Natural;
      Commented_Out  : Boolean := False;
      Use_Short_Name : Boolean := True;
      Type_Name      : String  := "");

   procedure Put_Closing_Comment_Section
     (Subp           : Subp_Info;
      Overloading_N  : Natural;
      Commented_Out  : Boolean := False;
      Use_Short_Name : Boolean := True);

   procedure Get_Subprograms_From_Package (File : String);

   procedure Get_Subprogram_From_Separate
     (File : String;
      UH   : Unique_Hash;
      Subp : Subp_Info);

   function Uncomment_Line (S : String) return String;
   --  Removes two dashes and two spaces from the beginning of the line.
   --  Returns argument string if commenting prefix not found.

   function Find_Same_Short_Name
     (MD_Map : Markered_Data_Maps.Map;
      Subp   : Subp_Info) return Markered_Data_Maps.Cursor;
   --  Searches for the test with given short name

   function "<" (L, R : Unique_Hash) return Boolean is
   begin
      if L.Version.all = R.Version.all then
         if L.Hash.all = R.Hash.all then
            return L.TC_Hash.all < R.TC_Hash.all;
         else
            return L.Hash.all < R.Hash.all;
         end if;
      else
         return L.Version.all < R.Version.all;
      end if;
   end "<";

   ---------------------------
   -- Nesting_Common_Prefix --
   ---------------------------

   function Nesting_Common_Prefix
     (Nesting_1, Nesting_2 : String) return String
   is
      L1, L2   : Integer;
      Last_Dot : Integer;
   begin
      L1 := Nesting_1'First;
      L2 := Nesting_2'First;
      loop

         if Nesting_1 (L1) = Nesting_2 (L2) then

            if L1 = Nesting_1'Last then
               return Nesting_1;
            end if;

            if L2 = Nesting_2'Last then
               return Nesting_2;
            end if;

            if Nesting_1 (L1) = '.' then
               Last_Dot := L1;
            end if;

            L1 := L1 + 1;
            L2 := L2 + 1;
         else
            return Nesting_1 (Nesting_1'First .. Last_Dot - 1);
         end if;

      end loop;

   end Nesting_Common_Prefix;

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

   -----------------
   -- Create_Tree --
   -----------------

   procedure Create_Tree (Full_Source_Name : String; Success : out Boolean) is
   begin
      Trace (Me, "Creating tree for " & Full_Source_Name);
      Compile
       (new String'(Full_Source_Name),
        Arg_List.all,
        Success,
        GCC          => ASIS_UL.Common.Gcc_To_Call,
        Display_Call => ASIS_UL.Debug.Debug_Flag_C);
   end Create_Tree;

   --------------------------
   -- Find_Same_Short_Name --
   --------------------------

   function Find_Same_Short_Name
     (MD_Map : Markered_Data_Maps.Map;
      Subp   : Subp_Info) return Markered_Data_Maps.Cursor
   is
      Short_Name : constant String := Subp.Subp_Text_Name.all;
      TC_Hash    : constant String :=
        (if Subp.Has_TC_Info then
            Sanitize_TC_Name (Subp.TC_Info.Name.all)
         else "");
      Cur : Markered_Data_Maps.Cursor := MD_Map.First;
      MD  : Markered_Data;
   begin
      Trace
        (Me,
         "Looking for a compatible dangling test for " & Short_Name);

      loop
         exit when Cur = Markered_Data_Maps.No_Element;

         MD := Markered_Data_Maps.Element (Cur);
         if
           MD.Short_Name_Used
           and then MD.Short_Name.all = Short_Name
         --  It is hard to understand what happens when test case name
         --  is changed, so we do not handle this scenario.
           and then Markered_Data_Maps.Key (Cur).TC_Hash.all = TC_Hash
         then
            exit;
         end if;

         Markered_Data_Maps.Next (Cur);
      end loop;
      return Cur;
   end Find_Same_Short_Name;

   -----------------
   -- Format_Time --
   -----------------

   function Format_Time (Time : OS_Time) return String is

      function Prefix_With_Zero (S : String) return String;

      function Prefix_With_Zero (S : String) return String is
         S_Trimmed : constant String := Trim (S, Both);
      begin
         if S_Trimmed'Length = 1 then
            return "0" & S_Trimmed;
         else
            return S_Trimmed;
         end if;
      end Prefix_With_Zero;
   begin
      return
        Trim (Integer'Image (GM_Year (Time)), Both) & "-" &
      Prefix_With_Zero (Integer'Image (GM_Month (Time))) & "-" &
      Prefix_With_Zero (Integer'Image (GM_Day (Time))) & " " &
      Prefix_With_Zero (Integer'Image (GM_Hour (Time))) & ":" &
      Prefix_With_Zero (Integer'Image (GM_Minute (Time))) & ":" &
      Prefix_With_Zero (Integer'Image (GM_Second (Time)));
   end Format_Time;

   -------------------
   --  Gather_Data  --
   -------------------

   procedure Gather_Data
     (The_Unit          :     Asis.Compilation_Unit;
      Data              : out Data_Holder;
      Suite_Data_List   : out Suites_Data_Type;
      Apropriate_Source : out Boolean)
   is separate;

   ---------------------------
   -- Gather_Direct_Callees --
   ---------------------------

   procedure Gather_Direct_Callees
     (Decl : Asis.Declaration; Set : in out String_Set.Set)
   is
      Control : Traverse_Control := Continue;
      State   : No_State         := Not_Used;

      Origin_Unit : constant Compilation_Unit :=
        Enclosing_Compilation_Unit (Decl);

      procedure Pre_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State);

      procedure Get_Callees is new Traverse_Element
        (Pre_Operation     => Pre_Operation,
         Post_Operation    => No_Op,
         State_Information => No_State);

      procedure Pre_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State)
      is
         pragma Unreferenced (Control, State);
         Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);

         Decl : Asis.Element;
      begin

         case Arg_Kind is

            when A_Procedure_Call_Statement =>
               Decl    := Corresponding_Called_Entity_Unwound (Element);

            when A_Function_Call =>
               Decl := Corresponding_Called_Function_Unwound (Element);

            when others =>
               return;
         end case;

         --  Process simple cases for now. Dispatchings, renamings and parts of
         --  instances are not yet supported.

         if Is_Nil (Decl)
           or else Is_Part_Of_Instance (Decl)
           or else Declaration_Kind (Decl) = Not_A_Declaration
         then
            return;
         end if;

         case Flat_Element_Kind (Decl) is
            when A_Function_Instantiation  |
                 A_Procedure_Instantiation =>
               --  No way to stub a generic
               return;
            when A_Function_Body_Declaration  |
                 A_Procedure_Body_Declaration |
                 A_Procedure_Body_Stub        |
                 A_Function_Body_Stub         =>
               --  No previous spec declaration, which means it is declared
               --  in same body; no need to call a setter.
               return;
            when An_Ordinary_Type_Declaration =>
               --  A function renaming an enumeration type's literal
               return;
            when others =>
               null;
         end case;

         if Is_Part_Of_Implicit (Decl) then
            if
              Flat_Element_Kind (Decl) = A_Function_Declaration and then
              (not Is_Nil (Corresponding_Equality_Operator (Decl)))
            then
               return;
            end if;
            Decl := Corresponding_Declaration (Decl);
         end if;

         case Flat_Element_Kind (Decl) is
            when A_Null_Procedure_Declaration   |
                 A_Formal_Procedure_Declaration |
                 A_Formal_Function_Declaration  =>
               return;
            when others =>
               null;
         end case;

         if
           Is_Equal (Enclosing_Compilation_Unit (Decl), Origin_Unit)
           or else Unit_Kind (Enclosing_Compilation_Unit (Decl)) /= A_Package
         then
            --  Callee is from the same unit spec or even from the body,
            --  it won't be stubbed.
            return;
         end if;

         declare
            Suffix : constant String :=
              "_"
              & Substring_6 (Mangle_Hash_Full (Decl))
              & "_"
              & Substring_6 (GNAT.SHA1.Digest (Get_Nesting (Decl)));
         begin
            Set.Include
              (Get_Nesting (Decl)
               & "."
               & Stub_Data_Unit_Name
               & "."
               & Setter_Prefix
               & To_String_First_Name (Decl)
               & Suffix);
         end;

      end Pre_Operation;
   begin
      Trace
        (Me_Direct_Callees,
         "Gathering direct callees for " & To_String_First_Name (Decl));
      Increase_Indent;
      Set.Clear;

      if Flat_Element_Kind (Decl) = An_Expression_Function_Declaration then
         --  Those do not have an actual bodyso we need to parse their return
         --  statement.
         Get_Callees (Result_Expression (Decl), Control, State);
      else
         if Is_Nil (Corresponding_Body (Decl)) then
            return;
         end if;
         Get_Callees (Corresponding_Body (Decl), Control, State);
      end if;
      Trace
        (Me_Direct_Callees,
         "Direct callees gathered");
      Decrease_Indent;
   end Gather_Direct_Callees;

   ------------------------------
   -- Gather_Substitution_Data --
   ------------------------------

   procedure Gather_Substitution_Data
     (Suite_Data_List : in out Suites_Data_Type)
   is
      TR    : GNATtest.Harness.Generator.Test_Routine_Info;
      TR_W  : Test_Routine_Info_Wrapper;
      LTR   : GNATtest.Harness.Generator.Test_Routine_Info_Enhanced;
      LTR_W : Test_Routine_Info_Enhanced_Wrapper;

      Test_Type_Wrapper : Test_Type_Info_Wrapper;

      Parent_Unit      :  Asis.Compilation_Unit;
      Parent_Unit_File : String_Access;

      Overridden_Subp : Asis.Element;
      Owner_Decl      : Asis.Element;

      Depth : Natural;
   begin
      for
        K in Suite_Data_List.TR_List.First_Index ..
          Suite_Data_List.TR_List.Last_Index
      loop
         TR_W := Suite_Data_List.TR_List.Element (K);
         TR   := TR_W.TR_Info;

         if Is_Overriding_Operation (TR_W.Original_Subp) then

            Overridden_Subp :=
              Corresponding_Overridden_Operation (TR_W.Original_Subp);

            if Is_Part_Of_Inherited (Overridden_Subp) then
               Overridden_Subp :=
                 Corresponding_Declaration (Overridden_Subp);
            end if;

            Parent_Unit := Enclosing_Compilation_Unit (Overridden_Subp);

            Parent_Unit_File := new String'
              (To_String (Text_Name (Parent_Unit)));

            if Is_Dispatching_Operation (Overridden_Subp) then
               --  In some cases it could be not dispatching

               Owner_Decl :=
                 Enclosing_Element (Primitive_Owner (Overridden_Subp));

               if
                 Source_Present (Parent_Unit_File.all)    and then
                 Is_Callable_Subprogram (Overridden_Subp) and then
                 Test_Types_Linked (Owner_Decl, TR_W.Original_Type) and then
                 No_Inheritance_Through_Generics
                   (Owner_Decl, TR_W.Original_Type)
               then
                  LTR.TR_Text_Name := new String'(TR.TR_Text_Name.all);

                  Depth :=
                    GNATtest.Harness.Generator.Inheritance_Depth
                      (TR_W.Original_Type, Owner_Decl);
                  LTR.Inheritance_Depth := Depth;

                  for
                    L in Suite_Data_List.Test_Types.First_Index ..
                      Suite_Data_List.Test_Types.Last_Index
                  loop

                     Test_Type_Wrapper :=
                       Suite_Data_List.Test_Types.Element (L);

                     if
                       Is_Equal
                         (Test_Type_Wrapper.Original_Type, TR_W.Original_Type)
                     then

                        if
                          Depth >
                            Test_Type_Wrapper.TT_Info.Max_Inheritance_Depth
                        then
                           Test_Type_Wrapper.TT_Info.Max_Inheritance_Depth :=
                             Depth;

                           Suite_Data_List.Test_Types.Replace_Element
                             (L, Test_Type_Wrapper);

                           exit;
                        end if;
                     end if;

                  end loop;

                  LTR_W.TR_Info       := LTR;
                  LTR_W.Original_Type := TR_W.Original_Type;
                  LTR_W.Test_Package  := new String'(TR_W.Test_Package.all);

                  --  adding sloc info
                  LTR_W.TR_Info.Tested_Sloc := new String'
                    (Base_Name (Parent_Unit_File.all)
                     & ":"
                     & Trim
                       (Integer'Image (First_Line_Number (Overridden_Subp)),
                        Both)
                     & ":"
                     & Trim
                       (Integer'Image (First_Column_Number (Overridden_Subp)),
                        Both)
                     & ": overridden at "
                     & Base_Name
                       (To_String
                          (Text_Name
                             (Enclosing_Compilation_Unit
                                (TR_W.Original_Type))))
                     & ":"
                     & Trim
                       (Integer'Image (First_Line_Number (TR_W.Original_Subp)),
                        Both)
                     & ":"
                     & Trim
                       (Integer'Image
                          (First_Column_Number (TR_W.Original_Subp)),
                        Both)
                     & ":");

                  Suite_Data_List.LTR_List.Append (LTR_W);

               end if;
            end if;
         end if;
      end loop;
   end Gather_Substitution_Data;

   -------------------------------
   -- Generate_Function_Wrapper --
   -------------------------------

   procedure Generate_Function_Wrapper
     (Current_Subp : Subp_Info; Declaration_Only : Boolean := False)
   is
      Str_Set : String_Set.Set;
      Cur     : String_Set.Cursor;
   begin
      S_Put (0, GT_Marker_Begin);
      New_Line_Count;
      S_Put
        (3,
         "function " &
         Wrapper_Prefix &
         Current_Subp.Subp_Mangle_Name.all);
      declare
         Params : constant
           Asis.Parameter_Specification_List := Parameter_Profile
             (Current_Subp.Subp_Declaration);
         Result : constant Asis.Element :=
           Result_Profile (Current_Subp.Subp_Declaration);

         Result_Image : constant String :=
           Trim (To_String (Element_Image (Result)), Both);
      begin
         for I in Params'Range loop
            if I = Params'First then
               S_Put (0, " (");
            end if;
            S_Put
              (0,
               Trim
                 (To_String (Element_Image (Params (I))),
                  Both));
            if I = Params'Last then
               S_Put
                 (0,
                  ") ");
            else
               S_Put (0, "; ");
            end if;
         end loop;

         S_Put (0, " return " & Result_Image);

         if Declaration_Only then
            return;
         end if;

         New_Line_Count;
         S_Put (3, "is");
         New_Line_Count;

         Str_Set := Current_Subp.TC_Info.Params_To_Temp;
         Cur := Str_Set.First;
         loop
            exit when Cur = String_Set.No_Element;

            S_Put (6, String_Set.Element (Cur));
            New_Line_Count;

            String_Set.Next (Cur);
         end loop;

         S_Put (3, "begin");
         New_Line_Count;

         if Current_Subp.TC_Info.Req_Image.all /= "" then
            S_Put (6, "begin");
            New_Line_Count;
            S_Put (9, "pragma Assert");
            New_Line_Count;
            S_Put
              (11,
               "(" &
               Current_Subp.TC_Info.Req_Image.all &
               ");");
            New_Line_Count;
            S_Put (9, "null;");
            New_Line_Count;
            S_Put (6, "exception");
            New_Line_Count;
            S_Put (12, "when System.Assertions.Assert_Failure =>");
            New_Line_Count;
            S_Put (15, "AUnit.Assertions.Assert");
            New_Line_Count;
            S_Put (17, "(False,");
            New_Line_Count;
            S_Put
              (18,
               """req_sloc("
               & Current_Subp.TC_Info.Req_Line.all
               & "):"
               & Current_Subp.TC_Info.Name.all
               & " test requirement violated"");");
            New_Line_Count;
            S_Put (6, "end;");
            New_Line_Count;
         end if;

         S_Put (6, "declare");
         New_Line_Count;
         S_Put
           (9,
            Current_Subp.Subp_Mangle_Name.all &
            "_Result : constant " &
            Result_Image &
            " := GNATtest_Generated.GNATtest_Standard." &
            Current_Subp.Nesting.all &
            "." &
            Current_Subp.Subp_Name_Image.all);

         if Params'Length = 0 then
            S_Put (0, ";");
         else
            S_Put (1, "(");
            for I in Params'Range loop
               declare
                  Name_List : constant Asis.Element_List := Names (Params (I));
               begin
                  for J in Name_List'Range loop
                     S_Put
                       (0,
                        To_String (Defining_Name_Image (Name_List (J))));
                     if J /= Name_List'Last then
                        S_Put (0, ", ");
                     end if;
                  end loop;
               end;

               if I = Params'Last then
                  S_Put (0, ");");
               else
                  S_Put (0, ", ");
               end if;
            end loop;
         end if;

         New_Line_Count;

         S_Put (6, "begin");
         New_Line_Count;

         if Current_Subp.TC_Info.Ens_Image.all /= "" then
            S_Put (9, "begin");
            New_Line_Count;
            S_Put (12, "pragma Assert");
            New_Line_Count;
            S_Put
              (14,
               "(" &
               Current_Subp.TC_Info.Ens_Image.all &
               ");");
            New_Line_Count;
            S_Put (12, "null;");
            New_Line_Count;
            S_Put (9, "exception");
            New_Line_Count;
            S_Put (12, "when System.Assertions.Assert_Failure =>");
            New_Line_Count;
            S_Put (15, "AUnit.Assertions.Assert");
            New_Line_Count;
            S_Put (17, "(False,");
            New_Line_Count;
            S_Put
              (18,
               """ens_sloc("
               & Current_Subp.TC_Info.Ens_Line.all
               & "):"
               & Current_Subp.TC_Info.Name.all
               & " test commitment violated"");");
            New_Line_Count;
            S_Put (9, "end;");
            New_Line_Count;
         end if;

         S_Put
           (9,
            "return " &
            Current_Subp.Subp_Mangle_Name.all &
            "_Result;");
         New_Line_Count;

         S_Put (6, "end;");
         New_Line_Count;

         S_Put
           (3,
            "end " &
            Wrapper_Prefix &
            Current_Subp.Subp_Mangle_Name.all &
            ";");
         New_Line_Count;
         S_Put (0, GT_Marker_End);
         New_Line_Count;
      end;
   end Generate_Function_Wrapper;

   -------------------------------
   -- Generate_Nested_Hierarchy --
   -------------------------------

   procedure Generate_Nested_Hierarchy (Data : Data_Holder)
   is
      Cur : Package_Info_List.Cursor := Data.Package_Data_List.First;
      Output_Dir  : constant String :=
        Get_Source_Output_Dir (Data.Unit_File_Name.all);
   begin
      loop
         exit when Cur = Package_Info_List.No_Element;

         declare
            S           : constant String :=
              Package_Info_List.Element (Cur).Name.all;
            S_Pack : constant String :=
              Data.Unit_Full_Name.all & "." &
              Test_Data_Unit_Name & "." &
              Test_Unit_Name & "." &
              Nesting_Difference (Data.Unit_Full_Name.all, S);
         begin
            if
              Data.Unit_Full_Name.all /= S
            then
               Create
                 (Output_Dir & Directory_Separator &
                  Unit_To_File_Name (S_Pack) & ".ads");

               S_Put (0, "package " & S_Pack & " is");
               Put_New_Line;
               S_Put (0, "end " & S_Pack & ";");
               Put_New_Line;

               Close_File;
            end if;
         end;

         Package_Info_List.Next (Cur);
      end loop;

      if not Data.Has_Simple_Case then
         Create
           (Output_Dir & Directory_Separator &
            Unit_To_File_Name
              (Data.Unit_Full_Name.all & "." &
               Test_Data_Unit_Name & "." &
               Test_Unit_Name) &
            ".ads");

         S_Put
           (0,
            "package " & Data.Unit_Full_Name.all &
            "." & Test_Data_Unit_Name & "." & Test_Unit_Name & " is");
         Put_New_Line;
         S_Put
           (0,
            "end " & Data.Unit_Full_Name.all &
            "." & Test_Data_Unit_Name & "." & Test_Unit_Name  & ";");
         Put_New_Line;

         Close_File;

         Excluded_Test_Package_Bodies.Include
           (Unit_To_File_Name
              (Data.Unit_Full_Name.all & "."
               & Test_Data_Unit_Name & "."
               & Test_Unit_Name)
            & ".adb");

         Create
           (Output_Dir & Directory_Separator &
            Unit_To_File_Name
              (Data.Unit_Full_Name.all & "." &
               Test_Data_Unit_Name) &
            ".ads");

         S_Put
           (0,
            "package " & Data.Unit_Full_Name.all &
            "." & Test_Data_Unit_Name & " is");
         Put_New_Line;
         S_Put
           (0,
            "end " & Data.Unit_Full_Name.all &
            "." & Test_Data_Unit_Name  & ";");
         Put_New_Line;

         Close_File;

         Excluded_Test_Package_Bodies.Include
           (Unit_To_File_Name
              (Data.Unit_Full_Name.all & "."
               & Test_Data_Unit_Name)
            & ".adb");
      end if;

   end Generate_Nested_Hierarchy;

   --------------------------------
   -- Generate_Procedure_Wrapper --
   --------------------------------

   procedure Generate_Procedure_Wrapper
     (Current_Subp : Subp_Info; Declaration_Only : Boolean := False)
   is
      Str_Set : String_Set.Set;
      Cur     : String_Set.Cursor;
   begin
      S_Put (0, GT_Marker_Begin);
      New_Line_Count;
      S_Put
        (3,
         "procedure " &
         Wrapper_Prefix &
         Current_Subp.Subp_Mangle_Name.all);
      declare
         Params : constant
           Asis.Parameter_Specification_List := Parameter_Profile
             (Current_Subp.Subp_Declaration);
      begin
         for I in Params'Range loop
            if I = Params'First then
               S_Put (0, " (");
            end if;
            S_Put
              (0,
               Trim
                 (To_String (Element_Image (Params (I))),
                  Both));
            if I = Params'Last then
               S_Put
                 (0,
                  ") ");
            else
               S_Put (0, "; ");
            end if;
         end loop;

         if Declaration_Only then
            return;
         end if;

         New_Line_Count;
         S_Put (3, "is");
         New_Line_Count;

         Str_Set := Current_Subp.TC_Info.Params_To_Temp;
         Cur := Str_Set.First;
         loop
            exit when Cur = String_Set.No_Element;

            S_Put (6, String_Set.Element (Cur));
            New_Line_Count;

            String_Set.Next (Cur);
         end loop;

         S_Put (3, "begin");
         New_Line_Count;

         if Current_Subp.TC_Info.Req_Image.all /= "" then
            S_Put (6, "begin");
            New_Line_Count;
            S_Put (9, "pragma Assert");
            New_Line_Count;
            S_Put
              (11,
               "(" &
               Current_Subp.TC_Info.Req_Image.all &
                 ");");
            New_Line_Count;
            S_Put (9, "null;");
            New_Line_Count;
            S_Put (6, "exception");
            New_Line_Count;
            S_Put (9, "when System.Assertions.Assert_Failure =>");
            New_Line_Count;
            S_Put (12, "AUnit.Assertions.Assert");
            New_Line_Count;
            S_Put (14, "(False,");
            New_Line_Count;
            S_Put
              (15,
               """req_sloc("
               & Current_Subp.TC_Info.Req_Line.all
               & "):"
               & Current_Subp.TC_Info.Name.all
               & " test requirement violated"");");
            New_Line_Count;
            S_Put (6, "end;");
            New_Line_Count;
         end if;

         S_Put
           (6,
            "GNATtest_Generated.GNATtest_Standard." &
            Current_Subp.Nesting.all &
            "." &
            Current_Subp.Subp_Text_Name.all);

         if Params'Length = 0 then
            S_Put (0, ";");
         else
            S_Put (1, "(");
            for I in Params'Range loop
               declare
                  Name_List : constant Asis.Element_List := Names (Params (I));
               begin
                  for J in Name_List'Range loop
                     S_Put
                       (0,
                        To_String (Defining_Name_Image (Name_List (J))));
                     if J /= Name_List'Last then
                        S_Put (0, ", ");
                     end if;
                  end loop;
               end;
               if I = Params'Last then
                  S_Put (0, ");");
               else
                  S_Put (0, ", ");
               end if;
            end loop;
         end if;

         New_Line_Count;

         if Current_Subp.TC_Info.Ens_Image.all /= "" then
            S_Put (6, "begin");
            New_Line_Count;
            S_Put (9, "pragma Assert");
            New_Line_Count;
            S_Put
              (11,
               "(" &
               Current_Subp.TC_Info.Ens_Image.all &
               ");");
            New_Line_Count;
            S_Put (9, "null;");
            New_Line_Count;
            S_Put (6, "exception");
            New_Line_Count;
            S_Put (9, "when System.Assertions.Assert_Failure =>");
            New_Line_Count;
            S_Put (12, "AUnit.Assertions.Assert");
            New_Line_Count;
            S_Put (14, "(False,");
            New_Line_Count;
            S_Put
              (15,
               """ens_sloc("
               & Current_Subp.TC_Info.Ens_Line.all
               & "):"
               & Current_Subp.TC_Info.Name.all
               & " test commitment violated"");");
            New_Line_Count;
            S_Put (6, "end;");
            New_Line_Count;
         end if;

         S_Put
           (3,
            "end " &
            Wrapper_Prefix &
            Current_Subp.Subp_Mangle_Name.all &
            ";");
         New_Line_Count;
         S_Put (0, GT_Marker_End);
         New_Line_Count;
      end;
   end Generate_Procedure_Wrapper;

   ---------------------------
   -- Generate_Project_File --
   ---------------------------

   procedure Generate_Project_File is
      Tmp_Str : String_Access;
      package Srcs is new
        Ada.Containers.Indefinite_Ordered_Sets (String);
      use Srcs;

      Out_Dirs     : Srcs.Set;
      Out_Dirs_Cur : Srcs.Cursor;

      Output_Prj : String_Access;

      Source_Prj_Name : String :=
        Base_Name (Source_Prj.all, File_Extension (Source_Prj.all));

   begin
      for I in Source_Prj_Name'Range loop
         if Source_Prj_Name (I) = '-' then
            Source_Prj_Name (I) := '_';
         end if;
      end loop;

      Reset_Source_Iterator;
      loop
         Tmp_Str := new String'(Next_Source_Name);
         exit when Tmp_Str.all = "";

         if Is_Directory (Get_Source_Output_Dir (Tmp_Str.all)) then
            Include (Out_Dirs, Get_Source_Output_Dir (Tmp_Str.all));
         end if;
         Free (Tmp_Str);
      end loop;

      Output_Prj :=
        new String'(Harness_Dir.all
                    & Directory_Separator
                    & Test_Prj_Prefix
                    & Source_Prj_Name
                    & ".gpr");

      Create (Output_Prj.all);

      S_Put (0, "with ""aunit"";");

      Put_New_Line;
      S_Put (0, "with ""gnattest_common.gpr"";");
      Put_New_Line;
      S_Put (0, "with """);
      S_Put
        (0,
         +Relative_Path
           (Create (+Source_Prj.all),
            Create (+Harness_Dir.all)) &
           """;");
      Put_New_Line;
      S_Put
        (0,
         "project "
         & Test_Prj_Prefix
         & Base_Name (Source_Prj_Name)
         & " is");
      Put_New_Line;
      Put_New_Line;

      S_Put (3, "for Source_Dirs use");
      Put_New_Line;

      if Out_Dirs.Is_Empty then
         S_Put (5, "(""common"");");

         Put_New_Line;
         Put_New_Line;
      else
         Out_Dirs_Cur := Out_Dirs.First;
         S_Put (5, "(""");
         S_Put
           (0,
            +Relative_Path
              (Create (+Srcs.Element (Out_Dirs_Cur)),
               Create (+Harness_Dir.all)) &
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
                  Create (+Harness_Dir.all)) &
                 """");

         end loop;
         S_Put (0, ",");
         Put_New_Line;
         S_Put (6, """common"");");

         Put_New_Line;
         Put_New_Line;
      end if;

      S_Put (3, "for Languages use Gnattest_Common'Languages & (""Ada"");");
      Put_New_Line;

      S_Put (3, "package Compiler renames Gnattest_Common.Compiler;");
      Put_New_Line;
      Put_New_Line;

      if IDE_Package_Present then
         S_Put
           (3,
            "package Ide renames " &
            Base_Name (Source_Prj.all, File_Extension (Source_Prj.all)) &
            ".Ide;");
         Put_New_Line;
         Put_New_Line;
      end if;

      if Make_Package_Present then
         S_Put
           (3,
            "package Make renames " &
            Base_Name (Source_Prj.all, File_Extension (Source_Prj.all)) &
            ".Make;");
         Put_New_Line;
         Put_New_Line;
      end if;

      S_Put
        (0,
         "end "
         & Test_Prj_Prefix
         & Base_Name (Source_Prj_Name)
         & ";");
      Put_New_Line;
      Close_File;

      Tmp_Test_Prj := new String'(Normalize_Pathname
                                  (Name => Output_Prj.all,
                                   Case_Sensitive => False));
   end Generate_Project_File;

   -----------------------------
   --  Generate_Test_Package  --
   -----------------------------

   procedure Generate_Test_Package (Data : Data_Holder) is

      Output_Dir             : constant String :=
        Get_Source_Output_Dir (Data.Unit_File_Name.all);

      Tmp_File_Name      : constant String :=
        "gnattest_tmp_test_package";

      Test_File_Name : String_Access;
      Data_Unit_Name : String_Access;
      Unit_Name      : String_Access;
      Unit_Pref      : String_Access;

      Setters_Set : String_Set.Set;
      Set_Cur     : String_Set.Cursor;

      Subp_Cur     : Subp_Data_List.Cursor;
      Pack_Cur     : Package_Info_List.Cursor;

      Current_Type : Base_Type_Info;
      --  The test type for which the primitives are
      --  put togather in the corresponding test package

      Test_Unit_Suffix : String_Access;
      --  Generic or non-generic test package suffix or.

      Actual_Test : Boolean;
      --  Indicates if current test package has at least one non-abstract test
      --  routine. In that case we need to include AUnit.Assertions.

      Gen_Tests : Generic_Tests;
      --  Used to store all test type names in case of generic tested package.
      --  They are to be added at generic test storage.

      Nesting_Add : String_Access;

      UH     : Unique_Hash;
      MD     : Markered_Data;
      MD_Cur : Markered_Data_Maps.Cursor;

      Subp_List : Subp_Data_List.List;
      Current_Subp : Subp_Info;
      Current_Pack : Package_Info;

      TP_Map  : TP_Mapping;
      TP_List : TP_Mapping_List.List;

      Tear_Down_Line_Add : Natural := 0;

      Short_Names_Used : String_Set.Set;

      package Elements_Set is new
        Ada.Containers.Indefinite_Ordered_Sets (Asis.Element, "<", Is_Equal);
      use Elements_Set;

      Shortnamed_Subps : Elements_Set.Set;

      --  overlodaing number counting
      Name_Numbers : Name_Frequency.Map;
      package Elem_Number_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps (Asis.Element, Natural);
      use Elem_Number_Maps;
      Elem_Numbers : Elem_Number_Maps.Map;

      Test_Data_Package_Name : String_Access;

      --  temporary storage for slocs of test routines
      type TR_SLOC_Buffer_Type is record
         TPtarg  : String_Access;
         Test_F  : String_Access;
         Test_T  : String_Access;
         Subp    : Subp_Info;
         TR_Line : Natural := 1;
      end record;

      package TR_SLOC_Buffer_Lists is new
        Ada.Containers.Doubly_Linked_Lists (TR_SLOC_Buffer_Type);
      use TR_SLOC_Buffer_Lists;

      TR_SLOC_Buffer : TR_SLOC_Buffer_Lists.List;

      procedure Add_Buffered_TR_Slocs
        (TP_List     : in out TP_Mapping_List.List;
         Common_Time : String);
      --  Pushes buffered test routine slocs into main mapping container.

      function Is_Unimplemented_Test
        (TR_Text : String_Vectors.Vector) return Boolean;
      --  Searches for specific text pattern which indicates that given test
      --  skeleton was not modified by user after generation.

      procedure Put_Test_Data_Header;

      procedure Put_TP_Header (TD_Package_Name : String);

      procedure Update_Generic_Packages (Instantiation : String);
      procedure Update_Generic_Packages (Gen_Pack      : Generic_Package);

      type Persistent_Section_Type is
        (With_Clauses,      -- /00/
         Body_Declarations, -- /01/
         Body_Statements);  -- /02/

      procedure Put_Persistent_Section (PS_Type : Persistent_Section_Type);
      --  Puts persistent section of given kind surrounded with read-only
      --  markers and corresponding specific Id.

      function Markered_Data_Map_Is_Empty return Boolean;
      --  Check if Markered_Data_Map is empty or the only element present is
      --  actually the Body_Statements persistent block.

      procedure Put_Persistent_Section (PS_Type : Persistent_Section_Type) is
         UH     : Unique_Hash;
         MD     : Markered_Data;
         MD_Cur : Markered_Data_Maps.Cursor;
      begin
         S_Put (0, "--  begin read only");
         New_Line_Count;
         case PS_Type is
            when With_Clauses =>
               S_Put (0, "--  id:" & Hash_Version & "/00/");
            when Body_Declarations =>
               S_Put (0, "--  id:" & Hash_Version & "/01/");
            when Body_Statements =>
               S_Put (0, "--  id:" & Hash_Version & "/02/");
         end case;
         New_Line_Count;
         S_Put (0, "--");
         New_Line_Count;
         case PS_Type is
            when With_Clauses =>
               S_Put (0, "--  This section can be used to add with "
                      & "clauses if necessary.");
            when Body_Declarations =>
               S_Put (0, "--  This section can be used to add global "
                      & "variables and other elements.");
            when Body_Statements =>
               S_Put (0, "--  This section can be used to add "
                      & "elaboration code for the global state.");
         end case;
         New_Line_Count;
         S_Put (0, "--");
         New_Line_Count;
         if PS_Type = Body_Statements then
            S_Put (0, "begin");
            New_Line_Count;
         end if;
         S_Put (0, "--  end read only");
         New_Line_Count;

         UH.Version := new String'(Hash_Version);
         case PS_Type is
            when With_Clauses =>
               UH.Hash := new String'("00");
            when Body_Declarations =>
               UH.Hash := new String'("01");
            when Body_Statements =>
               UH.Hash := new String'("02");
         end case;
         UH.TC_Hash := new String'("");
         MD_Cur := Find (Markered_Data_Map, UH);
         if MD_Cur /= Markered_Data_Maps.No_Element then
            MD := Markered_Data_Maps.Element (MD_Cur);
            for I in MD.TR_Text.First_Index .. MD.TR_Text.Last_Index loop
               S_Put (0, MD.TR_Text.Element (I));
               New_Line_Count;
            end loop;
            Markered_Data_Map.Delete (MD_Cur);
         else
            if PS_Type = Body_Statements then
               S_Put (3, "null;");
            end if;
            New_Line_Count;
         end if;

         S_Put (0, "--  begin read only");
         New_Line_Count;
         S_Put (0, "--  end read only");
         New_Line_Count;
      end Put_Persistent_Section;

      procedure Add_Buffered_TR_Slocs
        (TP_List     : in out TP_Mapping_List.List;
         Common_Time : String)
      is
         Cur : TR_SLOC_Buffer_Lists.Cursor := TR_SLOC_Buffer.First;
      begin
         loop
            exit when Cur = TR_SLOC_Buffer_Lists.No_Element;

            if TR_SLOC_Buffer_Lists.Element (Cur).Test_T /= null then
               Add_TR
                 (TP_List,
                  TR_SLOC_Buffer_Lists.Element (Cur).TPtarg.all,
                  TR_SLOC_Buffer_Lists.Element (Cur).Test_F.all,
                  "modified",
                  TR_SLOC_Buffer_Lists.Element (Cur).Subp,
                  TR_SLOC_Buffer_Lists.Element (Cur).TR_Line);
            else
               Add_TR
                 (TP_List,
                  TR_SLOC_Buffer_Lists.Element (Cur).TPtarg.all,
                  TR_SLOC_Buffer_Lists.Element (Cur).Test_F.all,
                  Common_Time,
                  TR_SLOC_Buffer_Lists.Element (Cur).Subp,
                  TR_SLOC_Buffer_Lists.Element (Cur).TR_Line);
            end if;
            TR_SLOC_Buffer_Lists.Next (Cur);
         end loop;

         TR_SLOC_Buffer.Clear;

      end Add_Buffered_TR_Slocs;

      function Is_Unimplemented_Test
        (TR_Text : String_Vectors.Vector) return Boolean
      is
         Unimplemented_Line : constant String :=
           """Test not implemented.""";
      begin

         if TR_Text.Is_Empty then
            return True;
         end if;

         for I in TR_Text.First_Index .. TR_Text.Last_Index loop
            if Index (TR_Text.Element (I), Unimplemented_Line) /= 0 then
               return True;
            end if;
         end loop;

         return False;

      end Is_Unimplemented_Test;

      function Markered_Data_Map_Is_Empty return Boolean is
         use Ada.Containers;
      begin
         if Markered_Data_Map.Is_Empty then
            return True;
         elsif Markered_Data_Map.Length = 1
           and then Markered_Data_Map.First_Key.Hash.all = "02"
         then
            return True;
         else
            return False;
         end if;
      end Markered_Data_Map_Is_Empty;

      procedure Put_Test_Data_Header is
      begin
         S_Put
           (0,
            "--  This package is intended to set up and tear down "
            & " the test environment.");
         Put_New_Line;
         S_Put
           (0,
            "--  Once created by GNATtest, this package will "
            & "never be overwritten");
         Put_New_Line;
         S_Put
           (0,
            "--  automatically. Contents of this package can be "
            & "modified in any way");
         Put_New_Line;
         S_Put
           (0,
            "--  except for sections surrounded by a 'read only' marker.");
         Put_New_Line;
         Put_New_Line;
      end Put_Test_Data_Header;

      procedure Put_TP_Header (TD_Package_Name : String) is
      begin
         S_Put
           (0,
            "--  This package has been generated automatically by GNATtest.");
         New_Line_Count;
         S_Put
           (0,
            "--  You are allowed to add your code to the bodies "
            & "of test routines.");
         New_Line_Count;
         S_Put
           (0,
            "--  Such changes will be kept during further regeneration "
            & "of this file.");
         New_Line_Count;
         S_Put
           (0,
            "--  All code placed outside of test routine bodies "
            & "will be lost. The");
         New_Line_Count;
         S_Put
           (0,
            "--  code intended to set up and tear down the test "
            & "environment should be");
         New_Line_Count;
         S_Put
           (0,
            "--  placed into "
            & TD_Package_Name & ".");
         New_Line_Count;
         New_Line_Count;
      end Put_TP_Header;

      procedure Update_Generic_Packages (Gen_Pack : Generic_Package) is
         Cur : Generic_Package_Storage.Cursor := Gen_Package_Storage.First;
         GP  : Generic_Package;
      begin
         while Cur /= Generic_Package_Storage.No_Element loop

            GP := Generic_Package_Storage.Element (Cur);

            if GP.Name.all = Gen_Pack.Name.all then
               if GP.Sloc /= null then
                  --  Same package can be added several times.
                  return;
               end if;
               GP.Sloc := Gen_Pack.Sloc;
               Gen_Package_Storage.Replace_Element (Cur, GP);
               return;
            end if;

            Next (Cur);
         end loop;

         Gen_Package_Storage.Append (Gen_Pack);
      end Update_Generic_Packages;

      procedure Update_Generic_Packages (Instantiation : String) is
         Cur : Generic_Package_Storage.Cursor := Gen_Package_Storage.First;
         GP  : Generic_Package;
      begin
         while Cur /= Generic_Package_Storage.No_Element loop

            GP := Generic_Package_Storage.Element (Cur);

            if GP.Name.all = Instantiation then
               if GP.Has_Instantiation then
                  --  Same package can be instantiated multiple times.
                  return;
               end if;
               GP.Has_Instantiation := True;
               Gen_Package_Storage.Replace_Element (Cur, GP);
               return;
            end if;

            Next (Cur);
         end loop;

         --  Instantiation is processed ahead of coresponding generic.
         --  Adding a template for it to later fill in the sloc.
         GP.Name := new String'(Instantiation);
         GP.Sloc := null;
         GP.Has_Instantiation := True;
         Gen_Package_Storage.Append (GP);
      end Update_Generic_Packages;

   begin

      if not Generate_Separates then
         Test_Info.Include (Data.Unit_File_Name.all, 0);
      end if;

      if Data.Is_Generic then
         Test_Unit_Suffix := new String'(Gen_Test_Unit_Name_Suff);
         Gen_Tests.Gen_Unit_Full_Name := new String'(Data.Unit_Full_Name.all);
      else
         Test_Unit_Suffix := new String'(Test_Unit_Name_Suff);
      end if;

      for I in
        Data.Type_Data_List.First_Index .. Data.Type_Data_List.Last_Index
      loop

         Current_Type := Data.Type_Data_List.Element (I);

         --  setting up current package
         Pack_Cur := Data.Package_Data_List.First;
         loop
            exit when Pack_Cur = Package_Info_List.No_Element;

            Current_Pack := Package_Info_List.Element (Pack_Cur);

            if Current_Type.Nesting.all = Current_Pack.Name.all then
               exit;
            end if;

            Pack_Cur := Package_Info_List.Next (Pack_Cur);
         end loop;

         Actual_Test := False;

         if Data.Unit_Full_Name.all = Current_Type.Nesting.all then
            Unit_Pref := new String'(Data.Unit_Full_Name.all);
         else
            Unit_Pref := new String'
              (Data.Unit_Full_Name.all & "." &
               Test_Data_Unit_Name & "." &
               Test_Unit_Name & "." &
               Nesting_Difference
                 (Data.Unit_Full_Name.all,
                  Current_Type.Nesting.all));
         end if;

         Data_Unit_Name := new String'
           (Unit_Pref.all & "."                  &
            Current_Type.Main_Type_Text_Name.all &
            Test_Data_Unit_Name_Suff);

         Test_File_Name := new String'(Unit_To_File_Name (Data_Unit_Name.all));

         --  saving test data package name for further reference
         Test_Data_Package_Name := new String'(Data_Unit_Name.all);

         if not Is_Regular_File
           (Output_Dir & Directory_Separator & Test_File_Name.all & ".ads")
         then

            Create
              (Output_Dir & Directory_Separator & Test_File_Name.all & ".ads");

            Put_Test_Data_Header;

            if not Current_Type.Has_Argument_Father then
               if Current_Pack.Data_Kind = Instantiation then
                  S_Put
                    (0,
                     "with "
                     & Current_Pack.Generic_Containing_Package.all
                     & "."
                     & Current_Type.Main_Type_Text_Name.all
                     & Test_Data_Unit_Name_Suff
                     & ";");
                  Put_New_Line;
                  S_Put
                    (0,
                     "with "
                     & Current_Pack.Generic_Containing_Package.all
                     & "."
                     & Current_Type.Main_Type_Text_Name.all
                     & Test_Data_Unit_Name_Suff
                     & "."
                     & Current_Type.Main_Type_Text_Name.all
                     & Test_Unit_Name_Suff
                     & ";");
               end if;
               Put_New_Line;
               S_Put (0, "with AUnit.Test_Fixtures;");
            else
               if
                 Current_Type.Argument_Father_Unit_Name.all =
                   Current_Type.Argument_Father_Nesting.all
               then
                  S_Put
                    (0,
                     "with "                                    &
                     Current_Type.Argument_Father_Unit_Name.all &
                     "."                                        &
                     Current_Type.Argument_Father_Type_Name.all &
                     Test_Data_Unit_Name_Suff                   &
                     "."                                        &
                     Current_Type.Argument_Father_Type_Name.all &
                     Test_Unit_Suffix.all                       &
                     ";");
               else
                  S_Put
                    (0,
                     "with "                                      &
                     Current_Type.Argument_Father_Unit_Name.all   &
                     "."                                          &
                     Test_Data_Unit_Name                          &
                     "."                                          &
                     Test_Unit_Name                               &
                     "."                                          &
                     Nesting_Difference
                       (Current_Type.Argument_Father_Unit_Name.all,
                        Current_Type.Argument_Father_Nesting.all) &
                     "."                                          &
                     Current_Type.Argument_Father_Type_Name.all   &
                     Test_Data_Unit_Name_Suff                     &
                     "."                                          &
                     Current_Type.Argument_Father_Type_Name.all   &
                     Test_Unit_Suffix.all                         &
                     ";");
               end if;
            end if;
            Put_New_Line;
            Put_New_Line;

            S_Put (0, "with GNATtest_Generated;");
            Put_New_Line;
            Put_New_Line;

            if Current_Pack.Is_Generic then
               S_Put (0, "generic");
               Put_New_Line;
               S_Put
                 (3,
                  "type GNATtest_Test_Type is new "
                  & "AUnit.Test_Fixtures.Test_Fixture");
               Put_New_Line;
               S_Put (5, "with private;");
               Put_New_Line;
            end if;

            S_Put (0, "package " & Data_Unit_Name.all & " is");
            Put_New_Line;
            Put_New_Line;

            if Current_Pack.Data_Kind = Declaration_Data then
               if Current_Type.Has_Argument_Father then
                  --  Declaring test type extension from another test type.
                  S_Put (0, GT_Marker_Begin);
                  Put_New_Line;
                  S_Put
                    (3,
                     "type Test_" &
                       Current_Type.Main_Type_Text_Name.all);
                  if Current_Type.Main_Type_Abstract then
                     S_Put (0, " is abstract new");
                  else
                     S_Put (0, " is new");
                  end if;
                  Put_New_Line;

                  if
                    Current_Type.Argument_Father_Unit_Name.all /=
                      Current_Type.Argument_Father_Nesting.all
                  then
                     Nesting_Add := new String'
                       (Test_Data_Unit_Name & "." &
                          Test_Unit_Name & "." &
                          Nesting_Difference
                          (Current_Type.Argument_Father_Unit_Name.all,
                           Current_Type.Argument_Father_Nesting.all) &
                          ".");
                  else
                     Nesting_Add := new String'("");
                  end if;

                  S_Put
                    (5,
                     "GNATtest_Generated.GNATtest_Standard."    &
                       Current_Type.Argument_Father_Unit_Name.all &
                       "."                                        &
                       Nesting_Add.all                            &
                       Current_Type.Argument_Father_Type_Name.all &
                       Test_Data_Unit_Name_Suff                   &
                       "."                                        &
                       Current_Type.Argument_Father_Type_Name.all &
                       Test_Unit_Suffix.all                       &
                       ".Test_"                                   &
                       Current_Type.Argument_Father_Type_Name.all);
                  Put_New_Line;
                  S_Put (0, GT_Marker_End);
                  Put_New_Line;
                  S_Put (3, "with null record;");

                  Free (Nesting_Add);

               else
                  --  Declaring access type to tested type.
                  S_Put
                    (3,
                     "type "                                 &
                       Current_Type.Main_Type_Text_Name.all    &
                       "_Access is access all "                &
                       "GNATtest_Generated.GNATtest_Standard." &
                       Current_Type.Nesting.all                &
                       "."                                     &
                       Current_Type.Main_Type_Text_Name.all    &
                       "'Class;");
                  Put_New_Line;
                  Put_New_Line;

                  --  Declaring root test type.
                  S_Put (0, GT_Marker_Begin);
                  Put_New_Line;
                  S_Put
                    (3,
                     "type Test_"                         &
                       Current_Type.Main_Type_Text_Name.all &
                       " is");
                  if Current_Type.Main_Type_Abstract then
                     S_Put (0, " abstract");
                  end if;
                  S_Put (0, " new AUnit.Test_Fixtures.Test_Fixture");
                  Put_New_Line;
                  S_Put (0, GT_Marker_End);
                  Put_New_Line;
                  S_Put (3, "with record");
                  Put_New_Line;
                  S_Put
                    (6,
                     "Fixture : "                         &
                       Current_Type.Main_Type_Text_Name.all &
                       "_Access;");
                  Put_New_Line;
                  S_Put (3, "end record;");
               end if;
            else
               S_Put (0, GT_Marker_Begin);
               Put_New_Line;
               S_Put
                 (3,
                  "type Test_"                         &
                    Current_Type.Main_Type_Text_Name.all &
                    " is");
               S_Put (0, " new AUnit.Test_Fixtures.Test_Fixture");
               Put_New_Line;
               S_Put (0, GT_Marker_End);
               Put_New_Line;
               S_Put (3, "with null record;");
            end if;

            Put_New_Line;
            Put_New_Line;

            if not Current_Type.Main_Type_Abstract then
               S_Put
                 (3,
                  "procedure Set_Up (Gnattest_T : in out Test_" &
                  Current_Type.Main_Type_Text_Name.all &
                  ");");
               Put_New_Line;
               S_Put
                 (3,
                  "procedure Tear_Down (Gnattest_T : in out Test_" &
                  Current_Type.Main_Type_Text_Name.all &
                  ");");
               Put_New_Line;
               Put_New_Line;
            end if;

            if Current_Pack.Data_Kind = Instantiation then
               S_Put (0, GT_Marker_Begin);
               Put_New_Line;
               S_Put
                 (3,
                  "package Gnattest_Data_Inst is new "
                  & "GNATtest_Generated.GNATtest_Standard."
                  & Current_Pack.Name.all
                  & "."
                  & Current_Type.Main_Type_Text_Name.all
                  & Test_Data_Unit_Name_Suff
                  & " (Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & ");");
               Put_New_Line;
               S_Put
                 (3,
                  "package Gnattest_Tests_Inst is new Gnattest_Data_Inst."
                  & Current_Type.Main_Type_Text_Name.all
                  & Test_Unit_Name_Suff
                  & ";");
               Put_New_Line;
               Put_New_Line;
               S_Put
                 (3,
                  "type New_Test is new Gnattest_Tests_Inst.Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & " with null record;");
               Put_New_Line;
               S_Put (0, GT_Marker_End);
               Put_New_Line;
               Put_New_Line;
               S_Put
                 (3,
                  "procedure User_Set_Up (Gnattest_T : in out New_Test);");
               Put_New_Line;
               S_Put
                 (3,
                  "procedure User_Tear_Down "
                  & "(Gnattest_T : in out New_Test);");
               Put_New_Line;
               Put_New_Line;
            end if;

            if Current_Pack.Is_Generic then
               S_Put
                 (3,
                  "procedure User_Set_Up (Gnattest_T : in out Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & ");");
               Put_New_Line;
               S_Put
                 (3,
                  "procedure User_Tear_Down (Gnattest_T : in out Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & ");");
               Put_New_Line;
               Put_New_Line;
            end if;

            S_Put (0, "end " & Data_Unit_Name.all & ";");
            Put_New_Line;

            Close_File;

         end if;

         if not Current_Type.Main_Type_Abstract and then
           not Is_Regular_File
           (Output_Dir & Directory_Separator & Test_File_Name.all & ".adb")
         then

            Create
              (Output_Dir & Directory_Separator & Test_File_Name.all & ".adb");

            Put_Test_Data_Header;

            S_Put (0, "package body " & Data_Unit_Name.all & " is");
            Put_New_Line;
            Put_New_Line;

            if Current_Pack.Data_Kind = Declaration_Data then
               if Current_Type.No_Default_Discriminant then
                  S_Put
                    (3,
                     "--  Local_"                            &
                       Current_Type.Main_Type_Text_Name.all    &
                       " : aliased "                           &
                       "GNATtest_Generated.GNATtest_Standard." &
                       Current_Type.Nesting.all                &
                       "."                                     &
                       Current_Type.Main_Type_Text_Name.all &
                       ";");
               else
                  S_Put
                    (3,
                     "Local_"                                &
                       Current_Type.Main_Type_Text_Name.all    &
                       " : aliased "                           &
                       "GNATtest_Generated.GNATtest_Standard." &
                       Current_Type.Nesting.all                &
                       "."                                     &
                       Current_Type.Main_Type_Text_Name.all    &
                       ";");
               end if;
               Put_New_Line;
            end if;

            S_Put
              (3,
               "procedure Set_Up (Gnattest_T : in out Test_" &
               Current_Type.Main_Type_Text_Name.all      &
               ") is");
            Put_New_Line;

            if Current_Pack.Data_Kind = Declaration_Data then
               if Current_Pack.Is_Generic then
                  S_Put
                    (6,
                     "X : Test_"
                     & Current_Type.Main_Type_Text_Name.all
                     & "'Class renames Test_"
                     & Current_Type.Main_Type_Text_Name.all
                     & "'Class (Gnattest_T);");
                  Put_New_Line;
               end if;
            end if;

            S_Put (3, "begin");
            Put_New_Line;

            if Current_Type.Has_Argument_Father then
               if
                 Current_Type.Argument_Father_Unit_Name.all /=
                   Current_Type.Argument_Father_Nesting.all
               then
                  Nesting_Add := new String'
                    (Test_Data_Unit_Name & "." &
                     Test_Unit_Name & "." &
                     Nesting_Difference
                       (Current_Type.Argument_Father_Unit_Name.all,
                        Current_Type.Argument_Father_Nesting.all) &
                     ".");
               else
                  Nesting_Add := new String'("");
               end if;

               S_Put
                 (6,
                  "GNATtest_Generated.GNATtest_Standard."    &
                  Current_Type.Argument_Father_Unit_Name.all &
                  "."                                        &
                  Nesting_Add.all                            &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Data_Unit_Name_Suff                   &
                  "."                                        &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Unit_Suffix.all                       &
                  ".Set_Up");
               Put_New_Line;
               S_Put
                 (8,
                  "(GNATtest_Generated.GNATtest_Standard."    &
                  Current_Type.Argument_Father_Unit_Name.all &
                  "."                                        &
                  Nesting_Add.all                            &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Data_Unit_Name_Suff                   &
                  "."                                        &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Unit_Suffix.all                       &
                  ".Test_"                                   &
                  Current_Type.Argument_Father_Type_Name.all &
                  " (Gnattest_T));");
               Put_New_Line;

               Free (Nesting_Add);
            end if;

            if Current_Pack.Data_Kind = Declaration_Data then
               if Current_Type.No_Default_Discriminant then
                  S_Put
                    (6, "null;");
                  Put_New_Line;
                  S_Put
                    (6, "--  Gnattest_T.Fixture := Local_"         &
                       Current_Type.Main_Type_Text_Name.all &
                       "'Access;");
                  Put_New_Line;
               else
                  S_Put
                    (6, "Gnattest_T.Fixture := Local_"             &
                       Current_Type.Main_Type_Text_Name.all &
                       "'Access;");
                  Put_New_Line;

                  if Current_Pack.Data_Kind = Declaration_Data then
                     if Current_Pack.Is_Generic then
                        S_Put (6, "User_Set_Up (X);");
                        Put_New_Line;
                     end if;
                  end if;
               end if;

            else
               S_Put
                 (6, "null;");
               Put_New_Line;
            end if;
            S_Put (3, "end Set_Up;");
            Put_New_Line;
            Put_New_Line;

            S_Put
              (3,
               "procedure Tear_Down (Gnattest_T : in out Test_" &
               Current_Type.Main_Type_Text_Name.all &
               ") is");
            Put_New_Line;

            if Current_Pack.Data_Kind = Declaration_Data then
               if Current_Pack.Is_Generic then
                  S_Put
                    (6,
                     "X : Test_"
                     & Current_Type.Main_Type_Text_Name.all
                     & "'Class renames Test_"
                     & Current_Type.Main_Type_Text_Name.all
                     & "'Class (Gnattest_T);");
                  Put_New_Line;
               end if;
            end if;

            S_Put (3, "begin");
            Put_New_Line;

            if Current_Type.Has_Argument_Father then
               if
                 Current_Type.Argument_Father_Unit_Name.all /=
                   Current_Type.Argument_Father_Nesting.all
               then
                  Nesting_Add := new String'
                    (Test_Data_Unit_Name & "." &
                     Test_Unit_Name & "." &
                     Nesting_Difference
                       (Current_Type.Argument_Father_Unit_Name.all,
                        Current_Type.Argument_Father_Nesting.all) &
                     ".");
               else
                  Nesting_Add := new String'("");
               end if;

               S_Put
                 (6,
                  "GNATtest_Generated.GNATtest_Standard."    &
                  Current_Type.Argument_Father_Unit_Name.all &
                  "."                                        &
                  Nesting_Add.all                            &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Data_Unit_Name_Suff                   &
                  "."                                        &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Unit_Suffix.all                       &
                  ".Tear_Down");
               Put_New_Line;
               S_Put
                 (8,
                  "(GNATtest_Generated.GNATtest_Standard."    &
                  Current_Type.Argument_Father_Unit_Name.all &
                  "."                                        &
                  Nesting_Add.all                            &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Data_Unit_Name_Suff                   &
                  "."                                        &
                  Current_Type.Argument_Father_Type_Name.all &
                  Test_Unit_Suffix.all                       &
                  ".Test_"                                   &
                  Current_Type.Argument_Father_Type_Name.all &
                  " (Gnattest_T));");

               Free (Nesting_Add);
            else
               if Current_Pack.Data_Kind = Declaration_Data
                 and then Current_Pack.Is_Generic
               then
                     S_Put (6, "User_Tear_Down (X);");
               else
                  S_Put
                    (6, "null;");
               end if;
            end if;

            Put_New_Line;
            S_Put (3, "end Tear_Down;");

            Put_New_Line;
            Put_New_Line;

            if Current_Pack.Data_Kind = Instantiation then
               S_Put
                 (3,
                  "procedure User_Set_Up "
                  & "(Gnattest_T : in out New_Test) is");
               Put_New_Line;
               S_Put (6, "pragma Unreferenced (Gnattest_T);");
               Put_New_Line;
               S_Put (3, "begin");
               Put_New_Line;
               S_Put (6, "null;");
               Put_New_Line;
               S_Put (3, "end User_Set_Up;");
               Put_New_Line;
               Put_New_Line;
               S_Put
                 (3,
                  "procedure User_Tear_Down "
                  & "(Gnattest_T : in out New_Test) is");
               Put_New_Line;
               S_Put (6, "pragma Unreferenced (Gnattest_T);");
               Put_New_Line;
               S_Put (3, "begin");
               Put_New_Line;
               S_Put (6, "null;");
               Put_New_Line;
               S_Put (3, "end User_Tear_Down;");
               Put_New_Line;
               Put_New_Line;
            end if;

            if Current_Pack.Is_Generic then
               S_Put
                 (3,
                  "procedure User_Set_Up (Gnattest_T : in out Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & ") is");
               Put_New_Line;
               S_Put (3, "begin");
               Put_New_Line;
               S_Put (6, "null;");
               Put_New_Line;
               S_Put (3, "end User_Set_Up;");
               Put_New_Line;
               Put_New_Line;
               S_Put
                 (3,
                  "procedure User_Tear_Down (Gnattest_T : in out Test_"
                  & Current_Type.Main_Type_Text_Name.all
                  & ") is");
               Put_New_Line;
               S_Put (3, "begin");
               Put_New_Line;
               S_Put (6, "null;");
               Put_New_Line;
               S_Put (3, "end User_Tear_Down;");
               Put_New_Line;
               Put_New_Line;
            end if;

            S_Put (0, "end " & Data_Unit_Name.all & ";");
            Put_New_Line;
            Close_File;

         end if;

         TP_Map.SetUp_Name    := new String'(Test_File_Name.all & ".adb");
         TP_Map.TearDown_Name := new String'(Test_File_Name.all & ".adb");
         TP_Map.SetUp_Line    := 9;
         TP_Map.SetUp_Column  := 4;

         Tear_Down_Line_Add := 0;
         if Current_Type.No_Default_Discriminant then
            Tear_Down_Line_Add := Tear_Down_Line_Add + 1;
         end if;
         if Current_Type.Has_Argument_Father then
            Tear_Down_Line_Add := Tear_Down_Line_Add + 1;
         end if;
         TP_Map.TearDown_Line := 14 + Tear_Down_Line_Add;
         TP_Map.TearDown_Column := 4;

         Free (Test_File_Name);

         Unit_Name := new
           String'(Unit_Pref.all                        &
                     "."                                  &
                     Current_Type.Main_Type_Text_Name.all &
                     Test_Data_Unit_Name_Suff             &
                     "."                                  &
                     Current_Type.Main_Type_Text_Name.all &
                     Test_Unit_Name_Suff);

         Free (Unit_Pref);

         Test_File_Name := new String'(Unit_To_File_Name (Unit_Name.all));

         ----------------------------------
         --  Creating test package spec  --
         ----------------------------------

         Create
           (Output_Dir & Directory_Separator & Test_File_Name.all & ".ads");

         Put_Harness_Header;
         S_Put (0, GT_Marker_Begin);
         Put_New_Line;

         S_Put (0, "with GNATtest_Generated;");
         Put_New_Line;
         if Stub_Mode_ON then
            S_Put (0, "with AUnit.Test_Caller;");
            Put_New_Line;
         end if;
         Put_New_Line;

         if Current_Pack.Is_Generic then
            S_Put (0, "generic");
            Put_New_Line;

            declare
               GP : Generic_Package;
            begin
               GP.Name := new String'(Current_Pack.Name.all);  --  ???
               GP.Sloc := new String'
                 (Base_Name (Data.Unit_File_Name.all)
                  & ":"
                  & Trim
                    (Integer'Image
                         (First_Line_Number (Current_Pack.Element)),
                     Both)
                  & ":"
                  & Trim
                    (Integer'Image
                         (First_Column_Number (Current_Pack.Element)),
                     Both));
               Update_Generic_Packages (GP);
            end;
         end if;

         S_Put (0, "package " & Unit_Name.all & " is");
         Put_New_Line;
         Put_New_Line;

         if Current_Pack.Data_Kind = Declaration_Data then
            S_Put
              (3,
               "type Test_" &
                 Current_Type.Main_Type_Text_Name.all);
            if Current_Type.Main_Type_Abstract then
               S_Put (0, " is abstract new");
            else
               S_Put (0, " is new");
            end if;
            Put_New_Line;

            if Data.Unit_Full_Name.all = Current_Type.Nesting.all then
               S_Put
                 (5,
                  "GNATtest_Generated.GNATtest_Standard."    &
                    Data.Unit_Full_Name.all                    &
                    "."                                        &
                    Current_Type.Main_Type_Text_Name.all &
                    Test_Data_Unit_Name_Suff                   &
                    ".Test_"                                   &
                    Current_Type.Main_Type_Text_Name.all &
                    " with null record;");
            else
               S_Put
                 (5,
                  "GNATtest_Generated.GNATtest_Standard."    &
                    Data.Unit_Full_Name.all                    &
                    "."                                        &
                    Test_Data_Unit_Name                        &
                    "."                                        &
                    Test_Unit_Name                             &
                    "."                                        &
                    Nesting_Difference
                    (Data.Unit_Full_Name.all,
                     Current_Type.Nesting.all)               &
                    "."                                        &
                    Current_Type.Main_Type_Text_Name.all &
                    Test_Data_Unit_Name_Suff                   &
                    ".Test_"                                   &
                    Current_Type.Main_Type_Text_Name.all &
                    " with null record;");
            end if;

         else
            S_Put
              (3,
               "type Test_"
               & Current_Type.Main_Type_Text_Name.all
               & " is new GNATtest_Generated.GNATtest_Standard."
               & Data_Unit_Name.all & ".New_Test with null record;");

            Update_Generic_Packages
              (Current_Pack.Generic_Containing_Package.all);
         end if;

         Put_New_Line;
         Put_New_Line;

         --  Adding test routine declarations.
         if Current_Pack.Data_Kind = Declaration_Data then
            Subp_Cur := Data.Subp_List.First;
            loop
               exit when Subp_Cur = Subp_Data_List.No_Element;

               if
                 Subp_Data_List.Element (Subp_Cur).Corresp_Type =
                 Current_Type.Type_Number
               then

                  if not Subp_Data_List.Element (Subp_Cur).Is_Abstract then
                     S_Put
                       (3,
                        "procedure "
                        & Subp_Data_List.Element
                          (Subp_Cur).Subp_Mangle_Name.all
                        & " (Gnattest_T : in out Test_"
                        & Current_Type.Main_Type_Text_Name.all
                        & ");");
                     Actual_Test := True;
                  end if;

                  Put_New_Line;
                  Print_Comment_Declaration
                    (Subp_Data_List.Element (Subp_Cur), 3);
                  Put_New_Line;
               end if;

               Subp_Data_List.Next (Subp_Cur);
            end loop;
         end if;

         if Stub_Mode_ON then
            S_Put
              (3,
               "package Caller is new AUnit.Test_Caller (Test_"
               & Current_Type.Main_Type_Text_Name.all
               & ");");
            Put_New_Line;
            Put_New_Line;
         end if;

         S_Put (0, "end " & Unit_Name.all & ";");
         Put_New_Line;
         S_Put (0, GT_Marker_End);
         Put_New_Line;
         Close_File;

         if not Current_Type.Main_Type_Abstract then
            TP_Map.TP_Name := new String'(Test_File_Name.all & ".ads");
            TP_List.Append (TP_Map);
         end if;

         ----------------------------------
         --  Creating test package body  --
         ----------------------------------

         if Actual_Test then

            Reset_Line_Counter;

            if Generate_Separates then
               Create
                 (Output_Dir
                  & Directory_Separator
                  & Test_File_Name.all
                  & ".adb");
               Put_Harness_Header;
            else
               Get_Subprograms_From_Package
                 (Output_Dir
                  & Directory_Separator
                  & Test_File_Name.all
                  & ".adb");
               Create (Tmp_File_Name);
               Put_TP_Header (Test_Data_Package_Name.all);

               --  gathering transition data
               if Transition then
                  Subp_Cur := Data.Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if
                       Current_Subp.Corresp_Type = Current_Type.Type_Number
                       and then not Current_Subp.Is_Abstract
                     then
                        UH.Version := new String'("1");
                        UH.Hash := new String'
                          (Subp_Data_List.Element
                             (Subp_Cur).Subp_Hash_V1.all);
                        if
                          Subp_Data_List.Element (Subp_Cur).Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Subp_Data_List.Element
                                (Subp_Cur).TC_Info.TC_Hash.all);
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        Current_Subp := Subp_Data_List.Element (Subp_Cur);

                        Get_Subprogram_From_Separate
                          (Output_Dir
                           & Directory_Separator
                           & Unit_To_File_Name
                             (Unit_Name.all
                              & "."
                              & Test_Routine_Prefix
                              & Current_Subp.Subp_Text_Name.all
                              & "_"
                              & Current_Subp.Subp_Hash_V1
                                (Current_Subp.Subp_Hash_V1'First ..
                                   Current_Subp.Subp_Hash_V1'First + 5)
                              & (if Current_Subp.Has_TC_Info
                                then "_" & Current_Subp.TC_Info.TC_Hash
                                  (Current_Subp.TC_Info.TC_Hash'First ..
                                     Current_Subp.TC_Info.TC_Hash'First + 5)
                                else ""))
                           & ".adb",
                           UH,
                           Current_Subp);
                     end if;
                     Subp_Data_List.Next (Subp_Cur);
                  end loop;
               end if;

               --  gathering used short names
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                    and then not Current_Subp.Is_Abstract
                  then
                     UH.Version := new String'(Hash_Version);
                     UH.Hash := new String'
                       (Current_Subp.Subp_Full_Hash.all);
                     if
                       Current_Subp.Has_TC_Info
                     then
                        UH.TC_Hash := new String'
                          (Sanitize_TC_Name (Current_Subp.TC_Info.Name.all));
                     else
                        UH.TC_Hash := new String'("");
                     end if;

                     MD_Cur := Find (Markered_Data_Map, UH);

                     if MD_Cur /= Markered_Data_Maps.No_Element then
                        MD := Markered_Data_Maps.Element (MD_Cur);
                        if MD.Short_Name_Used then
                           Short_Names_Used.Include
                             (To_Lower (MD.Short_Name.all));
                           Shortnamed_Subps.Include
                             (Current_Subp.Subp_Declaration);

                           Name_Numbers.Include
                             (To_Lower (Current_Subp.Subp_Text_Name.all), 1);
                           Elem_Numbers.Include
                             (Current_Subp.Subp_Declaration, 1);
                        end if;
                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  updating hash v.1 to hash v.2 where possible
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                    and then not Current_Subp.Is_Abstract
                  then
                     UH.Version := new String'("1");
                     UH.Hash := new String'
                       (Current_Subp.Subp_Hash_V1.all);
                     if
                       Current_Subp.Has_TC_Info
                     then
                        UH.TC_Hash := new String'
                          (Current_Subp.TC_Info.TC_Hash.all);
                     else
                        UH.TC_Hash := new String'("");
                     end if;

                     MD_Cur := Find (Markered_Data_Map, UH);

                     if MD_Cur /= Markered_Data_Maps.No_Element then
                        MD := Markered_Data_Maps.Element (MD_Cur);

                        Markered_Data_Map.Delete (MD_Cur);
                        Free (UH.Hash);
                        UH.Hash := new String'
                          (Current_Subp.Subp_Hash_V2_1.all);
                        Free (UH.Version);
                        UH.Version := new String'("2");

                        Markered_Data_Map.Include (UH, MD);
                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  updating hash v.2 to hash v.2.1 where possible
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                    and then not Current_Subp.Is_Abstract
                  then
                     UH.Version := new String'("2");
                     UH.Hash := new String'
                       (Current_Subp.Subp_Hash_V2_1 .all);

                     if Current_Subp.Has_TC_Info then
                        UH.TC_Hash := new String'
                          (Current_Subp.TC_Info.TC_Hash.all);
                     else
                        UH.TC_Hash := new String'("");
                     end if;

                     MD_Cur := Find (Markered_Data_Map, UH);

                     if MD_Cur /= Markered_Data_Maps.No_Element then
                        MD := Markered_Data_Maps.Element (MD_Cur);

                        Markered_Data_Map.Delete (MD_Cur);
                        Free (UH.Version);
                        UH.Version := new String'("2.1");
                        if UH.TC_Hash.all /= "" then
                           Free (UH.TC_Hash);
                           UH.TC_Hash := new String'
                             (Sanitize_TC_Name
                                (Current_Subp.TC_Info.Name.all));
                        end if;

                        Markered_Data_Map.Include (UH, MD);
                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  updating hash v.2.1 to hash v.2.2
               --  and looking for new short names
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                    and then not Current_Subp.Is_Abstract
                  then
                     UH.Version := new String'("2.1");
                     UH.Hash := new String'
                       (Current_Subp.Subp_Hash_V2_1 .all);

                     if Current_Subp.Has_TC_Info then
                        UH.TC_Hash := new String'
                          (Sanitize_TC_Name
                             (Current_Subp.TC_Info.Name.all));
                     else
                        UH.TC_Hash := new String'("");
                     end if;

                     MD_Cur := Find (Markered_Data_Map, UH);

                     if MD_Cur /= Markered_Data_Maps.No_Element then
                        MD := Markered_Data_Maps.Element (MD_Cur);

                        if not
                          Short_Names_Used.Contains (MD.Short_Name.all)
                          or else Shortnamed_Subps.Contains
                            (Current_Subp.Subp_Declaration)
                        then
                           Short_Names_Used.Include (MD.Short_Name.all);
                           Shortnamed_Subps.Include
                             (Current_Subp.Subp_Declaration);

                           Name_Numbers.Include
                             (To_Lower (Current_Subp.Subp_Text_Name.all), 1);
                           Elem_Numbers.Include
                             (Current_Subp.Subp_Declaration, 1);

                           MD.Short_Name_Used := True;
                        end if;

                        Markered_Data_Map.Delete (MD_Cur);
                        Free (UH.Hash);
                        UH.Hash := new String'
                          (Current_Subp.Subp_Full_Hash.all);
                        Free (UH.Version);
                        UH.Version := new String'(Hash_Version);
                        Markered_Data_Map.Include (UH, MD);
                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  creating markered_data and deciding on new short names
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                    and then not Current_Subp.Is_Abstract
                  then
                     UH.Version := new String'(Hash_Version);
                     UH.Hash := new String'
                       (Current_Subp.Subp_Full_Hash.all);
                     if Current_Subp.Has_TC_Info then
                        UH.TC_Hash := new String'
                          (Sanitize_TC_Name (Current_Subp.TC_Info.Name.all));
                     else
                        UH.TC_Hash := new String'("");
                     end if;

                     MD_Cur := Find (Markered_Data_Map, UH);

                     if MD_Cur = Markered_Data_Maps.No_Element then

                        MD.Commented_Out := False;
                        MD.Short_Name_Used := False;
                        MD.Short_Name := new String'
                          (To_Lower (Current_Subp.Subp_Text_Name.all));
                        MD.TR_Text.Clear;

                        if
                          not Short_Names_Used.Contains
                          (To_Lower (Current_Subp.Subp_Text_Name.all))
                          or else Shortnamed_Subps.Contains
                            (Current_Subp.Subp_Declaration)
                        then
                           --  Short name is free, we can use it
                           MD.Short_Name_Used := True;
                           Short_Names_Used.Include
                             (To_Lower (Current_Subp.Subp_Text_Name.all));
                           Shortnamed_Subps.Include
                             (Current_Subp.Subp_Declaration);

                           Name_Numbers.Include
                             (To_Lower (Current_Subp.Subp_Text_Name.all), 1);
                           Elem_Numbers.Include
                             (Current_Subp.Subp_Declaration, 1);

                           --  Looking for a dangling test with same short
                           --  name but different hash.
                           MD_Cur := Find_Same_Short_Name
                             (Markered_Data_Map,
                              Current_Subp);

                           if MD_Cur /= Markered_Data_Maps.No_Element then
                              --  Using corresponding dangling test

                              MD.TR_Text.Clear;
                              MD.TR_Text :=
                                Markered_Data_Maps.Element (MD_Cur).TR_Text;

                              --  also need to copy Commented_Out since
                              --  the test can be dangling for a long time
                              --  or just become dangling
                              MD.Commented_Out :=
                                Markered_Data_Maps.Element
                                  (MD_Cur).Commented_Out;

                              Markered_Data_Map.Delete (MD_Cur);
                              MD.Issue_Warning := True;
                           end if;

                        end if;

                        Markered_Data_Map.Insert (UH, MD);

                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  setting overloading numbers;
               Subp_Cur := Data.Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  Current_Subp := Subp_Data_List.Element (Subp_Cur);

                  if
                    Current_Subp.Corresp_Type = Current_Type.Type_Number
                    and then not Current_Subp.Is_Abstract
                  then

                     if
                       Name_Numbers.Find
                         (To_Lower (Current_Subp.Subp_Text_Name.all)) =
                       Name_Frequency.No_Element
                     then

                        Name_Numbers.Include
                          (To_Lower (Current_Subp.Subp_Text_Name.all), 1);
                        Elem_Numbers.Include
                          (Current_Subp.Subp_Declaration, 1);

                     else
                        if
                          Elem_Numbers.Find
                            (Current_Subp.Subp_Declaration) =
                            Elem_Number_Maps.No_Element
                        then

                           declare
                              X : constant Natural :=
                                Name_Numbers.Element
                                  (To_Lower
                                       (Current_Subp.Subp_Text_Name.all));
                           begin
                              Name_Numbers.Replace
                                (To_Lower (Current_Subp.Subp_Text_Name.all),
                                 X + 1);
                              Elem_Numbers.Include
                                (Current_Subp.Subp_Declaration, X + 1);
                           end;

                        end if;
                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;
               Name_Numbers.Clear;

            end if;

            S_Put (0, "with AUnit.Assertions; use AUnit.Assertions;");
            New_Line_Count;
            S_Put (0, "with System.Assertions;");
            New_Line_Count;
            if Stub_Mode_ON then
               declare
                  S_Cur : Asis_Element_List.Cursor := Data.Units_To_Stub.First;
                  Tmp : String_Access;
               begin
                  while S_Cur /= Asis_Element_List.No_Element loop
                     Tmp := new String'
                       (To_String
                          (Text_Name
                               (Enclosing_Compilation_Unit
                                    (Asis_Element_List.Element (S_Cur)))));

                     if
                       Source_Stubbed (Tmp.all) and then
                       not Excluded_Test_Data_Files.Contains
                         (Base_Name (Get_Source_Stub_Data_Spec (Tmp.all)))
                     then
                        S_Put
                          (0,
                           "with "
                           & To_String
                             (Defining_Name_Image
                                  (First_Name
                                       (Asis_Element_List.Element (S_Cur))))
                           & "."
                           & Stub_Data_Unit_Name
                           & "; use "
                           & To_String
                             (Defining_Name_Image
                                  (First_Name
                                       (Asis_Element_List.Element (S_Cur))))
                           & "."
                           & Stub_Data_Unit_Name
                           & ";");
                        New_Line_Count;
                     end if;

                     Free (Tmp);

                     Next (S_Cur);
                  end loop;
               end;
            end if;
            New_Line_Count;

            Put_Persistent_Section (With_Clauses);

            S_Put (0, "package body " & Unit_Name.all & " is");
            New_Line_Count;
            New_Line_Count;

            Put_Persistent_Section (Body_Declarations);

            --  Adding test routine body stubs.
            Subp_Cur := Data.Subp_List.First;
            loop
               exit when Subp_Cur = Subp_Data_List.No_Element;

               if
                 Subp_Data_List.Element (Subp_Cur).Corresp_Type =
                 Current_Type.Type_Number
               then
                  if not Subp_Data_List.Element (Subp_Cur).Is_Abstract then

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Has_TC_Info then

                        case
                          Declaration_Kind
                            (Subp_Data_List.Element
                                 (Subp_Cur).Subp_Declaration)
                        is

                           when A_Function_Declaration             |
                                An_Expression_Function_Declaration =>
                              Generate_Function_Wrapper
                                (Subp_Data_List.Element (Subp_Cur));

                           when A_Procedure_Declaration =>
                              Generate_Procedure_Wrapper
                                (Subp_Data_List.Element (Subp_Cur));

                           when others =>
                              null;

                        end case;

                     end if;

                     if Generate_Separates then
                        S_Put
                          (3,
                           "procedure "                         &
                             Subp_Data_List.Element
                             (Subp_Cur).Subp_Mangle_Name.all    &
                             " (Gnattest_T : in out Test_"        &
                             Current_Type.Main_Type_Text_Name.all &
                             ") is separate;");

                        New_Line_Count;
                        Print_Comment_Declaration
                          (Subp_Data_List.Element (Subp_Cur), 3);
                        New_Line_Count;

                     else

                        Test_Info.Replace
                          (Data.Unit_File_Name.all,
                           Test_Info.Element (Data.Unit_File_Name.all) + 1);

                        All_Tests_Counter := All_Tests_Counter + 1;

                        UH.Version := new String'(Hash_Version);
                        UH.Hash := new String'
                          (Subp_Data_List.Element
                             (Subp_Cur).Subp_Full_Hash.all);
                        if Subp_Data_List.Element (Subp_Cur).Has_TC_Info then
                           UH.TC_Hash := new String'
                             (Sanitize_TC_Name
                                (Subp_Data_List.Element
                                   (Subp_Cur).TC_Info.Name.all));
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);
                        MD := Markered_Data_Maps.Element (MD_Cur);

                        Put_Opening_Comment_Section
                          (Subp_Data_List.Element (Subp_Cur),
                           Elem_Numbers.Element
                             (Current_Subp.Subp_Declaration),
                           Use_Short_Name => MD.Short_Name_Used,
                           Type_Name => Current_Type.Main_Type_Text_Name.all);

                        if Is_Unimplemented_Test (MD.TR_Text) then
                           TR_SLOC_Buffer.Append
                             ((new String'(Test_File_Name.all & ".ads"),
                              new String'(Test_File_Name.all & ".adb"),
                              null,
                              Subp_Data_List.Element (Subp_Cur),
                              New_Line_Counter));
                        else
                           TR_SLOC_Buffer.Append
                             ((new String'(Test_File_Name.all & ".ads"),
                              new String'(Test_File_Name.all & ".adb"),
                              new String'("modified"),
                              Subp_Data_List.Element (Subp_Cur),
                              New_Line_Counter));
                        end if;

                        if MD.TR_Text.Is_Empty then

                           if Stub_Mode_ON then
                              Gather_Direct_Callees
                                (Current_Subp.Subp_Declaration,
                                 Setters_Set);
                           end if;

                           New_Tests_Counter := New_Tests_Counter + 1;
                           New_Line_Count;
                           S_Put (6, "pragma Unreferenced (Gnattest_T);");
                           New_Line_Count;
                           New_Line_Count;
                           S_Put (3, "begin");
                           New_Line_Count;
                           New_Line_Count;
                           if not Setters_Set.Is_Empty then
                              Set_Cur := Setters_Set.First;
                              while Set_Cur /= String_Set.No_Element loop
                                 S_Put
                                   (3,
                                    "--  "
                                    & String_Set.Element (Set_Cur)
                                    & "( );");
                                 New_Line_Count;
                                 Next (Set_Cur);
                              end loop;
                              New_Line_Count;
                              Setters_Set.Clear;
                           end if;
                           S_Put (6, "AUnit.Assertions.Assert");
                           New_Line_Count;
                           S_Put
                             (8, "(Gnattest_Generated.Default_Assert_Value,");
                           New_Line_Count;
                           S_Put (9,  """Test not implemented."");");
                           New_Line_Count;
                           New_Line_Count;
                        else

                           if MD.Issue_Warning then
                              Report_Std
                                (Base_Name (Data.Unit_File_Name.all)
                                 & ":"
                                 & Trim
                                   (Integer'Image (First_Line_Number
                                    (Current_Subp.Subp_Declaration)),
                                    Both)
                                 & ":"
                                 & Trim
                                   (Integer'Image (First_Column_Number
                                    (Current_Subp.Subp_Declaration)),
                                    Both)
                                 & ": warning: test for "
                                 & MD.Short_Name.all
                                 & " at "
                                 & Unit_Name.all
                                 & ":"
                                 & Trim
                                   (Integer'Image (New_Line_Counter),
                                    Both)
                                 & " might be out of date ("
                                 & MD.Short_Name.all
                                 & " has been changed)");
                           end if;

                           for I in
                             MD.TR_Text.First_Index .. MD.TR_Text.Last_Index
                           loop
                              if MD.Commented_Out then
                                 S_Put
                                   (0,
                                    Uncomment_Line (MD.TR_Text.Element (I)));
                              else
                                 S_Put (0, MD.TR_Text.Element (I));
                              end if;
                              New_Line_Count;
                           end loop;
                        end if;

                        Markered_Data_Map.Delete (MD_Cur);

                        Put_Closing_Comment_Section
                          (Subp_Data_List.Element (Subp_Cur),
                           Elem_Numbers.Element
                             (Current_Subp.Subp_Declaration),
                           Use_Short_Name => MD.Short_Name_Used);
                        New_Line_Count;

                     end if;

                  end if;
               end if;

               Subp_Data_List.Next (Subp_Cur);
            end loop;

            --  printing dangling tests

            if not Markered_Data_Map_Is_Empty then
               Report_Std
                 (" warning: "
                  & Unit_Name.all
                  & " has dangling test(s)");
            end if;

            MD_Cur := Markered_Data_Map.First;
            loop
               exit when MD_Cur = Markered_Data_Maps.No_Element;

               MD := Markered_Data_Maps.Element (MD_Cur);

               if Markered_Data_Maps.Key (MD_Cur).Hash.all /= "02" then
                  declare
                     Stub : Subp_Info;
                  begin

                     Stub.Subp_Full_Hash := new String'
                       (Markered_Data_Maps.Key (MD_Cur).Hash.all);

                     Stub.Subp_Text_Name := new String'
                       (Markered_Data_Maps.Element (MD_Cur).Short_Name.all);

                     Stub.Subp_Mangle_Name := new String'
                       (Test_Routine_Prefix
                        & Stub.Subp_Text_Name.all
                        & "_"
                        & Stub.Subp_Full_Hash
                          (Stub.Subp_Full_Hash'First ..
                               Stub.Subp_Full_Hash'First + 5));

                     if Markered_Data_Maps.Key (MD_Cur).TC_Hash.all = "" then
                        Stub.Has_TC_Info := False;
                     else
                        Stub.Has_TC_Info := True;
                        Stub.TC_Info.TC_Hash := new String'
                          (Markered_Data_Maps.Key (MD_Cur).TC_Hash.all);
                        Stub.TC_Info.Name := Stub.TC_Info.TC_Hash;
                     end if;

                     Put_Opening_Comment_Section
                       (Stub, 0, True, False,
                        Current_Type.Main_Type_Text_Name.all);

                     Add_DT
                       (TP_List,
                        Test_File_Name.all & ".ads",
                        Test_File_Name.all & ".adb",
                        New_Line_Counter,
                        1);

                     for I in
                       MD.TR_Text.First_Index .. MD.TR_Text.Last_Index
                     loop
                        if MD.Commented_Out then
                           S_Put (0, MD.TR_Text.Element (I));
                        else
                           S_Put (0, "--  " & MD.TR_Text.Element (I));
                        end if;
                        New_Line_Count;
                     end loop;

                     Put_Closing_Comment_Section
                       (Stub,
                        Elem_Numbers.Element
                          (Current_Subp.Subp_Declaration),
                        True,
                        False);
                     New_Line_Count;
                  end;
               end if;

               Markered_Data_Maps.Next (MD_Cur);
            end loop;

            Put_Persistent_Section (Body_Statements);

            S_Put (0, "end " & Unit_Name.all & ";");
            New_Line_Count;

            Close_File;

            Add_Buffered_TR_Slocs
              (TP_List,
               Format_Time
                 (File_Time_Stamp
                    (Tmp_File_Name)));

            if not Generate_Separates then
               declare
                  Old_Package : constant String :=
                    Output_Dir & Directory_Separator
                    & Test_File_Name.all & ".adb";
                  Success : Boolean;
               begin
                  if Is_Regular_File (Old_Package) then
                     Delete_File (Old_Package, Success);
                     if not Success then
                        Report_Err ("cannot delete " & Old_Package);
                        raise Fatal_Error;
                     end if;
                  end if;
                  Copy_File (Tmp_File_Name, Old_Package, Success);
                  if not Success then
                     Report_Err ("cannot copy tmp test package to "
                                 & Old_Package);
                     raise Fatal_Error;
                  end if;
                  Delete_File (Tmp_File_Name, Success);
                  if not Success then
                     Report_Err ("cannot delete tmp test package");
                     raise Fatal_Error;
                  end if;
               end;
            end if;

            Markered_Data_Map.Clear;
         end if;

         Short_Names_Used.Clear;
         Shortnamed_Subps.Clear;
         Elem_Numbers.Clear;

      end loop;

      --  Simple case

      if Data.Has_Simple_Case then

         Pack_Cur := Data.Package_Data_List.First;
         loop
            exit when Pack_Cur = Package_Info_List.No_Element;

            Current_Pack := Package_Info_List.Element (Pack_Cur);

            Subp_Cur := Data.Subp_List.First;
            loop
               exit when Subp_Cur = Subp_Data_List.No_Element;

               Current_Subp := Subp_Data_List.Element (Subp_Cur);
               if Current_Subp.Nesting.all = Current_Pack.Name.all then
                  Subp_List.Append (Current_Subp);
               end if;

               Subp_Data_List.Next (Subp_Cur);
            end loop;

            if Current_Pack.Name.all = Data.Unit_Full_Name.all then
               Data_Unit_Name := new String'
                 (Current_Pack.Name.all & "." &  Test_Data_Unit_Name);
            else
               Data_Unit_Name := new String'
                 (Data.Unit_Full_Name.all & "." &
                  Test_Data_Unit_Name & "." &
                  Test_Unit_Name & "." &
                  Nesting_Difference
                    (Current_Pack.Name.all,
                     Data.Unit_Full_Name.all) &
                  "." &  Test_Data_Unit_Name);
            end if;

            Test_File_Name := new String'
              (Unit_To_File_Name (Data_Unit_Name.all));

            --  saving test data package name for further reference
            Test_Data_Package_Name := new String'(Data_Unit_Name.all);

            --  Generating simple test data package spec
            if not Is_Regular_File
              (Output_Dir & Directory_Separator & Test_File_Name.all & ".ads")
            then
               Create
                 (Output_Dir & Directory_Separator &
                  Test_File_Name.all & ".ads");

               Put_Test_Data_Header;

               if Current_Pack.Data_Kind = Instantiation then
                  S_Put
                    (0,
                     "with "
                     & Current_Pack.Generic_Containing_Package.all
                     & "."
                     & Test_Data_Unit_Name
                     & ";");
                  Put_New_Line;
                  S_Put
                    (0,
                     "with "
                     & Current_Pack.Generic_Containing_Package.all
                     & "."
                     & Test_Data_Unit_Name
                     & "."
                     & Test_Unit_Name
                     & ";");
               else
                  S_Put (0, "with AUnit.Test_Fixtures;");
               end if;
               Put_New_Line;
               Put_New_Line;
               if Current_Pack.Is_Generic then
                  S_Put (0, "generic");
                  Put_New_Line;
                  S_Put
                    (3,
                     "type GNATtest_Test_Type is new "
                     & "AUnit.Test_Fixtures.Test_Fixture");
                  Put_New_Line;
                  S_Put (5, "with private;");
                  Put_New_Line;
               end if;
               S_Put (0, "package " & Data_Unit_Name.all & " is");
               Put_New_Line;
               Put_New_Line;
               S_Put (0, GT_Marker_Begin);
               Put_New_Line;
               S_Put
                 (3,
                  "type Test is new AUnit.Test_Fixtures.Test_Fixture");
               Put_New_Line;
               S_Put (0, GT_Marker_End);
               Put_New_Line;
               S_Put (3, "with null record;");
               Put_New_Line;
               Put_New_Line;
               S_Put (3, "procedure Set_Up (Gnattest_T : in out Test);");
               Put_New_Line;
               S_Put (3, "procedure Tear_Down (Gnattest_T : in out Test);");
               Put_New_Line;
               Put_New_Line;

               if Current_Pack.Data_Kind = Instantiation then
                  S_Put (0, GT_Marker_Begin);
                  Put_New_Line;
                  S_Put
                    (3,
                     "package Gnattest_Data_Inst is new "
                     & "GNATtest_Generated.GNATtest_Standard."
                     & Current_Pack.Name.all
                     & "."
                     & Test_Data_Unit_Name
                     & " (Test);");
                  Put_New_Line;
                  S_Put
                    (3,
                     "package Gnattest_Tests_Inst is new Gnattest_Data_Inst."
                     & Test_Unit_Name
                     & ";");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                     "type New_Test is new Gnattest_Tests_Inst.Test"
                     & " with null record;");
                  Put_New_Line;
                  S_Put (0, GT_Marker_End);
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                    "procedure User_Set_Up (Gnattest_T : in out New_Test);");
                  Put_New_Line;
                  S_Put
                    (3,
                     "procedure User_Tear_Down "
                     & "(Gnattest_T : in out New_Test);");
                  Put_New_Line;
                  Put_New_Line;
               end if;

               if Current_Pack.Is_Generic then
                  S_Put
                    (3,
                    "procedure User_Set_Up (Gnattest_T : in out Test);");
                  Put_New_Line;
                  S_Put
                    (3,
                    "procedure User_Tear_Down (Gnattest_T : in out Test);");
                  Put_New_Line;
                  Put_New_Line;
               end if;

               S_Put (0, "end " & Data_Unit_Name.all & ";");
               Put_New_Line;

               Close_File;
            end if;

            if not Is_Regular_File
              (Output_Dir & Directory_Separator & Test_File_Name.all & ".adb")
            then
               Create
                 (Output_Dir & Directory_Separator &
                  Test_File_Name.all & ".adb");

               Put_Test_Data_Header;

               S_Put (0, "package body " & Data_Unit_Name.all & " is");
               Put_New_Line;
               Put_New_Line;
               if Current_Pack.Data_Kind = Declaration_Data then
                  S_Put (3, "procedure Set_Up (Gnattest_T : in out Test) is");
                  Put_New_Line;
                  if Current_Pack.Is_Generic then
                     S_Put
                       (6, "X : Test'Class renames Test'Class (Gnattest_T);");
                     Put_New_Line;
                     S_Put (3, "begin");
                     Put_New_Line;
                     S_Put (6, "User_Set_Up (X);");
                  else
                     S_Put (6, "pragma Unreferenced (Gnattest_T);");
                     Put_New_Line;
                     S_Put (3, "begin");
                     Put_New_Line;
                     S_Put (6, "null;");
                  end if;
                  Put_New_Line;
                  S_Put (3, "end Set_Up;");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3, "procedure Tear_Down (Gnattest_T : in out Test) is");
                  Put_New_Line;
                  if Current_Pack.Is_Generic then
                     S_Put
                       (6, "X : Test'Class renames Test'Class (Gnattest_T);");
                     Put_New_Line;
                     S_Put (3, "begin");
                     Put_New_Line;
                     S_Put (6, "User_Tear_Down (X);");
                  else
                     S_Put (6, "pragma Unreferenced (Gnattest_T);");
                     Put_New_Line;
                     S_Put (3, "begin");
                     Put_New_Line;
                     S_Put (6, "null;");
                  end if;
                  Put_New_Line;
                  S_Put (3, "end Tear_Down;");
               else
                  S_Put
                    (3,
                     "procedure Set_Up "
                     & "(Gnattest_T : in out Test) is");
                  Put_New_Line;
                  S_Put (6, "pragma Unreferenced (Gnattest_T);");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end Set_Up;");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                     "procedure Tear_Down "
                     & "(Gnattest_T : in out Test) is");
                  Put_New_Line;
                  S_Put (6, "pragma Unreferenced (Gnattest_T);");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end Tear_Down;");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                     "procedure User_Set_Up "
                     & "(Gnattest_T : in out New_Test) is");
                  Put_New_Line;
                  S_Put (6, "pragma Unreferenced (Gnattest_T);");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end User_Set_Up;");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                     "procedure User_Tear_Down "
                     & "(Gnattest_T : in out New_Test) is");
                  Put_New_Line;
                  S_Put (6, "pragma Unreferenced (Gnattest_T);");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end User_Tear_Down;");

                  Put_New_Line;
                  Put_New_Line;
               end if;

               if Current_Pack.Is_Generic then
                  S_Put
                    (3,
                     "procedure User_Set_Up "
                     & "(Gnattest_T : in out Test) is");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end User_Set_Up;");
                  Put_New_Line;
                  Put_New_Line;
                  S_Put
                    (3,
                     "procedure User_Tear_Down "
                     & "(Gnattest_T : in out Test) is");
                  Put_New_Line;
                  S_Put (3, "begin");
                  Put_New_Line;
                  S_Put (6, "null;");
                  Put_New_Line;
                  S_Put (3, "end User_Tear_Down;");
                  Put_New_Line;
                  Put_New_Line;
               end if;

               S_Put (0, "end " & Data_Unit_Name.all & ";");
               Put_New_Line;

               Close_File;
            end if;

            TP_Map.SetUp_Name      := new String'(Test_File_Name.all & ".adb");
            TP_Map.TearDown_Name   := new String'(Test_File_Name.all & ".adb");
            TP_Map.SetUp_Line      := 8;
            TP_Map.SetUp_Column    := 4;
            TP_Map.TearDown_Line   := 14;
            TP_Map.TearDown_Column := 4;

            Free (Test_File_Name);

            if Current_Pack.Name.all = Data.Unit_Full_Name.all then
               Unit_Name := new String'
                 (Current_Pack.Name.all & "." &
                  Test_Data_Unit_Name & "." &
                  Test_Unit_Name);
            else
               Unit_Name := new String'
                 (Data.Unit_Full_Name.all & "." &
                  Test_Data_Unit_Name & "." &
                  Test_Unit_Name & "." &
                  Nesting_Difference
                    (Current_Pack.Name.all,
                     Data.Unit_Full_Name.all) &
                  "." & Test_Data_Unit_Name & "." & Test_Unit_Name);
            end if;

            Test_File_Name := new String'(Unit_To_File_Name (Unit_Name.all));

            Actual_Test := False;

            --  Generating simple test package spec.
            Create
              (Output_Dir & Directory_Separator & Test_File_Name.all & ".ads");

            Put_Harness_Header;
            S_Put (0, GT_Marker_Begin);
            Put_New_Line;

            S_Put (0, "with Gnattest_Generated;");
            Put_New_Line;
            if Stub_Mode_ON then
               S_Put (0, "with AUnit.Test_Caller;");
               Put_New_Line;
            end if;
            Put_New_Line;
            if Current_Pack.Is_Generic then
               S_Put (0, "generic");
               Put_New_Line;

               declare
                  GP : Generic_Package;
               begin
                  GP.Name := new String'(Current_Pack.Name.all);  --  ???
                  GP.Sloc := new String'
                    (Base_Name (Data.Unit_File_Name.all)
                     & ":"
                     & Trim
                       (Integer'Image
                            (First_Line_Number (Current_Pack.Element)),
                        Both)
                     & ":"
                     & Trim
                       (Integer'Image
                            (First_Column_Number (Current_Pack.Element)),
                        Both));
                  Update_Generic_Packages (GP);
               end;
            end if;

            S_Put (0, "package " & Unit_Name.all & " is");
            Put_New_Line;
            Put_New_Line;

            --  Declaring simple test type.
            if Current_Pack.Data_Kind = Declaration_Data then
               S_Put
                 (3,
                  "type Test is new GNATtest_Generated.GNATtest_Standard." &
                    Data_Unit_Name.all & ".Test");

            else
               S_Put
                 (3,
                  "type Test is new GNATtest_Generated.GNATtest_Standard." &
                    Data_Unit_Name.all & ".New_Test");

               Update_Generic_Packages
                 (Current_Pack.Generic_Containing_Package.all);
            end if;
            Put_New_Line;
            S_Put (3, "with null record;");
            Put_New_Line;
            Put_New_Line;

            --  Adding test routine declarations.

            if Current_Pack.Data_Kind = Declaration_Data then
               Subp_Cur := Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then

                     S_Put
                       (3,
                        "procedure "
                        & Subp_Data_List.Element
                          (Subp_Cur).Subp_Mangle_Name.all
                        & " (Gnattest_T : in out Test);");

                     Put_New_Line;
                     Print_Comment_Declaration
                       (Subp_Data_List.Element (Subp_Cur),
                        3);
                     Put_New_Line;

                     Actual_Test := True;
                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;
            end if;

            if Stub_Mode_ON then
               S_Put (3, "package Caller is new AUnit.Test_Caller (Test);");
               Put_New_Line;
               Put_New_Line;
            end if;

            S_Put (0, "end " & Unit_Name.all & ";");

            Put_New_Line;
            S_Put (0, GT_Marker_End);
            Put_New_Line;

            Close_File;

            TP_Map.TP_Name := new String'(Test_File_Name.all & ".ads");
            TP_List.Append (TP_Map);

            Reset_Line_Counter;

            --  Generating simple test package body
            if Actual_Test then

               if Generate_Separates then
                  Create
                    (Output_Dir
                     & Directory_Separator
                     & Test_File_Name.all
                     & ".adb");
                  Put_Harness_Header;
               else
                  Get_Subprograms_From_Package
                    (Output_Dir
                     & Directory_Separator
                     & Test_File_Name.all
                     & ".adb");

                  --  updating hash v2 to v2.1 and change TC hash to TC names
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then
                        UH.Version := new String'("2");
                        UH.Hash := new String'
                          (Subp_Data_List.Element
                             (Subp_Cur).Subp_Full_Hash.all);
                        if
                          Subp_Data_List.Element (Subp_Cur).Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Subp_Data_List.Element
                                (Subp_Cur).TC_Info.TC_Hash.all);
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);

                        if MD_Cur /= Markered_Data_Maps.No_Element then
                           MD := Markered_Data_Maps.Element (MD_Cur);

                           Free (UH.Version);
                           UH.Version := new String'(Hash_Version);
                           if UH.TC_Hash.all /= "" then
                              Free (UH.TC_Hash);
                              UH.TC_Hash := new String'
                                (Sanitize_TC_Name
                                   (Subp_Data_List.Element
                                      (Subp_Cur).TC_Info.Name.all));
                           end if;
                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;

                  --  gathering transition data
                  if Transition then
                     Subp_Cur := Subp_List.First;
                     loop
                        exit when Subp_Cur = Subp_Data_List.No_Element;

                        if
                          Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0
                        then
                           UH.Version := new String'("1");
                           UH.Hash := new String'
                             (Subp_Data_List.Element
                                (Subp_Cur).Subp_Hash_V1.all);
                           if
                             Subp_Data_List.Element (Subp_Cur).Has_TC_Info
                           then
                              UH.TC_Hash := new String'
                                (Subp_Data_List.Element
                                   (Subp_Cur).TC_Info.TC_Hash.all);
                           else
                              UH.TC_Hash := new String'("");
                           end if;

                           Current_Subp := Subp_Data_List.Element (Subp_Cur);

                           Get_Subprogram_From_Separate
                             (Output_Dir
                              & Directory_Separator
                              & Unit_To_File_Name
                                (Unit_Name.all
                                 & "."
                                 & Test_Routine_Prefix
                                 & Current_Subp.Subp_Text_Name.all
                                 & "_"
                                 & Current_Subp.Subp_Hash_V1
                                   (Current_Subp.Subp_Hash_V1'First ..
                                      Current_Subp.Subp_Hash_V1'First + 5)
                                 & (if Current_Subp.Has_TC_Info
                                   then "_" & Current_Subp.TC_Info.TC_Hash
                                     (Current_Subp.TC_Info.TC_Hash'First ..
                                        Current_Subp.TC_Info.TC_Hash'First + 5)
                                   else ""))
                              & ".adb",
                              UH,
                              Current_Subp);
                        end if;
                        Subp_Data_List.Next (Subp_Cur);
                     end loop;
                  end if;

                  --  gathering used short names
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then
                        UH.Version := new String'(Hash_Version);
                        UH.Hash := new String'
                          (Subp_Data_List.Element
                             (Subp_Cur).Subp_Full_Hash.all);
                        if
                          Subp_Data_List.Element (Subp_Cur).Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Sanitize_TC_Name
                                (Subp_Data_List.Element
                                   (Subp_Cur).TC_Info.Name.all));
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);

                        if MD_Cur /= Markered_Data_Maps.No_Element then
                           MD := Markered_Data_Maps.Element (MD_Cur);
                           if MD.Short_Name_Used then
                              Short_Names_Used.Include
                                (To_Lower (MD.Short_Name.all));
                              Shortnamed_Subps.Include
                                (Current_Subp.Subp_Declaration);

                              Name_Numbers.Include
                                (To_Lower (Current_Subp.Subp_Text_Name.all),
                                 1);
                              Elem_Numbers.Include
                                (Current_Subp.Subp_Declaration, 1);
                           end if;
                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;

                  --  updating short names from markered data with hash v.1
                  --  to hash v.2.1 where possible
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then
                        UH.Version := new String'("1");
                        UH.Hash := new String'(Current_Subp.Subp_Hash_V1.all);

                        if
                          Current_Subp.Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Current_Subp.TC_Info.TC_Hash.all);
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);

                        if MD_Cur /= Markered_Data_Maps.No_Element then
                           MD := Markered_Data_Maps.Element (MD_Cur);

                           Markered_Data_Map.Delete (MD_Cur);
                           Free (UH.Hash);
                           UH.Hash := new String'
                             (Current_Subp.Subp_Hash_V2_1.all);
                           Free (UH.Version);
                           UH.Version := new String'(Hash_Version);
                           if UH.TC_Hash.all /= "" then
                              Free (UH.TC_Hash);
                              UH.TC_Hash := new String'
                                (Sanitize_TC_Name
                                   (Current_Subp.TC_Info.Name.all));
                           end if;
                           Markered_Data_Map.Include (UH, MD);
                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;

                  --  updating short names from markered data with hash v.2.1
                  --  to hash v.2.2 where possible and gnathering short names
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then
                        UH.Version := new String'("2.1");
                        UH.Hash := new String'
                          (Current_Subp.Subp_Hash_V2_1.all);

                        if
                          Current_Subp.Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Sanitize_TC_Name
                                (Current_Subp.TC_Info.Name.all));
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);

                        if MD_Cur /= Markered_Data_Maps.No_Element then
                           MD := Markered_Data_Maps.Element (MD_Cur);

                           if not
                             Short_Names_Used.Contains (MD.Short_Name.all)
                             or else Shortnamed_Subps.Contains
                               (Current_Subp.Subp_Declaration)
                           then
                              Short_Names_Used.Include (MD.Short_Name.all);
                              Shortnamed_Subps.Include
                                (Current_Subp.Subp_Declaration);

                              Name_Numbers.Include
                                (To_Lower (Current_Subp.Subp_Text_Name.all),
                                 1);
                              Elem_Numbers.Include
                                (Current_Subp.Subp_Declaration, 1);

                              MD.Short_Name_Used := True;
                           end if;

                           Markered_Data_Map.Delete (MD_Cur);
                           Free (UH.Hash);
                           UH.Hash := new String'
                             (Current_Subp.Subp_Full_Hash.all);
                           Free (UH.Version);
                           UH.Version := new String'(Hash_Version);

                           Markered_Data_Map.Include (UH, MD);
                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;

                  --  creating markered_data and deciding on new short names
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then
                        UH.Version := new String'(Hash_Version);
                        UH.Hash := new String'
                          (Current_Subp.Subp_Full_Hash.all);
                        if
                          Subp_Data_List.Element (Subp_Cur).Has_TC_Info
                        then
                           UH.TC_Hash := new String'
                             (Sanitize_TC_Name
                                (Current_Subp.TC_Info.Name.all));
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);

                        if MD_Cur = Markered_Data_Maps.No_Element then

                           MD.Commented_Out := False;
                           MD.Short_Name_Used := False;
                           MD.Short_Name := new String'
                             (To_Lower (Current_Subp.Subp_Text_Name.all));
                           MD.TR_Text.Clear;

                           if
                             not Short_Names_Used.Contains
                               (To_Lower (Current_Subp.Subp_Text_Name.all))
                             or else Shortnamed_Subps.Contains
                               (Current_Subp.Subp_Declaration)
                           then
                              --  Short name is free, we can use it
                              MD.Short_Name_Used := True;
                              Short_Names_Used.Include
                                (To_Lower (Current_Subp.Subp_Text_Name.all));
                              Shortnamed_Subps.Include
                                (Current_Subp.Subp_Declaration);

                              Name_Numbers.Include
                                (To_Lower (Current_Subp.Subp_Text_Name.all),
                                 1);
                              Elem_Numbers.Include
                                (Current_Subp.Subp_Declaration, 1);

                              --  Looking for a dangling test with same short
                              --  name but different hash.
                              MD_Cur := Find_Same_Short_Name
                                (Markered_Data_Map,
                                 Current_Subp);

                              if MD_Cur /= Markered_Data_Maps.No_Element then
                                 --  Using corresponding dangling test

                                 MD.TR_Text.Clear;
                                 MD.TR_Text :=
                                   Markered_Data_Maps.Element (MD_Cur).TR_Text;

                                 --  also need to copy Commented_Out since
                                 --  the test can be dangling for a long time
                                 --  or just become dangling
                                 MD.Commented_Out :=
                                   Markered_Data_Maps.Element
                                     (MD_Cur).Commented_Out;

                                 Markered_Data_Map.Delete (MD_Cur);
                                 MD.Issue_Warning := True;
                              end if;

                           end if;

                           Markered_Data_Map.Insert (UH, MD);

                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;

                  --  setting overloading numbers;
                  Subp_Cur := Subp_List.First;
                  loop
                     exit when Subp_Cur = Subp_Data_List.No_Element;

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then

                        if
                          Name_Numbers.Find
                            (To_Lower (Current_Subp.Subp_Text_Name.all)) =
                            Name_Frequency.No_Element
                        then

                           Name_Numbers.Include
                             (To_Lower (Current_Subp.Subp_Text_Name.all), 1);
                           Elem_Numbers.Include
                             (Current_Subp.Subp_Declaration, 1);

                        else
                           if
                             Elem_Numbers.Find
                               (Current_Subp.Subp_Declaration) =
                               Elem_Number_Maps.No_Element
                           then
                              declare
                                 X : constant Natural :=
                                   Name_Numbers.Element
                                     (To_Lower
                                          (Current_Subp.Subp_Text_Name.all));
                              begin
                                 Name_Numbers.Replace
                                   (To_Lower (Current_Subp.Subp_Text_Name.all),
                                    X + 1);
                                 Elem_Numbers.Include
                                   (Current_Subp.Subp_Declaration, X + 1);
                              end;
                           end if;
                        end if;

                     end if;

                     Subp_Data_List.Next (Subp_Cur);
                  end loop;
                  Name_Numbers.Clear;

                  Create (Tmp_File_Name);
                  Put_TP_Header (Test_Data_Package_Name.all);
               end if;

               S_Put (0, "with AUnit.Assertions; use AUnit.Assertions;");
               New_Line_Count;
               S_Put (0, "with System.Assertions;");
               New_Line_Count;
               if Stub_Mode_ON then
                  declare
                     S_Cur : Asis_Element_List.Cursor :=
                       Data.Units_To_Stub.First;
                     Tmp : String_Access;
                  begin
                     while S_Cur /= Asis_Element_List.No_Element loop
                        Tmp := new String'
                             (To_String
                                  (Text_Name
                                       (Enclosing_Compilation_Unit
                                          (Asis_Element_List.Element
                                             (S_Cur)))));

                        if
                          Source_Stubbed (Tmp.all) and then
                          not Excluded_Test_Data_Files.Contains
                            (Base_Name (Get_Source_Stub_Data_Spec (Tmp.all)))
                        then
                           S_Put
                             (0,
                              "with "
                              & To_String
                                (Defining_Name_Image
                                     (First_Name
                                          (Asis_Element_List.Element (S_Cur))))
                              & "."
                              & Stub_Data_Unit_Name
                              & "; use "
                              & To_String
                                (Defining_Name_Image
                                     (First_Name
                                          (Asis_Element_List.Element (S_Cur))))
                              & "."
                              & Stub_Data_Unit_Name
                              & ";");
                           New_Line_Count;
                        end if;

                        Free (Tmp);

                        Next (S_Cur);
                     end loop;
                  end;
               end if;
               New_Line_Count;

               Put_Persistent_Section (With_Clauses);

               S_Put (0, "package body " & Unit_Name.all & " is");
               New_Line_Count;
               New_Line_Count;

               Put_Persistent_Section (Body_Declarations);

               --  Adding test routine body stubs.
               Subp_Cur := Subp_List.First;
               loop
                  exit when Subp_Cur = Subp_Data_List.No_Element;

                  if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then

                     Current_Subp := Subp_Data_List.Element (Subp_Cur);

                     if Subp_Data_List.Element (Subp_Cur).Has_TC_Info then

                        case
                          Declaration_Kind
                            (Subp_Data_List.Element
                                 (Subp_Cur).Subp_Declaration)
                        is

                        when A_Function_Declaration             |
                             An_Expression_Function_Declaration =>
                           Generate_Function_Wrapper
                             (Subp_Data_List.Element (Subp_Cur));

                        when A_Procedure_Declaration =>
                           Generate_Procedure_Wrapper
                             (Subp_Data_List.Element (Subp_Cur));

                        when others =>
                           null;

                        end case;

                     end if;

                     if Generate_Separates then
                        S_Put
                          (3,
                           "procedure "
                           & Subp_Data_List.Element
                             (Subp_Cur).Subp_Mangle_Name.all
                           & " (Gnattest_T : in out Test) is separate;");

                        New_Line_Count;
                        Print_Comment_Declaration
                          (Subp_Data_List.Element (Subp_Cur), 3);
                        New_Line_Count;

                     else

                        Test_Info.Replace
                          (Data.Unit_File_Name.all,
                           Test_Info.Element (Data.Unit_File_Name.all) + 1);

                        All_Tests_Counter := All_Tests_Counter + 1;

                        UH.Version := new String'(Hash_Version);
                        UH.Hash := new String'
                          (Subp_Data_List.Element
                             (Subp_Cur).Subp_Full_Hash.all);
                        if Subp_Data_List.Element (Subp_Cur).Has_TC_Info then
                           UH.TC_Hash := new String'
                             (Sanitize_TC_Name
                                (Subp_Data_List.Element
                                   (Subp_Cur).TC_Info.Name.all));
                        else
                           UH.TC_Hash := new String'("");
                        end if;

                        MD_Cur := Find (Markered_Data_Map, UH);
                        MD := Markered_Data_Maps.Element (MD_Cur);

                        Put_Opening_Comment_Section
                          (Subp_Data_List.Element (Subp_Cur),
                           Elem_Numbers.Element
                             (Current_Subp.Subp_Declaration),
                           Use_Short_Name => MD.Short_Name_Used);

                        if Is_Unimplemented_Test (MD.TR_Text) then
                           TR_SLOC_Buffer.Append
                             ((new String'(Test_File_Name.all & ".ads"),
                              new String'(Test_File_Name.all & ".adb"),
                              null,
                              Subp_Data_List.Element (Subp_Cur),
                              New_Line_Counter));
                        else
                           TR_SLOC_Buffer.Append
                             ((new String'(Test_File_Name.all & ".ads"),
                              new String'(Test_File_Name.all & ".adb"),
                              new String'("modified"),
                              Subp_Data_List.Element (Subp_Cur),
                              New_Line_Counter));
                        end if;

                        if MD.TR_Text.Is_Empty then

                           if Stub_Mode_ON then
                              Gather_Direct_Callees
                                (Current_Subp.Subp_Declaration,
                                 Setters_Set);
                           end if;

                           New_Tests_Counter := New_Tests_Counter + 1;
                           New_Line_Count;
                           S_Put (6, "pragma Unreferenced (Gnattest_T);");
                           New_Line_Count;
                           New_Line_Count;
                           S_Put (3, "begin");
                           New_Line_Count;
                           New_Line_Count;
                           if not Setters_Set.Is_Empty then
                              Set_Cur := Setters_Set.First;
                              while Set_Cur /= String_Set.No_Element loop
                                 S_Put
                                   (3,
                                    "--  "
                                    & String_Set.Element (Set_Cur)
                                    & "( );");
                                 New_Line_Count;
                                 Next (Set_Cur);
                              end loop;
                              New_Line_Count;
                              Setters_Set.Clear;
                           end if;
                           S_Put (6, "AUnit.Assertions.Assert");
                           New_Line_Count;
                           S_Put
                             (8, "(Gnattest_Generated.Default_Assert_Value,");
                           New_Line_Count;
                           S_Put (9,  """Test not implemented."");");
                           New_Line_Count;
                           New_Line_Count;
                        else

                           if MD.Issue_Warning then
                              Report_Std
                                (Base_Name (Data.Unit_File_Name.all)
                                 & ":"
                                 & Trim
                                   (Integer'Image (First_Line_Number
                                    (Current_Subp.Subp_Declaration)),
                                    Both)
                                 & ":"
                                 & Trim
                                   (Integer'Image (First_Column_Number
                                    (Current_Subp.Subp_Declaration)),
                                    Both)
                                 & ": warning: test for "
                                 & MD.Short_Name.all
                                 & " at "
                                 & Unit_Name.all
                                 & ":"
                                 & Trim
                                   (Integer'Image (New_Line_Counter),
                                    Both)
                                 & " might be out of date ("
                                 & MD.Short_Name.all
                                 & " has been changed)");
                           end if;

                           for I in
                             MD.TR_Text.First_Index .. MD.TR_Text.Last_Index
                           loop
                              if MD.Commented_Out then
                                 S_Put
                                   (0,
                                    Uncomment_Line (MD.TR_Text.Element (I)));
                              else
                                 S_Put (0, MD.TR_Text.Element (I));
                              end if;
                              New_Line_Count;
                           end loop;
                        end if;

                        Markered_Data_Map.Delete (MD_Cur);

                        Put_Closing_Comment_Section
                          (Subp_Data_List.Element (Subp_Cur),
                           Elem_Numbers.Element
                             (Current_Subp.Subp_Declaration),
                           Use_Short_Name => MD.Short_Name_Used);
                        New_Line_Count;

                     end if;

                  end if;

                  Subp_Data_List.Next (Subp_Cur);
               end loop;

               --  printing dangling tests

               if not Markered_Data_Map_Is_Empty then
                  Report_Std
                    (" warning: "
                     & Unit_Name.all
                     & " has dangling test(s)");
               end if;

               MD_Cur := Markered_Data_Map.First;
               loop
                  exit when MD_Cur = Markered_Data_Maps.No_Element;

                  MD := Markered_Data_Maps.Element (MD_Cur);

                  if Markered_Data_Maps.Key (MD_Cur).Hash.all /= "02" then
                     declare
                        Stub : Subp_Info;
                     begin

                        Stub.Subp_Full_Hash := new String'
                          (Markered_Data_Maps.Key (MD_Cur).Hash.all);
                        Stub.Subp_Text_Name := new String'
                          (MD.Short_Name.all);

                        if Markered_Data_Maps.Key (MD_Cur).TC_Hash.all = ""
                        then
                           Stub.Has_TC_Info := False;

                           Stub.Subp_Mangle_Name := new String'
                             (Test_Routine_Prefix
                              & Markered_Data_Maps.Element
                                (MD_Cur).Short_Name.all
                              & "_"
                              & Stub.Subp_Full_Hash
                                (Stub.Subp_Full_Hash'First ..
                                     Stub.Subp_Full_Hash'First + 5));

                        else
                           Stub.Has_TC_Info := True;
                           Stub.TC_Info.TC_Hash := new String'
                             (Markered_Data_Maps.Key (MD_Cur).TC_Hash.all);

                           Stub.TC_Info.Name := Stub.TC_Info.TC_Hash;
                           Stub.Subp_Mangle_Name := new String'
                             (Test_Routine_Prefix
                              & Markered_Data_Maps.Element
                                (MD_Cur).Short_Name.all
                              & "_"
                              & Stub.Subp_Full_Hash
                                (Stub.Subp_Full_Hash'First ..
                                     Stub.Subp_Full_Hash'First + 5)
                              & "_"
                              & Stub.TC_Info.TC_Hash.all);
                        end if;

                        Put_Opening_Comment_Section
                          (Stub, 0, True, MD.Short_Name_Used);

                        Add_DT
                          (TP_List,
                           Test_File_Name.all & ".ads",
                           Test_File_Name.all & ".adb",
                           New_Line_Counter,
                           1);

                        for I in
                          MD.TR_Text.First_Index .. MD.TR_Text.Last_Index
                        loop
                           if MD.Commented_Out then
                              S_Put (0, MD.TR_Text.Element (I));
                           else
                              S_Put (0, "--  " & MD.TR_Text.Element (I));
                           end if;
                           New_Line_Count;
                        end loop;

                        Put_Closing_Comment_Section
                          (Stub, 0, True, MD.Short_Name_Used);
                        New_Line_Count;
                     end;
                  end if;

                  Markered_Data_Maps.Next (MD_Cur);
               end loop;

               Put_Persistent_Section (Body_Statements);

               S_Put (0, "end " & Unit_Name.all & ";");
               New_Line_Count;

               Close_File;

               Add_Buffered_TR_Slocs
                 (TP_List,
                  Format_Time
                    (File_Time_Stamp
                       (Tmp_File_Name)));

               if not Generate_Separates then
                  declare
                     Old_Package : constant String :=
                       Output_Dir & Directory_Separator
                       & Test_File_Name.all & ".adb";
                     Success : Boolean;
                  begin
                     if Is_Regular_File (Old_Package) then
                        Delete_File (Old_Package, Success);
                        if not Success then
                           Report_Err ("cannot delete " & Old_Package);
                           raise Fatal_Error;
                        end if;
                     end if;
                     Copy_File (Tmp_File_Name, Old_Package, Success);
                     if not Success then
                        Report_Err ("cannot copy tmp test package to "
                                    & Old_Package);
                        raise Fatal_Error;
                     end if;
                     Delete_File (Tmp_File_Name, Success);
                     if not Success then
                        Report_Err ("cannot delete tmp test package");
                        raise Fatal_Error;
                     end if;
                  end;
               end if;

               Markered_Data_Map.Clear;

            else
               Excluded_Test_Package_Bodies.Include
                 (Test_File_Name.all & ".adb");
            end if;

            Short_Names_Used.Clear;
            Shortnamed_Subps.Clear;
            Elem_Numbers.Clear;
            Subp_List.Clear;
            Package_Info_List.Next (Pack_Cur);
         end loop;

      end if;

      Add_Test_List (Data.Unit_File_Name.all, TP_List);
      TP_List.Clear;

      if Data.Is_Generic then
         Gen_Tests_Storage.Append (Gen_Tests);
      end if;

   end Generate_Test_Package;

   -------------------------------------------
   --  Generate_Test_Package_Instantiation  --
   -------------------------------------------

   procedure Generate_Test_Package_Instantiation (Data : Data_Holder) is
      Output_Dir     : constant String :=
        Get_Source_Output_Dir (Data.Unit_File_Name.all);
      New_Unit_Name  : String_Access;
      Test_File_Name : String_Access;

      Cur_Stor  : Generic_Tests_Storage.Cursor;
      Gen_Tests : Generic_Tests;
      Cur_Test  : List_Of_Strings.Cursor;
   begin

      Cur_Stor := Gen_Tests_Storage.First;
      loop
         exit when Cur_Stor = Generic_Tests_Storage.No_Element;

         Gen_Tests := Generic_Tests_Storage.Element (Cur_Stor);

         if Gen_Tests.Gen_Unit_Full_Name.all = Data.Gen_Unit_Full_Name.all then
            Cur_Test := Gen_Tests.Tested_Type_Names.First;
            loop
               exit when Cur_Test = List_Of_Strings.No_Element;

               New_Unit_Name :=
                 new String'(Data.Unit_Full_Name.all        &
                             "."                            &
                             List_Of_Strings.Element (Cur_Test) &
                             "_"                            &
                             Inst_Test_Unit_Name);
               Test_File_Name :=
                 new String'(Unit_To_File_Name (New_Unit_Name.all));

               Create (Output_Dir & Directory_Separator &
                       Test_File_Name.all & ".ads");

               S_Put
                 (0,
                  "with "                        &
                  Data.Gen_Unit_Full_Name.all    &
                  "."                            &
                  List_Of_Strings.Element (Cur_Test) &
                  Gen_Test_Unit_Name_Suff        &
                  ";");
               Put_New_Line;
               Put_New_Line;
               S_Put (0, "package " & New_Unit_Name.all & " is new");
               Put_New_Line;
               S_Put (2,
                      Data.Unit_Full_Name.all        &
                      "."                            &
                      List_Of_Strings.Element (Cur_Test) &
                      Gen_Test_Unit_Name_Suff
                      & ";");
               Put_New_Line;

               Close_File;

               List_Of_Strings.Next (Cur_Test);
            end loop;

            if Gen_Tests.Has_Simple_Case then

               New_Unit_Name :=
                 new String'(Data.Unit_Full_Name.all        &
                             "."                            &
                             Inst_Test_Unit_Name);
               Test_File_Name :=
                 new String'(Unit_To_File_Name (New_Unit_Name.all));

               Create (Output_Dir & Directory_Separator &
                       Test_File_Name.all & ".ads");

               S_Put
                 (0,
                  "with "                     &
                  Data.Gen_Unit_Full_Name.all &
                  "."                         &
                  Gen_Test_Unit_Name          &
                  ";");
               Put_New_Line;
               Put_New_Line;
               S_Put (0, "package " & New_Unit_Name.all & " is new");
               Put_New_Line;
               S_Put (2,
                      Data.Unit_Full_Name.all      &
                      "."                          &
                      Gen_Test_Unit_Name           &
                      ";");
               Put_New_Line;

               Close_File;

            end if;

            exit;
         end if;

         Generic_Tests_Storage.Next (Cur_Stor);
      end loop;

   end Generate_Test_Package_Instantiation;

   --------------------------
   --  Generate_Skeletons  --
   --------------------------

   procedure Generate_Skeletons (Data : Data_Holder) is
      Output_Dir         : constant String :=
        Get_Source_Output_Dir (Data.Unit_File_Name.all);

      Tmp_File_Name      : constant String :=
        "gnattest_tmp_skeleton";
      --  Name of temporary file created to compare with already existing
      --  skeleton to check if the skeleton was modified by user.

      New_Skeleton : Boolean;
      --  True when the skeleton is generated for the first time.

      Unit_Name          : String_Access;
      --  Test package unit name.

      New_Unit_Full_Name : String_Access;

      Separate_Unit_Name : String_Access;
      --  Full name of the separated unit.

      Separate_File_Name : String_Access;
      --  File name for the separated unit.

      Separated_Name     : String_Access;
      --  Unit name of the separated test routine of environment management.

      Current_Type   : Base_Type_Info;

      Current_Subp : Subp_Info;

      Subp_Cur : Subp_Data_List.Cursor;

      TP_List : TP_Mapping_List.List;

      procedure Set_Current_Type (Type_Numb : Natural);
      --  Looks trough types and nested types and sets the value of
      --  Current_Type with correspondig element.

      procedure Set_Current_Type (Type_Numb : Natural) is
      begin

         for
           I in Data.Type_Data_List.First_Index ..
             Data.Type_Data_List.Last_Index
         loop

            if
              Data.Type_Data_List.Element (I).Type_Number = Type_Numb
            then
               Current_Type   := Data.Type_Data_List.Element (I);
               exit;
            end if;

         end loop;

      end Set_Current_Type;

   begin

      Test_Info.Include (Data.Unit_File_Name.all, 0);

      --  Setting up TP_List if there is one already from test_data stage.
      if GNATtest.Mapping.Mapping.Find (Data.Unit_File_Name.all) /=
        SP_Mapping.No_Element
      then
         TP_List :=
           SP_Mapping.Element
             (GNATtest.Mapping.Mapping.Find (Data.Unit_File_Name.all)).
               Test_Info;
      end if;

      --  Test routines.
      Subp_Cur := Data.Subp_List.First;
      loop
         exit when Subp_Cur = Subp_Data_List.No_Element;

         Current_Subp := Subp_Data_List.Element (Subp_Cur);

         Set_Current_Type (Current_Subp.Corresp_Type);

         if not Current_Subp.Is_Abstract then

            Separated_Name := new String'
              (Current_Subp.Subp_Mangle_Name.all);

            if Current_Subp.Nesting.all = Data.Unit_Full_Name.all then
               if Current_Subp.Corresp_Type = 0 then
                  if Data.Is_Generic then
                     New_Unit_Full_Name :=
                       new String'(Data.Unit_Full_Name.all &
                                   "."                     &
                                   Gen_Test_Unit_Name);
                  else
                     New_Unit_Full_Name :=
                       new String'(Data.Unit_Full_Name.all &
                                   "."                     &
                                   Test_Data_Unit_Name     &
                                   "."                     &
                                   Test_Unit_Name);
                  end if;
               else
                  New_Unit_Full_Name := new String'(Data.Unit_Full_Name.all);
               end if;
            else
               if Current_Subp.Corresp_Type = 0 then
                  New_Unit_Full_Name := new String'
                    (Data.Unit_Full_Name.all & "." &
                     Test_Data_Unit_Name & "."     &
                     Test_Unit_Name & "."          &
                     Nesting_Difference
                       (Current_Subp.Nesting.all,
                        Data.Unit_Full_Name.all) &
                     "." & Test_Data_Unit_Name & "." & Test_Unit_Name);

               else
                  Set_Current_Type (Current_Subp.Corresp_Type);

                  if Current_Type.Nesting.all = Data.Unit_Full_Name.all then
                     New_Unit_Full_Name := new String'
                       (Data.Unit_Full_Name.all & "." &
                        Nesting_Difference
                          (Current_Subp.Nesting.all,
                           Data.Unit_Full_Name.all));
                  else
                     New_Unit_Full_Name := new String'
                       (Data.Unit_Full_Name.all & "." &
                        Test_Data_Unit_Name & "." &
                        Test_Unit_Name & "." &
                        Nesting_Difference
                          (Current_Subp.Nesting.all,
                           Data.Unit_Full_Name.all));
                  end if;
               end if;
            end if;

            if Current_Subp.Corresp_Type = 0 then

               Unit_Name := new String'(New_Unit_Full_Name.all);

            else

               if Data.Is_Generic then
                  Unit_Name := new
                    String'(New_Unit_Full_Name.all              &
                            "."                                  &
                            Current_Type.Main_Type_Text_Name.all &
                            Gen_Test_Unit_Name_Suff);
               else
                  Unit_Name := new
                    String'(New_Unit_Full_Name.all              &
                            "."                                  &
                            Current_Type.Main_Type_Text_Name.all &
                            Test_Data_Unit_Name_Suff             &
                            "."                                  &
                            Current_Type.Main_Type_Text_Name.all &
                            Test_Unit_Name_Suff);
               end if;

            end if;

            Free (New_Unit_Full_Name);

            Separate_Unit_Name := new
              String'(Unit_Name.all &
                      "."           &
                      Separated_Name.all);

            Separate_File_Name :=
              new String'(Unit_To_File_Name (Separate_Unit_Name.all) & ".adb");

            Test_Info.Replace
              (Data.Unit_File_Name.all,
               Test_Info.Element (Data.Unit_File_Name.all) + 1);

            All_Tests_Counter := All_Tests_Counter + 1;

            if not Is_Regular_File (Output_Dir          &
                                    Directory_Separator &
                                    Separate_File_Name.all)
            then

               New_Tests_Counter := New_Tests_Counter + 1;

               Create
                 (Output_Dir & Directory_Separator & Separate_File_Name.all);

               New_Skeleton := True;
            else
               Create (Tmp_File_Name);
               New_Skeleton := False;
            end if;

            Print_Comment_Separate
              (Subp_Data_List.Element (Subp_Cur));
            Put_New_Line;
            S_Put (0, "with Gnattest_Generated;");
            Put_New_Line;
            Put_New_Line;
            S_Put (0, "separate (" & Unit_Name.all & ")");
            Put_New_Line;

            if not Subp_Data_List.Element (Subp_Cur).Is_Abstract then
               S_Put
                 (0,
                  "procedure "       &
                  Separated_Name.all &
                  " (Gnattest_T : in out ");

               if Subp_Data_List.Element (Subp_Cur).Corresp_Type = 0 then
                  S_Put (0, "Test) is");
               else
                  S_Put
                    (0,
                     "Test_"                              &
                     Current_Type.Main_Type_Text_Name.all &
                     ") is");
               end if;
               Put_New_Line;
               S_Put (3, "pragma Unreferenced (Gnattest_T);");
               Put_New_Line;

               if Subp_Data_List.Element (Subp_Cur).Has_TC_Info then
                  Put_Wrapper_Rename (3, Subp_Data_List.Element (Subp_Cur));
               end if;

               S_Put (0, "begin");
               Put_New_Line;
               S_Put (3,
                      "AUnit.Assertions.Assert");
               Put_New_Line;
               S_Put (5, "(Gnattest_Generated.Default_Assert_Value,");
               Put_New_Line;
               S_Put (6,  """Test not implemented."");");
               Put_New_Line;
               S_Put (0, "end " & Separated_Name.all & ";");
               Put_New_Line;

            end if;

            Close_File;

            declare
               Skeleton_Time : constant OS_Time :=
                 File_Time_Stamp
                   (Output_Dir          &
                    Directory_Separator &
                    Separate_File_Name.all);

               Old_File, New_File : Ada.Text_IO.File_Type;
               Old_File_Line, New_File_Line : String_Access;
               Idx : Integer;

               Unmodified : Boolean := True;
            begin
               if New_Skeleton then
                  Add_TR
                    (TP_List,
                     Unit_To_File_Name (Unit_Name.all) & ".ads",
                     Separate_File_Name.all,
                     Format_Time (Skeleton_Time),
                     Subp_Data_List.Element (Subp_Cur));
               else
                  Open (New_File, In_File, Tmp_File_Name);
                  Open
                    (Old_File, In_File,
                     Output_Dir          &
                     Directory_Separator &
                     Separate_File_Name.all);

                  --  Skipping header comments from both new and old skeletons.
                  --  Simple reformatting of source code can lead to
                  --  differences in how tested subprogram image is presented
                  --  while the test itself is still unmodified.
                  loop
                     exit when End_Of_File (Old_File);
                     Old_File_Line := new String'(Get_Line (Old_File));
                     Idx := Old_File_Line'First;
                     if
                       Old_File_Line'Length > 1 and then
                       Old_File_Line (Idx .. Idx + 1) = "--"
                     then
                        Free (Old_File_Line);
                     else
                        exit;
                     end if;
                  end loop;

                  loop
                     exit when End_Of_File (New_File);
                     New_File_Line := new String'(Get_Line (New_File));
                     Idx := New_File_Line'First;
                     if
                       New_File_Line'Length > 1 and then
                       New_File_Line (Idx .. Idx + 1) = "--"
                     then
                        Free (New_File_Line);
                     else
                        exit;
                     end if;
                  end loop;

                  loop
                     if
                       End_Of_File (New_File) and not End_Of_File (Old_File)
                     then
                        Unmodified := False;
                        exit;
                     end if;

                     if
                       End_Of_File (Old_File) and not End_Of_File (New_File)
                     then
                        Unmodified := False;
                        exit;
                     end if;

                     if End_Of_File (Old_File) and End_Of_File (New_File) then
                        exit;
                     end if;

                     Old_File_Line := new String'(Get_Line (Old_File));
                     New_File_Line := new String'(Get_Line (New_File));
                     if Old_File_Line.all /= New_File_Line.all then
                        Unmodified := False;
                        exit;
                     end if;
                  end loop;

                  if Unmodified then
                     Add_TR
                       (TP_List,
                        Unit_To_File_Name (Unit_Name.all) & ".ads",
                        Separate_File_Name.all,
                        Format_Time (Skeleton_Time),
                        Subp_Data_List.Element (Subp_Cur));
                  else
                     Add_TR
                       (TP_List,
                        Unit_To_File_Name (Unit_Name.all) & ".ads",
                        Separate_File_Name.all,
                        "modified",
                        Subp_Data_List.Element (Subp_Cur));
                  end if;

                  Close (New_File);
                  Close (Old_File);
               end if;
            end;

            Free (Separate_Unit_Name);
            Free (Separate_File_Name);
            Free (Separated_Name);
         end if;

         Subp_Data_List.Next (Subp_Cur);
      end loop;

      Add_Test_List (Data.Unit_File_Name.all, TP_List);
      TP_List.Clear;

   end Generate_Skeletons;

   ----------------------------------
   -- Get_Subprogram_From_Separate --
   ----------------------------------

   procedure Get_Subprogram_From_Separate
     (File : String;
      UH   : Unique_Hash;
      Subp : Subp_Info)
   is
      Input_File : Ada.Text_IO.File_Type;
      MD : Markered_Data;
      Line : String_Access;
      Append_Line : Boolean;
   begin
      if not Is_Regular_File (File) then
         return;
      end if;

      MD.Commented_Out := False;
      MD.TR_Text := String_Vectors.Empty_Vector;
      MD.Short_Name := new String'(Subp.Subp_Text_Name.all);

      Open (Input_File, In_File, File);

      loop
         exit when End_Of_File (Input_File);
         Line := new String'(Get_Line (Input_File));
         Append_Line := True;

         if To_Lower (Line.all) = "with gnattest_generated;" then
            Append_Line := False;
         end if;

         --  skipping test routine profile up to declaration section;
         --  depending on line breaks it can take different number of lines
         if Index (To_Lower (Line.all), "separate", Line'First) /= 0 then
            loop
               if
                 Index (To_Lower (Line.all), ") is", Line'First) /= 0
                 or else Trim (To_Lower (Line.all), Both) = "is"
               then
                  Append_Line := False;
                  exit;
               else
                  Free (Line);
                  Line := new String'(Get_Line (Input_File));
               end if;
            end loop;
         end if;

         --  skipping "end test_outine_name;"
         if
           Index
             (To_Lower (Line.all),
              "end "
              & To_Lower
                (Test_Routine_Prefix
                 & Subp.Subp_Text_Name.all
                 & "_"
                 & Subp.Subp_Hash_V1
                   (Subp.Subp_Hash_V1'First .. Subp.Subp_Hash_V1'First + 5))
              & ";",
              Line'First) /= 0
         then
            Append_Line := False;
         end if;

         if Append_Line then
            MD.TR_Text.Append (Line.all);
         end if;

         Free (Line);
      end loop;

      Close (Input_File);

      if Find (Markered_Data_Map, UH) = Markered_Data_Maps.No_Element then
         Markered_Data_Map.Insert (UH, MD);
      else
         Markered_Data_Map.Replace (UH, MD);
      end if;

   end Get_Subprogram_From_Separate;

   ----------------------------------
   -- Get_Subprograms_From_Package --
   ----------------------------------

   procedure Get_Subprograms_From_Package (File : String) is

      Input_File : Ada.Text_IO.File_Type;

      Line_Counter : Natural := 0;

      Line : String_Access;

      Idx, Idx2 : Natural;

      UH : Unique_Hash;
      MD : Markered_Data;

      ID_Found : Boolean;

      type Parsing_Modes is (TR, Marker, Other);

      Parsing_Mode      : Parsing_Modes := Other;
      Prev_Parsing_Mode : Parsing_Modes := Other;

      procedure Report_Corrupted_Marker;
      pragma Unreferenced (Report_Corrupted_Marker);

      procedure Report_Corrupted_Marker is
      begin
         Report_Err
           ("gnattest: marker corrupted at "
            & Base_Name (File)
            & ":"
            & Natural'Image (Line_Counter));
      end Report_Corrupted_Marker;

   begin

      if not Is_Regular_File (File) then
         return;
      end if;

      MD.Commented_Out   := False;
      MD.Short_Name_Used := False;
      MD.TR_Text := String_Vectors.Empty_Vector;
      UH.Hash    := new String'("");
      UH.TC_Hash := new String'("");

      Open (Input_File, In_File, File);

      loop
         exit when End_Of_File (Input_File);

         Line := new String'(Get_Line (Input_File));
         Line_Counter := Line_Counter + 1;

         case Parsing_Mode is
            when Other =>
               if Index (Line.all, GT_Marker_Begin) /= 0 then
                  Parsing_Mode := Marker;
                  Prev_Parsing_Mode := Other;
                  ID_Found := False;
               end if;

            when Marker =>

               Idx := Index (Line.all, "--  id:");
               if Idx /= 0 then
                  ID_Found := True;

                  Idx  := Idx + 7;
                  Idx2 := Index (Line.all, "/", Idx + 1);
                  UH.Version := new String'(Line (Idx .. Idx2 - 1));

                  Idx  := Idx2 + 1;
                  Idx2 := Index (Line.all, "/", Idx + 1);
                  UH.Hash := new String'(Line (Idx .. Idx2 - 1));

                  Idx  := Idx2 + 1;
                  Idx2 := Index (Line.all, "/", Idx + 1);
                  MD.Short_Name := new String'(Line (Idx .. Idx2 - 1));

                  Idx  := Idx2 + 1;
                  Idx2 := Index (Line.all, "/", Idx + 1);
                  if Line (Idx .. Idx2 - 1) = "1" then
                     MD.Short_Name_Used := True;
                  else
                     MD.Short_Name_Used := False;
                  end if;

                  Idx  := Idx2 + 1;
                  Idx2 := Index (Line.all, "/", Idx + 1);
                  if Line (Idx .. Idx2 - 1) = "1" then
                     MD.Commented_Out := True;
                  else
                     MD.Commented_Out := False;
                  end if;

                  if Idx2 < Line'Last then

                     Idx  := Idx2 + 1;
                     Idx2 := Index (Line.all, "/", Idx + 1);
                     UH.TC_Hash := new String'(Line (Idx .. Idx2 - 1));

                  end if;

               else

                  if Index (Line.all, GT_Marker_End) /= 0 then
                     if Prev_Parsing_Mode = Other then
                        if ID_Found then
                           Parsing_Mode := TR;
                        else
                           Parsing_Mode := Other;
                        end if;
                     end if;
                     if Prev_Parsing_Mode = TR then
                        Parsing_Mode := Other;
                     end if;
                  end if;

               end if;

            when TR =>

               if Index (Line.all, GT_Marker_Begin) /= 0 then
                  Markered_Data_Map.Include (UH, MD);
                  Prev_Parsing_Mode := TR;
                  Parsing_Mode := Marker;

                  MD.Commented_Out   := False;
                  MD.Short_Name_Used := False;
                  MD.TR_Text := String_Vectors.Empty_Vector;
                  UH.Hash    := new String'("");
                  UH.TC_Hash := new String'("");
               else
                  MD.TR_Text.Append (Line.all);
               end if;

         end case;

      end loop;

      Close (Input_File);
   end Get_Subprograms_From_Package;

   --------------------------
   --  Initialize_Context  --
   --------------------------

   function Initialize_Context (Source_Name : String) return Boolean is
      Success : Boolean;

      use type Asis.Errors.Error_Kinds; --  for EC12-013
   begin

      Create_Tree (Source_Name, Success);

      if not Success then
         Set_Source_Status (Source_Name, Bad_Content);

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

               Set_Source_Status (Source_Name, Bad_Content);
               Success := False;
            else
               raise;
            end if;

      end;

      return Success;
   end Initialize_Context;

   ----------------------------
   -- Is_Callable_Subprogram --
   ----------------------------

   function Is_Callable_Subprogram (Subp : Asis.Element) return Boolean
   is
   begin
      if Trait_Kind (Subp) = An_Abstract_Trait then
         return False;
      end if;
      if Declaration_Kind (Subp) = A_Null_Procedure_Declaration then
         return False;
      end if;
      return True;
   end Is_Callable_Subprogram;

   ------------------------------------
   -- Is_Declared_In_Regular_Package --
   ------------------------------------

   function Is_Declared_In_Regular_Package
     (Elem : Asis.Element)
      return Boolean
   is
      Encl : Asis.Element := Enclosing_Element (Elem);
   begin
      loop
         exit when Is_Nil (Encl);

         if Declaration_Kind (Encl) /= A_Package_Declaration then
            return False;
         end if;

         Encl := Enclosing_Element (Encl);

      end loop;

      return True;

   end Is_Declared_In_Regular_Package;

   ----------------------
   -- Is_Fully_Private --
   ----------------------

   function Is_Fully_Private
     (Arg : Asis.Declaration) return Boolean
   is
      Decl_1, Decl_2, Decl_3 : Asis.Element :=
        Asis.Nil_Element;

      function Is_Private_Or_Null (El : Asis.Element) return Boolean is
         (if Is_Nil (El) then True else Is_Private (El));

   begin
      case Declaration_Kind (Arg) is
         when An_Ordinary_Type_Declaration =>
            Decl_3 := Arg;
            Decl_2 := Corresponding_Type_Partial_View (Arg);
            if Declaration_Kind (Decl_2) in
              A_Private_Type_Declaration | A_Private_Extension_Declaration
            then
               Decl_1 := Corresponding_Type_Partial_View (Decl_2);
            end if;
         when A_Private_Type_Declaration      |
              A_Private_Extension_Declaration =>
            Decl_3 := Corresponding_Type_Completion (Arg);
            Decl_2 := Arg;
            Decl_1 := Corresponding_Type_Partial_View (Arg);
         when An_Incomplete_Type_Declaration       |
              A_Tagged_Incomplete_Type_Declaration =>
            Decl_1 := Arg;
            Decl_2 := Corresponding_Type_Completion (Arg);
            if Declaration_Kind (Decl_2) in
              A_Private_Type_Declaration | A_Private_Extension_Declaration
            then
               Decl_3 := Corresponding_Type_Completion (Decl_2);
            end if;
         when others =>
            null;
      end case;

      return Is_Private_Or_Null (Decl_1)
        and then Is_Private_Or_Null (Decl_2)
        and then Is_Private_Or_Null (Decl_3);
   end Is_Fully_Private;

   -----------------
   -- Mangle_Hash --
   -----------------

   function Mangle_Hash
     (Subp       : Asis.Declaration) return String
   is
      Full_Hash : String_Access;
   begin

      if Generate_Separates then
         Full_Hash := new String'(Mangle_Hash_Full (Subp, True));
      else
         Full_Hash := new String'(Mangle_Hash_Full (Subp));
      end if;

      return
        Test_Routine_Prefix
        & Get_Subp_Name (Subp)
        & "_"
        & Full_Hash (Full_Hash'First .. Full_Hash'First + 5);
   end Mangle_Hash;

   -------------------------------------
   -- No_Inheritance_Through_Generics --
   -------------------------------------

   function No_Inheritance_Through_Generics
     (Inheritance_Root_Type : Asis.Element;
      Inheritance_Final_Type : Asis.Element)
         return Boolean
   is
      Elem  : Asis.Element := Inheritance_Final_Type;
      Elem2 : Asis.Element := Inheritance_Root_Type;
   begin
      if Declaration_Kind (Inheritance_Root_Type) in
        A_Private_Type_Declaration | A_Private_Extension_Declaration
      then
         Elem2 := Corresponding_Type_Completion (Elem2);
      end if;

      loop
         if not Is_Declared_In_Regular_Package (Elem) then
            return False;
         end if;

         exit when
           Is_Equal (Elem, Elem2);
         Elem := Parent_Type_Declaration (Elem);
      end loop;
      return True;
   end No_Inheritance_Through_Generics;

   -------------------------------
   -- Print_Comment_Declaration --
   -------------------------------

   procedure Print_Comment_Declaration (Subp : Subp_Info; Span : Natural := 0)
   is
      File_Name : constant String    := Base_Name (To_String (Text_Name
        (Enclosing_Compilation_Unit (Subp.Subp_Declaration))));

      Elem_Span : constant Asis.Text.Span :=
        Element_Span (Subp.Subp_Declaration);
   begin
      S_Put
        (Span,
         "--  " &
         File_Name &
         ":" &
         Trim (Integer'Image (Elem_Span.First_Line), Both) &
         ":" &
         Trim (Integer'Image (Elem_Span.First_Column), Both) &
         ":" &
         Subp.Subp_Text_Name.all);
      if Subp.Has_TC_Info then
         S_Put (0, ":" & Subp.TC_Info.Name.all);
      end if;
      New_Line_Count;
   end Print_Comment_Declaration;

   ----------------------------
   -- Print_Comment_Separate --
   ----------------------------

   procedure Print_Comment_Separate (Subp : Subp_Info; Span : Natural := 0) is

      Params : constant Parameter_Specification_List :=
        Parameter_Profile (Subp.Subp_Declaration);

      Subp_Name : constant String := Get_Subp_Name (Subp.Subp_Declaration);

      Func_Profile_Span : Asis.Text.Span;
      Last_Arg_Span     : Asis.Text.Span;

   begin

      case Declaration_Kind (Subp.Subp_Declaration) is
         when A_Procedure_Declaration          |
              A_Procedure_Renaming_Declaration =>

            if Params'Length = 0 then

               S_Put (Span, "--  procedure " & Subp_Name);
               Put_New_Line;

            else

               Last_Arg_Span.First_Line :=
                 Element_Span (Subp.Subp_Declaration).First_Line;
               Last_Arg_Span.First_Column :=
                 Element_Span (Subp.Subp_Declaration).First_Column;
               Last_Arg_Span.Last_Line :=
                 Element_Span (Params (Params'First)).Last_Line;
               Last_Arg_Span.Last_Column :=
                 Element_Span (Params (Params'First)).Last_Column;

               declare
                  Proc_Lines : constant Line_List :=
                    Lines (Subp.Subp_Declaration, Last_Arg_Span);
               begin
                  for I in Proc_Lines'Range loop
                     S_Put
                       (Span,
                        "--  " &
                        Trim
                          (To_String (Non_Comment_Image (Proc_Lines (I))),
                           Both));
                     if I = Proc_Lines'Last then
                        S_Put (0, ")");
                     end if;
                     Put_New_Line;
                  end loop;
               end;
            end if;

         when others =>

            Func_Profile_Span.First_Line :=
              Element_Span (Subp.Subp_Declaration).First_Line;
            Func_Profile_Span.First_Column :=
              Element_Span (Subp.Subp_Declaration).First_Column;
            Func_Profile_Span.Last_Line :=
              Element_Span (Result_Profile (Subp.Subp_Declaration)).Last_Line;
            Func_Profile_Span.Last_Column :=
              Element_Span
                (Result_Profile (Subp.Subp_Declaration)).Last_Column;

            declare
               Func_Lines : constant Line_List :=
                 Lines (Subp.Subp_Declaration, Func_Profile_Span);
            begin
               for I in Func_Lines'Range loop
                  S_Put
                    (Span,
                     "--  " &
                     Trim
                       (To_String (Non_Comment_Image (Func_Lines (I))), Both));
                  Put_New_Line;
               end loop;
            end;

      end case;

      if Subp.Has_TC_Info then
         S_Put (Span, "--  Test Case """ & Subp.TC_Info.Name.all & """");
         Put_New_Line;
      end if;
   end Print_Comment_Separate;

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source (The_Unit : Asis.Compilation_Unit) is
      Source_Name      : String_Access;
      Data             : Data_Holder;

      Suite_Data_List  : Suites_Data_Type;
      Suite_Data       : GNATtest.Harness.Generator.Data_Holder;

      Apropriate_Source : Boolean;

      Test_Packages : String_Set.Set;
      Cur : String_Set.Cursor;

      procedure Get_Test_Packages_List (S_Data : Suites_Data_Type);

      function Get_Suite_Components
        (S_Data       : Suites_Data_Type;
         Package_Name : String)
         return GNATtest.Harness.Generator.Data_Holder;

      procedure Get_Test_Packages_List (S_Data : Suites_Data_Type)
      is
         Declared_In_Generic : Boolean;
         Elem : Asis.Element;
      begin
         for K in S_Data.TR_List.First_Index .. S_Data.TR_List.Last_Index loop

            Declared_In_Generic := False;
            Elem := S_Data.TR_List.Element (K).Original_Subp;
            loop
               exit when Is_Nil (Elem);

               if Declaration_Kind (Elem) = A_Generic_Package_Declaration then
                  Declared_In_Generic := True;
                  exit;
               end if;

               Elem := Enclosing_Element (Elem);
            end loop;

            if not Declared_In_Generic then
               Test_Packages.Include
                 (S_Data.TR_List.Element (K).Test_Package.all);
            end if;
         end loop;

         for
           K in S_Data.ITR_List.First_Index .. S_Data.ITR_List.Last_Index
         loop
            Test_Packages.Include
              (S_Data.ITR_List.Element (K).Test_Package.all);
         end loop;
      end Get_Test_Packages_List;

      function Get_Suite_Components
        (S_Data       : Suites_Data_Type;
         Package_Name : String)
         return GNATtest.Harness.Generator.Data_Holder
      is
         Suite_Data   : GNATtest.Harness.Generator.Data_Holder;
         Test_Routine : GNATtest.Harness.Generator.Test_Routine_Info;
         TT   : GNATtest.Harness.Generator.Test_Type_Info;
         TR_E : GNATtest.Harness.Generator.Test_Routine_Info_Enhanced;

         package Test_Type_Origins is new
           Ada.Containers.Vectors (Positive, Asis.Element, Is_Equal);
         use Test_Type_Origins;

         TT_Origins : Test_Type_Origins.Vector;
         --  Used to set test type numbers.

         Original_Type : Asis.Element;

         Type_Found : Boolean;
      begin

         Suite_Data.Test_Unit_Full_Name := new String'(Package_Name);

         for
           K in S_Data.Test_Types.First_Index .. S_Data.Test_Types.Last_Index
         loop

            if
              S_Data.Test_Types.Element (K).Test_Package.all = Package_Name
            then
               TT := S_Data.Test_Types.Element (K).TT_Info;
               TT.Tested_Type := S_Data.Test_Types.Element (K).Original_Type;
               Suite_Data.Test_Types.Append (TT);
               TT_Origins.Append (S_Data.Test_Types.Element (K).Original_Type);
            end if;
         end loop;

         for K in S_Data.TR_List.First_Index .. S_Data.TR_List.Last_Index loop

            if S_Data.TR_List.Element (K).Test_Package.all = Package_Name then

               Test_Routine := S_Data.TR_List.Element (K).TR_Info;

               --  Setting test type number;

               Original_Type := S_Data.TR_List.Element (K).Original_Type;
               Type_Found := False;

               for L in TT_Origins.First_Index .. TT_Origins.Last_Index loop
                  if Is_Equal (TT_Origins.Element (L), Original_Type) then
                     Test_Routine.Test_Type_Numb := L;
                     Type_Found := True;
                     exit;
                  end if;
               end loop;

               if Type_Found then
                  Suite_Data.TR_List.Append (Test_Routine);
                  Suite_Data.Good_For_Suite := True;
               end if;
            end if;
         end loop;

         for
           K in S_Data.ITR_List.First_Index .. S_Data.ITR_List.Last_Index
         loop
            if S_Data.ITR_List.Element (K).Test_Package.all = Package_Name then

               TR_E := S_Data.ITR_List.Element (K).TR_Info;

               --  Setting up test type number

               Original_Type := S_Data.ITR_List.Element (K).Original_Type;
               Type_Found := False;

               for L in TT_Origins.First_Index .. TT_Origins.Last_Index loop
                  if Is_Equal (TT_Origins.Element (L), Original_Type) then
                     TR_E.Test_Type_Numb := L;
                     Type_Found := True;
                     exit;
                  end if;
               end loop;

               if Type_Found then
                  Suite_Data.ITR_List.Append (TR_E);
                  Suite_Data.Good_For_Suite := True;
               end if;

            end if;
         end loop;

         for
           K in S_Data.LTR_List.First_Index .. S_Data.LTR_List.Last_Index
         loop
            if S_Data.LTR_List.Element (K).Test_Package.all = Package_Name then

               TR_E := S_Data.LTR_List.Element (K).TR_Info;

               --  Setting up test type number

               Original_Type := S_Data.LTR_List.Element (K).Original_Type;
               Type_Found := False;

               for L in TT_Origins.First_Index .. TT_Origins.Last_Index loop
                  if Is_Equal (TT_Origins.Element (L), Original_Type) then
                     TR_E.Test_Type_Numb := L;
                     Type_Found := True;
                     exit;
                  end if;
               end loop;

               if Type_Found then
                  TR_E.Tested_Type := Original_Type;
                  Suite_Data.LTR_List.Append (TR_E);
                  Suite_Data.Good_For_Substitution  := True;
               end if;
            end if;
         end loop;

         TT_Origins.Clear;

         return Suite_Data;

      end Get_Suite_Components;

   begin

      Source_Name :=
        new String'(To_String (Text_Name (The_Unit)));

      Report_Source (Source_Name.all);

      Gather_Data (The_Unit, Data, Suite_Data_List, Apropriate_Source);

      if Apropriate_Source then

         --  First, create stubs if needed. This will allow to import stub_data
         --  packages into test packages only for actually stubbed dependencies
         if Stub_Mode_ON then
            Process_Stubs (Data.Units_To_Stub);
         end if;

         declare
            F : File_Array_Access;
         begin
            Append
              (F,
               GNATCOLL.VFS.Create
                 (+(Get_Source_Output_Dir (Source_Name.all))));
            Create_Dirs (F);
         end;

         if Substitution_Suite then
            Gather_Substitution_Data (Suite_Data_List);
         end if;
         if Data.Data_Kind = Declaration_Data then
            Generate_Nested_Hierarchy (Data);
            Generate_Test_Package (Data);
            if Generate_Separates then
               Generate_Skeletons (Data);
            end if;

            Get_Test_Packages_List (Suite_Data_List);
            Cur := Test_Packages.First;
            loop
               exit when Cur = String_Set.No_Element;

               Suite_Data := Get_Suite_Components
                 (Suite_Data_List,
                  String_Set.Element (Cur));

               if Suite_Data.Good_For_Suite then
                  if not Stub_Mode_ON and then not Separate_Drivers then

                     GNATtest.Harness.Generator.Generate_Suite (Suite_Data);

                     if Suite_Data.Good_For_Substitution  then
                        GNATtest.Harness.Generator.
                          Generate_Substitution_Suite_From_Tested (Suite_Data);
                     end if;
                  end if;
               end if;

               String_Set.Next (Cur);
            end loop;

            if Stub_Mode_ON or else Separate_Drivers then

               Cur := Test_Packages.First;
               while Cur /= String_Set.No_Element loop

                  Suite_Data := Get_Suite_Components
                    (Suite_Data_List,
                     String_Set.Element (Cur));

                  if Suite_Data.Good_For_Suite then
                     GNATtest.Harness.Generator.Generate_Test_Drivers
                       (Suite_Data,
                        Data.Unit_File_Name.all,
                        Data.Units_To_Stub);
                  end if;
                  if Suite_Data.Good_For_Substitution
                    and then not Driver_Per_Unit
                  then
                     GNATtest.Harness.Generator.
                       Generate_Substitution_Test_Drivers (Suite_Data);
                  end if;
                  String_Set.Next (Cur);
               end loop;
            end if;

         end if;
         if Data.Data_Kind = Instantiation then
            Generate_Test_Package_Instantiation (Data);
         end if;
         Set_Source_Status (Source_Name.all, Processed);
      end if;

      if Data.Data_Kind = Declaration_Data then
         Clear (Data.Type_Data_List);
         Clear (Data.Subp_List);
         Clear (Data.Package_Data_List);
         Clear (Data.Subp_Name_Frequency);
         Clear (Data.Units_To_Stub);
      end if;

      Suite_Data.Test_Types.Clear;
      Suite_Data.TR_List.Clear;
      Suite_Data.ITR_List.Clear;
      Suite_Data.LTR_List.Clear;

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
                 new String'(To_String (Text_Name (All_CU (J))));

               if
                 Source_Present (File_Name.all) and then
                 Get_Source_Status (File_Name.all) = Waiting and then
                 not Has_Limited_View_Only (All_CU (J))
               then
                  Process_Source (All_CU (J));
               end if;

               Free (File_Name);
            end if;
         end loop;

      end Iterate_Sources;

      Cur : Tests_Per_Unit.Cursor;

   begin

      Asis.Implementation.Initialize ("-asis05 -ws");

      loop
         Source_Name := new String'(Next_Non_Processed_Source);
         exit when Source_Name.all = "";

         if
           (Stub_Mode_ON or else not Main_Units.Is_Empty)
           and then Get_Source_Body (Source_Name.all) /= ""
         then
            Successful_Initialization :=
              Initialize_Context (Get_Source_Body (Source_Name.all));

            if
              Get_Source_Status
                (Get_Source_Body (Source_Name.all)) = Bad_Content
            then
               --  If correspondig body is bad, the spec is also not usable
               --  for stubbing.

               Set_Source_Status (Source_Name.all, Bad_Content);
            end if;
         else
            Successful_Initialization := Initialize_Context (Source_Name.all);
         end if;

         if Successful_Initialization then

            if not Main_Units.Is_Empty then
               declare
                  Success  : Boolean;
                  ALI_File : constant String :=
                    Temp_Dir.all & Directory_Separator
                    & Get_Source_Suffixless_Name (Source_Name.all) & ".ali";
                  Subdir   : constant String :=
                    Temp_Dir.all & Directory_Separator & Closure_Subdir_Name;
               begin

                  --  If gnattest is invoked with -U unit.adb and there is
                  --  a correspodning unit.ads, then the unit.ali will already
                  --  be present in closure subdir. No harm ovewriting it,
                  --  we will still get all other errors.
                  Copy_File
                    (ALI_File,
                     Subdir,
                     Success,
                     Overwrite);
                  if not Success then
                     Trace (Me, "Cannot copy " & ALI_File & " to " & Subdir);
                     Report_Err ("cannot calculate closure");
                     raise Fatal_Error;
                  end if;
               end;
               Update_Closure;
            end if;

            if Stub_Mode_ON or else not Main_Units.Is_Empty then

               if Get_Source_Body (Source_Name.all) = "" then
                  The_Unit := Main_Unit_In_Current_Tree (The_Context);
               else
                  The_Unit :=
                    Corresponding_Declaration
                      (Main_Unit_In_Current_Tree (The_Context));
               end if;

               --  processing main unit
               Process_Source (The_Unit);

               --  Iterate_Sources won't work in stub mode since we need
               --  bodies corresponding to argument specs (if they exist),
               --  thus we need to recreate the tree almost each time and
               --  little to none optimisation can be gained with
               --  Iterate_Sources.

            else
               The_Unit := Main_Unit_In_Current_Tree (The_Context);

               --  processing main unit
               Process_Source (The_Unit);

               --  processing others in same context
               Iterate_Sources
                 (Asis.Compilation_Units.Compilation_Units (The_Context));
            end if;

         end if;

         Source_Clean_Up;
         Context_Clean_Up;
         Free (Source_Name);
      end loop;

      Asis.Implementation.Finalize;

      Generate_Project_File;
      Generate_Common_File;

      declare
         Cur_Stor  : Generic_Package_Storage.Cursor :=
           Gen_Package_Storage.First;
         GP : Generic_Package;
      begin
         while Cur_Stor /= Generic_Package_Storage.No_Element loop
            GP := Generic_Package_Storage.Element (Cur_Stor);
            if not GP.Has_Instantiation then
               Report_Std
                 (GP.Sloc.all
                  & ": warning: no instance of "
                  & GP.Name.all);
               Report_Std
                 (" corresponding tests are not included into harness");
            end if;

            Next (Cur_Stor);
         end loop;
      end;

      if not Main_Units.Is_Empty then
         --  Report exclusions that were not found dyring on-the-fly closure
         --  computation.
         Report_Exclusions_Not_Found;
      end if;

      if Verbose then
         Cur := Test_Info.First;
         loop
            exit when Cur = Tests_Per_Unit.No_Element;

            Report_Std
              (Natural'Image (Tests_Per_Unit.Element (Cur)) &
               " testable subprograms in " &
               Base_Name (Tests_Per_Unit.Key (Cur)));

            Tests_Per_Unit.Next (Cur);
         end loop;

         Test_Info.Clear;
         Report_Std
           ("gnattest:" &
            Natural'Image (All_Tests_Counter) &
            " testable subprogram(s) processed");
         Report_Std
           ("gnattest:" &
            Natural'Image (New_Tests_Counter) &
            " new skeleton(s) generated");
      end if;

      if Stub_Mode_ON then
         GNATtest.Harness.Generator.Generate_Stub_Test_Driver_Projects;
      elsif Separate_Drivers then
         GNATtest.Harness.Generator.Generate_Test_Driver_Projects;
      else
         GNATtest.Harness.Generator.Test_Runner_Generator;
         GNATtest.Harness.Generator.Project_Creator;
      end if;

   end Process_Sources;

   -------------------
   -- Process_Stubs --
   -------------------

   procedure Process_Stubs (List : Asis_Element_List.List)
   is
      Cur : Asis_Element_List.Cursor;
      CU  : Compilation_Unit;
      Str : String_Access;
   begin
      if Is_Empty (List) then
         return;
      end if;

      --  Once we change the context, contents of List won't make sense.
      Cur := List.First;
      while Cur /= Asis_Element_List.No_Element loop

         CU := Enclosing_Compilation_Unit (Asis_Element_List.Element (Cur));

         Str := new String'(To_String (Text_Name (CU)));

         if Get_Source_Body (Str.all) /= "" then
            if not Source_Stubbed (Str.all) then
               GNATtest.Stub.Generator.Process_Unit
                 (CU,
                  Get_Source_Stub_Dir (Str.all)
                  & Directory_Separator
                  & Base_Name (Get_Source_Body (Str.all)),
                  Get_Source_Stub_Dir (Str.all)
                  & Directory_Separator
                  & Get_Source_Stub_Data_Spec (Str.all),
                  Get_Source_Stub_Dir (Str.all)
                  & Directory_Separator
                  & Get_Source_Stub_Data_Body (Str.all));
               Mark_Sourse_Stubbed (Str.all);
            end if;
         end if;

         Free (Str);

         Next (Cur);
      end loop;

   end Process_Stubs;

   ---------------------------------
   -- Put_Closing_Comment_Section --
   ---------------------------------

   procedure Put_Closing_Comment_Section
     (Subp           : Subp_Info;
      Overloading_N  : Natural;
      Commented_Out  : Boolean := False;
      Use_Short_Name : Boolean := True)
   is
      Overloading_Prefix : String_Access;
   begin

      if Overloading_N /= 0 then
         if Subp.Is_Overloaded then
            if Use_Short_Name then
               Overloading_Prefix := new String'("1_");
            else
               Overloading_Prefix := new String'
                 (Trim (Natural'Image (Overloading_N), Both) & "_");
            end if;
         else
            Overloading_Prefix := new String'("");
         end if;
      end if;

      S_Put (0, "--  begin read only");
      New_Line_Count;

      if Commented_Out then
         S_Put
           (3,
            "--  end "
            & Test_Routine_Prefix
            & Subp.Subp_Text_Name.all
            & (if Subp.Has_TC_Info
              then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
              else "")
            & ";");
      else
         S_Put
           (3,
            "end "
            & Test_Routine_Prefix
            & Overloading_Prefix.all
            & Subp.Subp_Text_Name.all
            & (if Subp.Has_TC_Info
              then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
              else "")
            & ";");
      end if;
      New_Line_Count;
      S_Put (0, "--  end read only");
      New_Line_Count;

   end Put_Closing_Comment_Section;

   ---------------------------------
   -- Put_Opening_Comment_Section --
   ---------------------------------

   procedure Put_Opening_Comment_Section
     (Subp           : Subp_Info;
      Overloading_N  : Natural;
      Commented_Out  : Boolean := False;
      Use_Short_Name : Boolean := True;
      Type_Name      : String  := "")
   is
      Hash_Length_Used : constant := 15;
      Hash_First : constant Integer := Subp.Subp_Full_Hash'First;
      Hash_Last  : constant Integer :=
        Subp.Subp_Full_Hash'First + Hash_Length_Used;

      Overloading_Prefix : String_Access;
   begin

      if Overloading_N /= 0 then
         if Subp.Is_Overloaded then
            if Use_Short_Name then
               Overloading_Prefix := new String'("1_");
            else
               Overloading_Prefix := new String'
                 (Trim (Natural'Image (Overloading_N), Both) & "_");
            end if;
         else
            Overloading_Prefix := new String'("");
         end if;
      end if;

      New_Line_Count;
      S_Put (0, "--  begin read only");
      New_Line_Count;

      if Subp.Corresp_Type = 0 then
         if Commented_Out then
            S_Put
              (3,
               "--  procedure "
               & Test_Routine_Prefix
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               &  " (Gnattest_T : in out Test);");
            New_Line_Count;
            S_Put
              (3,
               "--  procedure "
               & Subp.Subp_Mangle_Name.all
               &  " (Gnattest_T : in out Test) renames "
               & Test_Routine_Prefix
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               & ";");
            New_Line_Count;
         else
            S_Put
              (3,
               "procedure "
               & Test_Routine_Prefix
               & Overloading_Prefix.all
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               &  " (Gnattest_T : in out Test);");
            New_Line_Count;
            S_Put
              (3,
               "procedure "
               & Subp.Subp_Mangle_Name.all
               &  " (Gnattest_T : in out Test) renames "
               & Test_Routine_Prefix
               & Overloading_Prefix.all
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               & ";");
            New_Line_Count;
         end if;
      else
         if Commented_Out then
            S_Put
              (3,
               "--  procedure "
               & Test_Routine_Prefix
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               &  " (Gnattest_T : in out Test_"
               & Type_Name
               & ");");
            New_Line_Count;
            S_Put
              (3,
               "--  procedure "
               & Subp.Subp_Mangle_Name.all
               &  " (Gnattest_T : in out Test_"
               & Type_Name
               & ") renames "
               & Test_Routine_Prefix
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               & ";");
            New_Line_Count;
         else
            S_Put
              (3,
               "procedure "
               & Test_Routine_Prefix
               & Overloading_Prefix.all
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               &  " (Gnattest_T : in out Test_"
               & Type_Name
               & ");");
            New_Line_Count;
            S_Put
              (3,
               "procedure "
               & Subp.Subp_Mangle_Name.all
               &  " (Gnattest_T : in out Test_"
               & Type_Name
               & ") renames "
               & Test_Routine_Prefix
               & Overloading_Prefix.all
               & Subp.Subp_Text_Name.all
               & (if Subp.Has_TC_Info
                 then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
                 else "")
               & ";");
            New_Line_Count;
         end if;
      end if;

      S_Put
        (0,
         "--  id:"
         & Hash_Version
         & "/"
         & Subp.Subp_Full_Hash (Hash_First .. Hash_Last)
         & "/"
         & Subp.Subp_Text_Name.all
         & "/"
         & (if Use_Short_Name then "1" else "0")
         & "/"
         & (if Commented_Out then "1" else "0")
         & "/");
      if Subp.Has_TC_Info then
         S_Put
           (0,
            Sanitize_TC_Name (Subp.TC_Info.Name.all)
            & "/");
      end if;
      New_Line_Count;

      if Commented_Out then
         S_Put
           (3,
            "--  procedure "
            & Test_Routine_Prefix
            & Subp.Subp_Text_Name.all
            & (if Subp.Has_TC_Info
              then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
              else "")
            & " (Gnattest_T : in out ");
      else
         S_Put
           (3,
            "procedure "
            & Test_Routine_Prefix
            & Overloading_Prefix.all
            & Subp.Subp_Text_Name.all
            & (if Subp.Has_TC_Info
              then "_" & Sanitize_TC_Name (Subp.TC_Info.Name.all)
              else "")
            & " (Gnattest_T : in out ");
      end if;
      if Subp.Corresp_Type = 0 then
         S_Put (0, "Test) is");
      else
         S_Put
           (0,
            "Test_"
            & Type_Name
            & ") is");
      end if;

      New_Line_Count;

      if not Commented_Out then

         --  we cannot relate to any sloc in case of a dangling test

         if not Omit_Sloc then
            S_Put
              (3,
               "--  "
               & Base_Name
                 (To_String
                      (Text_Name
                           (Enclosing_Compilation_Unit
                              (Subp.Subp_Declaration))))
               & ":"
               & Trim
                 (Integer'Image
                      (First_Line_Number (Subp.Subp_Declaration)), Both)
               & ":"
               & Trim
                 (Integer'Image (First_Column_Number (Subp.Subp_Declaration)),
                  Both)
               & ":"
               & Subp.Subp_Name_Image.all);
            New_Line_Count;
         end if;

         if Subp.Has_TC_Info then
            Put_Wrapper_Rename (6, Subp);
         end if;
      end if;

      S_Put (0, "--  end read only");
      New_Line_Count;

   end Put_Opening_Comment_Section;

   ------------------------
   -- Put_Wrapper_Rename --
   ------------------------

   procedure Put_Wrapper_Rename (Span : Natural; Current_Subp : Subp_Info) is
   begin

      case Declaration_Kind (Current_Subp.Subp_Declaration) is
         when A_Function_Declaration             |
              An_Expression_Function_Declaration =>
            S_Put
              (Span,
               "function " &
                 Current_Subp.Subp_Name_Image.all);

            declare
               Params : constant
                 Asis.Parameter_Specification_List :=
                   Parameter_Profile
                     (Current_Subp.Subp_Declaration);

               Result : constant Asis.Element :=
                 Result_Profile (Current_Subp.Subp_Declaration);

               Result_Image : constant String :=
                 Trim (To_String (Element_Image (Result)),
                       Both);
            begin

               if Params'Length /= 0 then
                  S_Put (1, "(");
                  for I in Params'Range loop
                     S_Put
                       (0,
                        Trim
                          (To_String
                             (Element_Image (Params (I))),
                           Both));
                     if I = Params'Last then
                        S_Put (0, ")");
                     else
                        S_Put (0, "; ");
                     end if;
                  end loop;
               end if;

               S_Put (1, "return " & Result_Image);
            end;

         when A_Procedure_Declaration =>
            S_Put
              (3,
               "procedure " &
                 Current_Subp.Subp_Name_Image.all);

            declare
               Params : constant
                 Asis.Parameter_Specification_List :=
                   Parameter_Profile
                     (Current_Subp.Subp_Declaration);
            begin

               if Params'Length /= 0 then
                  S_Put (1, "(");
                  for I in Params'Range loop
                     S_Put
                       (0,
                        Trim
                          (To_String
                             (Element_Image (Params (I))),
                           Both));
                     if I = Params'Last then
                        S_Put (0, ")");
                     else
                        S_Put (0, "; ");
                     end if;
                  end loop;
               end if;
            end;

         when others => null;

      end case;

      S_Put
        (1,
         "renames "                        &
           Wrapper_Prefix                    &
           Current_Subp.Subp_Mangle_Name.all &
           ";");
      New_Line_Count;
   end Put_Wrapper_Rename;

   ----------------------
   -- Sanitize_TC_Name --
   ----------------------

   function Sanitize_TC_Name (TC_Name : String) return String
   is
      Name : String := Trim (TC_Name, Both);

      Tmp  : String_Access := new String'("");
      Buff : String_Access;

      Underscore : Boolean := True;
   begin

      for I in Name'Range loop

         if Name (I) = ' ' then
            Name (I) := '_';
         end if;

      end loop;

      for I in Name'Range loop

         if Underscore then
            if Name (I) /= '_' then
               Underscore := False;
               if Is_Letter (Name (I)) or else Is_Digit (Name (I)) then
                  Buff := new String'(Tmp.all & Name (I));
                  Free (Tmp);
                  Tmp := Buff;
                  Buff := null;
               end if;
            end if;
         else
            if
              Is_Letter (Name (I))
              or else Is_Digit (Name (I))
              or else Name (I) = '_'
            then
               Buff := new String'(Tmp.all & Name (I));
               Free (Tmp);
               Tmp := Buff;
               Buff := null;
               if Name (I) = '_' then
                  Underscore := True;
               end if;
            end if;
         end if;

      end loop;

      return To_Lower (Tmp.all);
   end Sanitize_TC_Name;

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

      if Main_Units.Is_Empty then
         --  We need to keep ALI files for incremental closure recomputation.
         Delete_File (Last_Context_Name.all & ".ali", Success);
         if not Success then
            Report_Std ("gnattest: cannot delete " &
                          Last_Context_Name.all & ".ali");
         end if;
      end if;

      Free (Last_Context_Name);
   end Source_Clean_Up;

   ------------
   -- Add_DT --
   ------------

   procedure Add_DT
     (TP_List : in out TP_Mapping_List.List;
      TPtarg  : String;
      Test_F  : String;
      Line    : Natural;
      Column  : Natural)
   is
      TP : TP_Mapping;
      TD : DT_Mapping;

      TP_Cur : TP_Mapping_List.Cursor := TP_List.First;
   begin

      TD.File := new String'(Test_F);
      TD.Line := Line;
      TD.Column := Column;

      loop
         exit when TP_Cur = TP_Mapping_List.No_Element;

         if TP_Mapping_List.Element (TP_Cur).TP_Name.all = TPtarg then
            exit;
         end if;

         TP_Mapping_List.Next (TP_Cur);
      end loop;

      TP := TP_Mapping_List.Element (TP_Cur);
      TP.DT_List.Append (TD);
      TP_List.Replace_Element (TP_Cur, TP);

   end Add_DT;

   ------------
   -- Add_TR --
   ------------

   procedure Add_TR
     (TP_List : in out TP_Mapping_List.List;
      TPtarg  : String;
      Test_F  : String;
      Test_T  : String;
      Subp    : Subp_Info;
      TR_Line : Natural := 1)
   is
      TC : TC_Mapping;
      TR : TR_Mapping;
      TP : TP_Mapping;

      TR_Cur : TR_Mapping_List.Cursor;
      TP_Cur : TP_Mapping_List.Cursor := TP_List.First;

      Subp_Span : constant Asis.Text.Span :=
        Element_Span (First_Name (Subp.Subp_Declaration));
      TC_Span   : constant Asis.Text.Span :=
        Element_Span (Subp.TC_Info.Elem);
   begin

      loop
         exit when TP_Cur = TP_Mapping_List.No_Element;

         if TP_Mapping_List.Element (TP_Cur).TP_Name.all = TPtarg then
            exit;
         end if;

         TP_Mapping_List.Next (TP_Cur);
      end loop;

      if TP_Cur = TP_Mapping_List.No_Element then
         TP.TP_Name := new String'(TPtarg);
         TR.TR_Name := new String'(Subp.Subp_Text_Name.all);
         TR.Line := Subp_Span.First_Line;
         TR.Column := Subp_Span.First_Column;
         if Subp.Has_TC_Info then
            TC.T_Name := new String'(Subp.Subp_Mangle_Name.all);
            TC.TC_Name := new String'(Subp.TC_Info.Name.all);
            TC.Line := TC_Span.First_Line;
            TC.Column := TC_Span.First_Column;
            TC.Test := new String'(Test_F);
            TC.Test_Time := new String'(Test_T);
            TC.TR_Line := TR_Line;
            TR.TC_List.Append (TC);
         else
            TR.Test := new String'(Test_F);
            TR.Test_Time := new String'(Test_T);
            TR.TR_Line := TR_Line;
            TR.T_Name := new String'(Subp.Subp_Mangle_Name.all);
         end if;

         TP.TR_List.Append (TR);
         TP_List.Append (TP);

         return;
      end if;

      TP := TP_Mapping_List.Element (TP_Cur);

      TR_Cur := TP.TR_List.First;
      loop
         exit when TR_Cur = TR_Mapping_List.No_Element;

         if
           TR_Mapping_List.Element (TR_Cur).Line = Subp_Span.First_Line and
           TR_Mapping_List.Element (TR_Cur).Column = Subp_Span.First_Column
         then
            exit;
         end if;

         TR_Mapping_List.Next (TR_Cur);
      end loop;

      if TR_Cur = TR_Mapping_List.No_Element then

         TR.TR_Name := new String'(Subp.Subp_Text_Name.all);
         TR.Line := Subp_Span.First_Line;
         TR.Column := Subp_Span.First_Column;
         if Subp.Has_TC_Info then
            TC.T_Name := new String'(Subp.Subp_Mangle_Name.all);
            TC.TC_Name := new String'(Subp.TC_Info.Name.all);
            TC.Line := TC_Span.First_Line;
            TC.Column := TC_Span.First_Column;
            TC.Test := new String'(Test_F);
            TC.Test_Time := new String'(Test_T);
            TC.TR_Line := TR_Line;
            TR.TC_List.Append (TC);
         else
            TR.Test := new String'(Test_F);
            TR.Test_Time := new String'(Test_T);
            TR.TR_Line := TR_Line;
            TR.T_Name := new String'(Subp.Subp_Mangle_Name.all);
         end if;

         TP.TR_List.Append (TR);
         TP_List.Replace_Element (TP_Cur, TP);

         return;
      end if;

      TR := TR_Mapping_List.Element (TR_Cur);

      --  The only way that there is same subprogram already is when it has
      --  test_cases. So no need to check if it has TC_Info.
      TC.T_Name := new String'(Subp.Subp_Mangle_Name.all);
      TC.TC_Name := new String'(Subp.TC_Info.Name.all);
      TC.Line := TC_Span.First_Line;
      TC.Column := TC_Span.First_Column;
      TC.Test := new String'(Test_F);
      TC.Test_Time := new String'(Test_T);
      TC.TR_Line := TR_Line;
      TR.TC_List.Append (TC);

      TP.TR_List.Replace_Element (TR_Cur, TR);
      TP_List.Replace_Element (TP_Cur, TP);

   end Add_TR;

   -----------------------
   -- Test_Types_Linked --
   -----------------------

   function Test_Types_Linked
     (Inheritance_Root_Type : Asis.Element;
      Inheritance_Final_Type : Asis.Element)
      return Boolean
   is
      Elem  : Asis.Element := Inheritance_Final_Type;
      Elem2 : Asis.Element := Inheritance_Root_Type;
   begin
      if Declaration_Kind (Inheritance_Root_Type) in
        A_Private_Type_Declaration | A_Private_Extension_Declaration
      then
         Elem2 := Corresponding_Type_Completion (Elem2);
      end if;

      loop
         if Is_Fully_Private (Elem) then
            return False;
         end if;

         exit when Is_Equal (Elem, Elem2);
         Elem := Parent_Type_Declaration (Elem);
      end loop;
      return True;
   end Test_Types_Linked;

   --------------------
   -- Uncomment_Line --
   --------------------

   function Uncomment_Line (S : String) return String is
   begin
      if S = "--  " then
         return "";
      end if;

      if S'Length < 5 then
         return S;
      end if;

      if S (S'First .. S'First + 3) = "--  " then
         return S (S'First + 4 .. S'Last);
      end if;

      return S;
   end Uncomment_Line;

end GNATtest.Skeleton.Generator;
