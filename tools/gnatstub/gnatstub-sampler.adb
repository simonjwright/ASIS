------------------------------------------------------------------------------
--                                                                          --
--                           GNATSTUB COMPONENTS                            --
--                                                                          --
--                      G N A T S T U B . S A M P L E R                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1997-2017, Free Software Foundation, Inc.        --
--                                                                          --
-- Gnatstub  is  free  software;  you can  redistribute it and/or modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 3 or (at your option) any later --
-- version. Gnatstub is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING3. If not, --
-- go to http://www.gnu.org/licenses for a complete copy of the license.    --
--                                                                          --
-- Gnatstub  was  originally  developed  by  Alexei  Kuchumov  as a part of --
-- collaboration  between  Software  Engineering  Laboratory  of  the Swiss --
-- Federal  Institute  of  Technology  in  Lausanne,  Switzerland, and  the --
-- Scientific  Research  Computer  Center  of the  Moscow State University, --
-- Russia.  This  work  was  supported  by  a grant from the Swiss National --
-- Science Foundation,  no 7SUPJ048247,  funding a project  "Development of --
-- ASIS for GNAT with industry quality".                                    --
--                                                                          --
-- Gnatstub is now maintained by AdaCore (http://www.adacore.com).          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with GNAT.Directory_Operations;
with Ada.IO_Exceptions;
with Ada.Strings;                  use Ada.Strings;
with Ada.Strings.Wide_Fixed;       use Ada.Strings.Wide_Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Ada.Wide_Text_IO;             use Ada.Wide_Text_IO;

with GNAT.OS_Lib;                  use GNAT.OS_Lib;

with GNATCOLL.Projects;            use GNATCOLL.Projects;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;

with Asis.Ada_Environments;        use Asis.Ada_Environments;
with Asis.Compilation_Units;       use Asis.Compilation_Units;
with Asis.Declarations;            use Asis.Declarations;
with Asis.Elements;                use Asis.Elements;
with Asis.Errors;
with Asis.Exceptions;              use Asis.Exceptions;
with Asis.Extensions;              use Asis.Extensions;
with Asis.Extensions.Flat_Kinds;   use Asis.Extensions.Flat_Kinds;
with Asis.Implementation;          use Asis.Implementation;
with Asis.Iterator;                use Asis.Iterator;
with Asis.Text;                    use Asis.Text;

with Hostparm;
with Opt;                          use all type Opt.Ada_Version_Type;

with ASIS_UL.Common;               use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;     use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;                use ASIS_UL.Debug;
with ASIS_UL.Environment;
with ASIS_UL.Misc;                 use ASIS_UL.Misc;
with ASIS_UL.Options;              use ASIS_UL.Options;
with ASIS_UL.Output;               use ASIS_UL.Output;
with ASIS_UL.String_Utilities;     use ASIS_UL.String_Utilities;
with ASIS_UL.Utilities;            use ASIS_UL.Utilities;

with Gnatstub.Options;             use Gnatstub.Options;
with Gnatstub.Projects;

package body Gnatstub.Sampler is

   use type Asis.Errors.Error_Kinds;

   My_Context : Asis.Context;

   Level       : Integer := 0;
   --  nesting level of a spec being processed

   Body_File   : File_Type;
   Tree_File   : File_Type;
   Spec_File   : File_Type;

   Arg_File_Info : File_Info;
   Arg_Virt_File : Virtual_File;

   --  Needed when creating separate bodies:
   Subunit_Name_Temlate : String_Access;
   --  Used to create the name of a separate body to generate. Has the
   --  following structure:
   --
   --     <parent_body_name><dot_replacement><suffix>
   --
   --  where <suffix> is the suffix defined for separate bodies by the argument
   --  project file (if any) or just ".adb"

   First_Idx  : Natural;
   Last_Idx   : Natural;
   Insert_Idx : Natural;
   --  Indexes into Subunit_Name_Temlate. Insert_Idx points to the place where
   --  the separate body name should be inserted to get the subunit name -
   --  to the last character of <dot_replacement>

   type Subunit_Name_Casings is (Lowercase, Uppercase, Mixedcase);
   Subunit_Name_Casing : Subunit_Name_Casings := Lowercase;

   ------------------------------------
   -- The note about pretty-printing --
   ------------------------------------

   --  We do not put any special efforts in getting a very nice formatting of
   --  the sample body code generated by gnatstub. This code is supposed to be
   --  revised manually by a gnatstub user, so only some very general elements
   --  of pretty-printing are implemented in gnatstub

   -----------------------
   -- Local subprograms --
   -----------------------

   function Get_Tree_Name return String;
   --  Computes the name of the tree file from Short_File_Name.

   procedure Check_Parameters;
   --  Checks, that Gnatstub options and files existing in the file
   --  system fit each other. If the check fails, generates the diagnostic
   --  message and raises Parameter_Error

   procedure Prepare_Context;
   --  If in NON-GNSA mode, this procedure creates a tree file or checks if the
   --  tree file already exists, depending on options. Then it defines and
   --  opens the ASIS Context.
   --  In GNSA mode, it defines and opens the ASIS GNSA Context

   type Element_Node;
   type Link is access all Element_Node;

   type Element_Node is record
      Spec      : Asis.Element := Nil_Element;
      Spec_Name : Program_Text_Access;
      --  not used for incomplete type declarations
      Up        : Link;
      Down      : Link;
      Prev      : Link;
      Next      : Link;
      Last      : Link;
   end record;
   --  An element of a dynamic structure representing a "skeleton" of the body
   --  to be generated
   --
   --  Logically this structure is a list of elements representing local
   --  bodies and sublists representing the bodies which are a components of
   --  some local body. Each list and sublist is represented by its first
   --  element. For this first list element, the field Last is used to point
   --  to the last element in this list to speed up adding the new element if
   --  we do not have to order alphabetically the local bodies.

   Body_Structure        : aliased Element_Node;

   Body_Structure_Access : constant Link := Body_Structure'Access;
   --  this is a "design" for a body to generate. It contains references
   --  to the elements from the argument spec for which body samples should
   --  be generated, ordered alphabetically. The top of this link structure
   --  is the Element representing a unit declaration from the argument
   --  compilation unit.

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Element_Node, Name => Link);

   procedure Free_Body_Structure;
   --  Releases all the memory occupied by the internal representation of the
   --  body stub to generate. Is needed when separate units are generated, in
   --  this case the internal representation is created for each separate body.

   -------------------------------------------------
   --  Actuals for Traverse_Element instantiation --
   -------------------------------------------------

   type Body_State is record
      Argument_Spec : Boolean := True;
      --  flag indicating if we are in the very beginning (very top)
      --  of scanning the argument library unit declaration
      Current_List : Link;
      --  declaration list in which a currently processed spec
      --  should be inserted;
      Last_Top : Link;
      --  an element which represents a declaration from which the currently
      --  processed sublist was originated
      New_List_Needed : Boolean := False;
      --  flag indication if a new sublist should be created
   end record;

   Initial_State : constant Body_State :=
     (Argument_Spec   => True,
      Current_List    => null,
      Last_Top        => null,
      New_List_Needed => False);

   procedure Create_Element_Node
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Body_State);
   --  when visiting an Element representing something for which a body
   --  sample may be required, we check if the body is really required
   --  and insert the corresponding Element on the right place in Body_State
   --  if it is.

   procedure Go_Up
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Body_State);
   --  when leaving a [generic] package declaration or a protected [type]
   --  declaration, we have to go one step up in Body_State structure.

   procedure Create_Body_Structure is new Traverse_Element
     (State_Information => Body_State,
      Pre_Operation     => Create_Element_Node,
      Post_Operation    => Go_Up);
   --  Creates Body_Structure by traversing an argument spec and choosing
   --  specs to create body samples for

   function Requires_Body (El : Element) return Boolean;
   --  checks if a body sample should be created for an element

   function Name (El : Asis.Element) return Program_Text;
   --  returns a defining name string image for a declaration which
   --  defines exactly one name. This should definitely be made an extension
   --  query

   function Bodyless_Package (Node : Link) return Boolean;
   --  Checks if Node represents a local package which does not require
   --  a body. (It is an error to call this function for a null
   --  argument

   procedure Generate_CU_Header
     (Success      : out Boolean;
      For_Subunit  : Boolean      := False;
      Subunit_Name : Asis.Element := Nil_Element);
   --  Generates in Body_File the comment header for the sample body. Sets
   --  Success to True if the comment header is successfully generated.
   --  If For_Subunit is ON, Subunit_Name should be set to the defining name
   --  of the separate unit. In this case the standard header contains the
   --  name of the separate unit, but not the argument unit. If
   --  Gnatstub.Options.Header is set to From_Spec, for subunit it does NOT
   --  contain the subunit name, the only change is replacing "S p e c' with
   --  'B o d y'
   --  ??? Do we really need the Success parameter???

   procedure Generate_Unit_Header (Node : Link);
   --  Generates the comment header for a local program unit body. Does nothing
   --  if No_Local_Comment_Headers is ON.

   procedure Generate_Body_Structure;
   --  generates in Body_File the Ada part of the sample body, using
   --  the list structure created in Body_Structure as a template

   --  The following group of subprograms generate completion for specific
   --  kinds of specs:

   procedure Generate_Package_Body (Node : Link);

   procedure Generate_Function_Body (Node : Link);

   procedure Generate_Procedure_Body (Node : Link);

   procedure Generate_Entry_Body (Node : Link);

   procedure Generate_Protected_Body (Node : Link);

   procedure Generate_Task_Body (Node : Link);

   procedure Generate_Full_Type_Declaration (Node : Link);

   procedure Generate_Separate_Stubs (Body_Unit : Asis.Compilation_Unit);
   --  If Body_Unit is a library unit body, this procedure generates stub
   --  subunits for the body stubs contained in this unit

   procedure Generate_Profile (Node : Link; Change_Line : out Boolean);
   --  Generates an entry_body_formal_part, parameter or parameter and result
   --  profile for the body of a program unit represented by Node. Upon exit,
   --  sets Change_Line is set True  if the following "is" for the body should
   --  be generated on a new line;

   function Image_For_String (S : Program_Text) return Program_Text;
   --  If S starts and ends with '"', return '"' & S & '"", otherwise return S.
   --  This is needed to create an image of an operator symbol in generated
   --  string constants.

   procedure Emergency_Clean_Up;
   --  Does clean up actions in case if an exception was raised during
   --  creating a body sample (closes a Context, dissociates it, finalizes
   --  ASIS, closes and deletes needed files.

   function Allows_Var_Declaration
     (Res_Profile : Asis.Element)
      return        Boolean;
   --  This function assumes that Res_Profile represents the result profile
   --  from a function specification, it checks if this profile denotes a type
   --  that allow creating a local object of this type (to return as the
   --  function result in the function body stub).

   ----------------------------
   -- Allows_Var_Declaration --
   ----------------------------

   function Allows_Var_Declaration
     (Res_Profile : Asis.Element)
      return        Boolean
   is
      Tmp    : Asis.Element;
      Result : Boolean := False;
   begin

      if Element_Kind (Res_Profile) = An_Expression then
         --  Definitely False for functions returning anonymous access
         --  definitions

         if Attribute_Kind (Res_Profile) = A_Base_Attribute then
            Result := True;
         else
            Tmp    := Normalize_Reference (Res_Profile);
            Result := Can_Create_Return_Object (Tmp);
         end if;

      end if;

      return Result;
   end Allows_Var_Declaration;

   ----------------------
   -- Bodyless_Package --
   ----------------------

   function Bodyless_Package (Node : Link) return Boolean is
      Result    : Boolean                     := False;
      Arg_Kind  : constant Flat_Element_Kinds := Flat_Element_Kind (Node.Spec);
      Next_Node : Link;
      Next_List : Link;
   begin
      if Arg_Kind = A_Package_Declaration or else
         Arg_Kind = A_Generic_Package_Declaration
      then
         Result := True;

         if Node.Down /= null then

            Next_List := Node.Down;

            while Next_List.Prev /= null loop
               Next_List := Next_List.Prev;
            end loop;

            Next_Node := Next_List;

            while Next_Node /= null loop

               if not Bodyless_Package (Next_Node) then
                  Result := False;
                  exit;
               end if;

               Next_Node := Next_Node.Next;
            end loop;

         end if;

      end if;

      return Result;

   end Bodyless_Package;

   ----------------
   -- Brief_Help --
   ----------------

   procedure Brief_Help is
   begin
      pragma Style_Checks (Off);

      Info ("Usage: gnatstub [options] filename [-cargs gcc_switches]");
      Info ("");
      Info ("  filename               Ada source file");
      Info ("");
      Info ("options:");
      Info ("  --version              Display version and exit");
      Info ("  --help                 Display usage and exit");
      Info ("");
      Info ("  -Pproject              Use project file project. Only one such switch.");
      Info ("                         can be used.");
      Info ("  -Xname=value           specify an external reference for argument");
      Info ("                         project file");
      Info ("  -eL                    follow all symbolic links when processing");
      Info ("                         project files");
      Info ("");
      Info (" --subunits              generate separate bodies for body stubs");
      Info ("");
      Info ("  -f                     replace an existing body file (if any), with a body");
      Info ("                         sample (is not allowed with '--subunits')");

      Info ("  -gnatec<path>          use additional configuration file,");
      Info ("                         same meaning as for gcc");

      Info ("  -gnatyMnnn             maximum line length in a sample body");

      Info ("  -gnatyn                (n in 1 .. 9) number of spaces used for indentation in");
      Info ("                         a sample body");

      Info ("  -gnatyo                alphabetically order local bodies");

      Info ("  -hg                    insert a sample comment header");
      Info ("  -hs                    insert the comment header from the spec");

      Info ("  --header-file=filename insert the comment header from the specified file");

      Info ("  -Idir                  source search dir, has the same meaning as for");
      Info ("                         gcc and gnatmake");
      Info ("  -I-                    do not look for the sources in the default directory");

      Info ("  -in                    same as -gnatyn");
      Info ("  -k                     do not remove the tree file");
      Info ("  -lnnn                  same as -gnatyMnnn");

      Info ("  --no-exception         avoid raising Program_Error in stubs");

      Info ("  --no-local-header      no local comment headers for unit stubs");

      Info ("  -o body-name           the name of the file to place the body into.");

      Info ("  --dir=directory        place generated file(s) into directory");

      Info ("  -W(h|u|s|e|8|b)        sets the wide character encoding of the result file");
      Info ("                          h - Hex ESC encoding");
      Info ("                          u - Upper half encoding");
      Info ("                          s - Shift-JIS encoding");
      Info ("                          e - EUC Encoding");
      Info ("                          8 - UTF-8 encoding");
      Info ("                          b - Brackets encoding (this is the default)");
      Info ("");

      Info ("  -q                     quiet mode");
      Info ("  -r                     reuse the tree file (if any) instead of creating it");
      Info ("                         (-r also implies -k)");

      Info ("  -t                     overwrite the existing tree file");

      Info ("  -v                     verbose mode");

      Info ("  gcc_switches           switches to be passed to gcc called by " & ASIS_UL.Common.Tool_Name.all);

      pragma Style_Checks (On);
   end Brief_Help;

   ----------------------
   -- Check_Parameters --
   ----------------------

   procedure Check_Parameters is
      Str_Tmp       : String_Access;
   begin

      --  First, check for incompatible combination of options:

      if Generate_Subunits then

         if Body_Form /= null then
            Error ("result body name cannot be specified " &
                   "when generating subunits");
            raise Parameter_Error;
         end if;

         if Overwrite_Body then
            Error ("overwrite mode cannot be specified " &
                   "when generating subunits");
            raise Parameter_Error;
         end if;
      end if;

      if Body_Form = null then
         Body_Form := new String'("");
      end if;

      --  checking if the file to process really exists:
      if Gnatstub.Projects.Is_Specified (Gnatstub.Options.Gnatstub_Prj) then

         Arg_Virt_File :=
           Gnatstub.Projects.Create
             (Gnatstub.Options.Gnatstub_Prj, +File_Name.all);

         if Arg_Virt_File = No_File then
            Error ("cannot find " & File_Name.all);
            raise Parameter_Error;
         end if;

         Arg_File_Info := Info (Gnatstub.Options.Gnatstub_Prj, Arg_Virt_File);

         Str_Tmp := new String'(+Full_Name (Arg_Virt_File));
         Free (File_Name);
         File_Name := new String'(Str_Tmp.all);
         Free (Str_Tmp);

      elsif not Is_Regular_File (File_Name.all) then
         Error ("cannot find " & File_Name.all);
         raise Parameter_Error;
      end if;

      if Test_Mode then
         Alphabetical_Ordering := True;
         Quiet_Mode            := True;
         Overwrite_Tree        := True;
      end if;

      --  Naming for the result file(s):

      File_Name_Len   := File_Name'Length;
      File_Name_First := File_Name'First;
      File_Name_Last  := File_Name'Last;

      if Generate_Subunits then

         if not Gnatstub.Projects.Is_Specified (Gnatstub.Options.Gnatstub_Prj)
           and then
            not (File_Name_Len  >= 5 and then
                    File_Name (File_Name_Last - 3 .. File_Name_Last) = ".adb")
         then
            --  If we do not have a project file, than we expect a standard
            --  naming scheme
            Error ("standard naming scheme is expected " &
                   "for generating subunits");
            raise Parameter_Error;
         end if;
      else

         if not (File_Name_Len  >= 5 and then
                 File_Name (File_Name_Last - 3 .. File_Name_Last) = ".ads")
           and then
            Body_Name = null
         then

            if Gnatstub.Projects.Is_Specified
              (Gnatstub.Options.Gnatstub_Prj)
            then
               --  Try to compute the name of the result body from the project
               Body_Name :=
                 new String'(+GNATCOLL.Projects.File_From_Unit
                   (Project         => Project (Arg_File_Info),
                    Unit_Name       => Unit_Name (Arg_File_Info),
                    Part            => Unit_Body,
                    Language        => "Ada",
                    File_Must_Exist => False));

            else

               if File_Name_Len  >= 5 and then
                  File_Name (File_Name_Last - 3 .. File_Name_Last) = ".adb"
               then
                  --  A special case: the argument file looks like a body file
                  Error ("input file looks like a body");
               end if;

               --  The general case
               Error ("output file name should be provided because " &
                      File_Name.all &
                      " does not follow GNAT naming rules for " &
                      "spec files");

               raise Parameter_Error;
            end if;
         end if;
      end if;

      --  if destination is set, check if the destination directory exists:
      if Destination_Dir.all /= "" and then
         not Is_Directory (Destination_Dir.all)
      then
         Error ("directory " & Destination_Dir.all & " does not exist");
         raise Parameter_Error;
      end if;

      --  If subunits should be generated, we will prepare the template for
      --  subunit file name
      if Generate_Subunits then

         if Gnatstub.Projects.Is_Specified (Gnatstub.Options.Gnatstub_Prj) then
            declare
               Arrt_Dot_Replacement : constant Attribute_Pkg_String :=
                  Build (Naming_Package, "Dot_Replacement");

               Arrt_Body_Suffix : constant Attribute_Pkg_String :=
                  Build (Naming_Package, "Body_Suffix");

               Arrt_Separate_Suffix : constant Attribute_Pkg_String :=
                  Build (Naming_Package, "Separate_Suffix");

               Arrt_Casing : constant Attribute_Pkg_String :=
                  Build (Naming_Package, "Casing");

               Dot_Rep : constant String :=
                  Attribute_Value
                    (Project      => Project (Arg_File_Info),
                     Attribute    => Arrt_Dot_Replacement,
                     Use_Extended => True);

               Body_Suf : constant String :=
                  Attribute_Value
                    (Project      => Project (Arg_File_Info),
                     Index        => "Ada",
                     Attribute    => Arrt_Body_Suffix,
                     Use_Extended => True);

               Sep_Suf : constant String :=
                  Attribute_Value
                    (Project      => Project (Arg_File_Info),
                     Attribute    => Arrt_Separate_Suffix,
                     Use_Extended => True);

               Casing_Str : constant String :=
                  Attribute_Value
                    (Project      => Project (Arg_File_Info),
                     Attribute    => Arrt_Casing,
                     Use_Extended => True);

            begin
               if Destination_Dir.all /= "" then
                  Str_Tmp :=
                    new String'
                      (Destination_Dir.all & Directory_Separator &
                       GNAT.Directory_Operations.Base_Name (File_Name.all));
               else
                  Str_Tmp := new String'(File_Name.all);
               end if;

               First_Idx  := Str_Tmp'First;

               Insert_Idx :=
                 Natural'Max (Index (Str_Tmp.all, Body_Suf, Backward),
                              Index (Str_Tmp.all, Sep_Suf, Backward));

               Subunit_Name_Temlate := new String'
                 (Str_Tmp (First_Idx .. Insert_Idx - 1) &
                  Dot_Rep & Sep_Suf);

               Insert_Idx := Insert_Idx + Dot_Rep'Length - 1;

               if Casing_Str = "lowercase" then
                  Subunit_Name_Casing := Lowercase;
               elsif Casing_Str = "uppercase" then
                  Subunit_Name_Casing := Uppercase;
               elsif Casing_Str = "mixedcase" then
                  Subunit_Name_Casing := Mixedcase;
               else
                  Error ("cannot detect subunit file name casing");
               end if;

               Free (Str_Tmp);
            end;

         else
            First_Idx  := File_Name'First;
            Insert_Idx := File_Name'Last - 4;
            Subunit_Name_Temlate :=
              new String'(File_Name (First_Idx .. Insert_Idx) & "-.adb");
            Insert_Idx := Insert_Idx + 1;
         end if;

         Last_Idx := Subunit_Name_Temlate'Last;
      end if;

         --  Compute the body file name if it is not set or check that it does
         --  not contain the directory info otherwise

      Short_File_Name :=
        new String'(GNAT.Directory_Operations.Base_Name (File_Name.all));
      Short_File_Name_Len   := Short_File_Name'Length;
      Short_File_Name_First := Short_File_Name'First;
      Short_File_Name_Last  := Short_File_Name'Last;

      if not Generate_Subunits then
         if Body_Name = null then

            if Destination_Dir.all = "" then
               Body_Name := new String'(Short_File_Name.all);
            else
               Body_Name := new String'
                               (Destination_Dir.all &
                                Directory_Separator &
                                Short_File_Name.all);
            end if;

            Body_Name (Body_Name'Last) := 'b';

         else

            if GNAT.Directory_Operations.Base_Name (Body_Name.all) /=
               Body_Name.all
            then
               Error
                 ("output file name should not contain any path information");
               raise Parameter_Error;
            end if;

            if Destination_Dir.all /= "" then
               Str_Tmp := new String'(Destination_Dir.all &
                                            Directory_Separator &
                                            Body_Name.all);
               Free (Body_Name);
               Body_Name := new String'(Str_Tmp.all);
               Free (Str_Tmp);
            end if;

         end if;

         Full_Body_Name :=
         --  new String'(Str_Tmp.all);
           new String'(GNAT.Directory_Operations.Format_Pathname
                         (Normalize_Pathname (Body_Name.all)));

         --  Normalizing the argument name

         Str_Tmp :=
           new String'(GNAT.Directory_Operations.Format_Pathname
                         (Normalize_Pathname (File_Name.all)));

         Full_File_Name := new String'(Str_Tmp.all);
         Free (Str_Tmp);

         --  Check if the source and the destination are not the same

         if Full_File_Name.all = Full_Body_Name.all then
            Error ("output file is the same as the argument file - " &
                   "check the file names");
            raise Parameter_Error;
         end if;

         --  checking if a body already exists:

         if Is_Regular_File (Body_Name.all) then

            if Overwrite_Body then

               if Is_Writable_File (Body_Name.all) then

                  begin
                     Open (Body_File, Out_File, Body_Name.all, Body_Form.all);
                     Delete (Body_File);
                  exception
                     when others =>
                        Error ("cannot delete the existing body for " &
                          File_Name.all);
                        raise Parameter_Error;
                  end;
               else
                  Error ("the existing body for " & File_Name.all &
                         " cannot be overwritten");
                  raise Parameter_Error;
               end if;
            else
               Error ("the body for " & File_Name.all & " already exists; " &
                      "use -f to overwrite it");
               raise Parameter_Error;
            end if;

         end if;

      end if;

      --  Check that the comment header file exists:

      if Header = From_File
       and then
        not Is_Regular_File (Header_File_Name.all)
      then
            Error ("the file " & Header_File_Name.all &
                   " to take the comment header from does not exists");
            raise Parameter_Error;
      end if;

      --  now, checking the situation with the tree file:
      Tree_Name := new String'(Get_Tree_Name);

      if Is_Regular_File (Tree_Name.all) then
         Tree_Exists := True;

         if not (Reuse_Tree or else Overwrite_Tree) then
            Error ("tree file " & Tree_Name.all & " already exists" &
                   " use -r or -t to reuse or to overwrite it");
            raise Parameter_Error;
         end if;

      else

         if Reuse_Tree then
            Error ("cannot find tree file" & Tree_Name.all & " (-r is set)");
            raise Parameter_Error;
         end if;

      end if;

      if Reuse_Tree then
         Delete_Tree := False;
         Overwrite_Tree := False;
      end if;

      Store_I_Options;

      Free (Destination_Dir);

      if Gnatstub.Options.Gnatstub_Prj.Is_Specified then
         Gnatstub.Projects.Set_Global_Result_Dirs
           (Gnatstub.Options.Gnatstub_Prj);
         Gnatstub.Projects.Set_Individual_Source_Options
           (Gnatstub.Options.Gnatstub_Prj);
         Set_Arg_List;
      end if;

      Set_Arg_List;

   end Check_Parameters;

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up is
   begin

      if Delete_Tree and then Tree_Exists then
         --  Deleting the tree file itself
         Open (Tree_File, In_File, Tree_Name.all);
         Delete (Tree_File);

         --  Deleting the ALI file which was created along with the tree file
         --  We use the modified Tree_Name for this, because we do not need
         --  Tree_Name any more
         Tree_Name (Tree_Name'Last - 2 .. Tree_Name'Last) := "ali";

         if Is_Regular_File (Tree_Name.all) then
            Open (Tree_File, In_File, Tree_Name.all);
            Delete (Tree_File);
         end if;

      end if;

      if Need_Tmp_ADA_PRJ_INCLUDE_FILE
        and then
         ADA_PRJ_INCLUDE_FILE_Full_Name /= null
      then
         Open (Tree_File, In_File, ADA_PRJ_INCLUDE_FILE_Full_Name.all);
         Delete (Tree_File);
      end if;

      ASIS_UL.Environment.Clean_Up;

   end Clean_Up;

   -------------------------
   -- Create_Element_Node --
   -------------------------

   procedure Create_Element_Node
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Body_State)
   is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);

      Current_Node : Link;

      procedure Insert_In_List
        (State    : in out Body_State;
         El       : Asis.Element;
         New_Node : out Link);
      --  inserts an argument Element in the current list, keeping the
      --  alphabetic ordering. Creates a new sublist if needed.
      --  New_Node returns the reference to the newly inserted node

      --------------------
      -- Insert_In_List --
      --------------------

      procedure Insert_In_List
        (State    : in out Body_State;
         El       :        Asis.Element;
         New_Node :    out Link)
      is
         Next_Node    : Link;
         Insert_After : Link;

         Insert_First : Boolean := False;
         Insert_Last  : Boolean := False;
      begin
         New_Node      := new Element_Node;
         New_Node.Spec := El;

         New_Node.Spec_Name := new Program_Text'(Name (El));

         if State.New_List_Needed then
            --  here we have to set up a new sub-list:
            State.Current_List    := New_Node;
            New_Node.Up           := State.Last_Top;
            State.Last_Top.Down   := New_Node;
            State.New_List_Needed := False;

            New_Node.Last := New_Node;
            --  We've just created a new list. It contains a single element
            --  which is its last Element, so we are setting the link to the
            --  last element to the Prev field of the list head element
         else
            --  here we have to insert New_Node in an existing list,
            --  keeping the alphabetical order of program unit names

            New_Node.Up := State.Current_List.Up;

            if Arg_Kind in An_Incomplete_Type_Declaration ..
                 A_Tagged_Incomplete_Type_Declaration
            then
               --  no need for alphabetical ordering, inserting in the
               --  very beginning:

               New_Node.Last := State.Current_List.Last;
               --  New_Node will be the head element of the list, so we have
               --  to copy into this new head element the reference to the
               --  last element of the list.

               New_Node.Next           := State.Current_List;
               State.Current_List.Prev := New_Node;
               State.Current_List      := New_Node;
            else

               if Alphabetical_Ordering then

                  Next_Node := State.Current_List;

                  --  finding the right place in the current list
                  loop

                     if Flat_Element_Kind (Next_Node.Spec) in
                       An_Incomplete_Type_Declaration  ..
                       A_Tagged_Incomplete_Type_Declaration
                     then

                        if Next_Node.Next = null then
                           --  nothing except incomplete types in the list:
                           Insert_After := Next_Node;
                           exit;
                        end if;

                     else
                        --  here we have a program unit spec
                        if To_Lower_Case (New_Node.Spec_Name.all) <
                           To_Lower_Case (Next_Node.Spec_Name.all)
                        then

                           if Next_Node.Prev = null then
                              Insert_First := True;
                           else
                              Insert_After := Next_Node.Prev;
                           end if;

                           exit;
                        end if;

                     end if;

                     if Next_Node.Next = null then
                        Insert_After := Next_Node;
                        Insert_Last  := True;
                        exit;
                     else
                        Next_Node := Next_Node.Next;
                     end if;

                  end loop;

               else
                  Insert_After := State.Current_List.Last;
                  Insert_Last  := True;
               end if;

               --  inserting in the list:
               if Insert_First then
                  --  inserting in the beginning:
                  New_Node.Next           := State.Current_List;
                  State.Current_List.Prev := New_Node;
                  State.Current_List      := New_Node;
               elsif Insert_Last then
                  New_Node.Prev           := Insert_After;
                  Insert_After.Next       := New_Node;
                  State.Current_List.Last := New_Node;
               else
                  New_Node.Next          := Insert_After.Next;
                  Insert_After.Next.Prev := New_Node;
                  New_Node.Prev          := Insert_After;
                  Insert_After.Next      := New_Node;
               end if;

            end if;

         end if;

      end Insert_In_List;

   --  start of the processing of Create_Element_Node
   begin

      if State.Argument_Spec then
         Body_Structure.Spec      := Element;
         State.Argument_Spec      := False;
         Body_Structure.Spec_Name := new Program_Text'(Name (Element));
         Current_Node             := Body_Structure'Access;

      elsif Arg_Kind = A_Defining_Identifier then
         --  skipping a defining name of a spec which may contain local
         --  specs requiring bodies
         null;
      elsif Arg_Kind = A_Protected_Definition then
         --  we just have to go one level down to process protected items:
         null;
      elsif not Requires_Body (Element) then
         Control := Abandon_Children;
         return;

      else
         Insert_In_List (State, Element, Current_Node);
      end if;

      if Arg_Kind = A_Package_Declaration          or else
         Arg_Kind = A_Generic_Package_Declaration  or else
         Arg_Kind = A_Single_Protected_Declaration or else
         Arg_Kind = A_Protected_Type_Declaration
      then
         --  here we may have specs requiring bodies inside a construct
         State.New_List_Needed := True;
         State.Last_Top        := Current_Node;
      elsif Arg_Kind = A_Protected_Definition then
         --  we have to skip this syntax level
         null;
      else
         --  no need to go deeper
         Control := Abandon_Children;
      end if;

   end Create_Element_Node;

   -------------------
   -- Create_Sample --
   -------------------

   procedure Create_Sample is
      CU         : Asis.Compilation_Unit;
      CU_Kind    : Unit_Kinds;

      My_Control     : Traverse_Control := Continue;
      My_State       : Body_State;
      Header_Created : Boolean;

   begin

      CU := Main_Unit_In_Current_Tree (My_Context);

      CU_Kind := Unit_Kind (CU);

      if Is_Nil (CU) then
         Error ("file " & Gnatstub.Options.File_Name.all &
                " does not contain a unit to process");
         return;

      end if;

      if Generate_Subunits then
         Generate_Separate_Stubs (CU);
      else

         if not (CU_Kind in A_Subprogram_Declaration   or else
                    CU_Kind in A_Generic_Unit_Declaration or else
                    CU_Kind =  A_Package)
         then
            Error ("Compilation unit " & To_String (Unit_Full_Name (CU)) &
                   " cannot have a body" &
                   (if not Quiet_Mode then
                       "  Unit Kind: " & (CU_Kind'Img)
                    else
                       ""));

            return;

         elsif not Asis.Extensions.CU_Requires_Body (CU) then
            Error ("Compilation unit " & To_String (Unit_Full_Name (CU)) &
                   " does not require a body");

            return;

         else
            --  and here we have to do the job:

            begin
               Create (Body_File, Out_File, Body_Name.all, Body_Form.all);
            exception
               when Ada.IO_Exceptions.Name_Error =>
                  Put_Line (Standard_Error, "gnatstub: cannot create "
                        & "check the file name");
                  raise Fatal_Error;
            end;

            Create_Body_Structure (
               Element => Unit_Declaration (CU),
               Control => My_Control,
               State   => My_State);

            --  first, trying to create the header, if needed:
            Generate_CU_Header (Header_Created);

            if Header_Created then
               Put_Line (Body_File, "pragma Ada_2012;");
               Generate_Body_Structure;
               Close (Body_File);

               if not Quiet_Mode then
                  Info ("body is created for " &
                        Gnatstub.Options.File_Name.all);
               end if;

            else
               Error ("failed to write the comment header for the body for " &
                        Gnatstub.Options.File_Name.all);
            end if;

         end if;

      end if;

      Close (My_Context);
      Dissociate (My_Context);
      Asis.Implementation.Finalize;

   exception

      when Ex : Asis.Exceptions.ASIS_Inappropriate_Context
             |  Asis.Exceptions.ASIS_Inappropriate_Container
             |  Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit
             |  Asis.Exceptions.ASIS_Inappropriate_Element
             |  Asis.Exceptions.ASIS_Inappropriate_Line
             |  Asis.Exceptions.ASIS_Inappropriate_Line_Number
             |  Asis.Exceptions.ASIS_Failed
        =>
         New_Line (File => Standard_Error);

         if Asis.Implementation.Status = Asis.Errors.Use_Error and then
            Reuse_Tree and then Tree_Exists
         then
            Error ("the tree you try to reuse may be obsolete");
            Error_No_Tool_Name
              ("either recreate the tree or do not use -r option");
         else
            Report_Unhandled_ASIS_Exception (Ex);
         end if;

         Emergency_Clean_Up;
         raise Fatal_Error;

      when others =>
         Emergency_Clean_Up;
         raise;
   end Create_Sample;

   ---------------------
   -- Prepare_Context --
   ---------------------

   procedure Prepare_Context is separate;
   --  We need different bodies for GNSA and non-GNSA versions

   ------------------------
   -- Emergency_Clean_Up --
   ------------------------

   procedure Emergency_Clean_Up is
   begin
      if Is_Open (My_Context) then
         Close (My_Context);
      end if;

      Dissociate (My_Context);
      Asis.Implementation.Finalize;

      if Is_Open (Body_File) then
         --  No need to keep a broken body in case of an emergency clean up
         Delete (Body_File);
      end if;

      if Is_Open (Spec_File) then
         --  No need to keep a broken body in case of an emergency clean up
         Close (Spec_File);
      end if;

   end Emergency_Clean_Up;

   -------------------------
   -- Free_Body_Structure --
   -------------------------

   procedure Free_Body_Structure is
      procedure Free_Node (Node : in out Link);

      procedure Free_List (L  : Link);

      procedure Free_Node (Node : in out Link) is
      begin
         Free (Node.Spec_Name);
         Free_List (Node.Down);
         Free (Node);
      end Free_Node;

      procedure Free_List (L  : Link) is
         Current, Next : Link;
      begin
         Current := L;

         while Current /= null loop
            Next := Current.Next;
            Free_Node (Current);
            Current := Next;
         end loop;
      end Free_List;
   begin
      Free_List (Body_Structure_Access.Down);
      Free (Body_Structure_Access.Spec_Name);
   end Free_Body_Structure;

   -----------------------------
   -- Generate_Body_Structure --
   -----------------------------

   procedure Generate_Body_Structure is

      procedure Print_Node (Node : Link);
      --  outputs a Node into Body_File

      procedure Print_Node_List (List : Link);
      --  outputs a list of nodes into Body_File. These two procedures -
      --  Print_Node and Print_Node_List call each other recursively

      procedure Print_Node (Node : Link) is
         Arg_Kind : constant Flat_Element_Kinds :=
           Flat_Element_Kind (Node.Spec);
      begin

         if Node /= Body_Structure'Access and then Bodyless_Package (Node) then
            return;
         end if;

         if Level /= 0
           and then
            Arg_Kind not in An_Incomplete_Type_Declaration ..
                            A_Tagged_Incomplete_Type_Declaration
         then
            Generate_Unit_Header (Node);
         end if;

         case Arg_Kind is

            when A_Package_Declaration |
                 A_Generic_Package_Declaration =>
               Generate_Package_Body (Node);

            when A_Function_Declaration         |
                 A_Function_Body_Stub           |
                 A_Generic_Function_Declaration =>
               Generate_Function_Body (Node);

            when A_Procedure_Declaration        |
                 A_Procedure_Body_Stub          |
                 A_Generic_Procedure_Declaration =>
               Generate_Procedure_Body (Node);

            when An_Entry_Declaration =>
               Generate_Entry_Body (Node);

            when A_Single_Protected_Declaration |
                 A_Protected_Type_Declaration =>
               Generate_Protected_Body (Node);

            when A_Single_Task_Declaration |
                 A_Task_Type_Declaration =>
               Generate_Task_Body (Node);

            when An_Incomplete_Type_Declaration       |
                 A_Tagged_Incomplete_Type_Declaration =>
               Generate_Full_Type_Declaration (Node);

            when others =>
               Put      (Standard_Error, "gnatstub: unexpected element ");
               Put_Line (Standard_Error, "in the body structure");
               raise Fatal_Error;
         end case;

         if Node.Down /= null then
            Print_Node_List (Node.Down);
         end if;

      end Print_Node;

      procedure Print_Node_List (List : Link) is
         Next_Node  : Link;
         List_Start : Link := List;
      begin
         Level := Level + 1;

         --  here we have to go to the beginning of the list:

         while List_Start.Prev /= null loop
            List_Start := List_Start.Prev;
         end loop;

         Next_Node := List_Start;

         loop
            Print_Node (Next_Node);

            if Next_Node.Next /= null then
               Next_Node := Next_Node.Next;
            else
               exit;
            end if;

         end loop;

         --  finalizing the enclosing construct:
         Level := Level - 1;
         Next_Node := Next_Node.Up;

         Set_Col (Body_File, Positive_Count (1 + Level * Indent_Level));

         Put_Line (Body_File, "end " & Next_Node.Spec_Name.all & ";");

         if List.Up /= Body_Structure'Access then
            New_Line (Body_File);
         end if;

      end Print_Node_List;

   begin
      Print_Node (Body_Structure_Access);
   end Generate_Body_Structure;

   ------------------------
   -- Generate_CU_Header --
   ------------------------

   procedure Generate_CU_Header
     (Success      : out Boolean;
      For_Subunit  : Boolean      := False;
      Subunit_Name : Asis.Element := Nil_Element)
   is

      --  This local declarations are used to generate a sample comment
      --  header
      Unit_Name_Len          : Positive :=
        (if For_Subunit then
            Body_Structure.Spec_Name'Length + 1
         else
            Body_Structure.Spec_Name'Length);
      Left_Unit_Name_Spaces  : Positive;
      Right_Unit_Name_Spaces : Positive;
      Left_Body_Spaces       : Positive;
      Right_Body_Spaces      : Positive;
      Name_With_Spaces       : Boolean := True;
      Body_String            : constant Program_Text := "B o d y";
      Body_String_Len        : constant Positive     := Body_String'Length;

      --  These local declarations are used to copy a comment header from the
      --  argument spec
      Header_File            : File_Type;
      String_Buf             : Program_Text (1 .. Hostparm.Max_Line_Length);
      Header_Line_Len        : Natural;
      Idx                    : Natural;
      Line_Count             : Natural := 0;
      Spec_String_Start      : Natural;

      Subunit_Name_On_Separate_Line :           Boolean := False;
      Subunit_Name_Str              : constant Program_Text :=
        (if For_Subunit then Defining_Name_Image (Subunit_Name) else "");

      Subunit_Name_Len              : Natural := Subunit_Name_Str'Length;
      Left_Subunit_Name_Spaces      : Positive;
      Right_Subunit_Name_Spaces     : Positive;
      Total_Subunit_Name_Len        : Positive;

      procedure Strip_Trailing_Spaces;
      --  Strips trailing spaces from the string stored in String_Buf
      --  (assuming that Header_Line_Len points to the last character of this
      --  string) and resets Header_Line_Len accordingly.

      procedure Strip_Trailing_Spaces is
      begin

         for J in reverse 1 .. Header_Line_Len loop

            if not Is_White_Space (String_Buf (J)) then
               Header_Line_Len := J;
               exit;
            end if;

         end loop;

      end Strip_Trailing_Spaces;

   begin
      Success := False;

      case Header is

         when No_Header =>
            null;

         when Stand_Header =>
            --  first, checking if we can fit the maximum line length:

            if Unit_Name_Len + 6 > Max_Body_Line_Length
              or else
               (For_Subunit
               and then
                Subunit_Name_Len + 6 > Max_Body_Line_Length)
            then
               Put (Standard_Error, "gnatstub: unit name is too ");
               Put_Line (Standard_Error, "long to generate a comment header");
               Put      (Standard_Error, "gnatstub: try to increase ");
               Put_Line (Standard_Error, "the maximum body line length");
               raise Fatal_Error;
            end if;

            if (2 * Unit_Name_Len - 1) + 6 > Max_Body_Line_Length
              or else
               (For_Subunit
               and then
                (2 * Subunit_Name_Len - 1) + 6 > Max_Body_Line_Length)
            then
               Name_With_Spaces := False;
            else
               Unit_Name_Len    := 2 * Unit_Name_Len - 1;

               if For_Subunit then
                  Subunit_Name_Len := 2 * Subunit_Name_Len - 1;
               end if;
            end if;

            if For_Subunit then
               Total_Subunit_Name_Len := Unit_Name_Len + Subunit_Name_Len;

               if Name_With_Spaces then
                  Total_Subunit_Name_Len := Total_Subunit_Name_Len + 1;
               end if;

               if Total_Subunit_Name_Len + 6 > Max_Body_Line_Length then
                  Subunit_Name_On_Separate_Line := True;

                  Left_Subunit_Name_Spaces  :=
                    (Max_Body_Line_Length - 4 - Subunit_Name_Len) / 2;
                  Right_Subunit_Name_Spaces :=
                    Max_Body_Line_Length - Subunit_Name_Len -
                      4 - Left_Subunit_Name_Spaces;
               else
                  Unit_Name_Len := Total_Subunit_Name_Len;
               end if;

            end if;

            Left_Unit_Name_Spaces  :=
              (Max_Body_Line_Length - 4 - Unit_Name_Len) / 2;
            Right_Unit_Name_Spaces :=
              Max_Body_Line_Length - Unit_Name_Len -
                4 - Left_Unit_Name_Spaces;

            Left_Body_Spaces  :=
              (Max_Body_Line_Length - 4 - Body_String_Len) / 2;
            Right_Body_Spaces :=
               Max_Body_Line_Length - Body_String_Len - 4 - Left_Body_Spaces;

            Put_Line (Body_File, Max_Body_Line_Length * '-');
            Put_Line
              (Body_File, "--" & (Max_Body_Line_Length - 4) * ' ' & "--");

            Put (Body_File, "--" & Left_Unit_Name_Spaces * ' ');

            if Name_With_Spaces then
               Put (Body_File, To_Upper (Body_Structure.Spec_Name
                               (Body_Structure.Spec_Name'First)));

               for I in Body_Structure.Spec_Name'First + 1 ..
                        Body_Structure.Spec_Name'Last
               loop
                  Put
                    (Body_File, ' ' & To_Upper (Body_Structure.Spec_Name (I)));
               end loop;

               if For_Subunit and then not Subunit_Name_On_Separate_Line then
                  Put (Body_File, " .");

                  for I in Subunit_Name_Str'Range loop
                     Put (Body_File,
                          ' ' & To_Upper (Subunit_Name_Str (I)));
                  end loop;
               end if;

            else
               Put (Body_File, To_Upper (Body_Structure.Spec_Name.all));
               if For_Subunit and then not Subunit_Name_On_Separate_Line then
                  Put (Body_File, '.' & To_Upper (Subunit_Name_Str));
               end if;
            end if;

            Put_Line (Body_File, Right_Unit_Name_Spaces * ' ' & "--");

            if For_Subunit and then Subunit_Name_On_Separate_Line then
               Put (Body_File, "--" & Left_Subunit_Name_Spaces * ' ');

               if Name_With_Spaces then
                  Put (Body_File,
                       To_Upper (Subunit_Name_Str (Subunit_Name_Str'First)));

                  for I in Subunit_Name_Str'First + 1 ..
                           Subunit_Name_Str'Last
                  loop
                     Put (Body_File, ' ' & To_Upper (Subunit_Name_Str (I)));
                  end loop;
               else
                  Put (Body_File, To_Upper (Subunit_Name_Str));
               end if;

               Put_Line (Body_File, Right_Subunit_Name_Spaces * ' ' & "--");
            end if;

            Put_Line
              (Body_File, "--" & (Max_Body_Line_Length - 4) * ' ' & "--");

            Put_Line (Body_File,
                      "--" & Left_Body_Spaces * ' ' &
                      Body_String & Right_Body_Spaces * ' ' &  "--");

            Put_Line
              (Body_File, "--" & (Max_Body_Line_Length - 4) * ' ' & "--");
            Put_Line
              (Body_File, Max_Body_Line_Length * '-');
            New_Line (Body_File);

         when From_Spec =>

            Open (Header_File, In_File, File_Name.all, "");

            while not End_Of_File (Header_File) loop
               Get_Line (Header_File, String_Buf, Header_Line_Len);
               Strip_Trailing_Spaces;
               Line_Count := Line_Count + 1;

               exit when String_Buf (1 .. 2) /= "--";

               if Header_Line_Len  > Max_Body_Line_Length then
                  Error ("line" & Line_Count'Img &
                         " in spec's comment header is too long");
                  Close (Header_File);

                  raise Fatal_Error;
               end if;

               Spec_String_Start :=
                  Index (Source => String_Buf (1 .. Header_Line_Len),
                         Pattern => "S p e c");

               if Spec_String_Start /= 0 then
                  Overwrite (Source   => String_Buf (1 .. Header_Line_Len),
                             Position => Spec_String_Start,
                             New_Item => "B o d y");
               end if;

               Put_Line (Body_File, String_Buf (1 .. Header_Line_Len));

            end loop;

            Close (Header_File);

         when From_File =>

            Open (Header_File, In_File, Header_File_Name.all, "");

            while not End_Of_File (Header_File) loop
               Get_Line (Header_File, String_Buf, Header_Line_Len);
               Strip_Trailing_Spaces;
               Line_Count := Line_Count + 1;
               Idx        := ASIS_Index_Non_Blank (String_Buf);

               if Idx /= 0
                 and then
                  (Header_Line_Len - Idx = 0
                  or else
                   String_Buf (Idx .. Idx + 1) /= "--")
               then
                  Error ("line" & Line_Count'Img &
                         " in comment header file is not a comment line");

                  Close (Header_File);

                  raise Fatal_Error;
               elsif Header_Line_Len > Max_Body_Line_Length then
                  Error ("line" & Line_Count'Img &
                         " in comment header file is too long");
                  Close (Header_File);

                  raise Fatal_Error;
               end if;

               Put_Line (Body_File, String_Buf (1 .. Header_Line_Len));

            end loop;

            Close (Header_File);

      end case;

      Success := True;

   exception
      when others =>
         Emergency_Clean_Up;
         raise;
   end Generate_CU_Header;

   -------------------------
   -- Generate_Entry_Body --
   -------------------------

   procedure Generate_Entry_Body (Node : Link) is
      Change_Line : Boolean;
   begin
      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put (Body_File, "entry " & Node.Spec_Name.all);

      Generate_Profile (Node, Change_Line);

      if Change_Line then
         New_Line (Body_File);
         Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));
      else
         Put (Body_File, " ");
      end if;

      Put (Body_File, "when Standard.True");

      --  now we have to decide how to output "is"
      if Change_Line or else
         Natural (Col (Body_File)) + 3 > Max_Body_Line_Length
      then
         New_Line (Body_File);
         Set_Col  (Body_File, Positive_Count (1 + (Level) * Indent_Level));
      else
         Put (Body_File, ' ');
      end if;

      Put_Line (Body_File, "is");

      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put_Line (Body_File, "begin");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));
      Put_Line (Body_File, "--  Generated stub: replace with real body!");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));
      Put_Line (Body_File,
                "pragma Compile_Time_Warning (Standard.True, """ &
                To_Wide_String (To_String (Node.Spec_Name.all)) &
                " unimplemented"");");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));

      if No_Exception_In_Stubs then
         Put_Line (Body_File, "null;");
      else
         Put (Body_File, "raise Program_Error");

         if Opt.Ada_Version >= Ada_2005 then
            Put (Body_File,
                 " with ""Unimplemented entry " &
                 To_Wide_String (To_String (Node.Spec_Name.all)) & '"');
         end if;

         Put_Line (Body_File, ";");
      end if;

      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put_Line (Body_File, "end " & Node.Spec_Name.all & ";");

      New_Line (Body_File);
   end Generate_Entry_Body;

   ------------------------------------
   -- Generate_Full_Type_Declaration --
   ------------------------------------

   procedure Generate_Full_Type_Declaration (Node : Link) is
      Discr_Part : constant Asis.Element := Discriminant_Part (Node.Spec);
      Is_Tagged  : constant Boolean      := Flat_Element_Kind (Node.Spec) =
        A_Tagged_Incomplete_Type_Declaration;

   begin
      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put (Body_File, "type " & Node.Spec_Name.all & " ");

      if Flat_Element_Kind (Discr_Part) = A_Known_Discriminant_Part then
         --  we do not split components of a discriminant part to fit
         --  Max_Body_Line_Length constraint (if needed) - it does not make any
         --  sense, because a user will for sure change this sample completion
         --  for an incomplete type declaration
         Put (Body_File, ASIS_Trim (Element_Image (Discr_Part)) & " ");
      end if;

      Put (Body_File, "is ");

      if Is_Tagged then
         Put (Body_File, "tagged ");
      end if;

      Put_Line (Body_File, "null record;");

      New_Line (Body_File);
   end Generate_Full_Type_Declaration;

   ----------------------------
   -- Generate_Function_Body --
   ----------------------------

   procedure Generate_Function_Body (Node : Link) is
      No_Raise_Statement : Boolean := False;
      Change_Line : Boolean;
   begin
      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));

      if Is_Overriding_Declaration (Node.Spec) then
         Put (Body_File, "overriding ");
      elsif Is_Not_Overriding_Declaration (Node.Spec) then
         Put (Body_File, "not overriding ");
      end if;

      Put (Body_File, "function " & Node.Spec_Name.all);
      Generate_Profile (Node, Change_Line);

      if Change_Line then
         Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
         Put_Line (Body_File, "is");
      else
         Put_Line (Body_File, " is");
      end if;

      if No_Exception_In_Stubs
        and then
         Allows_Var_Declaration (Result_Profile (Node.Spec))
      then
         No_Raise_Statement := True;
         Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));
         Put (Body_File, "Result : ");
         Put (Body_File,
              ASIS_Trim (Element_Image (Result_Profile (Node.Spec))));
         Put_Line (Body_File, ";");
      end if;

      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put_Line (Body_File, "begin");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));
      Put_Line (Body_File, "--  Generated stub: replace with real body!");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));
      Put_Line (Body_File,
                "pragma Compile_Time_Warning (Standard.True, """ &
                Image_For_String
                  (To_Wide_String (To_String (Node.Spec_Name.all))) &
                " unimplemented"");");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));

      if No_Raise_Statement then
         Put_Line (Body_File, "return Result;");
      else
         --  We now use a return of a raise expression, to avoid a kludgey
         --  recursive call to itself. This is Ada 2012, so the checks for
         --  >= Ada_2005 are not actually necessary.

         Put (Body_File, "return raise Program_Error");

         if Opt.Ada_Version >= Ada_2005 then
            Put (Body_File,
                 " with ""Unimplemented function " &
                 To_Wide_String (To_String
                   (Image_For_String (Node.Spec_Name.all))) & '"');
         end if;

         Put_Line (Body_File, ";");
      end if;

      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put_Line (Body_File, "end " & Node.Spec_Name.all & ";");

      New_Line (Body_File);
   end Generate_Function_Body;

   ---------------------------
   -- Generate_Package_Body --
   ---------------------------

   procedure Generate_Package_Body (Node : Link) is
   begin
      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put_Line (Body_File, "package body " & Node.Spec_Name.all & " is");
      New_Line (Body_File);

      if Node = Body_Structure'Access and then Node.Down = null then
         --  this is a special case: an argument unit is a library [generic]
         --  package which requires a body but which does not contain any
         --  local declaration which itself requires a completion:
         Put_Line (Body_File, "end " & Node.Spec_Name.all & ";");
      end if;
   end Generate_Package_Body;

   -----------------------------
   -- Generate_Procedure_Body --
   -----------------------------

   procedure Generate_Procedure_Body (Node : Link) is
      Change_Line : Boolean;

   begin
      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));

      if Is_Overriding_Declaration (Node.Spec) then
         Put (Body_File, "overriding ");
      elsif Is_Not_Overriding_Declaration (Node.Spec) then
         Put (Body_File, "not overriding ");
      end if;

      Put (Body_File, "procedure " & Node.Spec_Name.all);
      Generate_Profile (Node, Change_Line);

      if Change_Line then
         Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
         Put_Line (Body_File, "is");
      else
         Put_Line (Body_File, " is");
      end if;

      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put_Line (Body_File, "begin");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));
      Put_Line (Body_File, "--  Generated stub: replace with real body!");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));
      Put_Line (Body_File,
                "pragma Compile_Time_Warning (Standard.True, """ &
                To_Wide_String (To_String (Node.Spec_Name.all)) &
                " unimplemented"");");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));

      if No_Exception_In_Stubs then
         Put_Line (Body_File, "null;");
      else
         Put (Body_File, "raise Program_Error");

         if Opt.Ada_Version >= Ada_2005 then
            Put (Body_File,
                 " with ""Unimplemented procedure " &
                 To_Wide_String (To_String (Node.Spec_Name.all)) & '"');
         end if;

         Put_Line (Body_File, ";");
      end if;

      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put_Line (Body_File, "end " & Node.Spec_Name.all & ";");

      New_Line (Body_File);
   end Generate_Procedure_Body;

   ----------------------
   -- Generate_Profile --
   ----------------------

   procedure Generate_Profile (Node : Link; Change_Line : out Boolean) is
      Arg_Kind  : constant Flat_Element_Kinds := Flat_Element_Kind (Node.Spec);
      Parameters : constant Asis.Element_List := Parameter_Profile (Node.Spec);
      Spec_Span : Span;
      Family_Def : Asis.Element;

      Construct_Len : Positive;

   begin
      Change_Line := False;

      --  first, generating an entry_index_specification for an entry_body,
      --  if needed:

      if Arg_Kind = An_Entry_Declaration then
         Family_Def := Entry_Family_Definition (Node.Spec);

         if not Is_Nil (Family_Def) then
            Spec_Span := Element_Span (Family_Def);

            --  checking how entry_index_specification should be printed
            --  "+ 12" below means " (for I in )"
            if (Spec_Span.First_Line /= Spec_Span.Last_Line) or else
                (Character_Position (Col (Body_File)) + 12 +
                 Spec_Span.Last_Column - Spec_Span.First_Column + 1) >
               Max_Body_Line_Length
            then
               Change_Line := True;
            end if;

            if Change_Line then
               New_Line (Body_File);

               if Indent_Level > 0 then
                  Set_Col (
                     Body_File,
                     Positive_Count (1 + (Level + 1) * Indent_Level) - 2);
               end if;

            end if;

            Put (Body_File, " (for I in ");
            Put (Body_File, ASIS_Trim (Element_Image (Family_Def)));
            Put (Body_File, ")");
         end if;
      end if;

      --  Now we have to decide, how to print parameter [and result] profile
      if Change_Line = False then

         if Arg_Kind = A_Generic_Procedure_Declaration or else
            Arg_Kind = A_Generic_Function_Declaration
         then
            --  Here we cannot use Span-based approach, so we use the
            --  rough parameter-number-based estimation:
            if Parameters'Length >= 2 then
               Change_Line   := True;
            end if;

         else

            Spec_Span := Element_Span (Node.Spec);

            if Spec_Span.First_Line /= Spec_Span.Last_Line then
               --  First, rough check: if an argument spec occupies more than
               --  one line, we print parameters specs on separate lines:
               Change_Line   := True;
            else
               --  We check if a construct plus additions needed for the body
               --  plus indentation level in the body fits maximum line length
               --  defined for the body. We assume that the argument spec is
               --  reasonably formatted

               Construct_Len := Spec_Span.Last_Column - Spec_Span.First_Column
                                + 1;

               if Arg_Kind = An_Entry_Declaration and then
                   not Is_Nil (Family_Def)
               then
                  Construct_Len := Construct_Len + 9;
                  --  "+ 9" stands for "for I in "
               else
                  Construct_Len := Construct_Len + 3;
                  --  "+ 3" stands for " is"
               end if;

               if Level * Indent_Level + Construct_Len >
                  Max_Body_Line_Length
               then
                  Change_Line := True;
               end if;
            end if;
         end if;
      end if;

      if not Is_Nil (Parameters) then

         if Change_Line then
            New_Line (Body_File);

            if Indent_Level > 0 then
               Set_Col  (Body_File,
                         Positive_Count (1 + (Level + 1) * Indent_Level - 1));
            end if;

            Put (Body_File, "(");
         else
            Put (Body_File, " (");
         end if;

         for I in Parameters'Range loop
            Put (Body_File, ASIS_Trim (Element_Image (Parameters (I))));

            if I /= Parameters'Last then

               if Change_Line then
                  Put_Line (Body_File, ";");
                  Set_Col  (Body_File,
                            Positive_Count (1 + (Level + 1) * Indent_Level));
               else
                  Put (Body_File, "; ");
               end if;
            end if;
         end loop;

         Put (Body_File, ")");

      end if;

      if Arg_Kind = A_Function_Declaration or else
         Arg_Kind = A_Generic_Function_Declaration
      then
         --  we have to output " return <type_mark>:
         if Change_Line then
            New_Line (Body_File);
            Set_Col  (Body_File,
                      Positive_Count (1 + (Level + 1) * Indent_Level));
            Put (Body_File, "return ");
         else
            Put (Body_File, " return ");
         end if;

         if Definition_Kind (Result_Profile (Node.Spec)) /=
            An_Access_Definition
          and then
            Is_Not_Null_Return (Node.Spec)
         then
            Put (Body_File, "not null ");
         end if;

         Put (Body_File,
           ASIS_Trim (Element_Image (Result_Profile (Node.Spec))));

      end if;

      if Col (Body_File) + 3 >
        Ada.Wide_Text_IO.Count (Max_Body_Line_Length)
      then
         Change_Line   := True;
      end if;
   end Generate_Profile;

   -----------------------------
   -- Generate_Protected_Body --
   -----------------------------

   procedure Generate_Protected_Body (Node : Link) is
   begin
      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put_Line (Body_File, "protected body " & Node.Spec_Name.all & " is");
      New_Line (Body_File);

      if Node.Down = null then
         --  protected definition with no protected operation is somewhat
         --  strange, but legal case
         Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
         Put_Line (Body_File, "end " & Node.Spec_Name.all & ";");
         New_Line (Body_File);
      end if;
   end Generate_Protected_Body;

   -----------------------------
   -- Generate_Separate_Stubs --
   -----------------------------

   procedure Generate_Separate_Stubs (Body_Unit : Asis.Compilation_Unit) is
      CU_Kind : constant Unit_Kinds := Unit_Kind (Body_Unit);
      Unit    : Asis.Element;

      function Get_Subunit_Name (Def_Name : Asis.Element) return String;
      --  Creates the name for the file where the subunit should be located
      --  using the template created from the argument body name.

      function Get_Subunit_Name (Def_Name : Asis.Element) return String is
         Def_Name_Image : String :=
           (To_String (Defining_Name_Image (Def_Name)));
      begin
         case Subunit_Name_Casing is
            when Lowercase =>
               Def_Name_Image := To_Lower (Def_Name_Image);
            when Uppercase =>
               Def_Name_Image := To_Upper (Def_Name_Image);
            when Mixedcase =>
               Def_Name_Image := Capitalize (Def_Name_Image);
         end case;

         return
           Subunit_Name_Temlate (First_Idx .. Insert_Idx) &
           Def_Name_Image &
           Subunit_Name_Temlate (Insert_Idx + 1 .. Last_Idx);

      end Get_Subunit_Name;

   begin
      if CU_Kind not in A_Procedure_Body         |
                        A_Function_Body          |
                        A_Package_Body           |
                        A_Procedure_Body_Subunit |
                        A_Function_Body_Subunit  |
                        A_Package_Body_Subunit   |
                        A_Task_Body_Subunit      |
                        A_Protected_Body_Subunit
      then
         Error ("argument unit cannot have subunits (" &
                Capitalize (CU_Kind'Img) & ")");
         return;
      end if;

      Unit := Unit_Declaration (Body_Unit);

      declare
         Dcls : constant Asis.Element_List :=
           (if Declaration_Kind (Unit) = A_Protected_Body_Declaration then
               Protected_Operation_Items (Unit)
            else
               Body_Declarative_Items (Unit));

         Body_El  : Asis.Element;
         Stub_Dcl : Asis.Element;
         Def_Name : Asis.Element;

         Success    : Boolean;
         My_Control : Traverse_Control := Continue;
         My_State   : Body_State;

      begin
         for J in Dcls'Range loop

            if Declaration_Kind (Dcls (J)) in A_Body_Stub then
               Body_El  := Corresponding_Subunit (Dcls (J));
               Def_Name := First_Name (Dcls (J));

               if not Is_Nil (Body_El) then
                  goto Continue;
               else
                  Body_Name := new String'(Get_Subunit_Name (Def_Name));
               end if;

               if GNAT.OS_Lib.Is_Regular_File (Body_Name.all) then

                  if not Quiet_Mode then
                     Put_Line
                       (Current_Error,
                        "separate body is not created for stub for " &
                        Defining_Name_Image (Def_Name));
                     Put_Line
                       (Current_Error,
                        "  file " & To_Wide_String (Body_Name.all) &
                        "already exists");
                  end if;

                  Free (Body_Name);
                  goto Continue;
               end if;

               begin
                  Create
                    (Body_File, Out_File, Body_Name.all, Body_Form.all);
               exception
                  when others =>
                     Error ("cannot create file " & Body_Name.all &
                            " for subunit");
                     goto Continue;
               end;

               Stub_Dcl := Corresponding_Declaration (Dcls (J));

               if Is_Nil (Stub_Dcl) then
                  --  Body stub acts as a spec
                  Stub_Dcl := Dcls (J);
               end if;

               Body_Structure.Spec_Name :=
                 new Program_Text'(Unit_Full_Name (Body_Unit));
               --  We need this because Generate_CU_Header takes the
               --  compilation unit name from Body_Structure

               Generate_CU_Header
                 (Success,
                  For_Subunit  => True,
                  Subunit_Name => Def_Name);

               Put_Line (Body_File, "pragma Ada_2012;");
               Put_Line
                 (Body_File,
                  "separate (" & Body_Structure.Spec_Name.all & ')');

               Free (Body_Structure.Spec_Name);

               My_State := Initial_State;

               Create_Body_Structure (
                  Element => Stub_Dcl,
                  Control => My_Control,
                  State   => My_State);

               Generate_Body_Structure;
               Free_Body_Structure;

               Close (Body_File);
               Free (Body_Name);

               if not Quiet_Mode then
                  Put_Line
                    (Current_Error,
                     "separate body is created for stub for " &
                     Defining_Name_Image (Def_Name));
               end if;
            end if;

            <<Continue>>
         end loop;
      end;

   end Generate_Separate_Stubs;

   ------------------------
   -- Generate_Task_Body --
   ------------------------

   procedure Generate_Task_Body (Node : Link) is
   begin
      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put_Line (Body_File, "task body " & Node.Spec_Name.all & " is");

      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put_Line (Body_File, "begin");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));
      Put_Line (Body_File, "--  Generated stub: replace with real body!");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));
      Put_Line (Body_File,
                "pragma Compile_Time_Warning (Standard.True, """ &
                To_Wide_String (To_String (Node.Spec_Name.all)) &
                " unimplemented"");");

      Set_Col  (Body_File, Positive_Count (1 + (Level + 1) * Indent_Level));

      if No_Exception_In_Stubs then
         Put_Line (Body_File, "null;");
      else
         Put (Body_File, "raise Program_Error");

         if Opt.Ada_Version >= Ada_2005 then
            Put (Body_File,
                 " with ""Unimplemented task " &
                 To_Wide_String (To_String (Node.Spec_Name.all)) & '"');
         end if;

         Put_Line (Body_File, ";");
      end if;

      Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
      Put_Line (Body_File, "end " & Node.Spec_Name.all & ";");
      New_Line (Body_File);
   end Generate_Task_Body;

   --------------------------
   -- Generate_Unit_Header --
   --------------------------

   procedure Generate_Unit_Header (Node : Link) is
      Header_Length : constant Natural := Node.Spec_Name'Length + 6;
   begin
      if not No_Local_Comment_Headers then
         Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
         Put_Line (Body_File, Header_Length * '-');

         Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
         Put      (Body_File, "-- ");
         Put      (Body_File, Node.Spec_Name.all);
         Put_Line (Body_File, " --");

         Set_Col  (Body_File, Positive_Count (1 + Level * Indent_Level));
         Put_Line (Body_File, Header_Length * '-');
         New_Line (Body_File);
      end if;
   end Generate_Unit_Header;

   -------------------
   -- Get_Tree_Name --
   -------------------

   function Get_Tree_Name return String is
      Dot_Index : Natural := Short_File_Name'Last;
   begin

      for J in reverse Short_File_Name'Range loop
         if Short_File_Name (J) = '.' then
            Dot_Index := J - 1;
            exit;
         end if;
      end loop;

      return Short_File_Name (Short_File_Name'First .. Dot_Index) & ".adt";
   end Get_Tree_Name;

   -----------
   -- Go_Up --
   -----------

   procedure Go_Up
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Body_State)
   is
      pragma Unreferenced (Control);

      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
   begin
      if not (Arg_Kind = A_Package_Declaration or else
              Arg_Kind = A_Generic_Package_Declaration or else
              Arg_Kind = A_Single_Protected_Declaration or else
              Arg_Kind = A_Protected_Type_Declaration)
      then
         return;
      end if;

      if State.New_List_Needed then
         --  no local body is needed for a given construct
         State.New_List_Needed := False;
      else
         --  we have to reset the current list:

         if State.Current_List /= null then
            State.Current_List := State.Current_List.Up;
            while State.Current_List.Prev /= null loop
               State.Current_List := State.Current_List.Prev;
            end loop;
         end if;
      end if;
   end Go_Up;

   -----------------------
   --  Image_For_String --
   -----------------------

   function Image_For_String (S : Program_Text) return Program_Text is
   begin
      if S (S'First) = '"' and then S (S'Last) = '"' then
         return '"' & S & '"';
      else
         return S;
      end if;
   end Image_For_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      ASIS_UL.Environment.Scan_Parameters (Gnatstub_Prj);
      Gnatstub.Projects.Set_Tree_Creator (Gnatstub_Prj);
      Check_Parameters;
      Prepare_Context;

      Initialized := True;

   exception
      when others =>
         Initialized := False;
         raise;
   end Initialize;

   ----------
   -- Name --
   ----------

   function Name (El : Asis.Element) return Program_Text is
      Def_Name : constant Asis.Element := Names (El) (1);
   begin
      return Defining_Name_Image (Def_Name);
   end Name;

   --------------------------
   -- Print_Gnatstub_Usage --
   --------------------------

   procedure Print_Gnatstub_Usage is
   begin
      Ada.Text_IO.Set_Error (Ada.Text_IO.Standard_Output);
      Brief_Help;

      New_Line;
      New_Line;
      Put_Line ("Report bugs to report@adacore.com");
   end Print_Gnatstub_Usage;

   -------------------
   -- Requires_Body --
   -------------------

   function Requires_Body (El : Element) return Boolean is
      Arg_Kind     : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Encl_El      : Asis.Element;
      Encl_El_Kind : Flat_Element_Kinds;
      Result       : Boolean := False;

   begin

      case Arg_Kind is
         when An_Incomplete_Type_Declaration       |
              A_Tagged_Incomplete_Type_Declaration =>
            Result := Is_Nil (Corresponding_Type_Declaration (El));
         when A_Task_Type_Declaration         |
              A_Protected_Type_Declaration    |
              A_Single_Task_Declaration       |
              A_Single_Protected_Declaration  |
              A_Package_Declaration           |
              A_Generic_Procedure_Declaration |
              A_Generic_Function_Declaration  |
              A_Generic_Package_Declaration    =>

            --  there is no harm to generate a local body sample for a local
            --  package or generic package
            Result := True;

         when A_Procedure_Declaration |
              A_Function_Declaration    =>

            --  there are two cases when a subprogram does not require
            --  completion: when it is already completed by renaming-as-body
            --  in a package spec or when it is abstract

            if Trait_Kind (El) /= An_Abstract_Trait then
               --  Result := Is_Nil (Corresponding_Body (El));  ???
               --  ??? the statement below implements the temporary solution
               --  ??? for subprograms completed by pragmas Import.
               --  ??? it should be revised when Asis.Extensions.Is_Completed
               --  ??? gets in a proper shape.

               Result := not (not Is_Nil (Corresponding_Body (El))
                          or else
                              Asis.Extensions.Is_Completed (El));
            end if;

         when An_Entry_Declaration =>
            Encl_El      := Enclosing_Element (El);
            Encl_El_Kind := Flat_Element_Kind (Encl_El);
            Result := Encl_El_Kind = A_Protected_Definition;
         when others =>
            null;
      end case;

      return Result;

   end Requires_Body;

end Gnatstub.Sampler;
