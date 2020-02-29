------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                  GNATTEST.SKELETON.GENERATOR.GATHER_DATA                 --
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

separate (GNATtest.Skeleton.Generator)
procedure Gather_Data
  (The_Unit          :     Asis.Compilation_Unit;
   Data              : out Data_Holder;
   Suite_Data_List   : out Suites_Data_Type;
   Apropriate_Source : out Boolean)
is
   Unit_SF_Name : constant String :=
     String'(To_String (Text_Name (The_Unit)));
   --  Stores the full name of the file containing the unit

   Control : Traverse_Control := Continue;

   Dummy_State : No_State := Not_Used;

   --  Temporal elements used to maintain readability

   Tmp_CU        : Asis.Compilation_Unit;
   Tmp_Element   : Asis.Element;
   Gen_Unit_Name : String_Access;

   Type_Counter      : Positive := 1;

   Dummy_Type_Counter : Natural := 0;

   Inside_Inst : Boolean := False;
   Inside_Gen  : Boolean := False;

   Inst_Elem : Asis.Element;

   --------------------------
   --  Inner Subprogramms  --
   --------------------------

   procedure First_Pre_Operation
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State);

   procedure Get_Records is new Traverse_Element
     (Pre_Operation     => First_Pre_Operation,
      Post_Operation    => No_Op,
      State_Information => No_State);
   --  Sets the vulue of Main_Type with the first tagged record element
   --  and checks that there's no more tagged records in the given unit.

   procedure Second_Pre_Operation
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State);

   procedure Get_Subprograms is new Traverse_Element
     (Pre_Operation     => Second_Pre_Operation,
      Post_Operation    => No_Op,
      State_Information => No_State);

   procedure Third_Pre_Operation
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State);

   procedure Gather_Inherited_Subprograms
     (Type_Def        : Asis.Element;
      Original_Type   : Asis.Element;
      Suite_Data_List : in out Suites_Data_Type);

   procedure Get_Nested_Packages is new Traverse_Element
     (Pre_Operation     => Third_Pre_Operation,
      Post_Operation    => No_Op,
      State_Information => No_State);

   function Parent_Subtype_Unit_Original
     (Type_Decl  : Asis.Element;
      Is_Generic : Boolean) return Asis.Compilation_Unit;
   pragma Unreferenced (Parent_Subtype_Unit_Original);
   --  Equivalent to Enclosing_Compilation_Unit (Corresponding_Parent_Subtype)
   --  for most cases. In case of a generic tested package tries to treat
   --  parent declaration as if it was declared in a formal package.

   procedure Gather_Test_Cases
     (Subp            :        Subp_Info;
      TR_Info         :        Test_Routine_Info_Wrapper;
      Data            : in out Data_Holder;
      Suite_Data_List : in out Suites_Data_Type;
      TC_Found        :    out Boolean);
   --  Adds one subprogram-to-test per each test case.
   --  Sets TC_Found if at least one Test_Case aspect or pragma has been found
   --  for given subprogram.

   procedure Get_Units_To_Stub;

   function Corresponding_Non_Generic_Subp
     (Subp : Asis.Declaration) return Asis.Declaration;
   --  Given the declaration of subprogram declared in package instantiation
   --  returns corresponding subprogram declaration declared in corresponding
   --  generic package.

   function Unrenamed_Generic_Unit
     (Declaration : Asis.Declaration)
      return        Asis.Declaration;
   --  Returns corresponding generic declaration unwinding possible renamings.

   function Package_Considered (Pack : Asis.Declaration) return Boolean;

   function Good_To_Stub (Pack : Asis.Declaration) return Boolean;

   function Unsupported_Unit_Kind (The_Unit : Asis.Compilation_Unit)
                                   return String;

   function Is_Ghost_Code (Decl : Asis.Declaration) return Boolean;

   procedure Check_For_Elaboration (The_Unit : Asis.Compilation_Unit);
   --  Checks if any elaboration control pragmas are present in spec and/or
   --  body of a package and issues a warning if so.

   procedure Check_Type_For_Elaboration (Decl : Asis.Element);
   --  Checking if is any of parent types have pragma
   --  Preelaborable_Initialization. This might cause
   --  elaboration conflicts in the harness, so a warning
   --  should be isued.

   procedure Add_Units_To_Stub (The_Unit : Asis.Compilation_Unit);

   Already_Stubbing : String_Set.Set := String_Set.Empty_Set;
   --  It is generally easier to store units to stub in a list, however
   --  to avoid duplications we use this local set since it is easier
   --  and faster to check membership in a set.

   -----------------------
   -- Add_Units_To_Stub --
   -----------------------

   procedure Add_Units_To_Stub (The_Unit : Asis.Compilation_Unit) is
      Spec_Clause_List : constant Asis.Context_Clause_List :=
        Context_Clause_Elements (The_Unit);
   begin
      for I in Spec_Clause_List'Range loop
         if
           Clause_Kind (Spec_Clause_List (I)) = A_With_Clause
           and then Trait_Kind (Spec_Clause_List (I)) not in
             A_Limited_Trait | A_Limited_Private_Trait
         then
            declare
               With_Names : constant Asis.Name_List :=
                 Clause_Names (Spec_Clause_List (I));

               Withed_Spec : Asis.Element;

               Parent_Unit : Compilation_Unit;
            begin
               for With_Name of With_Names loop
                  Withed_Spec :=
                    Enclosing_Element
                      (Corresponding_Name_Definition
                         (Normalize_Reference (With_Name)));
                  declare
                     Withed_Spec_Image : constant String :=
                       (Base_Name (To_String (Text_Name
                        (Enclosing_Compilation_Unit (Withed_Spec)))));
                  begin
                     Parent_Unit := Corresponding_Parent_Declaration
                       (Enclosing_Compilation_Unit (Withed_Spec));

                     if Good_To_Stub (Withed_Spec)
                       and then not Already_Stubbing.Contains
                         (Withed_Spec_Image)
                     then
                        Already_Stubbing.Include (Withed_Spec_Image);
                        Data.Units_To_Stub.Append (Withed_Spec);
                        Trace (Me, Withed_Spec_Image);
                     end if;
                  end;

                  --  Gathering parent packages
                  while not Is_Nil (Parent_Unit) loop
                     if
                       To_Lower
                         (To_String (Unit_Full_Name (Parent_Unit))) /=
                       "standard"
                     then
                        declare
                           Withed_Spec_Image : constant String :=
                             Base_Name (To_String (Text_Name (Parent_Unit)));
                        begin
                           if Good_To_Stub (Unit_Declaration (Parent_Unit))
                             and then not Already_Stubbing.Contains
                               (Withed_Spec_Image)
                           then
                              Already_Stubbing.Include (Withed_Spec_Image);
                              Data.Units_To_Stub.Append
                                (Unit_Declaration (Parent_Unit));
                              Trace (Me, Withed_Spec_Image);
                           end if;
                        end;
                     end if;
                     Parent_Unit :=
                       Corresponding_Parent_Declaration (Parent_Unit);

                  end loop;
               end loop;
            end;
         end if;
      end loop;

   end Add_Units_To_Stub;

   ---------------------------
   -- Check_For_Elaboration --
   ---------------------------

   procedure Check_For_Elaboration (The_Unit : Asis.Compilation_Unit) is
      Spec_Clause_List : constant Asis.Context_Clause_List :=
        Context_Clause_Elements (The_Unit, True);
      Spec_Pragmas_List : constant Asis.Context_Clause_List :=
        Corresponding_Pragmas (Unit_Declaration (The_Unit));
      Spec_Compilation_Pragmas : constant Asis.Context_Clause_List :=
        Compilation_Pragmas (The_Unit);

      Body_Unit : constant Compilation_Unit :=
        Corresponding_Body (The_Unit);
      Body_Clause_List : constant Asis.Context_Clause_List :=
        (if not Is_Nil (Body_Unit) and then not Is_Equal (Body_Unit, The_Unit)
         then Context_Clause_Elements (Body_Unit, True)
         else Nil_Element_List);
      Body_Pragmas_List : constant Asis.Context_Clause_List :=
        (if not Is_Nil (Body_Unit)
           and then not Is_Equal (Body_Unit, The_Unit)
           and then Unit_Kind (Body_Unit) /= A_Nonexistent_Body
         then Corresponding_Pragmas (Unit_Declaration (Body_Unit))
         else Nil_Element_List);
      Body_Compilation_Pragmas : constant Asis.Context_Clause_List :=
        (if not Is_Nil (Body_Unit)
           and then not Is_Equal (Body_Unit, The_Unit)
           and then Unit_Kind (Body_Unit) /= A_Nonexistent_Body
         then Compilation_Pragmas (Body_Unit)
         else Nil_Element_List);

      Spec : constant Asis.Element := Unit_Declaration (The_Unit);
      All_Decls : constant Asis.Declarative_Item_List :=
        Visible_Part_Declarative_Items (Spec, True)
        & Private_Part_Declarative_Items (Spec, True)
        & (if not Is_Nil (Body_Unit)
             and then not Is_Equal (Body_Unit, The_Unit)
             and then Unit_Kind (Body_Unit) /= A_Nonexistent_Body
           then Body_Statements (Unit_Declaration (Body_Unit))
           else Nil_Element_List);

      All_Pragmas : constant Asis.Element_List :=
        Spec_Clause_List
        & Spec_Pragmas_List
        & Spec_Compilation_Pragmas
        & Body_Clause_List
        & Body_Pragmas_List
        & Body_Compilation_Pragmas
        & All_Decls;

      Elaboration_Detected : Boolean := False;
      Elem_For_Sloc : Asis.Element;
   begin
      for I in All_Pragmas'Range loop
         if Element_Kind (All_Pragmas (I)) = A_Pragma then
            if
              To_Lower
                (To_String (Pragma_Name_Image (All_Pragmas (I)))) in
               "preelaborate"   |
               "pure"           |
               "elaborate"      |
               "elaborate_body" |
               "elaborate_all"  |
               "preelaborable_initialization"
            then
               Elaboration_Detected := True;
               Elem_For_Sloc := All_Pragmas (I);
               exit;
            end if;
         end if;
      end loop;

      if Elaboration_Detected then
         Report_Std
           ("warning: (gnattest) "
            & Base_Name (Unit_SF_Name)
            & ":"
            & Trim
              (Line_Number'Image (First_Line_Number (Elem_For_Sloc)), Both)
            & ":"
            & Trim
              (Line_Number'Image (First_Column_Number (Elem_For_Sloc)), Both)
            & ":"
            & " elaboration control pragma");
         Report_Std
           ("this can cause circularity in the test harness", 1);
      end if;
   end Check_For_Elaboration;

   --------------------------------
   -- Check_Type_For_Elaboration --
   --------------------------------

   procedure Check_Type_For_Elaboration (Decl : Asis.Element) is
      Tmp_Element : Asis.Element := Decl;

      El1, El2, El3 : Asis.Element := Nil_Element;
   begin
      while not  Is_Nil (Tmp_Element) loop

         exit when Declaration_Kind (Tmp_Element) =
           A_Formal_Type_Declaration;

         --  We need to check all 3 possible declarations.

         if Declaration_Kind (Tmp_Element) in
           An_Ordinary_Type_Declaration |
           A_Task_Type_Declaration      |
           A_Protected_Type_Declaration |
           A_Private_Type_Declaration   |
           A_Private_Extension_Declaration
         then
            El1 := Corresponding_Type_Partial_View (Tmp_Element);
         end if;

         if Declaration_Kind (El1) in
           An_Ordinary_Type_Declaration |
           A_Task_Type_Declaration      |
           A_Protected_Type_Declaration |
           A_Private_Type_Declaration   |
           A_Private_Extension_Declaration
         then
            El2 := Corresponding_Type_Partial_View (El1);
         end if;

         if Declaration_Kind (Tmp_Element) in
           An_Incomplete_Type_Declaration       |
           A_Tagged_Incomplete_Type_Declaration |
           A_Private_Type_Declaration           |
           A_Private_Extension_Declaration
         then
            El3 := Corresponding_Type_Completion (Tmp_Element);
         end if;

         declare
            function Pragmas_Or_Nil (El : Asis.Element)
                                     return Asis.Element_List
            is (if Is_Nil (El) then Nil_Element_List
                else Corresponding_Pragmas (El));

            Pragmas0 : constant Asis.Element_List :=
              Corresponding_Pragmas (Tmp_Element);
            Pragmas1 : constant Asis.Element_List :=
              Pragmas_Or_Nil (El1);
            Pragmas2 : constant Asis.Element_List :=
              Pragmas_Or_Nil (El2);
            Pragmas3 : constant Asis.Element_List :=
              Pragmas_Or_Nil (El3);

            Pragmas : constant Asis.Element_List :=
              Pragmas0 & Pragmas1 & Pragmas2 & Pragmas3;
         begin
            for I in Pragmas'Range loop
               if
                 To_Lower
                   (To_String (Pragma_Name_Image (Pragmas (I)))) =
                 "preelaborable_initialization"
               then
                  Report_Std
                    ("warning: (gnattest) "
                     & Base_Name (Unit_SF_Name)
                     & ":"
                     & Trim
                       (Line_Number'Image
                            (First_Line_Number (Decl)),
                        Both)
                     & ":"
                     & Trim
                       (Line_Number'Image
                            (First_Column_Number (Decl)),
                        Both)
                     & ":"
                     & " elaboration control pragma given"
                     & " for ancestor type of "
                     & To_String_First_Name (Decl));
                  Report_Std
                    ("this can cause circularity in the test harness",
                     1);
                  return;
               end if;
            end loop;
         end;

         Tmp_Element := Parent_Type_Declaration (Tmp_Element);
      end loop;
   end Check_Type_For_Elaboration;

   ------------------------------------
   -- Corresponding_Non_Generic_Subp --
   ------------------------------------

   function Corresponding_Non_Generic_Subp
     (Subp : Asis.Declaration) return Asis.Declaration
   is
      Elem : Asis.Element := Subp;

      Elem_Result : Asis.Element;

      Original_Span : constant Asis.Text.Span :=
        Element_Span_In_Template (Subp);

      Control : Traverse_Control := Continue;
      Dummy_State : No_State := Not_Used;

      procedure Look_For_Same_Span
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State);

      procedure Get_Subp_From_Gen_Package is new Traverse_Element
        (Pre_Operation     => Look_For_Same_Span,
         Post_Operation    => No_Op,
         State_Information => No_State);

      procedure Look_For_Same_Span
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State)
      is
         pragma Unreferenced (State);
      begin
         if Element_Span (Element) = Original_Span then
            Elem_Result := Element;
            Control := Terminate_Immediately;
         end if;
      end Look_For_Same_Span;

   begin
      loop
         if Element_Kind (Elem) = A_Declaration then
            if Declaration_Kind (Elem) = A_Package_Instantiation then
               Elem := Unrenamed_Generic_Unit (Elem);
               exit;
            end if;
         end if;
         Elem := Enclosing_Element (Elem);
      end loop;

      Get_Subp_From_Gen_Package (Elem, Control, Dummy_State);

      return Elem_Result;
   end Corresponding_Non_Generic_Subp;

   ----------------------------------
   -- Gather_Inherited_Subprograms --
   ----------------------------------

   procedure Gather_Inherited_Subprograms
     (Type_Def        : Asis.Element;
      Original_Type   : Asis.Element;
      Suite_Data_List : in out Suites_Data_Type)
   is
      ISubs : constant Declaration_List :=
        Implicit_Inherited_Subprograms (Type_Def);

      ISubp       : Asis.Element;
      Unit_Im     : String_Access;
      Unit_D      : String_Access;

      Owner_Decl  : Asis.Element;

      Test_Routine : GNATtest.Harness.Generator.Test_Routine_Info_Enhanced;
      Test_Routine_Wrapper : Test_Routine_Info_Enhanced_Wrapper;

      Tmp_Data        : Data_Holder;
      Tmp_Suites_Data : Suites_Data_Type;
      Tmp_Subp        : Subp_Info;
      Dummy_TR_Info   : Test_Routine_Info_Wrapper;
      Tmp_TR          : GNATtest.Harness.Generator.Test_Routine_Info;
      Dummy_Has_TC    : Boolean;

   begin

      --  Creating a stub for Subp_Info object
      Tmp_Subp.Nesting          := new String'("");
      Tmp_Subp.Subp_Text_Name   := new String'("");
      Tmp_Subp.Subp_Full_Hash   := new String'("");
      Tmp_Subp.Subp_Hash_V1     := new String'("");
      Tmp_Subp.Subp_Hash_V2_1   := new String'("");
      Tmp_Subp.Is_Abstract := False;

      for J in ISubs'Range loop

         ISubp := Corresponding_Declaration (ISubs (J));

         Unit_Im := new String'(To_String (Text_Name
           (Enclosing_Compilation_Unit (ISubp))));

         Unit_D := new String'
           (To_String (Unit_Full_Name (Enclosing_Compilation_Unit (ISubp))));

         if Is_Declared_In_Regular_Package (ISubp) then

            if Source_Present (Unit_Im.all) then

               Owner_Decl := Enclosing_Element (Primitive_Owner (ISubp));

               if Is_Callable_Subprogram (ISubp)
                 and then Test_Types_Linked (Owner_Decl, Original_Type)
                 and then
                   No_Inheritance_Through_Generics (Owner_Decl, Original_Type)
               then

                  --  Check if the inherited subprogram had Test_Cases. In that
                  --  case one test per Test_Case should be inherited.
                  Tmp_Data.Unit_File_Name := new
                    String'(Base_Name (Unit_Im.all));
                  Tmp_Subp.Subp_Declaration := ISubp;
                  Tmp_Subp.Subp_Text_Name :=
                    new String'(Get_Subp_Name (ISubp));
                  Tmp_Subp.Subp_Mangle_Name :=
                    new String'(Mangle_Hash (ISubp));
                  Tmp_Subp.Subp_Name_Image :=
                    new String'(To_String
                                (Defining_Name_Image (First_Name (ISubp))));
                  Gather_Test_Cases
                    (Tmp_Subp,
                     Dummy_TR_Info,
                     Tmp_Data,
                     Tmp_Suites_Data,
                     Dummy_Has_TC);

                  if Get_Nesting (ISubp) = Unit_D.all then
                     Test_Routine.TR_Rarent_Unit_Name := new String'
                       (Unit_D.all & "." &
                        To_String
                          (Defining_Name_Image (First_Name (Owner_Decl))) &
                        Test_Data_Unit_Name_Suff &
                        "." &
                        To_String
                          (Defining_Name_Image (First_Name (Owner_Decl))) &
                        Test_Unit_Name_Suff);

                     Test_Routine.Nesting := new String'
                       (Test_Routine.TR_Rarent_Unit_Name.all);

                  else
                     Test_Routine.TR_Rarent_Unit_Name := new String'
                       (Unit_D.all & "." &
                        Test_Data_Unit_Name & "." &
                        Test_Unit_Name & "." &
                        Nesting_Difference
                          (Get_Nesting (ISubp),
                           Unit_D.all) &
                        "." &
                        To_String
                          (Defining_Name_Image (First_Name (Original_Type))) &
                        Test_Data_Unit_Name_Suff &
                        "." &
                        To_String
                          (Defining_Name_Image (First_Name (Original_Type))) &
                        Test_Unit_Name_Suff);

                     Test_Routine.Nesting := new String'
                       (Test_Routine.TR_Rarent_Unit_Name.all);

                  end if;

                  if Get_Nesting (Original_Type) = Data.Unit_Full_Name.all then
                     Test_Routine_Wrapper.Test_Package := new String'
                       (Data.Unit_Full_Name.all &
                        "." &
                        To_String
                          (Defining_Name_Image (First_Name (Original_Type))) &
                        Test_Data_Unit_Name_Suff &
                        "." &
                        To_String
                          (Defining_Name_Image (First_Name (Original_Type))) &
                        Test_Unit_Name_Suff);

                  else
                     Test_Routine_Wrapper.Test_Package := new String'
                       (Data.Unit_Full_Name.all & "." &
                        Test_Data_Unit_Name & "." &
                        Test_Unit_Name & "." &
                        Nesting_Difference
                          (Get_Nesting (Original_Type),
                           Data.Unit_Full_Name.all) &
                        "." &
                        To_String
                          (Defining_Name_Image (First_Name (Original_Type))) &
                        Test_Data_Unit_Name_Suff &
                        "." &
                        To_String
                          (Defining_Name_Image (First_Name (Original_Type))) &
                        Test_Unit_Name_Suff);
                  end if;

                  --  Type is always the same, test_cases or not.
                  Test_Routine_Wrapper.Original_Type := Original_Type;

                  if not Tmp_Data.Subp_List.First_Element.Has_TC_Info then

                     --  There were no test_Cases, we just need to add the
                     --  single inherited test.

                     Test_Routine.TR_Text_Name   := new String'
                       (Mangle_Hash (ISubp));

                     --  adding sloc info
                     Test_Routine.Tested_Sloc := new String'
                       (Base_Name (Unit_Im.all)
                        & ":"
                        & Trim
                          (Integer'Image (First_Line_Number (ISubp)), Both)
                        & ":"
                        & Trim
                          (Integer'Image (First_Column_Number (ISubp)), Both)
                        & ": inherited at "
                        & Base_Name
                          (To_String
                               (Text_Name
                                    (Enclosing_Compilation_Unit
                                       (Original_Type))))
                        & ":"
                        & Trim
                          (Integer'Image (First_Line_Number (Original_Type)),
                           Both)
                        & ":"
                        & Trim
                          (Integer'Image (First_Column_Number (Original_Type)),
                           Both)
                        & ":");

                     Test_Routine_Wrapper.TR_Info := Test_Routine;

                     Suite_Data_List.ITR_List.Append (Test_Routine_Wrapper);
                  else

                     --  There were Test_Cases
                     for I in Tmp_Suites_Data.TR_List.First_Index ..
                       Tmp_Suites_Data.TR_List.Last_Index
                     loop
                        Tmp_TR := Tmp_Suites_Data.TR_List.Element (I).TR_Info;

                        Test_Routine.TR_Text_Name :=
                          new String'(Tmp_TR.TR_Text_Name.all);

                        --  adding sloc info
                        Test_Routine.Tested_Sloc := new String'
                          (Tmp_TR.Tested_Sloc.all
                           & " inherited at "
                           & Base_Name
                             (To_String
                                  (Text_Name
                                       (Enclosing_Compilation_Unit
                                          (Original_Type))))
                           & ":"
                           & Trim
                             (Integer'Image
                                  (First_Line_Number (Original_Type)),
                              Both)
                           & ":"
                           & Trim
                             (Integer'Image
                                  (First_Column_Number (Original_Type)),
                              Both)
                           & ":");

                        Test_Routine_Wrapper.TR_Info := Test_Routine;

                        Suite_Data_List.ITR_List.Append (Test_Routine_Wrapper);
                     end loop;

                  end if;
               end if;
            end if;
         end if;

         Tmp_Data.Subp_List.Clear;
         Tmp_Suites_Data.TR_List.Clear;

         Free (Unit_Im);
      end loop;
   end Gather_Inherited_Subprograms;

   -----------------------
   -- Gather_Test_Cases --
   -----------------------

   procedure Gather_Test_Cases
     (Subp            :        Subp_Info;
      TR_Info         :        Test_Routine_Info_Wrapper;
      Data            : in out Data_Holder;
      Suite_Data_List : in out Suites_Data_Type;
      TC_Found        :    out Boolean)
   is

      Me_TC : constant Trace_Handle :=
        Create ("Skeletons.Test_Cases", Default => Off);

      function Get_Aspect_List
        (Decl : Asis.Declaration) return Asis.Element_List;
      --  A wrapper that returns null if subprogram declaration is a renaming
      --  declaration and aspects specifications list otherwise.

      ---------------------
      -- Get_Aspect_List --
      ---------------------

      function Get_Aspect_List
        (Decl : Asis.Declaration) return Asis.Element_List
      is
      begin
         case Declaration_Kind (Decl) is
            when A_Function_Renaming_Declaration  |
                 A_Procedure_Renaming_Declaration =>
               return Asis.Nil_Element_List;

            when others =>
               return Aspect_Specifications (Decl);
         end case;
      end Get_Aspect_List;

      Aspect : Asis.Element;

      Subp_Add    : Subp_Info;
      TR_Info_Add : Test_Routine_Info_Wrapper;

      TC : Test_Case_Info;

      TC_Hash, Result_Value : String_Access;

      GT_Prefix : constant String := "Gnattest_";

      Params_To_Temp : String_Set.Set;

      Tmp : String_Access;

      type Old_Attr_Loc is record
         El_Span : Span;
         Is_Func_With_Params : Boolean;
         Number : String_Access;
      end record;

      package Source_Locations is new
        Ada.Containers.Indefinite_Vectors (Positive, Old_Attr_Loc);
      use Source_Locations;

      Old_Attr_Ref : Source_Locations.Vector;

      function Get_Condition_Image (Elem : Asis.Element) return String;
      --  Returns element image as a single line removing all double spaces

      function Get_Condition_Image
        (Lst : Asis_Element_List.List)
         return String;
      --  Returns "and"'ed element images in as a single line removing all
      --  double spaces.

      function Replace_Result_Attribute
        (Post   : String;
         F_Name : String;
         R_Name : String)
         return String;
      --  Replaces all entrances of function'Result in Post with R_Name

      function Replace_Old_Attribute (Elem : Asis.Element) return String;
      --  Replaces all entrances of <expr>'old in Post with
      --  Gnattest_<expr>'Old in Elem's image.

      procedure PC_Pre_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State);

      procedure Get_Postcondition_Image is new Traverse_Element
        (Pre_Operation     => PC_Pre_Operation,
         Post_Operation    => No_Op,
         State_Information => No_State);

      function Is_Function (Subp : Subp_Info) return Boolean;

      -------------------------
      -- Get_Condition_Image --
      -------------------------

      function Get_Condition_Image (Elem : Asis.Element) return String is

         Res, Tmp, Packed : String_Access;

         Idx   : Integer;
         Space : Boolean;

      begin
         Res := new String'(Replace_Old_Attribute (Elem));

         Tmp := new String'(Trim (Res.all, Both));
         Free (Res);
         Res := new String'(Tmp.all);
         Free (Tmp);

         Space := False;
         Packed := new String'("");
         Idx := Res'First;

         for I in Res'Range loop
            if Res (I) = ' ' then
               if not Space then
                  Space := True;
                  Tmp := new String'(Packed.all & " " & Res (Idx .. I - 1));
                  Free (Packed);
                  Packed := new String'(Tmp.all);
                  Free (Tmp);
               end if;

            else
               if Space then
                  Idx   := I;
                  Space := False;
               end if;
            end if;

            if I = Res'Last then
               Tmp := new String'(Packed.all & " " & Res (Idx .. I));
               Free (Packed);
               Packed := new String'(Tmp.all);
               Free (Tmp);
            end if;
         end loop;

         return Trim (Packed.all, Both);
      end Get_Condition_Image;

      function Get_Condition_Image
        (Lst : Asis_Element_List.List)
         return String
      is

         Res, Tmp, Packed : String_Access;

         Idx   : Integer;
         Space : Boolean;

         Cur : Asis_Element_List.Cursor;
      begin
         if Integer (Lst.Length) = 0 then
            return "True";
         end if;

         if Integer (Lst.Length) = 1 then
            return Get_Condition_Image (Lst.First_Element);
         end if;

         Res := new String'("((");

         Cur := Lst.First;

         loop
            if Cur = Asis_Element_List.No_Element then
               Tmp := new String'
                 (Res.all & "))");
               Free (Res);
               Res := new String'(Tmp.all);
               Free (Tmp);

               exit;
            end if;

            if Cur /= Lst.First then
               Tmp := new String'(Res.all & ") and (");
               Free (Res);
               Res := new String'(Tmp.all);
               Free (Tmp);
            end if;

            declare
               Image : constant Line_List :=
                 Lines (Asis_Element_List.Element (Cur));
            begin
               for I in Image'Range loop
                  Tmp := new String'
                    (Res.all & " " &
                     To_String (Non_Comment_Image (Image (I))));
                  Free (Res);
                  Res := new String'(Tmp.all);
                  Free (Tmp);
               end loop;
            end;

            Asis_Element_List.Next (Cur);
         end loop;

         Tmp := new String'(Trim (Res.all, Both));
         Free (Res);
         Res := new String'(Tmp.all);
         Free (Tmp);

         Space := False;
         Packed := new String'("");
         Idx := Res'First;
         for I in Res'Range loop
            if Res (I) = ' ' then
               if not Space then
                  Space := True;
                  Tmp := new String'(Packed.all & " " & Res (Idx .. I - 1));
                  Free (Packed);
                  Packed := new String'(Tmp.all);
                  Free (Tmp);
               end if;
            else
               if Space then
                  Idx   := I;
                  Space := False;
               end if;
            end if;

            if I = Res'Last then
               Tmp := new String'(Packed.all & " " & Res (Idx .. I));
               Free (Packed);
               Packed := new String'(Tmp.all);
               Free (Tmp);
            end if;
         end loop;

         return Trim (Packed.all, Both);

      end Get_Condition_Image;

      -----------------
      -- Is_Function --
      -----------------

      function Is_Function (Subp : Subp_Info) return Boolean is
      begin
         case Declaration_Kind (Subp.Subp_Declaration) is
            when A_Function_Declaration             |
                 An_Expression_Function_Declaration =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Function;

      ----------------------
      -- PC_Pre_Operation --
      ----------------------

      procedure PC_Pre_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State)
      is
         pragma Unreferenced (State, Control);

         Tmp : String_Access;

         Elem, Elem2 : Asis.Element;
         Func_Call   : String_Access;
         Elem_Span   : Old_Attr_Loc;
         Prefix_Call : Boolean;
         Number      : Positive := 1;

      begin
         if Element_Kind (Element) = An_Expression then
            if Expression_Kind (Element) = An_Attribute_Reference and then
              Attribute_Kind (Element) = An_Implementation_Defined_Attribute
            then
               Tmp := new String'
                 (To_String
                    (Name_Image (Attribute_Designator_Identifier (Element))));
               if To_Lower (Tmp.all) = "old" then
                  Elem_Span.El_Span := Element_Span (Element);
                  Elem_Span.Number := new String'
                    (Trim (Positive'Image (Number), Both));

                  Elem := Prefix (Element);
                  if
                    Expression_Kind (Elem) = A_Function_Call
                    and then Is_Prefix_Notation (Elem)
                  then
                     Prefix_Call := True;
                  else
                     Prefix_Call := False;
                  end if;
                  Func_Call := new String'
                    (Trim (To_String (Element_Image (Elem)), Both));

                  loop
                     case Expression_Kind (Elem) is
                        when An_Explicit_Dereference |
                             An_Attribute_Reference  |
                             A_Function_Call         |
                             An_Indexed_Component    |
                             A_Selected_Component    |
                             A_Slice                 =>

                           Elem := Prefix (Elem);

                        when others =>
                           exit;
                     end case;
                  end loop;

                  --  constructing temp variable assignments
                  Free (Tmp);
                  Elem2 := Corresponding_Name_Declaration (Elem);

                  if Declaration_Kind (Elem2) = A_Function_Declaration then
                     Elem2 := Result_Profile (Elem2);
                     Tmp := new String'
                       (GT_Prefix
                        & Trim (Positive'Image (Number), Both)
                        & "_"
                        & To_String (Name_Image (Elem))
                        & " : constant "
                        & Trim (To_String (Element_Image (Elem2)), Both)
                        & " := "
                        & Func_Call.all
                        & ";");
                     Free (Func_Call);

                     declare
                        Params_List : constant Asis.Element_List :=
                          Parameter_Profile
                            (Corresponding_Name_Declaration (Elem));
                     begin
                        if
                          Is_Nil (Params_List)
                          or else
                            (Params_List'Length = 1 and then Prefix_Call)
                        then
                           Elem_Span.Is_Func_With_Params := False;
                        else
                           Elem_Span.Is_Func_With_Params := True;
                        end if;
                     end;
                     Old_Attr_Ref.Append (Elem_Span);

                  else
                     Elem2 := Object_Declaration_View (Elem2);
                     Tmp := new String'
                       (GT_Prefix
                        & Trim (Positive'Image (Number), Both)
                        & "_"
                        & To_String (Name_Image (Elem))
                        & " : constant "
                        & Trim (To_String (Element_Image (Elem2)), Both)
                        & " := "
                        & To_String (Name_Image (Elem))
                        & ";");

                     Elem_Span.Is_Func_With_Params := False;
                     Old_Attr_Ref.Append (Elem_Span);
                  end if;
                  Number := Number + 1;

                  if Params_To_Temp.Find (Tmp.all) = String_Set.No_Element then
                     Params_To_Temp.Insert (Tmp.all);
                  end if;

               end if;
               Free (Tmp);
            end if;
         end if;
      end PC_Pre_Operation;

      ---------------------------
      -- Replace_Old_Attribute --
      ---------------------------

      function Replace_Old_Attribute (Elem : Asis.Element) return String is
         Elem_Span : constant Span := Element_Span (Elem);
         Cur_Span : Span;

         Prev_Line   : Line_Number_Positive        := Elem_Span.First_Line;
         Prev_Column : Character_Position_Positive := Elem_Span.First_Column;

         Result : String_Access := new String'("");
         Buff   : String_Access;

         Control : Traverse_Control := Continue;

         Dummy_State : No_State := Not_Used;

         function Process_Attr_Ref
           (S       : String;
            Is_Func : Boolean;
            Number : String) return String;

         function Is_Valid_Span (S : Span) return Boolean is
           (S.First_Line < S.Last_Line
            or else (S.First_Line = S.Last_Line
                     and then S.First_Column <= S.Last_Column));

         ----------------------
         -- Process_Attr_Ref --
         ----------------------

         function Process_Attr_Ref
           (S       : String;
            Is_Func : Boolean;
            Number : String) return String
         is
            Res  : String_Access := new String'("");
            Tmp  : String_Access;
            Idx  : Integer := S'First;
            Idx2 : Integer;

            Bracket_Counter : Integer := 0;
         begin
            for I in reverse S'Range loop
               if S (I) = '.' then
                  Idx := I + 1;
                  exit;
               end if;
            end loop;

            for I in Idx .. S'Last loop
               if S (I) = ''' then
                  Idx2 := I + 1;

                  loop
                     if S (Idx2) = ' ' then
                        Idx2 := Idx2 + 1;
                     else
                        exit;
                     end if;
                  end loop;

                  if Idx2 + 2 <= S'Last
                    and then To_Lower (S (Idx2 .. Idx2 + 2)) = "old"
                  then
                     Tmp := new String'(Res.all & S (Idx .. I - 1));
                     Free (Res);
                     Res := Tmp;
                     Tmp := null;

                     Idx := Idx2 + 3;
                  end if;
               end if;
            end loop;

            Tmp := new String'(Res.all & S (Idx .. S'Last));
            Free (Res);
            Res := Tmp;
            Tmp := null;

            if not Is_Func then
               return GT_Prefix & Number & "_" & Trim (Res.all, Both);
            end if;

            for I in Res'Range loop
               if Res (I) = '(' then
                  if Bracket_Counter = 0 then
                     Idx := I;
                  end if;
                  Bracket_Counter := Bracket_Counter + 1;
               end if;

               if Res (I) = ')' then
                  if Bracket_Counter = 0 then
                     Idx2 := I;
                     exit;
                  end if;
                  Bracket_Counter := Bracket_Counter - 1;
               end if;
            end loop;

            Tmp := new String'
              (Res.all (Res'First .. Idx - 1) &
               " "                            &
               Res.all (Idx2 + 1 .. Res'Last));

            return GT_Prefix & Number & "_" & Trim (Tmp.all, Both);
         end Process_Attr_Ref;

      begin
         Trace (Me_TC, "Replace_Old_Attribute");
         Increase_Indent (Me_TC);
         if Verbose then
            Trace (Me_TC, "called for: " & To_String (Debug_Image (Elem)));
         end if;
         if not Is_Nil (Elem) then
            Get_Postcondition_Image (Elem, Control, Dummy_State);
         end if;

         for K in Old_Attr_Ref.First_Index .. Old_Attr_Ref.Last_Index loop

            --  last unprocessed peice of image
            Cur_Span.First_Line   := Prev_Line;
            Cur_Span.First_Column := Prev_Column;
            if Old_Attr_Ref (K).El_Span.First_Column = 1 then
               Cur_Span.Last_Line    :=
                 Old_Attr_Ref (K).El_Span.First_Line - 1;
               Cur_Span.Last_Column  := 1;
            else
               Cur_Span.Last_Line    := Old_Attr_Ref (K).El_Span.First_Line;
               Cur_Span.Last_Column  :=
                 Old_Attr_Ref (K).El_Span.First_Column - 1;
            end if;

            Trace
              (Me_TC,
               "current span is "
               & Trim (Integer'Image (Cur_Span.First_Line), Both)
               & ":"
               & Trim (Integer'Image (Cur_Span.First_Column), Both)
               & "-"
               & Trim (Integer'Image (Cur_Span.Last_Line), Both)
               & ":"
               & Trim (Integer'Image (Cur_Span.Last_Column), Both));

            --  If the expresion begins with an 'Old reference right away
            --  then the first span will be empty.
            if Is_Valid_Span (Cur_Span) then
               declare
                  Image : constant Line_List := Lines (Elem, Cur_Span);
               begin
                  for I in Image'Range loop
                     Buff := new String'
                       (Result.all & " " &
                          To_String (Non_Comment_Image (Image (I))));
                     Free (Result);
                     Result := Buff;
                     Buff := null;
                  end loop;
               end;
            end if;

            --  current processed piece of image

            declare
               Image : constant Line_List :=
                 Lines (Elem, Old_Attr_Ref (K).El_Span);
               Tmp_Res : String_Access := new String'("");
            begin
               for I in Image'Range loop
                  Buff := new String'
                    (Tmp_Res.all & " " &
                     To_String (Non_Comment_Image (Image (I))));
                  Free (Tmp_Res);
                  Tmp_Res := Buff;
                  Buff := null;
               end loop;

               Buff := new String'
                 (Result.all &
                  Process_Attr_Ref
                    (Tmp_Res.all,
                     Old_Attr_Ref (K).Is_Func_With_Params,
                     Old_Attr_Ref (K).Number.all));
               Free (Tmp_Res);
               Free (Result);
               Result := Buff;
               Buff := null;

               if
                 Old_Attr_Ref (K).El_Span.Last_Column =
                 Length (Image (Old_Attr_Ref (K).El_Span.Last_Line))
               then
                  Prev_Line    := Old_Attr_Ref (K).El_Span.Last_Line + 1;
                  Prev_Column  := 1;
               else
                  Prev_Line    := Old_Attr_Ref (K).El_Span.Last_Line;
                  Prev_Column  := Old_Attr_Ref (K).El_Span.Last_Column + 1;
               end if;
            end;
         end loop;

         --  last unprocessed piece or the whole expression in case that no
         --  Old attribute present
         Cur_Span.First_Line   := Prev_Line;
         Cur_Span.First_Column := Prev_Column;
         Cur_Span.Last_Line    := Elem_Span.Last_Line;
         Cur_Span.Last_Column  := Elem_Span.Last_Column;

         if Cur_Span.First_Line /= Cur_Span.Last_Line
           or else Cur_Span.First_Column < Cur_Span.Last_Column
         then
            declare
               Image : constant Line_List := Lines (Elem, Cur_Span);
            begin
               for I in Image'Range loop
                  Buff := new String'
                    (Result.all & " " &
                     To_String (Non_Comment_Image (Image (I))));
                  Free (Result);
                  Result := Buff;
                  Buff := null;
               end loop;
            end;
         end if;

         Old_Attr_Ref.Clear;

         Decrease_Indent (Me_TC, "done replacing: " & Result.all);
         return Result.all;
      end Replace_Old_Attribute;

      ------------------------------
      -- Replace_Result_Attribute --
      ------------------------------

      function Replace_Result_Attribute
        (Post   : String;
         F_Name : String;
         R_Name : String)
         return String
      is
         Res : String_Access := new String'("");
         Tmp : String_Access;

         Quote : Boolean := False;

         Subp_Is_Operator : Boolean := False;
         Trying_Quote     : Boolean := False;

         F_Name_Length : constant Integer := F_Name'Length + 7;
         Idx           :          Integer := Post'First;

      begin
         if F_Name (F_Name'First) = '"' then
            Subp_Is_Operator := True;
         end if;

         for I in Post'Range loop
            if Post (I) = '"' then
               if Quote then
                  if I = Post'Last or else Post (I + 1) /= '"' then
                     Quote := False;
                  end if;

               else
                  Quote := True;
                  if Subp_Is_Operator then
                     Trying_Quote := True;
                  end if;
               end if;
            end if;

            if not Quote or else Trying_Quote then
               Trying_Quote := False;

               if Post'Last >= I + F_Name_Length - 1 then
                  if
                    Post (I .. I + F_Name_Length - 1) = F_Name & "'Result"
                  then
                     Tmp := new String'
                       (Res.all             &
                        Post (Idx .. I - 1) &
                        R_Name);
                     Free (Res);
                     Res := new String'(Tmp.all);
                     Free (Tmp);
                     Idx := I + F_Name_Length;
                  end if;
               end if;

               if Post'Last >= I + F_Name_Length then
                  if Post (I .. I + F_Name_Length) = F_Name & "' Result"
                    or else Post (I .. I + F_Name_Length) = F_Name & " 'Result"
                  then
                     Tmp := new String'
                       (Res.all             &
                        Post (Idx .. I - 1) &
                        R_Name);
                     Free (Res);
                     Res := new String'(Tmp.all);
                     Free (Tmp);
                     Idx := I + F_Name_Length + 1;
                  end if;

               end if;

               if Post'Last >= I + F_Name_Length + 1 then
                  if
                    Post (I .. I + F_Name_Length + 1) = F_Name & " ' Result"
                  then
                     Tmp := new String'
                       (Res.all             &
                        Post (Idx .. I - 1) &
                        R_Name);
                     Free (Res);
                     Res := new String'(Tmp.all);
                     Free (Tmp);
                     Idx := I + F_Name_Length + 2;
                  end if;

               end if;

               if Post'Last = I then
                  Tmp := new String'(Res.all & Post (Idx .. I));
                  Free (Res);
                  Res := new String'(Tmp.all);
                  Free (Tmp);
               end if;
            end if;
         end loop;

         return Res.all;
      end Replace_Result_Attribute;

      Aspect_List : constant Asis.Element_List :=
        (if Is_Part_Of_Instance (Subp.Subp_Declaration) then
              Get_Aspect_List
           (Corresponding_Non_Generic_Subp (Subp.Subp_Declaration))
         else
            Get_Aspect_List (Subp.Subp_Declaration));

      Pragma_List : constant Asis.Pragma_Element_List :=
        (if Is_Part_Of_Instance (Subp.Subp_Declaration) then
              Corresponding_Pragmas
           (Corresponding_Non_Generic_Subp (Subp.Subp_Declaration))
         else
            Corresponding_Pragmas (Subp.Subp_Declaration));

   begin
      Trace (Me_TC, "Looking for test cases of " & Subp.Subp_Text_Name.all);

      TC_Found := False;

      if Pragma_List'Length = 0 and Aspect_List'Length = 0 then
         if not Test_Case_Only then
            Data.Subp_List.Append (Subp);
            if not Subp.Is_Abstract then
               Suite_Data_List.TR_List.Append (TR_Info);
            end if;
         end if;
         Trace (Me_TC, "No test case found for " & Subp.Subp_Text_Name.all);
         return;
      end if;

      declare
         TC_Found_Loc : Boolean := False;
      begin
         for I in Pragma_List'Range loop
            if
              To_Lower (To_String (Pragma_Name_Image (Pragma_List (I)))) =
              "test_case"
            then
               TC_Found_Loc := True;
               exit;
            end if;
         end loop;

         for I in Aspect_List'Range loop

            Aspect := Aspect_Mark (Aspect_List (I));

            if Attribute_Kind (Aspect) /= A_Class_Attribute
              and then To_Lower (To_String (Name_Image (Aspect))) = "test_case"
            then
               TC_Found_Loc := True;
               exit;
            end if;
         end loop;

         if not TC_Found_Loc then
            Trace (Me_TC, "No test case found for " & Subp.Subp_Text_Name.all);
            if not Test_Case_Only then
               Data.Subp_List.Append (Subp);
               if not Subp.Is_Abstract then
                  Suite_Data_List.TR_List.Append (TR_Info);
               end if;
            end if;
            return;
         end if;
      end;

      if Subp.Is_Abstract then
         if not Test_Case_Only then
            Data.Subp_List.Append (Subp);
         end if;
         Trace
           (Me_TC, Subp.Subp_Text_Name.all
            & " is abstract, test cases are not of interest");
         return;
      end if;

      --  At this point we are pretty sure that at least one Test_Case exists.
      TC_Found := True;
      Options.Has_Test_Cases := True;

      for I in Pragma_List'Range loop
         if
           To_Lower (To_String (Pragma_Name_Image (Pragma_List (I)))) =
           "pre"
         then
            declare
               Associations : constant Asis.Association_List :=
                 Pragma_Argument_Associations (Pragma_List (I));
               Pragma_Arg : Asis.Element :=
                 Associations (Associations'First);
            begin
               if Element_Kind (Pragma_Arg) = An_Association then
                  Pragma_Arg := Actual_Parameter (Pragma_Arg);
               end if;
               TC.Pre.Append (Pragma_Arg);
            end;
         end if;

         if
           To_Lower (To_String (Pragma_Name_Image (Pragma_List (I)))) =
           "post"
         then
            declare
               Associations : constant Asis.Association_List :=
                 Pragma_Argument_Associations (Pragma_List (I));
               Pragma_Arg : Asis.Element :=
                 Associations (Associations'First);
            begin
               if Element_Kind (Pragma_Arg) = An_Association then
                  Pragma_Arg := Actual_Parameter (Pragma_Arg);
               end if;
               TC.Post.Append (Pragma_Arg);
            end;
         end if;
      end loop;

      for I in Aspect_List'Range loop
         Aspect := Aspect_Mark (Aspect_List (I));

         if Attribute_Kind (Aspect) /= A_Class_Attribute
           and then To_Lower (To_String (Name_Image (Aspect))) = "pre"
         then
            declare
               Aspect_Exp : constant Asis.Expression :=
                 Aspect_Definition (Aspect_List (I));
            begin
               TC.Pre.Append (Aspect_Exp);
            end;
         end if;

         if Attribute_Kind (Aspect) /= A_Class_Attribute
           and then To_Lower (To_String (Name_Image (Aspect))) = "post"
         then
            declare
               Aspect_Exp : constant Asis.Expression :=
                 Aspect_Definition (Aspect_List (I));
            begin
               TC.Post.Append (Aspect_Exp);
            end;
         end if;
      end loop;

      for I in Aspect_List'Range loop
         Aspect := Aspect_Mark (Aspect_List (I));

         if Attribute_Kind (Aspect) /= A_Class_Attribute
           and then To_Lower (To_String (Name_Image (Aspect))) = "test_case"
         then
            Subp_Add.Has_TC_Info := True;
            TC.Elem := Aspect_List (I);

            Subp_Add.Subp_Declaration := Subp.Subp_Declaration;
            Subp_Add.Is_Abstract      := Subp.Is_Abstract;
            Subp_Add.Corresp_Type     := Subp.Corresp_Type;
            Subp_Add.Nesting          := new String'(Subp.Nesting.all);
            Subp_Add.Subp_Text_Name   := new String'(Subp.Subp_Text_Name.all);
            Subp_Add.Subp_Name_Image  := new String'(Subp.Subp_Name_Image.all);
            Subp_Add.Subp_Full_Hash   := new String'(Subp.Subp_Full_Hash.all);
            Subp_Add.Subp_Hash_V1     := new String'(Subp.Subp_Hash_V1.all);
            Subp_Add.Subp_Hash_V2_1   := new String'(Subp.Subp_Hash_V2_1.all);

            declare
               Aspect_Exp : constant Asis.Expression :=
                 Aspect_Definition (Aspect_List (I));
               Associations : constant Asis.Association_List :=
                 Record_Component_Associations (Aspect_Exp);
               Idx : constant Integer := Associations'First;
            begin
               TC.Req := Asis.Nil_Element;
               TC.Ens := Asis.Nil_Element;

               --  setting up test case name

               TC.Name := new String'(To_String (Static_Expression_Value_Image
                 (Component_Expression (Associations (Idx)))));

               --  setting up test mode

               if
                 To_Lower
                   (To_String
                      (Name_Image
                         (Component_Expression
                            (Associations (Idx + 1))))) = "nominal"
               then
                  TC.Mode := Normal;
               else
                  TC.Mode := Robustness;
               end if;

               --  setting up requires and ensures

               if not (Associations'Length = 2) then
                  declare
                     Choices : constant Asis.Expression_List :=
                       Record_Component_Choices (Associations (Idx + 2));
                     Idx2 : constant Integer := Choices'First;
                  begin
                     if
                       To_Lower (To_String (Name_Image (Choices (Idx2)))) =
                       "requires"
                     then
                        TC.Req :=
                          Component_Expression (Associations (Idx + 2));
                        if Associations'Length > 3 then
                           TC.Ens := Component_Expression
                             (Associations (Idx + 3));
                        end if;

                     else
                        TC.Ens :=
                          Component_Expression (Associations (Idx + 2));
                     end if;
                  end;
               end if;
            end;

            --  creating second part of hash code for test routine name
            if TC.Mode = Normal then
               Tmp := new String'
                 (GNAT.SHA1.Digest
                    (TC.Name.all                   &
                     "#"                           &
                     Get_Condition_Image (TC.Pre)  &
                     "#"                           &
                     Get_Condition_Image (TC.Post) &
                     "#"                           &
                     Get_Condition_Image (TC.Req)  &
                     "#"                           &
                       Get_Condition_Image (TC.Ens)));
               TC_Hash := new String'(Tmp (Tmp'First .. Tmp'First + 15));

            else
               Tmp := new String'
                 (GNAT.SHA1.Digest
                    (TC.Name.all                  &
                     "#"                          &
                     Get_Condition_Image (TC.Req) &
                     "#"                          &
                       Get_Condition_Image (TC.Ens)));
               TC_Hash := new String'(Tmp (Tmp'First .. Tmp'First + 15));
            end if;

            if Is_Function (Subp) then
               Result_Value := new String'
                 (Subp.Subp_Mangle_Name.all                    &
                  "_"                                          &
                  TC_Hash (TC_Hash'First .. TC_Hash'First + 5) &
                  "_Result");
            end if;

            Subp_Add.Subp_Mangle_Name := new String'
              (Subp.Subp_Mangle_Name.all &
               "_"                       &
               TC_Hash (TC_Hash'First .. TC_Hash'First + 5));

            TC.TC_Hash := new String'(TC_Hash.all);

            --  creating condition images

            if TC.Mode = Normal then
               if Is_Nil (TC.Req) then
                  TC.Req_Image := new String'(Get_Condition_Image (TC.Pre));

               else
                  if TC.Pre.Is_Empty then
                     TC.Req_Image := new String'
                       (Get_Condition_Image (TC.Req));
                  else
                     TC.Req_Image := new String'
                       ("("                          &
                        Get_Condition_Image (TC.Pre) &
                        ") and ("                    &
                        Get_Condition_Image (TC.Req) &
                        ")");
                  end if;
               end if;

               if Is_Nil (TC.Ens) then
                  if Is_Function (Subp) then
                     TC.Ens_Image := new String'
                       (Replace_Result_Attribute
                          (Get_Condition_Image (TC.Post),
                           Subp.Subp_Name_Image.all,
                           Result_Value.all));
                  else
                     TC.Ens_Image := new String'
                       (Get_Condition_Image (TC.Post));
                  end if;

               else

                  if Is_Function (Subp) then
                     if TC.Post.Is_Empty then
                        TC.Ens_Image := new String'
                          (Replace_Result_Attribute
                             (Get_Condition_Image (TC.Ens),
                              Subp.Subp_Name_Image.all,
                              Result_Value.all));
                     else
                        TC.Ens_Image := new String'
                          (Replace_Result_Attribute
                             ("("                           &
                              Get_Condition_Image (TC.Post) &
                              ") and ("                     &
                              Get_Condition_Image (TC.Ens)  &
                              ")",
                              Subp.Subp_Name_Image.all,
                              Result_Value.all));
                     end if;

                  else
                     if TC.Post.Is_Empty then
                        TC.Ens_Image := new String'
                          (Get_Condition_Image (TC.Ens));
                     else
                        TC.Ens_Image := new String'
                          ("("                           &
                           Get_Condition_Image (TC.Post) &
                           ") and ("                     &
                           Get_Condition_Image (TC.Ens)  &
                           ")");
                     end if;
                  end if;
               end if;

            else
               if Is_Nil (TC.Req) then
                  TC.Req_Image := new String'("");
               else
                  TC.Req_Image := new String'(Get_Condition_Image (TC.Req));
               end if;

               if Is_Nil (TC.Ens) then
                  TC.Ens_Image := new String'("");

               else
                  if Is_Function (Subp) then
                     TC.Ens_Image := new String'
                       (Replace_Result_Attribute
                          (Get_Condition_Image (TC.Ens),
                           Subp.Subp_Name_Image.all,
                           Result_Value.all));

                  else
                     TC.Ens_Image := new String'
                       (Get_Condition_Image (TC.Ens));
                  end if;
               end if;
            end if;

            TC.Params_To_Temp := Params_To_Temp;
            Params_To_Temp.Clear;

            --  adding requiers and ensures slocs
            TC.Req_Line := new String'
              (Base_Name (Data.Unit_File_Name.all)
               & ":"
               & Trim (Integer'Image (First_Line_Number (TC.Req)), Both));
            TC.Ens_Line := new String'
              (Base_Name (Data.Unit_File_Name.all)
               & ":"
               & Trim (Integer'Image (First_Line_Number (TC.Ens)), Both)
               & ":");

            Subp_Add.TC_Info := TC;

            Data.Subp_List.Append (Subp_Add);

            if not Subp_Add.Is_Abstract then
               TR_Info_Add := TR_Info;
               TR_Info_Add.TR_Info.TR_Text_Name := new String'
                 (Subp_Add.Subp_Mangle_Name.all);

               --  changing tested sloc so it corresponds to test case instead
               --  of tested subprogram
               declare
                  TC_Span : constant Span := Element_Span (TC.Elem);
               begin
                  TR_Info_Add.TR_Info.Tested_Sloc := new String'
                    (Base_Name (Data.Unit_File_Name.all)
                     & ":"
                     & Trim (Integer'Image (TC_Span.First_Line), Both)
                     & ":"
                     & Trim (Integer'Image (TC_Span.First_Column), Both)
                     & ":");
               end;

               Suite_Data_List.TR_List.Append (TR_Info_Add);
            end if;
         end if;
      end loop;

      for I in Pragma_List'Range loop
         if
           To_Lower (To_String (Pragma_Name_Image (Pragma_List (I)))) =
           "test_case"
         then
            Subp_Add.Has_TC_Info := True;
            TC.Elem := Pragma_List (I);

            Subp_Add.Subp_Declaration := Subp.Subp_Declaration;
            Subp_Add.Is_Abstract      := Subp.Is_Abstract;
            Subp_Add.Corresp_Type     := Subp.Corresp_Type;
            Subp_Add.Nesting          := new String'(Subp.Nesting.all);
            Subp_Add.Subp_Text_Name   := new String'(Subp.Subp_Text_Name.all);
            Subp_Add.Subp_Name_Image  := new String'(Subp.Subp_Name_Image.all);
            Subp_Add.Subp_Full_Hash   := new String'(Subp.Subp_Full_Hash.all);
            Subp_Add.Subp_Hash_V1     := new String'(Subp.Subp_Hash_V1.all);
            Subp_Add.Subp_Hash_V2_1   := new String'(Subp.Subp_Hash_V2_1.all);

            declare
               Associations : constant Asis.Association_List :=
                 Pragma_Argument_Associations (Pragma_List (I));
               Idx : constant Integer := Associations'First;

            begin
               TC.Req := Asis.Nil_Element;
               TC.Ens := Asis.Nil_Element;

               --  setting up test case name
               TC.Name := new String'(To_String (Static_Expression_Value_Image
                 (Actual_Parameter (Associations (Idx)))));

               --  setting up test mode
               if
                 To_Lower (Trim (To_String (Element_Image (Actual_Parameter
                   (Associations (Idx + 1)))), Both)) = "nominal"
               then
                  TC.Mode := Normal;
               else
                  TC.Mode := Robustness;
               end if;

               --  setting up requires and ensures
               if Associations'Length = 2 then
                  TC.Req := Asis.Nil_Element;
                  TC.Ens := Asis.Nil_Element;
               else
                  if
                    To_Lower (Trim (To_String (Element_Image (Formal_Parameter
                      (Associations (Idx + 2)))), Both)) = "requires"
                  then
                     TC.Req := Actual_Parameter (Associations (Idx + 2));
                     if Associations'Length > 3 then
                        TC.Ens := Actual_Parameter (Associations (Idx + 3));
                     end if;
                  else
                     TC.Ens := Actual_Parameter (Associations (Idx + 2));
                  end if;
               end if;
            end;

            --  creating second part of hash code for test routine name
            if TC.Mode = Normal then
               Tmp := new String'
                 (GNAT.SHA1.Digest
                    (TC.Name.all                   &
                     "#"                           &
                     Get_Condition_Image (TC.Pre)  &
                     "#"                           &
                     Get_Condition_Image (TC.Post) &
                     "#"                           &
                     Get_Condition_Image (TC.Req)  &
                     "#"                           &
                       Get_Condition_Image (TC.Ens)));
               TC_Hash := new String'(Tmp (Tmp'First .. Tmp'First + 15));

            else
               Tmp := new String'
                 (GNAT.SHA1.Digest
                    (TC.Name.all                  &
                     "#"                          &
                     Get_Condition_Image (TC.Req) &
                     "#"                          &
                       Get_Condition_Image (TC.Ens)));
               TC_Hash := new String'(Tmp (Tmp'First .. Tmp'First + 15));
            end if;

            if Is_Function (Subp) then
               Result_Value := new String'
                 (Subp.Subp_Mangle_Name.all                    &
                  "_"                                          &
                  TC_Hash (TC_Hash'First .. TC_Hash'First + 5) &
                  "_Result");
            end if;

            Subp_Add.Subp_Mangle_Name := new String'
              (Subp.Subp_Mangle_Name.all &
               "_"                       &
                 TC_Hash (TC_Hash'First .. TC_Hash'First + 5));

            TC.TC_Hash := new String'(TC_Hash.all);

            --  creating condition images

            if TC.Mode = Normal then
               if Is_Nil (TC.Req) then
                  TC.Req_Image := new String'(Get_Condition_Image (TC.Pre));
               else
                  if TC.Pre.Is_Empty then
                     TC.Req_Image := new String'
                       (Get_Condition_Image (TC.Req));
                  else
                     TC.Req_Image := new String'
                       ("("                          &
                        Get_Condition_Image (TC.Pre) &
                        ") and ("                    &
                        Get_Condition_Image (TC.Req) &
                        ")");
                  end if;
               end if;

               if Is_Nil (TC.Ens) then
                  if Is_Function (Subp) then
                     TC.Ens_Image := new String'
                       (Replace_Result_Attribute
                          (Get_Condition_Image (TC.Post),
                           Subp.Subp_Name_Image.all,
                           Result_Value.all));
                  else
                     TC.Ens_Image := new String'
                       (Get_Condition_Image (TC.Post));
                  end if;

               else
                  if Is_Function (Subp) then

                     if TC.Post.Is_Empty then
                        TC.Ens_Image := new String'
                          (Replace_Result_Attribute
                             (Get_Condition_Image (TC.Ens),
                              Subp.Subp_Name_Image.all,
                              Result_Value.all));

                     else
                        TC.Ens_Image := new String'
                          (Replace_Result_Attribute
                             ("("                           &
                              Get_Condition_Image (TC.Post) &
                              ") and ("                     &
                              Get_Condition_Image (TC.Ens)  &
                              ")",
                              Subp.Subp_Name_Image.all,
                              Result_Value.all));
                     end if;

                  else
                     if TC.Post.Is_Empty then
                        TC.Ens_Image := new String'
                          (Get_Condition_Image (TC.Ens));

                     else
                        TC.Ens_Image := new String'
                          ("("                           &
                           Get_Condition_Image (TC.Post) &
                           ") and ("                     &
                           Get_Condition_Image (TC.Ens)  &
                           ")");
                     end if;
                  end if;
               end if;

            else
               if Is_Nil (TC.Req) then
                  TC.Req_Image := new String'("");
               else
                  TC.Req_Image := new String'(Get_Condition_Image (TC.Req));
               end if;

               if Is_Nil (TC.Ens) then
                  TC.Ens_Image := new String'("");

               else
                  if Is_Function (Subp) then
                     TC.Ens_Image := new String'
                       (Replace_Result_Attribute
                          (Get_Condition_Image (TC.Ens),
                           Subp.Subp_Name_Image.all,
                           Result_Value.all));
                  else
                     TC.Ens_Image := new String'(Get_Condition_Image (TC.Ens));
                  end if;
               end if;
            end if;

            TC.Params_To_Temp := Params_To_Temp;
            Params_To_Temp.Clear;

            --  adding requiers and ensures slocs
            TC.Req_Line := new String'
              (Base_Name (Data.Unit_File_Name.all)
               & ":"
               & Trim (Integer'Image (First_Line_Number (TC.Req)), Both)
               & ":");
            TC.Ens_Line := new String'
              (Base_Name (Data.Unit_File_Name.all)
               & ":"
               & Trim (Integer'Image (First_Line_Number (TC.Ens)), Both)
               & ":");

            Subp_Add.TC_Info := TC;

            Data.Subp_List.Append (Subp_Add);

            if not Subp_Add.Is_Abstract then
               TR_Info_Add := TR_Info;
               TR_Info_Add.TR_Info.TR_Text_Name := new String'
                 (Subp_Add.Subp_Mangle_Name.all);

               --  changing tested sloc so it corresponds to test case instead
               --  of tested subprogram
               declare
                  TC_Span : constant Span := Element_Span (TC.Elem);
               begin
                  TR_Info_Add.TR_Info.Tested_Sloc := new String'
                    (Base_Name (Data.Unit_File_Name.all)
                     & ":"
                     & Trim (Integer'Image (TC_Span.First_Line), Both)
                     & ":"
                     & Trim (Integer'Image (TC_Span.First_Column), Both)
                     & ":");
               end;

               Suite_Data_List.TR_List.Append (TR_Info_Add);
            end if;
         end if;
      end loop;

      TC.Pre.Clear;
      TC.Post.Clear;
   end Gather_Test_Cases;

   -----------------------
   -- Get_Units_To_Stub --
   -----------------------

   procedure Get_Units_To_Stub is
      Body_Unit : constant Compilation_Unit :=
        Corresponding_Body (The_Unit);
      Parent_Unit : Compilation_Unit :=
        Corresponding_Parent_Declaration (The_Unit);

      procedure Iterate_Separates (CU : Asis.Compilation_Unit);

      procedure Iterate_Separates (CU : Asis.Compilation_Unit) is
         Subs : constant Asis.Compilation_Unit_List := Subunits (CU);
      begin
         for I in Subs'Range loop
            Add_Units_To_Stub (Subs (I));
            Iterate_Separates (Subs (I));
         end loop;
      end Iterate_Separates;
   begin
      Trace
        (Me,
         "units to stub for "
         & Base_Name (To_String (Text_Name (The_Unit))));
      Increase_Indent (Me);

      --  Gathering with clauses from spec
      Add_Units_To_Stub (The_Unit);

      if not Is_Nil (Body_Unit)
        and then Unit_Kind (Body_Unit) /= A_Nonexistent_Body
        and then not Is_Equal (Body_Unit, The_Unit)
      then
         --  Gathering with clauses from body and separates
         Add_Units_To_Stub (Body_Unit);
         Iterate_Separates (Body_Unit);
      end if;

      --  Gathering parent packages
      while not Is_Nil (Parent_Unit) loop
         if
           To_Lower (To_String (Unit_Full_Name (Parent_Unit))) /= "standard"
         then
            declare
               Withed_Spec_Image : constant String :=
                 Base_Name (To_String (Text_Name (Parent_Unit)));
            begin
               if Good_To_Stub (Unit_Declaration (Parent_Unit))
                 and then not Already_Stubbing.Contains (Withed_Spec_Image)
               then
                  Already_Stubbing.Include (Withed_Spec_Image);
                  Data.Units_To_Stub.Append (Unit_Declaration (Parent_Unit));
                  Trace (Me, Withed_Spec_Image);
               end if;
            end;
         end if;
         Parent_Unit := Corresponding_Parent_Declaration (Parent_Unit);

      end loop;

      Decrease_Indent (Me);
      Already_Stubbing.Clear;
   end Get_Units_To_Stub;

   ------------------
   -- Good_To_Stub --
   ------------------

   function Good_To_Stub (Pack : Asis.Declaration) return Boolean is
      Unit_To_Stub : constant Asis.Compilation_Unit :=
        Enclosing_Compilation_Unit (Pack);
      File_Name     : constant String :=
        Base_Name (To_String (Text_Name (Unit_To_Stub)));
      Arg_File_Name : constant String :=
        Base_Name (To_String (Text_Name (The_Unit)));
   begin
      if not Source_Present (To_String (Text_Name (Unit_To_Stub))) then
         return False;
      end if;
      if Unit_Origin (Unit_To_Stub) /= An_Application_Unit then
         return False;
      end if;
      if Declaration_Kind (Pack) /= A_Package_Declaration then
         return False;
      end if;
      if Is_Equal (Enclosing_Compilation_Unit (Pack), The_Unit) then
         --  No self stubbing.
         return False;
      end if;
      if Is_RCI_Unit (Enclosing_Compilation_Unit (Pack)) then
         return False;
      end if;

      if Default_Stub_Exclusion_List.Contains (File_Name) then
         return False;
      end if;

      if Stub_Exclusion_Lists.Contains (Arg_File_Name) then
         if Stub_Exclusion_Lists.Element (Arg_File_Name).Contains (File_Name)
         then
            return False;
         end if;
      end if;
      return True;
   end Good_To_Stub;

   -------------------
   -- Is_Ghost_Code --
   -------------------

   function Is_Ghost_Code (Decl : Asis.Declaration) return Boolean is
      Aspects : constant Asis.Element_List := Aspect_Specifications (Decl);
   begin
      for I in Aspects'Range loop
         declare
            Aspect : constant Asis.Element := Aspect_Mark (Aspects (I));
         begin
            if
              Attribute_Kind (Aspect) /= A_Class_Attribute
              and then To_Lower (To_String (Name_Image (Aspect))) = "ghost"
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Is_Ghost_Code;

   ---------------------------
   --  First_Pre_Operation  --
   ---------------------------

   procedure First_Pre_Operation
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State)
   is

      Tmp_Element : Asis.Element;
      Cur_Element : Asis.Element := Element;

      Type_Data     : Base_Type_Info;
      Type_Doubling : Boolean := False;
      Test_Type     : GNATtest.Harness.Generator.Test_Type_Info;
      Test_Package  : String_Access;

      procedure Get_Type_Parent_Data (Type_Data : in out Base_Type_Info);
      --  Gathers data on parent type

      --------------------------
      -- Get_Type_Parent_Data --
      --------------------------

      procedure Get_Type_Parent_Data (Type_Data : in out Base_Type_Info) is
         Cur_Element : constant Asis.Element := Type_Data.Main_Type_Elem;

         Parent_Type_Check      : Asis.Element;
         Parent_Unit_Check      : Asis.Compilation_Unit;
         Parent_Unit_Check_Name : String_Access;

         function Is_Interface_Declaration
           (Arg : Asis.Declaration) return Boolean;
         --  Detects if Arg is a declaration of an interface

         procedure Set_No_Parent (Type_Data : in out Base_Type_Info);
         --  Sets all data relevant to parent type to null/false

         ------------------------------
         -- Is_Interface_Declaration --
         ------------------------------

         function Is_Interface_Declaration
           (Arg : Asis.Declaration) return Boolean
         is
         begin
            if Type_Kind (Type_Declaration_View (Arg)) =
              An_Interface_Type_Definition
            then
               return True;
            end if;

            return False;
         end Is_Interface_Declaration;

         -------------------
         -- Set_No_Parent --
         -------------------

         procedure Set_No_Parent (Type_Data : in out Base_Type_Info) is
         begin
            Type_Data.Argument_Father_Type_Name := null;
            Type_Data.Argument_Father_Nesting   := null;
            Type_Data.Argument_Father_Unit_Name := null;

            Type_Data.Has_Argument_Father       := False;
         end Set_No_Parent;

      begin

         if Stub_Mode_ON then
            Set_No_Parent (Type_Data);
            return;
         end if;

         if Inside_Gen or else Inside_Inst then
            Set_No_Parent (Type_Data);
            return;
         end if;

         if
           Type_Kind (Type_Declaration_View (Cur_Element)) =
           A_Tagged_Record_Type_Definition
         then
            --  No parent type at all
            Set_No_Parent (Type_Data);
            return;
         end if;

         Parent_Type_Check := Parent_Type_Declaration (Cur_Element);

         if Is_Interface_Declaration (Parent_Type_Check)
           or else Is_Fully_Private (Parent_Type_Check)
           or else not Is_Declared_In_Regular_Package (Parent_Type_Check)
         then
            --  Parent is an interface or fully private. No test package
            --  for parent type expected.
            Set_No_Parent (Type_Data);
            return;
         end if;

         Parent_Unit_Check :=
           Enclosing_Compilation_Unit
             (Parent_Type_Declaration (Cur_Element));

         Parent_Unit_Check_Name := new
           String'(To_String
                   (Text_Name (Parent_Unit_Check)));

         if not Source_Present (Parent_Unit_Check_Name.all) then
            --  The unit containing parent type declaration is not among
            --  argument packages. No test package for parent type expected.
            Set_No_Parent (Type_Data);
            return;
         end if;

         Type_Data.Argument_Father_Type_Name := new
           String'(To_String (Defining_Name_Image
             (First_Name (Parent_Type_Check))));

         Type_Data.Argument_Father_Nesting := new
           String'(Get_Nesting (Parent_Type_Check));

         Type_Data.Argument_Father_Unit_Name := new
           String'(To_String (Unit_Full_Name (Parent_Unit_Check)));

         Type_Data.Has_Argument_Father := True;
      end Get_Type_Parent_Data;

   begin
      if Is_Private (Element) then
         Control := Abandon_Siblings;
         return;
      end if;

      if Elements.Element_Kind (Cur_Element) = A_Declaration then

         if Is_Ghost_Code (Cur_Element) then
            Control := Abandon_Children;
            return;
         end if;

         case Declaration_Kind (Cur_Element) is
            when A_Generic_Package_Declaration =>

               if Stub_Mode_ON then
                  Control := Abandon_Children;
                  return;
               end if;

               if not Inside_Inst then
                  if not Package_Considered (Element) then
                     Control := Abandon_Children;
                  else
                     Inside_Gen := True;
                  end if;
               else
                  Control := Abandon_Children;
               end if;

            when A_Package_Instantiation =>

               if Stub_Mode_ON then
                  Control := Abandon_Children;
                  return;
               end if;

               if not Inside_Inst and then not Inside_Gen then

                  declare
                     Gen_Pack : constant Asis.Element :=
                       Unrenamed_Generic_Unit (Element);
                     Gen_Pack_CU : constant Asis.Compilation_Unit :=
                       Enclosing_Compilation_Unit (Gen_Pack);
                     File_Name : constant String :=
                       Normalize_Pathname
                         (Name  => To_String (Text_Name (Gen_Pack_CU)),
                          Resolve_Links  => False,
                          Case_Sensitive => False);
                  begin
                     if
                       Get_Nesting (Gen_Pack) = ""
                       and then not Is_Part_Of_Instance (Gen_Pack)
                       and then Source_Present (File_Name)
                     then
                        Inside_Inst := True;
                        Get_Records
                          (Corresponding_Declaration (Element),
                           Control,
                           State);
                        Inside_Inst := False;
                        Control := Continue;
                     end if;
                  end;
               end if;

            when An_Ordinary_Type_Declaration    |
                 A_Private_Type_Declaration      |
                 A_Private_Extension_Declaration =>

               Tmp_Element := Type_Declaration_View (Cur_Element);

               case Definition_Kind (Tmp_Element) is
               when A_Tagged_Private_Type_Definition |
                    A_Private_Extension_Definition =>

                  Cur_Element := Corresponding_Type_Completion (Cur_Element);
                  Tmp_Element := Type_Declaration_View (Cur_Element);

               when others =>
                  null;
               end case;

               case Type_Kind (Tmp_Element) is
               when A_Tagged_Record_Type_Definition |
                    A_Derived_Record_Extension_Definition =>

                  Type_Data.Main_Type_Elem := Cur_Element;

                  case Trait_Kind (Tmp_Element) is

                     when An_Abstract_Trait                 |
                          An_Abstract_Private_Trait         |
                          An_Abstract_Limited_Trait         |
                          An_Abstract_Limited_Private_Trait =>

                        Type_Data.Main_Type_Abstract := True;

                     when others =>
                        Type_Data.Main_Type_Abstract := False;
                        Data.Needs_Set_Up := True;
                  end case;

                  --  Checking if any of ancestor types had a discriminant part
                  Tmp_Element := Cur_Element;
                  Type_Data.No_Default_Discriminant := False;

                  loop

                     exit when Is_Nil (Tmp_Element);

                     declare
                        Comp : Asis.Element := Nil_Element;
                     begin
                        if Declaration_Kind (Tmp_Element) =
                          An_Ordinary_Type_Declaration
                        then
                           Comp :=
                             Corresponding_Type_Partial_View (Tmp_Element);
                        end if;

                        if not Is_Nil (Discriminant_Part (Tmp_Element)) or else
                          (not Is_Nil (Comp)
                           and then not Is_Nil (Discriminant_Part (Comp)))
                        then
                           Type_Data.No_Default_Discriminant := True;
                           exit;
                        end if;
                     end;

                     Tmp_Element := Parent_Type_Declaration (Tmp_Element);
                  end loop;

                  Check_Type_For_Elaboration (Cur_Element);

                  --  Gathering basic data about type
                  Type_Data.Main_Type_Text_Name := new
                    String'(To_String (Defining_Name_Image
                            (First_Name (Type_Data.Main_Type_Elem))));
                  Type_Data.Nesting := new String'(Get_Nesting (Cur_Element));

                  Get_Type_Parent_Data (Type_Data);
                  Data.Needs_Fixtures := True;

                  --  checking for doubled types.
                  --  should change to childreb abandoning for private parts
                  for I in Data.Type_Data_List.First_Index ..
                    Data.Type_Data_List.Last_Index
                  loop
                     if
                       Is_Equal
                         (Data.Type_Data_List.Element (I).Main_Type_Elem,
                          Cur_Element)
                     then
                        Type_Doubling := True;
                        exit;
                     end if;
                  end loop;

                  if not Type_Doubling then
                     Type_Data.Type_Number := Type_Counter;
                     Type_Counter          := Type_Counter + 1;

                     Data.Type_Data_List.Append (Type_Data);

                     --  Adding test type to suite data list
                     if
                       Type_Data.Nesting.all = Data.Unit_Full_Name.all
                     then
                        Test_Package := new String'
                          (Data.Unit_Full_Name.all           &
                             "."                               &
                             Type_Data.Main_Type_Text_Name.all &
                             Test_Data_Unit_Name_Suff          &
                             "."                               &
                             Type_Data.Main_Type_Text_Name.all &
                             Test_Unit_Name_Suff);

                     else
                        Test_Package := new String'
                          (Data.Unit_Full_Name.all           &
                             "." & Test_Data_Unit_Name & "."   &
                             Test_Unit_Name & "."              &
                             Nesting_Difference
                             (Type_Data.Nesting.all,
                              Data.Unit_Full_Name.all)       &
                             "."                               &
                             Type_Data.Main_Type_Text_Name.all &
                             Test_Data_Unit_Name_Suff          &
                             "."                               &
                             Type_Data.Main_Type_Text_Name.all &
                             Test_Unit_Name_Suff);
                     end if;

                     Test_Type.Test_Type := Asis.Nil_Element;
                     Test_Type.Test_Type_Name := new String'
                       ("Test_" &
                          Type_Data.Main_Type_Text_Name.all);
                     Test_Type.Nesting := new String'
                       (Test_Package.all);

                     if not Type_Data.Main_Type_Abstract then
                        Suite_Data_List.Test_Types.Append
                          ((TT_Info       => Test_Type,
                            Test_Package  => Test_Package,
                            Original_Type => Type_Data.Main_Type_Elem));
                     end if;
                  end if;

                  Control := Abandon_Children;

               when others =>
                  null;
               end case;

            when A_Variable_Declaration |
                 A_Constant_Declaration =>
               Control := Abandon_Children;

            when others => null;

         end case;
      end if;

   end First_Pre_Operation;

   ------------------------
   -- Package_Considered --
   ------------------------

   function Package_Considered (Pack : Asis.Declaration) return Boolean
   is
      Cur : Package_Info_List.Cursor;
      Name : String_Access;
   begin

      if Get_Nesting (Pack) = "" then
         Name := new String'
           (To_String (Defining_Name_Image (First_Name (Pack))));
      else
         Name := new String'
           (Get_Nesting (Pack) & "." &
              To_String (Defining_Name_Image (First_Name (Pack))));
      end if;

      Cur := Data.Package_Data_List.First;
      loop
         exit when Cur = Package_Info_List.No_Element;

         if Package_Info_List.Element (Cur).Name.all = Name.all then
            return True;
         end if;

         Package_Info_List.Next (Cur);
      end loop;

      return False;
   end Package_Considered;

   ------------------------------------
   --  Parent_Subtype_Unit_Original  --
   ------------------------------------

   function Parent_Subtype_Unit_Original
     (Type_Decl  : Asis.Element;
      Is_Generic : Boolean) return Asis.Compilation_Unit
   is
      Dec_Elem : Asis.Element :=
        Parent_Type_Declaration (Type_Decl);

      Old_Unit : constant Asis.Compilation_Unit :=
        Enclosing_Compilation_Unit (Type_Decl);

      New_Unit : constant Asis.Compilation_Unit :=
        Enclosing_Compilation_Unit (Dec_Elem);

   begin
      if not Is_Equal (New_Unit, Old_Unit) then
         return New_Unit;
      end if;

      --  If the unit is the same, that means that parent subtype is declared
      --  in the formal package declaration. We need to get the declaration of
      --  the corresponding generic package.

      Dec_Elem := Enclosing_Element (Dec_Elem);

      if Is_Generic then
         return
           Enclosing_Compilation_Unit
             (Corresponding_Generic_Package (Dec_Elem));
      else
         return New_Unit;
      end if;

   exception
      when Asis.Exceptions.ASIS_Inappropriate_Element =>
         return Asis.Nil_Compilation_Unit;
   end Parent_Subtype_Unit_Original;

   ----------------------------
   --  Second_Pre_Operation  --
   ----------------------------

   procedure Second_Pre_Operation
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State)
   is
      Subp       : Subp_Info;
      Owner_Def  : Asis.Element;
      Owner_Decl : Asis.Element;

      Type_Found           : Boolean;
      Test_Routine         : GNATtest.Harness.Generator.Test_Routine_Info;
      Test_Routine_Wrapper : Test_Routine_Info_Wrapper;
      Test_Package_Name    : String_Access;

      Original_Type : Asis.Element;

      Has_TC : Boolean;

      procedure Update_Name_Frequency (Subp_Name : String);

      ---------------------------
      -- Update_Name_Frequency --
      ---------------------------

      procedure Update_Name_Frequency (Subp_Name : String) is
         Cur : Name_Frequency.Cursor;
      begin
         Cur := Data.Subp_Name_Frequency.Find (To_Lower (Subp_Name));

         if Cur = Name_Frequency.No_Element then
            Data.Subp_Name_Frequency.Include (To_Lower (Subp_Name), 1);
         else
            Data.Subp_Name_Frequency.Replace_Element
              (Cur, (Name_Frequency.Element (Cur)) + 1);
         end if;

      end Update_Name_Frequency;

   begin

      if Element_Kind (Element) = A_Declaration then

         if Is_Ghost_Code (Element) then
            Control := Abandon_Children;
            return;
         end if;

         case Declaration_Kind (Element) is
            when An_Expression_Function_Declaration =>
               if not Is_Nil (Corresponding_Declaration (Element)) then
                  Control := Abandon_Children;
                  return;
               end if;

            when A_Procedure_Renaming_Declaration   |
                 A_Function_Renaming_Declaration    =>
               if Inside_Inst then
                  declare
                     Gen_Pack : constant Asis.Element :=
                       Enclosing_Element
                         (Corresponding_Generic_Element
                              (First_Name (Element)));
                  begin
                     case Declaration_Kind (Gen_Pack) is
                        when A_Formal_Function_Declaration  |
                             A_Formal_Procedure_Declaration =>
                           Control := Abandon_Children;
                           return;
                        when others =>
                           null;
                     end case;
                  end;

               end if;
               if not Is_Nil (Corresponding_Declaration (Element)) then
                  if
                    not Is_Equal (Corresponding_Declaration (Element), Element)
                  then
                     Control := Abandon_Children;
                     return;
                  end if;
               end if;

            when others =>
               null;
         end case;

         case Declaration_Kind (Element) is
            when A_Generic_Package_Declaration =>

               if Stub_Mode_ON then
                  Control := Abandon_Children;
                  return;
               end if;

               if not Inside_Inst then
                  if not Package_Considered (Element) then
                     Control := Abandon_Children;
                  else
                     Inside_Gen := True;
                  end if;
               else
                  Control := Abandon_Children;
               end if;

            when A_Protected_Type_Declaration   |
                 A_Single_Protected_Declaration =>
               Control := Abandon_Children;

            when A_Package_Instantiation =>

               if Stub_Mode_ON then
                  Control := Abandon_Children;
                  return;
               end if;

               if not Inside_Inst and then not Inside_Gen then

                  declare
                     Gen_Pack : constant Asis.Element :=
                       Unrenamed_Generic_Unit (Element);
                     Gen_Pack_CU : constant Asis.Compilation_Unit :=
                       Enclosing_Compilation_Unit (Gen_Pack);
                     File_Name : constant String :=
                       Normalize_Pathname
                         (Name  => To_String (Text_Name (Gen_Pack_CU)),
                          Resolve_Links  => False,
                          Case_Sensitive => False);
                  begin
                     if not Is_Part_Of_Instance (Gen_Pack)
                       and then Source_Present (File_Name)
                     then
                        Inside_Inst := True;
                        Inst_Elem := Element;
                        Get_Subprograms
                          (Corresponding_Declaration (Element),
                           Control,
                           State);
                        Inside_Inst := False;
                        Control := Continue;
                     end if;
                  end;
               end if;

            when A_Procedure_Declaration            |
                 A_Function_Declaration             |
                 A_Procedure_Renaming_Declaration   |
                 A_Function_Renaming_Declaration    |
                 A_Procedure_Instantiation          |
                 A_Function_Instantiation           |
                 An_Expression_Function_Declaration =>

               Subp.Subp_Declaration := Element;
               Subp.Subp_Text_Name   :=
                 new String'(Get_Subp_Name (Element));
               Subp.Subp_Name_Image   := new String'
                 (To_String (Defining_Name_Image (First_Name (Element))));
               Subp.Nesting := new String'(Get_Nesting (Element));

               case Trait_Kind (Element) is
                  when An_Abstract_Trait =>
                     Subp.Is_Abstract := True;
                  when others =>
                     Subp.Is_Abstract      := False;
                     Data.Needs_Assertions := True;
               end case;

               if
                 Is_Dispatching_Operation (Element)
                 and then Definition_Kind (Primitive_Owner (Element)) /=
                   A_Private_Type_Definition
               --  This is a special case for gnattest. Although the
               --  operation is dispatching, it is only so for the
               --  private part of child packages. So gnattest should
               --  treat it as a regular non-dispatching subprogram.
               then
                  Owner_Def  := Primitive_Owner (Element);
                  Owner_Decl := Enclosing_Element (Owner_Def);

                  case Definition_Kind (Owner_Def) is
                     when A_Private_Extension_Definition   |
                          A_Tagged_Private_Type_Definition =>
                        Owner_Decl :=
                          Corresponding_Type_Completion (Owner_Decl);
                     when others =>
                        null;
                  end case;

                  Type_Found := False;
                  for
                    I in Data.Type_Data_List.First_Index ..
                         Data.Type_Data_List.Last_Index
                  loop

                     if
                       Is_Equal
                         (Data.Type_Data_List.Element (I).Main_Type_Elem,
                          Owner_Decl)
                     then
                        Subp.Corresp_Type :=
                          Data.Type_Data_List.Element (I).Type_Number;
                        if Inside_Inst then
                           Subp.Subp_Mangle_Name := new String'
                             (Mangle_Hash
                                (Corresponding_Non_Generic_Subp (Element)));
                           Subp.Subp_Full_Hash := new String'
                             (Mangle_Hash_Full
                                (Corresponding_Non_Generic_Subp (Element)));
                           Subp.Subp_Hash_V1 := new String'
                             (Mangle_Hash_Full
                                (Corresponding_Non_Generic_Subp (Element),
                                 Case_Sensitive => True,
                                 N_Controlling => True));
                           Subp.Subp_Hash_V2_1 := new String'
                             (Mangle_Hash_Full
                                (Corresponding_Non_Generic_Subp (Element),
                                 N_Controlling => True));
                        else
                           Subp.Subp_Mangle_Name := new
                             String'(Mangle_Hash (Element));
                           Subp.Subp_Full_Hash := new
                             String'(Mangle_Hash_Full (Element));
                           Subp.Subp_Hash_V1 := new
                             String'(Mangle_Hash_Full (Element, True, True));
                           Subp.Subp_Hash_V2_1 := new
                             String'(Mangle_Hash_Full
                                     (Element,
                                        N_Controlling => True));
                        end if;
                        Type_Found := True;
                        exit;
                     end if;
                  end loop;

                  --  Setting suite info
                  if Type_Found then
                     Test_Routine.TR_Declaration := Asis.Nil_Element;
                     Test_Routine.TR_Text_Name := new String'
                       (Subp.Subp_Mangle_Name.all);
                     --  not setting test type number since it will be reser
                     --  during suite_data generation.
                     Original_Type := Owner_Decl;

                     --  Setting tested subprogram sloc for suite info
                     if Inside_Inst then
                        declare
                           Subp_Span : constant Span :=
                             Element_Span
                               (Corresponding_Non_Generic_Subp
                                    (Subp.Subp_Declaration));
                           Gen_CU : constant Asis.Compilation_Unit :=
                             Enclosing_Compilation_Unit
                               (Corresponding_Non_Generic_Subp
                                    (Subp.Subp_Declaration));
                           Inst_Span : constant Span :=
                             Element_Span (Inst_Elem);
                        begin
                           Test_Routine.Tested_Sloc := new String'
                             (Base_Name (To_String (Text_Name (Gen_CU)))
                              & ":"
                              & Trim
                                (Integer'Image (Subp_Span.First_Line),
                                 Both)
                              & ":"
                              & Trim
                                (Integer'Image (Subp_Span.First_Column), Both)
                              & " instance at "
                              & Base_Name (Data.Unit_File_Name.all)
                              & ":"
                              & Trim
                                (Integer'Image (Inst_Span.First_Line),
                                 Both)
                              & ":"
                              & Trim
                                (Integer'Image (Inst_Span.First_Column), Both)
                              & ":");
                        end;
                     else
                        declare
                           Subp_Span : constant Span :=
                             Element_Span (Subp.Subp_Declaration);
                        begin
                           Test_Routine.Tested_Sloc := new String'
                             (Base_Name (Data.Unit_File_Name.all) &
                                ":" &
                                Trim
                                (Integer'Image (Subp_Span.First_Line),
                                 Both) &
                                ":" &
                                Trim
                                (Integer'Image (Subp_Span.First_Column), Both)
                              & ":");
                        end;
                     end if;

                     if
                       Nesting_Difference
                         (Data.Unit_Full_Name.all, Subp.Nesting.all) = ""
                     then
                        Test_Package_Name := new String'
                          (Data.Unit_Full_Name.all         &
                           "."                             &
                           To_String (Defining_Name_Image
                             (First_Name (Original_Type))) &
                           Test_Data_Unit_Name_Suff & "."  &
                           To_String (Defining_Name_Image
                             (First_Name (Original_Type))) &
                           Test_Unit_Name_Suff);

                     else
                        Test_Package_Name := new String'
                          (Data.Unit_Full_Name.all & "."   &
                           Test_Data_Unit_Name & "."       &
                           Test_Unit_Name                  &
                           "."                             &
                           Nesting_Difference
                             (Data.Unit_Full_Name.all,
                              Subp.Nesting.all)            &
                           "."                             &
                           To_String (Defining_Name_Image
                             (First_Name (Original_Type))) &
                           Test_Data_Unit_Name_Suff & "."  &
                           To_String (Defining_Name_Image
                             (First_Name (Original_Type))) &
                           Test_Unit_Name_Suff);
                     end if;

                     Test_Routine.Nesting := new String'
                       (Test_Package_Name.all);
                  end if;

               else
                  if Is_Dispatching_Operation (Element) then
                     --  In such case the check for Elaboration controll
                     --  is not performed for the type in First_Pre_Operation
                     --  so we need to launch it here.
                     Owner_Def  := Primitive_Owner (Element);
                     Owner_Decl := Enclosing_Element (Owner_Def);

                     case Definition_Kind (Owner_Def) is
                     when A_Private_Extension_Definition   |
                          A_Tagged_Private_Type_Definition =>
                        Owner_Decl :=
                          Corresponding_Type_Completion (Owner_Decl);
                     when others =>
                        null;
                     end case;

                     Check_Type_For_Elaboration (Owner_Decl);
                  end if;

                  --  In simple case the type is always found, because in fact
                  --  we do not depend on it.

                  Type_Found            := True;
                  Subp.Corresp_Type     := 0;

                  if Inside_Inst then
                     Subp.Subp_Mangle_Name := new String'
                       (Mangle_Hash
                          (Corresponding_Non_Generic_Subp (Element)));
                     Subp.Subp_Full_Hash := new String'
                       (Mangle_Hash_Full
                          (Corresponding_Non_Generic_Subp (Element)));
                     Subp.Subp_Hash_V1 := new String'
                         (Mangle_Hash_Full
                            (Corresponding_Non_Generic_Subp (Element),
                             Case_Sensitive => True,
                             N_Controlling => True));
                     Subp.Subp_Hash_V2_1 := new String'
                         (Mangle_Hash_Full
                            (Corresponding_Non_Generic_Subp (Element),
                             N_Controlling => True));
                  else
                     Subp.Subp_Mangle_Name := new
                       String'(Mangle_Hash (Element));
                     Subp.Subp_Full_Hash := new
                       String'(Mangle_Hash_Full (Element));
                     Subp.Subp_Hash_V1 := new
                       String'(Mangle_Hash_Full (Element, True, True));
                     Subp.Subp_Hash_V2_1 := new
                       String'(Mangle_Hash_Full
                               (Element,
                                  N_Controlling => True));
                  end if;
                  Data.Has_Simple_Case  := True;
                  Data.Needs_Fixtures   := True;
                  Data.Needs_Set_Up     := True;
                  Data.Needs_Assertions := True;

                  --  Adding corresponding test routines for non-primitives to
                  --  the first element of suite data list.
                  if not Subp.Is_Abstract then
                     Test_Routine.TR_Declaration := Asis.Nil_Element;
                     Test_Routine.TR_Text_Name := new String'
                       (Subp.Subp_Mangle_Name.all);
                     Test_Routine.Test_Type_Numb := 1;

                     --  Setting tested subprogram sloc for suite info
                     if Inside_Inst then
                        declare
                           Subp_Span : constant Span :=
                             Element_Span
                               (Corresponding_Non_Generic_Subp
                                    (Subp.Subp_Declaration));
                           Gen_CU : constant Asis.Compilation_Unit :=
                             Enclosing_Compilation_Unit
                               (Corresponding_Non_Generic_Subp
                                    (Subp.Subp_Declaration));
                           Inst_Span : constant Span :=
                             Element_Span (Inst_Elem);
                        begin
                           Test_Routine.Tested_Sloc := new String'
                             (Base_Name (To_String (Text_Name (Gen_CU)))
                              & ":"
                              & Trim
                                (Integer'Image (Subp_Span.First_Line),
                                 Both)
                              & ":"
                              & Trim
                                (Integer'Image (Subp_Span.First_Column), Both)
                              & " instance at "
                              & Base_Name (Data.Unit_File_Name.all)
                              & ":"
                              & Trim
                                (Integer'Image (Inst_Span.First_Line),
                                 Both)
                              & ":"
                              & Trim
                                (Integer'Image (Inst_Span.First_Column), Both)
                              & ":");
                        end;
                     else
                        declare
                           Subp_Span : constant Span :=
                             Element_Span (Subp.Subp_Declaration);
                        begin
                           Test_Routine.Tested_Sloc := new String'
                             (Base_Name (Data.Unit_File_Name.all)
                              & ":"
                              & Trim
                                (Integer'Image (Subp_Span.First_Line),
                                 Both)
                              & ":"
                              & Trim
                                (Integer'Image (Subp_Span.First_Column), Both)
                              & ":");
                        end;
                     end if;

                     if
                       Nesting_Difference
                         (Data.Unit_Full_Name.all, Subp.Nesting.all) = ""
                     then
                        Test_Routine.Nesting := new String'
                          (Subp.Nesting.all & "." &
                           Test_Data_Unit_Name & "." &
                           Test_Unit_Name);

                     else
                        Test_Routine.Nesting := new String'
                          (Nesting_Common_Prefix
                             (Data.Unit_Full_Name.all, Subp.Nesting.all) &
                           "." & Test_Data_Unit_Name &
                           "." & Test_Unit_Name & "." &
                           Nesting_Difference
                             (Data.Unit_Full_Name.all, Subp.Nesting.all) &
                           "." & Test_Data_Unit_Name & "." & Test_Unit_Name);
                     end if;

                     Test_Package_Name := new String'
                       (Test_Routine.Nesting.all);
                     Original_Type := Asis.Nil_Element;
                  end if;
               end if;

               if not (Subp.Is_Abstract and not
                         Is_Dispatching_Operation (Element))
               then
                  if Type_Found then

                     Test_Routine_Wrapper :=
                       (TR_Info       => Test_Routine,
                        Test_Package  => Test_Package_Name,
                        Original_Type => Original_Type,
                        Original_Subp => Element);
                     Test_Package_Name := null;

                     Gather_Test_Cases
                       (Subp,
                        Test_Routine_Wrapper,
                        Data,
                        Suite_Data_List,
                        Has_TC);
                     if Has_TC or else not Test_Case_Only then
                        Update_Name_Frequency (Subp.Subp_Text_Name.all);
                     end if;
                  end if;
               end if;

               Control := Abandon_Children;

            when A_Variable_Declaration |
                 A_Constant_Declaration =>
               Control := Abandon_Children;

            when others =>
               null;
         end case;
      end if;
   end Second_Pre_Operation;

   -------------------------
   -- Third_Pre_Operation --
   -------------------------

   procedure Third_Pre_Operation
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State)
   is
      Package_Data : Package_Info;
   begin
      pragma Unreferenced (State);

      if Element_Kind (Element) = A_Declaration then

         if Is_Private (Element) then
            Control := Abandon_Siblings;
            return;
         end if;

         if Is_Ghost_Code (Element) then
            Control := Abandon_Children;
            return;
         end if;

         case Declaration_Kind (Element) is
            when A_Package_Declaration =>
               if Get_Nesting (Element) = "" then
                  Package_Data.Name := new String'
                    (To_String (Defining_Name_Image (First_Name (Element))));
               else
                  Package_Data.Name := new String'
                    (Get_Nesting (Element) & "." &
                     To_String (Defining_Name_Image (First_Name (Element))));
               end if;

               Package_Data.Is_Generic := False;
               Package_Data.Data_Kind := Declaration_Data;
               Package_Data.Element := Element;
               Data.Package_Data_List.Append (Package_Data);

            when A_Generic_Package_Declaration =>

               if Stub_Mode_ON then
                  Control := Abandon_Children;
                  return;
               end if;

               if
                 Is_Equal
                   (Element,
                    Unit_Declaration (Enclosing_Compilation_Unit (Element)))
               then
                  if Get_Nesting (Element) = "" then
                     Package_Data.Name := new String'
                       (To_String
                          (Defining_Name_Image (First_Name (Element))));
                  else
                     Package_Data.Name := new String'
                       (Get_Nesting (Element) & "."
                        & To_String
                          (Defining_Name_Image (First_Name (Element))));
                  end if;
                  Package_Data.Is_Generic := True;
                  Package_Data.Data_Kind := Declaration_Data;
                  Package_Data.Element := Element;
                  Data.Package_Data_List.Append (Package_Data);
               end if;
               --  Temporary stub to ignore nested generics
               Control := Abandon_Children;

            when A_Package_Instantiation =>

               if Stub_Mode_ON then
                  Control := Abandon_Children;
                  return;
               end if;

               --  check if corresponding generic is under test,
               --  if it is not nested in another generic and such
               Package_Data.Name := new String'
                 (Get_Nesting (Element) & "." &
                  To_String (Defining_Name_Image (First_Name (Element))));
               Package_Data.Data_Kind := Instantiation;
               Package_Data.Is_Generic := False;

               declare
                  Gen_Pack : constant Asis.Element :=
                    Unrenamed_Generic_Unit (Element);
                  Enclos   : constant Asis.Element :=
                    (Unit_Declaration (Enclosing_Compilation_Unit (Gen_Pack)));
               begin
                  --  no checks for nesting for now since we do not
                  --  deal with nested ones
                  if Is_Equal (Enclos, Gen_Pack) then
                     Package_Data.Generic_Containing_Package := new String'
                       (To_String (Defining_Name_Image (First_Name (Enclos))));
                     Package_Data.Element := Element;
                     Data.Package_Data_List.Append (Package_Data);
                     Control := Abandon_Children;
                  end if;
               end;

            when A_Variable_Declaration |
                 A_Constant_Declaration =>
               Control := Abandon_Children;

            when others =>
               null;
         end case;
      end if;
   end Third_Pre_Operation;

   ----------------------------
   -- Unrenamed_Generic_Unit --
   ----------------------------

   function Unrenamed_Generic_Unit
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Gen_Elem : constant Asis.Element :=
        Enclosing_Element
          (Corresponding_Name_Definition
             (Normalize_Reference
                (Generic_Unit_Name (Declaration))));
   begin
      case Declaration_Kind (Gen_Elem) is
         when A_Generic_Package_Renaming_Declaration   |
              A_Generic_Procedure_Renaming_Declaration |
              A_Generic_Function_Renaming_Declaration  =>
            return Enclosing_Element
              (Corresponding_Name_Definition
                 (Normalize_Reference
                      (Corresponding_Base_Entity (Gen_Elem))));

         when others =>
            return Gen_Elem;
      end case;
   end Unrenamed_Generic_Unit;

   ---------------------------
   -- Unsupported_Unit_Kind --
   ---------------------------

   function Unsupported_Unit_Kind (The_Unit : Asis.Compilation_Unit)
                                   return String
   is
   begin
      case Unit_Kind (The_Unit) is
         when A_Procedure                  |
              A_Function                   |
              A_Generic_Procedure          |
              A_Generic_Function           |
              A_Procedure_Instance         |
              A_Function_Instance          |
              A_Function_Renaming          |
              A_Procedure_Renaming         |
              A_Generic_Procedure_Renaming |
              A_Generic_Function_Renaming  |
              A_Procedure_Body             |
              A_Function_Body              =>
            return "subprogram";

         when A_Package_Instance =>
            return "package instance";

         when A_Package_Renaming         |
              A_Generic_Package_Renaming =>
            return "package renaming";

         when A_Package_Body_Subunit       |
              A_Task_Body_Subunit          |
              A_Protected_Body_Subunit     |
              A_Procedure_Body_Subunit     |
              A_Function_Body_Subunit      =>
            return "body subunit";

         when others =>
            return "unknown";
      end case;
   end Unsupported_Unit_Kind;

begin
   --  Adding test type for test package of non-primitives.
   --  It can remain empty of subprograms.

   if Is_RCI_Unit (The_Unit) then
      Apropriate_Source := False;
      Report_Std
        (Unit_SF_Name &
         " warning: " &
         "no test skeletons generated for subprograms in a RCI package.");
      Set_Source_Status (Unit_SF_Name, Processed_In_Vain);
      return;
   end if;

   case Declaration_Kind (Unit_Declaration (The_Unit)) is

      when A_Package_Declaration =>
         Data.Is_Generic := False;

      when A_Generic_Package_Declaration =>
         Data.Is_Generic := True;

      when A_Package_Instantiation =>
         if Get_Source_Status (Unit_SF_Name) = Waiting then
            Set_Source_Status (Unit_SF_Name, Processed);
            Apropriate_Source := False;
            return;
         end if;

         Tmp_Element := Normalize_Reference
           (Generic_Unit_Name (Unit_Declaration (The_Unit)));

         Tmp_CU := Enclosing_Compilation_Unit
           (Corresponding_Name_Declaration (Tmp_Element));

         Gen_Unit_Name := new String'(To_String (Text_Name (Tmp_CU)));

         --  Checking if corresponding generic package is an argument package

         if not Source_Present (Gen_Unit_Name.all) then
            Set_Source_Status (Unit_SF_Name, Bad_Inheritance);
            Apropriate_Source := False;
            Free (Gen_Unit_Name);
            return;
         end if;

         if Get_Source_Status (Gen_Unit_Name.all) /= Processed then
            Set_Source_Status (Unit_SF_Name, Bad_Inheritance);
            Apropriate_Source := False;
            Free (Gen_Unit_Name);
            return;
         end if;

         Data :=
           (Data_Kind      => Instantiation,
            Unit           => The_Unit,
            Unit_Full_Name =>
               new String'(To_String (Unit_Full_Name (The_Unit))),
            Unit_File_Name =>
               new String'(Base_Name (To_String (Text_Name (The_Unit)))),
            Gen_Unit => Tmp_CU,
            Gen_Unit_Full_Name  =>
               new String'(To_String (Unit_Full_Name (Tmp_CU))),
            Gen_Unit_File_Name  =>
               new String'(Gen_Unit_Name.all));

         Free (Gen_Unit_Name);
         Apropriate_Source := True;
         return;

      when others =>
         Report_Std
           ("gnattest: "
            & Base_Name (To_String (Text_Name (The_Unit)))
            & " is an unsupported kind of unit ("
            & Unsupported_Unit_Kind (The_Unit)
            & ").");

         Apropriate_Source := False;
         Set_Source_Status (Unit_SF_Name, Bad_Content);
         return;
   end case;

   if not Stub_Mode_ON and then not Separate_Drivers then
      --  Checking for private units among parents

      Tmp_CU := The_Unit;
      loop
         exit when To_String (Unit_Full_Name (Tmp_CU)) = "Standard";
         if Unit_Class (Tmp_CU) = A_Private_Declaration then
            Report_Std
              ("gnattest: "                               &
                 To_String (Unit_Full_Name (The_Unit)) &
                 " is private or child of private; skipping");
            Set_Source_Status (Unit_SF_Name, Bad_Content);
            Apropriate_Source := False;

            return;
         end if;

         Tmp_CU := Corresponding_Parent_Declaration (Tmp_CU);
      end loop;
   end if;

   --  Checking for ghost units among parents
   Tmp_CU := The_Unit;
   loop
      exit when To_String (Unit_Full_Name (Tmp_CU)) = "Standard";
      if Is_Ghost_Code (Unit_Declaration (Tmp_CU)) then
         Set_Source_Status (Unit_SF_Name, Bad_Content);
         Apropriate_Source := False;

         return;
      end if;

      Tmp_CU := Corresponding_Parent_Declaration (Tmp_CU);
   end loop;

   Increase_Indent
     (Me,
      "processing " & To_String (Unit_Full_Name (The_Unit))
      &  " (" & Base_Name (To_String (Text_Name (The_Unit))) & ")");

   --  Checking for elaboration pragmas
   Check_For_Elaboration (The_Unit);

   --  Setting the list of packages to stub.
   if Stub_Mode_ON then
      Get_Units_To_Stub;
   end if;

   Data.Unit           := The_Unit;
   Data.Unit_Full_Name := new String'(To_String (Unit_Full_Name (The_Unit)));
   Data.Unit_File_Name :=
     new String'
       (Normalize_Pathname
          (Name  => To_String (Text_Name (The_Unit)),
           Resolve_Links  => False,
           Case_Sensitive => False));

   Trace (Me, "Gathering nested packages");
   Get_Nested_Packages (Unit_Declaration (The_Unit), Control, Dummy_State);

   declare
      Test_Type : Test_Type_Info_Wrapper;
      Pack_Cur  : Package_Info_List.Cursor;

      Test_Package : constant String :=
        Data.Unit_Full_Name.all & "." &
        Test_Data_Unit_Name & "." & Test_Unit_Name;

      Nest_Dif : String_Access;
   begin
      Test_Type.TT_Info.Test_Type      := Asis.Nil_Element;
      Test_Type.TT_Info.Test_Type_Name := new String'("Test");

      Pack_Cur := Data.Package_Data_List.First;

      loop
         exit when Pack_Cur = Package_Info_List.No_Element;

         Nest_Dif := new String'
           (Nesting_Difference
              (Package_Info_List.Element (Pack_Cur).Name.all,
               Data.Unit_Full_Name.all));

         if Nest_Dif.all = "" then
            Test_Type.TT_Info.Nesting := new String'(Test_Package);

         else
            Test_Type.TT_Info.Nesting := new String'
              (Test_Package & "." &
               Nesting_Difference
                 (Package_Info_List.Element (Pack_Cur).Name.all,
                  Data.Unit_Full_Name.all) &
               "." & Test_Data_Unit_Name & "." & Test_Unit_Name);
         end if;

         Free (Nest_Dif);
         Test_Type.Test_Package := new String'
           (Test_Type.TT_Info.Nesting.all);

         Suite_Data_List.Test_Types.Append (Test_Type);
         Dummy_Type_Counter := Dummy_Type_Counter + 1;

         Package_Info_List.Next (Pack_Cur);
      end loop;
   end;

   Inside_Gen := False;
   Trace (Me, "Gathering tagged records");
   Get_Records (Unit_Declaration (The_Unit), Control, Dummy_State);
   Inside_Gen := False;
   Trace (Me, "Gathering subprograms");
   Get_Subprograms (Unit_Declaration (The_Unit), Control, Dummy_State);
   Decrease_Indent (Me, "Traversings finished");

   --  The first type is an imitation for non-primitives, so there is no
   --  inherited routines for it.
   if Inheritance_To_Suite and then not Inside_Gen then
      for
        K in Suite_Data_List.Test_Types.First_Index + Dummy_Type_Counter ..
          Suite_Data_List.Test_Types.Last_Index
      loop
         declare
            Tmp_Elem_Dec : constant Asis.Element :=
              Suite_Data_List.Test_Types.Element (K).Original_Type;
            Tmp_Elem_Def : constant Asis.Element :=
              Type_Declaration_View (Tmp_Elem_Dec);
            Corresp_Def  :          Asis.Element;
            Corresp_Decl :          Asis.Element;

            Decl_Kind : Declaration_Kinds;
         begin
            if
              Type_Kind (Tmp_Elem_Def) = A_Derived_Record_Extension_Definition
            then

               Corresp_Decl := Corresponding_Type_Declaration (Tmp_Elem_Dec);
               Decl_Kind := Declaration_Kind (Corresp_Decl);

               if
                 Is_Nil (Corresp_Decl) or else
                 Decl_Kind = A_Private_Type_Declaration or else
                 Decl_Kind = An_Incomplete_Type_Declaration or else
                 Decl_Kind = A_Tagged_Incomplete_Type_Declaration
               then
                  Gather_Inherited_Subprograms
                    (Tmp_Elem_Def,
                     Tmp_Elem_Dec,
                     Suite_Data_List);
               else
                  Corresp_Def := Type_Declaration_View
                    (Corresponding_Type_Declaration (Tmp_Elem_Dec));
                  case Definition_Kind (Corresp_Def) is
                  when A_Tagged_Private_Type_Definition =>

                     Gather_Inherited_Subprograms
                       (Tmp_Elem_Def,
                        Tmp_Elem_Dec,
                        Suite_Data_List);

                  when others =>
                     Gather_Inherited_Subprograms
                       (Corresp_Def,
                        Tmp_Elem_Dec,
                        Suite_Data_List);
                  end case;
               end if;
            end if;
         end;
      end loop;
   end if;

   if Data.Type_Data_List.Is_Empty and Data.Subp_List.Is_Empty then
      Apropriate_Source := False;
      Set_Source_Status (Unit_SF_Name, Processed_In_Vain);
   else
      Apropriate_Source := True;
   end if;

   declare
      Cur      : Subp_Data_List.Cursor;
      Tmp_Subp : Subp_Info;
   begin
      Cur := Data.Subp_List.First;
      loop
         exit when Cur = Subp_Data_List.No_Element;

         Tmp_Subp := Subp_Data_List.Element (Cur);

         if
           Data.Subp_Name_Frequency.Element
             (To_Lower (Tmp_Subp.Subp_Text_Name.all)) > 1
         then
            Tmp_Subp.Is_Overloaded := True;
         else
            Tmp_Subp.Is_Overloaded := False;
         end if;

         Data.Subp_List.Replace_Element (Cur, Tmp_Subp);

         Subp_Data_List.Next (Cur);
      end loop;
   end;
end Gather_Data;
