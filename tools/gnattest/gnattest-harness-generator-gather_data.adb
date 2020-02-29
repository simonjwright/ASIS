------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--           G N A T T E S T . H A R N E S S . G E N E R A T O R            --
--                                                                          --
--                                 B o d y                                  --
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
with GNATtest.Mapping; use GNATtest.Mapping;
separate (GNATtest.Harness.Generator)
procedure Gather_Data
  (The_Unit          : Asis.Compilation_Unit;
   Data              : in out Data_Holder;
   Apropriate_Source : out Boolean) is

   Local_TP_Mapping : User_Test_Package;

   Number_Of_Test_Types : Natural := 0;

   Processed_Unit : constant Asis.Compilation_Unit := The_Unit;

   Unit_SF_Name : String_Access;
   --  Stores the full name of the file containing the unit.

   Control : Traverse_Control := Continue;

   Tmp_CU        : Asis.Compilation_Unit;
   Tmp_Element   : Asis.Element;
   Gen_Unit_Name : String_Access;

   --------------------------
   --  Inner Subprogramms  --
   --------------------------

   Dummy_State : No_State := Not_Used;

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

   procedure Gather_Inherited_Subprograms
     (Type_Decl : Asis.Element;
      Data      : in out Data_Holder);
   --  Appends to the ISubp_List all the inherited subprograms implicitly
   --  declared for the given test type.

   function Parent_Subtype_Unit_Original
     (Type_Decl  : Asis.Element;
      Is_Generic : Boolean) return Asis.Compilation_Unit;
   --  Equivalent to Enclosing_Compilation_Unit (Corresponding_Parent_Subtype)
   --  for most cases. In case of a generic tested package tries to treat
   --  parent declaration as if it was declared in a formal package.

   function Test_Routine_Owner_Type (Elem : Asis.Element) return Asis.Element;
   --  Returns the type definition of the test type of the given test routine.

   function Get_Nesting (Elem : Asis.Element) return String;
   --  Returns the full package prefix if the element.

   -----------------
   -- Get_Nesting --
   -----------------

   function Get_Nesting (Elem : Asis.Element) return String is
      Res  : String_Access := new String'("");
      Buff : String_Access;

      Enclosing : Asis.Element;
   begin

      Enclosing := Enclosing_Element (Elem);

      loop

         exit when Is_Nil (Enclosing);

         if Res.all = "" then
            Free (Res);
            Res := new String'
              (To_String (Defining_Name_Image
               (First_Name (Enclosing))));
         else
            Buff :=
              new String'(To_String (Defining_Name_Image
                (First_Name (Enclosing))) &
                "." & Res.all);
            Free (Res);
            Res := new String'(Buff.all);
            Free (Buff);
         end if;

         Enclosing := Enclosing_Element (Enclosing);

      end loop;

      return Res.all;

   end Get_Nesting;

   ---------------------------
   --  First_Pre_Operation  --
   ---------------------------

   procedure First_Pre_Operation
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State) is
      pragma Unreferenced (State);

      Tmp_Element : Asis.Element;

      Type_Info : Test_Type_Info;

      Test_Case : Test_Case_Info;

   begin

      if Elements.Element_Kind (Element) = A_Declaration then

         if
           Elements.Declaration_Kind (Element) = A_Generic_Package_Declaration
         then
            if Get_Nesting (Element) /= "" then
               Control := Abandon_Children;
            end if;
         end if;

         if Elements.Declaration_Kind (Element) =
           An_Ordinary_Type_Declaration
         then
            Tmp_Element := Type_Declaration_View (Element);

            case Type_Kind (Tmp_Element) is

               when A_Derived_Record_Extension_Definition =>

                  case  Trait_Kind (Type_Declaration_View (Element)) is

                     when An_Abstract_Trait                 |
                          An_Abstract_Private_Trait         |
                          An_Abstract_Limited_Trait         |
                          An_Abstract_Limited_Private_Trait =>
                        null;

                     when others =>
                        if Is_AUnit_Part (Enclosing_Compilation_Unit
                                          (Root_Type_Declaration (Element)))
                        then

                           Number_Of_Test_Types := Number_Of_Test_Types + 1;

                           declare
                              N : constant Asis.Element :=
                                First_Name (Element);
                              N_Span : constant Asis.Text.Span :=
                                Element_Span (N);
                           begin
                              Free (Local_TP_Mapping.Type_Name);
                              Local_TP_Mapping.Type_Name := new String'
                                (To_String (Defining_Name_Image (N)));
                              Local_TP_Mapping.Type_Sloc :=
                                (N_Span.First_Line, N_Span.First_Column);
                           end;

                           if Is_Test_Case (Element) then

                              Test_Case.Name := new
                                String'(To_String (Defining_Name_Image
                                  (First_Name (Element))));
                              Test_Case.Nesting := new String'
                                (Get_Nesting (Element));

                              Data.TC_List.Append (Test_Case);
                           else

                              Type_Info.Test_Type := Element;
                              Type_Info.Test_Type_Name := new
                                String'(To_String (Defining_Name_Image
                                  (First_Name (Element))));
                              Type_Info.Nesting := new String'
                                (Get_Nesting (Element));

                              Data.Test_Types.Append (Type_Info);
                           end if;

                        end if;
                  end case;

                        Control := Abandon_Children;

               when others =>
                        Control := Abandon_Children;
            end case;

         end if;
      end if;

   end First_Pre_Operation;

   ------------------------------------
   --  Gather_Inherited_Subprograms  --
   ------------------------------------

   procedure Gather_Inherited_Subprograms
     (Type_Decl : Asis.Element;
      Data      : in out Data_Holder)
   is
      ISubs : constant Declaration_List :=
        Implicit_Inherited_Subprograms (Type_Declaration_View (Type_Decl));

      Test_Routine : Test_Routine_Info_Enhanced;

      ISubp       : Asis.Element;
      Unit_Im     : String_Access;
      Type_Number : Positive;

      Parent_Unit :          Asis.Compilation_Unit;
      Parent_File : String_Access;
   begin

      for I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index loop
         if
           Is_Equal
             (Data.Test_Types.Element (I).Test_Type,
              Type_Decl)
         then
            Type_Number := I;
            exit;
         end if;
      end loop;

      for J in ISubs'Range loop

         ISubp := Corresponding_Declaration (ISubs (J));
         Unit_Im := new String'(Base_Name (To_String (Text_Name
           (Enclosing_Compilation_Unit (ISubp)))));

         if
           Is_Test_Fixture_Routine (ISubp)
           and then Source_Present (Unit_Im.all)
         then

            Test_Routine.TR_Declaration := ISubp;

            Test_Routine.TR_Text_Name   :=
              new String'(Get_Subp_Name (ISubp));

            Test_Routine.Nesting        := new String'(Get_Nesting (ISubp));

            Parent_Unit := Enclosing_Compilation_Unit (ISubp);

            Test_Routine.TR_Parent_Unit_Decl := Parent_Unit;

            Test_Routine.TR_Rarent_Unit_Name :=
              new String'(To_String (Unit_Full_Name (Parent_Unit)));

            Parent_File := new String'
              (Base_Name (To_String (Text_Name (Parent_Unit))));

            Test_Routine.Test_Type_Numb := Type_Number;

            declare
               Subp_Span : constant Span :=
                 Element_Span (ISubp);

            begin
               Test_Routine.Tested_Sloc := new String'
                 (Parent_File.all
                  & ":"
                  & Trim
                    (Integer'Image (Subp_Span.First_Line),
                     Both)
                  & ":"
                  & Trim
                    (Integer'Image (Subp_Span.First_Column), Both)
                  & ": "
                  & Test_Routine.TR_Text_Name.all
                  & " inherited at "
                  & Base_Name
                       (To_String
                          (Text_Name
                             (Enclosing_Compilation_Unit (Type_Decl))))
                     & ":"
                     & Trim
                       (Integer'Image (First_Line_Number (Type_Decl)),
                        Both)
                     & ":"
                     & Trim
                       (Integer'Image (First_Column_Number (Type_Decl)),
                        Both)
                     & ":");
            end;

            Data.ITR_List.Append (Test_Routine);

         end if;

         Free (Unit_Im);

      end loop;

   end Gather_Inherited_Subprograms;

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
      Test_Routine : Test_Routine_Info;

      Owner_Def  : Asis.Element;
      Owner_Decl : Asis.Element;

      Type_Found : Boolean;

      Good_For_Suite : Boolean := False;

      Local_TR_Mapping : TR_Mapping;
   begin
      pragma Unreferenced (State);

      if Element_Kind (Element) = A_Declaration then

         if
           Elements.Declaration_Kind (Element) = A_Generic_Package_Declaration
         then
            if Get_Nesting (Element) /= "" then
               Control := Abandon_Children;
            end if;
         end if;

         case Declaration_Kind (Element) is

            when A_Procedure_Declaration =>
               --  Unlike in skeleton part, no functions here. A test routine
               --  cannot be a function.

               if Is_Test_Fixture_Routine (Element) then
                  Good_For_Suite := True;
               end if;

               declare
                  N : constant Asis.Element :=
                    First_Name (Element);
                  N_Span : constant Asis.Text.Span :=
                    Element_Span (N);
               begin
                  if Is_Test_Routine (Element) then
                     Local_TR_Mapping.TR_Name :=
                       new String'(Get_Subp_Name (Element));
                     Local_TR_Mapping.Line   := N_Span.First_Line;
                     Local_TR_Mapping.Column := N_Span.First_Column;

                     Local_TP_Mapping.TR_List.Append (Local_TR_Mapping);
                  elsif Is_Test_Related (Element) then
                     if Defining_Name_Image (N) = "Set_Up" then
                        Local_TP_Mapping.SetUp_Sloc :=
                          (N_Span.First_Line, N_Span.First_Column);
                     elsif Defining_Name_Image (N) = "Tear_Down" then
                        Local_TP_Mapping.TearDown_Sloc :=
                          (N_Span.First_Line, N_Span.First_Column);
                     end if;
                  end if;
               end;

               if Good_For_Suite then

                  Test_Routine.TR_Declaration := Element;
                  Test_Routine.TR_Text_Name   :=
                    new String'(Get_Subp_Name (Element));
                  Test_Routine.Nesting := new String'
                    (Get_Nesting (Element));

                  declare
                     Subp_Span : constant Span :=
                       Element_Span (Test_Routine.TR_Declaration);
                  begin
                     Test_Routine.Tested_Sloc := new String'
                       (Base_Name (Data.Test_Unit_File_Name.all) &
                          ":" &
                          Trim
                          (Integer'Image (Subp_Span.First_Line),
                           Both) &
                          ":" &
                          Trim
                          (Integer'Image (Subp_Span.First_Column), Both)
                        & ": "
                        & Test_Routine.TR_Text_Name.all
                        & ":");
                  end;

                  Owner_Def := Test_Routine_Owner_Type (Element);
                  Owner_Decl := Enclosing_Element (Owner_Def);

                  Type_Found := False;
                  for
                    I in Data.Test_Types.First_Index ..
                         Data.Test_Types.Last_Index
                  loop

                     if
                       Is_Equal
                         (Data.Test_Types.Element (I).Test_Type,
                          Owner_Decl)
                     then
                        Test_Routine.Test_Type_Numb := I;
                        Type_Found := True;
                        exit;
                     end if;

                  end loop;

                  if Type_Found then
                     Data.TR_List.Append (Test_Routine);
                  end if;

               end if;

               Control := Abandon_Children;

            when others =>
               null;

         end case;

      end if;

   end Second_Pre_Operation;

   -----------------------------
   -- Test_Routine_Owner_Type --
   -----------------------------

   function Test_Routine_Owner_Type (Elem : Asis.Element) return Asis.Element
   is
      Params     : constant Parameter_Specification_List :=
        Parameter_Profile (Elem);

      Param_Type : Asis.Element;
   begin
      if Is_Dispatching_Operation (Elem) then
         return Primitive_Owner (Elem);
      end if;

      Param_Type := Object_Declaration_View (Params (Params'First));

      Param_Type :=
        Corresponding_Name_Declaration (Normalize_Reference (Param_Type));

      return Type_Declaration_View (Param_Type);

   end Test_Routine_Owner_Type;

begin

   Unit_SF_Name :=
     new String'(Base_Name (To_String (Text_Name (Processed_Unit))));

   case Declaration_Kind (Unit_Declaration (Processed_Unit)) is

      when A_Package_Declaration         |
           A_Generic_Package_Declaration =>

         if Is_Body_Required (Processed_Unit) then

            --  CU must have a body.

            Tmp_Element := Corresponding_Body
              (Unit_Declaration (Processed_Unit));
            if not Is_Nil (Tmp_Element) then

               Tmp_CU := Enclosing_Compilation_Unit (Tmp_Element);

               if
                 Source_Present (Base_Name (To_String (Text_Name (Tmp_CU))))
               then

                  case Get_Source_Status
                    (Base_Name (To_String (Text_Name (Tmp_CU))))
                  is
                     when Processed         |
                          Processed_In_Vain =>

                        --  The body was already processed, no need to look
                        --  at the spec.

                        Set_Source_Status (Unit_SF_Name.all, Processed);
                        Apropriate_Source := False;
                        return;

                     when others =>

                        --  The body was not processed, marking it as
                        --  processed since we'll do everything here.
                        Set_Source_Status
                          (Base_Name (To_String (Text_Name (Tmp_CU))),
                           Processed);
                  end case;

               end if;

            end if;

         end if;

      when A_Package_Instantiation =>

         if Get_Source_Status (Unit_SF_Name.all) = Waiting then
            Set_Source_Status (Unit_SF_Name.all, Pending);
            Apropriate_Source := False;
            return;
         end if;

         Tmp_Element := Normalize_Reference
           (Generic_Unit_Name (Unit_Declaration (Processed_Unit)));

         Tmp_CU := Enclosing_Compilation_Unit
           (Corresponding_Name_Declaration (Tmp_Element));

         Gen_Unit_Name :=
           new String'(Base_Name (To_String (Text_Name (Tmp_CU))));

         --  Checking if corresponding generic package is an argument package.
         if
            not Source_Present (Gen_Unit_Name.all)
         then
            Set_Source_Status (Unit_SF_Name.all, Bad_Inheritance);
            Apropriate_Source := False;
            Free (Gen_Unit_Name);
            return;
         end if;

         if Get_Source_Status (Gen_Unit_Name.all) /= Processed then
            Set_Source_Status (Unit_SF_Name.all, Bad_Inheritance);
            Apropriate_Source := False;
            Free (Gen_Unit_Name);
            return;
         end if;

         Data :=
           (Data_Kind           => Instantination_Data,
            Test_Unit           => Processed_Unit,
            Test_Unit_Full_Name =>
               new String'(To_String (Unit_Full_Name (Processed_Unit))),
            Test_Unit_File_Name =>
               new String'(Base_Name (To_String (Text_Name (Processed_Unit)))),
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
           ("gnattest: "                                &
            Base_Name (To_String (Text_Name (Processed_Unit))) &
            " is an unsuported kind of unit.");
         Set_Source_Status (Unit_SF_Name.all, Bad_Content);
         Apropriate_Source := False;

         return;

   end case;

   case Declaration_Kind (Unit_Declaration (Processed_Unit)) is
      when A_Package_Declaration =>
         Data.Generic_Kind := False;

      when A_Generic_Package_Declaration =>
         Data.Generic_Kind := True;

      when others =>
         null;
   end case;

   --  Checking for private units among parents
   Tmp_CU := Processed_Unit;
   loop
      exit when To_String (Unit_Full_Name (Tmp_CU)) = "Standard";
      if Unit_Class (Tmp_CU) = A_Private_Declaration then
         Report_Std
           ("gnattest: "                               &
            To_String (Unit_Full_Name (Processed_Unit)) &
            " is private or child of private; skipping");
         Set_Source_Status (Unit_SF_Name.all, Bad_Content);
         Apropriate_Source := False;

         return;
      end if;

      Tmp_CU := Corresponding_Parent_Declaration (Tmp_CU);
   end loop;

   --  That's quite ulikely for AUnit itself to be included into the
   --  input files, yet still we have to check this.
   if Is_AUnit_Part (Processed_Unit) then

      Report_AUnit_Usage;
      raise Fatal_Error;

   end if;

   Data.Test_Unit := Processed_Unit;
   Data.Test_Unit_Full_Name :=
     new String'(To_String (Unit_Full_Name (Processed_Unit)));
   Data.Test_Unit_File_Name :=
     new String'(Base_Name (To_String (Text_Name (Processed_Unit))));

   Local_TP_Mapping.Name :=
     new String'(Base_Name (To_String (Text_Name (Processed_Unit))));

   Data.Good_For_Suite := False;
   Get_Records (Unit_Declaration (Processed_Unit), Control, Dummy_State);

   Control := Continue;
   Get_Subprograms (Unit_Declaration (Processed_Unit), Control, Dummy_State);

   for I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index loop
      Tmp_CU := Parent_Subtype_Unit_Original
        (Data.Test_Types.Element (I).Test_Type, Data.Generic_Kind);
      if not Is_AUnit_Part (Tmp_CU) then
         Tmp_Element := Data.Test_Types.Element (I).Test_Type;
         Gather_Inherited_Subprograms (Tmp_Element, Data);
      end if;
   end loop;

   if
     (not Data.TR_List.Is_Empty)  or
     (not Data.ITR_List.Is_Empty) or
     (not Data.TC_List.Is_Empty)
   then
      Data.Good_For_Suite := True;
      Set_Source_Status (Unit_SF_Name.all, Processed);
      if Number_Of_Test_Types = 1 then
         Additional_Mapping.Append (Local_TP_Mapping);
      else
         Report_Std
           ("warning: (gnattest) cannot create mapping for "
            & Data.Test_Unit_File_Name.all);
      end if;
   else
      Data.Good_For_Suite := False;
      Set_Source_Status (Unit_SF_Name.all, Processed_In_Vain);
   end if;

   Apropriate_Source := True;

end Gather_Data;
