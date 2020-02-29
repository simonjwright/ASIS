------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--  G N A T T E S T  . S T U B . G E N E R A T O R . G A T H E R _ D A T A  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2014-2017, AdaCore                     --
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

with Asis.Clauses; use Asis.Clauses;

separate (GNATtest.Stub.Generator)
procedure Gather_Data
  (The_Unit          :     Asis.Compilation_Unit;
   Data              : out Data_Holder)
is
   The_Spec : constant Asis.Declaration := Unit_Declaration (The_Unit);

   Spec_Base_File_Name : constant String :=
     Base_Name (To_String (Text_Name (The_Unit)));

   type Element_Node_State is record
      Tree      : Element_Node_Trees.Tree   := Element_Node_Trees.Empty_Tree;
      Cur       : Element_Node_Trees.Cursor;

      Flat_List : Element_Node_Lists.List := Element_Node_Lists.Empty_List;
   end record;

   EN_State : Element_Node_State;

   Generic_Layers_Counter : Natural := 0;
   --  All subprograms inside nested generic packages cannot have setters.
   --  This counter is used to know how many nested generic packages are
   --  enclosing current element.

   procedure Create_Element_Node
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Element_Node_State);
   --  When visiting an Element representing something for which a body
   --  sample may be required, we check if the body is really required
   --  and insert the corresponding Element on the right place in Data
   --  if it is.

   procedure Return_To_Ancestor
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Element_Node_State);
   --  Moves the cursor back to parent node.

   procedure Create_Body_Structure is new Traverse_Element
     (State_Information => Element_Node_State,
      Pre_Operation     => Create_Element_Node,
      Post_Operation    => Return_To_Ancestor);
   --  Creates Body_Structure by traversing an argument spec and choosing
   --  specs to create body samples for

   procedure Create_Element_Node
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Element_Node_State)
   is
      Elem_Node : Element_Node := Nil_Element_Node;

      Cur  : Element_Node_Trees.Cursor;

      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
   begin

      if Arg_Kind in A_Constant_Declaration | A_Variable_Declaration then
         --  Avoiding huge aggregates and long constant declarations.
         Control := Abandon_Children;
         return;
      end if;

      if Arg_Kind = A_Generic_Package_Declaration then
         Generic_Layers_Counter := Generic_Layers_Counter + 1;
      end if;

      if Arg_Kind in An_Ordinary_Type_Declaration .. A_Subtype_Declaration then
         --  Need to check if this type might pose some troubles from
         --  elaboration point of view.
         declare
            Aspects : constant Element_List :=
              Aspect_Specifications (Element);
            E : Asis.Element;
         begin
            for I in Aspects'Range loop
               E := Aspects (I);
               if To_Lower (To_String (Name_Image (Aspect_Mark (E)))) =
                 "type_invariant"
               then
                  Report_Std
                    ("warning: (gnattest) "
                     & Base_Name (Spec_Base_File_Name)
                     & ":"
                     & Trim
                       (Line_Number'Image (First_Line_Number (E)), Both)
                     & ":"
                     & Trim
                       (Line_Number'Image (First_Column_Number (E)), Both)
                     & ":"
                     & " type_invariant aspect");
                  Report_Std
                    ("this can cause circularity in the test harness", 1);
               end if;
            end loop;
         end;
      end if;

      if Requires_Body (Element) then
         Elem_Node.Spec := Element;
         if
           Arg_Kind in A_Function_Declaration | A_Procedure_Declaration
         then
            Elem_Node.Spec_Name := new String'(Get_Subp_Name (Element));
         else
            Elem_Node.Spec_Name := new String'
              (To_String (Defining_Name_Image (First_Name (Element))));
         end if;

         if Generic_Layers_Counter > 0 then
            Elem_Node.Inside_Generic := True;
         else
            Elem_Node.Inside_Generic := False;
         end if;

         if Is_Private (Element) then
            Elem_Node.Private_Part := True;
         else
            Elem_Node.Private_Part := False;
         end if;

         State.Tree.Insert_Child
           (State.Cur, Element_Node_Trees.No_Element, Elem_Node, Cur);

         if
           Arg_Kind in A_Function_Declaration | A_Procedure_Declaration
           and then Generic_Layers_Counter = 0
           and then Flat_Element_Kind
             (Enclosing_Element (Element)) /= A_Protected_Definition
         then
            State.Flat_List.Append (Elem_Node);
         end if;

         if
           Arg_Kind in A_Task_Type_Declaration | A_Single_Task_Declaration
         then
            Tasks_Present := True;
         end if;
      end if;

      if
        Arg_Kind = A_Package_Declaration          or else
        Arg_Kind = A_Generic_Package_Declaration  or else
        Arg_Kind = A_Single_Protected_Declaration or else
        Arg_Kind = A_Protected_Type_Declaration
      then
         State.Cur := Cur;
      end if;

   end Create_Element_Node;

   procedure Return_To_Ancestor
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Element_Node_State)
   is
      pragma Unreferenced (Control);
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
   begin
      if
        Arg_Kind = A_Package_Declaration          or else
        Arg_Kind = A_Generic_Package_Declaration  or else
        Arg_Kind = A_Single_Protected_Declaration or else
        Arg_Kind = A_Protected_Type_Declaration
      then
         State.Cur := Parent (State.Cur);
      end if;

      if Arg_Kind = A_Generic_Package_Declaration then
         Generic_Layers_Counter := Generic_Layers_Counter - 1;
      end if;
   end Return_To_Ancestor;

   Control : Traverse_Control := Continue;

   Spec_Clause_List : constant Asis.Context_Clause_List :=
     Context_Clause_Elements (The_Unit);
begin
   Trace (Me, "gathering data from " & To_String (Text_Name (The_Unit)));

   for I in Spec_Clause_List'Range loop
      if
        Clause_Kind (Spec_Clause_List (I)) = A_With_Clause
        and then Trait_Kind (Spec_Clause_List (I)) = A_Limited_Trait
      then
         declare
            With_Names : constant Asis.Name_List :=
              Clause_Names (Spec_Clause_List (I));
            First_Idx  : constant Integer := With_Names'First;
         begin
            Data.Limited_Withed_Units.Include
              (Trim
                 (To_String (Element_Image (With_Names (First_Idx))),
                  Both));
         end;
      end if;
   end loop;

   Tasks_Present := False;

   EN_State.Cur := EN_State.Tree.Root;
   Create_Body_Structure (The_Spec, Control, EN_State);
   Data.Elem_Tree := EN_State.Tree;
   Data.Flat_List := EN_State.Flat_List;

end Gather_Data;
