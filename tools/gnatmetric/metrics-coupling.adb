------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                     M E T R I C S . C O U P L I N G                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2008-2016, AdaCore                      --
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
-- GNAT Metrics Toolset is maintained by AdaCore (http://www.adacore.com).  --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

with GNAT.HTable;
with GNAT.OS_Lib;                      use GNAT.OS_Lib;

with Asis.Compilation_Units;           use Asis.Compilation_Units;
with Asis.Compilation_Units.Relations; use Asis.Compilation_Units.Relations;
with Asis.Declarations;                use Asis.Declarations;
with Asis.Elements;                    use Asis.Elements;
with Asis.Extensions;                  use Asis.Extensions;
with Asis.Extensions.Flat_Kinds;       use Asis.Extensions.Flat_Kinds;
with Asis.Extensions.Strings;          use Asis.Extensions.Strings;
with Asis.Iterator;                    use Asis.Iterator;
with Asis.Limited_Views;               use Asis.Limited_Views;
with Asis.Text;                        use Asis.Text;

with ASIS_UL.Common;                   use ASIS_UL.Common;
with ASIS_UL.Metrics.Definitions;      use ASIS_UL.Metrics.Definitions;
with ASIS_UL.Misc;                     use ASIS_UL.Misc;
with ASIS_UL.Output;                   use ASIS_UL.Output;
with ASIS_UL.Source_Table;             use ASIS_UL.Source_Table;

with METRICS.Options;                  use METRICS.Options;
with METRICS.Output;                   use METRICS.Output;

package body METRICS.Coupling is

   -----------------------------------------------
   -- Data structures for coupling dependencies --
   -----------------------------------------------

   type Unit_Id is new Integer range 0 .. Integer'Last;
   --  Index of a unit in the unit Table. We need IDs to organize dependency
   --  lists.

   No_Unit    : constant Unit_Id := Unit_Id'First;
   First_Unit : constant Unit_Id := No_Unit + 1;

   function Present (CU_Id : Unit_Id) return Boolean;
   function No      (CU_Id : Unit_Id) return Boolean;
   --  Check if CU_Id represents an existing entry in the unit table.

   subtype Existing_Unit_Id is Unit_Id range First_Unit .. Unit_Id'Last;

   package Dependency_Lists is new Ada.Containers.Ordered_Sets
     (Element_Type => Unit_Id);
   --  Lists to represent unit dependencies.

   type Unit_Record is record
      SF            : SF_Id;
      Unit_Name     : String_Loc;
      Unit_Span     : Span;

      Processed_Limited_View_Only : Boolean;

      Contains_Tagged_Types : Boolean;
      Defines_Subprograms   : Boolean;

      Descendants   : Dependency_Lists.Set;
      OO_Dependents : Dependency_Lists.Set;
      OO_Supporters : Dependency_Lists.Set;

      Subprogram_Dependents : Dependency_Lists.Set;
      Subprogram_Supporters : Dependency_Lists.Set;

      Dependents : Dependency_Lists.Set;
      Supporters : Dependency_Lists.Set;

      Hash_Link   : Unit_Id;
   end record;

   package Unit_Containers is new Ada.Containers.Vectors
      (Index_Type   => Existing_Unit_Id,
       Element_Type => Unit_Record);

   Unit_Table : Unit_Containers.Vector;

   ----------------
   -- Hash table --
   ----------------

   Hash_Num : constant Integer := 2**8;
   --  Number of headers in the hash table. There is no special reason in this
   --  choice.

   Hash_Max : constant Integer := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Integer range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of Unit_Id := (others => No_Unit);
   --  The hash table is used to locate existing entries in the unit table.
   --  The entries point to the first nodes table entry whose hash value
   --  matches the hash code. Then subsequent nodes table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   function Hash is new GNAT.HTable.Hash (Header_Num => Hash_Index_Type);
   function Hash (CU : Asis.Compilation_Unit) return Hash_Index_Type;

   procedure Print_Unit (U : Unit_Id);
   --  Prints out the debug info of the unit.

   procedure Print_List (List : Dependency_Lists.Set);
   --  Prints the debug image of the argument node list

   -----------------------------------------------
   -- Access and update routines for unit table --
   -----------------------------------------------

   type Unit_Record_Access is access Unit_Record;

   function Table (CU_Id : Unit_Id) return Unit_Record_Access;
   --  Mimics the notation Instantce_Name.Table (N) in the instantiation of the
   --  GNAT Table package. Returns the (pointer to the) unit with the index
   --  CU_Id from Unit_Table (see the body of the package). Raises
   --  Constraint_Error if a node with this index does not exists.

   function Unit_SF (CU_Id : Unit_Id) return SF_Id;
   --  Returns source file table entry for the argument unit. Returns No_SF_Id
   --  in case if No (CU_Id).

   function Unit_Name (CU_Id : Unit_Id) return String_Loc;
   --  Returns string table entry for the argument unit. Returns Nil_String_Loc
   --  in case if No (CU_Id).

   function Unit_Hash_Link (CU_Id : Unit_Id) return Unit_Id;
   --  Returns the hash link for the argument unit. Returns No_Unit in case
   --  if No (CU_Id).

   function Processed_Limited_View_Only (CU_Id : Unit_Id) return Boolean;
   procedure Set_Processed_Limited_View_Only (CU_Id : Unit_Id; Val : Boolean);

   procedure Set_Hash_Link (CU_Id : Unit_Id; Val : Unit_Id);
   --  Sets the hash link of the unit CU_Id to Val. It is erroneous to call it
   --  for No (CU_Id).

   procedure Add_Descendant           (To : Unit_Id; Descendant : Unit_Id);
   procedure Add_Dependent            (To : Unit_Id; Dependent  : Unit_Id);
   procedure Add_Supporter            (To : Unit_Id; Supporter  : Unit_Id);
   procedure Add_OO_Dependent         (To : Unit_Id; Dependent  : Unit_Id);
   procedure Add_OO_Supporter         (To : Unit_Id; Supporter  : Unit_Id);
   procedure Add_Subprogram_Dependent (To : Unit_Id; Dependent  : Unit_Id);
   procedure Add_Subprogram_Supporter (To : Unit_Id; Supporter  : Unit_Id);
   --  ????

   function Find_CU (CU : Asis.Compilation_Unit) return Unit_Id;
   --  Tries to locate in the unit table the entry corresponding to the
   --  argument unit. Returns No_Unit if there is no such entry.

   function Is_Equal
     (CU_Id : Unit_Id;
      CU    : Asis.Compilation_Unit)
      return  Boolean;
   --  Checks if CU_Id represents CU. CU_Id should not be No_Unit. This
   --  function assumes that Has_OO_Coupling_Metrics (CU) is true.

   -----------------------
   -- Local subprograms --
   -----------------------

   function Contains_Subprogram
     (CU   : Asis.Compilation_Unit)
      return Boolean;
   --  Checks if the argument CU is of interest for subprogram metrics. A unit
   --  is of interest if it satisfy to the following conditions:
   --
   --  *  it is a package declaration or a package instantiation that contains
   --     a subprogram declaration or subprogram instantiation
   --
   --  *  it is a subprogram (body) declaration
   --
   --  *  it is a subprogram instantiation

   function Has_OO_Coupling_Metrics
     (CU   : Asis.Compilation_Unit)
      return Boolean;
   --  Checks if the argument CU is of interest for coupling metrics. A unit is
   --  of interest if it satisfy to the following conditions:
   --
   --  * the corresponding library item is a package declaration, a generic
   --    package declaration or a package instantiation;
   --
   --  * the library item contains a declaration of a tagged type or an
   --    interface type (the corresponding expanded spec is considered in case
   --    of a generic instantion).
   --
   --  or
   --
   --  * the library item is a package body or a subunit, and the for the
   --    corresponding spec unit Has_Coupling_Metrics is True
   --
   --  What about things defined in nested generic packages???

   procedure Check_For_OO_Type
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  Used as Pre_Operation. Checks if its argument is a declaration (or
   --  a definition) of a tagged or interface type. If it is, sets State to
   --  True and terminates the traversal. Assumes that the top element of the
   --  traversal is a (generic) package declaration.

   procedure Check_For_Subprogram
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  Used as Pre_Operation. Checks if its argument is a subprogram
   --  declaration (or subprogram instantiation) or a package instantiation
   --  that defines a subprogram. If it is, sets State to True and terminates
   --  the traversal. Assumes that the top element of the traversal is a
   --  package declaration.

   procedure Do_Nothing
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  Used as a Post_Operation, does nothing.

   procedure Look_For_OO_Type is new Traverse_Element
     (State_Information => Boolean,
      Pre_Operation     => Check_For_OO_Type,
      Post_Operation    => Do_Nothing);
   --  Checks if the argument Element contains a declaration of OO type.

   procedure Look_For_Subprograms is new Traverse_Element
     (State_Information => Boolean,
      Pre_Operation     => Check_For_Subprogram,
      Post_Operation    => Do_Nothing);
   --  Checks if the argument Element contains a declaration of a subprogram.

   function Collect_Coupling_Dependencies
     (CU                : Asis.Compilation_Unit;
      From_Limited_View : Boolean := False)
      return              Unit_Id;
   --  Does the same as the procedure Collect_Coupling_Dependencies and returns
   --  the ID of the unit as the result.

   function Corresponding_Unit
     (CU   : Asis.Compilation_Unit)
      return Asis.Compilation_Unit;
   --  If the argument unit is a body or a subunit, returns the corresponding
   --  spec unit. Otherwise returns the argument Unit

   Ident_String : constant String := "   ";
   --  Used in the debug output of the unit table

   -----------------------
   -- Check_For_OO_Type --
   -----------------------

   procedure Check_For_OO_Type
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
   begin

      case Flat_Element_Kind (Element) is

         when An_Ordinary_Type_Declaration  |
              A_Private_Type_Declaration    |
              A_Package_Declaration         |
              A_Generic_Package_Declaration =>
            --  We have to look into the construct
            null;
         when A_Tagged_Incomplete_Type_Declaration              |
              A_Private_Extension_Declaration                   |
              A_Tagged_Record_Type_Definition                   |
              A_Tagged_Private_Type_Definition                  |
              A_Derived_Record_Extension_Definition             |
              An_Ordinary_Interface .. A_Synchronized_Interface =>
            --  We've found OO type, so:
            State := True;
            Control := Terminate_Immediately;

         when A_Package_Instantiation =>
            Look_For_OO_Type
              (Corresponding_Declaration (Element), Control, State);
         when others =>
            --  Definitely nothing interesting inside
            Control := Abandon_Children;
      end case;

   end Check_For_OO_Type;

   --------------------------
   -- Check_For_Subprogram --
   --------------------------

   procedure Check_For_Subprogram
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
   begin
      case Flat_Element_Kind (Element) is
         when A_Procedure_Declaration   |
              A_Function_Declaration    |
              A_Procedure_Instantiation |
              A_Function_Instantiation  =>
            State := True;
            Control := Terminate_Immediately;
         when A_Package_Instantiation =>
            Look_For_Subprograms
              (Corresponding_Declaration (Element), Control, State);
         when A_Package_Declaration =>
            null;
         when others =>
            Control := Abandon_Children;
      end case;
   end Check_For_Subprogram;

   -----------------------------------
   -- Collect_Coupling_Dependencies --
   -----------------------------------

   function Collect_Coupling_Dependencies
     (CU                : Asis.Compilation_Unit;
      From_Limited_View : Boolean := False)
      return              Unit_Id
   is
      Spec_CU  : constant Asis.Compilation_Unit := Corresponding_Unit (CU);
      Result   :          Unit_Id               := Find_CU (Spec_CU);
      New_Unit :          Boolean               := False;

      Arg_Contains_Tagged_Types : Boolean := False;
      Contains_Tagged_Types     : Boolean := False;
      Contains_Subprograms      : Boolean := False;

      Next_Parent    : Asis.Compilation_Unit;
      Next_Supporter : Unit_Id;

   begin
      --  First, check if we have to process this unit:

      if not From_Limited_View
        and then
         Compute_Control_Coupling_Metric
      then
         Contains_Subprograms := Contains_Subprogram (Spec_CU);
      end if;

      if Compute_OO_Coupling_Metric then
         Arg_Contains_Tagged_Types := Has_OO_Coupling_Metrics (Spec_CU);
      end if;

      if not (Compute_Unit_Coupling_Metric
             or else
              Compute_Control_Coupling_Metric
               --  We process a unit even if it does not define subprograms -
               --  it may depend on units with subprograms
             or else
              Arg_Contains_Tagged_Types)
      then
         return No_Unit;
      end if;

      if No (Result) then
         --  We have to create the corresponding entry in the unit table:
         declare
            New_Unit         : Unit_Record;
            Hash_Value       : constant Hash_Index_Type := Hash (Spec_CU);
            Last_In_Chain    :          Unit_Id := Hash_Table (Hash_Value);

         begin
            New_Unit.SF :=
              File_Find (Normalize_Pathname
                          (To_String (Text_Name (Spec_CU)),
                           Resolve_Links  => False,
                           Case_Sensitive => True),
                         Case_Sensitive => False);
            --  In non case-sensitive system we may have a problem trying to
            --  find a source file in the source table. Text_Name gets the file
            --  name from the tree, and in non case-sensitive systems the file
            --  names are always converted to lower case, so we have to use
            --  non case-sensitive search here.

            if not Present (New_Unit.SF) then
               --  Spec unit is not an argument of the gnatmetric call, we
               --  cannot computre any coupling metric for it!
               return No_Unit;
            end if;

            New_Unit.Processed_Limited_View_Only := From_Limited_View;
            New_Unit.Unit_Span := Element_Span (Unit_Declaration (Spec_CU));
            New_Unit.Unit_Name :=
              Enter_String (To_String (Unit_Full_Name (Spec_CU)));
            New_Unit.Hash_Link := No_Unit;
            New_Unit.Contains_Tagged_Types := Arg_Contains_Tagged_Types;
            New_Unit.Defines_Subprograms   := Contains_Subprograms;

            Unit_Containers.Append
              (Container => Unit_Table,
               New_Item  => New_Unit);

            Result := Unit_Containers.Last_Index (Unit_Table);

            if No (Last_In_Chain) then
               Hash_Table (Hash_Value) := Result;
            else

               while Present (Unit_Hash_Link (Last_In_Chain)) loop
                  Last_In_Chain := Unit_Hash_Link (Last_In_Chain);
               end loop;

               Set_Hash_Link
                 (CU_Id => Last_In_Chain,
                  Val   => Result);
            end if;

         end;

         New_Unit := True;

      elsif not From_Limited_View and then
            Processed_Limited_View_Only (Result)
      then
         --  Here we have to correct the unit information if all what we know
         --  about the unit has been collected from its limited view

--         if Compute_Control_Coupling_Metric then
--            Table (Result).Defines_Subprograms := Contains_Subprograms;
--         end if;

         Table (Result).Unit_Span := Element_Span (Unit_Declaration (Spec_CU));
--         New_Unit := True;
      end if;

      if New_Unit
       or else
         not Is_Equal (CU, Spec_CU)
       or else
         (not From_Limited_View
         and then
          Processed_Limited_View_Only (Result))
      then
         --  We have to create or to update the list of supporters and to
         --  update the dependents lists for new supporters accordingly

         declare

            Supporters : constant Asis.Compilation_Unit_List :=
               Semantic_Dependence_Order
                 (Compilation_Units => (1 => CU),
                  Dependent_Units   => Nil_Compilation_Unit_List,
                  The_Context       => The_Context,
                  Relation          => Asis.Supporters).Consistent;

            Next_Supporter    : Unit_Id;
            Next_Supporter_SF : SF_Id;
            Has_Limited_View  : Boolean;

         begin

            for J in Supporters'Range loop

               Has_Limited_View := Has_Limited_View_Only (Supporters (J));
               Next_Supporter   := Find_CU (Supporters (J));

               if not Has_Limited_View
                 and then
                  Compute_Control_Coupling_Metric
               then
                  Contains_Subprograms := Contains_Subprogram (Supporters (J));
               end if;

               if (not Has_Limited_View
                 or else
                   Unit_Kind (Supporters (J)) = A_Package)
                 and then
                   Compute_OO_Coupling_Metric
               then
                  Contains_Tagged_Types :=
                    Has_OO_Coupling_Metrics (Supporters (J));
               end if;

               if No (Next_Supporter) then
                  --  May be this is a unit of interest that has not been
                  --  processed yet.

                  if Compute_Unit_Coupling_Metric
                    or else
                     (Compute_Control_Coupling_Metric
                     and then
                      Contains_Subprograms)
                    or else
                     (Compute_OO_Coupling_Metric
                     and then
                      Contains_Tagged_Types)
                  then
                     Next_Supporter_SF :=
                       File_Find (GNAT.OS_Lib.Normalize_Pathname
                                  (To_String (Text_Name (Supporters (J))),
                                   Resolve_Links  => False,
                                   Case_Sensitive => True));
                  else
                     Next_Supporter_SF := No_SF_Id;
                  end if;

                  --  The only need for computing Next_Supporter_SF is to
                  --  ensure that coupling metrics are computed only for/from
                  --  the units that are arguments of gnatmetric call.

                  if Present (Next_Supporter_SF) then
                     Next_Supporter :=
                       Collect_Coupling_Dependencies
                         (Supporters (J), Has_Limited_View);
                  end if;

               end if;

               if Present (Next_Supporter)
                 and then
                  Next_Supporter /= Result
               then
                  --  If we are here, then Next_Supporter is definitely of
                  --  interest for the coupling metrics specified
                  if not Has_Limited_View
                    and then
                     Compute_Control_Coupling_Metric
                    and then
                     Contains_Subprograms
                  then
                     Add_Subprogram_Dependent
                       (To => Next_Supporter, Dependent => Result);
                     Add_Subprogram_Supporter
                       (To => Result, Supporter => Next_Supporter);
                  end if;

                  if Compute_OO_Coupling_Metric
                    and then
                     Arg_Contains_Tagged_Types
                    and then
                     Contains_Tagged_Types
                  then
                     Add_OO_Dependent
                       (To => Next_Supporter, Dependent => Result);
                     Add_OO_Supporter
                       (To => Result, Supporter => Next_Supporter);
                  end if;

                  if Compute_Unit_Coupling_Metric then
                     Add_Dependent (To => Next_Supporter, Dependent => Result);
                     Add_Supporter (To => Result, Supporter => Next_Supporter);
                  end if;

               end if;

            end loop;

         end;

      end if;

      if New_Unit and then Compute_OO_Coupling_Metric then
         --  we have to update  descendants relations

         Next_Parent := Corresponding_Parent_Declaration (Spec_CU);

         if not Is_Nil (Next_Parent) then

            while not Is_Nil
                        (Corresponding_Parent_Declaration (Next_Parent))
            loop
               --  Loop stops when Next_Parent is package Standard
               Next_Supporter := Find_CU (Next_Parent);

               if Present (Next_Supporter) then
                  Add_Descendant
                    (To => Next_Supporter, Descendant => Result);
               end if;

               Next_Parent :=
                 Corresponding_Parent_Declaration (Next_Parent);

            end loop;

         end if;

      end if;

      if Processed_Limited_View_Only (Result)
        and then
         not From_Limited_View
      then
         Set_Processed_Limited_View_Only (Result, False);
      end if;

      return Result;
   end Collect_Coupling_Dependencies;

   procedure Collect_Coupling_Dependencies (CU : Asis.Compilation_Unit) is
      Result : Unit_Id := Collect_Coupling_Dependencies (CU);
      pragma Unreferenced (Result);
   begin
      null;
   end Collect_Coupling_Dependencies;

   -------------------------
   -- Contains_Subprogram --
   -------------------------

   function Contains_Subprogram
     (CU   : Asis.Compilation_Unit)
      return Boolean
   is
      U       : Asis.Element;
      Control : Traverse_Control := Continue;
      Result  : Boolean := False;
   begin
      case Unit_Kind (CU) is
         when A_Procedure          |
              A_Function           |
              A_Procedure_Instance |
              A_Function_Instance  |
              A_Procedure_Body     |
              A_Function_Body      =>
            return True;
         when A_Package          |
              A_Package_Instance =>
            --  More detailed analysis is needed
            null;
         when others =>
            return False;
      end case;

      U := Corresponding_Declaration (Unit_Declaration (CU));

      Look_For_Subprograms (U, Control, Result);

      return Result;
   end Contains_Subprogram;

   ------------------------------
   -- Compute_Coupling_Metrics --
   ------------------------------

   procedure Compute_Coupling_Metrics is
   begin
      --  Do we really have anything to compute?
      null;
   end Compute_Coupling_Metrics;

   ------------------------
   -- Corresponding_Unit --
   ------------------------

   function Corresponding_Unit
     (CU   : Asis.Compilation_Unit)
      return Asis.Compilation_Unit
   is
      Result : Asis.Compilation_Unit := CU;
   begin

      case Unit_Kind (CU) is
         when A_Package_Body =>
            Result := Corresponding_Declaration (CU);
         when A_Subunit =>
            Result :=
              Corresponding_Unit (Corresponding_Subunit_Parent_Body (Result));
         when A_Procedure_Body |
              A_Function_Body  =>
            if not Acts_As_Spec (Unit_Declaration (CU)) then
               Result := Corresponding_Declaration (CU);
            end if;
         when others =>
            null;
      end case;

      return Result;
   end Corresponding_Unit;

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
   begin
      null;
   end Do_Nothing;

   -------------
   -- Find_CU --
   -------------

   function Find_CU (CU : Asis.Compilation_Unit) return Unit_Id is
      Result : Unit_Id;
   begin

      Result := Hash_Table (Hash (CU));

      while Present (Result) loop

         if Is_Equal (Result, CU) then
            exit;
         end if;

         Result := Table (Result).Hash_Link;
      end loop;

      return Result;
   end Find_CU;

   --------------------------
   -- Has_OO_Coupling_Metrics --
   --------------------------

   function Has_OO_Coupling_Metrics
     (CU : Asis.Compilation_Unit)
      return Boolean
   is
      Arg_Unit : constant Asis.Compilation_Unit := Corresponding_Unit (CU);

      Unit    : Asis.Element;
      Result  : Boolean := False;
      Control : Traverse_Control := Continue;
   begin

      if not Has_Limited_View_Only (Arg_Unit)
        or else
         Unit_Kind (Arg_Unit) = A_Package
      then

         case Unit_Kind (Arg_Unit) is
            when A_Package          |
                 A_Generic_Package  |
                 A_Package_Instance =>

               Unit := Unit_Declaration (Arg_Unit);

               if Unit_Kind (Arg_Unit) = A_Package_Instance then
                  Unit := Corresponding_Declaration (Unit);
               end if;

               Look_For_OO_Type (Unit, Control, Result);

            when others =>
               null;
         end case;

      end if;

      return Result;
   end Has_OO_Coupling_Metrics;

   ----------
   -- Hash --
   ----------

   function Hash (CU : Asis.Compilation_Unit) return Hash_Index_Type is
   begin
      return Hash (To_String (Unit_Full_Name (CU)));
   end Hash;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (CU_Id : Unit_Id;
      CU    : Asis.Compilation_Unit)
      return  Boolean
   is
      Result : Boolean := False;
   begin

      if Present (CU_Id)
        and then
         not Is_Nil (CU)
      then
         Result :=
           Get_String (Unit_Name (CU_Id)) = To_String (Unit_Full_Name (CU));
      end if;

      return Result;
   end Is_Equal;

   --------
   -- No --
   --------

   function No (CU_Id : Unit_Id) return Boolean is
   begin
      return CU_Id not in
        First_Unit .. Unit_Containers.Last_Index (Unit_Table);
   end No;

   -------------
   -- Present --
   -------------

   function Present (CU_Id : Unit_Id) return Boolean is
   begin
      return CU_Id in First_Unit .. Unit_Containers.Last_Index (Unit_Table);
   end Present;

   ----------------
   -- Print_List --
   ----------------

   procedure Print_List (List : Dependency_Lists.Set) is
      use Dependency_Lists;
      Next_El : Cursor := First (List);
   begin

      if Next_El = No_Element then
         Info_No_EOL (" ...nothing...");
      else

         while Next_El /= No_Element loop
            Info_No_EOL (Dependency_Lists.Element (Next_El)'Img);
            Next_El := Next (Next_El);
         end loop;

         Info_No_EOL (" (" & Length (List)'Img & ")");

      end if;

      Info ("");

   end Print_List;

   ----------------
   -- Print_Unit --
   ----------------

   procedure Print_Unit (U : Unit_Id) is
   begin
      Info ("Unit_Id =" & U'Img);

      Info_No_EOL (Ident_String);
      Info ("Source file - " &  Source_Name (Unit_SF (U)));

      Info_No_EOL (Ident_String);
      Info ("Unit name   - " &  Get_String (Unit_Name (U)));

      if Table (U).Contains_Tagged_Types then
         Info ("   defines tagged type");
      end if;

      if Table (U).Defines_Subprograms then
         Info ("   defines subprogram");
      end if;

      if Table (U).Processed_Limited_View_Only then
         Info ("   ONLY LIMITED VIEW PROCESSED!");
      end if;

      Info_No_EOL (Ident_String);
      Info ("Hash link   - " &  Unit_Hash_Link (U)'Img);

      Info_No_EOL (Ident_String & "Descendants :");
      Print_List (Table (U).Descendants);

      Info_No_EOL (Ident_String & "OO_Supporters  :");
      Print_List (Table (U).OO_Supporters);

      Info_No_EOL (Ident_String & "OO_Dependents  :");
      Print_List (Table (U).OO_Dependents);

      Info_No_EOL (Ident_String & "Subprogram_Supporters  :");
      Print_List (Table (U).Subprogram_Supporters);

      Info_No_EOL (Ident_String & "Subprogram_Dependents  :");
      Print_List (Table (U).Subprogram_Dependents);

      Info_No_EOL (Ident_String & "Supporters  :");
      Print_List (Table (U).Supporters);

      Info_No_EOL (Ident_String & "Dependents  :");
      Print_List (Table (U).Dependents);

   end Print_Unit;

   ----------------------
   -- Print_Unit_Table --
   ----------------------

   procedure Print_Unit_Table is
   begin

      for J in First_Unit .. Unit_Containers.Last_Index (Unit_Table) loop
         Print_Unit (J);
         Info ("");
      end loop;

   end Print_Unit_Table;

   ---------------------------------
   -- Processed_Limited_View_Only --
   ---------------------------------

   function Processed_Limited_View_Only (CU_Id : Unit_Id) return Boolean is
   begin

      if No (CU_Id) then
         return False;
      else
         return Table (CU_Id).Processed_Limited_View_Only;
      end if;

   end Processed_Limited_View_Only;

   -----------------------------
   -- Report_Coupling_Metrics --
   -----------------------------

   procedure Report_Coupling_Metrics
     (Text : Boolean;
      XML  : Boolean)
   is
      Crown : Dependency_Lists.Set;
      --  Contains terminal (leaf) units belonging to the category

      Category_Supporters : Dependency_Lists.Set;
      Category_Dependents : Dependency_Lists.Set;
      --  External dependencies of the category (a unit from Category set
      --  cannot belong to any of these lists.

      use Dependency_Lists;
      Next_El : Cursor;

      --  Wrapper routines for report routines - they generate the output only\
      --  if the corresponding flag is ON

      procedure Report_XML
        (Message : String;
         Depth   :   Natural   := 0);

      procedure Report
        (Message : String;
         Depth   :   Natural := 0);

      procedure Output_XML_Metric
        (Metric : String;
         Value  : Metric_Count;
         Depth  : Natural := 0);

      procedure Report_XML
        (Message : String;
         Depth   :   Natural   := 0)
      is
      begin
         if XML then
            METRICS.Output.Report_XML (Message, Depth);
         end if;
      end Report_XML;

      procedure Report
        (Message : String;
         Depth   :   Natural := 0)
      is
      begin
         if Text then
            METRICS.Output.Report (Message, Depth);
         end if;
      end Report;

      procedure Output_XML_Metric
        (Metric : String;
         Value  : Metric_Count;
         Depth  : Natural := 0)
      is
      begin
         if XML then
            METRICS.Output.Output_XML_Metric (Metric, Value, Depth);
         end if;
      end Output_XML_Metric;

      subtype Actual_Unit_Id is Existing_Unit_Id
        range First_Unit .. Unit_Containers.Last_Index (Unit_Table);
      type Unit_Id_Array is array (Actual_Unit_Id) of Actual_Unit_Id;
      Sorted_Unit_Ids : Unit_Id_Array;
      --  Sequence of the Unit_Ids in alphabetical order by name

      function Lt (X, Y : Actual_Unit_Id) return Boolean;
      pragma Inline (Lt);

      procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort
        (Actual_Unit_Id, Actual_Unit_Id, Unit_Id_Array, "<" => Lt);

      function Lt (X, Y : Actual_Unit_Id) return Boolean is
         U1 : constant String := Get_String (Unit_Name (X));
         U2 : constant String := Get_String (Unit_Name (Y));
      begin
         return U1 < U2;
      end Lt;

   --  Start of processing for Report_Coupling_Metrics

   begin

      for J in Actual_Unit_Id loop
         Sorted_Unit_Ids (J) := J;
      end loop;
      Sort (Sorted_Unit_Ids);

      Report ("");
      Report ("Coupling metrics:");
      Report ("=================");

      Report_XML ("<coupling>",
                  Depth => 1);

      for J of Sorted_Unit_Ids loop
         --  At the moment we use a rather straightforward, but not very
         --  effective way of computing the category coupling.
         --
         --  For fan-in coupling we collect all the units depending on
         --  a root unit and all its descendants, and then subtracts from
         --  the result the category itself (that is, the given unit
         --  plus all its descendants).
         --
         --  For fan-out coupling, we get a set of all the terminal
         --  units of the category. Because a descendant unit belongs on
         --  all the units all its ancestor units depends upon, we
         --  collect the dependencies of the terminal units only and then
         --  subtracts from the result the category itself.
         --
         --  The disadvantage of this approach is that the same things
         --  are computed many times...

         if Compute_Category_Efferent_Coupling then
            --  First, create the set of terminal nodes.

            Clear (Crown);
            Clear (Category_Supporters);

            if Is_Empty (Table (J).Descendants) then
               Insert (Crown, J);
            else
               Next_El := First (Table (J).Descendants);

               while Next_El /= No_Element loop

                  if Is_Empty
                    (Table (Dependency_Lists.Element (Next_El)).Descendants)
                  then
                     Insert (Crown, Dependency_Lists.Element (Next_El));
                  end if;

                  Next_El := Next (Next_El);
               end loop;

            end if;

            Next_El := First (Crown);

            while Next_El /= No_Element loop
               Union
                 (Category_Supporters,
                  Table (Dependency_Lists.Element (Next_El)).OO_Supporters);

               Next_El := Next (Next_El);
            end loop;

            Difference (Category_Supporters, Table (J).Descendants);
            Exclude    (Category_Supporters, J);

         end if;

         if Compute_Category_Afferent_Coupling then
            Clear (Category_Dependents);
            Union (Category_Dependents, Table (J).OO_Dependents);

            Next_El := First (Table (J).Descendants);

            while Next_El /= No_Element loop

               Union
                 (Category_Dependents,
                  Table (Dependency_Lists.Element (Next_El)).OO_Dependents);

               Next_El := Next (Next_El);
            end loop;

            Difference (Category_Dependents, Table (J).Descendants);
            Exclude    (Category_Dependents, J);

         end if;

         Report ("Unit " & Get_String (Unit_Name (J)) &
                 " (" & Source_Name_For_Output (Unit_SF (J)) & ")",
                 Depth => 1);

         Report_XML
           ("<file name=" & '"' &
            Source_Name_For_Output (Unit_SF (J))  & """>",
            Depth => 2);

         Report_XML
           ("<unit name=""" & Get_String (Unit_Name (J))                &
            """ line="""    & Image (Table (J).Unit_Span.First_Line)    &
            """ col="""     & Image (Table (J).Unit_Span.First_Column)  &
            """>",
            Depth => 3);

         if Compute_OO_Package_Efferent_Coupling
           and then
            Table (J).Contains_Tagged_Types
         then
            Report ("tagged fan-out coupling   :" &
                    Length (Table (J).OO_Supporters)'Img,
                    Depth => 2);

            Output_XML_Metric
              ("tagged_fan-out_coupling",
               Metric_Count (Length (Table (J).OO_Supporters)),
               Depth => 4);
         end if;

         if Compute_Category_Efferent_Coupling
           and then
            Table (J).Contains_Tagged_Types
         then
            Report ("hierarchy fan-out coupling:" &
                    Length (Category_Supporters)'Img,
                    Depth => 2);

            Output_XML_Metric
              ("hierarchy_fan-out_coupling",
               Metric_Count (Dependency_Lists.Length (Category_Supporters)),
               Depth => 4);
         end if;

         if Compute_OO_Package_Afferent_Coupling
           and then
            Table (J).Contains_Tagged_Types
         then
            Report ("tagged fan-in coupling    :" &
                    Length (Table (J).OO_Dependents)'Img,
                    Depth => 2);

            Output_XML_Metric
              ("tagged_fan-in_coupling",
               Metric_Count (Length (Table (J).OO_Dependents)),
               Depth => 4);
         end if;

         if Compute_Category_Afferent_Coupling
           and then
            Table (J).Contains_Tagged_Types
         then
            Report ("hierarchy fan-in coupling :" &
                    Length (Category_Dependents)'Img,
                    Depth => 2);

            Output_XML_Metric
              ("hierarchy_fan-in_coupling",
               Metric_Count (Length (Category_Dependents)),
               Depth => 4);
         end if;

         if Compute_Control_Efferent_Coupling then
            Report ("control fan-out coupling  :" &
                    Length (Table (J).Subprogram_Supporters)'Img,
                    Depth => 2);

            Output_XML_Metric
              ("control_fan-out_coupling",
               Metric_Count (Length (Table (J).Subprogram_Supporters)),
               Depth => 4);
         end if;

         if Compute_Control_Afferent_Coupling
           and then
            Table (J).Defines_Subprograms
         then
            Report ("control fan-in coupling   :" &
                    Length (Table (J).Subprogram_Dependents)'Img,
                    Depth => 2);

            Output_XML_Metric
              ("control_fan-in_coupling",
               Metric_Count (Length (Table (J).Subprogram_Dependents)),
               Depth => 4);
         end if;

         if Compute_Unit_Efferent_Coupling then
            Report ("unit fan-out coupling     :" &
                    Length (Table (J).Supporters)'Img,
                    Depth => 2);

            Output_XML_Metric
              ("unit_fan-out_coupling",
               Metric_Count (Length (Table (J).Supporters)),
               Depth => 4);
         end if;

         if Compute_Unit_Afferent_Coupling then
            Report ("unit fan-in coupling      :" &
                    Length (Table (J).Dependents)'Img,
                    Depth => 2);

            Output_XML_Metric
              ("unit_fan-in_coupling",
               Metric_Count (Length (Table (J).Dependents)),
               Depth => 4);
         end if;

         Report ("");

         Report_XML
           ("</unit>",
            Depth => 3);

         Report_XML
           ("</file>",
            Depth => 2);
      end loop;

      Report_XML ("</coupling>",
                  Depth => 1);

   end Report_Coupling_Metrics;

   -----------
   -- Table --
   -----------

   function Table (CU_Id : Unit_Id) return Unit_Record_Access is
      Result : Unit_Record_Access;

      procedure Process (E : in out Unit_Record);

      procedure Process (E : in out Unit_Record) is
      begin
         Result := E'Unrestricted_Access;
      end Process;
   begin

      Unit_Containers.Update_Element
        (Container => Unit_Table,
         Index     => CU_Id,
         Process   => Process'Access);

      return Result;

   end Table;

   --------------------
   -- Unit_Hash_Link --
   --------------------

   function Unit_Hash_Link (CU_Id : Unit_Id) return Unit_Id is
   begin

      if No (CU_Id) then
         return No_Unit;
      else
         return Table (CU_Id).Hash_Link;
      end if;

   end Unit_Hash_Link;

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name (CU_Id : Unit_Id) return String_Loc is
   begin

      if No (CU_Id) then
         return Nil_String_Loc;
      else
         return Table (CU_Id).Unit_Name;
      end if;

   end Unit_Name;

   -------------
   -- Unit_SF --
   -------------

   function Unit_SF (CU_Id : Unit_Id) return SF_Id is
   begin

      if No (CU_Id) then
         return No_SF_Id;
      else
         return Table (CU_Id).SF;
      end if;

   end Unit_SF;

   --------------------------------
   -- Unit table update routines --
   --------------------------------

   Bool_Tmp    : Boolean;
   Unit_Id_Tmp : Unit_Id;

   procedure Add_Descendant (For_Unit_Rec : in out Unit_Record);
   procedure Add_OO_Dependent         (For_Unit_Rec : in out Unit_Record);
   procedure Add_OO_Supporter         (For_Unit_Rec : in out Unit_Record);
   procedure Add_Subprogram_Dependent (For_Unit_Rec : in out Unit_Record);
   procedure Add_Subprogram_Supporter (For_Unit_Rec : in out Unit_Record);
   procedure Add_Dependent            (For_Unit_Rec : in out Unit_Record);
   procedure Add_Supporter            (For_Unit_Rec : in out Unit_Record);

   procedure Set_Hash_Link (For_Unit_Rec : in out Unit_Record);
   procedure Set_Processed_Limited_View_Only
     (For_Unit_Rec : in out Unit_Record);

   ----------------------------------------------------------

   procedure Add_Descendant (For_Unit_Rec : in out Unit_Record) is
      Tmp_Cursor  : Dependency_Lists.Cursor;
      Tmp_Boolean : Boolean;
      pragma Warnings (Off, Tmp_Cursor);
      pragma Warnings (Off, Tmp_Boolean);
   begin
      Dependency_Lists.Insert
          (Container => For_Unit_Rec.Descendants,
           New_Item  => Unit_Id_Tmp,
           Position  => Tmp_Cursor,
           Inserted  => Tmp_Boolean);
   end Add_Descendant;

   procedure Add_Dependent (For_Unit_Rec : in out Unit_Record) is
      Tmp_Cursor  : Dependency_Lists.Cursor;
      Tmp_Boolean : Boolean;
      pragma Warnings (Off, Tmp_Cursor);
      pragma Warnings (Off, Tmp_Boolean);
   begin
      Dependency_Lists.Insert
          (Container => For_Unit_Rec.Dependents,
           New_Item  => Unit_Id_Tmp,
           Position  => Tmp_Cursor,
           Inserted  => Tmp_Boolean);
   end Add_Dependent;

   procedure Add_Supporter (For_Unit_Rec : in out Unit_Record) is
      Tmp_Cursor  : Dependency_Lists.Cursor;
      Tmp_Boolean : Boolean;
      pragma Warnings (Off, Tmp_Cursor);
      pragma Warnings (Off, Tmp_Boolean);
   begin
      Dependency_Lists.Insert
          (Container => For_Unit_Rec.Supporters,
           New_Item  => Unit_Id_Tmp,
           Position  => Tmp_Cursor,
           Inserted  => Tmp_Boolean);
   end Add_Supporter;

   procedure Add_OO_Dependent (For_Unit_Rec : in out Unit_Record) is
      Tmp_Cursor  : Dependency_Lists.Cursor;
      Tmp_Boolean : Boolean;
      pragma Warnings (Off, Tmp_Cursor);
      pragma Warnings (Off, Tmp_Boolean);
   begin
      Dependency_Lists.Insert
          (Container => For_Unit_Rec.OO_Dependents,
           New_Item  => Unit_Id_Tmp,
           Position  => Tmp_Cursor,
           Inserted  => Tmp_Boolean);
   end Add_OO_Dependent;

   procedure Add_OO_Supporter (For_Unit_Rec : in out Unit_Record) is
      Tmp_Cursor  : Dependency_Lists.Cursor;
      Tmp_Boolean : Boolean;
      pragma Warnings (Off, Tmp_Cursor);
      pragma Warnings (Off, Tmp_Boolean);
   begin
      Dependency_Lists.Insert
          (Container => For_Unit_Rec.OO_Supporters,
           New_Item  => Unit_Id_Tmp,
           Position  => Tmp_Cursor,
           Inserted  => Tmp_Boolean);
   end Add_OO_Supporter;

   procedure Add_Subprogram_Dependent (For_Unit_Rec : in out Unit_Record) is
      Tmp_Cursor  : Dependency_Lists.Cursor;
      Tmp_Boolean : Boolean;
      pragma Warnings (Off, Tmp_Cursor);
      pragma Warnings (Off, Tmp_Boolean);
   begin
      Dependency_Lists.Insert
          (Container => For_Unit_Rec.Subprogram_Dependents,
           New_Item  => Unit_Id_Tmp,
           Position  => Tmp_Cursor,
           Inserted  => Tmp_Boolean);
   end Add_Subprogram_Dependent;

   procedure Add_Subprogram_Supporter (For_Unit_Rec : in out Unit_Record) is
      Tmp_Cursor  : Dependency_Lists.Cursor;
      Tmp_Boolean : Boolean;
      pragma Warnings (Off, Tmp_Cursor);
      pragma Warnings (Off, Tmp_Boolean);
   begin
      Dependency_Lists.Insert
          (Container => For_Unit_Rec.Subprogram_Supporters,
           New_Item  => Unit_Id_Tmp,
           Position  => Tmp_Cursor,
           Inserted  => Tmp_Boolean);
   end Add_Subprogram_Supporter;

   procedure Set_Hash_Link (For_Unit_Rec : in out Unit_Record) is
   begin
      For_Unit_Rec.Hash_Link := Unit_Id_Tmp;
   end Set_Hash_Link;

   procedure Set_Processed_Limited_View_Only
     (For_Unit_Rec : in out Unit_Record)
   is
   begin
      For_Unit_Rec.Processed_Limited_View_Only := Bool_Tmp;
   end Set_Processed_Limited_View_Only;

   ----------------------------------------------------------

   procedure Add_Descendant (To : Unit_Id; Descendant : Unit_Id) is
   begin
      Unit_Id_Tmp := Descendant;

      Unit_Containers.Update_Element
        (Container => Unit_Table,
         Index     => To,
         Process   => Add_Descendant'Access);
   end Add_Descendant;

   procedure Add_Dependent (To : Unit_Id; Dependent : Unit_Id) is
   begin
      Unit_Id_Tmp := Dependent;

      Unit_Containers.Update_Element
        (Container => Unit_Table,
         Index     => To,
         Process   => Add_Dependent'Access);
   end Add_Dependent;

   procedure Add_Supporter (To : Unit_Id; Supporter : Unit_Id) is
   begin
      Unit_Id_Tmp := Supporter;

      Unit_Containers.Update_Element
        (Container => Unit_Table,
         Index     => To,
         Process   => Add_Supporter'Access);
   end Add_Supporter;

   procedure Add_OO_Dependent (To : Unit_Id; Dependent : Unit_Id) is
   begin
      Unit_Id_Tmp := Dependent;

      Unit_Containers.Update_Element
        (Container => Unit_Table,
         Index     => To,
         Process   => Add_OO_Dependent'Access);
   end Add_OO_Dependent;

   procedure Add_OO_Supporter (To : Unit_Id; Supporter : Unit_Id) is
   begin
      Unit_Id_Tmp := Supporter;

      Unit_Containers.Update_Element
        (Container => Unit_Table,
         Index     => To,
         Process   => Add_OO_Supporter'Access);
   end Add_OO_Supporter;

   procedure Add_Subprogram_Dependent (To : Unit_Id; Dependent : Unit_Id) is
   begin
      Unit_Id_Tmp := Dependent;

      Unit_Containers.Update_Element
        (Container => Unit_Table,
         Index     => To,
         Process   => Add_Subprogram_Dependent'Access);
   end Add_Subprogram_Dependent;

   procedure Add_Subprogram_Supporter (To : Unit_Id; Supporter : Unit_Id) is
   begin
      Unit_Id_Tmp := Supporter;

      Unit_Containers.Update_Element
        (Container => Unit_Table,
         Index     => To,
         Process   => Add_Subprogram_Supporter'Access);
   end Add_Subprogram_Supporter;

   procedure Set_Hash_Link (CU_Id : Unit_Id; Val : Unit_Id) is
   begin
      Unit_Id_Tmp := Val;

      Unit_Containers.Update_Element
        (Container => Unit_Table,
         Index     => CU_Id,
         Process   => Set_Hash_Link'Access);
   end Set_Hash_Link;

   procedure Set_Processed_Limited_View_Only
     (CU_Id : Unit_Id;
      Val   : Boolean)
   is
   begin
      Bool_Tmp := Val;

      Unit_Containers.Update_Element
        (Container => Unit_Table,
         Index     => CU_Id,
         Process   => Set_Processed_Limited_View_Only'Access);
   end Set_Processed_Limited_View_Only;

end METRICS.Coupling;
