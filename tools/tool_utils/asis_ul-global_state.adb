------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                 A S I S _ U L . G L O B A L _ S T A T E
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Conversions;     use Ada.Characters.Conversions;
with Ada.Containers.Vectors;
with Ada.Text_IO;                    use Ada.Text_IO;
with Ada.Strings.Fixed;              use Ada.Strings.Fixed;

with GNAT.OS_Lib;                    use GNAT.OS_Lib;
with GNAT.Table;

with Asis.Compilation_Units;         use Asis.Compilation_Units;
with Asis.Declarations;              use Asis.Declarations;
with Asis.Definitions;               use Asis.Definitions;
with Asis.Elements;                  use Asis.Elements;
with Asis.Extensions;                use Asis.Extensions;
with Asis.Extensions.Flat_Kinds;     use Asis.Extensions.Flat_Kinds;
with Asis.Iterator;                  use Asis.Iterator;

with Asis.Set_Get;                   use Asis.Set_Get;

with ASIS_UL.Debug;                  use ASIS_UL.Debug;
with ASIS_UL.Global_State.CG;        use  ASIS_UL.Global_State.CG;
with ASIS_UL.Global_State.CG.Conditions;
with ASIS_UL.Global_State.Utilities; use ASIS_UL.Global_State.Utilities;
with ASIS_UL.Options;
with ASIS_UL.Utilities;              use ASIS_UL.Utilities;

with ASIS_UL.Output;                 use ASIS_UL.Output;

package body ASIS_UL.Global_State is

   ---------------------
   --  Global options --
   ---------------------

   Global_Objects_Accessed : Boolean := False;
   --  Set to True to compute global objects accessed directly or indirectly
   --  by subprograms

   procedure Do_Compute_Global_Objects_Accessed is
   begin
      Global_Objects_Accessed := True;
   end Do_Compute_Global_Objects_Accessed;

   function Compute_Global_Objects_Accessed return Boolean is
   begin
      return Global_Objects_Accessed;
   end Compute_Global_Objects_Accessed;

   -------------------
   -- Data entities --
   -------------------

   Environment_Task_Node_Rec : GS_Node_Record;

   package GS_Nodes_Container is new Ada.Containers.Vectors
      (Index_Type   => Existing_GS_Node_Id,
       Element_Type => GS_Node_Record);
   --  We can not use a hashed container. We need a hash function that works on
   --  ASIS Elements, but we cannot save ASIS Elements in the Container,
   --  because an Element becomes obsolete as soon as its enclosing Context
   --  gets closed. So we have to use "external" hash function onto the vector
   --  structure

   GS_Nodes_Table : GS_Nodes_Container.Vector;
   --  A set of nodes making up the global structure. (With the Table function,
   --  Mimics the Table variable from the instantiation of the GNAT.Table
   --  package)

   -------------------------------------------
   -- Hash table for global structure table --
   -------------------------------------------

   Hash_Num : constant Integer := 2**16;
   --  Number of headers in the hash table. There is no special reason in this
   --  choice.

   Hash_Max : constant Integer := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Integer range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of GS_Node_Id;
   --  The hash table is used to locate existing entries in the nodes table.
   --  The entries point to the first nodes table entry whose hash value
   --  matches the hash code. Then subsequent nodes table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   function Hash (El : Asis.Element) return Hash_Index_Type;
   --  Compute hash code for its argument. At the moment we are using rather
   --  primitive hash function, this should be revised at some point

   -----------------
   -- Scope_Stack --
   -----------------

   subtype Scope_Ind_Type is Natural;

   type Scope_Record is record
      Scope : Scope_Id;
      --  Used to keep stack of enclosed scopes

      Scope_Tree_Node : Node_Id;
      --  reference to the tree node of the corresponding node. (This node is
      --  returned by Einfo.Scope function for the entities defined in this
      --  scope)

   end record;

   package Scope_Stack is new GNAT.Table
     (Table_Component_Type => Scope_Record,
      Table_Index_Type     => Scope_Ind_Type,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 100,
      Table_Name           => "scope stack table");

   procedure Set_Scope_Level (N : GS_Node_Id; Val : Positive);
   --  Sets Val as a scope level of N

   ---------------------------------------------------------
   -- Internal global strucure access and update routines --
   ---------------------------------------------------------

   function Find_Node
     (El            : Asis.Element;
      Expected_Kind : GS_Node_Kinds := Not_A_Node)
      return          GS_Node_Id;
   --  Looks for the node corresponding to the given Element in the global data
   --  structure. If Expected_Kind differs from Not_A_Node, looks only for the
   --  nodes of the kind specified by Expected_Kind. Returns No_GS_Node if
   --  there is no such node. Note, that this function assumes that
   --  Corresponding_Element has been applied to its argument before the call.

   function Define_GS_Node_Kind (El : Asis.Element) return GS_Node_Kinds;
   --  Defines which node kind corresponds to the given Element

   function Is_Equal
     (N             : GS_Node_Id;
      El            : Asis.Element;
      Expected_Kind : GS_Node_Kinds := Not_A_Node)
      return          Boolean;
   --  Checks if the Element represented by N is equal to El. If Expected_Kind
   --  differs from Not_A_Node, assumes that the node corresponding to El
   --  should have the specified kind. Returns False if No (N).

   function Register_Node
     (El            : Asis.Element;
      Encl_Scope    : Scope_Id      := No_Scope;
      Expected_Kind : GS_Node_Kinds := Not_A_Node)
      return GS_Node_Id;
   --  Registers the node corresponding to the argument Element and returns its
   --  Id as a result. The caller is responsible for making sure that this
   --  Element has Corresponding_Element has been applied to its argument
   --  Element before the call. If set to non-empty value, Enclosing_Scope
   --  parameter is used to specify the enclosing scope of the node to be
   --  created. If set to a value different from Not_A_Node, Expected_Kind
   --  specifies the kind of the node to be returned.
   --
   --  As a side effect, this function may create some other nodes and links
   --  needed to represent the result node.
   --
   --  This function can never return No_GS_Node.

   function Get_Node_Name
     (El        : Asis.Element;
      Node_Kind : GS_Node_Kinds)
      return      String;
   --  Returns the name of the entity represented by the node in the global
   --  structure. Assumes that El is of A_Defining_Name kind. Depending on
   --  Node_Kind, some suffixes may be appended to the string image of the
   --  defining name.

   function GS_Node_Hash_Link (N : GS_Node_Id) return GS_Node_Id;
   --  Returns the hash link for the argumeny node. Returns No_GS_Node in case
   --  if No (N).

   procedure Set_Hash_Link (N : GS_Node_Id; Val : GS_Node_Id);
   --  Sets the hash link of the node N to Val. It is erroneous to call it for
   --  No (N).

   Ident_String : constant String := "   ";
   --  Used in the debug output of the global data structures

   procedure Collect_Discr_Init_Calls
     (Type_Decl      : Asis.Element;
      Init_Proc_Node : GS_Node_Id);
   --  Analyses the type declarations and stores all the calls issued from
   --  default discriminant initialization expressions as the calls made by
   --  the discriminant initialization procedure. This procedure assumes that
   --  Has_Discr_Init_Proc (Type_Decl) is True, and Init_Proc_Node is the node
   --  corresponding to the discriminant initialization procedure.

   procedure Collect_Type_Init_Calls
     (Type_Decl      : Asis.Element;
      Init_Proc_Node : GS_Node_Id);
   --  Analyses the type declarations and stores all the calls issued from
   --  default component initialization expressions as the calls made by
   --  the type initialization procedure. This procedure assumes that
   --  Has_Type_Init_Proc (Type_Decl) is True, and Init_Proc_Node is the node
   --  corresponding to the discriminant initialization procedure.

   procedure Collect_Component_Calls
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State);
   --  Pre-operation for type definition traversing aimed at extraction of
   --  function calls from default initialization expressions and needed calls
   --  to type initialization procedures for the components which types have
   --  such subprograms.

   procedure Collect_Calls_From_Components is new Traverse_Element
     (State_Information => No_State,
      Pre_Operation     => Collect_Component_Calls,
      Post_Operation    => No_Op);

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : SLOC_Link) return Boolean is
   begin
      return Left.Node < Right.Node;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : SLOC_Link) return Boolean is
   begin
      return Left.Node = Right.Node;
   end "=";

   -------------------------------------
   -- Add_SLOC_Node_List_To_Node_List --
   -------------------------------------

   procedure Add_SLOC_Node_List_To_Node_List
     (Target : in out Node_Lists.Set;
      Source :        SLOC_Node_Lists.Set)
   is
      Next_Element : SLOC_Node_Lists.Cursor;
      Tmp_Cursor   : Node_Lists.Cursor;
      Tmp_Boolean  : Boolean;
      pragma Warnings (Off, Tmp_Cursor);
      pragma Warnings (Off, Tmp_Boolean);
   begin

      if not SLOC_Node_Lists.Is_Empty (Source) then

         Next_Element := SLOC_Node_Lists.First (Source);

         while SLOC_Node_Lists.Has_Element (Next_Element) loop

            Node_Lists.Insert
             (Container => Target,
              New_Item  => SLOC_Node_Lists.Element (Next_Element).Node,
              Position  => Tmp_Cursor,
              Inserted  => Tmp_Boolean);

            Next_Element := SLOC_Node_Lists.Next (Next_Element);
         end loop;

      end if;

   end Add_SLOC_Node_List_To_Node_List;

   -----------------------------
   -- Collect_Component_Calls --
   -----------------------------

   procedure Collect_Component_Calls
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State)
   is
      pragma Unreferenced (State);
      Tmp : Asis.Element;
   begin

      case Flat_Element_Kind (Element) is
         when A_Component_Declaration =>

            Tmp := Initialization_Expression (Element);

            if Is_Nil (Tmp) then
               --  But the compoenent type may have initialization procedures!
               Tmp := Object_Declaration_View (Element);
               Tmp := Component_Definition_View (Tmp);

               if Definition_Kind (Tmp) = A_Subtype_Indication then
                  Tmp := Get_Subtype_Structure (Tmp);

                  if Has_Type_Init_Proc (Tmp) then
                     Add_Link_To_SLOC_List
                       (To_Node     => Current_Scope,
                        To_List     => Calls,
                        Link_To_Add =>
                          (Node => Corresponding_Node
                                     (El            => Tmp,
                                      Expected_Kind => A_Type_Init_Procedure),
                           SLOC => Build_GNAT_Location (Element)));

                  end if;

               end if;
            else
               Unconditionally_Collect_CG_Info_From_Construct (Tmp);
            end if;

         when A_Protected_Definition                |
              A_Derived_Record_Extension_Definition |
              A_Record_Type_Definition              |
              A_Tagged_Record_Type_Definition       |
              A_Record_Definition                   |
              A_Variant_Part                        |
              A_Variant                             =>
            null;
         when others =>
            Control := Abandon_Children;
      end case;

   end Collect_Component_Calls;

   ------------------------------
   -- Collect_Discr_Init_Calls --
   ------------------------------

   procedure Collect_Discr_Init_Calls
     (Type_Decl      : Asis.Element;
      Init_Proc_Node : GS_Node_Id)
   is
      Parent_Type : Asis.Element;
   begin
      Set_Current_Scope (Init_Proc_Node, Node (Type_Decl));

      Parent_Type := Discriminant_Part (Type_Decl);

      if Is_Nil (Parent_Type) then
         --  Derived type that inherits discriminants from parent
         Parent_Type := Type_Declaration_View (Type_Decl);
         Parent_Type := Corresponding_Parent_Subtype (Parent_Type);

         if Declaration_Kind (Parent_Type) = A_Subtype_Declaration then
            Parent_Type := Corresponding_First_Subtype (Parent_Type);
         end if;

         --  Here we add to Init_Proc_Node the call to the discriminant
         --  initialization procedure of the parent type

         Add_Link_To_SLOC_List
           (To_Node     => Init_Proc_Node,
            To_List     => Calls,
            Link_To_Add =>
              (Node => Corresponding_Node
                         (El            => Parent_Type,
                          Expected_Kind => A_Type_Discr_Init_Procedure),
               SLOC => Build_GNAT_Location (Type_Decl)));

      else
         declare
            Discrs : constant Asis.Element_List := Discriminants (Parent_Type);
         begin

            for J in Discrs'Range loop
               Unconditionally_Collect_CG_Info_From_Construct
                 (Initialization_Expression (Discrs (J)));
            end loop;

         end;
      end if;

      Set_Body_Analyzed (Init_Proc_Node);

      Remove_Current_Scope;
   end Collect_Discr_Init_Calls;

   -----------------------------
   -- Collect_Type_Init_Calls --
   -----------------------------

   procedure Collect_Type_Init_Calls
     (Type_Decl      : Asis.Element;
      Init_Proc_Node : GS_Node_Id)
   is
      Type_Def    : Asis.Element;
      Parent_Type : Asis.Element;

      Control     : Traverse_Control := Continue;
      Tmp_State   : No_State := Not_Used;
   begin
      Set_Current_Scope (Init_Proc_Node, Node (Type_Decl));

      Type_Def := Type_Declaration_View (Type_Decl);

      case Flat_Element_Kind (Type_Def) is
         when A_Derived_Type_Definition         |
              An_Unconstrained_Array_Definition |
              A_Constrained_Array_Definition    =>

         --  The only possibility is that the parent/component type has an
            --  initialization procedure
            if Type_Kind (Type_Def) = A_Derived_Type_Definition then
               Parent_Type := Parent_Subtype_Indication (Type_Def);
            else
               Parent_Type := Array_Component_Definition (Type_Def);
               Parent_Type := Component_Definition_View  (Parent_Type);
            end if;

            Parent_Type := Get_Subtype_Structure (Parent_Type);

            Add_Link_To_SLOC_List
              (To_Node     => Init_Proc_Node,
               To_List     => Calls,
               Link_To_Add =>
                 (Node => Corresponding_Node
                            (El            => Parent_Type,
                             Expected_Kind => A_Type_Init_Procedure),
                  SLOC => Build_GNAT_Location (Type_Decl)));

         when A_Protected_Definition                |
              A_Derived_Record_Extension_Definition |
              A_Record_Type_Definition              |
              A_Tagged_Record_Type_Definition      =>

            Collect_Calls_From_Components (Type_Def, Control, Tmp_State);

            if Type_Kind (Type_Def) =
                A_Derived_Record_Extension_Definition
            then
               Parent_Type := Parent_Subtype_Indication (Type_Def);
               Parent_Type := Get_Subtype_Structure (Parent_Type);

               if Has_Type_Init_Proc (Parent_Type) then
                  Add_Link_To_SLOC_List
                     (To_Node     => Init_Proc_Node,
                      To_List     => Calls,
                      Link_To_Add =>
                        (Node => Corresponding_Node
                                   (El            => Parent_Type,
                                    Expected_Kind => A_Type_Init_Procedure),
                         SLOC => Build_GNAT_Location (Type_Decl)));

               end if;

            end if;
         when others =>
            pragma Assert (False);
            null;
      end case;

      Set_Body_Analyzed (Init_Proc_Node);

      Remove_Current_Scope;
   end Collect_Type_Init_Calls;

   ------------------------
   -- Corresponding_Node --
   ------------------------

   function Corresponding_Node
     (El              : Element;
      Enclosing_Scope : Scope_Id      := No_Scope;
      Expected_Kind   : GS_Node_Kinds := Not_A_Node;
      Unconditionally : Boolean       := False)
      return            GS_Node_Id
   is
      Corresponding_El : constant Asis.Element := Corresponding_Element (El);

      Result : GS_Node_Id := Find_Node (Corresponding_El, Expected_Kind);
   begin

      if No (Result)
        and then
         (Unconditionally
         or else
          Expected_Kind in
            A_Type_Discr_Init_Procedure .. A_Type_Init_Procedure
         or else
          ASIS_UL.Global_State.CG.Conditions.Should_Be_In_CG
            (Corresponding_El))
      then
         Result :=
           Register_Node (Corresponding_El, Enclosing_Scope, Expected_Kind);
      end if;

      return Result;
   end Corresponding_Node;

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope return Scope_Id is
   begin

      if Scope_Stack.Last >= Scope_Stack.First then
         return Scope_Stack.Table (Scope_Stack.Last).Scope;
      else
         raise Scope_Stack_Error;
      end if;

   end Current_Scope;

   -----------------------------
   -- Current_Scope_Tree_Node --
   -----------------------------

   function Current_Scope_Tree_Node return Node_Id is
   begin

      if Scope_Stack.Last >= Scope_Stack.First then
         return Scope_Stack.Table (Scope_Stack.Last).Scope_Tree_Node;
      else
         raise Scope_Stack_Error;
      end if;

   end Current_Scope_Tree_Node;

   -------------------------
   -- Define_GS_Node_Kind --
   -------------------------

   function Define_GS_Node_Kind (El : Asis.Element) return GS_Node_Kinds is
      Result : GS_Node_Kinds := Not_A_Node;
   begin

      case Flat_Element_Kind (El) is
         when A_Package_Declaration |
            A_Generic_Package_Declaration =>
            Result := A_Package;

         when A_Procedure_Declaration |
              A_Formal_Procedure_Declaration =>

            if Definition_Kind (Enclosing_Element (El)) =
                 A_Protected_Definition
            then
               Result := A_Protected_Procedure;
            else
               Result := A_Procedure;
            end if;

         when A_Procedure_Body_Declaration |
              A_Procedure_Body_Stub        |
              A_Procedure_Instantiation    =>
            Result := A_Procedure;

         when A_Null_Procedure_Declaration =>
            Result := A_Null_Procedure;

         when A_Function_Declaration |
              A_Formal_Function_Declaration =>

            if Definition_Kind (Enclosing_Element (El)) =
                 A_Protected_Definition
            then
               Result := A_Protected_Function;
            else
               Result := A_Function;
            end if;

         when A_Function_Body_Declaration        |
              An_Expression_Function_Declaration |
              A_Function_Body_Stub               |
              A_Function_Instantiation           =>
            Result := A_Function;

         when A_Task_Type_Declaration    |
               A_Single_Task_Declaration =>
            Result := A_Task;

         when An_Entry_Declaration =>

            if Definition_Kind (Enclosing_Element (El)) =
                 A_Protected_Definition
            then
               Result := A_Protected_Entry;
            else
               Result := A_Task_Entry;
            end if;

         when A_Defining_Identifier =>
            Result := A_Data_Object;
         when others =>
            null;
      end case;

      return Result;
   end Define_GS_Node_Kind;

   ---------------
   -- Find_Node --
   ---------------

   function Find_Node
     (El            : Asis.Element;
      Expected_Kind : GS_Node_Kinds := Not_A_Node)
      return          GS_Node_Id
   is
      Result : GS_Node_Id;
   begin

      Result := Hash_Table (Hash (El));

      while Present (Result) loop

         if Is_Equal (Result, El, Expected_Kind) then
            exit;
         end if;

         Result := Table (Result).Hash_Link;
      end loop;

      return Result;
   end Find_Node;

   -------------------
   -- Get_Node_Name --
   -------------------

   function Get_Node_Name
     (El        : Asis.Element;
      Node_Kind : GS_Node_Kinds)
      return      String
   is
      Result : constant String := To_String (Defining_Name_Image (El));
   begin
      pragma Assert (Node_Kind /= Not_A_Node);

      case Node_Kind is
         when A_Type_Discr_Init_Procedure =>
            return Result & "'Discr_Init";
         when A_Type_Init_Procedure =>
            return Result & "'Init";
         when others =>
            return Result;
      end case;
   end Get_Node_Name;

   ----------
   -- Hash --
   ----------

   function Hash (El : Asis.Element) return Hash_Index_Type is
   begin
      return Asis.Elements.Hash (El) mod Hash_Num;
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      GS_Nodes_Container.Reserve_Capacity
        (Container => GS_Nodes_Table,
         Capacity  => 1_000);

      Scope_Stack.Init;
      Hash_Table   := (others => No_GS_Node);

      --  Locating the node representing the evironment task:
      GS_Nodes_Container.Append (Container => GS_Nodes_Table,
                                 New_Item  => Environment_Task_Node_Rec);

      Environment_Task_Node := Last_Node;

      Set_Current_Scope (Environment_Task_Node, Empty);
   end Initialize;

   ----------------------
   -- Is_Callable_Node --
   ----------------------

   function Is_Callable_Node (N : GS_Node_Id) return Boolean is
   begin
      pragma Assert (Present (N));
      return GS_Node_Kind (N) in Callable_Nodes;
   end Is_Callable_Node;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (N             : GS_Node_Id;
      El            : Asis.Element;
      Expected_Kind : GS_Node_Kinds := Not_A_Node)
      return          Boolean
   is
      Required_Kins : GS_Node_Kinds := Expected_Kind;
      Result        : Boolean       := False;
      El_For_SLOC   : Asis.Element;
   begin

      pragma Assert (Is_Equal (El, Corresponding_Element (El)));

      if Required_Kins = Not_A_Node then
         Required_Kins := Define_GS_Node_Kind (El);
      end if;

      if Defining_Name_Kind (El) /= A_Defining_Identifier then
         --  Callable entity
         El_For_SLOC := Get_Defining_Name (El);
      else
         El_For_SLOC := El;
      end if;

      if Present (N)
        and then
         not Is_Nil (El)
      then
         if GS_Node_Kind (N) = Required_Kins
           and then
             Get_String (GS_Node_SLOC (N)) = Build_GNAT_Location (El_For_SLOC)
         then
            Result := True;
         end if;

      end if;

      return Result;
   end Is_Equal;

   ------------------------
   -- Is_Subprogram_Node --
   ------------------------

   function Is_Subprogram_Node (N : GS_Node_Id) return Boolean is
   begin
      pragma Assert (Present (N));
      return GS_Node_Kind (N) in Subprogram_Nodes
            or else
             GS_Node_Kind (N) in Protected_Subprogram_Nodes;
   end Is_Subprogram_Node;

   ---------------
   -- Last_Node --
   ---------------

   function Last_Node return GS_Node_Id is
   begin
      return GS_Nodes_Container.Last_Index (GS_Nodes_Table);
   end Last_Node;

   --------
   -- No --
   --------

   function No (N : GS_Node_Id) return Boolean is
   begin
      return N = No_GS_Node;
   end No;

   -------------
   -- Present --
   -------------

   function Present (N : GS_Node_Id) return Boolean is
   begin
      return N /= No_GS_Node;
   end Present;

   ----------------------------
   -- Print_Global_Structure --
   ----------------------------

   All_Calls_Count     : Natural := 0;
   Direct_Calls_Count  : Natural := 0;
   Direct_Reads_Count  : Natural := 0;
   Direct_Writes_Count : Natural := 0;

   procedure Print_Global_Structure is
   begin
      if Debug_Flag_G then
         All_Calls_Count     := 0;
         Direct_Calls_Count  := 0;
         Direct_Reads_Count  := 0;
         Direct_Writes_Count := 0;

         for J in First_GS_Node .. Last_Node loop
            Print_Node (J);
            Info ("");
         end loop;

         New_Line;
         New_Line;

         Info ("Direct_Calls :" & Direct_Calls_Count'Img);
         Info ("All_Calls    :" & All_Calls_Count'Img);

         if Global_Objects_Accessed then
            Info ("Direct_Reads :" & Direct_Reads_Count'Img);
            Info ("Direct_Writes:" & Direct_Writes_Count'Img);
         end if;
      end if;
   end Print_Global_Structure;

   ----------------
   -- Print_List --
   ----------------

   procedure Print_List (Node_List : Node_Lists.Set) is
      Next_El : Node_Lists.Cursor := Node_Lists.First (Node_List);

      use Node_Lists;
   begin

      if Next_El = Node_Lists.No_Element then
         Info_No_EOL (" ...nothing...");
      else

         while Next_El /= Node_Lists.No_Element loop
            Info_No_EOL (Node_Lists.Element (Next_El)'Img);
            Next_El := Node_Lists.Next (Next_El);
         end loop;

      end if;

      Info ("");

   end Print_List;

   ----------------
   -- Print_Node --
   ----------------

   procedure Print_Node (N : GS_Node_Id) is
   begin
      --  Common node fields:

      Info_No_EOL ("Node_Id =" & N'Img);
      Info (" (" & GS_Node_Name (N) & ")");

      Info_No_EOL (Ident_String);
      Info ("Node_Kind       = " &  GS_Node_Kind (N)'Img);

      Info_No_EOL (Ident_String);
      Info_No_EOL ("SLOC            = " & Get_String (GS_Node_SLOC (N)));
      Info (" (SF_Id =" & Enclosing_Source (N)'Img & ")");

      Info_No_EOL (Ident_String);
      Info ("Enclosing_Scope = " & GS_Node_Enclosing_Scope (N)'Img);

      Info_No_EOL (Ident_String);
      Info ("Scope level =" & GS_Node_Scope_Level (N)'Img);

      if Is_Of_No_Interest (N) then
         Info_No_EOL (Ident_String);
         Info ("This node is of no interest for global analysis");
      end if;

      --  Fields that meaning depend on node class:

      if GS_Node_Kind (N) in Callable_Nodes then
         Info_No_EOL (Ident_String);
         Info ("Body analyzed    = " & Body_Analyzed (N)'Img);

         if GS_Is_Renaming (N) then
            Info_No_EOL (Ident_String);
            Info ("This is a renaming node");
         end if;

         if GS_Is_Task_Type (N) then
            Info_No_EOL (Ident_String);
            Info ("This is a task type node");
         end if;

         if GS_Node_Kind (N) in Subprogram_Nodes
           and then
            Is_Dispatching_Operation_Node (N)
         then
            Info_No_EOL (Ident_String);
            Info ("This is a dispatching operation node");
         end if;

         if GS_Node_Kind (N) in Subprogram_Nodes then

            if Is_Abstract_Subprogram_Node (N) then
               Info_No_EOL (Ident_String);
               Info ("This is an abstract operation node");
            end if;

            if Is_Implicit_Subprogram_Node (N) then
               Info_No_EOL (Ident_String);
               Info ("This is an implicit subprogram node");
            end if;

         end if;

         Info_No_EOL (2 * Ident_String & "direct calls             :");
         Print_SLOC_List (Table (N).SLOC_Node_List_1);

         Info_No_EOL (2 * Ident_String & "postponed disp calls     :");
         Print_SLOC_List (Table (N).Dispatching_Calls);

         Direct_Calls_Count :=
           Direct_Calls_Count +
             Natural (SLOC_Node_Lists.Length (Table (N).SLOC_Node_List_1));

         Info_No_EOL (2 * Ident_String & "all calls                :");
         Print_List (Table (N).Node_List_1);

         Info_No_EOL (2 * Ident_String & "direct dispatching calls :");
         Print_List (Table (N).Node_List_2);

         if GS_Node_Kind (N) in Subprogram_Nodes
           and then
            Is_Dispatching_Operation_Node (N)
         then
            Info_No_EOL (2 * Ident_String & "directly dispatched to   :");
            Print_List (Table (N).Node_List_3);

            Info_No_EOL (2 * Ident_String & "may be dispatched to     :");
            Print_List (Table (N).Node_List_4);
         end if;

         All_Calls_Count :=
           All_Calls_Count +
             Natural (Node_Lists.Length (Table (N).Node_List_1));

         Info_No_EOL (2 * Ident_String & "direct reads             :");
         Print_SLOC_List (Table (N).SLOC_Node_List_2);

         Direct_Reads_Count :=
           Direct_Reads_Count +
             Natural (SLOC_Node_Lists.Length (Table (N).SLOC_Node_List_2));

         Info_No_EOL (2 * Ident_String & "direct writes            :");
         Print_SLOC_List (Table (N).SLOC_Node_List_3);

         Direct_Writes_Count :=
           Direct_Writes_Count +
             Natural (SLOC_Node_Lists.Length (Table (N).SLOC_Node_List_3));

         Info_No_EOL (2 * Ident_String & "indirect reads           :");
         Print_List (Table (N).Node_List_5);

         Info_No_EOL (2 * Ident_String & "indirect writes          :");
         Print_List (Table (N).Node_List_6);
      else
         null;
      end if;

      --  Application_Specific flags:

      Info_No_EOL (Ident_String);
      Info ("Application flag 1  = " & Get_Application_Flag_1 (N)'Img);

   end Print_Node;

   ---------------------
   -- Print_SLOC_List --
   ---------------------

   procedure Print_SLOC_List (Node_List : SLOC_Node_Lists.Set) is
      Next_El : SLOC_Node_Lists.Cursor := SLOC_Node_Lists.First (Node_List);

      use SLOC_Node_Lists;
   begin

      if Next_El = SLOC_Node_Lists.No_Element then
         Info_No_EOL (" ...nothing...");
      else

         Info ("");

         while Next_El /= SLOC_Node_Lists.No_Element loop
            Info_No_EOL (Ident_String);
            Info_No_EOL (Ident_String);
            Info_No_EOL (SLOC_Node_Lists.Element (Next_El).Node'Img);
            Info ("(" &
                  Get_String (SLOC_Node_Lists.Element (Next_El).SLOC) & ")");
            Next_El := SLOC_Node_Lists.Next (Next_El);
         end loop;

      end if;

      Info ("");

   end Print_SLOC_List;

   -------------------
   -- Register_Node --
   -------------------

   function Register_Node
     (El            : Asis.Element;
      Encl_Scope    : Scope_Id      := No_Scope;
      Expected_Kind : GS_Node_Kinds := Not_A_Node)
      return GS_Node_Id
   is
      Encl_Scope_Node  :          Scope_Id        := Encl_Scope;
      Tmp              :          Asis.Element;
      New_Node         :          GS_Node_Record;
      New_Node_Kind    :          GS_Node_Kinds   := Expected_Kind;
      Hash_Value       : constant Hash_Index_Type := Hash (El);
      Last_In_Chain    :          GS_Node_Id      := Hash_Table (Hash_Value);
      Node_Source_File :          SF_Id           := No_SF_Id;
      Is_RTL_Node      : constant Boolean         :=
        Unit_Origin (Enclosing_Compilation_Unit (El)) /= An_Application_Unit;
      El_For_SLOC      :          Asis.Element;
      Entity_Def_Name  :          Asis.Element;

      Result           : GS_Node_Id;
   begin

      if No (Encl_Scope_Node) then
         --  The following may cause creation of a whole bunch of nodes

         Tmp := Enclosing_Scope (El);

         if Is_Nil (Tmp) then
            Encl_Scope_Node := Environment_Task_Node;
         else
            Encl_Scope_Node :=
              Corresponding_Node (Corresponding_Element (Tmp));
         end if;
      end if;

      if not ASIS_UL.Options.Process_RTL_Units
       and then
         Is_RTL_Node
      then
         null;
         --  To be completed later. Node_Source_File remains null...
      else
         Node_Source_File := File_Find (El);
         --  To be completed later...

         if not Present (Node_Source_File) then
            Node_Source_File :=
               Add_Needed_Source
                 (Normalize_Pathname
                    (To_String
                       (Asis.Compilation_Units.Text_Name
                          (Asis.Elements.Enclosing_Compilation_Unit (El))),
                     Resolve_Links  => False,
                     Case_Sensitive => True));

         end if;

      end if;

      if New_Node_Kind = Not_A_Node then
         New_Node_Kind := Define_GS_Node_Kind (El);
      end if;

      if Defining_Name_Kind (El) /= A_Defining_Identifier then
         --  Callable entity
         El_For_SLOC     := Get_Defining_Name (El);
         Entity_Def_Name := First_Name (El);
      else
         El_For_SLOC     := El;
         Entity_Def_Name := El;
      end if;

      New_Node :=
        (Node_Kind          => New_Node_Kind,
         SLOC               => Build_GNAT_Location (El_For_SLOC),
         Name               =>
           Enter_String (Get_Node_Name (Entity_Def_Name, New_Node_Kind)),
         Source_File        => Node_Source_File,
         Enclosing_Scope    => Encl_Scope_Node,
         Scope_Level        => 1,
         Hash_Link          => No_GS_Node,
         Is_RTL_Node        => Is_RTL_Node,
         Is_Of_No_Interest  => False,

         Bool_Flag_1        => False,
         Bool_Flag_2        => False,
         Bool_Flag_3        => False,
         Bool_Flag_4        => False,
         Bool_Flag_5        => False,
         Bool_Flag_6        => False,
         Application_Flag_1 => False,
         SLOC_Node_List_1   => SLOC_Node_Lists.Empty_Set,
         SLOC_Node_List_2   => SLOC_Node_Lists.Empty_Set,  --  Direct reads
         SLOC_Node_List_3   => SLOC_Node_Lists.Empty_Set,  --  Direct writes
         Dispatching_Calls  => SLOC_Node_Lists.Empty_Set,  -- Dispatching calls
         Node_List_1        => Node_Lists.Empty_Set,  --  All_Calls
         Node_List_2        => Node_Lists.Empty_Set, --  Direct disp calls
         Node_List_3        => Node_Lists.Empty_Set, --  Directly impl spbs
         Node_List_4        => Node_Lists.Empty_Set, --  All impl spbs
         Node_List_5        => Node_Lists.Empty_Set, --  Indirect reads
         Node_List_6        => Node_Lists.Empty_Set, --  Indirect writes
         Bool_Flag_7               => False          --  Missing_Body_Reported
         );

      pragma Assert (New_Node.Node_Kind /= Not_A_Node);

      if New_Node.Node_Kind in Subprogram_Nodes then
         New_Node.Bool_Flag_4 := Is_Dispatching_Operation (El);

         if Declaration_Kind (El) = An_Expression_Function_Declaration then
            New_Node.Bool_Flag_1 := True; --  Is_Body_Analyzed
         end if;

         if Trait_Kind (El) = An_Abstract_Trait then
            New_Node.Bool_Flag_1 := True; --  Is_Body_Analyzed
            New_Node.Bool_Flag_5 := True; --  Is_Abstract_Subprogram_Node
         end if;

         New_Node.Bool_Flag_6 := Is_Part_Of_Implicit (El);

         --  Temporary workaround for incomplete implementation of dispatching
         --  subprogram elimination. At the moment we cannot properly handle
         --  multiple inheritance case, so if a dispatching operation belongs
         --  to a type that inherits anything from some interface type, we
         --  mark it as used    ???

--         if New_Node.Bool_Flag_4
--           and then
--             Belongs_To_Multiple_Inheritance (El)
--         then
--            New_Node.Application_Flag_1 := True;
--         end if;

      end if;

      GS_Nodes_Container.Append
        (Container => GS_Nodes_Table,
         New_Item  => New_Node);

      Result := Last_Node;

      if No (Last_In_Chain) then
         Hash_Table (Hash_Value) := Result;
      else

         while Present (GS_Node_Hash_Link (Last_In_Chain)) loop
            Last_In_Chain := GS_Node_Hash_Link (Last_In_Chain);
         end loop;

         Set_Hash_Link
           (N   => Last_In_Chain,
            Val => Result);
      end if;

      --  Try to compute node properties...

      if New_Node.Node_Kind in Callable_Nodes then

         if Is_Imported_Subprogram (El)
           or else
            Is_Part_Of_Implicit (El)
           or else
            New_Node_Kind = A_Null_Procedure
         then
            Set_Body_Analyzed (Result);
         end if;

         if New_Node.Node_Kind /= A_Type_Discr_Init_Procedure
           and then
            Declaration_Kind (El) = A_Task_Type_Declaration
         then
            Set_Is_Task_Type (Result);
         end if;

         if Is_Exported (First_Name (El)) then
            --  Exported subprogram, is considered as being called by the
            --  environment task
            Add_Link_To_SLOC_List
              (To_Node     => Environment_Task_Node,
               Link_To_Add => (Result, New_Node.SLOC));
         end if;

         if Is_Controlling_Type_Operation (El) then
            --  Subprogram that is implicitly called when object of a
            --  controlled type is created, destroyed or assigned, is
            --  considered as being called by the scope where the subprogram
            --  is declared
            Add_Link_To_SLOC_List
              (To_Node     => Encl_Scope_Node,
               Link_To_Add => (Result, New_Node.SLOC));
         end if;

         if New_Node_Kind = A_Type_Discr_Init_Procedure then
            Collect_Discr_Init_Calls (El, Result);
         end if;

         if New_Node_Kind = A_Type_Init_Procedure then
            Collect_Type_Init_Calls (El, Result);
         end if;

         if ASIS_UL.Options.Represent_Dispatching_Calls
           and then
            Is_Dispatching_Operation_Node (Result)
           and then
            (Is_Overriding_Operation (El)
            or else
             Is_Part_Of_Inherited (El))
         then
            Set_Implementing_Node (Implemented_Operations (El), Result);
         end if;
      end if;

      return Result;

   end Register_Node;

   --------------------------
   -- Remove_Current_Scope --
   --------------------------

   procedure Remove_Current_Scope is
   begin

      if Scope_Stack.Last >= Scope_Stack.First then
         Scope_Stack.Decrement_Last;
      else
         raise Scope_Stack_Error;
      end if;

   end Remove_Current_Scope;

   -----------------------
   -- Set_Current_Scope --
   -----------------------

   procedure Set_Current_Scope
     (Scope           : GS_Node_Id;
      Scope_Tree_Node : Node_Id)
   is
   begin
      Scope_Stack.Append ((Scope, Scope_Tree_Node));

      Set_Scope_Level
        (N   => Scope,
         Val => Scope_Stack.Last);
   end Set_Current_Scope;

   -----------
   -- Table --
   -----------

   function Table (N : GS_Node_Id) return GS_Node_Record_Access is
      Result : GS_Node_Record_Access;

      procedure Process (E : in out GS_Node_Record);

      procedure Process (E : in out GS_Node_Record) is
      begin
         Result := E'Unrestricted_Access;
      end Process;
   begin

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Process'Access);

      return Result;

   end Table;

   ----------------------------
   -- Common node attributes --
   ----------------------------

   function All_Calls (N : GS_Node_Id) return Node_List_Access is
   begin
      pragma Assert (Present (N) and then Is_Callable_Node (N));
      return Table (N).Node_List_1'Unrestricted_Access;
   end All_Calls;

   function Direct_Calls (N : GS_Node_Id) return SLOC_Node_List_Access is
   begin
      pragma Assert (Present (N) and then Is_Callable_Node (N));
      return Table (N).SLOC_Node_List_1'Unrestricted_Access;
   end Direct_Calls;

   function Direct_Reads (N : GS_Node_Id) return SLOC_Node_List_Access is
   begin
      pragma Assert (Present (N) and then Is_Callable_Node (N));
      return Table (N).SLOC_Node_List_2'Unrestricted_Access;
   end Direct_Reads;

   function Direct_Writes (N : GS_Node_Id) return SLOC_Node_List_Access is
   begin
      pragma Assert (Present (N) and then Is_Callable_Node (N));
      return Table (N).SLOC_Node_List_3'Unrestricted_Access;
   end Direct_Writes;

   function Indirect_Reads (N : GS_Node_Id) return Node_List_Access is
   begin
      pragma Assert (Present (N) and then Is_Callable_Node (N));
      return Table (N).Node_List_5'Unrestricted_Access;
   end Indirect_Reads;

   function Indirect_Writes (N : GS_Node_Id) return Node_List_Access is
   begin
      pragma Assert (Present (N) and then Is_Callable_Node (N));
      return Table (N).Node_List_6'Unrestricted_Access;
   end Indirect_Writes;

   function Enclosing_Source (N : GS_Node_Id) return SF_Id is
   begin
      return Table (N).Source_File;
   end Enclosing_Source;

   function  Get_Application_Flag_1 (N : GS_Node_Id) return Boolean is
   begin
      pragma Assert (Present (N));
      return Table (N).Application_Flag_1;
   end Get_Application_Flag_1;

   function GS_Enclosed_CU_Name (N : GS_Node_Id) return String is
   begin
      pragma Assert (Present (N) and then N /= Environment_Task_Node);

      return CU_Name (Enclosing_Source (N));

   end GS_Enclosed_CU_Name;

   function GS_Node_Enclosing_Scope (N : GS_Node_Id) return Scope_Id is
   begin

      if No (N) then
         return No_Scope;
      else
         return Table (N).Enclosing_Scope;
      end if;

   end GS_Node_Enclosing_Scope;

   function GS_Node_Hash_Link (N : GS_Node_Id) return GS_Node_Id is
   begin

      if No (N) then
         return No_GS_Node;
      else
         return Table (N).Hash_Link;
      end if;

   end GS_Node_Hash_Link;

   function GS_Node_Kind (N : GS_Node_Id) return GS_Node_Kinds is
   begin
      if No (N) then
         return Not_A_Node;
      else
         return Table (N).Node_Kind;
      end if;
   end GS_Node_Kind;

   function GS_Node_Name (N : GS_Node_Id) return String is
   begin
      pragma Assert (Present (N));
      return Get_String (Table (N).Name);
   end GS_Node_Name;

   function GS_Node_Scope_Level (N : GS_Node_Id) return Natural is
   begin
      return Table (N).Scope_Level;
   end GS_Node_Scope_Level;

   function GS_Node_SLOC (N : GS_Node_Id) return String_Loc is
   begin
      if No (N) then
         return Nil_String_Loc;
      else
         return Table (N).SLOC;
      end if;
   end GS_Node_SLOC;

   function Is_Abstract_Subprogram_Node (N : GS_Node_Id) return Boolean is
   begin
      pragma Assert (Present (N));

      if GS_Node_Kind (N) in Subprogram_Nodes then
         return Table (N).Bool_Flag_5;
      else
         return False;
      end if;

   end Is_Abstract_Subprogram_Node;

   function Is_Implicit_Subprogram_Node (N : GS_Node_Id) return Boolean is
   begin
      pragma Assert (Present (N));

      if GS_Node_Kind (N) in Subprogram_Nodes then
         return Table (N).Bool_Flag_6;
      else
         return False;
      end if;

   end Is_Implicit_Subprogram_Node;

   function Is_Dispatching_Operation_Node (N : GS_Node_Id) return Boolean is
   begin
      pragma Assert (Present (N));

      if GS_Node_Kind (N) in Subprogram_Nodes then
         return Table (N).Bool_Flag_4;
      else
         return False;
      end if;

   end Is_Dispatching_Operation_Node;

   function Is_Of_No_Interest (N : GS_Node_Id) return Boolean is
   begin
      return Table (N).Is_Of_No_Interest
        or else
          (not ASIS_UL.Options.Process_RTL_Units
          and then
           Table (N).Is_RTL_Node);
   end Is_Of_No_Interest;

   function Is_RTL_Node (N : GS_Node_Id) return Boolean is
   begin
      return Table (N).Is_RTL_Node;
   end Is_RTL_Node;

   --------------------------------------
   -- Node attribute update procedures --
   --------------------------------------

   Bool_Tmp                : Boolean;
   SLOC_Link_List_Type_Tmp : SLOC_Link_List_Types;
   SLOC_Link_Tmp           : SLOC_Link;
   GS_Node_Tmp             : GS_Node_Id;
   Int_Tmp                 : Integer;
   --  Data to keep the value to be assigned to the record node field

   procedure Add_Link_To_SLOC_List  (For_Node_Rec : in out GS_Node_Record);
   procedure Set_Hash_Link          (For_Node_Rec : in out GS_Node_Record);
   procedure Set_Is_Of_No_Interest  (For_Node_Rec : in out GS_Node_Record);
   procedure Set_Scope_Level        (For_Node_Rec : in out GS_Node_Record);

   procedure Set_Bool_Flag_1        (For_Node_Rec : in out GS_Node_Record);
   procedure Set_Bool_Flag_2        (For_Node_Rec : in out GS_Node_Record);
   procedure Set_Bool_Flag_3        (For_Node_Rec : in out GS_Node_Record);
   procedure Set_Bool_Flag_4        (For_Node_Rec : in out GS_Node_Record);
   procedure Set_Bool_Flag_5        (For_Node_Rec : in out GS_Node_Record);
   procedure Set_Bool_Flag_6        (For_Node_Rec : in out GS_Node_Record);
   procedure Set_Bool_Flag_7        (For_Node_Rec : in out GS_Node_Record);
   procedure Set_Application_Flag_1 (For_Node_Rec : in out GS_Node_Record);
   --  Procedures for updating the node record fields.

   ------------------------------------------------------------------------

   procedure Add_Link_To_SLOC_List (For_Node_Rec : in out GS_Node_Record) is
      Tmp_SLOC_Cursor : SLOC_Node_Lists.Cursor;
      Tmp_Cursor      : Node_Lists.Cursor;
      Tmp_Boolean     : Boolean;
      pragma Warnings (Off, Tmp_SLOC_Cursor);
      pragma Warnings (Off, Tmp_Cursor);
      pragma Warnings (Off, Tmp_Boolean);
   begin

      case SLOC_Link_List_Type_Tmp is
         when Calls =>
            SLOC_Node_Lists.Insert
             (Container => For_Node_Rec.SLOC_Node_List_1,
              New_Item  => SLOC_Link_Tmp,
              Position  => Tmp_SLOC_Cursor,
              Inserted  => Tmp_Boolean);
         when Dispatching_Calls =>
            SLOC_Node_Lists.Insert
             (Container => For_Node_Rec.Dispatching_Calls,
              New_Item  => SLOC_Link_Tmp,
              Position  => Tmp_SLOC_Cursor,
              Inserted  => Tmp_Boolean);
--         when All_Calls =>
--            Node_Lists.Insert
--             (Container => For_Node_Rec.All_Calls_Chain,
--              New_Item  => Link_Tmp,
--              Position  => Tmp_Cursor,
--              Inserted  => Tmp_Boolean);
         when Direct_Read_References =>
            SLOC_Node_Lists.Insert
              (Container => For_Node_Rec.SLOC_Node_List_2,
               New_Item  => SLOC_Link_Tmp,
               Position  => Tmp_SLOC_Cursor,
               Inserted  => Tmp_Boolean);
         when Direct_Write_References =>
            SLOC_Node_Lists.Insert
              (Container => For_Node_Rec.SLOC_Node_List_3,
               New_Item  => SLOC_Link_Tmp,
               Position  => Tmp_SLOC_Cursor,
               Inserted  => Tmp_Boolean);
         when Indirect_Read_References =>
            Node_Lists.Insert
              (Container => For_Node_Rec.Node_List_5,
               New_Item  => SLOC_Link_Tmp.Node,
               Position  => Tmp_Cursor,
               Inserted  => Tmp_Boolean);
         when Indirect_Write_References =>
            Node_Lists.Insert
              (Container => For_Node_Rec.Node_List_6,
               New_Item  => SLOC_Link_Tmp.Node,
               Position  => Tmp_Cursor,
               Inserted  => Tmp_Boolean);
      end case;

   end Add_Link_To_SLOC_List;

   procedure Set_Bool_Flag_1 (For_Node_Rec : in out GS_Node_Record) is
   begin
      For_Node_Rec.Bool_Flag_1 := Bool_Tmp;
   end Set_Bool_Flag_1;

   procedure Set_Bool_Flag_2 (For_Node_Rec : in out GS_Node_Record) is
   begin
      For_Node_Rec.Bool_Flag_2 := Bool_Tmp;
   end Set_Bool_Flag_2;

   procedure Set_Bool_Flag_3 (For_Node_Rec : in out GS_Node_Record) is
   begin
      For_Node_Rec.Bool_Flag_3 := Bool_Tmp;
   end Set_Bool_Flag_3;

   procedure Set_Bool_Flag_4 (For_Node_Rec : in out GS_Node_Record) is
   begin
      For_Node_Rec.Bool_Flag_4 := Bool_Tmp;
   end Set_Bool_Flag_4;

   procedure Set_Bool_Flag_5 (For_Node_Rec : in out GS_Node_Record) is
   begin
      For_Node_Rec.Bool_Flag_5 := Bool_Tmp;
   end Set_Bool_Flag_5;

   procedure Set_Bool_Flag_6 (For_Node_Rec : in out GS_Node_Record) is
   begin
      For_Node_Rec.Bool_Flag_6 := Bool_Tmp;
   end Set_Bool_Flag_6;

   procedure Set_Bool_Flag_7 (For_Node_Rec : in out GS_Node_Record) is
   begin
      For_Node_Rec.Bool_Flag_7 := Bool_Tmp;
   end Set_Bool_Flag_7;

   procedure Set_Application_Flag_1 (For_Node_Rec : in out GS_Node_Record) is
   begin
      For_Node_Rec.Application_Flag_1 := Bool_Tmp;
   end Set_Application_Flag_1;

   procedure Set_Hash_Link (For_Node_Rec : in out GS_Node_Record) is
   begin
      For_Node_Rec.Hash_Link := GS_Node_Tmp;
   end Set_Hash_Link;

   procedure Set_Is_Of_No_Interest (For_Node_Rec : in out GS_Node_Record) is
   begin
      For_Node_Rec.Is_Of_No_Interest := Bool_Tmp;
   end Set_Is_Of_No_Interest;

   procedure Set_Scope_Level (For_Node_Rec : in out GS_Node_Record) is
   begin
      For_Node_Rec.Scope_Level := Int_Tmp;
   end Set_Scope_Level;

   ------------------------------------------------------------------------

   procedure Add_Link_To_SLOC_List
     (To_Node     : GS_Node_Id;
      Link_To_Add : SLOC_Link;
      To_List     : SLOC_Link_List_Types := Calls)
   is
   begin

      SLOC_Link_Tmp           := Link_To_Add;
      SLOC_Link_List_Type_Tmp := To_List;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => To_Node,
         Process   => Add_Link_To_SLOC_List'Access);
   end Add_Link_To_SLOC_List;

   procedure Set_Bool_Flag_1 (N : GS_Node_Id; Val : Boolean) is
   begin
      Bool_Tmp := Val;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Set_Bool_Flag_1'Access);
   end Set_Bool_Flag_1;

   procedure Set_Bool_Flag_2 (N : GS_Node_Id; Val : Boolean) is
   begin
      Bool_Tmp := Val;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Set_Bool_Flag_2'Access);
   end Set_Bool_Flag_2;

   procedure Set_Bool_Flag_3 (N : GS_Node_Id; Val : Boolean) is
   begin
      Bool_Tmp := Val;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Set_Bool_Flag_3'Access);
   end Set_Bool_Flag_3;

   procedure Set_Bool_Flag_4 (N : GS_Node_Id; Val : Boolean) is
   begin
      Bool_Tmp := Val;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Set_Bool_Flag_4'Access);
   end Set_Bool_Flag_4;

   procedure Set_Bool_Flag_5 (N : GS_Node_Id; Val : Boolean) is
   begin
      Bool_Tmp := Val;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Set_Bool_Flag_5'Access);
   end Set_Bool_Flag_5;

   procedure Set_Bool_Flag_6 (N : GS_Node_Id; Val : Boolean) is
   begin
      Bool_Tmp := Val;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Set_Bool_Flag_6'Access);
   end Set_Bool_Flag_6;

   procedure Set_Bool_Flag_7 (N : GS_Node_Id; Val : Boolean) is
   begin
      Bool_Tmp := Val;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Set_Bool_Flag_7'Access);
   end Set_Bool_Flag_7;

   procedure Set_Application_Flag_1 (N : GS_Node_Id; Val : Boolean) is
   begin
      Bool_Tmp := Val;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Set_Application_Flag_1'Access);
   end Set_Application_Flag_1;

   procedure Set_Hash_Link
     (N             : GS_Node_Id;
      Val           : GS_Node_Id)
   is
   begin
      GS_Node_Tmp := Val;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Set_Hash_Link'Access);
   end Set_Hash_Link;

   procedure Set_Is_Of_No_Interest (N : GS_Node_Id; Val : Boolean := True) is
   begin
      Bool_Tmp := Val;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Set_Is_Of_No_Interest'Access);
   end Set_Is_Of_No_Interest;

   procedure Set_Scope_Level (N : GS_Node_Id; Val : Positive) is
   begin
      Int_Tmp := Val;

      GS_Nodes_Container.Update_Element
        (Container => GS_Nodes_Table,
         Index     => N,
         Process   => Set_Scope_Level'Access);

   end Set_Scope_Level;

begin

   Environment_Task_Node_Rec :=
     (Node_Kind                 => Environment_Task,
      SLOC                      => Nil_String_Loc,
      Name                      => Enter_String ("__Environment_Task"),
      Enclosing_Scope           => No_Scope,
      Scope_Level               => 0,
      Source_File               => No_SF_Id,
      Hash_Link                 => No_GS_Node,
      Is_RTL_Node               => False,
      Is_Of_No_Interest         => False,
      Bool_Flag_1               => True,           --  Is_Body_Analyzed
      Bool_Flag_2               => False,          --  Is_Renaming
      Bool_Flag_3               => False,          --  Is_Task_Type
      Bool_Flag_4               => False,          --  Is_Dispatching_Operation
      Bool_Flag_5               => False,          --  Is_Abstract_Subprogram
      Bool_Flag_6               => False,          --  Is_Implicit_Subprogram
      Bool_Flag_7               => False,          --  Missing_Body_Reported
      Application_Flag_1        => False,
      SLOC_Node_List_1          => SLOC_Node_Lists.Empty_Set, --  Direct_Calls
      SLOC_Node_List_2          => SLOC_Node_Lists.Empty_Set, --  Direct reads
      SLOC_Node_List_3          => SLOC_Node_Lists.Empty_Set, --  Direct writes
      Dispatching_Calls         => SLOC_Node_Lists.Empty_Set, -- Disp calls
      Node_List_1               => Node_Lists.Empty_Set,      --  All_Calls
      Node_List_2               => Node_Lists.Empty_Set, --  Direct disp calls
      Node_List_3               => Node_Lists.Empty_Set, --  Directly impl spbs
      Node_List_4               => Node_Lists.Empty_Set, --  All impl spbs
      Node_List_5               => Node_Lists.Empty_Set, --  Indirect reads
      Node_List_6               => Node_Lists.Empty_Set  --  Indirect writes
     );

end ASIS_UL.Global_State;
