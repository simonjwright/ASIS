------------------------------------------------------------------------------
--                                                                          --
--                  COMMON ASIS TOOLS COMPONENTS LIBRARY                    --
--                                                                          --
--           A S I S _ U L . G L O B A L _ S T A T E . D A T A              --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2010-2016, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not,  write  to the  Free Software Foundation,  51 Franklin  --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;    use Ada.Characters.Handling;

with Asis.Declarations;          use Asis.Declarations;
with Asis.Elements;              use Asis.Elements;
with Asis.Exceptions;            use Asis.Exceptions;
with Asis.Expressions;           use Asis.Expressions;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;
with Asis.Statements;            use Asis.Statements;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Extensions.Strings;    use Asis.Extensions.Strings;

with Asis.Set_Get;               use Asis.Set_Get;

with Atree;                      use Atree;
with Sinfo;                      use Sinfo;
with Einfo;                      use Einfo;

with ASIS_UL.Utilities;          use ASIS_UL.Utilities;

package body ASIS_UL.Global_State.Data is

   -------------------
   -- Is_Global_For --
   -------------------

   function Is_Global_For
     (Scope : Scope_Id;
      Node  : GS_Node_Id)
      return  Boolean
   is
      Node_Encl_Scope  : constant GS_Node_Id := GS_Node_Enclosing_Scope (Node);
      Node_Scope_Level : constant Positive   :=
        GS_Node_Scope_Level (Node_Encl_Scope);

      Scope_Encl_Scope : GS_Node_Id;
      Result           : Boolean := False;
   begin

      pragma Assert (GS_Node_Scope_Level (Scope) > 0);

      if GS_Node_Kind (Node_Encl_Scope) in Global_Nodes then
         --  If variable is defined in a global scope (package or outter task
         --  environment), it is global for any scope
         Result := True;
      elsif Node_Scope_Level < GS_Node_Scope_Level (Scope) then
         Scope_Encl_Scope := GS_Node_Enclosing_Scope (Scope);

         while GS_Node_Scope_Level (Scope_Encl_Scope) /= Node_Scope_Level loop
            Scope_Encl_Scope := GS_Node_Enclosing_Scope (Scope_Encl_Scope);
         end loop;

         Result := Scope_Encl_Scope = Node_Encl_Scope;

      end if;

      return Result;
   end Is_Global_For;

   ---------------------------------
   -- Is_Global_For_Current_Scope --
   ---------------------------------

   function Is_Global_For_Current_Scope
     (Def_Name : Asis.Element)
      return     Boolean
   is
      Result          :          Boolean := True;
      Encl_Scope_Node :          Node_Id := Scope (Node (Def_Name));
      Curr_Scope_Node : constant Node_Id := Current_Scope_Tree_Node;
   begin

      --  If the enclosing scope is a package or package body, all variables
      --  defined in the package should be considered as global, because their
      --  lifetime extends to the complete program execution

      if Ekind (Encl_Scope_Node) = E_Package or else
        Ekind (Encl_Scope_Node) = E_Generic_Package or else
        Ekind (Encl_Scope_Node) = E_Package_Body
      then

         if Encl_Scope_Node = Curr_Scope_Node then
            return True;
         end if;

      end if;

      --  If Enclosing_Scope is a single task declaration, we may have to
      --  adjust Encl_Scope_Node: for local entities declared in the package
      --  body it will point to the  artificial task type entity:

      if Ekind (Encl_Scope_Node) = E_Task_Type
        and then
         not Comes_From_Source (Encl_Scope_Node)
      then
         Encl_Scope_Node := Corresponding_Body (Parent (Encl_Scope_Node));
      end if;

      while Present (Encl_Scope_Node) loop

         if Encl_Scope_Node = Curr_Scope_Node then
            Result := False;
            exit;
         end if;

         Encl_Scope_Node := Scope (Encl_Scope_Node);
      end loop;

      return Result;
   end Is_Global_For_Current_Scope;

   ---------------------
   -- Store_Reference --
   ---------------------

   procedure Store_Reference
     (N              : GS_Node_Id;
      At_SLOC        : String_Loc;
      Reference_Kind : Reference_Kinds)
   is
   begin

      if Reference_Kind = Read
        or else
         Reference_Kind = Read_Write
      then
         Add_Link_To_SLOC_List
           (To_Node     => Current_Scope,
            To_List     => Direct_Read_References,
            Link_To_Add => (Node => N, SLOC => At_SLOC));

--           if GS_Node_Kind (Current_Scope) in Task_Nodes
--             or else
--              GS_Is_Foreign_Thread (Current_Scope)
--           then
--              Add_Node_To_List
--                (To_Node     => N,
--                 To_List     => Direct_Read_References,
--                 Link_To_Add => (Node => Current_Scope, SLOC => At_SLOC));
--           end if;

      end if;

      if Reference_Kind = Write
        or else
         Reference_Kind = Read_Write
      then
         Add_Link_To_SLOC_List
           (To_Node     => Current_Scope,
            To_List     => Direct_Write_References,
            Link_To_Add => (Node => N, SLOC => At_SLOC));

--           if GS_Node_Kind (Current_Scope) in Task_Nodes
--             or else
--              GS_Is_Foreign_Thread (Current_Scope)
--           then
--              Add_Node_To_List
--                (To_Node     => N,
--                 To_List     => Direct_Write_References,
--                 Link_To_Add => (Node => Current_Scope, SLOC => At_SLOC));
--           end if;

      end if;

   end Store_Reference;

   ------------------------
   --  Local subprograms --
   ------------------------

   function Get_Reference_Kind
     (Identifier : Asis.Element)
      return       Reference_Kinds;
   --  Checks if Identifier (that is supposed to be An_Identifier) Element is
   --  read, write or read-write reference. Returns Not_A_Reference if
   --  Identifier is not of An_Identifier kind.
   --
   --  This function does not check if Identifier is indeed a reference to a
   --  data object, this should be checked before the call.

   -------------------------------
   -- Check_If_Global_Reference --
   -------------------------------

   procedure Check_If_Global_Reference
     (Element                       :     Asis.Element;
      Definition                    : out Asis.Element;
      Is_Global_Reference           : out Boolean;
      Can_Be_Accessed_By_Local_Task : out Boolean;
      Reference_Kind                : out Reference_Kinds;
      Compute_Reference_Kind        :     Boolean := False)
   is
      Tmp : Asis.Element;
--        Decl_Element : Asis.Element;
   begin
      --  This implementation does not care very much about performance...

      Is_Global_Reference           := False;
      Can_Be_Accessed_By_Local_Task := False;
      Reference_Kind                := Not_A_Reference;

      begin
         if Flat_Element_Kind (Element) = A_Defining_Identifier then
            --  For a variable declaration, the definition IS the element
            Definition := Element;
         else
            Definition := Corresponding_Name_Definition (Element);
         end if;
      exception
         when ASIS_Inappropriate_Element =>
            --  El is definitely not a reference to a variable!
            return;
      end;

      if Defining_Name_Kind (Definition) /= A_Defining_Identifier
        or else
         Nkind (Node (Definition)) /= N_Defining_Identifier --  statememt names
        or else
          (Ekind (Node (Definition)) /= E_Variable and then
             Ekind (Node (Definition)) /= E_Generic_In_Parameter and then
             Ekind (Node (Definition)) /= E_Generic_In_Out_Parameter and then
             Ekind (Node (Definition)) not in Formal_Kind)
      then
         --  This is also not a variable reference for sure
         return;
      end if;

      --  Formal parameters are not enclosed in a surrounding declaration.
      --  Treat them like variable declarations.
      if Ekind (Node (Definition)) in Formal_Kind then
         Is_Global_Reference :=
           (Is_Global_For_Current_Scope (Definition));
      else

         Tmp := Enclosing_Element (Definition);

         case Declaration_Kind (Tmp) is
         when A_Variable_Declaration |
              A_Formal_Object_Declaration =>

--              if not (Is_Concurrent (Definition)
--                 --  We do not count references to task or protected objects.
--                    or else
--                      Gnatcheck.ASIS_Utilities.Is_Volatile (Definition)
--                    or else
--                      Is_Atomic (Definition)
--                    or else
--                      Is_Reference_To_Councurrent_Component (Element))
--              then
            Is_Global_Reference :=
              (Is_Global_For_Current_Scope (Definition));

--              if not Is_Global_Reference then
--                 Can_Be_Accessed_By_Local_Task :=
--                   Can_Be_Accessed_By_Enclosed_Tasks (Tmp);
--              end if;

--              end if;

         when An_Object_Renaming_Declaration =>

            --  We have to unwind the renaming in order to detect what data
            --  object is really referenced. There are two specal situations
            --  here:
            --
            --  1. The renamed object is a function call or a component
            --     thereof. In this case we have a constant declaration, we
            --     do not store this as a reference.
            --
            --  2. When unwinding renamings, we may go through some access
            --     value(s). But here we do not care about indirect access
            --     through the access values, the corresponding diagnostic
            --     should be generated separately.

            --  We have to unwind renaming by recursive calls to this
            --  procedure, because Corresponding_Base_Entity stops if the
            --  renaming object is a component of another object

            Tmp := Corresponding_Base_Entity (Tmp);

            case Expression_Kind (Tmp) is

               when An_Identifier =>
                  null;
               when An_Explicit_Dereference |
                    An_Indexed_Component    |
                    A_Slice                 |
                    An_Attribute_Reference  =>
                  Tmp := Prefix (Tmp);

               when A_Type_Conversion  =>
                  Tmp := Converted_Or_Qualified_Expression (Tmp);

               when A_Selected_Component =>
                  --  In case of A.B we may have a component of A or an
                  --  expanded name of B

                  if Is_Component (Tmp) then
                     Tmp := Prefix (Tmp);
                  else
                     Tmp := Selector (Tmp);
                  end if;

               when others =>
                  --  Is_Global_Reference is False.
                  --  Here we have either impossible cases (such as an
                  --  aggregate) or cases that make this renaming a constant
                  --  declaration (such as a function call or an enumeration
                  --  literal). So:
                  return;
            end case;

            Check_If_Global_Reference
              (Element                       => Tmp,
               Definition                    => Definition,
               Is_Global_Reference           => Is_Global_Reference,
               Can_Be_Accessed_By_Local_Task => Can_Be_Accessed_By_Local_Task,
               Reference_Kind                => Reference_Kind);

         when A_Constant_Declaration           |
               --  we care about variables only!
              A_Choice_Parameter_Specification |
              A_Single_Task_Declaration        |
              A_Single_Protected_Declaration   =>
            Is_Global_Reference := False;
         when others =>
            pragma Assert (False);
            null;
         end case;

      end if;

      if (Is_Global_Reference
         or else
          Can_Be_Accessed_By_Local_Task)
        and then
          Compute_Reference_Kind
      then
         Reference_Kind := Get_Reference_Kind (Element);
      end if;

   end Check_If_Global_Reference;

   ------------------------
   -- Get_Reference_Kind --
   ------------------------

   function Get_Reference_Kind
     (Identifier : Asis.Element)
      return       Reference_Kinds
   is
      Result        : Reference_Kinds := Not_A_Reference;

      Enclosing     : Asis.Element;
      Enclosing_Old : Asis.Element := Identifier;
      --  When going up the ASIS tree,
      --  Enclosing = Enclosing_Element (Enclosing_Old)

   begin

      --  Variable declarations (at the package-level) should be counted as
      --  writes when there is an initialization expression, and not at all
      --  otherwise.
      if Flat_Element_Kind (Identifier) = A_Defining_Identifier then
         Enclosing := Enclosing_Element (Enclosing_Old);

         if Flat_Element_Kind (Enclosing) = A_Variable_Declaration or else
           Flat_Element_Kind (Enclosing) = A_Formal_Object_Declaration
         then
            if not Is_Nil (Initialization_Expression (Enclosing)) then
               Result := Write;
            end if;
            --  else the declaration is not a reference
         end if;

      elsif Expression_Kind (Identifier) = An_Identifier then
         Enclosing := Enclosing_Element (Enclosing_Old);

         loop

            case Flat_Element_Kind (Enclosing) is

               when An_Assignment_Statement =>

                  if Is_Equal
                       (Enclosing_Old, Assignment_Variable_Name (Enclosing))
                  then
                     if Expression_Kind (Enclosing_Old) = An_Identifier then
                        Result := Write;
                     else
                        --  Update to a part of an aggregate counts as
                        --  read-write (useful for SPARK generation)
                        Result := Read_Write;
                     end if;
                  else
                     Result := Read;
                  end if;

                  exit;

               when A_Parameter_Association =>
                  Enclosing_Old := Enclosing;
                  Enclosing     := Enclosing_Element (Enclosing_Old);

                  if Expression_Kind (Enclosing) = A_Function_Call then
                     Result := Read;

                  elsif Expression_Kind (Called_Name (Enclosing)) =
                          An_Attribute_Reference
                  then
                     Result := Read;
                  else
                     Enclosing :=
                       Get_Parameter_Declaration (Enclosing_Old);

                     case Mode_Kind (Enclosing) is
                        when A_Default_In_Mode |
                             An_In_Mode        =>
                           Result := Read;
                        when An_Out_Mode =>
                           Result := Write;
                        when An_In_Out_Mode =>
                           Result := Read_Write;
                        when others =>
                           null;
                           pragma Assert (False);
                     end case;

                  end if;

                  exit;
               when Flat_Expression_Kinds =>

                  case Expression_Kind (Enclosing) is
                     when An_Attribute_Reference =>

                        if Attribute_Kind (Enclosing) = An_Access_Attribute
                          or else
                           (Attribute_Kind (Enclosing) =
                              An_Implementation_Defined_Attribute
                           and then
                            To_Lower (To_String
                                      (Name_Image
                                       (Attribute_Designator_Identifier
                                                   (Enclosing)))) =
                                  "unrestricted_access")
                        then
                           --  An access value pointing to this object is
                           --  created, we have no idea how it is used, so:
                           Result := Read_Write;
                        else
                           --  For all other cases related to attributes, only
                           --  read access is possible
                           Result := Read;
                        end if;

                        exit;

                     when An_Indexed_Component =>
                        --  If is is an index value - it is a read access

                        if not Is_Equal
                          (Prefix (Enclosing), Enclosing_Old)
                        then
                           Result := Read;
                           exit;
                        end if;

                     when A_Function_Call =>
                           Result := Read;
                           exit;
                     when others =>
                        --  Continue bottom-up traversal...
                        null;
                  end case;

               when others =>
                  Result := Read;
                  exit;
            end case;

            Enclosing_Old := Enclosing;
            Enclosing     := Enclosing_Element (Enclosing_Old);
         end loop;

      end if;

      return Result;
   end Get_Reference_Kind;

   ------------------------------
   -- Process_Global_Reference --
   ------------------------------

   procedure Process_Global_Reference
     (Element                           : Asis.Element;
      Definition                        : Asis.Element;
      Reference_Kind                    : Reference_Kinds)
--        Local_Var_Accessed_By_Local_Tasks : Boolean)
   is
      Encl_Element : constant Asis.Element :=
                       Enclosing_Element (Enclosing_Element (Definition));
      Encl_Scope   : Scope_Id;
      Def_Node     : GS_Node_Id;
   begin
      --  If the enclosing scope is a package or package body, use it

      if Flat_Element_Kind (Encl_Element) = A_Package_Declaration
        or else Flat_Element_Kind (Encl_Element)
        = A_Generic_Package_Declaration
        or else Flat_Element_Kind (Encl_Element) = A_Package_Body_Declaration
      then
         Encl_Scope := Corresponding_Node (Encl_Element);
      else
         Encl_Scope := No_Scope;
      end if;

      Def_Node := Corresponding_Node (Definition, Encl_Scope);

      pragma Assert (Present (Def_Node));
      pragma Assert (Reference_Kind /= Not_A_Reference);

--        if Local_Var_Accessed_By_Local_Tasks then
--           Set_Is_Local_Var_Accessed_By_Local_Tasks (Def_Node);
--        end if;

      Store_Reference
        (N              => Def_Node,
         At_SLOC        => Build_GNAT_Location (Element),
         Reference_Kind => Reference_Kind);
   end Process_Global_Reference;

end ASIS_UL.Global_State.Data;
