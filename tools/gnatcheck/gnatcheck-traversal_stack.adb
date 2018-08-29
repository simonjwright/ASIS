------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--            G N A T C H E C K . T R A V E R S A L _ S T A C K             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

with Asis;          use Asis;
with Asis.Elements; use Asis.Elements;

with GNAT.Table;

package body Gnatcheck.Traversal_Stack is

   ----------------------
   --  Traversal stack --
   ----------------------

   type Traversal_Step is record
      Element : Asis.Element;

      --  Flags:
      Flag_1 : Boolean;

      --  Fields:
      --  No fields for now
   end record;

   -------------------------------
   -- Usage of flags and fields --
   -------------------------------

   --  ???

   --  Flag_1    A_Function_Body_Declaration    ON if a return statement is
   --                                           encountered for this body

   package Stack_Table is new GNAT.Table
     (Table_Component_Type => Traversal_Step,
      Table_Index_Type     => Elmt_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 100,
      Table_Name           => "Traversal Stack");

   Stack : Stack_Table.Table_Ptr renames Stack_Table.Table;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Is_Empty return Boolean;
   --  Checks if the traversal stack is empty

   -------------------------------
   -- Enclosing_Subprogram_Body --
   -------------------------------

   function Enclosing_Subprogram_Body return Elmt_Idx is
      Result : Elmt_Idx := No_Element;
   begin

      for J in reverse 1 .. Stack_Table.Last loop

         if Declaration_Kind (Stack (J).Element) in
           A_Procedure_Body_Declaration .. A_Function_Body_Declaration
           or else
            Declaration_Kind (Stack (J).Element) = An_Entry_Body_Declaration
           or else
            Statement_Kind (Stack (J).Element) = An_Accept_Statement
         then
            Result := J;
            exit;
         end if;

      end loop;

      return Result;
   end Enclosing_Subprogram_Body;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (From_Idx : Elmt_Idx) return Asis.Element is
   begin

      if From_Idx not in 1 .. Stack_Table.Last then
         raise Stack_Error;
      else
         return Stack (From_Idx).Element;
      end if;

   end Get_Element;

   ---------------------------
   -- Get_Enclosing_Element --
   ---------------------------

   function Get_Enclosing_Element
     (Steps_Up : Elmt_Idx := 0)
      return     Asis.Element
   is
   begin

      if Steps_Up = 0 and then Is_Empty then
         return Nil_Element;
      elsif Steps_Up >= Stack_Table.Last then
         raise Stack_Error;
      else
         return Stack (Stack_Table.Last - Steps_Up).Element;
      end if;

   end Get_Enclosing_Element;

   ----------------
   -- Has_Return --
   ----------------

   function Has_Return (El : Elmt_Idx) return Boolean is
   begin
      pragma Assert (Declaration_Kind (Stack (El).Element) =
                     A_Function_Body_Declaration);
      return Stack (El).Flag_1;
   end Has_Return;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Stack_Table.Init;
      Push (Nil_Element);
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty return Boolean is
   begin
      return Stack_Table.Last = 0;
   end Is_Empty;

   ---------
   -- Pop --
   ---------

   procedure Pop is
   begin

      if not Is_Empty then
         Stack_Table.Decrement_Last;
      else
         raise Stack_Error;
      end if;

   end Pop;

   -------------
   -- Present --
   -------------

   function Present (Idx : Elmt_Idx) return Boolean is
   begin
      return Idx /= No_Element;
   end Present;

   ----------
   -- Push --
   ----------

   procedure Push (E : Asis.Element) is
   begin
      Stack_Table.Append (New_Val => (Element => E, Flag_1 => False));
   end Push;

   --------------------
   -- Set_Has_Return --
   --------------------

   procedure Set_Has_Return (El : Elmt_Idx) is
   begin
      pragma Assert (Declaration_Kind (Stack (El).Element) =
                     A_Function_Body_Declaration);

      Stack (El).Flag_1 := True;
   end Set_Has_Return;

end Gnatcheck.Traversal_Stack;
