------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                            A 4 G . S T A N D                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1999-2015, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your option)  any later --
-- version.  ASIS-for-GNAT  is  distributed  in  the  hope  that it will be --
-- useful,  but  WITHOUT ANY WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have  received  a copy of the  GNU General Public License and --
-- a copy of the  GCC Runtime Library Exception  distributed with GNAT; see --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Asis.Set_Get; use Asis.Set_Get;
with A4G.A_Types;  use A4G.A_Types;
with A4G.Contt;    use A4G.Contt;

with Stand;        use Stand;
with Atree;        use Atree;
with Sinfo;        use Sinfo;

package body A4G.Stand is

   --------------------------------
   -- Get_Numeric_Error_Renaming --
   --------------------------------

   function Get_Numeric_Error_Renaming return Asis.Element is
      Result : Asis.Element := Numeric_Error_Template;
   begin
      Set_Encl_Tree         (Result, Get_Current_Tree);
      Set_Enclosing_Context (Result, Get_Current_Cont);
      Set_Obtained          (Result, A_OS_Time);

      return Result;
   end Get_Numeric_Error_Renaming;

   ---------------------------
   -- Is_Standard_Char_Type --
   ---------------------------

   function Is_Standard_Char_Type (N : Node_Id) return Boolean is
      Result   : Boolean := False;
      Type_Ent : Entity_Id;
   begin
      if Sloc (N)  = Standard_Location and then
         Nkind (N) = N_Enumeration_Type_Definition
      then
         Type_Ent := Defining_Identifier (Parent (N));

         if Type_Ent in Standard_Character .. Standard_Wide_Character then
            Result := True;
         end if;

      end if;

      return Result;
   end Is_Standard_Char_Type;

   -------------------------
   -- Standard_Char_Decls --
   -------------------------

   function Standard_Char_Decls
     (Type_Definition : Asis.Type_Definition;
      Implicit        : Boolean := False)
      return Asis.Element_List
   is
      Arg_Node     : constant Node_Id := Node (Type_Definition);
      Rel_Len      : Asis.ASIS_Positive;
      Type_Ent     : Entity_Id;
      Tmp_Template : Element := Char_Literal_Spec_Template;
   begin
      --  Adjusting the template for the artificial character literal
      --  specification:

      Set_Encl_Unit_Id      (Tmp_Template, Encl_Unit_Id (Type_Definition));
      Set_Encl_Tree         (Tmp_Template, Encl_Tree (Type_Definition));
      Set_Node              (Tmp_Template, Arg_Node);
      Set_R_Node            (Tmp_Template, Arg_Node);
      Set_Enclosing_Context (Tmp_Template, Encl_Cont_Id (Type_Definition));
      Set_Obtained          (Tmp_Template, A_OS_Time);
      Set_From_Instance     (Tmp_Template, Is_From_Instance (Type_Definition));
      Set_From_Implicit     (Tmp_Template, Implicit);
      Set_From_Inherited    (Tmp_Template, Implicit);

      if Implicit then
         Set_Node_Field_1 (Tmp_Template, Parent (Arg_Node));
      end if;

      Type_Ent := Defining_Identifier (Parent (Arg_Node));

      while Type_Ent /= Etype (Type_Ent) loop
         Type_Ent := Etype (Type_Ent);
      end loop;

      if Type_Ent = Standard_Character then
         Rel_Len := 256;
      else
         Rel_Len := 65536;
      end if;

      declare
         Result : Asis.Element_List (1 .. Rel_Len) := (others => Tmp_Template);
      begin

         for J in 1 .. Rel_Len loop
            Set_Character_Code (Result (J), Char_Code (J - 1));
         end loop;

         return Result;

      end;

   end Standard_Char_Decls;

end A4G.Stand;
