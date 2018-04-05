------------------------------------------------------------------------------
--                                                                          --
--                       ASIS TUTORIAL COMPONENTS                           --
--                                                                          --
--           A C T U A L S _ F O R _ T R A V E R S I N G . P R E _ O P      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (c) 2000, Free Software Foundation, Inc.            --
--                                                                          --
-- ASIS  Application  Templates are  free software; you can redistribute it --
-- and/or  modify it under  terms  of the  GNU  General  Public  License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option) any later version. ASIS Application Templates are distributed in --
-- the hope that they will be useful, but  WITHOUT  ANY  WARRANTY; without  --
-- even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR --
-- PURPOSE. See the GNU General Public License for more details. You should --
-- have  received a copy of the GNU General Public License distributed with --
-- distributed  with  GNAT;  see  file  COPYING. If not, write to the Free  --
-- Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, --
-- USA.                                                                     --
--                                                                          --
-- ASIS Tutorial was developed and are now maintained by Ada Core           --
-- Technologies Inc (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This is the body of Pre_Op to be used as an example of the style
--  checker solution (Task 2) built on top of the ASIS Application Templates
--  provided in ASIS-for-GNAT. This file is supposed to replace the file with
--  the same name which is a part of the ASIS Application Templates
--
--  This Ada unit is obtained as an "extension" of the code which is the
--  solution for Task 1

with Ada.Wide_Text_IO;
with Ada.Characters.Handling;
with Ada.Exceptions;

with Asis.Exceptions;
with Asis.Errors;
with Asis.Implementation;
with Asis.Elements;
with Asis.Declarations;
with Asis.Expressions;

with Style_Checker_Utilities;

separate (Actuals_For_Traversing)
procedure Pre_Op
  (Element :        Asis.Element;
   Control : in out Asis.Traverse_Control;
   State   : in out Traversal_State)
is
   Argument_Kind             : Asis.Element_Kinds;
   Argument_Declaration_Kind : Asis.Declaration_Kinds;

   Argument_Association_Kind : Asis.Association_Kinds;
   --  Added for Task 2

begin
   --  Note, that the code below may be rewritten in more compact way (with
   --  the same functionality). But we prefer to go step-by-step,
   --  demonstrating the important ASIS queries

   Argument_Kind := Asis.Elements.Element_Kind (Element);

   case Argument_Kind is

      when Asis.An_Association =>
         --  The first rule added by Task 1 is about generic associations,
         --  so we have to add one more alternative to the external case
         --  statement - for An_Association Element_Kinds value.

         --  Inside this alternative you first have to define the exact
         --  association kind:

         Argument_Association_Kind := Asis.Elements.Association_Kind (Element);

         --  and for A_Generic_Association Element:

         case Argument_Association_Kind is

            when Asis.A_Generic_Association =>
               --  you have to check that they are in named form.
               --  In ASIS terms this means, that the result of
               --  Asis.Expressions.Formal_Parameter query applied
               --  to the association Element is not Nil

               if Asis.Elements.Is_Nil
                  (Asis.Expressions.Formal_Parameter (Element))
               then
                  Style_Checker_Utilities.Report_Style_Violation
                    (The_Element => Element,
                     Diagnosis   => "Positional generic association");
               end if;

            when others =>
               --  Nothing to do with other association kinds, so
               null;
         end case;

      when Asis.A_Declaration =>

         --  The second rule added by Task 2 is about declarations in general.
         --  Actually, it have to be checked only for declarations which can
         --  define more then one entity (such as object declarations and
         --  parameter declarations), and this rule is always true for other
         --  declarations (such as package declarations, type declarations
         --  etc.) But to simplify the code needed to check this rule, we may
         --  check it for all declaration kinds. The check itself is very
         --  simple - we have to get the list of the names defined by a
         --  given declaration (see the query Asis.Declarations.Names) and
         --  check how many they are.

         if Asis.Declarations.Names (Element)'Length >= 2 then
            Style_Checker_Utilities.Report_Style_Violation
              (The_Element => Element,
               Diagnosis   => "Declaration with more then one name");
         end if;

         --  The following check came from Task 1
         --  The rule to check is about a specific kinds of declarations
         --  only - we have to check that each subprogram body has a separate
         --  spec. So we have first to define a more specific declaration
         --  kind of the argument Element and then we have to check our rule
         --  for subprogram bodies only

         Argument_Declaration_Kind := Asis.Elements.Declaration_Kind (Element);

         case Argument_Declaration_Kind is

            when Asis.A_Procedure_Body_Declaration |
                 Asis.A_Function_Body_Declaration  =>

               if Asis.Elements.Is_Nil
                  (Asis.Declarations.Corresponding_Declaration (Element))
               then
                  Style_Checker_Utilities.Report_Style_Violation
                    (The_Element => Element,
                     Diagnosis   => "Subprogram body with no explicit spec");
               end if;

            when others =>
               --  For the other declaration kinds we have nothing to check,
               --  so:
               null;
         end case;

      when others =>
         --  Our the only rule for Task 1 is about declarations, so if
         --  we have something which is not A_Declaration, we have nothing
         --  to do
         null;
   end case;

exception

   when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
             Asis.Exceptions.ASIS_Inappropriate_Container        |
             Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
             Asis.Exceptions.ASIS_Inappropriate_Element          |
             Asis.Exceptions.ASIS_Inappropriate_Line             |
             Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
             Asis.Exceptions.ASIS_Failed                         =>

      Ada.Wide_Text_IO.Put ("Pre_Op : ASIS exception (");

      Ada.Wide_Text_IO.Put (Ada.Characters.Handling.To_Wide_String (
              Ada.Exceptions.Exception_Name (Ex)));

      Ada.Wide_Text_IO.Put (") is raised");
      Ada.Wide_Text_IO.New_Line;

      Ada.Wide_Text_IO.Put ("ASIS Error Status is ");

      Ada.Wide_Text_IO.Put
        (Asis.Errors.Error_Kinds'Wide_Image (Asis.Implementation.Status));

      Ada.Wide_Text_IO.New_Line;

      Ada.Wide_Text_IO.Put ("ASIS Diagnosis is ");
      Ada.Wide_Text_IO.New_Line;
      Ada.Wide_Text_IO.Put (Asis.Implementation.Diagnosis);
      Ada.Wide_Text_IO.New_Line;

      Asis.Implementation.Set_Status;

   when Ex : others =>

      Ada.Wide_Text_IO.Put ("Pre_Op : ");

      Ada.Wide_Text_IO.Put (Ada.Characters.Handling.To_Wide_String (
              Ada.Exceptions.Exception_Name (Ex)));

      Ada.Wide_Text_IO.Put (" is raised (");

      Ada.Wide_Text_IO.Put (Ada.Characters.Handling.To_Wide_String (
              Ada.Exceptions.Exception_Information (Ex)));

      Ada.Wide_Text_IO.Put (")");
      Ada.Wide_Text_IO.New_Line;

end Pre_Op;