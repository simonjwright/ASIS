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

--  This is the body of Pre_Op to be used as an example of the metrics tool
--  solution (Task 1) built on top of the ASIS Application Templates
--  provided in ASIS-for-GNAT. This file is supposed to replace the file with
--  the same name which is a part of the ASIS Application Templates

with Ada.Wide_Text_IO;
with Ada.Characters.Handling;
with Ada.Exceptions;

with Asis.Exceptions;
with Asis.Errors;
with Asis.Implementation;
with Asis.Elements;

with Metrics_Utilities;

separate (Actuals_For_Traversing)
procedure Pre_Op
  (Element :        Asis.Element;
   Control : in out Asis.Traverse_Control;
   State   : in out Traversal_State)
is
   Argument_Kind             : Asis.Element_Kinds;
begin
   --  Note, that the code below may be rewritten in more compact way (with
   --  the same functionality). But we prefer to go step-by-step,
   --  demonstrating the important ASIS queries

   Argument_Kind := Asis.Elements.Element_Kind (Element);

   case Argument_Kind is

      when Asis.A_Statement =>
         --  We have to compute the total number of all the statements, so:
         Metrics_Utilities.Total_Statements :=
            Metrics_Utilities.Total_Statements + 1;

      when Asis.A_Declaration =>
         --  We have to compute the total number of all the declarations, so:
         Metrics_Utilities.Total_Declarations :=
            Metrics_Utilities.Total_Declarations + 1;

      when others =>
         --  Our metrics for Task 1 are about declarations and statements,
         --  so if we have something else, we have nothing to do.
         null;
   end case;

   ----------------------
   -- Hints for Task 2 --
   ----------------------

   --  Task 2 adds three new metrics to compute.
   --
   --  The first two metrics (computing the simple statements and the compound
   --  statements) can be implemented by detecting the subordinate statement
   --  kind in case if the argument being visited is a statement and
   --  increasing the corresponding metric counter.
   --
   --  To compute the last metric (the total number of all the explicitly
   --  declared names), we have to take into account that some declarations
   --  may define more then one name. Therefore for each declaration we have
   --  to get the list of the declared names and to increase the corresponding
   --  metric counter by the number of the names in this list (see the query
   --  Asis.Declarations.Names).
   --
   --  Another approach could be to count all the Elements of A_Defining_Name
   --  kind, but in this case we will have to be aware, that each Element of
   --  A_Defining_Expanded_Name kind contains the subcomponnet of
   --  A_Defining_Identifier (or A_Defining_Operator_Symbol) kind as its
   --  selector component, so we should not count A_Defining_Expanded_Name
   --  Elements, otherwise defining program unit names which have the form
   --  of expanded names would be counted twice.
   --
   --  Any of these two possibilities could be taken. In the solution
   --  presented in the 'task_2' subdirectory the first approach is used.
   --
   --  These comments are kept in slightly modified form in the solution we
   --  provide for Task 2 as comments to the corresponding fragments of the
   --  code

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