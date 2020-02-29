------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--            A S I S _ U L . P R O J E C T S . A G G R E G A T E           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2018-2019, AdaCore                    --
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

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Containers.Ordered_Sets;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;       use GNAT.OS_Lib;

with GNATCOLL.VFS;      use GNATCOLL.VFS;

with ASIS_UL.Common;    use ASIS_UL.Common;
with ASIS_UL.Misc;
with ASIS_UL.Options;   use ASIS_UL.Options;
with ASIS_UL.Output;    use ASIS_UL.Output;

package body ASIS_UL.Projects.Aggregate is

   --------------------------------------------
   --  Storage for projects being aggregated --
   --------------------------------------------

   type Project_Record is record
      Project_Path : Virtual_File := No_File;
      --  Full path to a project file

      --  What else may we need?..
   end record;

   function "=" (L, R : Project_Record) return Boolean;
   function "<" (L, R : Project_Record) return Boolean;
   --  These two functions compare project paths

   package Aggregated_Projects_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Project_Record);
   use Aggregated_Projects_Sets;

   Aggregated_Projects : Set;

   --  Project iterator - old-fasion manual iteration through the ordered set
   --  is used.
   Iterator_Done   : Boolean := True;
   Iterator_C      : Cursor  := No_Element;
   Iterator_El     : Project_Record;

   Aggregated_Prj_Name : String_Access;
   --  The name of a project file passed as a parameter of '-A' option

   ------------------------
   --  Local subprograms --
   ------------------------

   ------------
   --  Debug --
   ------------

   procedure Stored_Aggregated_Project_Debug_Info (C : Cursor);
   --  Prints out the debig info about a stored aggregated project.

   -------------
   -- "=" "<" --
   -------------

   function "=" (L, R : Project_Record) return Boolean is
   begin
      if R.Project_Path = No_File and then R.Project_Path = No_File then
         return True;
      elsif R.Project_Path = No_File or else R.Project_Path = No_File then
         return False;
      else
         return Filesystem_String'(Full_Name (L.Project_Path, True)) =
                  Full_Name (R.Project_Path, True);
      end if;
   end "=";

   function "<" (L, R : Project_Record) return Boolean is
   begin
      if L = R then
         return False;
      elsif L.Project_Path = No_File then
         return True;
      elsif R.Project_Path = No_File then
         return False;
      else
         return Filesystem_String'(Full_Name (L.Project_Path, True)) <
                  Full_Name (R.Project_Path, True);
      end if;
   end "<";

   -------------------------------------
   -- Aggregated_Projects_Debug_Image --
   -------------------------------------

   procedure Aggregated_Projects_Debug_Image is
   begin
      if Aggregated_Projects.Is_Empty then
         Info ("No aggregated projects stored");
      else
         Info ("***Stored aggregated projects debug info***");
         Aggregated_Projects.Iterate
           (Process => Stored_Aggregated_Project_Debug_Info'Access);
      end if;
   end Aggregated_Projects_Debug_Image;

   ---------------------------------
   -- Collect_Aggregated_Projects --
   ---------------------------------

   procedure Collect_Aggregated_Projects (P : Project_Type) is
      Iter : Project_Iterator := Start (Root_Project     => P,
                                        Recursive        => True,
                                        Direct_Only      => True);

      Arg_Prj_Name : constant Filesystem_String :=
        Full_Name (P.Project_Path, Normalize => True);

      New_Prj_Rec : Project_Record;

      Inserted : Boolean;
      C        : Cursor;
   begin
      if Debug_Flag_A then
         Ada.Text_IO.Put_Line (String (Arg_Prj_Name));
      end if;

      while Current (Iter) /= No_Project loop

         if Is_Aggregate_Project (Current (Iter)) then
            if Arg_Prj_Name /= Full_Name (Current (Iter).Project_Path,
                                          Normalize => True)
            then
               --  Iterator also returns the top project the iteration starts
               --  from, we should not process it again to awoid cycling
               Collect_Aggregated_Projects (Current (Iter));
            end if;
         else
            New_Prj_Rec.Project_Path :=  Current (Iter).Project_Path;

            Insert (Aggregated_Projects,
                    New_Prj_Rec,
                    C,
                    Inserted);
         end if;

         Next (Iter);
      end loop;

   end Collect_Aggregated_Projects;

   ----------------------------
   -- Get_Aggregated_Prj_Src --
   ----------------------------

   function Get_Aggregated_Prj_Src return Virtual_File is
     (Aggregated_Projects.First_Element.Project_Path);

   ----------------------------
   -- Get_Aggregated_Project --
   ----------------------------

   function Get_Aggregated_Project return String is (Aggregated_Prj_Name.all);

   -------------------
   -- Next_Prj_Name --
   -------------------

   function Next_Prj_Name return Filesystem_String_Access is
   begin
      if not Iterator_Done then
         return new Filesystem_String'(Full_Name (Iterator_El.Project_Path));
      else
         Error ("attempt to get project name for non-active iterator");
         raise Fatal_Error;
      end if;
   end Next_Prj_Name;

   --------------------------------
   -- Num_Of_Aggregated_Projects --
   --------------------------------

   function Num_Of_Aggregated_Projects return Natural is
     (Natural (Aggregated_Projects.Length));

   -----------------------
   -- Prj_Iterator_Done --
   -----------------------

   function Prj_Iterator_Done return Boolean is (Iterator_Done);

   -----------------------
   -- Prj_Iterator_Next --
   -----------------------

   procedure Prj_Iterator_Next is
   begin
      Iterator_C := Next (Iterator_C);

      if not Has_Element (Iterator_C) then
         Iterator_Done := True;
      else
         Iterator_El := Element (Iterator_C);
      end if;

   end Prj_Iterator_Next;

   ---------------------------------
   -- Process_Aggregated_Projects --
   ---------------------------------

   procedure Process_Aggregated_Projects
     (My_Project : Arg_Project_Type'Class)
   is
      Report_File_Name     : constant String := (if Text_Report_ON then
                                                     Get_Report_File_Name
                                                 else
                                                     "");
      XML_Report_File_Name : constant String := (if XML_Report_ON then
                                                    Get_XML_Report_File_Name
                                                 else
                                                    "");

      Count : Natural := 1;
      --  Counts iterations on aggregated projects, this number is used to
      --  create a uniqie name of the report files for each iteration

      Prj_Out_File_Start     : String_Access;
      Prj_XML_Out_File_Start : String_Access;
      --  Start of the name of the tool out file for each iteration. To make
      --  a unique name, we add the text image of Count after it.

      Idx : Natural;

      Prj_Args       : Argument_List (1 .. 2);
      Out_Args       : Argument_List (1 .. 4);
      Out_Args_Count : constant Integer :=
        (if Text_Report_ON and then XML_Report_ON then 4 else 2);

      Args      : Argument_List (1 .. Argument_Count);
      Arg_Count : Natural := 0;
      Skip_Next : Boolean := False;

      Exit_Code : Integer;

      Full_Tool_Name : constant String_Access :=
        Locate_Exec_On_Path (Tool_Name.all);
   begin
      if Full_Tool_Name = null then
         Error ("Cannot locate Tool_Name.all on path, possible installation " &
                "problem");
         raise Fatal_Error;
      end if;

      if Text_Report_ON then
         Idx := Index (Report_File_Name,
                       (1 => Directory_Separator),
                       Backward);

         Prj_Out_File_Start := new String'(
            (if Idx = 0 then
                ""
             else
                Report_File_Name (Report_File_Name'First .. Idx)) &
                Tool_Name.all & "_");
      end if;

      if XML_Report_ON then
         Idx := Index (XML_Report_File_Name,
                       (1 => Directory_Separator),
                       Backward);

         Prj_XML_Out_File_Start := new String'(
            (if Idx = 0 then
                ""
             else
                XML_Report_File_Name (XML_Report_File_Name'First .. Idx)) &
                Tool_Name.all & "_");
      end if;

      Prj_Args (1) := new String'("-A");
      Out_Args (1) := new String'(if Text_Report_ON then "-o" else "-ox");

      if Out_Args_Count = 4 then
         Out_Args (3) := new String'("-ox");
      end if;

      for J in 1 .. Argument_Count loop
         declare
            Arg : constant String := Argument (J);
         begin
            if Skip_Next then
               Skip_Next := False;
            else
               if Arg = "-o" or else Arg = "-ox" then
                  Skip_Next := True;
               else
                  Idx := Index (Arg, "-o=", Forward);

                  if Idx = 0 then
                     Idx := Index (Arg, "-ox=", Forward);
                  end if;

                  if Idx = 0 then
                     Arg_Count := Arg_Count + 1;

                     Args (Arg_Count) := new String'(Arg);
                  end if;
               end if;
            end if;
         end;

      end loop;

      Aggregate_Project_Report_Header (My_Project);

      Start_Prj_Iterator;

      while not Prj_Iterator_Done loop

         if Verbose_Mode then
            Info ("Processing aggregated project " &
                  String (Next_Prj_Name.all));
         end if;

         Free (Prj_Args (2));
         Prj_Args (2) := new String'(String (Next_Prj_Name.all));

         Free (Out_Args (2));

         Out_Args (2) := new String'
                           ((if Text_Report_ON then
                                Prj_Out_File_Start.all &
                                ASIS_UL.Misc.Image (Count) & ".out"
                             else
                                Prj_XML_Out_File_Start.all &
                                ASIS_UL.Misc.Image (Count) & ".xml"));

         if Out_Args_Count = 4 then
            Out_Args (4) :=
              new String'(Prj_XML_Out_File_Start.all &
                          ASIS_UL.Misc.Image (Count) & ".xml");
         end if;

         if Debug_Flag_C then
            Info ("Processing aggregated project " &
                   String (Next_Prj_Name.all));
            Info_No_EOL (Tool_Name.all);

            for J in Prj_Args'Range loop
               Info_No_EOL (" " & Prj_Args (J).all);
            end loop;

            for J in 1 .. Out_Args_Count loop
               Info_No_EOL (" " & Out_Args (J).all);
            end loop;

            for J in 1 .. Arg_Count loop
               Info_No_EOL (" " & Args (J).all);
            end loop;

            Info ("");

         end if;

         Report_Aggregated_Project
           (Aggregate_Prj          => My_Project,
            Arrgegated_Prj_Name    => Prj_Args (2).all,
            Expected_Text_Out_File => (if Text_Report_ON then
                                          Out_Args (2).all
                                       else
                                          ""),
            Expected_XML_Out_File  => (if XML_Report_ON then
                                          (if Out_Args_Count = 2 then
                                              Out_Args (2).all
                                          else
                                              Out_Args (4).all)
                                       else
                                          ""));

         Exit_Code := Spawn (Program_Name => Full_Tool_Name.all,
                             Args         => Prj_Args                       &
                                             Out_Args (1 .. Out_Args_Count) &
                                             Args (1 .. Arg_Count));

         Report_Aggregated_Project_Exit_Code
           (Aggregate_Prj       => My_Project,
            Exit_Code           => Exit_Code);

         Prj_Iterator_Next;
         Count := Count + 1;
      end loop;

      Close_Aggregate_Project_Report (My_Project);

   end Process_Aggregated_Projects;

   ------------------------
   -- Start_Prj_Iterator --
   ------------------------

   procedure Start_Prj_Iterator is
   begin
      Iterator_Done   := False;
      Iterator_C      := First (Aggregated_Projects);
      Iterator_El     := Element (Iterator_C);
   exception
      when others =>
         Error ("Cannot start iterator on aggregated projects");
         raise Fatal_Error;
   end Start_Prj_Iterator;

   ------------------------------
   -- Store_Aggregated_Project --
   ------------------------------

   procedure Store_Aggregated_Project (S : String) is
   begin
      Free (Aggregated_Prj_Name);
      Aggregated_Prj_Name := new String'(S);
   end Store_Aggregated_Project;

   ------------------------------------------
   -- Stored_Aggregated_Project_Debug_Info --
   ------------------------------------------

   procedure Stored_Aggregated_Project_Debug_Info (C : Cursor) is
      PR  : Project_Record;
      Pad : constant String := "   ";
   begin
      if not Has_Element (C) then
         Info (Pad & "No project stored!!!");
      end if;

      PR := Element (C);

      Info (Pad & "project name: " &
            String (Filesystem_String'(Full_Name (PR.Project_Path, True))));
      Info ("");

   end Stored_Aggregated_Project_Debug_Info;

end ASIS_UL.Projects.Aggregate;
