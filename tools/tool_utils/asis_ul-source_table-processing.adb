------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--      A S I S _ U L . S O U R C E _ T A B L E . P R O C E S S I N G       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2018, AdaCore                     --
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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Exceptions;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with A4G.GNAT_Int;
with Asis;                       use Asis;
with Asis.Ada_Environments;
with Asis.Compilation_Units;     use Asis.Compilation_Units;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Implementation;
with Asis.Set_Get;

with ASIS_UL.Common;             use ASIS_UL.Common;
with ASIS_UL.Debug;              use ASIS_UL.Debug;
with ASIS_UL.Environment;        use ASIS_UL.Environment;
with ASIS_UL.Misc;               use ASIS_UL.Misc;
with ASIS_UL.Output;             use ASIS_UL.Output;
with ASIS_UL.Tree_Creation;      use ASIS_UL.Tree_Creation;
with ASIS_UL.Utilities;          use ASIS_UL.Utilities;

package body ASIS_UL.Source_Table.Processing is

   ------------------------
   --  Local subprograms --
   ------------------------

   procedure Process_Sources_From_Table
     (Only_Bodies        : Boolean;
      Need_Semantic_Info : Boolean;
      Add_Needed_Sources : Boolean;
      Keep_ALI           : Boolean := False);
   --  Processes sources stores in the sources table trying to minimize
   --  compilations needed to create the tree files. If Only_Bodies is set ON,
   --  only files with .adb suffixes are compiled for the trees.
   --  Need_Semantic_Info parameter is used to control unit processing on the
   --  base of each tree being created as a part of the call to
   --  Process_Sources_From_Table, see the documentation for the Process_Source
   --  routine.
   --  Add_Needed_Sources is used to specify if the needed sources (spec for
   --  a body and subunits for stubs) should be processed even if they are not
   --  in the source table, see the documentation for the Process_Source
   --  routine.

   procedure ASIS_Processing (CU : Asis.Compilation_Unit; SF : SF_Id);
   --  This procedure incapsulates all the actions performed in the opened
   --  Context with the compilation unit CU corresponding to the source file
   --  SF (the caller is responsible for the fact that CU with this SF are
   --  represented by the tree making up the currently processed ASIS Context).
   --  The corresponding processing is entirely tool-specific, so each tool
   --  should provide its own subunit as the actual implementation of this
   --  routine.

   ---------------------
   -- ASIS_Processing --
   ----------------------

   --  This is entirely tool-specific, so the ASIS Utility Library provides
   --  an empty place-holder here.

   procedure ASIS_Processing (CU : Asis.Compilation_Unit;  SF : SF_Id) is
     separate;

   ----------------
   -- Initialize --
   ----------------

   --  This is entirely tool-specific, so the ASIS Utility Library provides
   --  an empty place-holder here.

   procedure Initialize is separate;

   --------------------------------------
   -- All_Files_Successfully_Processed --
   --------------------------------------

   function All_Files_Successfully_Processed return Boolean is
   begin
      for SF in First_SF_Id .. Last_Argument_Source loop
         if Source_Status (SF) /= Processed then
            return False;
         end if;
      end loop;

      return True;
   end All_Files_Successfully_Processed;

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source
     (SF                 : SF_Id;
      Only_Bodies        : Boolean;
      Need_Semantic_Info : Boolean;
      Add_Needed_Sources : Boolean := Mimic_gcc;
      Keep_ALI_Files     : Boolean := Mimic_gcc)
   is
      procedure Process_Subunits (CU : Asis.Compilation_Unit);
      --  Assuming that CU is of Compilation_Unit kind that may have subunits,
      --  recursively process all the subunits that are the arguments of the
      --  tool (or, if Add_Needed_Sources is True, all subunits whether or not
      --  they are arguments).

      procedure Process_Needed_Source (Needed_CU : Asis.Compilation_Unit);
      --  If Add_Needed_Sources is True, add the needed Source_File to the
      --  Source_Table if it's not already there. Then do ASIS_Processing on it
      --  if it is now there (i.e. was already there or is newly added).
      --
      --  When Process_Source is called for a library unit body, it calls
      --  Process_Needed_Source for the corresponding spec and for all
      --  subunits. In Mimic_gcc mode, Process_Source is called ONLY for bodies
      --  (and specs without bodies). For library unit bodies, it is important
      --  that the spec (if any) be processed as part of that, using the tree
      --  for the body, because otherwise the tree file for the spec would
      --  overwrite the one for the body, which would confuse
      --  gnatmake. Subunits are also processed as part of the library unit
      --  body.

      procedure Process_Needed_Source (Needed_CU : Asis.Compilation_Unit) is
         File_Name : constant String :=
           Normalize_Pathname
             (To_String (Text_Name (Needed_CU)),
              Resolve_Links  => False,
              Case_Sensitive => True);
         Needed_SF : SF_Id := File_Find (File_Name);
      begin
         if Add_Needed_Sources and then not Present (Needed_SF) then
            Needed_SF := Add_Needed_Source (File_Name);
            Total_Sources := Natural (Last_Source);
            Sources_Left  := Sources_Left + 1;
         end if;
         pragma Assert (if Add_Needed_Sources then Present (Needed_SF));

         if Present (Needed_SF)
           and then Source_Status (Needed_SF) = Waiting
         then
            Output_Source (Needed_SF);
            ASIS_Processing (Needed_CU, Needed_SF);

            if Source_Status (Needed_SF) in Waiting | Tree_Is_Ready then
               Set_Source_Status (Needed_SF, Processed);
            end if;
         end if;

      exception
         when Ex :
           Asis.Exceptions.ASIS_Inappropriate_Context          |
           Asis.Exceptions.ASIS_Inappropriate_Container        |
           Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
           Asis.Exceptions.ASIS_Inappropriate_Element          |
           Asis.Exceptions.ASIS_Inappropriate_Line             |
           Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
           Asis.Exceptions.ASIS_Failed                         =>
            ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);
            Source_Clean_Up (Needed_SF, Keep_ALI_Files);
            raise Fatal_Error;

         when Ex : others =>
            Error
              ("unknown bug detected when processing " &
                Source_Name (Needed_SF));
            Error_No_Tool_Name
              ("Please submit bug report to report@adacore.com");
            Report_Unhandled_Exception (Ex);
            Source_Clean_Up (Needed_SF, Keep_ALI_Files);
            raise Fatal_Error;
      end Process_Needed_Source;

      procedure Process_Subunits (CU : Asis.Compilation_Unit) is
         Subunit_List : constant Asis.Compilation_Unit_List := Subunits (CU);
      begin
         for J in Subunit_List'Range loop
            if Asis.Set_Get.Kind (Subunit_List (J)) /= A_Nonexistent_Body then
               Process_Needed_Source (Subunit_List (J));
               Process_Subunits (Subunit_List (J));
            end if;
         end loop;
      end Process_Subunits;

      CU_Tmp  : Asis.Compilation_Unit;
      Success : Boolean;

      --  Start of processing for Process_Source

   begin
      --  The current directory is Tool_Temp_Dir. In Mimic_gcc mode, we need to
      --  switch to Tool_Inner_Dir so the ALI file ends up in the right
      --  directory. We switch back at the end (see <<Done>> below).

      if Mimic_gcc then
         pragma Assert (Is_Argument_Source (SF));
         pragma Assert (Get_Current_Dir = Tool_Temp_Dir.all &
                          Directory_Separator);
         Change_Dir (Tool_Inner_Dir.all);
      end if;

      Output_Source (SF);

      if Source_Status (SF) = Waiting then
         Create_Tree (SF, Success);
      end if;

      Start_Tree_Creations (SF, Only_Bodies);

      if Source_Status (SF) in
           Not_A_Legal_Source | Not_A_Legal_Source_Needs_Listing_Processing
      then
         Set_Source_Status (SF, Not_A_Legal_Source);
         goto Done;
      end if;

      if Use_Parallel_Tree_Creation then
         Asis.Ada_Environments.Associate
          (The_Context => The_Context,
           Name        => "",
           Parameters  => "-C1 "
                         & To_Wide_String
                             (Image (Integer (SF)) & Directory_Separator &
                              Suffixless_Name (SF) & ".adt"));
      else
         Asis.Ada_Environments.Associate
          (The_Context => The_Context,
           Name        => "",
           Parameters  => "-C1 "
                         & To_Wide_String (Suffixless_Name (SF) & ".adt"));
      end if;

      declare
         use type Asis.Errors.Error_Kinds;
      begin
         Asis.Ada_Environments.Open (The_Context);
         Success := True;

         if Debug_Flag_T then
            Print_Tree_Sources;
         end if;
      exception
         when Asis.Exceptions.ASIS_Failed =>
            --  The only known situation when we can not open a C1 context for
            --  newly created tree is recompilation of System (see D617-017)

            if Asis.Implementation.Status = Asis.Errors.Use_Error
              and then
               Asis.Implementation.Diagnosis = "Internal implementation error:"
               & " Asis.Ada_Environments.Open - System is recompiled"
            then
               Error ("can not process redefinition of System in " &
                       Source_Name (SF));

               Set_Source_Status (SF, Not_A_Legal_Source);
               Success := False;
            else
               raise;
            end if;

      end;

      if Success then
         The_CU := Main_Unit_In_Current_Tree (The_Context);

         --  If a subunit is given on the command line in --incremental mode,
         --  then the inner invocation of gnatpp for that subunit should do
         --  nothing, because the subunit will be processed as part of its
         --  parent body.

         if Mimic_gcc and then Unit_Class (The_CU) = A_Separate_Body then
            Set_Source_Status (SF, Processed);
            goto Done;
         end if;

         if Unit_Origin (The_CU) /= An_Application_Unit
           and then not Process_RTL_Units
         then
            Error ("cannot process RTL unit " & Source_Name (SF) &
                   " Use '-a' option for processing RTL components");
            Set_Source_Status (SF, Processed);
         else
            ASIS_Processing (The_CU, SF);

            if Source_Status (SF) in Waiting | Tree_Is_Ready then
               Set_Source_Status (SF, Processed);
            end if;

            if Need_Semantic_Info then
               --  If it's a library unit body, process the spec

               case Unit_Class (The_CU) is
                  when A_Public_Body | A_Private_Body =>
                     CU_Tmp := Corresponding_Declaration (The_CU);
                     Process_Needed_Source (CU_Tmp);
                  when A_Public_Declaration | A_Private_Declaration =>
                     null; -- If Mimic_gcc, must be bodiless
                  when A_Public_Declaration_And_Body =>
                     null; -- no spec
                  when A_Separate_Body =>
                     if Mimic_gcc then
                        pragma Assert (False); -- subunits are skipped above
                     end if;
                  when Not_A_Class =>
                     pragma Assert (False);
               end case;

               --  If it's a body, process any subunits

               if Unit_Class (The_CU) in
                 A_Public_Body | A_Private_Body | A_Separate_Body
               then
                  Process_Subunits (The_CU);
               end if;

            else
               declare
                  All_CUs : constant Asis.Compilation_Unit_List :=
                    Asis.Compilation_Units.Compilation_Units (The_Context);
               begin
                  for J in All_CUs'Range loop
                     if Process_RTL_Units
                       or else Unit_Origin (All_CUs (J)) = An_Application_Unit
                     then
                        Process_Needed_Source (All_CUs (J));
                     end if;
                  end loop;
               end;
            end if;
         end if;
      end if;

      <<Done>>
      Source_Clean_Up (SF, Keep_ALI_Files);
      if Mimic_gcc then
         pragma Assert (Get_Current_Dir = Tool_Inner_Dir.all &
                          Directory_Separator);
         Change_Dir (Tool_Temp_Dir.all);
      end if;

   exception
      when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
                Asis.Exceptions.ASIS_Inappropriate_Container        |
                Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
                Asis.Exceptions.ASIS_Inappropriate_Element          |
                Asis.Exceptions.ASIS_Inappropriate_Line             |
                Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
                Asis.Exceptions.ASIS_Failed                         =>
         ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);
         Source_Clean_Up (SF, Keep_ALI_Files);
         raise Fatal_Error;

      when Ex : A4G.GNAT_Int.Version_Mismatch =>
         Error ("Inconsistent versions of GNAT and ASIS");
         Error_No_Tool_Name (Ada.Exceptions.Exception_Message (Ex));
         Error_No_Tool_Name ("Possible installation problem");
         raise Fatal_Error;

      when Fatal_Error =>
         raise;

      when Ex : others =>
         Error ("unknown bug detected when processing " & Source_Name (SF));
         Error_No_Tool_Name ("Please submit bug report to report@adacore.com");
         Report_Unhandled_Exception (Ex);
         Source_Clean_Up (SF, Keep_ALI_Files);
         raise Fatal_Error;
   end Process_Source;

   ---------------------
   -- Process_Sources --
   ---------------------

   procedure Process_Sources
     (Need_Semantic_Info : Boolean := True;
      Add_Needed_Sources : Boolean := Mimic_gcc;
      Keep_ALI           : Boolean := False)
   is
   begin
      Asis.Implementation.Initialize ("-k -ws -asis05 -sv");

      if True then -- Can we get rid of Only_Bodies????
         Process_Sources_From_Table
           (Only_Bodies        => True,
            Need_Semantic_Info => Need_Semantic_Info,
            Add_Needed_Sources => Add_Needed_Sources,
            Keep_ALI           => Keep_ALI or else Mimic_gcc);
      end if;

      Process_Sources_From_Table
        (Only_Bodies        => False,
         Need_Semantic_Info => Need_Semantic_Info,
         Add_Needed_Sources => Add_Needed_Sources,
         Keep_ALI           => Keep_ALI or else Mimic_gcc);

      Asis.Implementation.Finalize;
   end Process_Sources;

   --------------------------------
   -- Process_Sources_From_Table --
   --------------------------------

   procedure Process_Sources_From_Table
     (Only_Bodies        : Boolean;
      Need_Semantic_Info : Boolean;
      Add_Needed_Sources : Boolean;
      Keep_ALI           : Boolean := False)
   is
      Next_SF : SF_Id;
   begin
      Reset_Source_Iterator;

      loop
         Next_SF := Next_Non_Processed_Source
           (Only_Bodies, Include_Needed_Sources => Add_Needed_Sources);
         exit when not Present (Next_SF);

         Process_Source
           (Next_SF,
            Only_Bodies,
            Need_Semantic_Info,
            Add_Needed_Sources,
            Keep_ALI);

         Check_Tree_Creations;
      end loop;

   end Process_Sources_From_Table;

   --------------
   -- Finalize --
   --------------

   --  This is entirely tool-specific, so the ASIS Utility Library provides
   --  an empty place-holder here.

   procedure Finalize is separate;

end ASIS_UL.Source_Table.Processing;
