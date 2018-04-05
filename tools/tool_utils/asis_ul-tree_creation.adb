------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                A S I S _ U L . T R E E _ C R E A T I O N                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2013-2017, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not,  write  to the  Free Software Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------
pragma Ada_2012;

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with A4G.GNAT_Int;              use A4G.GNAT_Int;

with ASIS_UL.Common;            use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;  use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;             use ASIS_UL.Debug;
with ASIS_UL.Misc;              use ASIS_UL.Misc;
with ASIS_UL.Options;           use ASIS_UL.Options;
with ASIS_UL.Output;            use ASIS_UL.Output;
with ASIS_UL.Projects;          use ASIS_UL.Projects;

package body ASIS_UL.Tree_Creation is

   Parallel_Tree_Creation : Boolean := False;

   type Tree_Process_Rec is record
      SF           : SF_Id;
      PId          : Process_Id;
      Mapping_File : Natural;
      --  Id of a mapping file used by the process PId to compile SF
   end record;

   Max_Processes : Integer := Process_Num - 2;

   No_Process    : constant Integer := -1;
   First_Process : constant Natural := 0;
--   subtype Tree_Proc_Num is Integer range No_Process .. Max_Processes;
--   subtype Actual_Tree_Proc_Num is Natural range
--     First_Process .. Max_Processes;

   Currently_Running : Integer := 0;
   --  The number of tree creation processes that have been started and we do
   --  not know if they are finished

   subtype Full_Tree_Process_Table_Idx is Integer
     range No_Process .. Natural'Last;

   subtype Tree_Process_Table_Idx is Natural
     range First_Process .. Natural'Last;

   type Tree_Process_Table_Type is array (Tree_Process_Table_Idx range <>)
      of Tree_Process_Rec;

   type Tree_Process_Table_Type_Access is access  Tree_Process_Table_Type;

   Tree_Process_Table : Tree_Process_Table_Type_Access;

   Last_Stored   : Full_Tree_Process_Table_Idx := No_Process;
   Last_Finished : Full_Tree_Process_Table_Idx := No_Process;

   Next_Free_Mapping_File : Natural := No_Mapping_File_Copy;
   --  Id of the mapping file that is not used by any compilation process so
   --  it can be used by the new process to spawn.

   ------------------------
   --  Local subprograms --
   ------------------------

   function Get_Next_Source_For_Tree_Creation
     (SF                     :        SF_Id;
      Only_Bodies            : in out Boolean;
      Include_Needed_Sources :        Boolean := Mimic_gcc)
      return                          SF_Id;
   --  Returns the next source the tree can be created for. If Only_Bodies is
   --  True, looks for body sources only. If there is no body sources to create
   --  tree for any more sets Only_Bodies to False and start looking for other
   --  kinds of units.

   function Start_Tree_Creation
     (SF                   : SF_Id;
      Need_Compiler_Output : Boolean := False;
      All_Warnings_Off     : Boolean := True)
      return                 Process_Id;
   --  Calls the compiler to create the tree for SF using Non_Blocking_Spawn.

   procedure Store_Process (SF : SF_Id; Process : Process_Id);
   --  Stores in the Tree_Process_Table the pair SF <-> Id of the process that
   --  creates the tree for this SF

   procedure Store_Process_Termination
     (Process : Process_Id;
      Success : Boolean);
   --  Locates the record corresponding to Process in Tree_Process_Table.
   --  Changes the status of the corresponding source according to Success and
   --  removes the record about this process from Tree_Process_Table;

   --------------------------
   -- Check_Tree_Creations --
   --------------------------

   procedure Check_Tree_Creations is
      Next_Proc : Process_Id;
      Success   : Boolean;
   begin
      if not Use_Parallel_Tree_Creation then
         return;
         --  ???
      end if;

      --  We are waiting for the first tree:
      Wait_Process (Next_Proc, Success);

      while Next_Proc /= Invalid_Pid loop
         Currently_Running := Currently_Running - 1;

         Store_Process_Termination (Next_Proc, Success);

         if Success then
            --  We have a tree ready for analysis
            exit;
         end if;

         Wait_Process (Next_Proc, Success);
      end loop;

   end Check_Tree_Creations;

   ---------------------------------------
   -- Get_Next_Source_For_Tree_Creation --
   ---------------------------------------

   function Get_Next_Source_For_Tree_Creation
     (SF                     :        SF_Id;
      Only_Bodies            : in out Boolean;
      Include_Needed_Sources :        Boolean := Mimic_gcc)
      return               SF_Id
   is
      Up_To  : SF_Id;
      Result : SF_Id := No_SF_Id;
   begin
      --  There are some duplications with
      --  ASIS_UL.Source_Table.Next_Non_Processed_Source, needs revising

      if Include_Needed_Sources then
         Up_To := Last_Source;
      else
         Up_To := Last_Argument_Source;
      end if;

      for J in SF .. Up_To loop

         if Source_Status (J) in Waiting
           and then
            (not Only_Bodies
            or else
             Is_A_Body (J))
         then
            Result := J;
            exit;
         end if;

      end loop;

      if not Present (Result) and then Only_Bodies then
         Only_Bodies := False;
         Result      := Get_Next_Source_For_Tree_Creation (SF, Only_Bodies);
      end if;

      return Result;

   end Get_Next_Source_For_Tree_Creation;

   -----------------------
   -- Set_Max_Processes --
   -----------------------

   procedure Set_Max_Processes is
   begin
      if Process_Num = 1
        or else
         Use_Gnatmake_To_Compile
        or else
         Use_Project_File_Obsolete
        or else
         Incremental_Mode
      then
         Parallel_Tree_Creation := False;
         return;
         --  ???
      else
         Parallel_Tree_Creation := True;
      end if;

      Max_Processes := Process_Num - 2;
      Tree_Process_Table := new Tree_Process_Table_Type
        (First_Process .. Max_Processes);
      Tree_Process_Table.all :=
        (others => (SF           => No_SF_Id,
                    PId          => Invalid_Pid,
                    Mapping_File => No_Mapping_File_Copy));
   end Set_Max_Processes;

   -------------------------
   -- Start_Tree_Creation --
   -------------------------

   function Start_Tree_Creation
     (SF                   : SF_Id;
      Need_Compiler_Output : Boolean := False;
      All_Warnings_Off     : Boolean := True)
      return                 Process_Id
   is
      Comp_Args : Argument_List
        (Arg_List'First .. Arg_List'Last + 12 + 1 + 2 +
         (if Get_Config_File_Name /= "" then 1 else 0));

      First_Idx : constant Integer := Comp_Args'First;
      Last_Idx  : Integer := First_Idx;

      Is_GNAAMP_Call : Boolean := False;
      --  In case of the call to GNAAMP we should not set '-x ada' flags

      Result    : Process_Id;

   begin

      if Get_Config_File_Name /= "" then
         Next_Free_Mapping_File := Get_Free_Mapping_File;
      end if;

      Make_Dir (Image (Integer (SF)));

      Is_GNAAMP_Call :=
        Index (To_Lower (Base_Name (Gcc_To_Call.all)), "gnaamp") /= 0;

      Comp_Args (Last_Idx) := Comp_Flag;
      Last_Idx := Last_Idx + 1;

      Comp_Args (Last_Idx) := GNAT_Flag_ct;
      Last_Idx := Last_Idx + 1;

      if not Is_GNAAMP_Call then
         Comp_Args (Last_Idx) := GCC_Flag_X;
         Last_Idx := Last_Idx + 1;
         Comp_Args (Last_Idx) := GCC_Par_Ada;
         Last_Idx := Last_Idx + 1;
      end if;

      for J in Arg_List'Range loop
         Comp_Args (Last_Idx) := Arg_List (J);
         Last_Idx := Last_Idx + 1;
      end loop;

      if All_Warnings_Off then
         Comp_Args (Last_Idx) := GNAT_Flag_ws;
         Last_Idx := Last_Idx + 1;

         Comp_Args (Last_Idx) := GNAT_Flag_yN;
         Last_Idx := Last_Idx + 1;

      end if;

      if Get_Config_File_Name /= "" then
         Comp_Args (Last_Idx) :=
           new String'("-gnatem=" &
                       Get_Mapping_File_Copy_Name (Next_Free_Mapping_File));
         Last_Idx := Last_Idx + 1;
      end if;

      Comp_Args (Last_Idx) := new String'("-o");
      Last_Idx := Last_Idx + 1;

      Comp_Args (Last_Idx) := new String'(
        Image (Integer (SF)) & Directory_Separator &
        Suffixless_Name (SF) & ".o");
      Last_Idx := Last_Idx + 1;

      Comp_Args (Last_Idx) := new String'(Source_Name (SF));

      if Debug_Flag_C then
         Info ("...parallel tree creation...");
         Info_No_EOL (Gcc_To_Call.all);

         for J in First_Idx .. Last_Idx loop
            Info_No_EOL (" ");
            Info_No_EOL (Comp_Args (J).all);
         end loop;

         Info ("");
      end if;

      if Need_Compiler_Output then
         Result := Non_Blocking_Spawn
                     (Program_Name => Gcc_To_Call.all,
                      Args         => Comp_Args (First_Idx .. Last_Idx) &
                                      Compilation_Switches (SF),
                      Output_File  => Get_Compiler_Out_File_Name (SF),
                      Err_To_Out   => True);
      else
         Result := Non_Blocking_Spawn
                     (Program_Name => Gcc_To_Call.all,
                      Args         => Comp_Args (First_Idx .. Last_Idx) &
                                      Compilation_Switches (SF));
      end if;

      return Result;
   end Start_Tree_Creation;

   --------------------------
   -- Start_Tree_Creations --
   --------------------------

   procedure Start_Tree_Creations
     (SF                   : SF_Id;
      Only_Bodies          : Boolean;
      Need_Compiler_Output : Boolean := False;
      All_Warnings_Off     : Boolean := True)
   is
      Next_SF              : SF_Id := SF;
      Next_Process         : Process_Id;
      Look_For_Bodies_Only : Boolean := Only_Bodies;
   begin

      if not Use_Parallel_Tree_Creation then
         return;
         --  ???
      end if;

      if Max_Processes = -1 then
         return;
      end if;

      while Currently_Running <= Max_Processes loop
         Next_SF :=
           Get_Next_Source_For_Tree_Creation (Next_SF, Look_For_Bodies_Only);

         exit when not Present (Next_SF);

         Next_Process :=
           Start_Tree_Creation (Next_SF,
                                Need_Compiler_Output => Need_Compiler_Output,
                                All_Warnings_Off     => All_Warnings_Off);

         if Next_Process = Invalid_Pid then
            Error ("Cannot compile " & Source_Name (SF));
            raise Fatal_Error;
         else
            Set_Source_Status (Next_SF, Preparing_Tree);
            Currently_Running := Currently_Running + 1;
            Store_Process (Next_SF, Next_Process);
         end if;

      end loop;

   end Start_Tree_Creations;

   -------------------
   -- Store_Process --
   -------------------

   procedure Store_Process (SF : SF_Id; Process : Process_Id) is
      Idx : Tree_Process_Table_Idx range First_Process .. Max_Processes
        := First_Process;
      Count : Natural := 0;
   begin

      if Last_Finished /= No_Process then
         Tree_Process_Table (Last_Finished) :=
           (SF, Process, Next_Free_Mapping_File);
         Set_Busy (Next_Free_Mapping_File);
         Last_Stored   := Last_Finished;
         Last_Finished := No_Process;
         return;
      end if;

      if Last_Stored /= No_Process then
         Idx := (Last_Stored + 1) mod (Max_Processes + 1);
      end if;

      while Tree_Process_Table (Idx).SF /= No_SF_Id loop
         Count := Count + 1;
         pragma Assert (Count <= Max_Processes);
         Idx := (Idx + 1) mod (Max_Processes + 1);
      end loop;

      Tree_Process_Table (Idx) := (SF, Process, Next_Free_Mapping_File);
      Set_Busy (Next_Free_Mapping_File);
      Last_Stored := Idx;
   end Store_Process;

   -------------------------------
   -- Store_Process_Termination --
   -------------------------------

   procedure Store_Process_Termination
     (Process : Process_Id;
      Success : Boolean)
   is
      Idx : Full_Tree_Process_Table_Idx range No_Process .. Max_Processes
        := No_Process;
   begin
      for J in First_Process .. Max_Processes loop
         if Tree_Process_Table (J).PId = Process then
            Idx := J;
            exit;
         end if;
      end loop;

      pragma Assert (Idx in First_Process .. Max_Processes);

      if Success then
         Set_Source_Status (Tree_Process_Table (Idx).SF, Tree_Is_Ready);
      else
         Error ("cannot compile """ &
                Short_Source_Name (Tree_Process_Table (Idx).SF) & """");

         Set_Source_Status
           (Tree_Process_Table (Idx).SF,
            Not_A_Legal_Source_Needs_Listing_Processing);
      end if;

      Set_Free (Tree_Process_Table (Idx).Mapping_File);

      Tree_Process_Table (Idx) :=
        (SF           => No_SF_Id,
         PId          => Invalid_Pid,
         Mapping_File => No_Mapping_File_Copy);

      Last_Finished := Idx;
   end Store_Process_Termination;

   --------------------------------
   -- Use_Parallel_Tree_Creation --
   --------------------------------

   function Use_Parallel_Tree_Creation return Boolean is
   begin
      return Parallel_Tree_Creation;
   end Use_Parallel_Tree_Creation;

end ASIS_UL.Tree_Creation;
