------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . G N A T _ I N T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2016, Free Software Foundation, Inc.       --
--
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Asis.Errors;
with Asis.Exceptions;           use Asis.Exceptions;
with Asis.Extensions;           use Asis.Extensions;

with A4G.A_Debug;               use A4G.A_Debug;
with A4G.A_Opt;                 use A4G.A_Opt;
with A4G.A_Output;              use A4G.A_Output;
with A4G.Contt;                 use A4G.Contt;
with A4G.Vcheck;                use A4G.Vcheck;

with Aspects;
with Atree;
with Csets;
with Elists;
with Fname;
with Gnatvsn;
with Lib;
with Namet;
with Nlists;
with Opt;                       use Opt;
with Repinfo;
with Sem_Aux;
with Sinput;
with Stand;
with Stringt;
with Uintp;
with Urealp;
with Tree_IO;

package body A4G.GNAT_Int is

   LT           : String renames ASIS_Line_Terminator;
   Standard_GCC : constant String_Access :=
     GNAT.OS_Lib.Locate_Exec_On_Path ("gcc");

   -----------------
   -- Create_Tree --
   -----------------

   procedure Create_Tree (Source_File   :     String_Access;
                          Context       :     Context_Id;
                          Is_Predefined :     Boolean;
                          Success       : out Boolean)
   is
   begin
      if Is_Predefined then
         Compile (Source_File  => Source_File,
                  Args         => GNAT_Flag & Get_Extra_Options (Context),
                  Success      => Success,
                  GCC          => Gcc_To_Call (Context),
                  Display_Call => Debug_Mode or else Debug_Flag_C);
      else
         Compile (Source_File  => Source_File,
                  Args         => I_Options (Context)
                                & Get_Extra_Options (Context),
                  Success      => Success,
                  GCC          => Gcc_To_Call (Context),
                  Display_Call => Debug_Mode or else Debug_Flag_C);
      end if;
   exception
      when others =>
         Raise_ASIS_Failed ("A4G.GNAT_Int.Create_Tree:" & LT &
                         "  check the path and environment settings for gcc!");
   end Create_Tree;

   -------------
   -- Execute --
   -------------

   function Execute
     (Program      : String_Access;
      Args         : Argument_List;
      Compiler_Out : String := "";
      Display_Call : Boolean :=
        A4G.A_Debug.Debug_Mode or else A4G.A_Debug.Debug_Flag_C)
      return         Boolean
   is
      Success     : Boolean;
      Return_Code : Integer;

      Execute : String_Access := Program;
   begin

      if Execute = null then
         Execute := Standard_GCC;
      end if;

      if Execute = null then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "A4G.GNAT_Int.Execute: Can not locate program to execute");
      end if;

      if Display_Call then
         Put (Standard_Error, Execute.all);

         for J in Args'Range loop
            Put (Standard_Error, " ");
            Put (Standard_Error, Args (J).all);
         end loop;

         New_Line (Standard_Error);

         if False then -- disable for now
            Print_Env;
         end if;
      end if;

      if Compiler_Out /= "" then
         GNAT.OS_Lib.Spawn
           (Execute.all,
            Args,
            Compiler_Out,
            Success,
            Return_Code);

         Success := Return_Code = 0;
      else
         GNAT.OS_Lib.Spawn (Execute.all, Args, Success);
      end if;

      return Success;
   end Execute;

   ----------------------------------------------
   -- General Interfaces between GNAT and ASIS --
   ----------------------------------------------

   function A_Time (T : Time_Stamp_Type) return Time is
      Year      : Year_Number;
      Month     : Month_Number;
      Day       : Day_Number;
      Hours     : Integer range 0 .. 23;
      Minutes   : Integer range 0 .. 59;
      Seconds   : Integer range 0 .. 59;
      Day_Time  : Day_Duration;
   begin
      Split_Time_Stamp
        (TS      => T,
         Year    => Nat (Year),
         Month   => Nat (Month),
         Day     => Nat (Day),
         Hour    => Nat (Hours),
         Minutes => Nat (Minutes),
         Seconds => Nat (Seconds));

      Day_Time := Duration (Seconds + 60 * Minutes + 3600 * Hours);

      return Time_Of (Year, Month, Day, Day_Time);
   end A_Time;

   --------------------------------
   -- Tree_In_With_Version_Check --
   --------------------------------

   procedure Tree_In_With_Version_Check
     (Desc    : File_Descriptor;
      Cont    : Context_Id;
      Success : out Boolean)
   is
      use type Lib.Fatal_Type;

      Cont_Mode   : constant Context_Mode := Context_Processing_Mode (Cont);
      File_Closed :          Boolean      := False;
      ASIS_GNAT_V : constant String       := Gnatvsn.Gnat_Version_String;
      First_A_Idx :          Natural;
      Last_A_Idx  :          Natural;
      First_T_Idx :          Natural;
      Last_T_Idx  :          Natural;

      procedure Note_Version_Mismatch;
      --  Close Desc and raise Version_Mismatch

      procedure Note_Version_Mismatch is
      begin
         Close (Desc, File_Closed);
         raise Version_Mismatch with
           "GNAT " & Tree_Version_String.all &
           " vs. ASIS " & ASIS_GNAT_V & ": " &
           A_Name_Buffer (1 .. A_Name_Len);
      end Note_Version_Mismatch;

   --  Start of processing for Tree_In_With_Version_Check

   begin
      Success := False;

      Tree_IO.Tree_Read_Initialize (Desc);

      Opt.Tree_Read;

      if Strong_Version_Check then
         --  We check only the dates here!
         First_A_Idx :=
           Index (Source  => ASIS_GNAT_V,
                  Pattern => "(") + 1;

         First_T_Idx :=
           Index (Source  => Tree_Version_String.all,
                  Pattern => "(") + 1;

         Last_A_Idx := Index (Source  => ASIS_GNAT_V,
                     Pattern => ")") - 1;

         if Index
              (Source  => ASIS_GNAT_V (First_A_Idx .. Last_A_Idx),
               Pattern => "-") /= 0
         then
            Last_A_Idx :=
              Index (Source  => ASIS_GNAT_V (First_A_Idx .. Last_A_Idx),
                     Pattern => "-") - 1;
         end if;

         Last_T_Idx := Index (Source  => Tree_Version_String.all,
                              Pattern => ")") - 1;

         if Index
              (Source  => Tree_Version_String.all (First_T_Idx .. Last_T_Idx),
               Pattern => "-") /= 0
         then
            Last_T_Idx :=
              Index (Source  =>
                       Tree_Version_String.all (First_T_Idx .. Last_T_Idx),
                     Pattern => "-") - 1;
         end if;

         if ASIS_GNAT_V (First_A_Idx .. Last_A_Idx) /=
            Tree_Version_String (First_T_Idx .. Last_T_Idx)
         then
            Note_Version_Mismatch;
         end if;

      end if;

      --  GNAT/ASIS version check

      if Tree_ASIS_Version_Number /= Tree_IO.ASIS_Version_Number then
         Note_Version_Mismatch;
      end if;

      --  Check that ASIS Pro uses the tree created by GNAT Pro

      if ASIS_GNAT_V (ASIS_GNAT_V'First .. ASIS_GNAT_V'First + 2) = "Pro"
        and then
          Tree_Version_String
            (Tree_Version_String'First .. Tree_Version_String'First + 2) /=
          "Pro"
      then
         Note_Version_Mismatch;
      end if;

      if Operating_Mode /= Check_Semantics then

         if Cont_Mode  = One_Tree then
            --  If in one-tree mode we can not read the only tree we have,
            --  there is no reason to continue, so raising an exception
            --  is the only choice:

            Close (Desc, File_Closed);

            --  We did not check File_Closed here, because the fact that the
            --  tree is not compile-only seems to be more important for ASIS

            Set_Error_Status
              (Status    => Asis.Errors.Use_Error,
               Diagnosis => "Asis.Ada_Environments.Open:"
               &  ASIS_Line_Terminator
               & "tree file "
               & Base_Name (A_Name_Buffer (1 .. A_Name_Len))
               & " is not compile-only with full semantics");

            raise ASIS_Failed;

         elsif Cont_Mode = N_Trees
           or else
               Cont_Mode = All_Trees
         then

            --  no need to read the rest of this tree file, but
            --  we can continue even if we can not read some trees...

            ASIS_Warning
              (Message => "Asis.Ada_Environments.Open: "
               &  ASIS_Line_Terminator
               & "tree file "
               & Base_Name (A_Name_Buffer (1 .. A_Name_Len))
               & " is not compile-only with full semantics, ignored",
               Error   => Asis.Errors.Use_Error);
         end if;

         --  debug stuff...

         if (Debug_Flag_O    or else
             Debug_Lib_Model or else
             Debug_Mode)
            and then
             Cont_Mode /= One_Tree and then
             Cont_Mode /= N_Trees
         then
            Put (Standard_Error, "tree file ");
            Put (Standard_Error, Base_Name (A_Name_Buffer (1 .. A_Name_Len)));
            Put (Standard_Error, " is not compile-only with full semantics");
            New_Line (Standard_Error);
         end if;
      else
         Atree.Tree_Read;
         Elists.Tree_Read;
         Fname.Tree_Read;
         Lib.Tree_Read;
         Namet.Tree_Read;
         Nlists.Tree_Read;
         Sem_Aux.Tree_Read;
         Sinput.Tree_Read;
         Stand.Tree_Read;
         Stringt.Tree_Read;
         Uintp.Tree_Read;
         Urealp.Tree_Read;
         Repinfo.Tree_Read;
         Aspects.Tree_Read;

         Csets.Initialize;

         --  Check that the tree corresponds to a legal unit

         if Lib.Fatal_Error (Types.Main_Unit) /= Lib.None then
            Put (Standard_Error, "tree file ");
            Put (Standard_Error, Base_Name (A_Name_Buffer (1 .. A_Name_Len)));
            Put (Standard_Error, " corresponds to illegal unit");
            New_Line (Standard_Error);
            Success := False;
         else
            --  debug stuff...
            if Debug_Flag_O    or else
               Debug_Lib_Model or else
               Debug_Mode
            then
               Put (Standard_Error, "The tree file ");
               Put (Standard_Error,
                    Base_Name (A_Name_Buffer (1 .. A_Name_Len)));
               Put (Standard_Error, " is OK");
               New_Line (Standard_Error);
            end if;

            Success := True;
         end if;

      end if;

      Close (Desc, File_Closed);

      if not File_Closed then
         Raise_ASIS_Failed
           (Diagnosis => "Asis.Ada_Environments.Open: "               &
                         "Can not close tree file: "                  &
                          Base_Name (A_Name_Buffer (1 .. A_Name_Len)) &
                          ASIS_Line_Terminator                        &
                          "disk is full or file may be used by other program",
            Stat      => Asis.Errors.Data_Error);
      end if;

   exception
      when Tree_IO.Tree_Format_Error =>
         Close (Desc, File_Closed);

         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity, "Inconsistent versions of GNAT and ASIS");

   end Tree_In_With_Version_Check;

end A4G.GNAT_Int;
