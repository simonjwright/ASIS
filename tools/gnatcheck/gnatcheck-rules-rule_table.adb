------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--           G N A T C H E C K . R U L E S . R U L E _ T A B L E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2019, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Strings;              use Ada.Strings;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Text_IO;              use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with ASIS_UL.Common;
with ASIS_UL.Debug;            use ASIS_UL.Debug;
with ASIS_UL.Misc;             use ASIS_UL.Misc;
with ASIS_UL.Output;           use ASIS_UL.Output;

with Gnatcheck.Compiler;       use Gnatcheck.Compiler;
with Gnatcheck.Ids;            use Gnatcheck.Ids;

package body Gnatcheck.Rules.Rule_Table is

   -----------------------
   -- Local subprograms --
   -----------------------

   type Rule_File_Record is record
      Arg_Name : String_Access;
      --  Rule file name as it is given in '-from=...' option, used to
      --  generate diagnostic message
      Full_Name : String_Access;
   end record;

   package Rule_File_Stack is new GNAT.Table
    (Table_Component_Type => Rule_File_Record,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Rule file stack");
   --  Keeps the names of the "nested" rule files, in the order of the
   --  macro expansion that is currently performed, is used to detect looping
   --  in macro expansions

   procedure Check_For_Looping (RF_Name : String; Success : in out Boolean);
   --  Checks if we have a looping in rule files macro expansions. That is,
   --  checks if RF_Name is already stored in Rule_File_Stack. If it is,
   --  generates the corresponding diagnostic message and sets Success OFF.
   --  Otherwise appends the record corresponding to the rule file to
   --  Rule_File_Stack.
   --  This procedure is supposed to be called when we already know that
   --  RF_Name is the name of some existing file.

   procedure Pop_Rule_File;
   --  Removes the last record corresponding to the latest processed rule file
   --  from Rule_File_Stack (we can not just call
   --  Rule_File_Stack.Decrement_Last, because we have to free memory occupied
   --  by the dynamic strings)

   function Is_Old_Metric_Rule_Syntax (Option : String) return Boolean;
   --  Checks if Option is a part of the rule option that uses the old syntax
   --  for metric rules. This part is supposed to start from the symbol next to
   --  (+|-)R, Note that this routine recognizes only three metric names:
   --  Cyclomatic_Complexity, Essential_Complexity and LSLOC.

   procedure Process_Old_Metric_Rule_Option
     (Par        : String;
      Enable     : Boolean;
      Defined_At : String);
   --  Supposing that Par is a part of the rule parameter starting after
   --  (+|-)R, and Is_Old_Metric_Rule_Syntax (Par) is True, processes the
   --  old format of the metrics rule parameters.

   ---------------------------------
   -- Activate_Rules_In_Test_Mode --
   ---------------------------------

   procedure Activate_Rules_In_Test_Mode is
   begin
      --  A little bit more then just a placeholder! Should be revised!!!

      for J in All_Rules.First .. All_Rules.Last loop

         if All_Rules.Table (J).Rule_Status = Fully_Implemented
           and then
            All_Rules.Table (J).all not in Internal_Rule_Template'Class
         then
            Activate_In_Test_Mode (All_Rules.Table (J).all);
         end if;

      end loop;

   end Activate_Rules_In_Test_Mode;

   -----------------------
   -- Check_For_Looping --
   -----------------------

   procedure Check_For_Looping (RF_Name : String; Success : in out Boolean) is
      Full_Name : constant String  := Normalize_Pathname (RF_Name);
   begin

      for J in 1 .. Rule_File_Stack.Last loop

         if Full_Name = Rule_File_Stack.Table (J).Full_Name.all then
            Success := False;
            exit;
         end if;

      end loop;

      if not Success then
         Error ("cycling in rule files:");

         for J in 1 .. Rule_File_Stack.Last loop

            Info_No_EOL (Rule_File_Stack.Table (J).Arg_Name.all);
            Info_No_EOL (" needs ");

            if J < Rule_File_Stack.Last then
               Info (Rule_File_Stack.Table (J + 1).Arg_Name.all);
            end if;

         end loop;

         Info (RF_Name);
         Info ("");

         raise ASIS_UL.Common.Fatal_Error;

      else
         --  Add new file to the rule file stack
         Rule_File_Stack.Append (
           (Arg_Name =>  new String'(RF_Name),
            Full_Name => new String'(Full_Name)));
      end if;

   end Check_For_Looping;

   -------------------
   -- Get_Next_Rule --
   -------------------

   function Get_Next_Rule
     (R           : Rule_Id;
      From_Status : Rule_Statuses := Fully_Implemented)
      return        Rule_Id
   is
      Result : Rule_Id := All_Rules.Table (R).Next_In_Category;
   begin
      pragma Assert (Present (R));

      while Present (Result) loop
         exit when Rule_Status (Result) >= From_Status;
         Result := All_Rules.Table (Result).Next_In_Category;
      end loop;

      return Result;
   end Get_Next_Rule;

   --------------
   -- Get_Rule --
   --------------

   function Get_Rule (Rule_Name : String) return Rule_Id is
      Result               : Rule_Id          := No_Rule;
      Normalised_Rule_Name : constant String  := To_Lower (Rule_Name);
   begin

      --  First, check if we have a compiler check:

      if Normalised_Rule_Name = "restrictions" then
         return Restrictions_Id;
      elsif Normalised_Rule_Name = "style_checks" then
         return Style_Checks_Id;
      elsif Normalised_Rule_Name = "warnings" then
         return Warnings_Id;
      end if;

      --  This is a rather noneffective implementation. At some point we
      --  should think about a hash table and about more efficient rule
      --  names normalization

      for J in First_Rule .. All_Rules.Last loop

         --  Check rule name first:
         if To_Lower (All_Rules.Table (J).Name.all) = Normalised_Rule_Name then
            Result := J;
            exit;
         end if;

         if All_Rules.Table (J).Synonym /= null
           and then
            To_Lower (All_Rules.Table (J).Synonym.all) = Normalised_Rule_Name
         then
            Result := J;
            exit;
         end if;

      end loop;

      return Result;
   end Get_Rule;

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled (Rule : Rule_Id) return Boolean is
      Result : Boolean := False;
   begin
      if not Present (Rule) then
         raise ASIS_UL.Common.Fatal_Error;
      end if;

      case Rule is
         when Restrictions_Id =>
            Result := Check_Restrictions;
         when Style_Checks_Id =>
            Result := Use_gnaty_Option;
         when Warnings_Id =>
            Result := Use_gnatw_Option;
         when others =>
            Result := Is_Enable (All_Rules.Table (Rule).all);
      end case;

      return Result;
   end Is_Enabled;

   ---------------
   -- Is_Global --
   ---------------

   function Is_Global (Rule : Rule_Id) return Boolean is
      Result : Boolean := False;
   begin
      if not Present (Rule) then
         raise ASIS_UL.Common.Fatal_Error;
      end if;

      if Rule not in Compiler_Checks then
         Result := All_Rules.Table (Rule).all in Global_Rule_Template'Class;
      end if;

      return Result;
   end Is_Global;

   -------------------------------
   -- Is_Old_Metric_Rule_Syntax --
   -------------------------------

   function Is_Old_Metric_Rule_Syntax (Option : String) return Boolean is
      Len       : constant Natural := Option'Length;
      First_Idx :          Natural := Option'First;
      Last_Idx  :          Natural;
      Result    :          Boolean := False;
   begin

      if Len >= 24
        and then
         To_Lower (Option (First_Idx .. First_Idx + 16)) = "metrics_violation"
      then
         First_Idx := Index (Option, ":");

         if First_Idx > 0 then
            First_Idx := First_Idx + 1;
            Last_Idx  := Index (Option, ">");

            if Last_Idx > 0 then
               Last_Idx := Last_Idx - 1;
            else
               Last_Idx := Option'Last;
            end if;

            declare
               Metric_Name : constant String :=
                 To_Lower (Trim (Option (First_Idx .. Last_Idx), Both));
            begin
               Result := Metric_Name = "cyclomatic_complexity"
                 or else Metric_Name = "essential_complexity"
                 or else Metric_Name = "lsloc";
            end;

         end if;

      end if;

      return Result;
   end Is_Old_Metric_Rule_Syntax;

   --------
   -- No --
   --------

   function No (Id : Rule_Id) return Boolean is
   begin
      return Id not in All_Rules.First .. All_Rules.Last;
   end No;

   ---------------------
   -- Parent_Category --
   ---------------------

   function Parent_Category (R : Rule_Id) return Category_Id is
   begin
      pragma Assert (Present (R));
      return All_Rules.Table (R).Rule_Category;
   end Parent_Category;

   -------------------
   -- Pop_Rule_File --
   -------------------

   procedure Pop_Rule_File is
   begin
      Free (Rule_File_Stack.Table (Rule_File_Stack.Last).Arg_Name);
      Free (Rule_File_Stack.Table (Rule_File_Stack.Last).Full_Name);
      Rule_File_Stack.Decrement_Last;
   end Pop_Rule_File;

   ---------------------
   -- Print_Rule_List --
   ---------------------

   procedure Print_Rule_List
     (First_Rule  : Rule_Id;
      Level       : Natural;
      From_Status : Rule_Statuses)
   is
      Next_Rule : Rule_Id := First_Rule;
   begin

      pragma Assert (Rule_Status (First_Rule) >= From_Status);

      while Present (Next_Rule) loop
         Info_No_EOL
           (Level * Ident_String & All_Rules.Table (Next_Rule).Name.all);

         if Rule_Status (Next_Rule) = Under_Construction then
            Info_No_EOL (" (under construction)");
         elsif Rule_Status (Next_Rule) = Non_Documented then
            Info_No_EOL (" (not fully documented)");
         end if;

         Info ("");

         Next_Rule := Get_Next_Rule (Next_Rule, From_Status);
      end loop;

   end Print_Rule_List;

   -------------
   -- Present --
   -------------

   function Present (Id : Rule_Id) return Boolean is
   begin
      return Id in Compiler_Checks
           or else Id in First_Rule .. All_Rules.Last;
   end Present;

   ------------------------------------
   -- Process_Old_Metric_Rule_Option --
   ------------------------------------

   procedure Process_Old_Metric_Rule_Option
     (Par        : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      Metric_Rule_Name : String (1 .. Par'Length);
      Start_Idx        : Natural;
      End_Idx          : Natural;
      Rule             : Rule_Id;
   begin
      Metric_Rule_Name (1 .. 8) := "Metrics_";
      Start_Idx                 := Index (Par, ":") + 1;
      End_Idx                   := Index (Par, ">");

      if End_Idx > 0 then
         End_Idx := End_Idx - 1;
      else
         End_Idx := Par'Last;
      end if;

      while Is_White_Space (Par (Start_Idx)) loop
         Start_Idx := Start_Idx + 1;
      end loop;

      while Is_White_Space (Par (End_Idx)) loop
         End_Idx := End_Idx - 1;
      end loop;

      Metric_Rule_Name (9 .. 9 + (End_Idx - Start_Idx)) :=
         Par (Start_Idx .. End_Idx);

      Rule := Get_Rule (Metric_Rule_Name (1 .. 9 + (End_Idx - Start_Idx)));

      Start_Idx := Index (Par, ">");

      if Start_Idx = 0 then
         Start_Idx := 1;
         End_Idx   := 0;
         --  No parameter is specified, so we need indexes for empty string2
      else
         Start_Idx := Start_Idx + 1;

         while Is_White_Space (Par (Start_Idx)) loop
            Start_Idx := Start_Idx + 1;
         end loop;

         End_Idx := Par'Last;

         while Is_White_Space (Par (End_Idx)) loop
            Start_Idx := End_Idx - 1;
         end loop;
      end if;

      Process_Rule_Parameter
        (Rule       => All_Rules.Table (Rule).all,
         Param      => Par (Start_Idx .. End_Idx),
         Enable     => Enable,
         Defined_At => Defined_At);

   end Process_Old_Metric_Rule_Option;

   -----------------------
   -- Process_Rule_File --
   -----------------------

   procedure Process_Rule_File (RF_Name : String) is
      RF           : File_Type;

      Rule_Start_Line : Natural := 1;
      --  Number of the line of the rule file where the latest beginning of a
      --  rule or '-from' option is detected, used in diagnostic messages

      Current_Line : Natural := 0;
      --  Number of the currently processed line from the rule file, used in
      --  diagnostic messages

      Last_Rule_Opt_Start_Col  : Natural := 0;
      Last_Rule_Opt_Start_Line : Natural := 0;
      --  Point to the position where the last scanned rule option starts
      Last_Rule_Opt_Start_Col_Old  : Natural := 0;
      Last_Rule_Opt_Start_Line_Old : Natural := 0;
      --  Point to the position where the next before the last scanned rule
      --  option starts

      Line_Buf : String (1 .. 1024);
      Line_Len : Natural := 0;
      --  Buffer to read next line from the file into

      Rule_Buf_Last : constant Positive :=  16 * 1024;
      Rule_Buf      :          String (1 .. Rule_Buf_Last);
      Rule_Len      :          Natural := 0;
      --  Buffer to form the new rule option

      type Scan_Status is (
         Indefinite,      --  we do not know what option is scanned
         In_Rule_Option,  --  we are scanning the rule option
         In_From_Option); --  we are scanning the '-from' option
      --  This type is used to represent the current state of the rule file
      --  scanning process

      New_State : Scan_Status := Indefinite;
      --  Corresponds to the option that is just detected in the rule file

      Old_State : Scan_Status := Indefinite;
      --  represents the option that was detected before detecting the new
      --  one.

      Success : Boolean := True;

      function Get_Rule_File_Name (RF : String) return String is
        (if Is_Absolute_Path (RF_Name)
           or else
            not Gnatcheck.Options.Gnatcheck_Prj.Is_Specified
         then
            RF
         else
            Normalize_Pathname
              (Dir_Name (Gnatcheck.Options.Gnatcheck_Prj.Source_Prj) & RF));
      --  If gnatcheck is called with a project file, all the (relative) names
      --  of the rule files are considered as related to the project file
      --  directory, otherwise - as related to the current directory

      Rule_File_Name : constant String := Get_Rule_File_Name (RF_Name);

      Include_RF_Name : String_Access;

      procedure Scan_Line_Buf (Success : in out Boolean);
      --  Scans Line_Buff, tries to select the rule option or '-from' option
      --  from the sequence of scanned lines and copies this option into
      --  Rule_Buf, setting Rule_Len accordingly. Spaces, comments and empty
      --  lines are skipped.
      --
      --  At the moment, we do not process spaces inside parameters. We will
      --  come back to this problem as soon as we get the first rule that would
      --  need spaces in the parameters
      --
      --  Success is set OFF if a serious problem that does not allow to
      --  continue parsing the rule file has been encountered

      function Is_Opt_Start (Idx : Positive) return Boolean;
      --  Check that Idx (that is supposed to be an index in Line_Buf) points
      --  to the beginning of the rule or '-from' option. If the result is
      --  True, as a side effect this function sets New_State to In_Rule_Option
      --  if the beginning of the rule option is detected, or it sets it
      --  to In_From_Option if the beginning of '-from' option is detected.
      --  If the result is False, this function does not change New_State

      procedure Set_File_Name (Success : in out Boolean);
      --  This procedure is supposed to be called when the  rule buffer
      --  contains (the whole) '-from' option. It scans the buffer and tries
      --  to locate the name of the rule file that is a part of this option.
      --  It sets Success OFF if it can not locate the file name because of
      --  any reason. Otherwise it copies the file name in the beginning of the
      --  line buffer and updates Rule_Len accordingly

      ------------------
      -- Is_Opt_Start --
      ------------------

      function Is_Opt_Start (Idx : Positive) return Boolean is
         Result : Boolean := False;
      begin

         if (Line_Buf (Idx) = '+' or else Line_Buf (Idx) = '-')
           and then
            (Idx = 1 or else Is_White_Space (Line_Buf (Idx - 1)))
           and then
             Idx + 1 < Line_Len
         then

            if Line_Buf (Idx + 1) = 'R' then
               Result    := True;
               New_State := In_Rule_Option;
            elsif Idx <= Line_Len - 3
               and then
                  Line_Buf (Idx + 1 .. Idx + 3) = "ALL"
            then

               if Idx + 3 = Line_Len
                 or else
                  (Idx + 3 < Line_Len
                  and then
                   Is_White_Space (Line_Buf (Idx + 4)))
                 or else
                  (Idx + 4 < Line_Len
                  and then
                    Line_Buf (Idx + 4 .. Idx + 5) = "--")
               then
                  Result    := True;
                  New_State := In_Rule_Option;
               end if;

            elsif Idx + 4 <= Line_Len
              and then
               Line_Buf (Idx .. Idx + 4) = "-from"
            then
               Result    := True;
               New_State := In_From_Option;
            end if;

         end if;

         return Result;
      end Is_Opt_Start;

      -------------------
      -- Scan_Line_Buf --
      -------------------

      procedure Scan_Line_Buf (Success : in out Boolean) is
         Idx : Positive := 1;
      begin

         while Idx <= Line_Len loop

            --  White spaces are just ignored. We also skip CR and LF in case
            --  if the rule file has wrong line ends for the given platform

            if Is_White_Space (Line_Buf (Idx))
              or else
               Line_Buf (Idx) = ASCII.CR
              or else
               Line_Buf (Idx) = ASCII.LF
            then
               Idx := Idx + 1;
            else

               --  First, filter out the situation when we have a comment
               if Line_Buf (Idx) = '-'
                 and then
                   Idx < Line_Len
                 and then
                   Line_Buf (Idx + 1) = '-'
               then
                  --  nothing else can be done with this line, so
                  return;
               end if;

               --  Here we have non-blank character that is not the beginning
               --  of the comment

               if Is_Opt_Start (Idx) then
                  --  Start of the new rule option

                  Last_Rule_Opt_Start_Col_Old  := Last_Rule_Opt_Start_Col;
                  Last_Rule_Opt_Start_Line_Old := Last_Rule_Opt_Start_Line;

                  if Line_Buf (Idx) = '+'
                    and then
                     Line_Buf (Idx + 1) = 'R'
                  then
                     Last_Rule_Opt_Start_Line     := Current_Line;
                     Last_Rule_Opt_Start_Col      := Idx;
                  end if;

                  if Rule_Len > 0 then
                     --  We need this condition to process correctly the very
                     --  first option in the rule file

                     case Old_State is
                        when In_Rule_Option =>
                           Process_Rule_Option
                             (Rule_Buf (1 .. Rule_Len),
                              Rule_File_Name & ":" &
                              Image (Last_Rule_Opt_Start_Line_Old) & ":" &
                              Image (Last_Rule_Opt_Start_Col_Old));
                        when In_From_Option =>
                           Set_File_Name (Success);

                           if not Success then
                              Error
                                 ("bad format of rule file "   &
                                  RF_Name & ", part of lines " &
                                  Image (Rule_Start_Line)      &
                                  ":"                          &
                                  Image (Current_Line)         &
                                  " ignored");

                              Success := True;
                              --  To allow further processing of this rule file
                           else
                              if Is_Regular_File
                                (Rule_Buf (1 .. Rule_Len))
                              then
                                 Process_Rule_File (Rule_Buf (1 .. Rule_Len));
                              else
                                 Error ("can not locate rule file " &
                                 Rule_Buf (1 .. Rule_Len));
                              end if;

                           end if;

                        when Indefinite =>
                           Error
                             ("bad format of rule file "   &
                              RF_Name & ", lines "         &
                              Image (Rule_Start_Line)      &
                              ":"                          &
                              Image (Current_Line - 1)     &
                              " do not have format of rule option");
                     end case;
                  end if;

                  Rule_Len  := 0;
                  Rule_Start_Line := Current_Line;
                  Old_State := New_State;

                  Rule_Len            := Rule_Len + 1;
                  Rule_Buf (Rule_Len) := Line_Buf (Idx);
                  Idx := Idx + 1;
               else

                  if Rule_Len < Rule_Buf_Last then
                     Rule_Len            := Rule_Len + 1;
                     Rule_Buf (Rule_Len) := Line_Buf (Idx);
                     Idx := Idx + 1;
                  else
                     Error ("can not read rule options from " & RF_Name);
                     Error_No_Tool_Name
                       ("(too long rule option, the content of the file " &
                        "ignored starting from line " & Image (Current_Line));
                     Success := False;
                     return;
                  end if;

               end if;

            end if;

         end loop;

      end Scan_Line_Buf;

      -------------------
      -- Set_File_Name --
      -------------------

      procedure Set_File_Name (Success : in out Boolean) is
         First_Idx : Natural := 0;
         Last_Idx  : Natural := Rule_Len;
         --  We will try to set First_Idx and Last_Idx pointing to the part
         --  of Line_Buf that could be a file name

         Eq_Detected : Boolean := False;
      begin

         --  Set First_Idx:

         for J in 6 .. Rule_Len loop
            --  6 means that we skip '-from'

            if not Is_White_Space (Rule_Buf (J)) then

               case Rule_Buf (J) is
                  when '=' =>

                     if not Eq_Detected then
                        Eq_Detected := True;
                        --  this means that we have '-from = <file_name>'
                     else
                        --  a file name can not start from '='
                        exit;
                     end if;

                  when '.'        |
                       '/'        |
                       '\'        |
                       '~'        |
                       'a' .. 'z' |
                       'A' .. 'Z' |
                       '0' .. '9' =>
                     --  This can be the beginning of a file name
                     First_Idx := J;
                     exit;
                  when others =>
                     --  a file name can not start from this character
                     exit;
               end case;

            end if;

         end loop;

         if First_Idx = 0 then
            Success := False;
            return;
         end if;

         --  Set Last_Idx:

         for J in First_Idx + 1 .. Rule_Len loop

            if Is_White_Space (Rule_Buf (J)) then
               Last_Idx := J - 1;
               exit;
            end if;

         end loop;

         --  Check that we have nothing after Last_Idx:

         for J in Last_Idx + 1 .. Rule_Len loop

            if not Is_White_Space (Line_Buf (J)) then
               Success := False;
               exit;
            end if;

         end loop;

         if Success then
            Rule_Len                 := Last_Idx - First_Idx + 1;
            Rule_Buf (1 .. Rule_Len) := Rule_Buf (First_Idx .. Last_Idx);
         end if;

      end Set_File_Name;

   begin -- Process_Rule_File

      if not Is_Regular_File (Rule_File_Name) then
         Error ("can not locate rule file " & Rule_File_Name);
         return;
      else
         Check_For_Looping (Rule_File_Name, Success);

         if not Success then
            return;
         end if;

      end if;

      Open (RF, In_File, Rule_File_Name);

      Rule_Len := 0;

      while Success and then not End_Of_File (RF) loop
         Get_Line (RF, Line_Buf, Line_Len);
         Current_Line :=  Current_Line + 1;

         Scan_Line_Buf (Success);
      end loop;

      --  Process the last rule option, if any

      if Rule_Len > 0 then

         case Old_State is
            when In_Rule_Option =>
               Process_Rule_Option
                 (Rule_Buf (1 .. Rule_Len),
                  Rule_File_Name & ":" & Image (Last_Rule_Opt_Start_Line) &
                  ":" & Image (Last_Rule_Opt_Start_Col));
            when In_From_Option =>
               Set_File_Name (Success);

               if not Success then
                  Error
                     ("bad format of rule file "          &
                      Rule_File_Name & ", part of lines " &
                      Image (Rule_Start_Line)             &
                      ":"                                 &
                      Image (Current_Line)                &
                      " ignored");

                  Success := True;
                  --  To allow further processing of this rule file
               else
                  Include_RF_Name :=
                    new String'(Get_Rule_File_Name (Rule_Buf (1 .. Rule_Len)));

                  if Is_Regular_File (Include_RF_Name.all) then
                     Process_Rule_File (Include_RF_Name.all);
                  else
                     Error ("can not locate rule file " &
                     Rule_Buf (1 .. Rule_Len));
                  end if;

                  Free (Include_RF_Name);
               end if;

            when Indefinite =>
               Error
                 ("bad format of rule file "          &
                  Rule_File_Name & ", lines "         &
                  Image (Rule_Start_Line)             &
                  ":"                                 &
                  Image (if New_State = Indefinite then
                            Current_Line
                         else Current_Line - 1)       &
                  " do not have format of rule option");
         end case;

      end if;

      Close (RF);

      Pop_Rule_File;
   exception
      when ASIS_UL.Common.Fatal_Error =>
         raise;
      when others =>
         Error ("cannot read rule options from " & Rule_File_Name);

         if Is_Open (RF) then
            Close (RF);
         end if;

         --  Exception info will be generated in main driver
         raise;
   end Process_Rule_File;

   -------------------------
   -- Process_Rule_Option --
   -------------------------

   procedure Process_Rule_Option
     (Option     : String;
      Defined_At : String)
   is
      First_Idx : constant Natural := Option'First;
      Last_Idx  : constant Natural := Option'Last;

      Word_Start : Natural := 0;
      Word_End   : Natural := 0;
      --  Should be set to select the next subword from Option - either the
      --  rule name or a rule parameter

      Rule_Synonym_Start : Natural := 0;
      Rule_Synonym_End   : Natural := 0;
      --  Set to point to the beginning and to the end of the user-defined
      --  rule synonyv (if any).

      procedure Set_Parameter;
      --  Provided that Word_Start points to the beginning of the rule name or
      --  rule parameter, sets Word_Start and Word_End to point to the next
      --  parameter, Sets Word_Start to 0 if there is no parameter any more.
      --  This procedure also checks the syntax of the rule option - that is,
      --  that the rule name is separated from parameter(s) by ':', and
      --  parameters are separated by ',', if this check fails, Word_Start is
      --  set to 0.

      Rule    : Rule_Id;
      Enable  : Boolean;

      Diag_Defined_At : constant String :=
        (if Defined_At = "" then
            ""
         else " (" & Defined_At & ")");

      procedure Set_Parameter is
         Found : Boolean := False;
      begin
         if Word_End < Last_Idx then
            Word_Start := Word_End + 2;
         else
            Word_Start := 0;
            return;
         end if;

         --  Set Word_Start to the first non-blank and non-comma character.
         --  We skip empty parameters like this
         --
         --     +RRule:"par1, par2,  , par3"

         for J in Word_Start .. Option'Last loop

            if not (Is_White_Space (Option (J))
                 or else
                    Option (J) = ',')
            then
               Found      := True;
               Word_Start := J;
               exit;
            end if;

         end loop;

         if not Found then
            Word_Start := 0;
            return;
         end if;

         Word_End := Last_Idx;

         for J in Word_Start + 1 .. Last_Idx loop

            if Option (J) = ',' then
               Word_End := J - 1;
               exit;
            end if;

         end loop;

      end Set_Parameter;

   begin

      if Option = "-ALL" then
         Turn_All_Rules_Off;
         return;
      end if;

      if Last_Idx - First_Idx > 2
       and then
         (Option (First_Idx) = '+'
         or else
          Option (First_Idx) = '-')
       and then
          Option (First_Idx + 1) = 'R'
      then
         Enable := Option (First_Idx) = '+';

         --  Computing the rule name and defining the rule

         Word_Start := First_Idx + 2;

         --  Check if we have a user-defined rule synonym for the rule name
         if Option (Word_Start) = ':' then
            Word_End := Index (Option (Word_Start + 1 .. Last_Idx), ":");

            if Word_End = 0 then
               Error ("bad structure of rule option " & Option &
                      Diag_Defined_At);
               return;
            end if;

            Rule_Synonym_Start := Word_Start + 1;
            Rule_Synonym_End   := Word_End - 1;

            Word_Start := Word_End + 1;
         end if;

         Word_End := Last_Idx;

         for J in Word_Start + 1 .. Last_Idx loop

            if Option (J) = ':' then
               Word_End := J - 1;
               exit;
            end if;

         end loop;

         --  Special processing for Metrics rule (old rule syntax)
         if Is_Old_Metric_Rule_Syntax (Option (Word_Start .. Last_Idx)) then
            Process_Old_Metric_Rule_Option
              (Option (Word_Start .. Last_Idx),
               Enable,
               Defined_At);

            return;
         end if;

         --  Separate processing for restrictions, warnings and ordinary
         --  rules

         if To_Lower (Option (Word_Start .. Word_End)) = "restrictions" then

            Set_Parameter;

            if Word_Start = 0 then
               Error ("restrictions rule option must have a parameter" &
                      Diag_Defined_At);
               return;
            else

               while Word_Start /= 0 loop
                  Process_Restriction_Param
                    (Option (Word_Start .. Word_End),
                     Enable);

                  Set_Parameter;
               end loop;

            end if;

         elsif To_Lower (Option (Word_Start .. Word_End)) = "style_checks" then

            if not Enable then
               Error ("there is no -R option for style checks, " &
                      "use style options to turn checks OFF"     &
                      Diag_Defined_At);
               return;
            end if;

            Set_Parameter;

            if Word_Start = 0 then
               Error ("style_checks rule option must have a parameter" &
                      Diag_Defined_At);
               return;
            else

               while Word_Start /= 0 loop
                  Process_Style_Check_Param
                    (Option (Word_Start .. Word_End));

                  Set_Parameter;
               end loop;

            end if;

         elsif To_Lower (Option (Word_Start .. Word_End)) = "warnings" then

            if not Enable then
               Error ("there is no -R option for warnings, "     &
                      "use warning options to turn warnings OFF" &
                       Diag_Defined_At);
               return;
            end if;

            Set_Parameter;

            if Word_Start = 0 then
               Error ("warnings rule option must have a parameter" &
                      Diag_Defined_At);
               return;
            else

               while Word_Start /= 0 loop
                  Process_Warning_Param (Option (Word_Start .. Word_End));
                  Set_Parameter;
               end loop;

            end if;

         else

            Rule := Get_Rule (Option (Word_Start .. Word_End));

            if Present (Rule) then

               Set_Parameter;

               if Enable
                and then
                  Rule_Synonym_Start > 0
               then
                  Free (All_Rules.Table (Rule).User_Synonym);
                  All_Rules.Table (Rule).User_Synonym :=
                    new String'(Option
                      (Rule_Synonym_Start .. Rule_Synonym_End));
               end if;

               if Word_Start = 0 then

--                  if Enable then
--                     All_Rules.Table (Rule).Rule_State := Enabled;
--                  else
--                     All_Rules.Table (Rule).Rule_State := Disabled;
--                  end if;

                  Process_Rule_Parameter
                    (Rule       => All_Rules.Table (Rule).all,
                     Param      => "",
                     Enable     => Enable,
                     Defined_At => Defined_At);
               else

                  while Word_Start /= 0 loop

                     Process_Rule_Parameter
                       (Rule       => All_Rules.Table (Rule).all,
                        Param      => Option (Word_Start .. Word_End),
                        Enable     => Enable,
                        Defined_At => Defined_At);

                     Set_Parameter;
                  end loop;

               end if;

            else
               Error ("unknown rule : " & Option (Word_Start .. Word_End) &
                      ", ignored" & Diag_Defined_At);
            end if;

         end if;

      else
         Error ("unknown rule option: " & Option & ", ignored" &
                 Diag_Defined_At);
      end if;

   end Process_Rule_Option;

   ------------------------------
   -- Processed_Rule_File_Name --
   ------------------------------

   function Processed_Rule_File_Name return String is
   begin
      if Rule_File_Stack.Is_Empty then
         return "";
      else
         return Rule_File_Stack.Table (Rule_File_Stack.Last).Full_Name.all;
      end if;
   end Processed_Rule_File_Name;

   ---------------
   -- Rule_Name --
   ---------------

   function Rule_Name (R : Rule_Id) return String is
   begin
      pragma Assert (Present (R));

      case R is
         when Restrictions_Id =>
            return "Restrictions";
         when Style_Checks_Id =>
            return "Style_Checks";
         when Warnings_Id =>
            return "Warnings";
         when others =>
            return All_Rules.Table (R).Name.all;
      end case;
   end Rule_Name;

   -----------------
   -- Rule_Status --
   -----------------

   function Rule_Status (R : Rule_Id) return Rule_Statuses is
   begin
      pragma Assert (Present (R));
      return All_Rules.Table (R).Rule_Status;
   end Rule_Status;

   ----------------
   -- Rules_Help --
   ----------------

   procedure Rules_Help is
   begin

      Info ("gnatcheck currently implements the following rules:");

      if All_Rules.Last < First_Rule then
            Info ("  There is no rule implemented");
      else

         for J in First_Rule .. All_Rules.Last loop

            if All_Rules.Table (J).Rule_Status = Fully_Implemented then
               Print_Rule_Help (All_Rules.Table (J).all);
            end if;

         end loop;

      end if;

      Info ("gnatcheck allows activation of the following checks " &
            "provided by GNAT");
      Info ("using the same syntax to control these checks as for other " &
            "rules:");
      Info ("  Warnings     - compiler warnings" &
            (if Debug_Flag_RR then
                " - EASY"
             else ""));

      Info ("  Style_Checks - compiler style checks" &
            (if Debug_Flag_RR then
                " - TRIVIAL"
             else ""));

      Info ("  Restrictions - checks made by pragma Restriction_Warnings" &
            (if Debug_Flag_RR then
                " - EASY"
             else ""));

   end Rules_Help;

   ---------------------
   --  Set_Rule_State --
   ---------------------

   procedure Set_Rule_State (For_Rule : Rule_Id; To_State : Rule_States) is
   begin
      pragma Assert (Present (For_Rule));
      All_Rules.Table (For_Rule).Rule_State := To_State;
   end Set_Rule_State;

   ------------------------
   -- Turn_All_Rules_Off --
   ------------------------

   procedure Turn_All_Rules_Off is
   begin

      for J in All_Rules.First .. All_Rules.Last loop

         if All_Rules.Table (J).all not in Internal_Rule_Template'Class then
            All_Rules.Table (J).Rule_State := Disabled;
         end if;

      end loop;

   end Turn_All_Rules_Off;

   -------------------------------
   -- Turn_All_Global_Rules_Off --
   -------------------------------

   procedure Turn_All_Global_Rules_Off is
   begin

      for J in All_Rules.First .. All_Rules.Last loop

         if All_Rules.Table (J).all in Global_Rule_Template'Class then
            All_Rules.Table (J).Rule_State := Disabled;
         end if;

      end loop;

   end Turn_All_Global_Rules_Off;

   ------------------------------
   -- Turn_All_Global_Rules_On --
   ------------------------------

   procedure Turn_All_Global_Rules_On is
   begin

      for J in All_Rules.First .. All_Rules.Last loop

         if All_Rules.Table (J).all in Global_Rule_Template'Class then
            All_Rules.Table (J).Rule_State := Enabled;
         end if;

      end loop;

   end Turn_All_Global_Rules_On;

end Gnatcheck.Rules.Rule_Table;
