------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                   G N A T C H E C K . C O M P I L E R                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

with System.Rident;

with Asis.Extensions.Strings; use Asis.Extensions.Strings;

with GNAT.OS_Lib;             use GNAT.OS_Lib;

with ASIS_UL.Misc;            use ASIS_UL.Misc;
with ASIS_UL.Output;          use ASIS_UL.Output;

with Gnatcheck.Diagnoses_Old;
with Gnatcheck.Diagnoses;     use Gnatcheck.Diagnoses;
with Gnatcheck.Ids;           use Gnatcheck.Ids;
with Gnatcheck.Options;

package body Gnatcheck.Compiler is

   Tmp_Options    : String_Access;

   Style_Options_String : String_Access;
   --  Stores parameters of the Style_Checks rule as is.

   Warning_Options_String : String_Access;
   --  Stores parameters of the Warnings rule as is.

   procedure Process_Style_Options (Param : String);
   --  Stores Param as parameter of the compiler -gnaty... option as is,
   --  (if some -gnaty... parameter has already been stored, appends Param to
   --  it.)

   function Adjust_Message
     (Diag         : String;
      Message_Kind : Compiler_Message_Kinds;
      SF           : SF_Id)
      return String;
   --  Does the following adjustments:
   --
   --  * Remove from the diagnostic message the reference to the configuration
   --    file with restriction pragmas that is created by gnatcheck.
   --
   --  * If Gnatcheck.Options.Mapping_Mode is ON, annotates the message by
   --    adding the compiler check (if for a warning message '.d' is specified,
   --    the trailing part that indicates the warning message that causes this
   --    warning is removed from the diagnosis, and the corresponding warning
   --    parameter is added to the annotation.
   --
   --  * if '-l' option is set (use full file names), replaces the short file
   --    name with the corresponding full name

   function Annotation
     (Message_Kind : Compiler_Message_Kinds;
      Parameter    : String)
      return         String;
   --  Returns annotation to be added to the compiler diagnostic message if
   --  Gnatcheck.Options.Mapping_Mode is ON. Parameter, if non-empty, is the
   --  parameter of '-gnatw' option that causes the diagnosis

   function Get_Rule_Id (Check : Compiler_Message_Kinds) return Rule_Id;
   --  Returns the Id corresponding to the given compiler check

   ---------------------------------------------------------
   -- Data structures and routines for restriction checks --
   ---------------------------------------------------------

   subtype Option_Parameter is Natural;

   package Gnatcheck_Restrictions is new System.Rident;
   use Gnatcheck_Restrictions;
   --  We cannot use the instantiation of System.Rident in System.Restrictions
   --  because of the pragma Discard_Names that does not allow to use
   --  Restriction_Id'Value when analyzing gnatcheck restriction parameters.

   type Restriction_State is record
      Active : Boolean;
      Param  : String_List_Access;
   end record;
   --  We can not use Option_Parameter here, because some restrictions (e.g.
   --  Max_Task_Entries) may be active and may have zero parameter

   Restriction_Setting : array (All_Restrictions) of Restriction_State :=
     (others => (False, null));
   --  This array represents only restrictions that are values of
   --  System.Rident.Restriction_Id. But we need to process restrictions that
   --  are not included in values of this type.

   type Special_Restriction_Id is
      (Not_A_Special_Restriction_Id,
       No_Dependence);

   subtype All_Special_Restrictions is Special_Restriction_Id range
     No_Dependence .. No_Dependence;
   --  All special restrictions, excluding Not_A_Special_Restriction_Id.

   subtype All_Special_Parameter_Restrictions is Special_Restriction_Id range
     No_Dependence .. No_Dependence;
   --  All special restrictions that have a parameter

   function Needs_Parameter_In_Exemption
     (R    : Restriction_Id;
      SR   : Special_Restriction_Id)
      return Boolean;
   --  Checks if R or SR denotes a restriction that needs a restriction
   --  parameter if used in parametric rule exemption (such as
   --  'No_Dependence => Foo).

   Special_Restriction_Setting : array (All_Special_Restrictions)
     of Boolean := (others => False);
   --  This array only indicates if a given special restriction is ON or OFF,
   --  we cannot store any restriction parameter information, because
   --  parameter format is restriction-specific

   package Forbidden_Units_Dictionary is new Simple_String_Dictionary
     (Dictionary_Name => "Forbidden units dictionary");

   --------------------
   -- Adjust_Message --
   --------------------

   function Adjust_Message
     (Diag         : String;
      Message_Kind : Compiler_Message_Kinds;
      SF           : SF_Id)
    return String
    is
      Result    : constant String (1 .. Diag'Length) := Diag;
      Last_Idx  :           Natural;
      Diag_End  :           Natural;
      Par_Start :           Natural := 1;
      Par_End   :           Natural := 0;
      Fname_End :           Natural := 0;
   begin
      Last_Idx := Index (Result, Gnatcheck_Config_File);

      if Last_Idx = 0 then
         Last_Idx := Result'Last;
      else
         Last_Idx := Last_Idx - 5;
      end if;

      if Gnatcheck.Options.Mapping_Mode then

         if Message_Kind = General_Warning then
            Diag_End := Index (Source  => Result (1 .. Last_Idx),
                               Pattern => "[-gnatw",
                               Going   => Backward);

            if Diag_End = 0 then
               Diag_End := Last_Idx;
            else
               Par_Start := Diag_End + 7;
               Par_End   := Par_Start;

               if Result (Par_End) = '.' then
                  Par_End := Par_End + 1;
               end if;

               Diag_End := Diag_End - 2;

            end if;
         else
            Diag_End := Last_Idx;
         end if;

         if Gnatcheck.Options.Full_Source_Locations then
            Fname_End := Index (Result, (1 => ':'));
            return Source_Name (SF) & Result (Fname_End .. Diag_End) &
                   Annotation (Message_Kind, Result (Par_Start .. Par_End));
         else
            return Result (1 .. Diag_End) &
                   Annotation (Message_Kind, Result (Par_Start .. Par_End));
         end if;
      else
         if Gnatcheck.Options.Full_Source_Locations then
            Fname_End := Index (Result, (1 => ':'));
            return Source_Name (SF) & Result (Fname_End .. Last_Idx);
         else
            return Result (1 .. Last_Idx);
         end if;
      end if;
   end Adjust_Message;

   ----------------------------
   -- Analyze_Error_Messages --
   ----------------------------

   procedure Analyze_Error_Messages
     (Compiler_Out :     String;
      Wrong_Option : out Boolean)
   is
      Next_Line     : String (1 .. 1024);
      Line_Len      : Natural;
      Comp_Out_File : File_Type;

   begin
      Wrong_Option := False;

      Open (File => Comp_Out_File,
            Mode => In_File,
            Name => Compiler_Out);

      while not End_Of_File (Comp_Out_File) loop
         Get_Line (Comp_Out_File, Next_Line, Line_Len);

         if Line_Len >= 24
           and then
            Next_Line (1 .. 24) = "gnat1: bad -gnaty switch"
         then
            Wrong_Option := True;
         end if;

         if not Wrong_Option
           and then
            Line_Len > 29
           and then
            Next_Line (1 .. 29) = "gnat1: invalid switch: -gnatw"
         then
            Wrong_Option := True;
         end if;

         if Wrong_Option then
            Error ("wrong parameter specified for compiler-related rule:");
            Error_No_Tool_Name (Next_Line (1 .. Line_Len));
            exit;
         end if;
      end loop;

      Close (Comp_Out_File);
   end Analyze_Error_Messages;

   -----------------
   -- Annotation --
   ----------------

   function Annotation
     (Message_Kind : Compiler_Message_Kinds;
      Parameter    : String)
      return         String
   is
   begin
      case Message_Kind is
         when Not_A_Compiler_Nessage =>
            pragma Assert (False);
            return "";
         when General_Warning =>
            return " [Warnings" &
                   (if Parameter = "" then
                       ""
                    else
                       ":" & Parameter) & "]";
         when Style =>
            return " [Style_Checks]";
         when Restriction =>
            return " [Restrictions]";
      end case;
   end Annotation;

   -------------------------------
   -- Analyze_Compiler_Warnings --
   -------------------------------

   procedure Analyze_Compiler_Warnings
     (Compiler_Out :     String;
      For_File     :     SF_Id;
      Success      : out Boolean)
   is
      Next_Line     : String (1 .. 1024);
      Line_Len      : Natural;
      Comp_Out_File : File_Type;

      procedure Analyze_Warning (Msg : String);
      --  Analyses one line containing the compiler warning. Inserts the
      --  warning messages into gnatcheck diagnoses table.

      procedure Analyze_Warning (Msg : String) is
         SF       : SF_Id;
         Line_Num : Positive;
         Col_Num  : Natural;
         --  Coordinates of the warning message

         Diag     : String_Loc;
         --  We store the whole warning message generated by the compiler as is
         --  This would result in some considerable duplications, but what
         --  would be better approach here ???

         Compiler_Message_Kind : Compiler_Message_Kinds :=
           Not_A_Compiler_Nessage;

         First_Idx : constant Natural := Msg'First;
         Last_Idx  : Natural  := Msg'Last;
         Idx       : Natural := First_Idx;
         Word_End  : Natural  := 0;

      begin
         if Last_Idx = 0 then
            --  An empty line?
            return;
         end if;

         --  We assume the following format of the message:
         --
         --   filename:line:column: <message body>
         --
         --  If -dJ is set, <message body> has the following structure
         --
         --    [warning] scopename: line:col: text
         --
         --  So the first thing we have to do is to skip 3 colons and to define
         --  the source the message is siiued for, and the line and column
         --  numbers:

         Idx := Index (Msg (Idx .. Last_Idx), ":");

         if Idx = 0 then
            Error ("Unexpected format of compiler message for " &
                   Short_Source_Name (For_File) & ":");

            Error_No_Tool_Name (Msg);

            Success := False;
            return;
         end if;

         SF := File_Find (Msg (First_Idx .. Idx - 1), Use_Short_Name => True);

         if not Is_Argument_Source (SF) then
            --  This source is not an argument of this check
            return;
         end if;

         Word_End := Index (Msg (Idx + 1 .. Last_Idx), ":");

         if Word_End = 0 then
            Error ("Unexpected format of compiler message for " &
                   Short_Source_Name (For_File) & ":");

            Error_No_Tool_Name (Msg);

            Success := False;
            return;
         end if;

         begin
            Line_Num := Positive'Value (Msg (Idx + 1 .. Word_End - 1));
         exception
            when others =>
               Error ("Unexpected format of compiler message for " &
                      Short_Source_Name (For_File) & ":");

               Error_No_Tool_Name (Msg);

               Success := False;
               return;
         end;

         Idx := Word_End;

         Word_End := Index (Msg (Idx + 1 .. Last_Idx), ":");

         if Word_End = 0 then
            Error ("Unexpected format of compiler message for " &
                   Short_Source_Name (For_File) & ":");

            Error_No_Tool_Name (Msg);

            Success := False;
            return;
         end if;

         begin
            Col_Num := Positive'Value (Msg (Idx + 1 .. Word_End - 1));
         exception
            when others =>
               Error ("Unexpected format of compiler message for " &
                      Short_Source_Name (For_File) & ":");

               Error_No_Tool_Name (Msg);

               Success := False;
               return;
         end;

         Idx := Word_End + 2;

         if Msg (Idx .. Idx + 8) = "warning: " then

            if Index (Msg (Idx .. Last_Idx), ": violation of restriction") /= 0
            then
               Compiler_Message_Kind := Restriction;
            else
               Compiler_Message_Kind := General_Warning;
            end if;

         elsif Index (Msg (Idx .. Last_Idx), "(style)") /= 0 then
            Compiler_Message_Kind := Style;
         else
            Error ("Unexpected format of compiler message for " &
                   Short_Source_Name (For_File) & ":");

            Error_No_Tool_Name (Msg);

            Success := False;
            return;
         end if;

         if Compiler_Message_Kind = Restriction then
            Last_Idx := Index (Msg, Gnatcheck_Config_File);
            Last_Idx := Last_Idx - 5;
         end if;

         Diag := Enter_String (Msg (Idx .. Last_Idx));

         Gnatcheck.Diagnoses_Old.Store_Compiler_Message
           (In_SF        => SF,
            Line_Num     => Line_Num,
            Col_Num      => Col_Num,
            Message      => Diag,
            Message_Kind => Compiler_Message_Kind);

         Gnatcheck.Diagnoses.Store_Diagnosis
            (Text           => Adjust_Message (Msg, Compiler_Message_Kind, SF),
             Diagnosis_Kind => Gnatcheck.Diagnoses.Rule_Violation,
             SF             => SF,
             Rule           => Get_Rule_Id (Compiler_Message_Kind));

      end Analyze_Warning;

   begin
      Open (File => Comp_Out_File,
            Mode => In_File,
            Name => Compiler_Out);

      Success := True;

      while not End_Of_File (Comp_Out_File) loop
         Get_Line (Comp_Out_File, Next_Line, Line_Len);
         Analyze_Warning (Next_Line (1 .. Line_Len));
      end loop;

      Close (Comp_Out_File);
   exception
      when Ex : others =>
         if Is_Open (Comp_Out_File) then
            Close (Comp_Out_File);
         end if;

         Error
           ("unknown bug detected when analyzing compiler warnings");
         Error_No_Tool_Name
           ("Please submit bug report to report@adacore.com");
         Report_Unhandled_Exception (Ex);

         Success := False;
   end Analyze_Compiler_Warnings;

   -------------------------------------
   -- Create_Restriction_Pragmas_File --
   -------------------------------------

   procedure Create_Restriction_Pragmas_File is
      RPF : File_Type;
   begin
      Create (File => RPF,
              Mode => Out_File,
              Name => Gnatcheck_Config_File);

      Put_Line (RPF, "pragma Warnings (Off, ""[enabled by default]"");");

      for R in All_Restrictions loop

         if Restriction_Setting (R).Active then
            if R in All_Boolean_Restrictions then
               Put_Line (RPF, "pragma Restriction_Warnings (" & R'Img & ");");
            else
               for J in Restriction_Setting (R).Param'Range loop
                  Put (RPF, "pragma Restriction_Warnings (");
                  Put (RPF, R'Img);
                  Put (RPF, " =>"  & Restriction_Setting (R).Param (J).all);
                  Put_Line (RPF, ");");
               end loop;
            end if;
         end if;

      end loop;

      for R in Special_Restriction_Setting'Range loop

         if Special_Restriction_Setting (R) then

            case R is

               when No_Dependence =>
                  Forbidden_Units_Dictionary.Reset_Iterator;

                  while not Forbidden_Units_Dictionary.Done loop
                     Put
                       (RPF,
                        "pragma Restriction_Warnings (No_Dependence => ");
                     Put_Line
                       (RPF,
                        Forbidden_Units_Dictionary.Next_Entry & ");");

                  end loop;

            end case;

         end if;

      end loop;

      Close (RPF);
   end Create_Restriction_Pragmas_File;

   -----------------
   -- Get_Rule_Id --
   -----------------

   function Get_Rule_Id (Check : Compiler_Message_Kinds) return Rule_Id is
   begin
      case Check is
         when Not_A_Compiler_Nessage =>
            pragma Assert (False);
            return No_Rule;
         when General_Warning =>
            return Warnings_Id;
         when Style =>
            return Style_Checks_Id;
         when Restriction =>
            return Restrictions_Id;
      end case;
   end Get_Rule_Id;

   ----------------------------------
   -- Get_Specified_Warning_Option --
   ----------------------------------

   function Get_Specified_Warning_Option return String is
   begin
      if Warning_Options_String /= null then
         return "-gnatw" & Warning_Options_String.all;
      else
         return "";
      end if;
   end Get_Specified_Warning_Option;

   ----------------------
   -- Get_Style_Option --
   ----------------------

   function Get_Style_Option return String is
   begin
      return "-gnaty" & Style_Options_String.all;
   end Get_Style_Option;

   ------------------------
   -- Get_Warning_Option --
   ------------------------

   function Get_Warning_Option return String is
   begin
      --  We disable defaults first
      if Warning_Options_String /= null then
         return "-gnatw" & "AIOVZX" & Warning_Options_String.all;
      else
         return "-gnatwAIOVZX";
      end if;

   end Get_Warning_Option;

   ----------------------------------
   -- Needs_Parameter_In_Exemption --
   ----------------------------------

   function Needs_Parameter_In_Exemption
     (R    : Restriction_Id;
      SR   : Special_Restriction_Id)
      return Boolean
   is
      Result : Boolean := False;
   begin
      if SR in All_Special_Parameter_Restrictions then
         Result := True;
      elsif R in All_Parameter_Restrictions then
         --  Not all the restrictions from All_Parameter_Restrictions require
         --  restriction parameter in parametric exemptions
         Result := R not in Integer_Parameter_Restrictions;
      end if;

      return Result;
   end Needs_Parameter_In_Exemption;

   ----------------------------------
   -- Is_Restriction_Exemption_Par --
   ----------------------------------

   function Is_Restriction_Exemption_Par (Par : String) return Boolean is
      Result        :          Boolean  := False;
      Arrow_Idx     : constant Natural  := Index (Par, "=>");
      Rest_Name_End :          Natural  := Par'Last;
      Par_Start     : constant Positive := Par'First;
      R_Id          :          Restriction_Id         := Not_A_Restriction_Id;
      Special_R_Id  :          Special_Restriction_Id :=
        Not_A_Special_Restriction_Id;
   begin

      if Arrow_Idx /= 0 then
         Rest_Name_End := Arrow_Idx - 1;

         while Rest_Name_End >= Par_Start
            and then
               Par (Par_Start) = ' '
         loop
            Rest_Name_End := Rest_Name_End - 1;
         end loop;

      end if;

      begin
         R_Id := Restriction_Id'Value (Par (Par_Start .. Rest_Name_End));
      exception
         when Constraint_Error =>
            R_Id := Not_A_Restriction_Id;
      end;

      if R_Id = Not_A_Restriction_Id then

         begin
            Special_R_Id :=
              Special_Restriction_Id'Value (Par (Par_Start .. Rest_Name_End));
         exception
            when Constraint_Error =>
               Special_R_Id := Not_A_Special_Restriction_Id;
         end;

      end if;

      if R_Id /= Not_A_Restriction_Id
        or else
         Special_R_Id /= Not_A_Special_Restriction_Id
      then
         if Arrow_Idx /= 0 then
            Result := Needs_Parameter_In_Exemption (R_Id, Special_R_Id);
         else
            Result := not Needs_Parameter_In_Exemption (R_Id, Special_R_Id);
         end if;
      end if;

      return Result;
   end Is_Restriction_Exemption_Par;

   ------------------------------
   -- Is_Warning_Exemption_Par --
   ------------------------------

   function Is_Warning_Exemption_Par (Par : String) return Boolean is
      Last_Idx : constant Positive := Par'Last;
      Result   :          Boolean  := True;
   begin
      --  We consider any string that can be used as a parameter of '-gnatw'
      --  option as allowed exemption parameter
      for J in Par'Range loop
         if not (Par (J) = '.' or else Is_Letter (Par (J))) then
            Result := False;
            exit;
         end if;

         if Par (J) = '.'
          and then
            (J = Last_Idx or else not Is_Letter (Par (J + 1)))
         then
            Result := False;
            exit;
         end if;
      end loop;

      return Result;
   end Is_Warning_Exemption_Par;

   -------------------------------
   -- Print_Active_Restrictions --
   -------------------------------

   procedure Print_Active_Restrictions (Ident_Level : Natural := 0) is
      Bool_Tmp : Boolean := True;
   begin

      for R in Restriction_Setting'Range loop

         if Restriction_Setting (R).Active then
            if R in All_Boolean_Restrictions then
               Report (Proper_Case (R'Img), Ident_Level);
            else
               for J in Restriction_Setting (R).Param'Range loop
                  Report_No_EOL (Proper_Case (R'Img), Ident_Level);
                  Report (" =>"  & Restriction_Setting (R).Param (J).all);
               end loop;
            end if;

         end if;

      end loop;

      for R in Special_Restriction_Setting'Range loop

         if Special_Restriction_Setting (R) then
            Report_No_EOL (Proper_Case (R'Img), Ident_Level);

            case R is
               when No_Dependence =>
                  Report_No_EOL (" => ");

                  Forbidden_Units_Dictionary.Reset_Iterator;

                  while not Forbidden_Units_Dictionary.Done loop

                     if Bool_Tmp then
                        Report (Forbidden_Units_Dictionary.Next_Entry);
                        Bool_Tmp := False;
                     else
                        Report
                          ("No_Dependence => " &
                           Forbidden_Units_Dictionary.Next_Entry,
                           Ident_Level);
                     end if;

                  end loop;

            end case;

         end if;

      end loop;

   end Print_Active_Restrictions;

   ---------------------------------------
   -- Print_Active_Restrictions_To_File --
   ---------------------------------------

   procedure Print_Active_Restrictions_To_File (Rule_File : File_Type) is
   begin

      for R in Restriction_Setting'Range loop

         if Restriction_Setting (R).Active then
            if R in All_Boolean_Restrictions then
               Put_Line (Rule_File, "+RRestrictions : " & Proper_Case (R'Img));
            else
               for J in Restriction_Setting (R).Param'Range loop
                  Put (Rule_File, "+RRestrictions : " & Proper_Case (R'Img));
                  Put_Line (Rule_File,
                            " =>"  & Restriction_Setting (R).Param (J).all);
               end loop;
            end if;

         end if;

      end loop;

      for R in Special_Restriction_Setting'Range loop

         if Special_Restriction_Setting (R) then

            case R is
               when No_Dependence =>
                  Forbidden_Units_Dictionary.Reset_Iterator;

                  while not Forbidden_Units_Dictionary.Done loop

                     Put      (Rule_File, "+RRestrictions : ");
                     Put      (Rule_File, Proper_Case (R'Img) & " => ");
                     Put_Line (Rule_File,
                               Forbidden_Units_Dictionary.Next_Entry);
                  end loop;

            end case;

         end if;

      end loop;

   end Print_Active_Restrictions_To_File;

   -------------------------------
   -- Process_Restriction_Param --
   -------------------------------

   procedure Process_Restriction_Param
     (Parameter : String;
      Enable    : Boolean)
   is
      Param        : constant String  := Trim (Parameter, Both);
      First_Idx    : constant Natural := Param'First;
      Last_Idx     :          Natural := Param'Last;
      Arg_Present  :          Boolean := False;
      R_Id         :          Restriction_Id;
      Special_R_Id :          Special_Restriction_Id;
      R_Val        :          Option_Parameter;
   begin
      --  Param should have the format
      --
      --   restriction_parameter_identifier[ => restriction_parameter_argument]
      --
      --  We assume that it can be spaces around '=>'

      --  First, try to define the restriction name.

      for J in First_Idx + 1 .. Last_Idx loop

         if Param (J) = ' '
            or else Param (J) = '='
         then
            Last_Idx := J - 1;
            exit;
         end if;

      end loop;

      begin
         R_Id := Restriction_Id'Value (Param (First_Idx .. Last_Idx));
      exception
         when Constraint_Error =>
            R_Id := Not_A_Restriction_Id;
      end;

      if R_Id = Not_A_Restriction_Id then

         begin
            Special_R_Id :=
              Special_Restriction_Id'Value (Param (First_Idx .. Last_Idx));
         exception
            when Constraint_Error =>
               Special_R_Id := Not_A_Special_Restriction_Id;
         end;

      end if;

      if R_Id = Not_A_Restriction_Id
        and then
         Special_R_Id = Not_A_Special_Restriction_Id
      then
         Error ("wrong restriction identifier : " &
                 Param (First_Idx .. Last_Idx) & ", ignored");
         return;
      end if;

      --  Check if we have a restriction_parameter_argument, and if we do,
      --  set First_Idx to the first character after '=>'

      for J in Last_Idx + 1 .. Param'Last - 2 loop

         if Param (J) = '=' then

            if J <= Param'Last - 2
               and then Param (J + 1) = '>'
            then
               Arg_Present := True;
               Last_Idx := J + 2;
               exit;
            else
               Error ("wrong structure of restriction rule parameter " &
                      Param & ", ignored");
               return;
            end if;

         end if;

      end loop;

      if not Enable then

         if R_Id in All_Restrictions then
            Restriction_Setting (R_Id).Active := False;
         else
            Special_Restriction_Setting (Special_R_Id) := False;
            --  We may need to correct stored parameters of some restrictions

            if Arg_Present then

               case Special_R_Id is
                  when No_Dependence =>
                     Forbidden_Units_Dictionary.Remove_From_Dictionary
                       (Trim (Param (Last_Idx .. Param'Last), Both));

                  when others =>
                     null;
               end case;

            end if;
         end if;

         return;
      end if;

      if R_Id in All_Boolean_Restrictions then

         if Arg_Present then
            Error ("RESTRICTIONS rule parameter: " & Param &
                   " can not contain expression, ignored");
         else
            Restriction_Setting (R_Id).Active := Enable;
         end if;

      elsif R_Id /= Not_A_Restriction_Id then

         if not Arg_Present then
            Error ("RESTRICTIONS rule parameter: " & Param &
                    " should contain an expression, ignored");
            return;
         else
            if R_Id in Integer_Parameter_Restrictions then

               begin
                  R_Val :=
                    Option_Parameter'Value
                      (Trim (Param (Last_Idx .. Param'Last), Both));

                  if Restriction_Setting (R_Id).Param /= null
                    and then
                     Gnatcheck.Options.Check_Param_Redefinition
                  then
                     Free (Restriction_Setting (R_Id).Param);
                     Last_Idx := Index (Param, "=", Backward) - 1;

                     for J in reverse First_Idx .. Last_Idx loop
                        if Param (J) /= ' ' then
                           Last_Idx := J;
                           exit;
                        end if;
                     end loop;

                     Error ("expression for RESTRICTIONS rule parameter: " &
                            Param (First_Idx .. Last_Idx) &
                            " is specified more than once");
                  end if;

                  Restriction_Setting (R_Id).Param  :=
                    new String_List'(1 => new String'(R_Val'Img));
               exception
                  when Constraint_Error =>
                     Error ("wrong restriction parameter expression in " &
                             Param & ", ignored");
                  return;
               end;
            else
               --  No check is made for the moment for non-integer restriction
               --  parameters:
                  if Restriction_Setting (R_Id).Param = null then
                     Restriction_Setting (R_Id).Param  :=
                       new String_List'(1 => new String'
                         (Trim (Param (Last_Idx .. Param'Last), Both)));
                  else
                     declare
                        Tmp : constant String_List :=
                          Restriction_Setting (R_Id).Param.all &
                          new String'(Trim
                            (Param (Last_Idx .. Param'Last), Both));
                     begin
                        Restriction_Setting (R_Id).Param :=
                          new String_List'(Tmp);
                     end;
                  end if;
            end if;

         end if;

         Restriction_Setting (R_Id).Active := Enable;

      else
         --  If we are here, R_Id = Not_A_Restriction_Id, therefore
         --  Special_R_Id /= Not_A_Special_Restriction_Id

         case Special_R_Id is
            when No_Dependence =>

               if not Arg_Present then
                  Error ("RESTRICTIONS rule parameter: " & Param &
                          " should contain an unit name, ignored");
                  return;
               end if;

               Special_Restriction_Setting (Special_R_Id) := True;
               Forbidden_Units_Dictionary.Add_To_Dictionary
                 (Trim (Param (Last_Idx .. Param'Last), Both));

            when Not_A_Special_Restriction_Id =>
               null;
               pragma Assert (False);
         end case;
      end if;

   end Process_Restriction_Param;

   -------------------------------
   -- Process_Style_Check_Param --
   -------------------------------

   procedure Process_Style_Check_Param (Param  : String) is
   begin

      if To_Lower (Param) = "all_checks" then
         Process_Style_Options ("y");
      else
         Process_Style_Options (Param);
      end if;

   end Process_Style_Check_Param;

   ---------------------------
   -- Process_Style_Options --
   ---------------------------

   procedure Process_Style_Options (Param : String) is
   begin

      if Style_Options_String = null then
         Style_Options_String := new String'(Param);
      else
         Tmp_Options := new String'(Style_Options_String.all);
         Free (Style_Options_String);
         Style_Options_String := new String'(Tmp_Options.all & Param);
         Free (Tmp_Options);
      end if;

   end Process_Style_Options;

   ---------------------------
   -- Process_Warning_Param --
   ---------------------------

   procedure Process_Warning_Param (Param  : String) is
   begin

      if Warning_Options_String = null then
         Warning_Options_String := new String'(Param);
      else
         --  Checking for 'e' and 's' that should not be supplied for gnatcheck
         --  Warnings rule.
         for J in Param'Range loop
            if Param (J) in 'e' | 's'
              and then
               (J = Param'First
               or else
                Param (J - 1) /= '.')
            then
               Error ("Warnings rule cannot have " & Param (J) &
                      " parameter, parameter string " & Param & " ignored");
               return;
            end if;
         end loop;

         Tmp_Options := new String'(Warning_Options_String.all);
         Free (Warning_Options_String);
         Warning_Options_String := new String'(Tmp_Options.all & Param);
         Free (Tmp_Options);
      end if;

   end Process_Warning_Param;

   --------------------------------
   -- Restriction_Rule_parameter --
   ---------------------------------

   function Restriction_Rule_Parameter (Diag : String) return String is
      R_Name_Start :          Natural;
      R_Name_End   :          Natural;
      Par_End      :          Natural;
      Arr_Idx      :          Natural;
      Diag_End     : constant Positive := Diag'Last;
      R_Id          :          Restriction_Id         := Not_A_Restriction_Id;
      Special_R_Id  :          Special_Restriction_Id :=
        Not_A_Special_Restriction_Id;
   begin
      R_Name_Start := Index (Diag, "of restriction ");
      pragma Assert (R_Name_Start /= 0);

      R_Name_Start := R_Name_Start + 16;

      Arr_Idx := Index (Diag (R_Name_Start .. Diag_End), "=>");

      if Arr_Idx /= 0 then
         R_Name_End := Arr_Idx - 1;

         while R_Name_End > R_Name_Start loop
            exit when Diag (R_Name_End) /= ' ';
            R_Name_End := R_Name_End - 1;
         end loop;
      else
         R_Name_End := Index (Diag (R_Name_Start .. Diag_End), """") - 1;
      end if;

      begin
         R_Id := Restriction_Id'Value (Diag (R_Name_Start .. R_Name_End));
      exception
         when Constraint_Error =>
            R_Id := Not_A_Restriction_Id;
      end;

      if R_Id = Not_A_Restriction_Id then

         begin
            Special_R_Id :=
              Special_Restriction_Id'Value
                (Diag (R_Name_Start .. R_Name_End));
         exception
            when Constraint_Error =>
               Special_R_Id := Not_A_Special_Restriction_Id;
         end;

      end if;

      if Arr_Idx /= 0
        and then
         Needs_Parameter_In_Exemption (R_Id, Special_R_Id)
      then
         Par_End := Index (Diag (R_Name_Start .. Diag_End), """") - 1;

         return To_Lower (Remove_Spaces (Diag (R_Name_Start .. Par_End)));
      else
         return To_Lower (Diag (R_Name_Start .. R_Name_End));
      end if;

   end Restriction_Rule_Parameter;

   -------------------------
   -- Set_Compiler_Checks --
   -------------------------

   procedure Set_Compiler_Checks is
   begin

      Use_gnaty_Option := Style_Options_String /= null;
      Use_gnatw_Option := Warning_Options_String /= null;

      --  Check_Restrictions

      for J in Restriction_Setting'Range loop

         if Restriction_Setting (J).Active then
            Check_Restrictions := True;
            exit;
         end if;

      end loop;

      if not Check_Restrictions then

         for J in Special_Restriction_Setting'Range loop

            if Special_Restriction_Setting (J) then
               Check_Restrictions := True;
               exit;
            end if;

         end loop;

      end if;

   end Set_Compiler_Checks;

   ----------------------------
   -- Warning_Rule_Parameter --
   ----------------------------

   function Warning_Rule_Parameter (Diag : String) return String is
      First_Idx, Last_Idx :          Natural;
      String_To_Search    : constant String := "[Warnings:";
   begin
      --  This function returns non-empty result only if .d parameter is
      --  specified for Warnings rule or is --show-rule gnatcheck option is
      --  set (that is, if Diag ends with "[Warnings:<option>]"

      First_Idx := Index (Diag, String_To_Search);

      if First_Idx = 0 then
         return "";
      else
         First_Idx := First_Idx + String_To_Search'Length;
         Last_Idx  := (if Diag (First_Idx) = '.' then
                          First_Idx + 1
                       else
                          First_Idx);
      end if;

      return Diag (First_Idx .. Last_Idx);

   end Warning_Rule_Parameter;

   -----------------------------------
   -- XML_Print_Active_Restrictions --
   -----------------------------------

   procedure XML_Print_Active_Restrictions (Indent_Level : Natural := 0) is
   begin
      XML_Report ("<rule id=""Restrictions"">", Indent_Level);

      for R in Restriction_Setting'Range loop

         if Restriction_Setting (R).Active then
            if R in All_Boolean_Restrictions then
               XML_Report
                 ("<parameter>" & Proper_Case (R'Img) & "</parameter>",
                  Indent_Level + 1);
            else
               for J in Restriction_Setting (R).Param'Range loop
                  XML_Report
                    ("<parameter>" & Proper_Case (R'Img) &
                     "=>"  & Restriction_Setting (R).Param (J).all &
                     "</parameter>",
                     Indent_Level + 1);
               end loop;
            end if;
         end if;

      end loop;

      for R in Special_Restriction_Setting'Range loop

         if Special_Restriction_Setting (R) then

            case R is
               when No_Dependence =>
                  Forbidden_Units_Dictionary.Reset_Iterator;

                  while not Forbidden_Units_Dictionary.Done loop
                     XML_Report
                       ("<parameter>No_Dependence=>" &
                          Forbidden_Units_Dictionary.Next_Entry &
                          "</parameter>",
                        Indent_Level + 1);
                  end loop;

            end case;

         end if;

      end loop;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Active_Restrictions;

end Gnatcheck.Compiler;
