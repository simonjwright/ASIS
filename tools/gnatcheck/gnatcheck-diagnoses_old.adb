------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              G N A T C H E C K . D I A G N O S E S _ O L D               --
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

with Ada.Calendar;               use Ada.Calendar;
with Ada.Command_Line;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Text_IO;                use Ada.Text_IO;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Table;

with Atree;                      use Atree;
with Gnatvsn;                    use Gnatvsn;
with Sinput;                     use Sinput;
with Types;

with Asis.Set_Get;               use Asis.Set_Get;

with ASIS_UL.Common;             use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;
with ASIS_UL.Misc;               use ASIS_UL.Misc;
with ASIS_UL.Options;            use ASIS_UL.Options;
with ASIS_UL.Output;             use ASIS_UL.Output;

with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Rules.Output;     use Gnatcheck.Rules.Output;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;

package body Gnatcheck.Diagnoses_Old is

   --  Because of historical reasons, we have two different formats of the
   --  report file and the corresponding mechanisms. One (referenced below as
   --  old format) is rather verbose and can report rule violations ordered
   --  in different ways. This format contains neither compiler error messages
   --  for non-compilable files nor warnings from rule exemption mechanism.
   --
   --  Another format (referenced below as new format or gnatcheck
   --  qualification report) is more laconic and contains both compiler error
   --  messages and rule exemption warnings.

   -----------------------------------
   -- Old format of the report file --
   -----------------------------------

   All_Section_On : Boolean := True;
   --  This flag is set of by the first '-s{1|2|3} option together with setting
   --  OFF all the flags responsible for outputting a specific report file
   --  option

   ---------------------
   -- Diagnoses Table --
   ---------------------

   type Diag_Id is new Natural;
   No_Diag    : constant Diag_Id := Diag_Id'First;
   First_Diag : constant Diag_Id := No_Diag + 1;

   function No      (D : Diag_Id) return Boolean;
   function Present (D : Diag_Id) return Boolean;
   --  Check whether or not the argument is the Id of some existing diagnosis

   type Diagnosis_Record is record
      Rule          : Rule_Id;
      SF            : SF_Id;
      Diagnosis_Num : Diagnosis_Variant;

      Line          : Types.Physical_Line_Number;
      Col           : Types.Column_Number;
      --  We use GNAT types for line and column numbers

      Next_Diag           : Diag_Id;
      Prev_Diag           : Diag_Id;
      Next_Same_Rule_Diag : Diag_Id;
      Prev_Same_Rule_Diag : Diag_Id;
      --  For each source file, diagnostic messages are chained twice. First,
      --  we have the chain of all the rule violations detected for the current
      --  source, ordered by increasing the Sloc of the violation, and second,
      --  for each rule we have a chain of the violations of this particular
      --  rule, also ordered by increasing the Slocs

      Diag_Text : String_Loc;
      --  For the "rules" corresponding to the checks performed by the compiler
      --  (that is, general warnings, style warnings and restriction warnings)
      --  we do not have any diagnosis stored as a part of the corresponding
      --  rule, so we have to store the diagnostic messages as it is extracted
      --  from the compiler-generated information. This field is used to store
      --  such a diagnostic message

      Exempt_Justification : String_Loc;
      --  If set not to Nil_String_Loc, denotes the diagnoses for exempted rule
      --  and represents the justification of exemption.

      SLOC : String_Loc;
      --  We use the Line and Col fields to order diagnoses, and we use the
      --  SLOC field to put the full GNAT location in the report

   end record;

   package Rule_Violations is new GNAT.Table (
     Table_Component_Type => Diagnosis_Record,
     Table_Index_Type     => Diag_Id,
     Table_Low_Bound      => First_Diag,
     Table_Initial        => 10000,
     Table_Increment      => 100,
     Table_Name           => "diagnoses database");

   Diag_Table : Rule_Violations.Table_Ptr renames Rule_Violations.Table;

   ----------------------------------------
   --  File-Rule-Diagnosis Mapping Table --
   ----------------------------------------

   --  This table for each rule and for each source contains the links to the
   --  first and to the last detected diagnosis corresponding to the violation
   --  of the given rule in the given source. It also contains as its last row
   --  links to the first and to the latest detected violation of (any) rule
   --  detected in the given source.

   type Detected_Violations is record
      First : Diag_Id;
      Last  : Diag_Id;
   end record;
   --  An elementary entry in the Mapping Table. Contains the links to the
   --  first and to the last diagnosis for the given rule in the given file.

   No_Violation : constant Detected_Violations := (No_Diag, No_Diag);

   type File_Mapping_Type is array (Rule_Id range <>) of Detected_Violations;
   --  The diagnosis mapping for a single file

   type File_Mapping_Type_Access is access File_Mapping_Type;
   --  We do not know how many rules we will have, so we have to use dynamic
   --  mappings

   package Mapping_Table_Package is  new GNAT.Table (
     Table_Component_Type => File_Mapping_Type_Access,
     Table_Index_Type     => SF_Id,
     Table_Low_Bound      => First_SF_Id,
     Table_Initial        => 10000,
     Table_Increment      => 100,
     Table_Name           => "diagnoses mapping database");
   --  The Mapping Table. We have to use dynamic table here, because we do not
   --  know in advance how many needed sources may be added during the rule
   --  checking

   Mapping_Table : Mapping_Table_Package.Table_Ptr renames
     Mapping_Table_Package.Table;

   All_Diags : Rule_Id;
   --  The index of the last row in the table that contains the beginning and
   --  the end of all the diagnosis for the given file;

   Warning_Diags     : Rule_Id;
   Style_Diags       : Rule_Id;
   Restriction_Diags : Rule_Id;
   --  Rows to store compiler warnings, they immediately precede All_Diags

   function First_Diagnosis
     (SF            : SF_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id;

   function First_Diagnosis (SF : SF_Id) return Diag_Id;

   function First_Rule_Diagnosis
     (SF            : SF_Id;
      R             : Rule_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id;

   function First_Rule_Diagnosis
     (SF     : SF_Id;
      R      : Rule_Id)
      return   Diag_Id;

   function Last_Diagnosis
     (SF            : SF_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id;

   function Last_Diagnosis (SF : SF_Id) return Diag_Id;

   function Last_Rule_Diagnosis
     (SF            : SF_Id;
      R             : Rule_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id;
   pragma Unreferenced (Last_Rule_Diagnosis);

   function Last_Rule_Diagnosis
     (SF     : SF_Id;
      R      : Rule_Id)
      return   Diag_Id;

   function Next_Diagnosis
     (D             : Diag_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id;

   function Next_Same_Rule_Diagnosis
     (D             : Diag_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id;

   --  Returns the first (last, next) diagnostic stored for the given source
   --  (and for the given rule). If Exempted_Rule is ON, only diagnoses for
   --  exempted rules are considered, otherwise only diagnoses for non-exempted
   --  rules are considered. If a function does not have Exempted_Rule
   --  parameter, the exemption state of the rule is not taken into account.

   function Is_For_Exempted_Rule (D : Diag_Id) return Boolean;
   --  Tells if the argument diagnosis has been generated for exempted rule

   procedure Set_First_Diagnosis      (SF : SF_Id; D : Diag_Id);
   procedure Set_First_Rule_Diagnosis (SF : SF_Id; R : Rule_Id; D : Diag_Id);
   procedure Set_Last_Diagnosis       (SF : SF_Id; D : Diag_Id);
   procedure Set_Last_Rule_Diagnosis  (SF : SF_Id; R : Rule_Id; D : Diag_Id);
   --  Sets the first (last) diagnostic stored for the given source (and for
   --  the given rule)

   procedure Store_Rule_Violation_Internal
     (For_Rule      : Rule_Id;
      Line_Num      : Types.Physical_Line_Number;
      Col_Num       : Types.Column_Number;
      In_SF         : SF_Id;
      Justification : String_Loc;
      Diagnosis_Num : Diagnosis_Variant := 0;
      Diag_Text     : String_Loc        := Nil_String_Loc;
      Element_SLOC  : String_Loc        := Nil_String_Loc);
   --  This routine does the actual job for storing the diagnosis into
   --  diagnosis table. It needs to know only the rule, the file, the line and
   --  column to associate the diagnosis with and the diagnostic variant.
   --  This procedure also checks compiler messages for duplications and it
   --  does not store duplicated diagnoses.

   -----------------------
   -- Local subprograms --
   -----------------------

   --  The following subprograms are used to generate GNATCHECK report.
   --  (Should all of these routines be defined here?)

   procedure Print_Report_Header;
   --  Generates the report header, including the date, tool version and
   --  tool command liner invocation sequence.

   procedure Print_Active_Rules;
   --  Generates the list of rules used (set as active) for the given
   --  gnatcheck run.

   --  Prints the reference to the (actual argument or artificially created)
   --  file that contains the list of all the rules that are active for the
   --  given gnatcheck run

   --  Prints the reference to the (actual argument or artificially created)
   --  file that contains the list of all the files passed to gnatcheck

   procedure Print_Disabled_Rules;
   --  Generate the list of rules currently defined in gnatcheck, but not used
   --  (set as disabled) for the given gnatcheck run.

   procedure Print_Source_List;
   --  Prints list of sources set as the arguments for the given gnatcheck run.

   procedure Print_Failure_Info;
   --  Prints info about non-fatal failures detected during gnatcheck run.

   procedure Print_Sections_Summary;
   --  Gives a short summary of the sections contained in the report.

   procedure Print_Exempted_Rules_Header;
   --  Generates the header of the section containing diagnoses for exempted
   --  rules.

   procedure Print_Section_1 (Exempted_Rules : Boolean := False);
   --  Print out the list of rule violations in compiler-like style (grouped
   --  by files with no specific order, and for each file diagnosis are
   --  ordered by the source locations of the wrong constructs). If
   --  Exempted_Rules is OFF, only diagnoses of non-exempted rules are printed,
   --  and the other way around. This procedure duplicates diagnoses into
   --  Stderr.

   procedure Print_Section_2 (Exempted_Rules : Boolean := False);
   --  Prints out the list of violations ordered by rules. For each rule
   --  violations detected in the same file are grouped together and are
   --  ordered  by the source locations of the wrong constructs.  If
   --  Exempted_Rules is OFF, only diagnoses of non-exempted rules are printed,
   --  and the other way around.

   procedure Print_Section_3 (Exempted_Rules : Boolean := False);
   --  Prints the list of violations where diagnoses are first ordered by
   --  rules, and for each file - by rules.  If Exempted_Rules is OFF, only
   --  diagnoses of non-exempted rules are printed, and the other way around.

   procedure Compute_Alignment
     (For_File       : SF_Id;
      Exempted_Rules : Boolean;
      Line_Pos       : out Natural;
      Col_Pos        : out Natural);
   --  For the diagnostic messages generated for  For_File computes the max
   --  number of positions for line and column number (the caller is
   --  responsible for the fact that diagnosis chain for For_File is not
   --  empty).  Exempted_Rules is ON, alignment is computed for diagnoses
   --  generated for exempted rules, otherwise - for non-exempted rules.

   function Line_Col
     (D        : Diag_Id;
      Line_Pos : Natural := 0;
      Col_Pos  : Natural := 0)
      return     String;
   --  For the given diagnosis, returns the location in the form
   --  "line_number:coulumn_number". If Line_Pos and Col_Pos are not equal to
   --  zero, uses exactly Line_Pos positions for line_number and exactly
   --  Col_Pos positions for coulumn_number, raises Constraint_Error if the
   --  room is not sufficient

   function Line (D : Diag_Id) return Types.Physical_Line_Number;
   function Col (D : Diag_Id)  return Types.Column_Number;
   --  For the given diagnosis, return the line and column numbers

   function Text_Diag (D : Diag_Id) return String;
   --  Returns the text of the corresponding diagnosis message. This includes
   --  resolving the diagnosis variants, if needed

   function Strip_Column (SLOC : String) return String;
   --  Remove from the standard SLOC string the column number if
   --  No_Column_Num_In_Diagnoses is ON. returns the (unchanged) argument
   --  otherwise.

   -----------------------------------------
   -- Routines used to compose the report --
   -----------------------------------------

   procedure Print_Gnatcheck_Command_Line;
   --  Prints the gnatcheck command line. In case if gnatcheck has been
   --  called from the GNAT driver, prints the call to the GNAT driver, but not
   --  the gnatcheck call generated by the GNAT driver.

   procedure Print_Runtime;
   --  Prints the runtime version used for gnatcheck call. It is either the
   --  parameter of --RTS option used for (actual) gnatcheck call or the
   --  "<default>" string if --RTS parameter is not specified.

   --  Prints the total numbers of: all the argument files, non-compilable
   --  files, files with no violations, files with violations, files with
   --  exempted violations only.

   --  Checks if all the argument sources are listed in a single user-provided
   --  file. In case if gnatcheck is called from the GNAT driver analyses
   --  the original call sequence for the GNAT driver.

   --  Computes the number of violations and diagnoses of different kinds.
   --  Results are stored in Error_Statistics global variable.

   --  Prints the total number of detected (non-exempted) violations and
   --  total number of exempted violations

   function Text_Justification (D : Diag_Id) return String;
   --  Returns the text of exemption justification

   -------------------------------
   -- Add_Line_To_Mapping_Table --
   -------------------------------

   procedure Add_Line_To_Mapping_Table is
   begin
      Mapping_Table_Package.Append
        (new File_Mapping_Type'(All_Rules.First .. All_Diags =>
                                No_Violation));
   end Add_Line_To_Mapping_Table;

   ---------
   -- Col --
   ---------

   function Col (D : Diag_Id)  return Types.Column_Number is
   begin
      return Diag_Table (D).Col;
   end Col;

   ------------------------
   --  Compute_Alignment --
   ------------------------

   procedure Compute_Alignment
     (For_File       : SF_Id;
      Exempted_Rules : Boolean;
      Line_Pos       : out Natural;
      Col_Pos        : out Natural)
   is
      Max_Pos     : Natural;
      Max_Col_Num : Types.Column_Number := 1;
      Next_Diag   : Diag_Id;
   begin
      --  Max line positions
      Max_Pos  := Positive (Line (Last_Diagnosis (For_File, Exempted_Rules)));
      Line_Pos := 0;

      while Max_Pos > 0 loop
         Line_Pos := Line_Pos + 1;
         Max_Pos  := Max_Pos / 10;
      end loop;

      --  Max column positions

      Next_Diag := First_Diagnosis (For_File, Exempted_Rules);

      while Present (Next_Diag) loop

         if Types.">" (Col (Next_Diag), Max_Col_Num) then
            Max_Col_Num :=  Col (Next_Diag);
         end if;

         Next_Diag := Next_Diagnosis (Next_Diag, Exempted_Rules);
      end loop;

      Max_Pos := Positive (Max_Col_Num);
      Col_Pos := 0;

      while Max_Pos > 0 loop
         Col_Pos := Col_Pos + 1;
         Max_Pos := Max_Pos / 10;
      end loop;

   end Compute_Alignment;

   --------------------------
   -- Create_Mapping_Table --
   --------------------------

   procedure Create_Mapping_Table is
      Rule_Number : constant Rule_Id := All_Rules.Last + 1 + 3;
      --  "+1" - for the total number of violations
      --  "+3" - for general warnings, style warnings and restriction warnings
   begin
      Mapping_Table_Package.Set_Last (Last_Argument_Source);

      for J in First_SF_Id .. Last_Argument_Source loop
         Mapping_Table (J) :=
           new File_Mapping_Type'(All_Rules.First .. Rule_Number =>
               No_Violation);
      end loop;

      All_Diags := Rule_Number;

      Warning_Diags     := All_Diags - 3;
      Style_Diags       := All_Diags - 2;
      Restriction_Diags := All_Diags - 1;

   end Create_Mapping_Table;

   ----------------------------------------------------
   -- Diag_Structure_Debug_Image (old report format) --
   ----------------------------------------------------

   procedure Diag_Structure_Debug_Image is
      Ident_String : constant String := "   ";
      procedure Print_Diag_Node (N : Diag_Id);
      --  Prints out one diag node

      procedure Pring_File_Mapping (SF : SF_Id);
      --  Prints out mapping for the argument file

      procedure Print_Diag_Node (N : Diag_Id) is
         Rule : constant Rule_Id := Diag_Table (N).Rule;
      begin
         Put_Line ("Diag_Id =" & N'Img);
         Put (Ident_String);
         Put ("Rule = ");

         if Rule = Warning_Diags then
            Put_Line ("Compiler warnings");
         elsif Rule = Style_Diags then
            Put_Line ("Compiler style checks");
         elsif Rule = Restriction_Diags then
            Put_Line ("Compiler restrictions");
         else
            Put_Line (Rule_Name (All_Rules.Table (Rule).all));
         end if;

         Put (Ident_String);
         Put_Line ("SF =  " & Diag_Table (N).SF'Img);

         Put (Ident_String);
         Put_Line ("Line =" & Diag_Table (N).Line'Img &
                  " Col=" & Diag_Table (N).Col'Img);

         Put (Ident_String);
         Put_Line ("Next_Diag =           " & Diag_Table (N).Next_Diag'Img);

         Put (Ident_String);
         Put_Line ("Prev_Diag =           " & Diag_Table (N).Prev_Diag'Img);

         Put (Ident_String);
         Put_Line ("Next_Same_Rule_Diag = " &
                   Diag_Table (N).Next_Same_Rule_Diag'Img);

         Put (Ident_String);
         Put_Line ("Prev_Same_Rule_Diag = " &
                   Diag_Table (N).Prev_Same_Rule_Diag'Img);

         if Is_For_Exempted_Rule (N) then
            Put (Ident_String);
            Put_Line ("Is exempted");
         end if;

      end Print_Diag_Node;

      procedure Pring_File_Mapping (SF : SF_Id) is
         procedure Print_Rule_Mapping (R : Rule_Id; SF : SF_Id);
         --  Prints out the concrete diagnosis mapping

         procedure Print_Rule_Mapping (R : Rule_Id; SF : SF_Id) is
         begin

            Put (Ident_String);

            if R <= All_Rules.Last then
               Put_Line ("Rule = " &
                         Rule_Name (All_Rules.Table (R).all));
            else
               Put_Line ("All Rules");
            end if;

            Put (Ident_String);
            Put (Ident_String);
            Put_Line ("First =" & Mapping_Table (SF) (R) .First'Img &
                      " Last =" & Mapping_Table (SF) (R).Last'Img);

         end Print_Rule_Mapping;
      begin
         Put_Line ("Mapping for file " & SF'Img);

         for J in Mapping_Table (SF)'Range loop
            Print_Rule_Mapping (J, SF);
         end loop;

      end Pring_File_Mapping;

   begin
      Put_Line ("**** Diag_Table start *****");

      for J in First_Diag .. Rule_Violations.Last loop
         Print_Diag_Node (J);
      end loop;

      Put_Line ("**** Diag_Table end *****");

      Put_Line ("**** Mappint_Table start *****");

      for J in First_SF_Id .. Mapping_Table_Package.Last loop
         Pring_File_Mapping (J);
      end loop;

      Put_Line ("**** Mapping_Table end *****");

   end Diag_Structure_Debug_Image;

   ---------------------
   -- First_Diagnosis --
   ---------------------

   function First_Diagnosis
     (SF            : SF_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id
   is
      Result : Diag_Id := Mapping_Table (SF) (All_Diags).First;
   begin

      while Present (Result)
        and then
            Is_For_Exempted_Rule (Result) /= Exempted_Rule
      loop
         Result := Diag_Table (Result).Next_Diag;
      end loop;

      return Result;
   end First_Diagnosis;

   function First_Diagnosis (SF : SF_Id) return Diag_Id is
   begin
      return Mapping_Table (SF) (All_Diags).First;
   end First_Diagnosis;

   --------------------------
   -- First_Rule_Diagnosis --
   --------------------------

   function First_Rule_Diagnosis
     (SF            : SF_Id;
      R             : Rule_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id
   is
      Result : Diag_Id := Mapping_Table (SF) (R).First;
   begin

      while Present (Result)
        and then
            Is_For_Exempted_Rule (Result) /= Exempted_Rule
      loop
         Result := Diag_Table (Result).Next_Same_Rule_Diag;
      end loop;

      return Result;
   end First_Rule_Diagnosis;

   function First_Rule_Diagnosis
     (SF     : SF_Id;
      R      : Rule_Id)
      return   Diag_Id
   is
   begin
      return  Mapping_Table (SF) (R).First;
   end First_Rule_Diagnosis;

   -----------------------------
   -- Generate_Regular_Report --
   -----------------------------

   procedure Generate_Regular_Report is
   begin

      if not Short_Report then
         Print_Report_Header;
         Print_Active_Rules;
         Print_Disabled_Rules;
         Print_Source_List;
         Print_Failure_Info;
         Print_Sections_Summary;
      end if;

      if Output_Section_1
        or else
         not Quiet_Mode
      then
         Print_Section_1;
      end if;

      if Output_Section_2 then
         Print_Section_2;
      end if;

      if Output_Section_3 then
         Print_Section_3;
      end if;

      if Print_Exemption_Section then
         Print_Exempted_Rules_Header;

         if Output_Section_1 then
            Print_Section_1 (Exempted_Rules => True);
         end if;

         if Output_Section_2 then
            Print_Section_2 (Exempted_Rules => True);
         end if;

         if Output_Section_3 then
            Print_Section_3 (Exempted_Rules => True);
         end if;
      end if;
   end Generate_Regular_Report;

   --------------------------
   -- Is_For_Exempted_Rule --
   --------------------------

   function Is_For_Exempted_Rule (D : Diag_Id) return Boolean is
   begin
      return Diag_Table (D).Exempt_Justification /= Nil_String_Loc;
   end Is_For_Exempted_Rule;

   --------------------
   -- Last_Diagnosis --
   --------------------

   function Last_Diagnosis
     (SF            : SF_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id
   is
      Result : Diag_Id := Mapping_Table (SF) (All_Diags).Last;
   begin

      while Present (Result)
        and then
            Is_For_Exempted_Rule (Result) /= Exempted_Rule
      loop
         Result := Diag_Table (Result).Prev_Diag;
      end loop;

      return Result;
   end Last_Diagnosis;

   function Last_Diagnosis (SF : SF_Id) return Diag_Id is
   begin
      return Mapping_Table (SF) (All_Diags).Last;
   end Last_Diagnosis;

   -------------------------
   -- Last_Rule_Diagnosis --
   -------------------------

   function Last_Rule_Diagnosis
     (SF            : SF_Id;
      R             : Rule_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id
   is
      Result : Diag_Id := Mapping_Table (SF) (R).Last;
   begin

      while Present (Result)
        and then
            Is_For_Exempted_Rule (Result) /= Exempted_Rule
      loop
         Result := Diag_Table (Result).Prev_Same_Rule_Diag;
      end loop;

      return Result;
   end Last_Rule_Diagnosis;

   function Last_Rule_Diagnosis
     (SF     : SF_Id;
      R      : Rule_Id)
      return   Diag_Id
   is
   begin
      return Mapping_Table (SF) (R).Last;
   end Last_Rule_Diagnosis;

   --------------
   -- Line_Col --
   --------------

   function Line_Col
     (D        : Diag_Id;
      Line_Pos : Natural := 0;
      Col_Pos  : Natural := 0)
      return     String
   is
      Line_Str : constant String := Trim (Diag_Table (D).Line'Img, Left);
      Col_Str  : constant String := Trim (Diag_Table (D).Col'Img, Left);
   begin
      return
        (1 .. Line_Pos - Line_Str'Length => ' ') &
        Line_Str                                 &
        ':'                                      &
        (1 .. Col_Pos - Col_Str'Length => ' ')   &
        Col_Str;
   end Line_Col;

   ----------
   -- Line --
   ----------

   function Line (D : Diag_Id) return Types.Physical_Line_Number is
   begin
      return Diag_Table (D).Line;
   end Line;

   --------------------
   -- Next_Diagnosis --
   --------------------

   function Next_Diagnosis
     (D             : Diag_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id
   is
      Result : Diag_Id := Diag_Table (D).Next_Diag;
   begin

      while Present (Result)
        and then
            Is_For_Exempted_Rule (Result) /= Exempted_Rule
      loop
         Result := Diag_Table (Result).Next_Diag;
      end loop;

      return Result;
   end Next_Diagnosis;

   ------------------------------
   -- Next_Same_Rule_Diagnosis --
   ------------------------------

   function Next_Same_Rule_Diagnosis
     (D             : Diag_Id;
      Exempted_Rule : Boolean)
      return          Diag_Id
   is
      Result : Diag_Id := Diag_Table (D).Next_Same_Rule_Diag;
   begin

      while Present (Result)
        and then
            Is_For_Exempted_Rule (Result) /= Exempted_Rule
      loop
         Result := Diag_Table (Result).Next_Same_Rule_Diag;
      end loop;

      return Result;
   end Next_Same_Rule_Diagnosis;

   --------
   -- No --
   --------

   function No (D : Diag_Id) return Boolean is
   begin
      return D not in First_Diag .. Rule_Violations.Last;
   end No;

   -------------
   -- Present --
   -------------

   function Present (D : Diag_Id) return Boolean is
   begin
      return D in First_Diag .. Rule_Violations.Last;
   end Present;

   ------------------------
   -- Print_Active_Rules --
   ------------------------

   procedure Print_Active_Rules is
   begin
      Report ("coding standard (applied rules):");

      for Rule in All_Rules.First .. All_Rules.Last loop

         if All_Rules.Table (Rule).Diagnosis /= null
          and then
            Is_Enable (All_Rules.Table (Rule).all)
         then
            --  Note, that if a rule does not have its own diagnoses, this
            --  means that it is implemented by some other rules, so it
            --  should not go into the report

            Print_Rule (All_Rules.Table (Rule).all, 1);
            Report_EOL;
         end if;
      end loop;

      Report_EOL;

      --  Compiler-made checks:

      if Use_gnaty_Option then
         Report_No_EOL ("Compiler style checks: ", 1);
         Report (Get_Style_Option);
         Report_EOL;
      end if;

      if Use_gnatw_Option then
         Report_No_EOL ("Compiler warnings: ", 1);
         Report (Get_Specified_Warning_Option);
         Report_EOL;
      end if;

      if Check_Restrictions then
         Report ("Compiler-checked restrictions:", 1);
         Print_Active_Restrictions (2);
         Report_EOL;
      end if;

   end Print_Active_Rules;

   --------------------------
   -- Print_Disabled_Rules --
   --------------------------

   procedure Print_Disabled_Rules is
   begin
      Report ("Disabled rules:");

      for Rule in All_Rules.First .. All_Rules.Last loop

         if All_Rules.Table (Rule).Diagnosis /= null
          and then
            not Is_Enable (All_Rules.Table (Rule).all)
         then
            --  Note, that if a rule does not have its own diagnoses, this
            --  means that it is implemented by some other rules, so it
            --  should not go into the report
            Report_No_EOL
              ("(" & Rule_Name (All_Rules.Table (Rule).all) & ") ", 1);
            Report (All_Rules.Table (Rule).Help_Info.all);
         end if;
      end loop;

      Report_EOL;

      --  Compiler-made checks:

      if not Use_gnaty_Option then
         Report ("No active compiler style check", 1);
      end if;

      if not Use_gnatw_Option then
         Report ("No active compiler warning:", 1);
      end if;

      if not Check_Restrictions then
         Report ("No active compiler-checked restriction", 1);
      end if;

      Report_EOL;

   end Print_Disabled_Rules;

   ---------------------------------
   -- Print_Exempted_Rules_Header --
   ---------------------------------

   procedure Print_Exempted_Rules_Header is
   begin
      Report_EOL;
      Report ("============== Start Exempted Rules Section  ===============");
      Report ("   This section contains diagnoses for exempted rules ");
      Report_EOL;

   end Print_Exempted_Rules_Header;

   ------------------------
   -- Print_Failure_Info --
   ------------------------

   procedure Print_Failure_Info is
   begin

      if Tool_Failures > 0 then
         Report ("Total gnatcheck failures:" & Tool_Failures'Img);
         Report_EOL;
      end if;

   end Print_Failure_Info;

   ----------------------------------
   -- Print_Gnatcheck_Command_Line --
   ----------------------------------

   procedure Print_Gnatcheck_Command_Line is
      GNAT_Driver_Call : constant String_Access :=
       Getenv ("GNAT_DRIVER_COMMAND_LINE");

   begin
      if GNAT_Driver_Call /= null
       and then
         GNAT_Driver_Call.all /= ""
      then
         Report (GNAT_Driver_Call.all);
      else
         Report_No_EOL (Ada.Command_Line.Command_Name);

         for Arg in 1 .. Ada.Command_Line.Argument_Count loop
            Report_No_EOL (" " & Ada.Command_Line.Argument (Arg));
         end loop;

         Report_EOL;
      end if;

   end Print_Gnatcheck_Command_Line;

   -------------------------
   -- Print_Report_Header --
   -------------------------

   procedure Print_Report_Header is
      Time_Of_Check   : constant Time := Clock;
      Month_Of_Check  : constant Month_Number := Month (Time_Of_Check);
      Day_Of_Check    : constant Day_Number   := Day (Time_Of_Check);
      Sec_Of_Check    : constant Day_Duration := Seconds (Time_Of_Check);

      Hour_Of_Chech   :          Integer range 0 .. 23;
      Minute_Of_Check :          Integer range 0 .. 59;
      Seconds_In_Hour : constant Integer := 60 * 60;

   begin
      Report ("GNATCheck report");
      Report_EOL;

      Report_No_EOL ("date              : ");
      Report_No_EOL (Trim (Year (Time_Of_Check)'Img, Left) & '-');

      if Month_Of_Check < 10 then
         Report_No_EOL ("0");
      end if;

      Report_No_EOL (Trim (Month_Of_Check'Img, Left) & '-');

      if Day_Of_Check < 10 then
         Report_No_EOL ("0");
      end if;

      Report_No_EOL (Trim (Day_Of_Check'Img, Left) & ' ');

      Hour_Of_Chech   := Integer (Sec_Of_Check) / Seconds_In_Hour;
      Minute_Of_Check := (Integer (Sec_Of_Check) rem Seconds_In_Hour) / 60;

      if Hour_Of_Chech < 10 then
         Report_No_EOL ("0");
      end if;

      Report_No_EOL (Trim (Hour_Of_Chech'Img, Left) & ':');

      if Minute_Of_Check < 10 then
         Report_No_EOL ("0");
      end if;

      Report        (Trim (Minute_Of_Check'Img, Left));

      Report_No_EOL ("gnatcheck version : ");
      Report_No_EOL (Tool_Name.all &  ' ');
      Report        (Gnat_Version_String);

      Report_No_EOL ("command line      : ");
      Print_Gnatcheck_Command_Line;

      Report_No_EOL ("runtime           : ");
      Print_Runtime;
   end Print_Report_Header;

   -------------------
   -- Print_Runtime --
   -------------------

   procedure Print_Runtime is
   begin
      if ASIS_UL.Compiler_Options.Custom_RTS /= null then
         Report (ASIS_UL.Compiler_Options.Custom_RTS.all);
      else
         Report ("<default>");
      end if;
   end Print_Runtime;

   ---------------------
   -- Print_Section_1 --
   ---------------------

   procedure Print_Section_1 (Exempted_Rules : Boolean := False) is
      SF_Name : String_Access;
      --  Points to the name of the current source file. We use a string access
      --  value instead of using ASIS_UL.Source_Table.Short_Source_Name because
      --  of the performance reasons

      Next_Diag : Diag_Id;

      Line_Pos : Natural := 0;
      Col_Pos  : Natural := 0;
      --  Max positions for lines and columns, are  needed to align
      --  diagnoses in the report file

      Diagnoses_Reported : Natural := 0;
      --  Counts diagnoses that are printed out into Stdout
   begin

      if Output_Section_1
        and then
         not Short_Report
      then
         Report_EOL;
         Report ("-------- Start Section 1 ------------");
         Report ("   (compiler-style report, diagnoses are grouped by files,");
         Report ("    and for each file diagnoses are ordered by increasing");
         Report ("    the source location of the corresponding construct)");
         Report_EOL;
      end if;

      for SF in First_SF_Id .. Last_Source loop
         Next_Diag := First_Diagnosis (SF, Exempted_Rules);

         if Present (Next_Diag) then

            if Full_Source_Locations then
               SF_Name := new String'(Source_Name (SF));
            else
               SF_Name := new String'(Short_Source_Name (SF));
            end if;

            if Output_Section_1
              and then
               not Full_Source_Locations
              and then
               not No_Column_Num_In_Diagnoses
            then
               Compute_Alignment (SF, Exempted_Rules, Line_Pos, Col_Pos);
            end if;

            while Present (Next_Diag) loop

               if Output_Section_1 then

                  if Full_Source_Locations
                     and then Diag_Table (Next_Diag).Rule not in
                              Warning_Diags .. Restriction_Diags
                  then
                     Report_No_EOL
                       (Get_String (Diag_Table (Next_Diag).SLOC));
                  else
                     Report_No_EOL (SF_Name.all & ':');

                     if No_Column_Num_In_Diagnoses then
                        Report_No_EOL
                          (Image (Integer (Diag_Table (Next_Diag).Line)));
                     else
                        Report_No_EOL
                          (Line_Col
                            (D        => Next_Diag,
                             Line_Pos => Line_Pos,
                             Col_Pos  => Col_Pos));
                     end if;

                  end if;

                  Report_No_EOL (": " & Text_Diag (Next_Diag));

                  if Exempted_Rules then
                     pragma Assert (Is_For_Exempted_Rule (Next_Diag));

                     Report (" (" & Text_Justification (Next_Diag) & ")");
                  else
                     Report_EOL;
                  end if;

               end if;

               --  This generates the diagnostic messages into Stdout
               if not Quiet_Mode
                 and then
                  not Is_For_Exempted_Rule (Next_Diag)
                 and then
                   (Max_Diagnoses = 0
                    or else
                     Max_Diagnoses > Diagnoses_Reported)
               then

                  if Diag_Table (Next_Diag).Rule not in
                     Warning_Diags .. Restriction_Diags
                  then
                     Put_Line
                       (Strip_Column
                         (Get_String (Diag_Table (Next_Diag).SLOC)) &
                          ": " &
                          Text_Diag (Next_Diag));
                  else
                     Put (SF_Name.all & ':');
                     Put (Image (Integer (Diag_Table (Next_Diag).Line)) &
                          ':');

                     if not No_Column_Num_In_Diagnoses then
                        Put (Image (Integer (Diag_Table (Next_Diag).Col))
                             & ":");
                     end if;

                     Put_Line  (' ' & Text_Diag (Next_Diag));
                  end if;

                  Diagnoses_Reported := Diagnoses_Reported + 1;

                  if Diagnoses_Reported = Max_Diagnoses then
                     Error ("Maximum diagnoses reached, " &
                            "see the report file for full details");
                  end if;

               end if;

               Next_Diag := Next_Diagnosis (Next_Diag, Exempted_Rules);
            end loop;

            Free (SF_Name);
         end if;

      end loop;

      if not Short_Report then
         Report ("-------- End Section 1 ------------");
         Report_EOL;
      end if;

   end Print_Section_1;

   ---------------------
   -- Print_Section_2 --
   ---------------------

   procedure Print_Section_2 (Exempted_Rules : Boolean := False) is

      procedure Print_Rule_Matches (Rule : Rule_Id);
      --  Prints out matches for a given rule detected in all the files
      --  that have been processed

      procedure Print_Rule_Matches (Rule : Rule_Id) is
         SF_Name : String_Access;
         --  Points to the name of the current source file. We use a string
         --  access value instead of using
         --  ASIS_UL.Source_Table.Short_Source_Name because of the performance
         --  reasons

         Next_Diag  : Diag_Id;
         No_Matches : Boolean := True;

      begin
         for SF in First_SF_Id .. Last_Source loop

            if Full_Source_Locations then
               SF_Name := new String'(Source_Name (SF));
            else
               SF_Name := new String'(Short_Source_Name (SF));
            end if;

            Next_Diag := First_Rule_Diagnosis (SF, Rule, Exempted_Rules);

            if Present (Next_Diag) then
               Report ("Matches detected in file " & SF_Name.all, 1);

               while Present (Next_Diag) loop

                  if Full_Source_Locations then
                     Report (Get_String (Diag_Table (Next_Diag).SLOC), 2);
                  else
                     Report_No_EOL (Line_Col (Next_Diag), 2);

                     if Exempted_Rules then
                        pragma Assert (Is_For_Exempted_Rule (Next_Diag));

                        Report (" (" & Text_Justification (Next_Diag) & ")");
                     else
                        Report_EOL;
                     end if;

                  end if;

                  Next_Diag :=
                    Next_Same_Rule_Diagnosis (Next_Diag, Exempted_Rules);
               end loop;

               Report_EOL;

               No_Matches := False;

            end if;

            --  Report_EOL;
            Free (SF_Name);
         end loop;

         if No_Matches then
               Report ("No matches detected in processed files", 2);
         end if;

      end Print_Rule_Matches;

   begin
      if not Short_Report then
         Report_EOL;
         Report ("-------- Start Section 2 ------------");
         Report ("   (diagnoses are grouped by rules, and for each rule -");
         Report ("    by files, and for each file - by increasing the source");
         Report ("    location of the corresponding construct)");
         Report_EOL;
      end if;

      for Rule in All_Rules.First .. All_Rules.Last loop

         if All_Rules.Table (Rule).Rule_State /= Disabled
           and then
            All_Rules.Table (Rule).Diagnosis /= null
         then

            --  Note, that if a rule does not have its own diagnoses, this
            --  means that it is implemented by some other rules, so it
            --  should not go into the report

            --  Rule identification info, something more smart should be
            --  printed out here  ???

            Report (All_Rules.Table (Rule).Help_Info.all);
            Print_Rule_Matches (Rule);
            Report_EOL;
         end if;

      end loop;

      --  Compiler-made checks:

      if Use_gnaty_Option then
         Report ("Compiler style checks");
         Print_Rule_Matches (Style_Diags);
      end if;

      if Use_gnatw_Option then
         Report ("Compiler warnings");
         Print_Rule_Matches (Warning_Diags);
      end if;

      if Check_Restrictions then
         Report ("Compiler-checked restrictions");
         Print_Rule_Matches (Restriction_Diags);
      end if;

      if not Short_Report then
         Report ("-------- End Section 2 ------------");
         Report_EOL;
      end if;

   end Print_Section_2;

   ---------------------
   -- Print_Section_3 --
   ---------------------

   procedure Print_Section_3 (Exempted_Rules : Boolean := False) is
      SF_Name : String_Access;
      --  Points to the name of the current source file. We use a string access
      --  value instead of using ASIS_UL.Source_Table.Short_Source_Name because
      --  of the performance reasons

      Next_Diag : Diag_Id;

      procedure Print_Rule_Matches (Rule : Rule_Id; SF : SF_Id);
      --  Prints out matches for a given rule detected in the given file

      procedure Print_Rule_Matches (Rule : Rule_Id; SF : SF_Id) is
         Next_Diag : Diag_Id;
      begin
         Next_Diag := First_Rule_Diagnosis (SF, Rule, Exempted_Rules);

         if Present (Next_Diag) then

            while Present (Next_Diag) loop

               if Full_Source_Locations then
                  Report (Get_String (Diag_Table (Next_Diag).SLOC), 2);
               else
                  Report_No_EOL (Line_Col (Next_Diag), 2);

                  if Exempted_Rules then
                     pragma Assert (Is_For_Exempted_Rule (Next_Diag));

                     Report (" (" & Text_Justification (Next_Diag) & ")");
                  else
                     Report_EOL;
                  end if;

               end if;

               Next_Diag :=
                 Next_Same_Rule_Diagnosis (Next_Diag, Exempted_Rules);
            end loop;

         else
            Report ("No matches detected", 2);
         end if;

      end Print_Rule_Matches;

   begin
      if not Short_Report then
         Report_EOL;
         Report ("-------- Start Section 3 ------------");
         Report ("   (diagnoses are grouped by files, and for each file they");
         Report ("    are first grouped by rules and then - by increasing");
         Report ("    the source location of the corresponding construct)");
         Report_EOL;
      end if;

--      for SF in First_SF_Id .. Last_Argument_Source loop
      for SF in First_SF_Id .. Last_Source loop
         Next_Diag := First_Diagnosis (SF, Exempted_Rules);

         if Full_Source_Locations then
            SF_Name := new String'(Source_Name (SF));
         else
            SF_Name := new String'(Short_Source_Name (SF));
         end if;

         if Present (Next_Diag) then

            Report ("Matches detected in file " & SF_Name.all);

            for Rule in All_Rules.First .. All_Rules.Last loop

               if All_Rules.Table (Rule).Rule_State /= Disabled
                 and then
                  All_Rules.Table (Rule).Diagnosis /= null
               then

                  --  Note, that if a rule does not have its own diagnoses,
                  --  this means that it is implemented by some other rules, so
                  --  it  should not go into the report

                  --  Rule identification info, something more smart should be
                  --  printed out here  ???

                  Report (All_Rules.Table (Rule).Help_Info.all, 1);
                  Print_Rule_Matches (Rule, SF);
                  Report_EOL;
               end if;

            end loop;

            Report_EOL;

            --  Compiler-made checks:

            if Use_gnaty_Option then
               Report ("Compiler style checks");
               Print_Rule_Matches (Style_Diags, SF);
            end if;

            if Use_gnatw_Option then
               Report ("Compiler warnings");
               Print_Rule_Matches (Warning_Diags, SF);
            end if;

            if Check_Restrictions then
               Report ("Compiler-checked restrictions");
               Print_Rule_Matches (Restriction_Diags, SF);
            end if;

         elsif Source_Status (SF) = Processed then
            Report ("No matches for enabled rules detected in file " &
                     SF_Name.all);
            Report_EOL;
         end if;

         Free (SF_Name);

      end loop;

      if not Short_Report then
         Report ("-------- End Section 3 ------------");
      end if;

   end Print_Section_3;

   ----------------------------
   -- Print_Sections_Summary --
   ----------------------------

   procedure Print_Sections_Summary is
   begin
      Report ("This report contains following sections:");
      Report_EOL;

      Report_No_EOL ("Section 1 - ");

      if Output_Section_1 then
         Report ("compiler-style report, diagnoses are grouped by files and");
         Report ("            for each file ordered by line numbers");

      else
         Report ("skipped");
      end if;

      Report_No_EOL ("Section 2 - ");

      if Output_Section_2 then
         Report ("diagnoses are grouped by rules, then - by files and then -");
         Report ("            by line numbers");
      else
         Report ("skipped");
      end if;

      Report_No_EOL ("Section 3 - ");

      if Output_Section_3 then
         Report ("diagnoses are grouped by files, then - by rules and then -");
         Report ("            by line numbers");
      else
         Report ("skipped");
      end if;

   end Print_Sections_Summary;

   -----------------------
   -- Print_Source_List --
   -----------------------

   procedure Print_Source_List is
   begin
      Report_EOL;

      Report ("Checked argument sources:");

      for SF in First_SF_Id .. Last_Argument_Source loop
         Report_No_EOL (Short_Source_Name (SF), 1);

         if Source_Status (SF) = Not_A_Legal_Source then
            Report (" - illegal source, no check is made");
         else
            Report_EOL;
         end if;

      end loop;

      if ASIS_UL.Options.Buld_Call_Graph
        and then
         Last_Argument_Source < Last_Source
      then
         Report ("Additional sources analyzed to check global rules:");

         for SF in Last_Argument_Source + 1 .. Last_Source loop
            Report_No_EOL (Short_Source_Name (SF), 1);

            if Source_Status (SF) = Not_A_Legal_Source then
               Report (" - illegal source, no check is made");
            else
               Report_EOL;
            end if;

         end loop;
      end if;

      Report_EOL;

   end Print_Source_List;

   ------------------------------------------
   -- Process_Report_File_Format_Parameter --
   ------------------------------------------
   procedure Process_Report_File_Format_Parameter
     (Parameter :     String;
      Success   : out Boolean)
   is
   begin
      Success := True;

      if Parameter = "" then
         Short_Report := True;
         return;
      end if;

      if All_Section_On then
         All_Section_On  := False;
         Output_Section_1 := False;
         Output_Section_2 := False;
         Output_Section_3 := False;
      end if;

      for J in Parameter'Range loop

         case Parameter (J) is
            when '1' =>
               Output_Section_1 := True;
            when '2' =>
               Output_Section_2 := True;
            when '3' =>
               Output_Section_3 := True;
            when others =>
               Success := False;
               Error ("Wrong parameter of '-s' option: " & Parameter);
               return;
         end case;

      end loop;

   end Process_Report_File_Format_Parameter;

   ---------------------------
   -- Process_User_Filename --
   ---------------------------

   procedure Process_User_Filename (Fname : String) is
   begin

      if Is_Regular_File (Fname) then

         if User_Info_File /= null then
            Error ("--include-file option can be given only once, " &
                   "all but first ignored");
         else
            User_Info_File           := new String'(Fname);
            User_Info_File_Full_Path := new String'
              (Normalize_Pathname
                 (Fname,
                  Resolve_Links  => False,
                  Case_Sensitive => False));
         end if;

      else
         Error (Fname & " not found, --include-file option ignored");
      end if;

   end Process_User_Filename;

   -------------------------
   -- Set_First_Diagnosis --
   -------------------------

   procedure Set_First_Diagnosis (SF : SF_Id; D : Diag_Id) is
   begin
      Mapping_Table (SF) (All_Diags).First := D;
   end Set_First_Diagnosis;

   ------------------------------
   -- Set_First_Rule_Diagnosis --
   ------------------------------

   procedure Set_First_Rule_Diagnosis (SF : SF_Id; R : Rule_Id; D : Diag_Id) is
   begin
      Mapping_Table (SF) (R).First := D;
   end Set_First_Rule_Diagnosis;

   ------------------------
   -- Set_Last_Diagnosis --
   ------------------------

   procedure Set_Last_Diagnosis (SF : SF_Id; D : Diag_Id) is
   begin
      Mapping_Table (SF) (All_Diags).Last := D;
   end Set_Last_Diagnosis;

   -----------------------------
   -- Set_Last_Rule_Diagnosis --
   -----------------------------

   procedure Set_Last_Rule_Diagnosis (SF : SF_Id; R : Rule_Id; D : Diag_Id) is
   begin
      Mapping_Table (SF) (R).Last := D;
   end Set_Last_Rule_Diagnosis;

   ----------------------------
   -- Store_Compiler_Message --
   ----------------------------

   procedure Store_Compiler_Message
     (In_SF        : SF_Id;
      Line_Num     : Natural;
      Col_Num      : Natural;
      Message      : String_Loc;
      Message_Kind : Compiler_Message_Kinds)
   is
      For_Rule : Rule_Id;
      --  Artificial Rule_Id for storing the compiler message
   begin
      case Message_Kind is
         when Not_A_Compiler_Nessage =>
            pragma Assert (False);
            return;
         when General_Warning =>
            For_Rule := Warning_Diags;
         when Style =>
            For_Rule := Style_Diags;
         when Restriction =>
            For_Rule := Restriction_Diags;
      end case;

      Store_Rule_Violation_Internal
        (For_Rule      => For_Rule,
         Line_Num      => Types.Physical_Line_Number (Line_Num),
         Col_Num       => Types.Column_Number (Col_Num),
         In_SF         => In_SF,
         Justification => Nil_String_Loc, --  exemption for compiler tests is
                                          --  not implemented yet
         Diagnosis_Num => 0,   --  temporary solution
         Diag_Text     => Message,
         Element_SLOC  => Nil_String_Loc);
   end Store_Compiler_Message;

   --------------------------
   -- Store_Rule_Violation --
   --------------------------

   procedure Store_Rule_Violation
     (For_Rule : Rule_Id;
      On       : GS_Node_Id)
   is
      --  Should We keep line and column numbers as a part of the global
      --  structure node structure

      SLOC : constant String := Get_String (GS_Node_SLOC (On));

      function Get_Line_Number return Types.Physical_Line_Number;
      --  Separates the line number from SLOC and returns it as the value
      --  of Physical_Line_Number

      function Get_Column_Number return Types.Column_Number;
      function Get_Column_Number_Old return Types.Column_Number;
      pragma Unreferenced (Get_Column_Number_Old);
      --  Separates the column number from SLOC and returns it as the value
      --  of Column_Number
      --  Get_Column_Number_Old accumes the old format of the SLOC from
      --  expanded generic (with [])

      function Get_Line_Number return Types.Physical_Line_Number is
         First_Colon  : Positive;
         Second_Colon : Positive;
      begin
         First_Colon := Index (SLOC, ":");

         if First_Colon = SLOC'First + 1
           and then
            SLOC (First_Colon + 1) = '\'
         then
            First_Colon := Index (SLOC (First_Colon + 2 .. SLOC'Last), ":");
         end if;

         Second_Colon := Index (SLOC (First_Colon + 1 .. SLOC'Last), ":");

         return Types.Physical_Line_Number'Value
           (SLOC (First_Colon + 1 .. Second_Colon - 1));
      end Get_Line_Number;

      function Get_Column_Number return Types.Column_Number is
         First_Colon  : Positive;
         Second_Colon : Positive;
         Third_Colon  : Natural;
      begin
         First_Colon := Index (SLOC, ":");

         if First_Colon = SLOC'First + 1
           and then
            SLOC (First_Colon + 1) = '\'
         then
            First_Colon := Index (SLOC (First_Colon + 2 .. SLOC'Last), ":");
         end if;

         Second_Colon := Index (SLOC (First_Colon + 1 .. SLOC'Last), ":");
         Third_Colon  := Index
                           (SLOC (Second_Colon + 1 .. SLOC'Last),
                            Instance_SLOC_Txt);

         if Third_Colon = 0 then
            Third_Colon := SLOC'Last;
         else
            Third_Colon := Third_Colon - 1;
         end if;

         return Types.Column_Number'Value
           (SLOC (Second_Colon + 1 .. Third_Colon));
      end Get_Column_Number;

      function Get_Column_Number_Old return Types.Column_Number is
         First_Colon  : Positive;
         Second_Colon : Positive;
         Third_Colon  : Natural;
      begin
         First_Colon := Index (SLOC, ":");

         if First_Colon = SLOC'First + 1
           and then
            SLOC (First_Colon + 1) = '\'
         then
            First_Colon := Index (SLOC (First_Colon + 2 .. SLOC'Last), ":");
         end if;

         Second_Colon := Index (SLOC (First_Colon + 1 .. SLOC'Last), ":");
         Third_Colon  := Index (SLOC (Second_Colon + 1 .. SLOC'Last), "[");

         if Third_Colon = 0 then
            Third_Colon := SLOC'Last;
         else
            Third_Colon := Third_Colon - 1;
         end if;

         return Types.Column_Number'Value
           (SLOC (Second_Colon + 1 .. Third_Colon));
      end Get_Column_Number_Old;

      Line_Num : constant Types.Physical_Line_Number := Get_Line_Number;
      Col_Num  : constant Types.Column_Number        := Get_Column_Number;

   begin

      Store_Rule_Violation_Internal
        (For_Rule      => For_Rule,
         Line_Num      => Line_Num,
         Col_Num       => Col_Num,
         In_SF         => Enclosing_Source (On),
         Justification => Nil_String_Loc, --  exemption for global rules is
                                          --  not implemented yet
         Diagnosis_Num => 0, --  temporary solution
         Element_SLOC  => GS_Node_SLOC (On));

   end Store_Rule_Violation;

   procedure Store_Rule_Violation
     (For_Rule      : Rule_Id;
      On            : Element;
      In_SF         : SF_Id;
      Justification : String_Loc;
      Diagnosis_Num : Diagnosis_Variant := 0;
      Diag_Pars     : String_Loc;
      Element_SLOC  : String_Loc)
   is
      P        : constant Types.Source_Ptr := Sloc (Node (On));
      Line_Num : constant Types.Physical_Line_Number :=
        Get_Physical_Line_Number (P);
      Col_Num  : constant Types.Column_Number := Get_Column_Number (P);

   begin
      Store_Rule_Violation_Internal
        (For_Rule      => For_Rule,
         Line_Num      => Line_Num,
         Col_Num       => Col_Num,
         In_SF         => In_SF,
         Justification => Justification,
         Diagnosis_Num => Diagnosis_Num,
         Diag_Text     => Diag_Pars,
         Element_SLOC  => Element_SLOC);

   end Store_Rule_Violation;

   -----------------------------------
   -- Store_Rule_Violation_Internal --
   -----------------------------------

   procedure Store_Rule_Violation_Internal
     (For_Rule      : Rule_Id;
      Line_Num      : Types.Physical_Line_Number;
      Col_Num       : Types.Column_Number;
      In_SF         : SF_Id;
      Justification : String_Loc;
      Diagnosis_Num : Diagnosis_Variant := 0;
      Diag_Text     : String_Loc        := Nil_String_Loc;
      Element_SLOC  : String_Loc        := Nil_String_Loc)
   is
      New_Diag  : Diag_Id;
      Diag_Link : Diag_Id;
      Tmp       : Diag_Id;
      use Types;
      --  To make type operations visible in the body

      Duplication_Possible : Boolean := False;
   begin

      --  Check for possible duplications of diagnoses for compiler checks

      if For_Rule in Warning_Diags .. Restriction_Diags then
         Diag_Link := First_Rule_Diagnosis (In_SF, For_Rule);

         while Present (Diag_Link) loop
            --  Go to the diagnoses with same line and column
            if Line (Diag_Link) > Line_Num
             or else
               (Line (Diag_Link) = Line_Num
               and then
                Col (Diag_Link) > Col_Num)
            then
               exit;
            elsif Line (Diag_Link) = Line_Num
                and then
                  Col (Diag_Link) = Col_Num
            then
               Duplication_Possible := True;
               exit;
            else
               Diag_Link := Diag_Table (Diag_Link).Next_Same_Rule_Diag;
            end if;
         end loop;

         if Duplication_Possible then
            while Present (Diag_Link)
               and then
                  Line (Diag_Link) = Line_Num
                and then
                  Col (Diag_Link) = Col_Num
            loop

               if Text_Diag (Diag_Link) = Get_String (Diag_Text) then
                  --  Here we have a duplication, so - nothing to store!
                  return;
               else
                  Diag_Link := Diag_Table (Diag_Link).Next_Same_Rule_Diag;
               end if;

            end loop;
         end if;

      end if;

      --  ??? Do we need to store the source file Id here?

      Rule_Violations.Append
        ((Rule                 => For_Rule,
          SF                   => In_SF,
          Diagnosis_Num        => Diagnosis_Num,
          Line                 => Line_Num,
          Col                  => Col_Num,
          Next_Diag            => No_Diag,
          Prev_Diag            => No_Diag,
          Next_Same_Rule_Diag  => No_Diag,
          Prev_Same_Rule_Diag  => No_Diag,
          Diag_Text            => Diag_Text,
          Exempt_Justification => Justification,
          SLOC                 => Element_SLOC));

      New_Diag := Rule_Violations.Last;

      --  And now we have to update the chains:

      Diag_Link := Last_Diagnosis (In_SF);

      if No (Diag_Link) then
         --  The first diagnosis for the given source

         Set_First_Diagnosis (In_SF, New_Diag);

         --  If this is the first diagnosis for the given source, it
         --  is the first diagnosis for the given rule in the given source
         --  as well.

         Set_First_Rule_Diagnosis (In_SF, For_Rule, New_Diag);

         Set_Last_Diagnosis (In_SF, New_Diag);
         Set_Last_Rule_Diagnosis (In_SF, For_Rule, New_Diag);
      else

         --  Set all diagnoses chain
         if For_Rule > All_Rules.Last
          or else
            All_Rules.Table (For_Rule).all in Global_Rule_Template'Class
         then
            Diag_Link := First_Diagnosis (In_SF);

            while Present (Diag_Link)
              and then
                 (Line (Diag_Link) < Line (New_Diag)
                 or else
                  (Line (Diag_Link) = Line (New_Diag)
                  and then
                   Col (Diag_Link) < Col (New_Diag)))
            loop
               Diag_Link := Diag_Table (Diag_Link).Next_Diag;
            end loop;

            if No (Diag_Link) then
               --  So the new diagnosis is the rightmost in this file
               Diag_Link                        := Last_Diagnosis (In_SF);
               Diag_Table (New_Diag).Prev_Diag  := Diag_Link;
               Diag_Table (Diag_Link).Next_Diag := New_Diag;
               Set_Last_Diagnosis (In_SF, New_Diag);
            else
               Tmp := Diag_Table (Diag_Link).Prev_Diag;
               --  We have in insert the new diagnosis between Tmp and
               --  Diag_Link

               if No (Tmp) then
                  --  The new diagnosis is the leftmost for this file
                  Set_First_Diagnosis (In_SF, New_Diag);

                  Diag_Table (New_Diag).Next_Diag  := Diag_Link;
                  Diag_Table (Diag_Link).Prev_Diag := New_Diag;
               else
                  Diag_Table (Tmp).Next_Diag  := New_Diag;

                  Diag_Table (New_Diag).Prev_Diag := Tmp;
                  Diag_Table (New_Diag).Next_Diag := Diag_Link;

                  Diag_Table (Diag_Link).Prev_Diag  := New_Diag;

               end if;

            end if;

         else
            --  Diag_Link points to the last diagnosis!
            Diag_Table (New_Diag).Prev_Diag  := Diag_Link;
            Diag_Table (Diag_Link).Next_Diag := New_Diag;
            Set_Last_Diagnosis (In_SF, New_Diag);
         end if;

         --  Update the rule chain

         if For_Rule > All_Rules.Last
           or else
            All_Rules.Table (For_Rule).all in Global_Rule_Template'Class
         then
            Diag_Link := First_Rule_Diagnosis (In_SF, For_Rule);

            if No (Diag_Link) then
               --  First diagnosis for the given rule
               Set_First_Rule_Diagnosis (In_SF, For_Rule, New_Diag);
               Set_Last_Rule_Diagnosis  (In_SF, For_Rule, New_Diag);
            else

               while Present (Diag_Link)
                 and then
                     (Line (Diag_Link) < Line (New_Diag)
                     or else
                      (Line (Diag_Link) = Line (New_Diag)
                      and then
                       Col (Diag_Link) < Col (New_Diag)))
               loop
                  Diag_Link := Diag_Table (Diag_Link).Next_Same_Rule_Diag;
               end loop;

               if No (Diag_Link) then
                  --  So the new diagnosis is the rightmost in this file
                  --  for the given rule

                  Diag_Link := Last_Rule_Diagnosis (In_SF, For_Rule);
                  Diag_Table (New_Diag).Prev_Same_Rule_Diag  := Diag_Link;
                  Diag_Table (Diag_Link).Next_Same_Rule_Diag := New_Diag;
                  Set_Last_Rule_Diagnosis (In_SF, For_Rule, New_Diag);
               else
                  Tmp := Diag_Table (Diag_Link).Prev_Same_Rule_Diag;
                  --  We have in insert the new diagnosis between Tmp and
                  --  Diag_Link

                  if No (Tmp) then
                     --  The new diagnosis is the leftmost for this file
                     Set_First_Rule_Diagnosis    (In_SF, For_Rule, New_Diag);

                     Diag_Table (New_Diag).Next_Same_Rule_Diag  := Diag_Link;
                     Diag_Table (Diag_Link).Prev_Same_Rule_Diag := New_Diag;
                  else
                     Diag_Table (Tmp).Next_Same_Rule_Diag := New_Diag;

                     Diag_Table (New_Diag).Prev_Same_Rule_Diag := Tmp;
                     Diag_Table (New_Diag).Next_Same_Rule_Diag := Diag_Link;

                     Diag_Table (Diag_Link).Prev_Same_Rule_Diag := New_Diag;

                  end if;

               end if;
            end if;

         else
            Diag_Link := Last_Rule_Diagnosis (In_SF, For_Rule);

            if No (Diag_Link) then
               --  The first diagnosis for the given rule in the given source
               Set_First_Rule_Diagnosis (In_SF, For_Rule, New_Diag);
            else
               Diag_Table (New_Diag).Prev_Same_Rule_Diag  := Diag_Link;
               Diag_Table (Diag_Link).Next_Same_Rule_Diag := New_Diag;
            end if;

            Set_Last_Rule_Diagnosis (In_SF, For_Rule, New_Diag);

         end if;

      end if;

   end Store_Rule_Violation_Internal;

   ------------------
   -- Strip_Column --
   ------------------

   function Strip_Column (SLOC : String) return String is
   begin

      if No_Column_Num_In_Diagnoses then
         return SLOC (SLOC'First .. Index (SLOC, ":", Backward) - 1);
      else
         return SLOC;
      end if;

   end Strip_Column;

   ---------------
   -- Text_Diag --
   ---------------

   function Text_Diag (D : Diag_Id) return String is
   begin

      if Diag_Table (D).Rule in Warning_Diags .. Restriction_Diags then
         return Get_String (Diag_Table (D).Diag_Text);
      else
         return Insert_Actuals
                  (Message => Select_Variant
                             (Message =>
                                 All_Rules.Table (Diag_Table (D).Rule).
                                    Diagnosis.all,
                              Num     => Diag_Table (D).Diagnosis_Num),

                   Actuals => Diag_Table (D).Diag_Text);
      end if;
   end Text_Diag;

   ------------------------
   -- Text_Justification --
   ------------------------

   function Text_Justification (D : Diag_Id) return String is
   begin
      return Get_String (Diag_Table (D).Exempt_Justification);
   end Text_Justification;

end Gnatcheck.Diagnoses_Old;
