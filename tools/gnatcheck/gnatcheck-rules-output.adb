------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--               G N A T C H E C K . R U L E S . O U T P U T                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Text_IO;                use Ada.Text_IO;

with Asis.Extensions.Strings;    use Asis.Extensions.Strings;

with Types;                      use Types;

with Gnatcheck.ASIS_Utilities;
with Gnatcheck.Diagnoses_Old;
with Gnatcheck.Diagnoses;
with Gnatcheck.Options;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;

package body Gnatcheck.Rules.Output is

   Out_File : constant File_Access := Standard_Output;
   --  Temporary solution

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Report_Diagnosis
     (Message       : String;
      Name          : String;
      On            : String_Loc;
      Diagnosis_Num : Diagnosis_Variant);
      pragma Unreferenced (Report_Diagnosis);
   --  Outputs the diagnosis. Message represents all possible variants of the
   --  diagnosis. Uses Diagnosis_No to select the proper variant of the
   --  diagnosis if appropriate. (All the insertion characters are replaced
   --  with the corresponding information extracted from the Element passed as
   --  the actual for ON???). Name should be a rule name (rule ID), it is
   --  printed out as a part of diagnostic message
   --  ???
   --  At the moment this routine is not used. It should be deleted completely
   --  when gnatcheck is more stabilized

   procedure Report_Diagnosis_Variant
     (Message  : String;
      Name     : String;
      GNAT_Loc : String_Loc);
--      On      : Element);      --  ???
   --  Reports Message (that is considered as the diagnosis to output with
   --  no possible variations). (All the insertion characters are replaced with
   --  the corresponding information extracted from the Element passed as the
   --  actual for ON???) Name should be a rule name (rule ID), it is printed
   --  out as a part of diagnostic message

   function Rule_Mapping (R : Rule_Id) return String;
   --  If Gnatcheck.Options.Mapping_Mode is ON returns "(Rule_Name) ",
   --  otherwise returns a null string.
   pragma Unreferenced (Rule_Mapping);

   Max_Diag_Len : constant Integer := 1024;
   Diag_Buffer  : String (1 ..  Max_Diag_Len);
   Diag_Len     : Integer range 0 .. Max_Diag_Len := 0;
   --  Diagnosis buffer used to insert actuals in parametrized diagnostic
   --  messages

   --------------------
   -- Insert_Actuals --
   --------------------

   function Insert_Actuals
     (Message : String;
      Actuals : String_Loc)
   return String
   is
   begin

      if Actuals = Nil_String_Loc then
         return Message;
      else

         declare
            Message_Last : constant Natural := Message'Last;

            First_Idx    : Natural  := Message'First;
            Last_Idx     : Positive := Message_Last;
            --  Indexes used to parse Message

            Next_Par_N   : Positive;

            procedure Set_Next_Part;
            --  If First_Idx points to the beginning of the constant part
            --  of the message, sets Last_Idx to end of this part (either
            --  Last_Idx points to '%' of the next formal parameter or
            --  Last_Idx is equal to Message_Last. If First_Idx points
            --  to '%' or First_Idx is greater than Message_Last, Last_Idx is
            --  set to equal to First_Idx.

            Params      : constant String  := Get_String (Actuals);
            Params_Last : constant Natural := Params'Last;
            Next_Par    :          String_Ptr;

            function Get_Parameter (N : Positive) return String_Ptr;
            --  Returns the parameter number N from Params string

            function Get_Parameter (N : Positive) return String_Ptr is
               Start_Idx : Natural := Params'First;
               End_Idx   : Natural;
               Next_Num  : Positive := Positive'Last;
               Found     : Boolean  := False;
            begin
               --  Set Star_Idx pointing to the first character after the
               --  needed parameter number

               while Start_Idx < Params_Last and then Next_Num /= N loop

                  for J in Start_Idx .. Params_Last - 3 loop

                     if Params (J) = '%'
                       and then
                        Params (J + 2) = '%'
                       and then
                        Ada.Characters.Handling.Is_Digit (Params (J + 1))
                     then
                        Start_Idx := J + 1;
                        Found := True;
                        exit;
                     end if;

                  end loop;

                  if not Found then
                     raise Diagnosis_Error;
                  end if;

                  End_Idx := Start_Idx;
                  Found   := False;

                  for J in End_Idx + 1 .. Params_Last loop

                     if Params (J) = '%' then
                        End_Idx := J - 1;
                        Found := True;
                        exit;
                     end if;

                  end loop;

                  if not Found then
                     raise Diagnosis_Error;
                  end if;

                  Next_Num := Positive'Value (Params (Start_Idx .. End_Idx));

                  Start_Idx := End_Idx + 2;

               end loop;

               if Next_Num /= N then
                  raise Diagnosis_Error;
               end if;

               --  If we are here, Start_Idx points to the first character of
               --  the actual parameter for the needed parameter number. So we
               --  have to move End_Idx either to the next '%' or to the end
               --  of the Params string

               End_Idx := Params_Last;

               for J in Start_Idx .. Params_Last - 2 loop

                  if Params (J) = '%'
                    and then
                     Params (J + 2) = '%'
                    and then
                     Ada.Characters.Handling.Is_Digit (Params (J + 1))
                  then
                     End_Idx := J - 1;
                     exit;
                  end if;

               end loop;

               return new String'(Params (Start_Idx .. End_Idx));

            end Get_Parameter;

            procedure Set_Next_Part is
            begin

               if First_Idx > Message_Last
                or else
                  Message (First_Idx) = '%'
               then
                  Last_Idx := First_Idx;
               else
                  Last_Idx := Message_Last;

                  for J in First_Idx + 1 .. Message_Last loop

                     if Message (J) = '%' then
                        Last_Idx := J - 1;
                        exit;
                     end if;

                  end loop;

               end if;

            end Set_Next_Part;

         begin
            Diag_Len := 0;

            Set_Next_Part;

            while Last_Idx <= Message_Last loop

               if Message (First_Idx) /= '%' then
                  --  Copy the next constant part of the diagnosis
                  Diag_Buffer
                    (Diag_Len + 1 .. Diag_Len + Last_Idx - First_Idx + 1) :=
                      Message (First_Idx .. Last_Idx);

                  Diag_Len := Diag_Len + Last_Idx - First_Idx + 1;
               end if;

               if Last_Idx < Message_Last then

                  if First_Idx = Last_Idx
                    and then
                     First_Idx = Message'First
                    and then
                     Message (First_Idx) = '%'
                  then
                     --  Message starts from a parameter
                     Next_Par_N := 1;
                     First_Idx  := First_Idx + 3;
                  else
                     --  Message (Last_Idx + 1) points to the first '% ' of the
                     --  parameter number

                     First_Idx := Last_Idx + 2;

                     for J in First_Idx + 1 .. Message_Last loop

                        if Message (J) = '%' then
                           Last_Idx := J - 1;
                           exit;
                        end if;

                     end loop;

                     if Last_Idx < First_Idx then
                        --  We have not found the second '%'
                        raise Diagnosis_Error;
                     end if;

                     Next_Par_N :=
                       Positive'Value (Message (First_Idx .. Last_Idx));
                     --  last_Idx points to a digit before '%'
                     First_Idx := Last_Idx + 2;

                  end if;

                  Next_Par := Get_Parameter (Next_Par_N);

                  Diag_Buffer (Diag_Len + 1 .. Diag_Len + Next_Par'Length) :=
                    Next_Par.all;

                  Diag_Len := Diag_Len + Next_Par'Length;

                  Free (Next_Par);

                  Set_Next_Part;
               else
                  --  Just to stop at the next iteration:
                  Last_Idx := Message_Last + 1;
               end if;

            end loop;

         end;

         return Diag_Buffer (1 .. Diag_Len);

      end if;

   end Insert_Actuals;

   ----------------------
   -- Report_Detection --
   ----------------------

   procedure Report_Detection
     (For_Rule      : Rule_Id;
      On            : Element;
      In_SF         : SF_Id;
      Justification : String_Access;
      Diagnosis_Num : Diagnosis_Variant := 0;
      Diag_Actuals  : String_Loc;
      Diag_Line     : Natural;
      Diag_Column   : Natural)
   is
      SLOC : constant String_Loc :=
        Build_GNAT_Location (On, Diag_Line, Diag_Column);

      function Get_String_Loc (S : String_Access) return String_Loc;

      function Get_String_Loc (S : String_Access) return String_Loc is
         Result : String_Loc := Nil_String_Loc;
      begin
         if S /= null then
            Result := Enter_String (S.all);
         end if;

         return Result;
      end Get_String_Loc;

   begin

      --  For old report file:
      Gnatcheck.Diagnoses_Old.Store_Rule_Violation
        (For_Rule      => For_Rule,
         On            => On,
         In_SF         => In_SF,
         Justification => Get_String_Loc (Justification),
         Diagnosis_Num => Diagnosis_Num,
         Diag_Pars     => Diag_Actuals,
         Element_SLOC  => SLOC);

      Gnatcheck.Diagnoses.Store_Diagnosis
        (Text           =>
           Get_String (SLOC) & ": "  &
             Gnatcheck.ASIS_Utilities.Scope_Name (On) &
             Insert_Actuals
               (Message =>
                  Select_Variant
                    (Message => All_Rules.Table (For_Rule).Diagnosis.all,
                     Num     => Diagnosis_Num),
                Actuals => Diag_Actuals) &
             Annotate_Rule (All_Rules.Table (For_Rule).all, Diagnosis_Num),
         Diagnosis_Kind => Gnatcheck.Diagnoses.Rule_Violation,
         SF             => In_SF,
         Rule           => For_Rule,
         Justification  => Justification);

   end Report_Detection;

   ----------------------
   -- Report_Diagnosis --
   ----------------------

   procedure Report_Diagnosis
     (Message       : String;
      Name          : String;
      On            : String_Loc;
      Diagnosis_Num : Diagnosis_Variant)
   is
   begin

      if Message (Message'First) = '#' then
         Report_Diagnosis_Variant
           (Select_Variant (Message, Diagnosis_Num), Name, On);
      else
         Report_Diagnosis_Variant (Message, Name, On);
      end if;

   end Report_Diagnosis;

   ------------------------------
   -- Report_Diagnosis_Variant --
   ------------------------------

   procedure Report_Diagnosis_Variant
     (Message  : String;
      Name     : String;
      GNAT_Loc : String_Loc)
   is
   begin
      --  Just a first prototype, no processing of insertion character is
      --  implemented

      Put (Out_File.all, Get_String (GNAT_Loc) & ":");
      Put (Out_File.all, ' ' & Message & " (" & Name & ")");
      New_Line (Out_File.all);

   end Report_Diagnosis_Variant;

   ----------------------------------
   -- Report_Global_Rule_Detection --
   ----------------------------------

   procedure Report_Global_Rule_Detection
     (For_Rule : Rule_Id;
      On       : GS_Node_Id)
   is
   begin
      Gnatcheck.Diagnoses_Old.Store_Rule_Violation (For_Rule, On);

      --  For the moment, for global rules we have neither diagnostic variants
      --  nor parameters

      Gnatcheck.Diagnoses.Store_Diagnosis
        (Text =>
           Get_String (GS_Node_SLOC (On)) & ": "        &
                       All_Rules.Table (For_Rule).Diagnosis.all &
                       Annotate_Rule (All_Rules.Table (For_Rule).all),
         Diagnosis_Kind => Gnatcheck.Diagnoses.Rule_Violation,
         SF             => Enclosing_Source (On),
         Rule           => For_Rule,
         Justification  => null);

   end Report_Global_Rule_Detection;

   ------------------
   -- Rule_Mapping --
   ------------------

   function Rule_Mapping (R : Rule_Id) return String is
   begin
      if Gnatcheck.Options.Mapping_Mode then
         return '(' & Rule_Name (R) & ") ";
      else
         return "";
      end if;
   end Rule_Mapping;

   --------------------
   -- Select_Variant --
   --------------------

   function Select_Variant
     (Message : String;
      Num     : Diagnosis_Variant)
      return String
   is
      Variant_Num      :          Diagnosis_Variant;
      First_Idx        :          Natural := Message'First;
      Last_Idx         :          Natural;
      From_Last        : constant Natural := Message'Last;
      Variant_Detected :          Boolean := False;

      function Value (S : String) return Diagnosis_Variant;
      --  Supposing that S represents a positive integer value (with no
      --  space or '+' sign), converts it into the corresponding value of
      --  Diagnosis_Variant. Otherwise raises Diagnosis_Error. (S should not
      --  be an empty string).

      function Value (S : String) return Diagnosis_Variant is
         Result : Diagnosis_Variant;
         F_Idx, L_Idx : Natural;
      begin

         if S = "" then
            return 0;
         else
            F_Idx := S'First;
            L_Idx := S'Last;

            case S (L_Idx) is
               when '0' => Result := 0;
               when '1' => Result := 1;
               when '2' => Result := 2;
               when '3' => Result := 3;
               when '4' => Result := 4;
               when '5' => Result := 5;
               when '6' => Result := 6;
               when '7' => Result := 7;
               when '8' => Result := 8;
               when '9' => Result := 9;
               when others => raise Diagnosis_Error;
            end case;

            return Result + 10 * Value (S (F_Idx .. L_Idx - 1));

         end if;

      end Value;

   begin

      if Num = 0 or else Message (First_Idx) /= '#' then
         return Message;
      end if;

      while First_Idx <= From_Last and then Message (First_Idx) = '#' loop

         Last_Idx := First_Idx + 2;

         for J in Last_Idx .. From_Last loop

            if Message (J) = '#' then
               Last_Idx := J;
               exit;
            end if;

         end loop;

         if Last_Idx <= From_Last and then Message (Last_Idx) = '#' then
            --  Message (First_Idx) and Message (Last_Idx) now points to '#',
            --  and there is something in between. This should be a number of
            --  diagnosis variant
            Variant_Num := Value (Message (First_Idx + 1 .. Last_Idx - 1));
         else
            raise Diagnosis_Error;
         end if;

         if Num = Variant_Num then
            --  We have found the beginning of the needed diagnosis variant

            First_Idx := Last_Idx + 1;
            Last_Idx  := First_Idx;

            for J in First_Idx .. From_Last loop
               if Message (J) = '#' then
                  Last_Idx := J - 1;
                  exit;
               end if;
            end loop;

            if Last_Idx = First_Idx then
               Last_Idx := From_Last;
            end if;

            if Last_Idx in First_Idx + 1 .. From_Last then
               Variant_Detected := True;
            end if;

            exit;
         else
            --  Lets's reset First_Idx to look for the next possible variant
            First_Idx := Last_Idx + 1;

            for J in First_Idx .. From_Last loop
               if Message (J) = '#' then
                  First_Idx := J;
                  exit;
               end if;
            end loop;

         end if;

      end loop;

      if Variant_Detected then
         return Message (First_Idx .. Last_Idx);
      else
         raise Diagnosis_Error;
      end if;

   end Select_Variant;

end Gnatcheck.Rules.Output;
