------------------------------------------------------------------------------
--                                                                          --
--                        ASIS-for-GNAT COMPONENTS                          --
--                                                                          --
--              A S I S . E X T E N S I O N S . S T R I N G S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2017, AdaCore                      --
--                                                                          --
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
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package was a part of ASIS Utility Library (as ASIS_UL.Strings). It
--  has been moved into Asis.Extensions hierarchy to optimize dependencies on
--  GNATCOOL sources.

pragma Ada_2012;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with GNAT.Table;

with Asis.Elements;           use Asis.Elements;
with Asis.Set_Get;            use Asis.Set_Get;

with Atree;                   use Atree;
with Namet;                   use Namet;
with Sinput;                  use Sinput;

package body Asis.Extensions.Strings is

   Full_Names_In_SLOC : Boolean := False;

   package Chars is new GNAT.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 10000,
     Table_Increment      => 1000,
     Table_Name           => "character container");

   Table : Chars.Table_Ptr renames Chars.Table;

   -------------------------
   -- Build_GNAT_Location --
   -------------------------

   function Build_GNAT_Location_Old
     (For_Elem : Asis.Element;
      Line     : Natural := 0;
      Column   : Natural := 0)
      return     String_Loc
   is
      S     :          String_Loc := Nil_String_Loc;
      First : constant Natural    := Chars.Last + 1;

      P              : Source_Ptr;
      Sindex         : Source_File_Index;
      Instance_Depth : Natural := 0;

      function Strip_Space (S : String) return String;
      pragma Inline (Strip_Space);
      --  Is applied to the result of 'Img attribute. Cuts out the leading
      --  space.

      function Strip_Space (S : String) return String is
         First_Idx : constant Positive := S'First + 1;
         Result    : constant String := S (First_Idx .. S'Last);
      begin
         return Result;
      end Strip_Space;

      procedure Enter_Sloc
        (Line     : Natural := 0;
         Column   : Natural := 0);
      --  For the current value of P, sets in the string table the string
      --  of the form file_name:line_number. Also computes Sindex as the
      --  Id of the sourse file of P. If Line and Column are equal to zero,
      --  computes line and column number from P.

      procedure Enter_Sloc
        (Line     : Natural := 0;
         Column   : Natural := 0)
       is
         F_Name : File_Name_Type;
      begin
         Sindex := Get_Source_File_Index (P);
         F_Name := File_Name (Sindex);

         Get_Name_String (F_Name);

         S := Enter_String (Name_Buffer (1 .. Name_Len) & ":");

         if Line = 0 then
            S := Enter_String (Strip_Space (Get_Physical_Line_Number (P)'Img));
         else
            S := Enter_String (Strip_Space (Line'Img));
         end if;

         S := Enter_String (":");

         if Column = 0 then
            S := Enter_String (Strip_Space (Get_Column_Number (P)'Img));
         else
            S := Enter_String (Strip_Space (Column'Img));
         end if;

      end Enter_Sloc;

      procedure Enter_Node_Sloc
        (Line     : Natural := 0;
         Column   : Natural := 0);
      --  Enter the whole SLOC starting from the current value of P. This
      --  value should be created from the node of interest.

      procedure Enter_Node_Sloc
        (Line     : Natural := 0;
         Column   : Natural := 0)
      is
      begin
         if P = No_Location then
            S := Enter_String ("no location");
         elsif P <= Standard_Location then
            S := Enter_String ("Standard location");
         else
            Enter_Sloc (Line, Column);

            P := Instantiation (Sindex);

            while P /= No_Location loop
               pragma Assert (Line = 0 and then Column = 0);

               S              := Enter_String ("[");
               Instance_Depth := Instance_Depth + 1;

               Enter_Sloc;

               P := Instantiation (Sindex);
            end loop;

            for J in 1 .. Instance_Depth loop
               S := Enter_String ("]");
            end loop;

         end if;
      end Enter_Node_Sloc;

   begin
      --  The implementation is adopted from
      --  Gnatelim.Asis_Utilities.Build_Sloc_Trace

      P := Sloc (Node (For_Elem));

      Enter_Node_Sloc (Line, Column);

      if Is_Part_Of_Inherited (For_Elem) then
         S := Enter_String ("(");
         P := Sloc (Node_Field_1 (For_Elem));
         Enter_Node_Sloc (Line, Column);
         S := Enter_String (")");
      end if;

      S.First := First;
      S.Last  := Chars.Last;

      return S;
   end Build_GNAT_Location_Old;

   function Build_GNAT_Location_Old
     (For_Elem : Asis.Element;
      Line     : Natural := 0;
      Column   : Natural := 0)
      return     String
   is
      Last_Backup : constant Integer    := Chars.Last;
      Res_Sloc    : constant String_Loc :=
        Build_GNAT_Location (For_Elem, Line, Column);
   begin

      declare
         Result : constant String := Get_String (Res_Sloc);
      begin
         Chars.Set_Last (Last_Backup);
         return Result;
      end;

   end Build_GNAT_Location_Old;

   -----------------------------
   -- Build_GNAT_Location_New --
   -----------------------------

   function Build_GNAT_Location
     (For_Elem : Asis.Element;
      Line     : Natural := 0;
      Column   : Natural := 0)
      return     String_Loc
   is
      S     :          String_Loc := Nil_String_Loc;
      First : constant Natural    := Chars.Last + 1;

      P      : Source_Ptr;
      Sindex : Source_File_Index;

--      function Strip_Space (S : String) return String;
--      pragma Inline (Strip_Space);
--      --  Is applied to the result of 'Img attribute. Cuts out the leading
--      --  space.

--      function Strip_Space (S : String) return String is
--         First_Idx : constant Positive := S'First + 1;
--         Result    : constant String := S (First_Idx .. S'Last);
--      begin
--         return Result;
--      end Strip_Space;

      procedure Enter_Sloc
        (Line     : Natural := 0;
         Column   : Natural := 0);
      --  For the current value of P, sets in the string table the string
      --  of the form file_name:line_number. Also computes Sindex as the
      --  Id of the sourse file of P. If Line and Column are equal to zero,
      --  computes line and column number from P.

      function Adjust_Column (C : String) return String is
        (if C'Length = 1 then '0' & C else C);
      --  Converts the column number into the format used in the compiler - if
      --  it is less than 10 - adds a leading '0'.

      procedure Enter_Sloc
        (Line     : Natural := 0;
         Column   : Natural := 0)
       is
         F_Name : File_Name_Type;
      begin
         Sindex := Get_Source_File_Index (P);

         if Full_Names_In_SLOC then
            F_Name := Full_File_Name (Sindex);
         else
            F_Name := File_Name (Sindex);
         end if;

         Get_Name_String (F_Name);

         S := Enter_String (Name_Buffer (1 .. Name_Len) & ":");

         if Line = 0 then
            S := Enter_String (Trim (Get_Physical_Line_Number (P)'Img, Left));
         else
            S := Enter_String (Trim (Line'Img, Left));
         end if;

         S := Enter_String (":");

         if Column = 0 then
            S :=
              Enter_String
                (Adjust_Column (Trim (Get_Column_Number (P)'Img, Left)));
         else
            S := Enter_String (Adjust_Column (Trim (Column'Img, Left)));
         end if;

      end Enter_Sloc;

      procedure Enter_Node_Sloc
        (Line     : Natural := 0;
         Column   : Natural := 0);
      --  Enter the whole SLOC starting from the current value of P. This
      --  value should be created from the node of interest.

      procedure Enter_Node_Sloc
        (Line     : Natural := 0;
         Column   : Natural := 0)
      is
      begin
         if P = No_Location then
            S := Enter_String ("no location");
         elsif P <= Standard_Location then
            S := Enter_String ("Standard location");
         else
            Enter_Sloc (Line, Column);

            P := Instantiation (Sindex);

            while P /= No_Location loop
               pragma Assert (Line = 0 and then Column = 0);
               S := Enter_String (Instance_SLOC_Txt);

               Enter_Sloc;

               P := Instantiation (Sindex);
            end loop;

         end if;
      end Enter_Node_Sloc;

   begin
      --  The implementation is adopted from
      --  Gnatelim.Asis_Utilities.Build_Sloc_Trace

      P := Sloc (Node (For_Elem));

      Enter_Node_Sloc (Line, Column);

      if Is_Part_Of_Inherited (For_Elem) then
         S := Enter_String ("(");
         P := Sloc (Node_Field_1 (For_Elem));
         Enter_Node_Sloc (Line, Column);
         S := Enter_String (")");
      end if;

      S.First := First;
      S.Last  := Chars.Last;

      return S;
   end Build_GNAT_Location;

   function Build_GNAT_Location
     (For_Elem : Asis.Element;
      Line     : Natural := 0;
      Column   : Natural := 0)
      return     String
   is
      Last_Backup : constant Integer    := Chars.Last;
      Res_Sloc    : constant String_Loc :=
        Build_GNAT_Location (For_Elem, Line, Column);
   begin

      declare
         Result : constant String := Get_String (Res_Sloc);
      begin
         Chars.Set_Last (Last_Backup);
         return Result;
      end;

   end Build_GNAT_Location;

   ------------------
   -- Enter_String --
   ------------------

   function Enter_String (S : String) return String_Loc is
      Len   : constant Integer := S'Length;
      F     :          Integer;
   begin

      if Len = 0 then
         return Nil_String_Loc;
      else
         Chars.Increment_Last;
         F := Chars.Last;
         Chars.Set_Last (F + Len - 1);

         Table (F .. F + Len - 1) := Chars.Table_Type (S);

         return (F, F + Len - 1);
      end if;

   end Enter_String;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (SL : String_Loc) return String is
   begin

      if SL = Nil_String_Loc then
         return "";
      else
         return String (Table (SL.First .. SL.Last));
      end if;

   end Get_String;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Chars.Init;
   end Init;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (S : String; SL : String_Loc) return Boolean is
      Result : Boolean := False;
   begin

      if SL.First in 1 .. Chars.Last
        and then
         SL.Last in 1 .. Chars.Last
        and then
         SL.Last - SL.First + 1 = S'Length
      then
         Result := S = Get_String (SL);
      end if;

      return Result;

   end Is_Equal;

   ----------------
   -- Old_Format --
   ----------------

   function Old_Format (SLOC : String) return String is
      Result     :          String (SLOC'Range);
      Res_Idx    :          Natural := Result'First;
      Count      :          Natural := 0;
      SLOC_Start :          Natural;
      SLOC_End   :          Natural;
      SLOC_Last  : constant Natural := SLOC'Last;
   begin
      SLOC_End := Index (SLOC, Instance_SLOC_Txt);

      if SLOC_End = 0 then
         return SLOC;
      end if;

      SLOC_Start := SLOC'First;

      while SLOC_End /= 0 loop
         SLOC_End   := SLOC_End - 1;
         Result (Res_Idx .. Res_Idx + (SLOC_End - SLOC_Start)) :=
            SLOC (SLOC_Start .. SLOC_End);
         Res_Idx          := Res_Idx + (SLOC_End - SLOC_Start + 1);
         Result (Res_Idx) := '[';
         Res_Idx          := Res_Idx + 1;
         Count            := Count + 1;

         SLOC_Start := SLOC_End + Instance_SLOC_Txt'Length + 1;
         SLOC_End   :=
           Index (SLOC (SLOC_Start + 1 .. SLOC_Last), Instance_SLOC_Txt);
      end loop;

      Result (Res_Idx .. Res_Idx + (SLOC_Last - SLOC_Start)) :=
         SLOC (SLOC_Start .. SLOC_Last);
      Res_Idx          := Res_Idx + (SLOC_Last - SLOC_Start);

      for J in 1 .. Count loop
         Res_Idx          := Res_Idx + 1;
         Result (Res_Idx) := ']';
      end loop;

      return Result (Result'First .. Res_Idx);

   end Old_Format;

   --------------------
   -- Set_Full_Names --
   --------------------

   procedure Set_Full_Names (On : Boolean) is
   begin
      Full_Names_In_SLOC := On;
   end Set_Full_Names;

   --------------------
   -- SLOC_Less_Than --
   --------------------

   function SLOC_Less_Than (L, R : String) return Boolean is

      L_Start : Natural := L'First;
      R_Start : Natural := R'First;

      L_End   : Natural := Index (L, ":");
      R_End   : Natural := Index (R, ":");
      L_Val   : Positive;
      R_Val   : Positive;
   begin
      --  When comparing SLOCs we have the following problems:
      --  1. file names may be in different casing, and X.ads < a.ads for
      --     predefined "<"
      --
      --  2. ':' that separates file name, line and column is greater than
      --     any digit, so for the predefined "<" a.ads:50:20 is less than
      --     a.ads 5:20
      --
      --  3. we cannot use alphabetical comparision for the whole SLOC, because
      --     line and column numbers should be compared as digits
      --
      --  4. if Full_Source_Locations is ON, in Windows we may have ':' as a
      --     part of a full file name.

      if L_End = L_Start + 1
        and then
         L (L_End + 1) = '\'
      then
         L_End := Index (L (L_End + 2 .. L'Last), ":");
      end if;

      if R_End = R_Start + 1
        and then
         R (R_End + 1) = '\'
      then
         R_End := Index (R (R_End + 2 .. R'Last), ":");
      end if;

      if L_End = 0 or else R_End = 0 then
         return False;
      elsif To_Lower (L (L_Start .. L_End)) <
            To_Lower (R (R_Start .. R_End))
      then
         return True;
      elsif To_Lower (L (L_Start .. L_End)) >
            To_Lower (R (R_Start .. R_End))
      then
         return False;
      else
         --  file names are the same, we have to compare line and column
         --  numbers
         L_Start := L_End + 1;
         R_Start := R_End + 1;
         L_End   := Index (L (L_Start .. L'Last), ":") - 1;
         R_End   := Index (R (R_Start .. R'Last), ":") - 1;
         L_Val   := Positive'Value (L (L_Start .. L_End));
         R_Val   := Positive'Value (R (R_Start .. R_End));

         if L_Val < R_Val then
            return True;
         elsif L_Val > R_Val then
            return False;
         else
            --  line numbers are also the same
            L_Start := L_End + 2;
            R_Start := R_End + 2;

            L_End   := Index (L (L_Start .. L'Last), Instance_SLOC_Txt);
            R_End   := Index (R (R_Start .. R'Last), Instance_SLOC_Txt);

            --  For old SLOC format:
            --
            --  L_End   := Index (L (L_Start .. L'Last), "[");
            --  R_End   := Index (R (R_Start .. R'Last), "[");

            if L_End = 0 then
               L_End   := L'Last;
            else
               --  SLOC from instantiation
               L_End := L_End - 1;
            end if;

            if R_End = 0 then
               R_End   := R'Last;
            else
               --  SLOC from instantiation
               R_End := R_End - 1;
            end if;

            L_Val   := Positive'Value (L (L_Start .. L_End));
            R_Val   := Positive'Value (R (R_Start .. R_End));

            if L_Val < R_Val then
               return True;
            else
               return False;
            end if;

         end if;
      end if;

   end SLOC_Less_Than;

end Asis.Extensions.Strings;
