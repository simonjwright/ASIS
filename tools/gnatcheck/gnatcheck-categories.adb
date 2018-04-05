------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                 G N A T C H E C K . C A T E G O R I E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;

with Rident;
with GNAT.Table;

with ASIS_UL.Common;             use ASIS_UL.Common;
with ASIS_UL.Output;             use ASIS_UL.Output;
with ASIS_UL.String_Utilities;   use ASIS_UL.String_Utilities;

with Gnatcheck.Rules;            use Gnatcheck.Rules;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;

package body Gnatcheck.Categories is

   ---------------------
   --  Category Table --
   ---------------------

   package All_Categories is new GNAT.Table (
     Table_Component_Type => Category_Access,
     Table_Index_Type     => Category_Id,
     Table_Low_Bound      => Root_Category_Id,
     Table_Initial        => 100,
     Table_Increment      => 100,
     Table_Name           => "Category table");
   --  ????

   Categories : All_Categories .Table_Ptr renames All_Categories.Table;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Get_Category (Category_Name : String) return Category_Id;
   --  Returns the Id of the category specified by Category_Name. Returns
   --  No_Category if there is no such category. Returns Root_Category_Id if
   --  the actual is an empty string. For categories that are subordinates of
   --  the root category, a simple name should be indicated, for a category
   --  that is a subordinate of some other category, the full expanded name
   --  should be indicated, starting from the name of the child of the root
   --  category.
   --  The function assumes that Category_Name does not contain any leading or
   --  trailing space, or any white space character inside.

   procedure Category_Help
     (Top_Category   : Category_Id;
      From_Status    : Rule_Statuses;
      Recursively    : Boolean;
      Level          : Natural;
      Start_Category : Boolean);
   --  Internal implementation procedure for the interface Category_Help
   --  procedure, works on Category Id instead of category name. Start_Category
   --  is set on for the first call of this procedure issued from the interface
   --  procedure to get the full info from the category regardless on the
   --  actual for Recursively.

   procedure Pretty_Print_Category
     (C     : Category_Id;
      Level : Natural);
   --  Prints a "nice looking" debug image of the category. Level defines the
   --  indentation level of the output.

   procedure Print_Category (Id : Category_Id);
   --  Prints out a simple debug image of the category stored under Id.

   procedure XML_Category_Help
     (C     : Category_Id;
      Level : Natural);
   --  Prints out the XML help for a rule category including all the rules
   --  related to category and all the subordinate categories (recursively).
   --  Level specifies the indentation level of the output.

   procedure Restrictions_Help (Level : Natural);
   --  Prints out an XML tag representing the gnatcheck Restrictions rule. Note
   --  that the representation is incomplete: the rule may have as its
   --  parameters either restriction identifiers or pairs
   --  'Restriction_Identifier => Restriction_Parameter', but the generated tag
   --  includes information about restriction identifiers only, but not about
   --  parameters that may be needed by some restrictions.

   procedure Restriction_Help (R : Rident.All_Restrictions; Level : Natural);
   --  Prints out an XML tag representing the rule Restrictions with the
   --  corresponding restriction_Id as a parameter. Takes care about
   --  restrictions that need parameters.

   procedure Exception_Cases (Level : Natural);
   --  Prints out XML tags for restrictions that because of some reason are not
   --  included in the set of values of Rident.All_Restrictions type. At the
   --  moment the only restriction that is known as this exception is
   --  No_Dependence.

   function Has_Natural_Parameter (R : Rident.All_Restrictions) return Boolean;
   function Has_Name_Parameter (R : Rident.All_Restrictions) return Boolean;
   --  Tries to guess what kind of parameter the argument restriction has.

   ------------------
   -- Add_Category --
   ------------------

   procedure Add_Category
     (New_Category      : Category_Access;
      New_Category_Name : String;
      As_Subcateory_Of  : Category_Access)
   is
      Name    : constant String := Trim (New_Category_Name, Both);
      Tmp_Id  :          Category_Id;
      Tmp_Id1 :          Category_Id;
   begin

      --  Check if this category already is in hierarchy
      if Present (New_Category.Self_Id) then
         Error ("Add_Category: category already in hierarchy");
         raise Fatal_Error;
      end if;

      --  Check if the parent is already added:
      if No (As_Subcateory_Of.Self_Id) then
         Error ("Add_Category: parent not in hierarchy");
         raise Fatal_Error;
      end if;

      --  Check if a name is specified for a new category
      if Name = "" then
         Error ("Add_Category: no name specified for a new category");
         raise Fatal_Error;
      end if;

      --  Check if the parent do not already have a subcategory with the same
      --  name

      Tmp_Id := As_Subcateory_Of.First_Child;

      while Present (Tmp_Id) loop

         if Categories (Tmp_Id).Name.all = Name then
            Error ("Add_Category: duplicated subcategory name");
            raise Fatal_Error;
         end if;

         Tmp_Id  := Categories (Tmp_Id).Next;
      end loop;

      --  If we are here, we can add new category to the category hierarchy

      All_Categories.Append (New_Category);
      New_Category.Name        := new String'(Name);
      New_Category.Self_Id     := All_Categories.Last;
      New_Category.Parent      := As_Subcateory_Of.Self_Id;
      New_Category.First_Child := No_Category;

      Tmp_Id := As_Subcateory_Of.First_Child;

      if No (Tmp_Id) then
         As_Subcateory_Of.First_Child := New_Category.Self_Id;
         New_Category.Next            := No_Category;
      else
         if Name < Categories (Tmp_Id).Name.all then
            --  Place the new category as the first child:
            As_Subcateory_Of.First_Child := New_Category.Self_Id;
            New_Category.Next            := Tmp_Id;
         else
            Tmp_Id1 := Tmp_Id;
            Tmp_Id  := Categories (Tmp_Id).Next;

            while Present (Tmp_Id) loop
               if Name < Categories (Tmp_Id).Name.all then
                  --  we have to add the new category between Tmp_Id1 and
                  --  Tmp_Id:
                  Categories (Tmp_Id1).Next := New_Category.Self_Id;
                  New_Category.Next         := Tmp_Id;
                  exit;
               end if;

               Tmp_Id1 := Tmp_Id;
               Tmp_Id  := Categories (Tmp_Id).Next;
            end loop;

            if No (Tmp_Id) then
               --  This means that new category should be the last in the
               --  children chain
               Categories (Tmp_Id1).Next := New_Category.Self_Id;
               New_Category.Next         := No_Category;
            end if;

         end if;
      end if;

   end Add_Category;

   -------------------
   -- Category_Help --
   -------------------

   procedure Category_Help
     (Category_Name : String        := "";
      From_Status   : Rule_Statuses := Fully_Implemented;
      Recursively   : Boolean       := False;
      Level         : Natural       := 0)
   is
      Top_Category      : Category_Id;
      Subordinate_Level : Natural := Level;
   begin

      if Category_Name = "" then
         Top_Category := Root_Category_Id;
         Info
           ("gnatcheck currently implements the following rule categories:");
         Subordinate_Level := 1;
      else
         Top_Category := Get_Category (Category_Name);

         if No (Top_Category) then
            Error ("Wrong category name: " & Category_Name);
            return;
         end if;

      end if;

      Category_Help
        (Top_Category,
         From_Status,
         Recursively,
         Subordinate_Level,
         Start_Category => True);

   end Category_Help;

   procedure Category_Help
     (Top_Category   : Category_Id;
      From_Status    : Rule_Statuses;
      Recursively    : Boolean;
      Level          : Natural;
      Start_Category : Boolean)
   is
      Child_Category          : Category_Id;
      Level_For_Subcategories : Natural;
   begin

      if Top_Category /= Root_Category_Id then
         Info (Level * Ident_String & Categories (Top_Category).Name.all &
               " (rule category)");
      end if;

      if Start_Category or else Recursively then

         if Present (First_Rule_In_Category (Top_Category, From_Status)) then

            if Top_Category /= Root_Category_Id then
               Info ((Level + 1) * Ident_String & "Rules in category:");

               Print_Rule_List
                 (First_Rule_In_Category (Top_Category, From_Status),
                  Level + 2,
                  From_Status);
            else
               Info ((Level) * Ident_String &
               "Rules that does not belong to any category:");

               Print_Rule_List
                 (First_Rule_In_Category (Top_Category, From_Status),
                  Level + 1,
                  From_Status);
            end if;

         end if;

         Child_Category := Categories (Top_Category).First_Child;

         if Present (Child_Category) then
            --  Info ("");

            if Top_Category /= Root_Category_Id then
               Info ((Level + 1) * Ident_String & "Subcategories");
               Level_For_Subcategories := Level + 2;
            else
               Level_For_Subcategories := Level;
            end if;

            while Present (Child_Category) loop
               Category_Help
                 (Child_Category,
                  From_Status,
                  Recursively,
                  Level_For_Subcategories,
                  Start_Category => False);

               Child_Category := Categories (Child_Category).Next;
            end loop;

         end if;

      end if;

   end Category_Help;

   -------------------
   -- Category_Name --
   -------------------

   function Category_Name (Id : Category_Id) return String is
   begin
      pragma Assert (Present (Id));

      if Id = Root_Category_Id then
         return "";
      else
         return Categories (Id).Name.all;
      end if;

   end Category_Name;

   ---------------------
   -- Exception_Cases --
   ---------------------

   procedure Exception_Cases (Level : Natural) is
   begin

      --  No_Dependence
      Info ((Level + 1) * Ident_String &
         "<field switch=""+RRestrictions:No_Dependence" &
         """ label=""No_Dependence"  &
         ", specify one unit to check"" separator=""=&gt;""/>");
   end Exception_Cases;

   -----------------
   -- First_Child --
   -----------------

   function First_Child (Id : Category_Id) return Category_Id is
   begin
      pragma Assert (Present (Id));
      return Categories (Id).First_Child;
   end First_Child;

   ----------------------------
   -- First_Rule_In_Category --
   ----------------------------

   function First_Rule_In_Category
     (Id          : Category_Id;
      From_Status : Rule_Statuses := Fully_Implemented)
      return        Rule_Id
   is
      Result : Rule_Id := Categories (Id).First_Rule;
   begin
      pragma Assert (Present (Id));

      while Present (Result) loop
         exit when Rule_Status (Result) >= From_Status;
         Result := Get_Next_Rule (Result, From_Status);
      end loop;

      return Result;
   end First_Rule_In_Category;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category (Category_Name : String) return Category_Id is
      First_Idx          :          Natural;
      Last_Idx           :          Natural;
      Category_Name_Last : constant Natural     := Category_Name'Last;
      Parent_Category    :          Category_Id := Root_Category_Id;
      Result             :          Category_Id := No_Category;
   begin

      if Category_Name = "" then
         Result := Root_Category_Id;
      else
         First_Idx := Category_Name'First;

         while First_Idx < Category_Name_Last loop
            Last_Idx :=
              Index (Category_Name (First_Idx + 1 .. Category_Name_Last), ".");

            if Last_Idx = 0 then
               Last_Idx := Category_Name_Last;
            else
               Last_Idx := Last_Idx - 1;
            end if;

            Result := Categories (Parent_Category).First_Child;

            while Present (Result) loop

               if To_Lower (Categories (Result).Name.all) =
                    To_Lower (Category_Name (First_Idx .. Last_Idx))
               then
                  exit;
               else
                  Result := Categories (Result).Next;
               end if;

            end loop;

            exit when No (Result);

            First_Idx := Last_Idx + 2;
            Parent_Category := Result;

         end loop;

      end if;

      return Result;
   end Get_Category;

   ------------------------
   -- Has_Name_Parameter --
   ------------------------

   function Has_Name_Parameter (R : Rident.All_Restrictions) return Boolean is
      Result : Boolean := False;
   begin
      if R in Rident.All_Parameter_Restrictions then
         declare
            Img       : constant String  := R'Img;
            Img_First : constant Natural := Img'First;
         begin
            if Img'Length >= 4 then
               Result := Img (Img_First .. Img_First + 2) = "NO_";
            end if;
         end;
      end if;

      return Result;
   end Has_Name_Parameter;

   ---------------------------
   -- Has_Natural_Parameter --
   ---------------------------

   function Has_Natural_Parameter
     (R    : Rident.All_Restrictions)
      return Boolean
   is
      Result : Boolean := False;
   begin
      if R in Rident.All_Parameter_Restrictions then
         declare
            Img       : constant String  := R'Img;
            Img_First : constant Natural := Img'First;
         begin
            if Img'Length >= 5 then
               Result := Img (Img_First .. Img_First + 3) = "MAX_";
            end if;
         end;
      end if;

      return Result;
   end Has_Natural_Parameter;

   -------------------
   -- Nesting_Level --
   -------------------

   function Nesting_Level (Id : Category_Id) return Natural is
      Result   : Natural := 0;
      Parent_C : Category_Id;
   begin
      pragma Assert (Present (Id));

      Parent_C := Parent_Category (Id);

      while Present (Parent_C) loop
         Result := Result + 1;
         Parent_C := Parent_Category (Parent_C);
      end loop;

      return Result;
   end Nesting_Level;

   ----------
   -- Next --
   ----------

   function Next (Id : Category_Id) return Category_Id is
   begin
      pragma Assert (Present (Id));

      return Categories (Id).Next;
   end Next;

   --------
   -- No --
   --------

   function No (Id : Category_Id) return Boolean is
   begin
      return Id not in All_Categories.First .. All_Categories.Last;
   end No;

   ---------------------
   -- Parent_Category --
   ---------------------

   function Parent_Category (Id : Category_Id) return Category_Id is
   begin
      pragma Assert (Present (Id));
      return Categories (Id).Parent;
   end Parent_Category;

   -------------
   -- Present --
   -------------

   function Present (Id : Category_Id) return Boolean is
   begin
      return Id in All_Categories.First .. All_Categories.Last;
   end Present;

   ---------------------------
   -- Pretty_Print_Category_--
   ---------------------------

   procedure Pretty_Print_Category
     (C     : Category_Id;
      Level : Natural)
   is
      Tmp : Category_Id;
   begin
      pragma Assert (Present (C));

      if Categories (C).Name /= null then
         --  not a Root category
         Info (Level * Ident_String & Categories (C).Name.all);
      end if;

      Info_No_EOL ((Level + 1) * Ident_String & "Rules in category:");

      if Present (Categories (C).First_Rule) then
         Info ("");
         Print_Rule_List
           (Categories (C).First_Rule,
            Level + 2,
            Under_Construction);
      else
         Info (" No");
      end if;

      Tmp := Categories (C).First_Child;

      if Present (Tmp) then
         Info ("");
         Info ((Level + 1) * Ident_String & "SUBCATEGORIES:");
         while Present (Tmp) loop
            Pretty_Print_Category (Tmp, Level + 1);
            Tmp := Categories (Tmp).Next;
         end loop;

      end if;

   end Pretty_Print_Category;

   -------------------------------------
   -- Pretty_Print_Category_Hierarchy --
   -------------------------------------

   procedure Pretty_Print_Category_Hierarchy is
   begin
      Info ("Currently defined rule category hierarchy is:");
      Info ("Root category (anonymous)");
      Pretty_Print_Category (Root_Category.Self_Id, 0);
   end Pretty_Print_Category_Hierarchy;

   --------------------
   -- Print_Category --
   --------------------

   procedure Print_Category (Id : Category_Id) is
      Tmp      : Category_Id;
      Tmp_Rule : Rule_Id;
   begin
      Info ("Id =" & Id'Img);

      if Categories (Id).Name = null then
         Info ("   Root category");
      else
         Info ("   Name - " & Categories (Id).Name.all);
      end if;

      Info ("   Self Id -" & Categories (Id).Self_Id'Img);
      Info ("   Parent  -" & Categories (Id).Parent'Img);
      Info ("   Next    -" & Categories (Id).Next'Img);

      Info_No_EOL ("   Subcategories:");

      if Present (Categories (Id).First_Child) then
         Tmp := Categories (Id).First_Child;

         while Present (Tmp) loop
            Info_No_EOL  (Tmp'Img);
            Tmp := Categories (Tmp).Next;
         end loop;

         Info ("");
      else
         Info (" No");
      end if;

      Info_No_EOL ("   Rules in subcategory:");

      if Present (Categories (Id).First_Rule) then
         Tmp_Rule := Categories (Id).First_Rule;

         while Present (Tmp_Rule) loop
            Info  (Tmp_Rule'Img);
            exit; --  !!!! temporary solution! Should be replaced with the real
            --  implementation when rules are chained for categories in the
            --  rule table.
         end loop;

      else
         Info (" No");
      end if;

   end Print_Category;

   ------------------------------
   -- Print_Category_Hierarchy --
   ------------------------------

   procedure Print_Category_Hierarchy is
   begin
      --  Print out category table:
      for J in All_Categories.First .. All_Categories.Last loop
         Print_Category (J);
      end loop;

   end Print_Category_Hierarchy;

   -------------------------------------
   -- Process_Category_Help_Parameter --
   -------------------------------------

   procedure Process_Category_Help_Parameter
     (P       : String;
      Success : out Boolean)
   is
   begin
      Success := True;

      for J in P'Range loop
         case P (J) is
            when 'r' => Recursive_Help     := True;
            when '1' => Rule_Report_Status := Non_Documented;
            when '2' => Rule_Report_Status := Under_Construction;

            when others =>
               Error ("wrong category help parameter: " & P);
               Success := False;
               exit;
         end case;
      end loop;

   end Process_Category_Help_Parameter;

   ----------------------
   -- Restriction_Help --
   ----------------------

   procedure Restriction_Help (R : Rident.All_Restrictions; Level : Natural) is
   begin
      if R in Rident.All_Parameter_Restrictions then
         if Has_Natural_Parameter (R) then
            Info ((Level + 1) * Ident_String &
               "<spin switch=""+RRestrictions:" & Capitalize (R'Img) &
               """ label=""" & Capitalize (R'Img) &
               """ min=""1"" max=""99999"" default=""0""" &
               " separator=""=&gt;""/>");
         elsif Has_Name_Parameter (R) then
            Info ((Level + 1) * Ident_String &
               "<field switch=""+RRestrictions:" & Capitalize (R'Img) &
               """ label=""" & Capitalize (R'Img) &
               ", specify one feature to check"" separator=""=&gt;""/>");
         else
            Error ("restriction " & R'Img & " unknown");
            pragma Assert (False);
         end if;
      else
         null;
         Info ((Level + 1) * Ident_String &
            "<check switch=""+RRestrictions:" & Capitalize (R'Img) &
            """ label=""" & Capitalize (R'Img) &
            """/>");
      end if;
   end Restriction_Help;

   -----------------------
   -- Restrictions_Help --
   -----------------------

   procedure Restrictions_Help (Level : Natural) is
   begin
      Info (Level * Ident_String & "<category name =""Restriction rules"">");

      Exception_Cases (Level + 1);

      for R in Rident.All_Restrictions loop
         Restriction_Help (R, Level + 1);
      end loop;

      Info (Level * Ident_String & "</category>");
   end Restrictions_Help;

   -------------------------
   -- XML_Categories_Help --
   -------------------------

   procedure XML_Categories_Help is
      Next_Category : Category_Id := Root_Category.First_Child;
   begin
      Info ("<?xml version=""1.0""?>");

      Info ("<gnatcheck>");

      while Present (Next_Category) loop
         XML_Category_Help (Next_Category, Level => 1);
         Next_Category := Categories (Next_Category).Next;
      end loop;

      Restrictions_Help (Level => 1);

      Info ("</gnatcheck>");
   end XML_Categories_Help;

   -----------------------
   -- XML_Category_Help --
   -----------------------
   procedure XML_Category_Help
     (C     : Category_Id;
      Level : Natural)
   is
      Next_Rule        : Rule_Id;
      Next_Subcategory : Category_Id;
   begin
      Info_No_EOL (Level * Ident_String & "<category name =""");
--      Info_No_EOL (To_Lower (Categories (C).Name.all));
      Info_No_EOL (Categories (C).Name.all);
      Info        (""">");

      Next_Rule := Categories (C).First_Rule;

      while Present (Next_Rule) loop
         XML_Rule_Help (All_Rules.Table (Next_Rule).all, Level + 1);
         Next_Rule := Get_Next_Rule (Next_Rule);
      end loop;

      Next_Subcategory := Categories (C).First_Child;

      while Present (Next_Subcategory) loop
         XML_Category_Help (Next_Subcategory, Level + 1);
         Next_Subcategory := Categories (Next_Subcategory).Next;
      end loop;

      Info (Level * Ident_String & "</category>");
   end XML_Category_Help;

begin
   --  Create the root of the category hierarchy
   All_Categories.Append (Root_Category'Access);
   Root_Category.Self_Id := All_Categories.Last;

   Add_Category
      (New_Category      => Metrics'Access,
       New_Category_Name => "Metrics-Related Rules",
       As_Subcateory_Of  => Root_Category'Access);

   Add_Category
      (New_Category      => Spark'Access,
       New_Category_Name => "SPARK Ada Rules",
       As_Subcateory_Of  => Root_Category'Access);

   Add_Category
      (New_Category      => Feature_Use_Detection'Access,
       New_Category_Name => "Feature Usage Rules",
       As_Subcateory_Of  => Root_Category'Access);

   --  Create part of the hierarchy inspired by the Ada 95 Q&S  book:
   Add_Category
      (New_Category      => Ada_95_Quality_and_Style'Access,
       New_Category_Name => "Style-Related Rules",
       As_Subcateory_Of  => Root_Category'Access);

   Add_Category
      (New_Category      => Source_Code_Presentation'Access,
       New_Category_Name => "Source Code Presentation",
       As_Subcateory_Of  => Ada_95_Quality_and_Style'Access);

   Add_Category
      (New_Category      => Readability'Access,
       New_Category_Name => "Readability",
       As_Subcateory_Of  => Ada_95_Quality_and_Style'Access);

   Add_Category
      (New_Category      => Program_Structure'Access,
       New_Category_Name => "Program Structure",
       As_Subcateory_Of  => Ada_95_Quality_and_Style'Access);

   Add_Category
      (New_Category      => Programming_Practices'Access,
       New_Category_Name => "Programming Practice",
       As_Subcateory_Of  => Ada_95_Quality_and_Style'Access);

   Add_Category
      (New_Category      => Object_Oriented_Features'Access,
       New_Category_Name => "Object Orientation",
       As_Subcateory_Of  => Ada_95_Quality_and_Style'Access);

   Add_Category
      (New_Category      => Concurrency'Access,
       New_Category_Name => "Tasking",
       As_Subcateory_Of  => Ada_95_Quality_and_Style'Access);

   Add_Category
      (New_Category      => Portability'Access,
       New_Category_Name => "Portability",
       As_Subcateory_Of  => Ada_95_Quality_and_Style'Access);

end Gnatcheck.Categories;
