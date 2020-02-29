------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                    G N A T 2 X M L . A D A _ T R E E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Wide_Text_IO;
with Unchecked_Deallocation;
with System.WCh_Con;
use type System.WCh_Con.WC_Encoding_Method;
with GNAT.OS_Lib;

with Snames;
with Types;
use type Types.Int;

with ASIS_UL.Generic_Formatted_Output;
with ASIS_UL.Output; use ASIS_UL;
with ASIS_UL.Common;
with Pp.Formatting.Dictionaries;
with Ada_Trees.Self_Rep;

separate (Ada_Trees.Formatting)

procedure Tree_To_Ada
  (Root      : Ada_Tree;
   Src_Buf   : in out Buffer;
   Write_BOM : Boolean;
   Source_Name : String;
   Options   : Formatting_Options;
   Output_Name : String;
   Form_String : String;
   Do_Diff : Boolean;
   Output_Written : out Boolean)
is

   pragma Warnings (Off); -- ????
   --  Miscellaneous useful Name_Ids:

   Name_Empty : constant Name_Id := Name_Find ("");

   Name_Semicolon : constant Name_Id := Name_Find (";");
   Name_L_Paren   : constant Name_Id := Name_Find ("(");
   Name_R_Paren   : constant Name_Id := Name_Find (")");
   Name_Colon     : constant Name_Id := Name_Find (":");
   Name_Assign    : constant Name_Id := Name_Find (":=");
   Name_Bang      : constant Name_Id := Name_Find ("!");
   Name_Bar       : constant Name_Id := Name_Find ("|");
   Name_Arrow     : constant Name_Id := Name_Find ("=>");
   Name_Dot       : constant Name_Id := Name_Find (".");

   Name_And_Then : constant Name_Id := Name_Find ("and then");
   Name_Or_Else  : constant Name_Id := Name_Find ("or else");

   Name_Q_And : constant Name_Id := Name_Find ("""and""");
   Name_Q_Or  : constant Name_Id := Name_Find ("""or""");
   Name_Q_Xor : constant Name_Id := Name_Find ("""xor""");
   Name_Q_Mod : constant Name_Id := Name_Find ("""mod""");
   Name_Q_Rem : constant Name_Id := Name_Find ("""rem""");
   Name_Q_Abs : constant Name_Id := Name_Find ("""abs""");
   Name_Q_Not : constant Name_Id := Name_Find ("""not""");

   Name_Depends : constant Name_Id := Name_Find ("Depends");
   Name_Refined_Depends : constant Name_Id := Name_Find ("Refined_Depends");

   Name_Tab_Insertion_Point : constant Name_Id :=
     Name_Find ("tab insertion point");
   Name_Tab_In_Out : constant Name_Id := Name_Find ("tab in out");
   Name_Dot_Dot : constant Name_Id := Name_Find ("..");
   Name_R_Sq : constant Name_Id := Name_Find ("]");

   Op_Sym_Table : constant array (Positive range <>) of Name_Id :=
     (Name_Q_And,
      Name_Q_Or,
      Name_Q_Xor,
      Name_Q_Mod,
      Name_Q_Rem,
      Name_Q_Abs,
      Name_Q_Not);

   function Is_Op_Sym_With_Letters
     (N    : Name_Id)
      return Boolean is
     (for some Op of Op_Sym_Table => N = Op);
   --  True if N looks like a string literal that can be used as an operator
   --  symbol containing letters, so case might matter. N should be in all
   --  lower case.
   pragma Warnings (On);

   procedure Put_To_Buffer (C : W_Char);
   --  Append C to Buffer

   procedure Init_Template_Table;
   --  We call this to initialize Template_Table the first time Tree_To_Ada
   --  is called, so that we can base the initialization in part on the
   --  command-line options.

   procedure Init_Pp_Off_And_On;
   --  Initialize Pp_Off_On_Delimiters from Options

   procedure Assert_No_Trailing_Blanks (S : W_Str);
   --  Assert that there are no lines with trailing blanks in S.

   function Id_With_Casing
     (Id                       : Name_Id;
      Kind                     : Opt_ASIS_Elems;
      Is_Predef                : Boolean;
      Use_Name_Casing_For_Nils : Boolean := False)
      return                     W_Str;
   --  This handles casing of defining names and usage names, converting to
   --  the appropriate case based on command-line options. Kind is the kind of
   --  declaration denoted by Id, or an attribute, or nil. Is_Predef is True if
   --  Id denotes a predefined Ada or GNAT identifier.
   --
   --  This is called early (during Subtree_To_Ada). Casing of reserved words
   --  is handled later, in a separate pass (see Keyword_Casing), because they
   --  are not explicit in the tree, except that operator symbols are handled
   --  here. All of the Ada_Templates have reserved words in lower case.
   --
   --  Id_With_Casing is used for Def_Names, Usage_Names and pragmas. For
   --  Def_Names, the Kind comes from the Symbol_Table, which only works
   --  because it's within one unit. That doesn't work for Usage_Names; we
   --  use the Decl_Kind attribute, which includes declared entities and
   --  attributes. For pragmas, we use the Kind of the pragma node.
   --
   --  Is_Predef comes from the Is_Predef attribute of Usage_Names. It is
   --  always False for Def_Names and pragmas.
   --
   --  Use_Name_Casing_For_Nils is documented in Do_Usage_Name.

   procedure Put_To_Buffer (C : W_Char) is
   begin
      pragma Assert
        (if
           Check_Whitespace and then Options.Par_Specs_Threshold = Natural'Last
         then
           (if C = ' ' then Lookback (Out_Buf) /= ' '));
      --  No double blanks. Except that there is one special case when the
      --  Par_Specs_Threshold switch is used, where we have an extra blank
      --  (see Subp_Decl_With_Hard_Breaks).
      pragma Assert (if C = NL then Lookback (Out_Buf) /= ' ');
      --  no trailing blanks

      Append_Any (Out_Buf, C);
   end Put_To_Buffer;

   Name_CPP_Class : aliased constant W_Str := "CPP_Class";
   Name_CPP_Constructor : aliased constant W_Str := "CPP_Constructor";
   Name_CPP_Virtual : aliased constant W_Str := "CPP_Virtual";
   Name_CPP_Vtable  : aliased constant W_Str := "CPP_Vtable ";
   Name_CPU : aliased constant W_Str := "CPU";
   Name_Persistent_BSS : aliased constant W_Str := "Persistent_BSS";
   Name_SPARK_Mode : aliased constant W_Str := "SPARK_Mode";
   Name_Use_VADS_Size : aliased constant W_Str := "Use_VADS_Size";
   Name_VADS_Size : aliased constant W_Str := "VADS_size";

   Special_Case_Names : constant
       array (Positive range <>) of access constant W_Str :=
     (Name_CPP_Class'Access,
      Name_CPP_Constructor'Access,
      Name_CPP_Virtual'Access,
      Name_CPP_Vtable 'Access,
      Name_CPU'Access,
      Name_Persistent_BSS'Access,
      Name_SPARK_Mode'Access,
      Name_Use_VADS_Size'Access,
      Name_VADS_Size'Access);

   function Id_With_Casing
     (Id                       : Name_Id;
      Kind                     : Opt_ASIS_Elems;
      Is_Predef                : Boolean;
      Use_Name_Casing_For_Nils : Boolean := False)
      return                     W_Str
   is

      Str : W_Str := Get_Name_String (Id);
      --  This is the name as declared
      pragma Assert (Str'First = 1);

      --  If it's a character literal, we want As_Declared -- it would be
      --  unfortunate to turn 'a' into 'A'. Operators go by keyword casing.
      --  Operator symbols (quoted) do so also, which seems wrong, but we're
      --  going to mimic the old gnatpp for now. Note that some reserved
      --  words can be an operator or an attribute name; hence the check
      --  for Flat_Attribute_Reference_Kinds below. Predefined names use
      --  As_Declared unless Use_Predefined_Casing is turned off. For
      --  everything else, we use the appropriate option based on the Kind.

      Casing : constant PP_Casing :=
        (if Str (1) = ''' then As_Declared
         elsif
           Kind not in Flat_Attribute_Reference_Kinds
           and then
           (Str (1) = '"' -- operator symbol
            or else Snames.Is_Keyword_Name (Id)
            or else Id = Name_And_Then
            or else Id = Name_Or_Else)
         then
           Options.PP_Keyword_Casing
         elsif
           Is_Predef and then Options.Use_Predefined_Casing
         then
           As_Declared
         else
           (case Kind is
              when Flat_Attribute_Reference_Kinds =>
                Options.PP_Attribute_Casing,
              when Flat_Pragma_Kinds => Options.PP_Pragma_Casing,
              when An_Enumeration_Literal_Specification =>
                Options.PP_Enum_Literal_Casing,
              when A_Flat_Type_Declaration |
                A_Subtype_Declaration |
                A_Formal_Type_Declaration |
                A_Formal_Incomplete_Type_Declaration |
                A_Task_Body_Declaration |
                A_Protected_Body_Declaration =>
                Options.PP_Type_Casing,
              when A_Flat_Number_Declaration => Options.PP_Nnumbers_Casing,
              when Not_An_Element            =>
                (if
                   Options.Is_PP
                   and then not Use_Name_Casing_For_Nils
                   and then Options.PP_Name_Casing = As_Declared
                 then
                   Mixed
                 else Options.PP_Name_Casing),
              when others => Options.PP_Name_Casing));
      --  The Not_An_Element case is for identifiers specific to pragmas
      --  and the like. But that only works if the Decl_Kind field is set,
      --  which isn't true in xml2gnat, so we use PP_Name_Casing (which is
      --  As_Declared) in that case.

      use Pp.Formatting.Dictionaries;
   begin
      if Options.Use_Dictionary then
         Check_With_Dictionary (Ada_Name => Str, Casing => Casing);
         return Str;
      else
         case Casing is
            when Lower_Case =>
               return To_Lower (Str);

            when Upper_Case =>
               return To_Upper (Str);

            when Mixed =>
               if Kind in Flat_Attribute_Reference_Kinds | Flat_Pragma_Kinds
               then
                  --  Handle pragma and attribute names that are special cases
                  --  (some portion should be in ALL CAPS).

                  declare
                     Lower : constant W_Str := To_Lower (Str);
                  begin
                     for Special of Special_Case_Names loop
                        if Lower = To_Lower (Special.all) then
                           return Special.all;
                        end if;
                     end loop;
                  end;
               end if;

               return Capitalize (Str);

            when As_Declared =>
               return Str;
         end case;
      end if;
   end Id_With_Casing;

   package Buffered_Output is new ASIS_UL.Generic_Formatted_Output
     (W_Char,
      W_Str,
      Basic_Put_Char => Put_To_Buffer);

   procedure Indent (Amount : Integer);

   procedure Indent (Amount : Integer) is
      pragma Assert
        (abs (Amount) in
           0 |
             1 |
             Options.PP_Indentation |
             Options.PP_Cont_Line_Indentation);
      Line_Breaks : Line_Break_Vector renames All_Line_Breaks;
   begin
      Cur_Indentation := Cur_Indentation + Amount;

      if abs (Amount) = Options.PP_Indentation then
         pragma Assert (Point (Out_Buf) = Last_Position (Out_Buf) + 1);
         if Last_Position (Out_Buf) =
           Position (Out_Buf, Line_Breaks (Last (Line_Breaks)).Mark)
         then
--  pragma Assert (At_Point (Out_Buf, Line_Breaks (Last (Line_Breaks)).Mark));
            Line_Breaks (Last (Line_Breaks)).Indentation := Cur_Indentation;
         end if;
      end if;
   end Indent;

   procedure Append_Line_Break
     (Hard     : Boolean;
      Affects_Comments : Boolean;
      Level    : Nesting_Level;
      Kind     : Ada_Tree_Kind;
      Template : Name_Id);

   function Max_Nesting_Increment (Temp : Ada_Template) return Nesting_Level;
   --  If a digit occurs after '@', this is an additional "nesting increment"
   --  to be added to the nesting level when we recursively process the
   --  subtree. This is intended to allow some line breaks to have precedence
   --  over others. If no such digit occurs, the default is zero. This function
   --  returns the maximum such nesting increment in the template.

   function New_Level
     (Tree          : Ada_Tree;
      Subtree_Index : Query_Index;
      Cur_Level     : Nesting_Level;
      Temp          : Ada_Template)
      return          Nesting_Level;
   --  Compute a new nesting level for a subtree. This is usually one more than
   --  the current level, but we also add in Max_Nesting_Increment.

   procedure If_Statement_Check_1;
   procedure If_Statement_Check_2 (Level_Of_If : Nesting_Level);
   --  The above are for a special check related to if_statements, which comes
   --  in two parts. If_Statement_Check_1 and _2 are called before and after
   --  calling Subtree_To_Ada on the condition of an 'if'.
   --
   --  The compiler style checks complain if "then" appears by itself on the
   --  line immediately following "if" (still true???), as in:
   --     if <condition>
   --     then
   --  where <condition> is just long enough to split the line before "then",
   --  but not long enough to be split itself. To avoid that, we make sure
   --  at least one line break in <condition> is at the same level as the one
   --  just before "then", thus ensuring that if the latter is enabled, some
   --  line break within <condition> will also be enabled. The same goes for
   --  "elsif".
   --
   --  Part _1 remembers the index of the first line break for the condition.
   --  Then the condition is walked, possibly inserting some line breaks. Part
   --  _2 then finds the minimum nested level (i.e. outermost), and patches
   --  that to equal the level of the 'if'. If there are no line breaks in the
   --  condition, but it is still long enough to force the "then" onto the next
   --  line, then there's not much we can do -- the style check will fail in
   --  that unlikely case.

   procedure Append_Line_Break
     (Hard     : Boolean;
      Affects_Comments : Boolean;
      Level    : Nesting_Level;
      Kind     : Ada_Tree_Kind;
      Template : Name_Id)
   is
      pragma Unreferenced (Kind);
      Line_Breaks : Line_Break_Vector renames All_Line_Breaks;
   begin
      --  If we see two line breaks in a row, we take the least indented one.

      if Hard and then Lookback (Out_Buf) = NL then
         if Line_Breaks (Last_Index (Line_Breaks)).Indentation >
           Cur_Indentation
         then
            Line_Breaks (Last_Index (Line_Breaks)).Indentation :=
              Cur_Indentation;
         end if;

         if not Options.Insert_Blank_Lines then
            return;
         end if;
      end if;

      Append
        (Line_Breaks,
         Line_Break'
           (Mark        => Mark (Out_Buf, Name => (if Hard then '$' else '@')),
            Hard        => Hard,
            Affects_Comments => Affects_Comments,
            Enabled     => Hard,
            Level       => Level,
            Indentation => Cur_Indentation,
            Length      => <>,
--            Kind        => Kind,
            Template    => Template,
            UID         => Next_Line_Break_Unique_Id));
      Next_Line_Break_Unique_Id := Next_Line_Break_Unique_Id + 1;

      --  A hard line break gets NL

      if Hard then
         Buffered_Output.Put_Char (NL);
      end if;
   end Append_Line_Break;

   function Max_Nesting_Increment (Temp : Ada_Template) return Nesting_Level is
      J : Positive := Temp'First;
      C : W_Char;

   begin
      return Result : Nesting_Level := 0 do
         while J <= Temp'Last loop
            C := Temp (J);

            case C is
               when '@' =>
                  declare
                     Digit     : W_Char;
                     Increment : Nesting_Level;

                  begin
                     if J < Temp'Last and then Temp (J + 1) in '0' .. '9' then
                        J         := J + 1;
                        Digit     := Temp (J);
                        Increment := Nesting_Level (Char_To_Digit (Digit));

                     else
                        Increment := 0;
                     end if;

                     Result := Nesting_Level'Max (Result, Increment);
                  end;

               when others =>
                  null;
            end case;

            J := J + 1;
         end loop;
      end return;
   end Max_Nesting_Increment;

   function New_Level
     (Tree          : Ada_Tree;
      Subtree_Index : Query_Index;
      Cur_Level     : Nesting_Level;
      Temp          : Ada_Template)
      return          Nesting_Level
   is
   begin
      pragma Assert
        (if Tree.Kind in An_If_Path | An_Elsif_Path then Subtree_Index = 1);

      return Cur_Level + Max_Nesting_Increment (Temp) + 1;
   end New_Level;

   First_If_Line_Break : Line_Break_Index;
   --  Valid only between calls to If_Statement_Check_1 and
   --  If_Statement_Check_2. Set by _1 to 1 past the end of the table, which
   --  is where the next line break will be placed. Used by _2 to find the
   --  first line break (if any) belonging to the condition.

   procedure If_Statement_Check_1 is
      Line_Breaks : Line_Break_Vector renames All_Line_Breaks;
   begin
      First_If_Line_Break := Last_Index (Line_Breaks) + 1;
   end If_Statement_Check_1;

   procedure If_Statement_Check_2 (Level_Of_If : Nesting_Level) is
      Line_Breaks : Line_Break_Vector renames All_Line_Breaks;
      Min : Nesting_Level := Nesting_Level'Last;
   begin
      --  Find the minimum level:
      for J in First_If_Line_Break .. Last_Index (Line_Breaks) loop
         Min := Nesting_Level'Min (Min, Line_Breaks (J).Level);
      end loop;

      --  Overwrite all line breaks at the minimum level to the level of the
      --  'if':
      for J in First_If_Line_Break .. Last_Index (Line_Breaks) loop
         if Line_Breaks (J).Level = Min then
            Line_Breaks (J).Level := Level_Of_If;
         end if;
      end loop;
   end If_Statement_Check_2;

   function Remove_Extra_Line_Breaks return Char_Vector;
   --  Removes extra NL's. The result has exactly one NL at the beginning, and
   --  exactly one at the end. Also, if Preserve_Blank_Lines is False, we
   --  collapse 3 or more NL's in a row down to 2.  ???It would be cleaner if
   --  we didn't put multiple blank lines in in the first place.
   --
   --  This also converts LF to CRLF if appropriate.

   --  Wide_Text_IO accepts a Form parameter that inserts CR's on windows, but
   --  it doesn't do that on unix, so we insert CR's by hand.

   function Remove_Extra_Line_Breaks return Char_Vector is
      Is_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';
      Add_CR : constant Boolean :=
        (case Output.Out_File_Format is
           when Output.Default => (if Is_Windows then True else False),
           when Output.CRLF => True,
           when Output.LF => False);
      --  True if we should convert LF to CRLF -- if it was requested on the
      --  command line, or if we're on windows an nothing was requested.

   begin
      --  Optimize the case where we're not changing anything. The reason
      --  Remove_Extra_Line_Breaks keeps the initial NL is that this
      --  optimization wouldn't work otherwise.

      if Options.Preserve_Blank_Lines and then not Add_CR then
         return To_Vector (Out_Buf);
      end if;

      declare
         Result : Char_Vector;
      begin
         while Cur (Out_Buf) = NL loop
            Move_Forward (Out_Buf);
         end loop;
         Append (Result, W_LF);
         --  We don't want a CR here; caller skips the one LF character

         loop
            declare
               NL_Count : Natural := 0;
            begin
               while Cur (Out_Buf) = NL loop
                  Move_Forward (Out_Buf);
                  NL_Count := NL_Count + 1;
               end loop;

               exit when At_End (Out_Buf);

               if not Options.Preserve_Blank_Lines and then NL_Count > 2 then
                  NL_Count := 2;
               end if;

               for J in 1 .. NL_Count loop
                  if Add_CR then
                     Append (Result, W_CR);
                  end if;
                  Append (Result, W_LF);
               end loop;
            end;

            pragma Assert (Cur (Out_Buf) /= NL);
            Append (Result, Cur (Out_Buf));
            Move_Forward (Out_Buf);
         end loop;

         if Add_CR then
            Append (Result, W_CR);
         end if;
         Append (Result, W_LF);
         Reset (Out_Buf);
         pragma Assert (Result (1) = NL);
         pragma Assert (Result (2) /= NL);
         if not Add_CR then
            pragma Assert (Result (Last_Index (Result) - 1) /= NL);
            pragma Assert (Result (Last_Index (Result)) = NL);
         end if;
         return Result;
      end;
   end Remove_Extra_Line_Breaks;

   procedure Write_Str (Out_Elems : W_Str);
   procedure Write_Out_Buf (Out_Vec : Char_Vector);
   procedure Write_Src_Buf;
   --  Write_Out_Buf writes Out_Buf to the output. This is the normal
   --  case. Write_Src_Buf writes the Src_Buf to the output. Write_Str is the
   --  code common to both Write_Out_Buf and Write_Src_Buf.

   procedure Write_Str (Out_Elems : W_Str) is
      use Wide_Text_IO;
      Out_File : File_Type;
   begin
--  ???
--      if False then -- ???Messes up the diff's.
--         Formatted_Output.Put
--           ("--  ???Inner_Loop_Count = \1\n",
--            Image (Inner_Loop_Count));
--      end if;

      Output_Written := True;
      if Output_Name /= "" then
         --  If Output_Name = "", leave Current_Output pointing to standard
         --  output; otherwise point it to the file.
         Create (Out_File, Name => Output_Name,
                 Form => Form_String & ",Text_Translation=NO");
         Set_Output (Out_File);
      end if;

      --  If a BOM (byte order mark) was found in the input, we want to put it
      --  in the output.

      if Write_BOM then
         if Options.Output_Encoding /= System.WCh_Con.WCEM_UTF8 then
            raise Program_Error;
         end if;
         Put (W_Char'Val (16#FEFF#)); -- BOM as a wide character
      end if;

      --  We must call New_Line for LF's (at least for the last one in the
      --  Out_Elems), because otherwise Wide_Text_IO adds an annoying blank
      --  line to the end of the file. It would probably be better to avoid
      --  Wide_Text_IO altogether, but we're currently using it to do Unicode
      --  encoding transformations. Note that Put(CR) is not guaranteed to work
      --  by the Ada standard, but the GNAT implementation won't molest it.

      for C of Out_Elems loop
         if C = W_LF then
            New_Line;
         else
            Put (C);
         end if;
      end loop;

      if Output_Name /= "" then
         Close (Out_File);
         Set_Output (Ada.Wide_Text_IO.Standard_Output);
      end if;
   end Write_Str;

   procedure Write_Out_Buf (Out_Vec : Char_Vector) is
      pragma Assert (Point (Out_Buf) = 1);
      Out_Elems : W_Str renames Elems (Out_Vec)
        (2 .. Last_Index (Out_Vec)); -- 2 to skip initial NL
   begin
      --  In Do_Diff mode, don't write the output if it is identical to the
      --  input.

      if Do_Diff then
         declare
            Src_Elems : W_Str renames Elements (Src_Buf)
              (1 .. Last_Position (Src_Buf));
         begin
            if Out_Elems = Src_Elems then
               Output_Written := False;
               return;
            end if;
         end;
      end if;

      Write_Str (Out_Elems);
   end Write_Out_Buf;

   procedure Write_Src_Buf is
      Out_Elems : W_Str renames Elements (Src_Buf)
        (1 .. Last_Position (Src_Buf));
   begin
      Write_Str (Out_Elems);
   end Write_Src_Buf;

   procedure Subtree_To_Ada
     (Tree            : Ada_Tree;
      Cur_Level       : Nesting_Level;
      Index_In_Parent : Query_Index);
   --  We recursively walk the tree, and for most nodes, take the template
   --  from Template_Table, and pass it to Interpret_Template. Some nodes
   --  need special casing, and bypass the Template_Table. Subtree_To_Ada is
   --  directly recursive, and also mutually recursive with Interpret_Template.

   procedure Convert_Tree_To_Ada (Tree : Ada_Tree);
   --  Subtree_To_Ada with initial values for Cur_Level and Index_In_Parent,
   --  along with some fix-ups. In particular, we add a sentinel Line_Break
   --  at the beginning, and a sentinel Tab at the end.

   type Tree_Stack_Index is new Positive;
   type Tree_Array is array (Tree_Stack_Index range <>) of Ada_Tree;
   package Tree_Stacks is new ASIS_UL.Vectors
     (Tree_Stack_Index,
      Ada_Tree,
      Tree_Array);
   use Tree_Stacks;
   --  use all type Tree_Stacks.Vector;

   Tree_Stack : Tree_Stacks.Vector;
   --  Stack of trees that we're in the process of traversing. Pushed and
   --  popped at the beginning and end of Subtree_To_Ada.

   function Ancestor_Tree
     (N    : Tree_Stack_Index)
     return Ada_Tree;
   --  Returns the N'th ancestor of the current tree. Ancestor (0) is the
   --  current tree, Ancestor (1) is the parent of the current tree, Ancestor
   --  (2) is the grandparent of the current tree, and so on. Nil if the tree
   --  isn't deep enough.

   function Ancestor_Tree
     (N    : Tree_Stack_Index)
     return Ada_Tree is
   begin
      if Last_Index (Tree_Stack) <= N then
         return Nil;
      else
         return Tree_Stack (Last_Index (Tree_Stack) - N);
      end if;
   end Ancestor_Tree;

   function Parent_Tree return Ada_Tree is (Ancestor_Tree (1));

   Implicit_Null_Statement_Seen : Boolean := False;
   --  See the comments about labels under "when A_Null_Statement =>" below for
   --  an explanation of this.

   function Munge_Template
     (T    : Ada_Template;
      Kind : Ada_Tree_Kind)
      return Ada_Template;
   --  Modify the template in certain ways based on command-line options and
   --  the like.

   function Subp_Decl_With_Hard_Breaks
     (Tree : Ada_Tree;
      Is_Function, Is_Body : Boolean)
      return                 Ada_Template;
   --  For implementing Par_Specs_Threshold. This replaces the soft line break
   --  between parameters with a hard line break. If Is_Function is True, put
   --  a hard line break before "return". If Is_Body is True, put a hard line
   --  break before "is".

   function Munge_Template
     (T    : Ada_Template;
      Kind : Ada_Tree_Kind)
      return Ada_Template
   is
   begin
      if not Options.RM_Style_Spacing then
         return T;
      end if;
      declare
         Result : Bounded_W_Str (Max_Length => T'Length * 2);
         X      : Natural := T'First;
         function C return W_Char is (T (X));
         function Match
           (S    : Ada_Template)
            return Boolean is
           (T (X .. Natural'Min (T'Last, X + S'Length - 1)) = S);
      begin
         while X <= T'Last loop
            if Options.RM_Style_Spacing then
               if Match (" (") or else Match (" @(") then
                  X := X + 1; -- skip ' ' before '('
               elsif Match (" ^:") and then not Match (" ^:=") then
                  X := X + 1; -- skip ' ' before ':'
               elsif Kind in
                   A_Loop_Statement |
                     A_While_Loop_Statement |
                     A_For_Loop_Statement |
                     A_Block_Statement
                 and then Match (" :")
               then
                  X := X + 1; -- skip ' ' before ':' for statement name
               end if;
            end if;

            Append (Result, C);
            X := X + 1;
         end loop;

         return Ada_Template (To_String (Result));
      end;
   end Munge_Template;

   function Subp_Decl_With_Hard_Breaks
     (Tree : Ada_Tree;
      Is_Function, Is_Body : Boolean)
      return                 Ada_Template
   is
      T : Ada_Template renames Template_Table (Tree.Kind).all;
      T1 : constant W_Str :=
        (if Options.RM_Style_Spacing
           then Must_Replace (W_Str (T), "[@(~;@ ~)]",  "[$(~;$~)]")
           else Must_Replace (W_Str (T), "[@ (~;@ ~)]", "[$(~;$~)]"));
      T2 : constant W_Str :=
        (if Is_Function
           then Must_Replace (T1, "@1 return", "$ return")
           else T1);
      T3 : constant W_Str :=
        (if Is_Body and then Options.Separate_Line_For_IS
          then Must_Replace (T2, "@ is$", "$is$")
          else T2);
   begin
      return Result : constant Ada_Template := Ada_Template (T3) do
         if Assert_Enabled then
            if Result = T then
               Self_Rep.Stdo;
               Self_Rep.Put_Ada_Tree (Tree);
               Wide_Text_IO.Put_Line ("T = " & W_Str (T));
               Wide_Text_IO.Put_Line ("Result = " & W_Str (Result));
            end if;
            pragma Assert (Result /= T);
         end if;
      end return;
   end Subp_Decl_With_Hard_Breaks;

   package Alternative_Templates is

      --  Some templates that are used instead of the ones in Template_Table

      Prefix_Notation_Call_Alt_Templ_1 : constant Ada_Template :=
        Munge_Template (" @(", A_Function_Call);

      Prefix_Notation_Call_Alt_Templ_2 : constant Ada_Template :=
        Munge_Template ("[$(", A_Function_Call);

      Accept_Statement_Alt_Templ : constant Ada_Template :=
        Munge_Template
          (Labels & "accept !? @(~~)~?[ @(~;@ ~)]~!!",
           An_Accept_Statement);
      --  The last "!!" generates nothing, but satisfies the requirement that
      --  we use all the subtrees.

      --  ???Is the following correct for multi-dim arrays (only some indices
      --  need "range")?
      Constrained_Array_Definition_Alt_Templ_1 : constant Ada_Template :=
        Munge_Template
          ("array @(?range ~, range ~~) of !",
           A_Constrained_Array_Definition);

      Constrained_Array_Definition_Alt_Templ_2 : constant Ada_Template :=
        Munge_Template
          ("array @(?~, ~~) of !",
           A_Constrained_Array_Definition);

      Pragma_Alt_Templ : constant Ada_Template :=
        Munge_Template ("?[ @(~,@ ~)]~", Flat_Pragma_Kinds'First);

      Parameter_Specification_Alt_Templ : constant Ada_Template :=
        Munge_Template (" ^: ", A_Parameter_Specification);

      Block_Statement_Alt_Templ_1 : constant Ada_Template :=
        Munge_Template
          (Labels & "?~~ : ~!" & Handled_Seq_2,
           A_Block_Statement);

      Block_Statement_Alt_Templ_2 : constant Ada_Template :=
        Munge_Template
          (Labels & "?~~ : ~?declare$" & "{~;$~;$$}~" & Handled_Seq_2,
           A_Block_Statement);

      Extended_Return_Statement_Alt_Templ : constant Ada_Template :=
        Munge_Template (Labels & "return !!!", An_Extended_Return_Statement);
      --  The last "!!" generates nothing, but satisfies the requirement that
      --  we use all the subtrees.

   end Alternative_Templates;

   procedure Subtree_To_Ada
     (Tree            : Ada_Tree;
      Cur_Level       : Nesting_Level;
      Index_In_Parent : Query_Index)
   is
      pragma Unreferenced (Index_In_Parent); -- ???Needed?

      Line_Breaks : Line_Break_Vector renames All_Line_Breaks;

      procedure Subtrees_To_Ada
        (Tree               : Ada_Tree;
         Pre, Between, Post : Ada_Template);

      procedure Interpret_Template
        (T         : Ada_Template   := Template_Table (Tree.Kind).all;
         Subtrees  : Ada_Tree_Array := Tree.Subtrees;
         Cur_Level : Nesting_Level  := Subtree_To_Ada.Cur_Level;
         Kind      : Ada_Tree_Kind  := Tree.Kind);
      --  Interpret the template, printing literal characters, and recursively
      --  calling Subtree_To_Ada when the template calls for a subnode. Kind is
      --  for debugging.

      procedure Prefix_Notation_Call (Label_Names, Callee, Actuals : Ada_Tree);
      --  This is called for A_Function_Call and A_Procedure_Call_Statement
      --  when the Is_Prefix_Notation subtree is True. Prefix notation calls
      --  have special visibility rules, so we don't want to turn X.F(Y) into
      --  F(X, Y). Label_Names is always empty for function calls.

      procedure Append_Tab
        (Parent, Tree  : Ada_Tree_Base;
         T             : Ada_Template;
         Token_Text    : Name_Id;
         Index_In_Line : Tab_Index_In_Line;
         Is_Insertion_Point : Boolean);
      --  Append a Tab_Rec onto Tabs. If Token is Name_Empty, get the token
      --  from the template T.
      --
      --  Handling of "fake tabs":
      --  Fake tabs are used to deal with situations like this:
      --
      --     A_Long_Var_Name      : T          := 123;
      --     X                    : A_Long_Type_Name;
      --     A_Long_Constant_Name : constant T := 123;
      --
      --  where we wish to align the ":" and ":=" tokens. But the
      --  Insert_Alignment algorithm doesn't align things unless subsequent
      --  lines "match", which includes having the same number of tabs. But X
      --  has no ":=", so we add a fake tab so it will match the preceding and
      --  following lines.
      --
      --  Append_Tab inserts a fake tab after each ":" tab. If there is no
      --  ":=" following, the fake tab remains. If there IS a ":=", a real
      --  tab replaces the fake one.
      --
      --  Fake tabs initially have the same position as the preceding ":" tab.
      --  When Insert_Alignment calculates Max_Col, it ignores the fake ones,
      --  so they won't push anything further to the right. It sets the Col of
      --  the fake ones to Max_Col; hence Num_Blanks will be zero, so fake tabs
      --  won't insert any blanks.
      --
      --  Context clauses are handled in a similar manner:
      --
      --     with Ada.Characters.Handling; use Ada.Characters.Handling;
      --     with Ada.Exceptions;
      --     with Ada.Strings;             use Ada.Strings;

      procedure Append_Tab
        (Parent, Tree  : Ada_Tree_Base;
         T             : Ada_Template;
         Token_Text    : Name_Id;
         Index_In_Line : Tab_Index_In_Line;
         Is_Insertion_Point : Boolean)
      is
         Text : Name_Id;
         Pa              : Ada_Tree_Base := Parent;
         Tr              : Ada_Tree_Base := Tree;

         procedure Maybe_Replace_Fake_Tab;
         --  Replace a fake tab with a real one, if appropriate. In particular,
         --  if the last tab is fake, and the current one has the same
         --  Index_In_Line, Tree, and Parent, then the current one replaces the
         --  fake one.

         function Tab_Token (T : Ada_Template) return Name_Id;
         --  Returns the text of the token at the beginning of T, which is the
         --  portion of an Ada_Template immediately following "^".

         procedure Maybe_Replace_Fake_Tab is
         begin
            if Is_Empty (Tabs) then
               return;
            end if;

            declare
               Tb : constant Tab_Rec := Last_Element (Tabs);
            begin
               if Tb.Is_Fake
                 and then Tb.Index_In_Line = Index_In_Line
                 and then Tb.Tree = Tr
                 and then Tb.Parent = Pa
               then
                  pragma Assert (Tb.Token = Text);
                  pragma Assert
                    ((Text = Name_Assign and then Index_In_Line in 2 | 4)
                     or else
                       (Text = Snames.Name_Use and then Index_In_Line = 2));
                  pragma Assert (not Is_Insertion_Point);
                  Delete_Last (Tabs); -- replace fake tab with this real one
               end if;
            end;
         end Maybe_Replace_Fake_Tab;

         function Tab_Token (T : Ada_Template) return Name_Id is
            --  There is a limited number of possibilities, and we take
            --  advantage of that for efficiency. Currently, the only tokens
            --  that can follow "^" in templates are as shown below. This needs
            --  to be changed if we add more tabbing to templates.
            Tok  : Scanner.Token;
            Text : Name_Id;
         begin
            if T = "" then
               pragma Assert
                 (Tree.Kind in
                    A_Parameter_Specification | A_Formal_Object_Declaration);
               Text := Name_Tab_In_Out;
            else
               case T (T'First) is
                  when ':' =>
                     if Has_Prefix (W_Str (T), Prefix => ":=") then
                        Text := Name_Assign;
                     else
                        Text := Name_Colon;
                     end if;
                  when '|' =>
                     Text := Name_Bar;
                  when '=' =>
                     pragma Assert (Has_Prefix (W_Str (T), Prefix => "=>"));
                     Text := Name_Arrow;
                  when 'a' =>
                     pragma Assert (Has_Prefix (W_Str (T), Prefix => "at"));
                     Text := Snames.Name_At;
                  when 'r' =>
                     pragma Assert (Has_Prefix (W_Str (T), Prefix => "range"));
                     Text := Snames.Name_Range;
                  when '.' =>
                     pragma Assert (Tree.Kind in A_Component_Clause);
                     pragma Assert (Has_Prefix (W_Str (T), Prefix => ".."));
                     Text := Name_Dot_Dot;
                  when ']' =>
                     pragma Assert (Tree.Kind in A_Component_Clause);
                     Text := Name_R_Sq;
                     goto Skip_Assertion; -- ']' is not a legal token
                  when others =>
                     pragma Assert (False);
               end case;
               if Assert_Enabled then
                  Tok := Scanner.Get_Token (W_Str (T));
                  pragma Assert (Text = Tok.Normalized);
                  pragma Assert (Tok.Sloc.First = 1);
               end if;
               <<Skip_Assertion>>
            end if;
            pragma Assert
              (Text in
                 Name_Tab_In_Out |
                 Name_Assign |
                 Name_Colon |
                 Name_Arrow |
                 Name_Bar |
                 Snames.Name_At |
                 Snames.Name_Range |
                 Name_Dot_Dot |
                 Name_R_Sq);
            return Text;
         end Tab_Token;

      --  Start of processing for Append_Tab

      begin
         if not Alignment_Enabled (Options) then
            return;
         end if;

         if Tree /= null and then Tree.Kind = A_With_Clause then
            if Is_Nil (Get (Tree, Has_Limited))
              and then Is_Nil (Get (Tree, Has_Private))
            then
               Pa   := null;
               Tr   := null;
               Text := Snames.Name_With;
            else
               return; -- ignore "limited with" and "private with"
            end if;
         elsif Token_Text = Name_Empty then
            if Is_Insertion_Point then
               Text := Name_Tab_Insertion_Point;
            else
               Text := Tab_Token (T);
            end if;
         else
            Text := Token_Text;
         end if;

         Maybe_Replace_Fake_Tab;

         pragma Assert
           (Point (Out_Buf) =
            Last_Position (Out_Buf) + 1); -- ???Do we need Last_Position?
         Append
           (Tabs,
            Tab_Rec'
              (Pa,
               Tr,
               Token           => Text,
               Mark            => Mark (Out_Buf, '^'),
               Index_In_Line   => Index_In_Line,
               Col             => <>,
               Num_Blanks      => <>,
               Is_Fake         => False,
               Is_Insertion_Point => Is_Insertion_Point));
         pragma Assert
           (Position (Out_Buf, Last_Element (Tabs).Mark) =
            Last_Position (Out_Buf) + 1);

         --  Append a fake tab if appropriate

         if Tree /= null and then not Is_Insertion_Point then
            case Tree.Kind is
               when A_Variable_Declaration |
                 A_Constant_Declaration |
                 An_Integer_Number_Declaration |
                 A_Real_Number_Declaration |
                 A_Discriminant_Specification |
                 A_Component_Declaration |
                 A_Return_Variable_Specification =>
                  if Index_In_Line = 1 then
                     pragma Assert (Text = Name_Colon);
                     Append
                       (Tabs,
                        Tab_Rec'
                          (Parent          => Pa,
                           Tree            => Tr,
                           Token           => Name_Assign,
                           Mark            => Mark (Out_Buf, '^'),
                           Index_In_Line   => 2,
                           Col             => <>,
                           Num_Blanks      => <>,
                           Is_Fake         => True,
                           Is_Insertion_Point => False));
                  end if;

               when A_Parameter_Specification | A_Formal_Object_Declaration =>
                  if Index_In_Line = 3 then
                     pragma Assert (Text = Name_Tab_In_Out);
                     Append
                       (Tabs,
                        Tab_Rec'
                          (Parent          => Pa,
                           Tree            => Tr,
                           Token           => Name_Assign,
                           Mark            => Mark (Out_Buf, '^'),
                           Index_In_Line   => 4,
                           Col             => <>,
                           Num_Blanks      => <>,
                           Is_Fake         => True,
                           Is_Insertion_Point => False));
                  end if;

               when A_With_Clause =>
                  if Index_In_Line = 1 then
                     pragma Assert (Text = Snames.Name_With);
                     Append
                       (Tabs,
                        Tab_Rec'
                          (Parent          => Pa,
                           Tree            => Tr,
                           Token           => Snames.Name_Use,
                           Mark            => Mark (Out_Buf, '^'),
                           Index_In_Line   => 2,
                           Col             => <>,
                           Num_Blanks      => <>,
                           Is_Fake         => True,
                           Is_Insertion_Point => False));
                  end if;

               when A_Variant |
                 An_Aspect_Specification |
                 A_For_All_Quantified_Expression |
                 A_For_Some_Quantified_Expression |
                 An_Assignment_Statement |
                 A_Case_Path |
                 A_Select_Path |
                 An_Or_Path |
                 A_Case_Expression_Path |
                 A_Component_Clause |
                 An_Exception_Handler |
                 An_Exception_Renaming_Declaration |
                 An_Exception_Declaration =>
                  null;

               when A_Pragma_Argument_Association |
                 A_Discriminant_Association       |
                 A_Record_Component_Association   |
                 An_Array_Component_Association   |
                 A_Parameter_Association          |
                 A_Generic_Association            =>
                  null;

               when others =>
                  --  No other tree kinds have tabs
                  pragma Assert (False);
            end case;
         end if;
      end Append_Tab;

      procedure Subtrees_To_Ada
        (Tree               : Ada_Tree;
         Pre, Between, Post : Ada_Template)
      is
         procedure Check_Between;
         --  Assert that Between doesn't contain any indentation or similar, so
         --  we don't need special processing as for Keep_Indentation.

         function Keep_Indentation (Post : Ada_Template) return Ada_Template;
         --  Remove everything from Post except for indentation commands

         procedure Check_Between is
         begin
            for X of Between loop
               if X in '{' | '}' | '[' | ']' | '(' | ')' | '&' |
                 '!' | '?' | '~'
               then
                  Self_Rep.Stdo;
                  Self_Rep.Put_Ada_Tree (Tree);
                  Wide_Text_IO.Put_Line
                    ("Incorrect Between string: " & W_Str (Between));
                  pragma Assert (False);
               end if;
            end loop;
         end Check_Between;

         pragma Debug (Check_Between);

         function Keep_Indentation (Post : Ada_Template) return Ada_Template is
            Result : Bounded_W_Str (Max_Length => Post'Length);
         begin
            for X of Post loop
               pragma Assert (X not in '(' | ')');
               if X in '{' | '}' | '[' | ']' then
                  Append (Result, X);
               end if;
            end loop;
            return Ada_Template (To_String (Result));
         end Keep_Indentation;

         pragma Assert (Tree.Kind in Flat_List_Kinds);
         Prev_With : Ada_Tree_Base := null;
         --  See Use_Same_Line below

      begin
         if Tree.Subtree_Count = 0 then
            return;
         end if;

         Interpret_Template (Pre, Subtrees => Empty_Tree_Array);

         for Index in 1 .. Tree.Subtree_Count loop
            declare
               Subtree : constant Ada_Tree := Tree.Subtrees (Index);

               function Use_Same_Line return Boolean;
               --  Special case for use_package_clauses: We want to print "with
               --  A.B; use A.B;" on one line. Also, things like "with A.B; use
               --  A; use A.B;". This returns True in these cases. We don't do
               --  this special processing for use type clauses.

               function Has_Prefix (X, Y : Ada_Tree) return Boolean with
                  Pre => X.Kind in Usage_Names | A_Selected_Component
                  and then Y.Kind in Usage_Names | A_Selected_Component;
                  --  True if X contains Y, as in "A.B.C.D" contains "A.B".
                  --  I.e. if Y is a prefix of X.

               function Has_Prefix (X, Y : Ada_Tree) return Boolean is
               begin
                  if Ref (X) = Ref (Y) then
                     return True;
                  elsif X.Kind in Usage_Names then
                     return False;
                  else
                     pragma Assert (X.Kind = A_Selected_Component);
                     return Has_Prefix (X.Subtrees (1), Y);
                  end if;
               end Has_Prefix;

               function Use_Same_Line return Boolean is
               begin
                  --  For a with clause followed by one or more use package
                  --  clauses, Prev_With will be the with clause when
                  --  processing the use clauses. Otherwise, Prev_With is null.

                  if Prev_With = null
                    or else Options.Separate_Line_For_USE
                  then
                     return False; -- usual case
                  end if;

                  declare
                     pragma Assert (Prev_With.Kind = A_With_Clause);
                     With_Names : constant Ada_Tree := Prev_With.Subtrees (3);
                     Next_Subtree : constant Ada_Tree :=
                       Tree.Subtrees (Index + 1);
                  begin
                     if Next_Subtree.Kind = A_Use_Package_Clause
                       and then Next_Subtree.Subtrees (1).Subtree_Count = 1
                       and then With_Names.Subtree_Count = 1
                     then
                        declare
                           W : constant Ada_Tree := With_Names.Subtrees (1);
                           U : constant Ada_Tree :=
                             Next_Subtree.Subtrees (1).Subtrees (1);
                        begin
                           if Has_Prefix (W, U) or else Has_Prefix (U, W) then
                              return True;
                           end if;
                        end;
                     end if;
                  end;

                  return False; -- usual case
               end Use_Same_Line;

            begin
               pragma Assert (Tree.Kind not in An_If_Path | An_Elsif_Path);
               --  No need for If_Statement_Check here
               Subtree_To_Ada
                 (Subtree,
                  New_Level (Tree, Index, Cur_Level, Pre & Between & Post),
                  Index);
               --  ???Shouldn't this use the entire template?

               case Subtree.Kind is
                  when A_With_Clause =>
                     if Is_Nil (Get (Subtree, Has_Limited))
                       and then Is_Nil (Get (Subtree, Has_Private))
                     then
                        Prev_With := Subtree;
                     else
                        --  ignore "limited with" and "private with"
                        Prev_With := null;
                     end if;
                  when A_Use_Package_Clause =>
                     null; -- Leave Prev_With alone
                  when others =>
                     Prev_With := null;
               end case;

               if Index < Tree.Subtree_Count then
                  declare
                     Same_Line : constant Boolean := Use_Same_Line;
                     pragma Assert (if Same_Line then Between = ";$");
                     Tween : constant Ada_Template :=
                       (if
                          Same_Line
                        then
                          (if Prev_With = Tree.Subtrees (Index) then ";@ "
                           else ";$")
                        else -- else ";@1 "???
                        Between);
                  begin
                     if Subtree.Kind /= A_Comment then
                        Interpret_Template
                          (Tween, Subtrees => Empty_Tree_Array);
                     end if;
                     if Same_Line then
                        Append_Tab
                          (Parent        => null,
                           Tree          => null,
                           T             => "",
                           Token_Text    => Snames.Name_Use,
                           Index_In_Line => 2,
                           Is_Insertion_Point => False);
                     end if;
                  end;

               else
                  pragma Assert (Index = Tree.Subtree_Count);
                  if Subtree.Kind = A_Comment then
                     Interpret_Template
                       (Keep_Indentation (Post), Subtrees => Empty_Tree_Array);
                  else
                     Interpret_Template (Post, Subtrees => Empty_Tree_Array);
                  end if;
               end if;
            end;
         end loop;
      end Subtrees_To_Ada;

      procedure Interpret_Template
        (T         : Ada_Template   := Template_Table (Tree.Kind).all;
         Subtrees  : Ada_Tree_Array := Tree.Subtrees;
         Cur_Level : Nesting_Level  := Subtree_To_Ada.Cur_Level;
         Kind      : Ada_Tree_Kind  := Tree.Kind)
      is

         pragma Assert (T = Munge_Template (T, Kind));
         J : Positive := T'First;
         subtype Subtrees_Index is Query_Index range 1 .. Subtrees'Last;
         Used : array (Subtrees_Index) of Boolean := (others => False);
         Cur_Subtree_Index : Query_Count                       := 0;
         Numeric_Arg       : Boolean;
         C                 : W_Char;

         function Debug_Template return Name_Id;

         function Debug_Template return Name_Id is
         begin
            if False then
               return W_Name_Find
                   ("X" & W_Str (T) & "X    [" & From_UTF8 (Image (J)) & "]");
            else
               return Name_Empty;
            end if;
         end Debug_Template;

         Nesting_Increment : Nesting_Level;

      --  Start of processing for Interpret_Template

      begin
         while J <= T'Last loop
            Numeric_Arg := False;
            C           := T (J);

            case C is
               --  The following characters are not currently used in templates
               --  (as literal text, or as the initial character of a special
               --  character sequence); reserved for future use.

               when '0' .. '9' |
                 '~'           |
                 '#'           |
                 '*'           |
                 '_'           |
                 '"'           |
                 '\'           |
                 '/'           =>
                  raise Program_Error with "Illegal template character";

               when '$' | '%' =>
                  Append_Line_Break
                    (Hard     => True,
                     Affects_Comments => C = '$',
                     Level    => Cur_Level,
                     Kind     => Kind,
                     Template => Debug_Template);
               when '@' =>
                  if J < T'Last and then T (J + 1) in '0' .. '9' then
                     J                 := J + 1;
                     Nesting_Increment :=
                       Nesting_Level (Char_To_Digit (T (J)));
                  else
                     Nesting_Increment := 0;
                  end if;
                  Append_Line_Break
                    (Hard     => False,
                     Affects_Comments => False,
                     Level    => Cur_Level + Nesting_Increment,
                     Kind     => Kind,
                     Template => Debug_Template);

               when '{' =>
                  Indent (Options.PP_Indentation);
               when '}' =>
                  Indent (-Options.PP_Indentation);

               when '[' =>
                  Indent (Options.PP_Cont_Line_Indentation);
               when ']' =>
                  Indent (-Options.PP_Cont_Line_Indentation);

               when '(' =>
                  Buffered_Output.Put_Char (C);
                  Indent (1); -- extra indentation
               when ')' =>
                  Buffered_Output.Put_Char (C);
                  Indent (-1);

               when '^' | '&' =>
                  declare
                     Index_In_Line : Tab_Index_In_Line;
                     Par           : Ada_Tree := Parent_Tree;
                  begin
                     if J < T'Last and then T (J + 1) in '0' .. '9' then
                        J             := J + 1;
                        Index_In_Line :=
                          Tab_Index_In_Line (Char_To_Digit (T (J)));

                     else
                        Index_In_Line := 1;
                     end if;
                     if Par = Tree then
                        Par := Ancestor_Tree (2); -- up one more level
                     end if;
                     Append_Tab
                       (Par,
                        Tree,
                        T (J + 1 .. T'Last),
                        Name_Empty,
                        Index_In_Line => Index_In_Line,
                        Is_Insertion_Point => C = '&');
                  end;

               when '!' | '?' =>
                  if J < T'Last and then T (J + 1) in '0' .. '9' then
                     Numeric_Arg := True;
                     J           := J + 1;

                  else
                     Cur_Subtree_Index := Cur_Subtree_Index + 1;
                  end if;

                  declare
                     Subtree_Index : Query_Index;

                  begin
                     if Numeric_Arg then
                        Subtree_Index := Query_Index (Char_To_Digit (T (J)));

                     else
                        Subtree_Index := Cur_Subtree_Index;
                     end if;
                     pragma Assert (Subtree_Index in Subtrees_Index);
                     --  Put ("Subtree_Index = \1, not in \2..\3 <<\4>>\n",
                     --  Image (Subtree_Index), Image (Subtrees'First), Image
                     --  (Subtrees'Last), "???Image (Tr.Kind)");

                     declare
                        Subtree : constant Ada_Tree :=
                          Subtrees (Subtree_Index);

                     begin
                        Used (Subtree_Index) := True;
                        if C = '!' then
                           if Tree.Kind in An_If_Path | An_Elsif_Path then
                              pragma Assert (Subtree_Index = 1);
                              If_Statement_Check_1;
                           end if;

                           Subtree_To_Ada
                             (Subtree,
                              New_Level (Tree, Subtree_Index, Cur_Level, T),
                              Subtree_Index);

                           if Tree.Kind in An_If_Path | An_Elsif_Path then
                              If_Statement_Check_2 (Cur_Level);
                           end if;

                        else
                           pragma Assert (C = '?');

                           declare
                              function Scan_To_Tilde return Positive;

                              function Scan_To_Tilde return Positive is
                              begin
                                 loop
                                    J := J + 1;
                                    exit when T (J) = '~';
                                 end loop;
                                 return J - 1;
                              end Scan_To_Tilde;

                              Pre_First : constant Positive := J + 1;
                              Pre_Last  : constant Positive := Scan_To_Tilde;
                              pragma Assert (T (J) = '~');

                              Between_First : constant Positive := J + 1;
                              Between_Last  : constant Positive :=
                                Scan_To_Tilde;
                              pragma Assert (T (J) = '~');

                              Post_First : constant Positive := J + 1;
                              Post_Last  : constant Positive := Scan_To_Tilde;
                              pragma Assert (T (J) = '~');

                           begin
                              Used (Subtree_Index) := True;
                              --  ???The following could use some cleanup
                              case Subtree.Kind is
                                 when Flat_List_Kinds =>
                                    Append (Tree_Stack, Subtree); -- push
                                    Subtrees_To_Ada
                                      (Subtree,
                                       T (Pre_First .. Pre_Last),
                                       T (Between_First .. Between_Last),
                                       T (Post_First .. Post_Last));
                                    Delete_Last (Tree_Stack); -- pop

                                 when Not_An_Element =>
                                    null;

                                 when others =>
                                    Interpret_Template
                                      (T (Pre_First .. Pre_Last),
                                       Subtrees => Empty_Tree_Array);
                                    --  ???
                                    --  if False and then Between /= "" then
                                    --  Put ("\1, \2: ???Between = <<\3>>, " &
                                    --  "T = <<\4>>\n", "???Image (Tr.Kind)",
                                    --  Image (Subtree.Kind), String (Between),
                                    --  String (T)); pragma Assert (Between =
                                    --  ""); end if;
                                    pragma Assert
                                      (Kind not in An_If_Path | An_Elsif_Path);
                                    --  No need for If_Statement_Check here
                                    Subtree_To_Ada
                                      (Subtree,
                                       New_Level
                                         (Tree,
                                          Subtree_Index,
                                          Cur_Level,
                                          T),
                                       Subtree_Index);
                                    Interpret_Template
                                      (T (Post_First .. Post_Last),
                                       Subtrees => Empty_Tree_Array);
                              end case;
                           end;
                        end if;
                     end;
                  end;

               when ';' =>
                  if Implicit_Null_Statement_Seen then
                     Implicit_Null_Statement_Seen := False;

                  else
                     Buffered_Output.Put_Char (C);
                  end if;

               when others =>
                  Buffered_Output.Put_Char (C);

            end case;

            J := J + 1;
         end loop;

         if Used /= (Subtrees_Index => True) then -- ???
            Buffered_Output.Put ("???Not all used: \1", "???Image (Tr.Kind)");
         end if;
         --  ???pragma Assert (Used = (Subtrees_Index => True));
      end Interpret_Template;

      use Alternative_Templates;

      function Past_Call_Threshold (Actuals : Ada_Tree) return Boolean is
         (Natural (Actuals.Subtree_Count) >
            Options.Par_Associations_Threshold
            and then
            (for some Assoc of Actuals.Subtrees =>
               Assoc.Subtrees (1).Kind /= Not_An_Element));
      --  True if there are more parameter associations than the threshold,
      --  and at least one of them is named.

      function Hard_Breaks_For_Call (Kind : Ada_Tree_Kind) return Ada_Template
        is (Ada_Template (Must_Replace
             (W_Str (Template_Table (Kind).all),
              "@ (~,@ ~)", "%(~,%~)")));
      --  We use % instead of $ here, so that the indentation of these will not
      --  affect following comments.

      procedure Prefix_Notation_Call
        (Label_Names, Callee, Actuals : Ada_Tree)
      is

         --  For X.F(Y,Z), which is shorthand for F(X,Y,Z), First is X and Rest
         --  is Y,Z.

         First : constant Ada_Tree := Actuals.Subtrees (1);

         The_Rest : aliased Ada_Tree_Rec :=
             (Kind          => An_Association_List,
              Checks        => Asis.Extensions.Empty_Check_Set,
              Subtree_Count => Actuals.Subtree_Count - 1,
              Sloc          => Asis.Text.Nil_Span,
              Subtrees      => Actuals.Subtrees (2 .. Actuals.Subtree_Count));
         Rest : constant Ada_Tree := The_Rest'Unchecked_Access;
         Past : constant Boolean := Past_Call_Threshold (Rest);

      begin
         if Label_Names.Subtree_Count /= 0 then
            raise Program_Error
              with "labeled prefix calls not yet implemented";
         end if;

         --  ???Work around possible bug in Function_Call_Parameters. Not sure
         --  why Callee would be A_Selected_Component.

         if Callee.Kind /= A_Selected_Component then
            Subtree_To_Ada (First, Cur_Level + 1, Index_In_Parent => 1);
            Buffered_Output.Put (".");
         end if;
         Subtree_To_Ada (Callee, Cur_Level + 1, Index_In_Parent => 2);
         Subtrees_To_Ada
           (Rest,
            Pre     =>
              (if Past
                 then Prefix_Notation_Call_Alt_Templ_2
                 else Prefix_Notation_Call_Alt_Templ_1),
            Between => (if Past then ",$" else ", "),
            Post    => (if Past then ")]" else ")"));
      end Prefix_Notation_Call;

      procedure Maybe_Blank_Line;

      procedure Maybe_Blank_Line is
         Insert_Blank_Line_Before : Boolean := False;
      begin
         if not Options.Insert_Blank_Lines then
            return;
         end if;

         if Tree.Kind = A_Compilation_Unit then
            Insert_Blank_Line_Before := True;
         end if;

         if Tree.Kind in
             An_Ordinary_Type_Declaration |
            --  ???(if rec etc)A_Record_Type_Definition
            --  A_Derived_Record_Extension_Definition

               A_Task_Type_Declaration |
               A_Protected_Type_Declaration |
               A_Single_Task_Declaration |
               A_Single_Protected_Declaration |
               A_Procedure_Body_Declaration |
               A_Function_Body_Declaration |
               A_Package_Declaration | -- ???(non lib unit)
               A_Package_Body_Declaration |
               A_Task_Body_Declaration |
               A_Protected_Body_Declaration |
               An_Entry_Body_Declaration |
               A_Generic_Procedure_Declaration |
               A_Generic_Function_Declaration |
               A_Generic_Package_Declaration |
               An_Enumeration_Type_Definition | --???(if big)
               A_Loop_Statement |
               A_While_Loop_Statement |
               A_For_Loop_Statement |
               A_Block_Statement |
               An_Extended_Return_Statement |
               An_Accept_Statement |
               A_Selective_Accept_Statement |
               A_Timed_Entry_Call_Statement |
               A_Conditional_Entry_Call_Statement |
               An_Asynchronous_Select_Statement |
               An_If_Path | --???look up to If_Statement, then up to list.
               An_Elsif_Path |
               An_Else_Path |
               A_Case_Statement |
               A_Variant_Part |
               A_Case_Path |
               A_Record_Representation_Clause
--           An_Exception_Handler |???

         then
            declare
               Parent : constant Ada_Tree := Parent_Tree;
            begin
               if Parent.Kind in Flat_List_Kinds then
                  if Parent.Subtrees (1) /= Tree then
                     Insert_Blank_Line_Before := True;
                  end if;
               end if;
            end;
         end if;

         if Insert_Blank_Line_Before then
            pragma Assert (Line_Breaks (Last (Line_Breaks)).Hard);
            pragma Assert
              (Point (Out_Buf) =
               Last_Position (Out_Buf) + 1); -- ???Do we need Last_Position?
            pragma Assert
              (Position (Out_Buf, Line_Breaks (Last (Line_Breaks)).Mark) =
               Last_Position (Out_Buf));
            pragma Assert (Lookback (Out_Buf) = NL);
            --  There should already be a hard line break here; we're about to
            --  add another one.

            Append_Line_Break
              (Hard     => True,
               Affects_Comments => False,
               Level    => 0,
               Kind     => Tree.Kind,
               Template => Name_Find ("Maybe_Blank_Line"));
         end if;
      end Maybe_Blank_Line;

      use Asis;
      Index : Query_Index := 1;

      --  Procedures for formatting the various kinds of node that are not
      --  fully covered by Template_Table:

      procedure Do_Accept_Statement;
      procedure Do_Array_Aggregate;
      procedure Do_Association;
      procedure Do_Attribute_Reference;
      procedure Do_Block_Statement;
      procedure Do_Compilation_Unit;
      procedure Do_Comment;
      procedure Do_Case_Path;
      procedure Do_Case_Statement;
      procedure Do_Component_Clause;
      procedure Do_Constrained_Array_Definition; -- also generic formal
      procedure Do_Def_Name;
      procedure Do_Extended_Return_Statement;
      procedure Do_Extension_Aggregate;
      procedure Do_Function_Call;
      procedure Do_List;
      procedure Do_Literal;
      procedure Do_Null_Statement;
      procedure Do_Ordinary_Type_Declaration;
      procedure Do_Parameter_Specification; -- also Formal_Object_Declaration
      procedure Do_Pragma;
      procedure Do_Procedure_Call_Statement; -- also Entry_Call_Statement
      procedure Do_Qualified_Expression;
      procedure Do_Record_Aggregate;
      procedure Do_Single_Task_Declaration;
      procedure Do_Subp_Decl -- subprograms and the like
        (Is_Function, Is_Body : Boolean;
         Params_Query         : Structural_Queries);
      procedure Do_Subtype_Indication;
      procedure Do_Task_Type_Declaration;
      procedure Do_Usage_Name;

      procedure Do_Others; -- anything not listed above

      procedure Do_Accept_Statement is
      begin
         --  If there are no statements or exception handlers, use short form

         if Tree.Subtrees (5).Subtree_Count = 0
           and then Tree.Subtrees (6).Subtree_Count = 0
         then
            Interpret_Template (Accept_Statement_Alt_Templ);
         else
            Interpret_Template;
         end if;
      end Do_Accept_Statement;

      procedure Do_Array_Aggregate is
      begin
         if Parent_Tree.Kind = An_Enumeration_Representation_Clause then
            Interpret_Template ("?[@(~,@ ~)]~");
         else
            Interpret_Template;
         end if;
      end Do_Array_Aggregate;

      procedure Do_Association is
         --  Some have a single name before the "=>", and some have a list
         --  separated by "|". Single_Name_Only is True in the former case.
         --  Positional_Notation is True if there are no names (no "=>").
         --  Single_Name is True if there is a single name before "=>",
         --  regardless of whether a list is allowed.
         Single_Name_Only : constant Boolean :=
           (case Tree.Kind is
              when A_Pragma_Argument_Association |
                A_Parameter_Association |
                A_Generic_Association =>
                True,
              when A_Discriminant_Association |
                A_Record_Component_Association |
                An_Array_Component_Association =>
                False,
              when others => False); -- Can't happen
         Positional_Notation : constant Boolean :=
           (if Single_Name_Only then Tree.Subtrees (1).Kind = Not_An_Element
            else Tree.Subtrees (1).Subtree_Count = 0 -- list length 0?
            );
      begin
         if Positional_Notation then
            Interpret_Template ("?~~~!");
         --  The "?~~~" generates nothing.

         else
            declare
               Single_Name : constant Boolean :=
                 Single_Name_Only or else Tree.Subtrees (1).Subtree_Count = 1;
            begin
               --  This is needed because the "[]" is not properly nested with
               --  the "?~~~".
               if Single_Name then
                  Interpret_Template ("?~~ ^=>[@ ~!]");
               else
                  Interpret_Template ("?~ ^|@ ~ ^=>[@ ~!]");
               end if;
            end;
         end if;
      end Do_Association;

      procedure Do_Attribute_Reference is
         Attribute_Designator_Id : constant String :=
           To_Lower (Get_Name_String (Tree.Subtrees (2).Ref_Name));
      begin
         --  If the Attribute_Designator_Identifier is "Update", then we need
         --  to avoid generating an extra pair of parentheses, because ASIS
         --  represents X'Update(X => Y) as an attribute reference whose
         --  Attribute_Designator_Expressions is a list containing the
         --  aggregate (X => Y), so it would otherwise come out as
         --      X'Update((X => Y)).

         if Attribute_Designator_Id = "update" then
            pragma Assert (Tree.Kind = An_Implementation_Defined_Attribute);
            Interpret_Template ("!'[@!? @~, ~~]");
         else
            Interpret_Template;
         end if;
      end Do_Attribute_Reference;

      procedure Do_Block_Statement is
      begin
         --  If Block_Declarative_Items is empty, leave off the "declare"

         if Tree.Subtrees (3).Subtree_Count = 0 then
            Interpret_Template (Block_Statement_Alt_Templ_1);
         else
            Interpret_Template (Block_Statement_Alt_Templ_2);
         end if;
      end Do_Block_Statement;

      use Buffered_Output;

      procedure Do_Compilation_Unit is
      begin
--          Put ("--  \1 = \2", "Unit_Kind", Capitalize (Tree.Unit_Kind'Img));
--          Interpret_Template ("$", Subtrees => Empty_Tree_Array);
--          Put
--            ("--  \1 = \2",
--             "Unit_Class",
--             Capitalize (Tree.Unit_Class'Img));
--          Interpret_Template ("$", Subtrees => Empty_Tree_Array);
--          Put
--            ("--  \1 = \2",
--             "Unit_Origin",
--             Capitalize (Tree.Unit_Origin'Img));
--          Interpret_Template ("$", Subtrees => Empty_Tree_Array);
--          Interpret_Template ("$", Subtrees => Empty_Tree_Array);
         Subtrees_To_Ada
           (Tree.Subtrees (1),
            Pre     => "",
            Between => ";$",
            Post    => ";$$");
         --  If it's a subunit, we need "separate (Parent.Name)"

         if Tree.Unit_Kind in A_Subunit then
            declare
               N    : constant W_Str := Get_Name_String (Tree.Unit_Full_Name);
               Last : Positive       := N'Last;

            begin
               --  Determine parent name by searching for the last '.'

               while N (Last) /= '.' loop
                  Last := Last - 1;
               end loop;
               Last := Last - 1;

               Put
                 ("separate\1(\2)",
                  (if Options.RM_Style_Spacing then "" else " "),
                  N (1 .. Last));
               Interpret_Template ("$", Subtrees => Empty_Tree_Array);
            end;
         end if;

         case Tree.Unit_Class is
            when A_Private_Declaration =>
               Put ("private ");

            when A_Public_Declaration       |
              A_Public_Body                 |
              A_Public_Declaration_And_Body |
              A_Private_Body                |
              A_Separate_Body               =>
               null;

            when Not_A_Class =>
               raise Program_Error;
         end case;

         Subtree_To_Ada
           (Tree.Subtrees (2),
            Cur_Level + 1,
            Index_In_Parent => 2);
         Put (";");
         Interpret_Template ("$", Subtrees => Empty_Tree_Array);
         Subtrees_To_Ada
           (Tree.Subtrees (3),
            Pre     => "",
            Between => ";$",
            Post    => ";$");
      end Do_Compilation_Unit;

      procedure Do_Comment is
         S : constant W_Str := Get_Name_String (Tree.Text);
         pragma Assert (S in Scanner.Gen_Plus | Scanner.Gen_Minus);
         --  These are the only ones used, for now.
         Gen_Indent : constant Natural :=
           Good_Column (Options.PP_Indentation,
                        Options.Max_Line_Length - Cur_Indentation - S'Length);
         pragma Assert ((Gen_Indent mod Options.PP_Indentation) = 0);
      begin
         pragma Assert (Check_Whitespace);
         Check_Whitespace := False;
         Interpret_Template
           ((1 .. Gen_Indent => ' '),
            Subtrees => Empty_Tree_Array);
         Interpret_Template
           (Ada_Template (S),
            Subtrees => Empty_Tree_Array);
         Check_Whitespace := True;
         Interpret_Template ("$", Subtrees => Empty_Tree_Array);
         if S = Scanner.Gen_Minus then
            Interpret_Template ("$", Subtrees => Empty_Tree_Array);
         end if;
      end Do_Comment;

      procedure Do_Case_Path is
         Stms : constant Ada_Tree := Tree.Subtrees (2);

      begin
         --  If the statement list is a single block statement that starts on
         --  the same line as the "when", then we assume the user wants to keep
         --  it that way. For example:
         --
         --     when Upper_Case => Upper_Case_Case : begin

         if Stms.Subtree_Count = 1
           and then Stms.Subtrees (1).Kind = A_Block_Statement
           and then Stms.Subtrees (1).Sloc.First_Line = Tree.Sloc.First_Line
         then
            Interpret_Template ("when ?[@~ |@ ~]~ => " & "?~~;$~");

         else
            Interpret_Template;
         end if;
      end Do_Case_Path;

      procedure Do_Case_Statement is
         --  If all the "when"s appear in the same column as "case", then we
         --  assume that's what the user intended, and avoid indenting the
         --  "when"s. ???But the old gnatpp doesn't do that, so disable it
         --  for now.

         Case_Col : constant Positive := Tree.Sloc.First_Column;
         --  Column in which "case" appears
         Whens_Col : Positive :=
           Tree.Subtrees (3).Subtrees (1).Sloc.First_Column;
      --  Column in which all the "when"s appear, if they're all the same

      begin
         for W of Tree.Subtrees (3).Subtrees loop
            if W.Sloc.First_Column /= Whens_Col then
               Whens_Col := Positive'Last; -- not all the same
            end if;
         end loop;

         Whens_Col := Positive'Last; -- ???disable for now
         if Case_Col = Whens_Col and then Case_Col /= 1 then
            Interpret_Template (Labels & "case[@ !]@ is$" & "!" & "end case");

         else
            Interpret_Template;
         end if;
      end Do_Case_Statement;

      procedure Do_Component_Clause is
         --  We use "&" to right-justify the three expressions X, Y, and Z in
         --  "at X range Y .. Z". We need to lift the Y and Z expressions up so
         --  they appear at the same level as X, so the Tree and Parent of the
         --  "&" will match that of the following "^". The Index_In_Lines must
         --  also match. The end result will be something like:
         --     Thing   at 0 range   0 ..  127;
         --     Thing_2 at 0 range 128 .. 1023;

         pragma Assert
           (Tree.Subtrees (3).Kind = A_Discrete_Simple_Expression_Range);
         Subtrees : constant Ada_Tree_Array :=
           Tree.Subtrees (1 .. 2) & Tree.Subtrees (3).Subtrees;
         pragma Assert (Subtrees'Last = 4);
         Cc_Templ : constant Ada_Template :=
           "! ^at &2! ^2range [@&3! ^3..[@ &4!^4]]";
      begin
         Interpret_Template (Cc_Templ, Subtrees);
      end Do_Component_Clause;

      procedure Do_Constrained_Array_Definition is
      begin
         case Tree.Subtrees (1).Subtrees (1).Kind is
            when A_Range_Attribute_Reference | A_Simple_Expression_Range =>
               Interpret_Template (Constrained_Array_Definition_Alt_Templ_1);
            when others =>
               Interpret_Template (Constrained_Array_Definition_Alt_Templ_2);
         end case;
      end Do_Constrained_Array_Definition;

      procedure Do_Def_Name is
         Kind : Ada_Tree_Kind;
      begin
         if Tree.Kind = A_Defining_Expanded_Name then
            Interpret_Template ("![@.!]");
         else
            --  Odd special case for task and protected bodies: If we have
            --  "task body T is...", what casing rule should be used for "T"?
            --  If the spec is a task type declaration, we should use the rule
            --  for types, but if it's a single task declaration, we should use
            --  the rule for other names. This is only relevant if
            --  PP_Type_Casing /= PP_Name_Casing, which is hardly ever the
            --  case.

            if Decl_Of_Def (Symtab, Tree).Kind in
              A_Task_Body_Declaration | A_Protected_Body_Declaration
            then
               Kind := Decl_Of_Def_Kind (Symtab, Spec_Of_Body (Symtab, Tree));
            else
               Kind := Decl_Of_Def_Kind (Symtab, Tree);
            end if;

            Put ("\1",
                 Id_With_Casing (Tree.Def_Name, Kind, Is_Predef => False));
         end if;
      end Do_Def_Name;

      procedure Do_Extended_Return_Statement is
      begin
         --  If there are no statements or exception handlers, use short form

         if Tree.Subtrees (3).Subtree_Count = 0
           and then Tree.Subtrees (4).Subtree_Count = 0
         then
            Interpret_Template (Extended_Return_Statement_Alt_Templ);
         else
            Interpret_Template;
         end if;
      end Do_Extended_Return_Statement;

      procedure Do_Extension_Aggregate is
      begin
         if Tree.Subtrees (2).Subtree_Count = 0 then
            Interpret_Template ("@(! with @" & "null record)!");

         else
            Interpret_Template;
         end if;
      end Do_Extension_Aggregate;

      type Precedence_Level is range 1 .. 7;
      function Precedence (Expr : Ada_Tree) return Precedence_Level;

      function Precedence (Expr : Ada_Tree) return Precedence_Level is
      begin
         case Expr.Kind is
--  ???Don't treat membership tests as operators, for now
--            when An_In_Membership_Test | A_Not_In_Membership_Test =>
--               return 1;
            when An_And_Then_Short_Circuit | An_Or_Else_Short_Circuit =>
               return 2;

            when A_Function_Call =>
               --  Binary operator using operator notation

               if Expr.Subtrees (3).Kind /= An_Is_Prefix_Call
                 and then Expr.Subtrees (2).Subtree_Count /= 1
               then
                  pragma Assert
                    (Expr.Subtrees (4).Kind /= An_Is_Prefix_Notation);
                  pragma Assert (Expr.Subtrees (2).Subtree_Count = 2);
                  case Expr.Subtrees (1).Kind is
                     when An_And_Operator | An_Or_Operator | An_Xor_Operator =>
                        return 2; -- same as 'and then' and 'or else'

                     when An_Equal_Operator             |
                       A_Not_Equal_Operator             |
                       A_Less_Than_Operator             |
                       A_Less_Than_Or_Equal_Operator    |
                       A_Greater_Than_Operator          |
                       A_Greater_Than_Or_Equal_Operator =>
                        return 3;

                     when A_Plus_Operator     |
                       A_Minus_Operator       |
                       A_Concatenate_Operator =>
                        return 4;

                     when A_Multiply_Operator |
                       A_Divide_Operator      |
                       A_Mod_Operator         |
                       A_Rem_Operator         =>
                        return 5;

                     when An_Exponentiate_Operator =>
                        return 6;

                     when others =>
                        raise Program_Error;
                  end case;

               --  Unary operator or normal function-call notation

               else
                  return 7;
               end if;

            --  Assume anything else is a primary (highest precedence)

            when others =>
               return 7;
         end case;
      end Precedence;

      function Get_Arg (Expr : Ada_Tree; N : Query_Index) return Ada_Tree;

      function Get_Arg (Expr : Ada_Tree; N : Query_Index) return Ada_Tree is
         Assoc : constant Ada_Tree := Expr.Subtrees (2).Subtrees (N);
         pragma Assert (Assoc.Kind = A_Parameter_Association);
         function Is_Positional
           (Assoc : Ada_Tree)
            return  Boolean is
           (Assoc.Subtrees (1).Kind = Not_An_Element);
         pragma Assert (Is_Positional (Assoc));

      begin
         return Assoc.Subtrees (2);
      end Get_Arg;

      function Make_Op (Expr : Ada_Tree) return Ada_Tree;
      --  Create operator node. This is a separate function to reduce stack
      --  usage (for example long strings of "&" can cause deep recursion).

      function Make_Op (Expr : Ada_Tree) return Ada_Tree is
      begin
         return Result : constant Ada_Tree := Make (An_Identifier) do
            case Expr.Kind is
               when A_Function_Call =>
                  declare
                     Q_Op_Sym : constant String :=
                       To_Lower (Get_Name_String (Expr.Subtrees (1).Ref_Name));
                     Un_Q : constant String (1 .. Q_Op_Sym'Length - 2) :=
                       Q_Op_Sym (2 .. Q_Op_Sym'Last - 1);
                  --  Strip off quotes
                  begin
                     Result.Ref := Name_Find (Un_Q);
                  end;

               when An_And_Then_Short_Circuit =>
                  Result.Ref := Name_And_Then;

               when An_Or_Else_Short_Circuit =>
                  Result.Ref := Name_Or_Else;

               when others =>
                  raise Program_Error;
            end case;
            Result.Ref_Name := Result.Ref;
         end return;
      end Make_Op;

      procedure Do_Unary_Operator (Expr : Ada_Tree);

      procedure Do_Binary_Operator
        (Expr      : Ada_Tree;
         Is_Right  : Boolean;
         Cur_Level : Nesting_Level);
      --  Also handles some things that look like operators, like "and then".
      --  Is_Right is True if Expr is the right-hand argument of an outer
      --  binary operator. Otherwise (Expr is the left-hand argument, or Expr's
      --  parent is something else, like a parenthesized expression), Is_Right
      --  is False.

      function Is_Bin_Op (Expr : Ada_Tree) return Boolean;

      procedure Do_Unary_Operator (Expr : Ada_Tree) is
         Op       : constant Ada_Tree       := Make_Op (Expr);
         Arg1     : constant Ada_Tree       := Get_Arg (Expr, 1);
      begin
         --  First we have a special case for the Depends and Refined_Depends
         --  aspect specifications. We want to pretend that "=>+" is an
         --  operator, so we print: "Depends => (A =>+ B)" instead of
         --  "Depends => (A => +B)". We don't bother with this for pragma
         --  [Refined_]Depends, because that's mainly for the compiler's
         --  implementation of the aspect, so we don't expect it to be used
         --  much.

         if Ancestor_Tree (4).Kind = An_Aspect_Specification
           and then Ancestor_Tree (4).Subtrees (1).Ref_Name in
                      Name_Depends | Name_Refined_Depends
         then
            pragma Assert (Expr.Subtrees (1).Kind = A_Unary_Plus_Operator);
            pragma Assert
              (Slice (Out_Buf, Point (Out_Buf) - 4, Point (Out_Buf) - 1)
                 = " => ");
            declare
               Subtrees : constant Ada_Tree_Array := (1 => Arg1);
            begin
               Replace_Previous (Out_Buf, '+');
               Interpret_Template (" !", Subtrees);
            end;

         --  No special "[Refined_]Depends" case. Put a space after the
         --  operator, except for "+" and "-".

         else
            declare
               Subtrees : constant Ada_Tree_Array := (Op, Arg1);
            begin
               if Expr.Subtrees (1).Kind in
                 A_Unary_Plus_Operator | A_Unary_Minus_Operator
               then
                  Interpret_Template ("!!", Subtrees);
               else
                  Interpret_Template ("! !", Subtrees);
               end if;
            end;
         end if;
      end Do_Unary_Operator;

      function Is_Bin_Op (Expr : Ada_Tree) return Boolean is
      begin
         case Expr.Kind is
            when A_Function_Call =>
               return Expr.Subtrees (3).Kind /= An_Is_Prefix_Call
                 and then Expr.Subtrees (2).Subtree_Count = 2;

            when An_And_Then_Short_Circuit | An_Or_Else_Short_Circuit =>
               return True;

            when others =>
               return False;
         end case;
      end Is_Bin_Op;

      procedure Do_Binary_Operator
        (Expr      : Ada_Tree;
         Is_Right  : Boolean;
         Cur_Level : Nesting_Level)
      is
         Is_Short_C : constant Boolean :=
           Expr.Kind in An_And_Then_Short_Circuit | An_Or_Else_Short_Circuit;
         Is_Expon : constant Boolean := -- True for "**"
           (Expr.Kind in A_Function_Call
            and then Expr.Subtrees (1).Kind = An_Exponentiate_Operator);
         Op          : constant Ada_Tree := Make_Op (Expr);
         Arg1, Arg2  : Ada_Tree;
         Arg1_Higher : Boolean; -- Arg1 is higher precedence than Expr

      --  Calculate template fragments for the args (Arg1/2_T), that indent
      --  if the arg is a higher precedence binary operator than the whole
      --  expression.

      --  Start of processing for Do_Binary_Operator

      begin
         if Is_Short_C then
            Arg1 := Expr.Subtrees (1);
            Arg2 := Expr.Subtrees (2);

         else -- function call
            Arg1 := Get_Arg (Expr, 1);
            Arg2 := Get_Arg (Expr, 2);
         end if;

         --  The arguments can't have lower precedence than the expression as
         --  a whole; that's what precedence means -- you need parens to put
         --  a "+" inside a "*". The right-hand argument can't have equal
         --  precedence, because Ada has no right-associative binary operators.

         pragma Assert (Precedence (Arg1) >= Precedence (Expr));
         pragma Assert (Precedence (Arg2) > Precedence (Expr));

         Arg1_Higher := Precedence (Arg1) > Precedence (Expr);

         --  The recursive calls to Do_Binary_Operator below bypass the
         --  normal recursion via Subtree_To_Ada, so we need to pass along the
         --  Cur_Level to Interpret_Template. When we reach something that's
         --  not a binary op, we switch back to the normal recursion via
         --  Interpret_Template on the Arg. We split lines after the
         --  operator symbol, as in:
         --     Some_Long_Thing +
         --     Some_Other_Long_Thing
         --  except in the case of short circuits:
         --     Some_Long_Thing
         --     and then Some_Other_Long_Thing
         --  The --split-line-before-op switch causes all operators to be
         --  treated like short circuits in this regard.
         --
         --  All operators are surrounded by blanks, except for "**":
         --     Max : constant := 2**31 - 1;

         if Is_Bin_Op (Arg1) then
            if Is_Right and then Arg1_Higher then
               Interpret_Template ("[@", Empty_Tree_Array, Cur_Level);
            end if;
            Do_Binary_Operator
              (Arg1,
               Is_Right  => Is_Right,
               Cur_Level => Cur_Level + (if Arg1_Higher then 1 else 0));
            if Is_Right and then Arg1_Higher then
               Interpret_Template ("]", Empty_Tree_Array, Cur_Level);
            end if;

         else
            Interpret_Template
              ("!",
               Subtrees  => (1 => Arg1),
               Cur_Level => Cur_Level);
         end if;

         if Is_Short_C or Options.Split_Line_Before_Op then
            Interpret_Template ("@", Empty_Tree_Array, Cur_Level);
         end if;
         Interpret_Template
           ((if Is_Expon then "!" else " ! "), -- no blanks for "**"
            Subtrees  => (1 => Op),
            Cur_Level => Cur_Level);
         if not (Is_Short_C or Options.Split_Line_Before_Op) then
            Interpret_Template ("@", Empty_Tree_Array, Cur_Level);
         end if;

         if Is_Bin_Op (Arg2) then
            Interpret_Template ("[@", Empty_Tree_Array, Cur_Level + 1);
            Do_Binary_Operator
              (Arg2,
               Is_Right  => True,
               Cur_Level => Cur_Level + 1);
            Interpret_Template ("]", Empty_Tree_Array, Cur_Level + 1);

         else
            Interpret_Template
              ("!",
               Subtrees  => (1 => Arg2),
               Cur_Level => Cur_Level + 1);
         end if;
      end Do_Binary_Operator;

      procedure Do_Function_Call is
      begin
         --  Note: Is_Prefix_Notation is for Object.Operation(...) notation,
         --  whereas Is_Prefix_Call is for anything that's not an operator
         --  notation call. Thus Is_Prefix_Call is True for "&"(X, Y), and
         --  False for X&Y.

         if Tree.Subtrees (4).Kind = An_Is_Prefix_Notation then
            pragma Assert (Tree.Subtrees (3).Kind = An_Is_Prefix_Call);
            Prefix_Notation_Call
              (Label_Names => Empty (A_Defining_Name_List),
               Callee      => Tree.Subtrees (1),
               Actuals     => Tree.Subtrees (2));

         --  Determine whether to use operator notation, like X+Y instead of
         --  "+"(X,Y). We can use operator notation if it's an operator call,
         --  and the argument(s) are in positional notation (not named). ???We
         --  must use operator notation for "/=", to work around compiler bug.
         --  In some cases, "/="(X, Y) doesn't work (on access types?), so we
         --  generate (X /= Y) instead.

         --  We don't want to translate "&" (STRING'("AB"), STRING'("CDEF"))(5)
         --  /= CHARACTER'('E') into ((STRING'("AB") & STRING'("CDEF"))(5)
         --  /= CHARACTER'('E')) because an operator-notation call is not a
         --  name, and therefore cannot be used as the prefix of an indexed
         --  component.

         elsif Tree.Subtrees (3).Kind = An_Is_Prefix_Call then
            if Past_Call_Threshold (Tree.Subtrees (2)) then
               Interpret_Template (Hard_Breaks_For_Call (Tree.Kind));
            else
               Interpret_Template; -- normal "F (X)" notation
            end if;

         --  Operator notation:

         else
            pragma Assert
              (Tree.Subtrees (1).Kind in Flat_Operator_Symbol_Kinds);
            pragma Assert (Tree.Subtrees (2).Subtree_Count in 1 .. 2);

            --  Unary operator

            if Tree.Subtrees (2).Subtree_Count = 1 then
               Do_Unary_Operator (Tree);

            --  Binary operator

            else
               Do_Binary_Operator
                 (Tree,
                  Is_Right  => False,
                  Cur_Level => Cur_Level);
            end if;
         end if;
      end Do_Function_Call;

      procedure Do_List is
      --  This formats the list elements with a hard line break in between. It
      --  is called when a "!" in a template refers to a list subtree. If you
      --  don't want this formatting, you must use "?" instead of "!". See,
      --  for example, the template for An_If_Expression, where we want soft
      --  line breaks in between paths. Sometimes this is called for a list
      --  of one element, in which case the Between doesn't matter (e.g.
      --  Defining_Name_List, where there is only one).
      begin
         Subtrees_To_Ada (Tree, Pre => "", Between => "$", Post => "");
      end Do_List;

      procedure Do_Literal is
         S : constant W_Str := Get_Name_String (Tree.Lit_Val);

         function Last_Digit
           (First : Positive; Based : Boolean) return Positive;
         --  Returns the index of the last digit in S starting at
         --  First

         procedure Put_With_Underscores
           (Part : W_Str; Grouping : Positive; Int : Boolean);
         --  Part is the integer part (before the '.', if any) or the
         --  fractional part (after the '.'). Int is True for the integer part.
         --  For example, for "16#12345.67890#e2", this will be called for Part
         --  = "12345" and Int = True, then for Part = "67890" and Int = False.
         --  We want to get "16#1_2345.6789_0#e2" (assuming Grouping = 4).

         procedure Put_With_Underscores
           (Part : W_Str; Grouping : Positive; Int : Boolean)
         is
            Count : Natural := (if Int then Part'Length else 0);
            Inc : constant Integer := (if Int then -1 else 1);
            --  For the integer part, we count downward from the Length; for
            --  the fractional part, we count upward from zero. If Count is
            --  divisible by Grouping, the next character should be preceded by
            --  an underscore, except there is never a leading underscore.
         begin
            for J in Part'Range loop
               if J /= Part'First and then Count mod Grouping = 0 then
                  Put_Char ('_');
               end if;
               Put_Char (Part (J));
               Count := Count + Inc;
            end loop;
         end Put_With_Underscores;

         function Last_Digit
           (First : Positive; Based : Boolean) return Positive
         is
         begin
            for J in First .. S'Last loop
               if Is_Digit (S (J)) then
                  null;
               elsif Based and then Is_Letter (S (J)) then
                  null;
               else
                  return J - 1;
               end if;
            end loop;
            return S'Last;
         end Last_Digit;

      --  Start of processing for Do_Literal

      begin
         pragma Assert (Check_Whitespace);
         Check_Whitespace := False;

         --  In most cases, we simply print out S. All of the complicated code
         --  below is for the --decimal-grouping and --based-grouping
         --  switches. If --decimal-grouping was used to specify a nonzero
         --  value, and we have a numeric literal without a base, and that
         --  literal contains no underscores, we insert underscores. Similarly
         --  for --based-grouping. A based literal is one containing "#" or
         --  ":"; note that "10#...#" is considered based, not decimal.

         case Tree.Kind is
            when A_String_Literal =>
               Put ("\1", S);

            when An_Integer_Literal | A_Real_Literal =>
               if Options.Decimal_Grouping = 0
                 and then Options.Based_Grouping = 0
               then
                  Put ("\1", S);
               else
                  declare
                     Sharp : constant Natural :=
                       (if Find (S, "#") /= 0 then Find (S, "#")
                        else Find (S, ":"));
                     Underscore : constant Natural := Find (S, "_");

                     Grouping : constant Natural :=
                       (if Underscore /= 0 then 0
                        elsif Sharp = 0 then Options.Decimal_Grouping
                        else Options.Based_Grouping);

                     Int_First, Int_Last, Frac_First, Frac_Last : Natural;
                     --  These point to the slices of the literal that should
                     --  have underscores inserted. For example:
                     --     For 12345 or 12345E6:
                     --       S (Int_First .. Int_Last) = "12345"
                     --     For 12345.6789 or 16#12345.6789#E-3:
                     --       S (Int_First .. Int_Last) = "12345", and
                     --       S (Frac_First .. Frac_Last) = "6789"
                  begin
                     if Grouping = 0 then
                        Put ("\1", S);
                     else
                        Int_First := Sharp + 1;
                        Int_Last :=
                          Last_Digit (Int_First, Based => Sharp /= 0);
                        Put ("\1", S (1 .. Sharp));
                        Put_With_Underscores
                          (S (Int_First .. Int_Last),
                           Grouping, Int => True);
                        if Tree.Kind = An_Integer_Literal then
                           Put ("\1", S (Int_Last + 1 .. S'Last));
                        else
                           Frac_First := Int_Last + 2; -- skip '.'
                           Frac_Last := Last_Digit
                             (Frac_First, Based => Sharp /= 0);
                           pragma Assert
                             (S (Int_Last + 1 .. Frac_First - 1) = ".");
                           Put_Char ('.');
                           Put_With_Underscores
                             (S (Frac_First .. Frac_Last),
                              Grouping, Int => False);
                           Put ("\1", S (Frac_Last + 1 .. S'Last));
                        end if;
                     end if;
                  end;
               end if;

            when others => raise Program_Error;
         end case;

         Check_Whitespace := True;
      end Do_Literal;

      procedure Do_Null_Statement is
      begin
         --  If a label comes at the end of a statement list, as allowed in Ada
         --  2012, ASIS inserts an extra implicit null statement to hang the
         --  label off of. We don't want to print that statement, because
         --  it wasn't in the source code. We can detect such implicit null
         --  statements by checking for a nil Sloc. We also need to suppress
         --  the ";" that comes after the implicit 'null', which is the purpose
         --  of Implicit_Null_Statement_Seen. We set that flag True here, and
         --  the very next template character seen by Interpret_Template will
         --  be that ";", so Interpret_Template will suppress the ";" and reset
         --  Implicit_Null_Statement_Seen to False.

         if Tree.Subtrees (1).Subtree_Count /= 0
           and then Asis.Text.Is_Nil (Tree.Sloc)
         then
            Interpret_Template (Labels);
            Implicit_Null_Statement_Seen := True;

         else
            Interpret_Template;
         end if;
      end Do_Null_Statement;

      procedure Do_Ordinary_Type_Declaration is
      begin
         if Tree.Subtrees (3).Kind in
             A_Derived_Record_Extension_Definition |
               A_Record_Type_Definition |
               A_Tagged_Record_Type_Definition |
               An_Access_To_Procedure |
               An_Access_To_Protected_Procedure |
               An_Access_To_Function |
               An_Access_To_Protected_Function
         then
            Interpret_Template ("type !! is !" & Aspects);
         --  Record_Definition or other subtree will take care of new lines.
         --  ???It might be better to have a *weak* newline, though.
         else
            Interpret_Template;
         end if;
      end Do_Ordinary_Type_Declaration;

      procedure Do_Others is
      begin
         if Template_Table (Tree.Kind) = null then
--            Put ("null templ:\1", Image (Tree.Kind));
            Subtrees_To_Ada (Tree, Pre => "{", Between => "|", Post => "}");
            raise Program_Error;
         else
            Interpret_Template;
         end if;
      end Do_Others;

      procedure Do_Parameter_Specification is
      begin
         Subtrees_To_Ada
           (Tree.Subtrees (Index),
            Pre     => "",
            Between => ",@ ",
            Post    => "");
         Interpret_Template
           (Parameter_Specification_Alt_Templ,
            Subtrees => Empty_Tree_Array);

         case Tree.Kind is
            when A_Parameter_Specification =>
               Index := Index + 1;

               if Tree.Subtrees (Index).Kind /=
                 Not_An_Element
               then -- "aliased"
                  Subtree_To_Ada (Tree.Subtrees (Index), Cur_Level + 1, Index);
                  Put (" ");
               end if;

            when A_Formal_Object_Declaration =>
               null; -- A_Formal_Object_Declaration doesn't have "aliased"

            when others =>
               raise Program_Error;
         end case;

         if Tree.Mode in An_In_Mode | An_In_Out_Mode then
            Put ("in ");
         end if;
         Interpret_Template ("^2", Subtrees => Empty_Tree_Array);
         if Tree.Mode in An_Out_Mode | An_In_Out_Mode then
            Put ("out ");
         end if;
         Interpret_Template ("^3", Subtrees => Empty_Tree_Array);

         Index := Index + 1;

         if Tree.Subtrees (Index).Kind /= Not_An_Element then -- "not null"
            Subtree_To_Ada (Tree.Subtrees (Index), Cur_Level + 1, Index);
            Put (" ");
         end if;

         Index := Index + 1;
         Subtree_To_Ada (Tree.Subtrees (Index), Cur_Level + 1, Index);

         Index := Index + 1;
         if Tree.Subtrees (Index).Kind /= Not_An_Element then
            Interpret_Template
              (" ^4:=[@ !]",
               Subtrees => (1 => Tree.Subtrees (Index)));
         end if;
      end Do_Parameter_Specification;

      procedure Do_Pragma is
      begin
         Put
           ("pragma \1",
            Id_With_Casing (Tree.Pragma_Name, Tree.Kind, Is_Predef => False));
         Interpret_Template (Pragma_Alt_Templ);
      end Do_Pragma;

      procedure Do_Procedure_Call_Statement is
      begin
         if Tree.Kind = A_Procedure_Call_Statement
           and then Tree.Subtrees (4).Kind = An_Is_Prefix_Notation
         then
            Prefix_Notation_Call
              (Label_Names => Tree.Subtrees (1),
               Callee      => Tree.Subtrees (2),
               Actuals     => Tree.Subtrees (3));
         elsif Past_Call_Threshold (Tree.Subtrees (3)) then
            Interpret_Template (Hard_Breaks_For_Call (Tree.Kind));
         else
            Interpret_Template;
         end if;
      end Do_Procedure_Call_Statement;

      procedure Do_Qualified_Expression is
      begin
         if Tree.Subtrees (2).Kind in
             A_Record_Aggregate |
               An_Extension_Aggregate |
               A_Positional_Array_Aggregate |
               A_Named_Array_Aggregate
         then
            Interpret_Template ("!'[@!]");
         --  If the thing after the ' is an aggregate, we leave out the
         --  parentheses here, because the aggregate will insert them. We
         --  want T'(X, Y, Z), not T'((X, Y, Z)).

         else
            Interpret_Template;
         end if;
      end Do_Qualified_Expression;

      procedure Do_Record_Aggregate is
      begin
         if Tree.Subtrees (1).Subtree_Count = 0 then
            Interpret_Template ("@(null record)!");
         else
            Interpret_Template;
         end if;
      end Do_Record_Aggregate;

      procedure Do_Single_Task_Declaration is
      begin
         --  For single task declarations, use short form if
         --  Object_Declaration_View is Nil

         if Is_Nil (Tree.Subtrees (4)) then
            Interpret_Template ("task !" & Aspects & "!!");

         else
            Interpret_Template;
         end if;
      end Do_Single_Task_Declaration;

      procedure Do_Subp_Decl
        (Is_Function, Is_Body : Boolean;
         Params_Query         : Structural_Queries)
         --  Params_Query is the query for getting the formal parameters
      is
         --  This is for subprogram declarations and the like -- everything
         --  that has a formal parameter list.

         Param_Count : constant Query_Count :=
           Get (Tree, Params_Query).Subtree_Count +
           Boolean'Pos (Is_Function); -- Add one extra for function result
      begin
         if (Options.Par_Specs_Threshold = 0
               and then Options.Separate_Line_For_IS)
            or else Param_Count > Query_Count (Options.Par_Specs_Threshold)
         then
            Interpret_Template
              (Subp_Decl_With_Hard_Breaks (Tree, Is_Function, Is_Body));
         else
            Interpret_Template;
         end if;
      end Do_Subp_Decl;

      procedure Do_Subtype_Indication is
      begin
         if Tree.Subtrees (4).Kind in
             A_Range_Attribute_Reference |
               A_Simple_Expression_Range
         then
            Interpret_Template ("?~~ ~?~~ ~!? range ~~~");
         elsif Options.RM_Style_Spacing
           and then Tree.Subtrees (4).Kind = An_Index_Constraint
         then
            Interpret_Template ("?~~ ~?~~ ~!?~~~");
         else
            Interpret_Template ("?~~ ~?~~ ~!? ~~~");
         end if;
      end Do_Subtype_Indication;

      procedure Do_Task_Type_Declaration is
      begin
         --  For task type declarations, use short form if
         --  Type_Declaration_View is Nil

         if Is_Nil (Tree.Subtrees (5)) then
            Interpret_Template ("task type !!" & Aspects & "!!");

         else
            Interpret_Template;
         end if;
      end Do_Task_Type_Declaration;

      procedure Do_Usage_Name is
         --  The following works around a compiler limitation related to
         --  'Elab_Spec and 'Elab_Body attributes. For something like
         --  "Ada.Text_IO'Elab_Spec", the compiler does not analyze the prefix
         --  "Ada.Text_IO", so it looks like a name that doesn't denote
         --  anything, like an identifier specific to a pragma. Setting
         --  Elab_Spec_Seen to True tells Id_With_Casing to treat it like a
         --  normal name (it really DOES denote something).
         Elab_Spec_Seen : Boolean          := False;
         N              : Tree_Stack_Index := Last_Index (Tree_Stack);
         P              : Ada_Tree_Base;
         A              : Name_Id;
      begin
         while N > 1 and then Tree_Stack (N - 1).Kind = A_Selected_Component
         loop
            N := N - 1;
         end loop;
         if N > 1 then
            P := Tree_Stack (N - 1);
            if P.Kind = An_Implementation_Defined_Attribute then
               A := P.Subtrees (2).Ref_Name;
               if
                 (A = Name_Find ("Elab_Spec")
                  or else A = Name_Find ("Elab_Body"))
                 and then P.Subtrees (1) = Tree_Stack (N)
               then
                  Elab_Spec_Seen := True;
               end if;
            end if;
         end if;
         --  End special handling for 'Elab_Spec and 'Elab_Body

         Put
           ("\1",
            Id_With_Casing
              (Tree.Ref_Name,
               Tree.Decl_Kind,
               Tree.Is_Predef,
               Use_Name_Casing_For_Nils => Elab_Spec_Seen));
      end Do_Usage_Name;

   --  Start of processing for Subtree_To_Ada

   begin
      Append (Tree_Stack, Tree); -- push

      Maybe_Blank_Line;

      case Tree.Kind is
         when A_Compilation_Unit =>
            Do_Compilation_Unit;

         when A_Comment =>
            pragma Assert (False); -- not used
            Do_Comment;

         when Def_Names =>
            Do_Def_Name;

         when Usage_Names =>
            Do_Usage_Name;

         when An_Integer_Literal | A_Real_Literal | A_String_Literal =>
            Do_Literal;

         when Flat_Pragma_Kinds =>
            Do_Pragma;

         when A_Null_Statement =>
            Do_Null_Statement;

         when An_Ordinary_Type_Declaration =>
            Do_Ordinary_Type_Declaration;

         when A_Procedure_Call_Statement | An_Entry_Call_Statement =>
            Do_Procedure_Call_Statement;

         when A_Function_Call =>
            Do_Function_Call;

         when An_And_Then_Short_Circuit | An_Or_Else_Short_Circuit =>
            Do_Binary_Operator
              (Tree,
               Is_Right  => False,
               Cur_Level => Cur_Level);

         when A_Task_Type_Declaration =>
            Do_Task_Type_Declaration;

         when A_Single_Task_Declaration =>
            Do_Single_Task_Declaration;

         when A_Pragma_Argument_Association |
           A_Discriminant_Association       |
           A_Record_Component_Association   |
           An_Array_Component_Association   |
           A_Parameter_Association          |
           A_Generic_Association            =>
            Do_Association;

         when Flat_Attribute_Reference_Kinds =>
            Do_Attribute_Reference;

         when A_Block_Statement =>
            Do_Block_Statement;

         when A_Subtype_Indication =>
            Do_Subtype_Indication;

         when A_Case_Path =>
            Do_Case_Path;

         when A_Case_Statement =>
            Do_Case_Statement;

         when A_Component_Clause =>
            Do_Component_Clause;

         when A_Constrained_Array_Definition     |
           A_Formal_Constrained_Array_Definition =>
            Do_Constrained_Array_Definition;

         when An_Extended_Return_Statement =>
            Do_Extended_Return_Statement;

         when An_Accept_Statement =>
            Do_Accept_Statement;

         when A_Positional_Array_Aggregate |
             A_Named_Array_Aggregate =>
            Do_Array_Aggregate;

         when A_Qualified_Expression =>
            Do_Qualified_Expression;

         when A_Record_Aggregate =>
            Do_Record_Aggregate;

         when An_Extension_Aggregate =>
            Do_Extension_Aggregate;

         when A_Parameter_Specification | A_Formal_Object_Declaration =>
            Do_Parameter_Specification;

         when A_Procedure_Declaration       |
           A_Null_Procedure_Declaration     |
           A_Procedure_Renaming_Declaration |
           An_Entry_Declaration             |
           A_Generic_Procedure_Declaration  |
           A_Formal_Procedure_Declaration   |
           A_Procedure_Body_Stub            =>
            --  An_Accept_Statement goes through Do_Accept_Statement
            Do_Subp_Decl
              (Is_Function  => False,
               Is_Body      => False,
               Params_Query => Parameter_Profile);

         when A_Procedure_Body_Declaration |
           An_Entry_Body_Declaration       =>
            Do_Subp_Decl
              (Is_Function  => False,
               Is_Body      => True,
               Params_Query => Parameter_Profile);

         when An_Access_To_Procedure                  |
           An_Access_To_Protected_Procedure           |
           An_Anonymous_Access_To_Procedure           |
           An_Anonymous_Access_To_Protected_Procedure |
           A_Formal_Access_To_Procedure               |
           A_Formal_Access_To_Protected_Procedure     =>
            Do_Subp_Decl
              (Is_Function  => False,
               Is_Body      => False,
               Params_Query => Access_To_Subprogram_Parameter_Profile);

         when A_Function_Declaration          |
           An_Expression_Function_Declaration |
           A_Function_Renaming_Declaration    |
           A_Generic_Function_Declaration     |
           A_Formal_Function_Declaration      |
           A_Function_Body_Stub               =>
            Do_Subp_Decl
              (Is_Function  => True,
               Is_Body      => False,
               Params_Query => Parameter_Profile);

         when A_Function_Body_Declaration  =>
            Do_Subp_Decl
              (Is_Function  => True,
               Is_Body      => True,
               Params_Query => Parameter_Profile);

         when An_Access_To_Function                  |
           An_Access_To_Protected_Function           |
           An_Anonymous_Access_To_Function           |
           An_Anonymous_Access_To_Protected_Function |
           A_Formal_Access_To_Function               |
           A_Formal_Access_To_Protected_Function     =>
            Do_Subp_Decl
              (Is_Function  => True,
               Is_Body      => False,
               Params_Query => Access_To_Subprogram_Parameter_Profile);

         when Flat_List_Kinds =>
            Do_List;

         when others =>
            Do_Others;
      end case;

      Delete_Last (Tree_Stack); -- pop
   end Subtree_To_Ada;

   procedure Convert_Tree_To_Ada (Tree : Ada_Tree) is
   begin
      Append_Line_Break
        (Hard     => True,
         Affects_Comments => True,
         Level    => 0,
         Kind     => Not_An_Element,
         Template => Name_Empty);
      pragma Assert (Check_Whitespace);
      Subtree_To_Ada (Tree, Cur_Level => 0, Index_In_Parent => 1);
      pragma Debug (Assert_No_Trailing_Blanks (To_W_Str (Out_Buf)));
      Append
        (Tabs,
         Tab_Rec'
           (Parent | Tree => null, Mark => Mark (Out_Buf, '$'), others => <>));
      --  Append a sentinel tab, whose Position is greater than any actual
      --  position. This ensures that as we step through Tabs, there is
      --  always one more.
      pragma Assert (Is_Empty (Tree_Stack));
      Reset (Out_Buf);
      pragma Assert (Cur_Indentation = 0);
   end Convert_Tree_To_Ada;

   procedure Assert_No_Trailing_Blanks (S : W_Str) is
   begin
      pragma Assert (S'First = 1);
      for X in 2 .. S'Last loop
         pragma Assert (if S (X) /= ' ' then not Is_Space (S (X)));
         if S (X) = NL then
            pragma Assert (S (X - 1) /= ' ');
         end if;
      end loop;
      pragma Assert (S (S'Last) = NL);
   end Assert_No_Trailing_Blanks;

   function Replacements (T : Ada_Template) return Ada_Template;

   function Replacements (T : Ada_Template) return Ada_Template is
      Temp : W_Str_Access := new W_Str'(W_Str (T));
   begin
      --  Replacements inserting soft line breaks

      Temp := Replace_All (Temp, "? @(~; ~)~", "?[@ (~;@ ~)]~");
      Temp := Replace_All (Temp, "? @(~, ~)~", "?[@ (~,@ ~)]~");
      Temp := Replace_All (Temp, "? := ~~~", "? :=[@ ~~]~");
      Temp := Replace_All (Temp, " renames !", " renames[@ !]");
      --  ???Should be a weaker @, at least for function renamings.
      Temp := Replace_All (Temp, "? and ~ and ~~", "? and[@ ~ and@ ~]~");
      Temp := Replace_All (Temp, " => !", " =>[@ !]");

      --  Replacements inserting tabs

      Temp := Replace_All (Temp, "=>", "^=>");
      Temp :=
        Replace_All
          (Temp,
           "?~, ~~ :? ~~~ !? :=[@ ~~]~",
           "?~, ~~ ^:? ~~~ !? ^2:=[@ ~~]~");
      Temp :=
        Replace_All
          (Temp,
           "?~, ~~ :? ~~~ constant !? :=[@ ~~]~",
           "?~, ~~ ^:? ~~~ constant !? ^2:=[@ ~~]~");
      --  This doesn't cover A_Parameter_Specification, which is handled
      --  specially by Do_Parameter_Specification.

      --  Replacements inserting soft line breaks in comma-separated lists of
      --  defining identifiers.

      Temp := Replace_All (Temp, "?~, ~~ ^:", "?~,@ ~~ ^:");
      --  Note @ without []

      --  Replacements for --no-separate-is

      if not Options.Separate_Line_For_IS then
         Temp := Replace_All (Temp, "@ is", " is");
      end if;

      --  If the --no-end-id switch was given, do not insert names after "end"
      --  during the Convert_Tree_To_Ada pass. Instead, insert them during
      --  Insert_Comments_And_Blank_Lines, and only if they are present in the
      --  source.

      if not Options.End_Id then
         Temp := Replace_All (Temp, "end !1", "end");
         Temp := Replace_All (Temp, "end !2", "end");
         Temp := Replace_All (Temp, "end?1 ~~~", "end");
         Temp := Replace_All (Temp, "end?2 ~~~", "end");
         Temp := Replace_All (Temp, "end?3 ~~~", "end");
      end if;

      return Result : constant Ada_Template := Ada_Template (Temp.all) do
         Free (Temp);
      end return;
   end Replacements;

   procedure Free is new Unchecked_Deallocation
     (Ada_Template, Ada_Template_Ptr);

   procedure Replace_One (Kind : Ada_Tree_Kind; From, To : W_Str);
   --  Replace From with To in the template for Kind

   procedure Replace_One (Kind : Ada_Tree_Kind; From, To : W_Str) is
      Temp : Ada_Template_Ptr := Template_Table (Kind);
   begin
      Template_Table (Kind) :=
        new Ada_Template'(Ada_Template
          (Must_Replace (W_Str (Temp.all), From, To)));
      Free (Temp);
   end Replace_One;

   procedure Init_Template_Table is
   begin
      pragma Assert (not Template_Table_Initialized);
      Template_Table_Initialized := True;

      --  We can't initialize Template_Table with an aggregate, because we
      --  refer to the Kind. The following case-within-loop construction may
      --  look odd, but it accomplishes two goals: the 'case' requires full
      --  coverage, so the items left null are done so explicitly, and the
      --  'for' provides the Kind value to each sub-case that needs it.
      --  The 'case' we're talking about is in Template_For_Kind.

      for Kind in Ada_Tree_Kind loop
         declare
            Temp : Ada_Template_Ptr := Template_For_Kind (Kind);
         begin
            if Temp = null then
               Template_Table (Kind) := null;
            else
               Template_Table (Kind) :=
                 new Ada_Template'
                   (Munge_Template (Replacements (Temp.all), Kind));
               Free (Temp);
            end if;
         end;
      end loop;

      --  Some more-specific replacements

      --  For Separate_Line_For_THEN_and_LOOP, we want a hard line break before
      --  "then" and "loop".

      if Options.Separate_Line_For_THEN_and_LOOP then
         Replace_One (An_If_Path, "@ then$", "$then$");
         Replace_One (An_Elsif_Path, "@ then$", "$then$");
         Replace_One (A_While_Loop_Statement, "@ loop$", "$loop$");
         Replace_One (A_For_Loop_Statement, "@ loop$", "$loop$");

      --  For No_Separate_Line_For_THEN_and_LOOP, we remove the soft line break
      --  before "then" and "loop".

      elsif Options.No_Separate_Line_For_THEN_and_LOOP then
         Replace_One (An_If_Path, "@ then$", " then$");
         Replace_One (An_Elsif_Path, "@ then$", " then$");
         Replace_One (A_While_Loop_Statement, "@ loop$", " loop$");
         Replace_One (A_For_Loop_Statement, "@ loop$", " loop$");
      end if;

      --  Now do some validity checking on the templates

      for Kind in Ada_Tree_Kind loop
         declare
            T : constant Ada_Template_Ptr := Template_Table (Kind);

         begin
            if T /= null then
               declare
                  subtype Constrained_Query_Count is
                    Query_Count range 0 .. Num_Queries (Kind);
                  Subtree_Count : Query_Count := 0;

               begin
                  for J in T'Range loop
                     case T (J) is
                        when '!' | '?' =>
                           if J < T'Last and then T (J + 1) in '1' .. '9' then
                              pragma Assert
                                (Query_Index (Char_To_Digit (T (J + 1))) in
                                   Constrained_Query_Count);

                           else
                              Subtree_Count := Subtree_Count + 1;
                           end if;

                        --  ??? "{" is always preceded by "$"; we might want a
                        --  short-hand for "${".

                        when '{' =>
                           pragma Assert (T (J - 1) = '$');

                        when others =>
                           null;
                     end case;
                  end loop;

                  if Subtree_Count /= Constrained_Query_Count'Last then
                     raise Program_Error
                       with "Wrong Subtree_Count: " & Kind'Img;
                  end if;
               end;
            end if;
         end;
      end loop;

      if Debug_Mode then
         Put_Ada_Templates;
      end if;
   end Init_Template_Table;

   procedure Init_Pp_Off_And_On is
      pragma Warnings (Off);
      --  Suppress warning about useless use clause
      use type GNAT.OS_Lib.String_Access;
      pragma Warnings (On);
   begin
      if Options.Pp_Off_String /= null then
         pragma Assert (Options.Pp_Off_String.all /= "");
         Pp_Off_On_Delimiters.Off := new W_Str'
           ("--" & To_Wide_String (Options.Pp_Off_String.all));
      end if;
      if Options.Pp_On_String /= null then
         pragma Assert (Options.Pp_On_String.all /= "");
         Pp_Off_On_Delimiters.On := new W_Str'
           ("--" & To_Wide_String (Options.Pp_On_String.all));
      end if;
   end Init_Pp_Off_And_On;

   use Scanner;
--  use all type Token_Vector;

--  Start of processing for Tree_To_Ada

begin
   if Debug_Mode then
      ASIS_UL.Dbg_Out.Output_Enabled := True;
   end if;

   if not Template_Table_Initialized then
      Init_Template_Table;
      Init_Pp_Off_And_On;
   end if;

   --  Note that if we're processing multiple files, we will get here multiple
   --  times, so we need to clear out data structures left over from last time.

   pragma Assert (Cur_Indentation = 0);
   Clear (All_Line_Breaks);
   Clear (Tabs);

   Get_Tokens (Src_Buf, Src_Tokens, Pp_Off_On_Delimiters);
   if Debug_Mode then
      Dbg_Out.Put ("Src_Tokens:\n");
      Put_Tokens (Src_Tokens);
      Dbg_Out.Put ("end Src_Tokens:\n");
   end if;

   Clear (Out_Buf);

   --  If --comments-only was specified, format the comments and quit

   if Options.Comments_Only then
      Do_Comments_Only (Lines_Data, Src_Buf, Options);
   else
      --  Otherwise, convert the tree to text, and
      --  then run all the text-based passes.

      Convert_Tree_To_Ada (Root);
      Post_Tree_Phases
        (Lines_Data, Get_Name_String (Root.Source_File), Src_Buf, Options);
   end if;

   --  Finally, print out the result to Current_Output

   declare
      Out_Vec : constant Char_Vector := Remove_Extra_Line_Breaks;
   begin
      Write_Out_Buf (Out_Vec);
   end;

exception
   --  If we got an error, don't produce output

   when Common.Fatal_Error =>
      raise;

   when X : others =>
      --  In order to avoid damaging the user's source code, if there is a bug
      --  (like a token mismatch in Final_Check), we avoid writing the output
      --  file in Do_Diff mode; otherwise, we write the input to the output
      --  unchanged. This happens only in production builds.
      --
      --  Raise_Token_Mismatch includes the file name and source location in
      --  the message; include that if available.

      declare
         use Ada.Exceptions;
         Loc : constant String :=
           (if Exception_Identity (X) = Token_Mismatch'Identity
              then Exception_Message (X) else "");
      begin
         Output.Error
           (Source_Name & ":" & Loc &
              ": pretty-printing failed; unable to format");
      end;

      if Enable_Token_Mismatch then
         raise;
      else
         if Do_Diff then
            Output_Written := False;
         else
            if not At_Beginning (Src_Buf) then
               while not At_End (Src_Buf) loop
                  Move_Forward (Src_Buf);
               end loop;
               Reset (Src_Buf);
            end if;

            Write_Src_Buf;
         end if;
      end if;
end Tree_To_Ada;
