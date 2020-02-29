------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                      G N A T T E S T . C O M M O N                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
--                                                                          --
-- GNATTEST  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either  version  2, or (at your option) any later --
-- version.  GNATTEST  is  distributed  in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License distributed with GNAT; see file COPYING. If --
-- not, write to the  Free  Software  Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.,                                      --
--                                                                          --
-- GNATTEST is maintained by AdaCore (http://www.adacore.com).              --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Text_IO;                use Ada.Text_IO;

with Asis;                       use Asis;
with Asis.Declarations;          use Asis.Declarations;
with Asis.Definitions;           use Asis.Definitions;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Exceptions;            use Asis.Exceptions;
with Asis.Implementation;        use Asis.Implementation;
with Asis.Expressions;           use Asis.Expressions;
with Asis.Set_Get;               use Asis.Set_Get;

with ASIS_UL.Common;
with ASIS_UL.Compiler_Options;   use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;

with GNAT.Directory_Operations;   use GNAT.Directory_Operations;
with GNAT.SHA1;

with GNATtest.Options;            use GNATtest.Options;
with GNATtest.Skeleton.Source_Table;

with GNATCOLL.Traces;             use GNATCOLL.Traces;

with Types; use Types;

package body GNATtest.Common is

   Me_Hash    : constant Trace_Handle := Create ("Hash", Default => Off);
   Me_Stub    : constant Trace_Handle := Create ("Stubs", Default => Off);
   Me_Closure : constant Trace_Handle := Create ("Closure", Default => Off);

   Infix : Natural := 0;

   Closure : String_Set.Set := String_Set.Empty_Set;

   function Operator_Image (Op : Defining_Name) return String;
   --  According to operator symbols returns their literal names to make the
   --  names of the testing routines correct.

   function Root_Type_Declaration
     (Type_Dec : Asis.Element) return Asis.Element;
   --  Unlike Corresponding_Root_Type unwinds all the tagged record type
   --  hierarchy disregart the privacy of intermidiate extensions.
   --  If the argument allready is a record type declaration, returns itself.
   --  If given not a tagged record declaration or extension declaration
   --  returns Nil_Element.

   function Get_Closure_Diff return List_Of_Strings.List;
   --  Recomputes the closure based on new ALI files created by last invocation
   --  of gcc and returns the list of new files.

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Asis.Element) return Boolean is
   begin
      return R_Node (Left) < R_Node (Right);
   end "<";

   ------------
   -- Create --
   ------------

   procedure Create (Name : String) is
      B_Name :          String := Base_Name (Name);
      P_Name : constant String := Dir_Name (Name);
   begin
      if B_Name (B_Name'First + 1) = '-'
        and then B_Name (B_Name'First) in 'a' | 's' | 'i' | 'g'
      then
         B_Name (B_Name'First + 1) := '~';
      end if;
      Char_Sequential_IO.Create
        (Output_File, Char_Sequential_IO.Out_File, P_Name & B_Name);
   end Create;

   ----------------
   -- Create_ALI --
   ----------------

   procedure Create_ALI (Source : String; Success : out Boolean) is
      Delete_Success : Boolean;
      Sufexless_Name : constant String :=
        GNATtest.Skeleton.Source_Table.Get_Source_Suffixless_Name (Source);
   begin
      Trace (Me_Closure, "Creating intermidiate tree for " & Source);
      Compile
       (new String'(Source),
        Arg_List.all,
        Success,
        GCC          => ASIS_UL.Common.Gcc_To_Call,
        Display_Call => ASIS_UL.Debug.Debug_Flag_C);
      Delete_File (Sufexless_Name & ".adt", Delete_Success);
      if not Delete_Success then
         Report_Std ("gnattest: cannot delete " &
                     Sufexless_Name & ".adt");
      end if;
   end Create_ALI;

   -----------
   -- Close --
   -----------

   procedure Close_File is
   begin
      Char_Sequential_IO.Close (Output_File);
   end Close_File;

   -----------------
   -- Create_Dirs --
   -----------------

   procedure Create_Dirs (Target_Dirs : File_Array_Access) is
      First : Integer;
   begin
      for J in Target_Dirs'Range loop
         declare
            Target_Dir : constant String :=
                           Target_Dirs.all (J).Display_Full_Name;
         begin
            First := Target_Dir'First;

            if Is_Regular_File (Target_Dir) then
               Report_Err ("gnattest: cannot create dir " & Target_Dir);
               raise Fatal_Error;
            end if;

            for Idx in Target_Dir'Range loop
               if Target_Dir (Idx) = Directory_Separator
                 or else Idx = Target_Dir'Last
               then
                  if not Is_Directory (Target_Dir (First .. Idx)) then
                     begin
                        Make_Dir (Target_Dir (First .. Idx));
                     exception
                        when Directory_Error =>
                           Report_Err ("gnattest: cannot create dir " &
                                       Target_Dir (First .. Idx));
                           raise Fatal_Error;
                     end;
                  end if;
               end if;
            end loop;
         end;
      end loop;
   end Create_Dirs;

   --------------------------
   -- Generate_Common_File --
   --------------------------

   procedure Generate_Common_File is
      Common_Package_Name : constant String := "Gnattest_Generated";
      Common_File_Subdir  : constant String :=
        Harness_Dir.all & Directory_Separator & "common";
   begin
      if not Is_Directory (Common_File_Subdir) then
         Make_Dir (Common_File_Subdir);
      end if;
      Create (Common_File_Subdir &
              Directory_Separator &
              Unit_To_File_Name (Common_Package_Name) & ".ads");

      S_Put (0, "package Gnattest_Generated is");
      Put_New_Line;
      S_Put (3, "package GNATtest_Standard renames Standard;");
      Put_New_Line;
      S_Put (3, "Default_Assert_Value : Boolean := ");
      if Skeletons_Fail then
         S_Put (0, "False;");
      else
         S_Put (0, "True;");
      end if;
      Put_New_Line;
      S_Put (0, "end Gnattest_Generated;");

      Close_File;
   end Generate_Common_File;

   ----------------------
   -- Get_Closure_Diff --
   ----------------------

   function Get_Closure_Diff return List_Of_Strings.List is
      Result : List_Of_Strings.List := List_Of_Strings.Empty_List;

      Closure_Files : GNATCOLL.VFS.File_Array_Access;
      Main_Files    : GNATCOLL.VFS.File_Array_Access;
      Status        : Status_Type;
   begin
      Increase_Indent (Me_Closure, "Get_Closure_Diff");
      for Main_Unit of Options.Main_Units loop
         Append (Main_Files, Create (+Main_Unit));
      end loop;
      Get_Closures
        (Source_Project_Tree.Root_Project,
         Main_Files,
         Status => Status,
         Result => Closure_Files);

      if Closure_Files /= null then
         for I in Closure_Files'Range loop
            if not Closure.Contains (Closure_Files (I).Display_Full_Name) then
               Trace
                 (Me_Closure,
                  "new file: " & Closure_Files (I).Display_Base_Name);
               Result.Append (Closure_Files (I).Display_Full_Name);
               Closure.Include (Closure_Files (I).Display_Full_Name);
            else
               Trace
                 (Me_Closure,
                  "old file: " & Closure_Files (I).Display_Base_Name);
            end if;
         end loop;
      end if;

      Decrease_Indent (Me_Closure);

      return Result;
   end Get_Closure_Diff;

   -----------------
   -- Get_Nesting --
   -----------------

   function Get_Nesting (Elem : Asis.Element) return String is
      Res  : String_Access := new String'("");
      Buff : String_Access;

      Enclosing : Asis.Element;
   begin

      Enclosing := Enclosing_Element (Elem);
      if Definition_Kind (Enclosing) = A_Protected_Definition
      then
         Enclosing := Enclosing_Element (Enclosing);
      end if;

      loop

         exit when Is_Nil (Enclosing);

         if Res.all = "" then
            Free (Res);
            Res := new String'
              (To_String (Defining_Name_Image
               (First_Name (Enclosing))));
         else
            Buff :=
              new String'(To_String (Defining_Name_Image
                (First_Name (Enclosing))) &
                "." & Res.all);
            Free (Res);
            Res := new String'(Buff.all);
            Free (Buff);
         end if;

         Enclosing := Enclosing_Element (Enclosing);
         if Declaration_Kind (Enclosing) = A_Package_Instantiation then
            Enclosing := Enclosing_Element (Enclosing);
         end if;

      end loop;

      return Res.all;

   end Get_Nesting;

   --------------------
   -- Get_Next_Infix --
   --------------------

   function Get_Next_Infix return String is
   begin
      Infix := Infix + 1;
      return Trim (Natural'Image (Infix), Both) & "_";
   end Get_Next_Infix;

   ---------------------
   --  Get_Subp_Name  --
   ---------------------

   function Get_Subp_Name (Subp : Asis.Element) return String is
   begin
      --  checking for overloaded operators
      if Defining_Name_Kind (First_Name (Subp)) =
        A_Defining_Operator_Symbol
      then
         return Operator_Image (First_Name (Subp));
      else
         return To_String (Defining_Name_Image (First_Name (Subp)));
      end if;

   end Get_Subp_Name;

   ----------------------
   -- Mangle_Hash_Full --
   ----------------------

   function Mangle_Hash_Full
     (Subp           : Asis.Declaration;
      Case_Sensitive : Boolean := False;
      N_Controlling  : Boolean := False;
      For_Stubs      : Boolean := False)
      return String
   is

      Tagged_Rec     : Asis.Declaration := Asis.Nil_Element;

      L_Subp : constant Asis.Element :=
        (if Declaration_Kind (Subp) in
             A_Function_Instantiation | A_Procedure_Instantiation
         then
            Corresponding_Declaration (Subp)
         else
            Subp);

      L_Subp_Span : constant Asis.Text.Span := Element_Span (L_Subp);

      Subp_Name_Im  : constant String :=
        To_String (Defining_Name_Image (First_Name (L_Subp)));

      SW_Buff     : String_Access;
      Sign_Image  : String_Access;
      Param       : Asis.Element;
      Root_Ignore : Asis.Element;

      Controlling_Params : constant Asis.Element_List :=
        Controlling_Parameters (L_Subp);

      Attr_Flag : Boolean;
      --  Used to add a special marking to subprogram parameters whose types
      --  have'Class and 'Base attributes (same parameter can't have both of
      --  those attributes, so the same marking is used).

      Same_Type_Params : Integer;

      Params : constant Parameter_Specification_List :=
        Parameter_Profile (L_Subp);
      --  Root level parameters list.

      function Unsubtype (Arg : Asis.Declaration) return Asis.Declaration;
      --  If argumnet is a subtype declaration returns corresponding type
      --  declaration, otherwise returns Arg.

      function Parameter_Image (Param_Input : Asis.Element) return String;
      --  Returns the image of given subprogram parameter.

      function Full_Name_Image (Elem : Asis.Element) return String;
      --  Takes a type declaration as an argument.
      --  Returns the image of the type name with full package name
      --  prefix.

      function Handle_Parameters
        (Params : Parameter_Specification_List;
         Result_Profile : Asis.Element)
         return String;
      --  Returns an image of the types from parameters list and the result
      --  type in case of a function for a given list of parameter
      --  specifications.

      function Is_Controlling_Parameter (Param : Asis.Element) return Boolean;
      --  Returns true if given parameter is a controlling one.

      function Has_Controlling_Result_W (Param : Asis.Element) return Boolean;
      --  Wrapper for Asis.Extensions.Has_Controlling_Result that returns
      --  False in stub mode.

      -----------------------
      --  Full_Name_Image  --
      -----------------------
      function Full_Name_Image (Elem : Asis.Element) return String is
         Enclosing : Asis.Element;

         Elem_Full_Image : String_Access :=
           new String'(To_String (Defining_Name_Image (First_Name (Elem))));

         Exch_Buff       : String_Access;
      begin

         Enclosing := Elem;
         loop
            case Declaration_Kind (Enclosing) is
               when A_Package_Declaration         |
                    A_Generic_Package_Declaration =>

                  Exch_Buff :=
                    new String'(To_String (Defining_Name_Image
                      (First_Name (Enclosing))) &
                      "." & Elem_Full_Image.all);
                  Free (Elem_Full_Image);
                  Elem_Full_Image := new String'(Exch_Buff.all);
                  Free (Exch_Buff);

               when others =>
                  null;
            end case;

            Enclosing := Enclosing_Element (Enclosing);
            exit when Is_Nil (Enclosing);

         end loop;

         return Elem_Full_Image.all;

      end Full_Name_Image;

      -------------------------
      --  Handle_Parameters  --
      -------------------------
      function Handle_Parameters
        (Params : Parameter_Specification_List;
         Result_Profile : Asis.Element)
         return String
      is
         Params_Full_Image : String_Access := new String'("");
         Exchange_Buff     : String_Access;

         Param : Asis.Element;

      begin

         for I in Params'Range loop

            Param := Params (I);

            if Params_Full_Image.all = "" then
               Exchange_Buff :=
                 new String'("(" & Params_Full_Image.all &
                             Parameter_Image (Param));
            else
               Exchange_Buff :=
                 new String'(Params_Full_Image.all &
                             ";" & Parameter_Image (Param));
            end if;
            Free (Params_Full_Image);
            Params_Full_Image := new String'(Exchange_Buff.all);
            Free (Exchange_Buff);

         end loop;

         if not Is_Nil (Result_Profile) then

            Attr_Flag := False;

            case Definition_Kind (Result_Profile) is

               when Not_A_Definition =>

                  if
                    Expression_Kind (Result_Profile) = An_Attribute_Reference
                    and then
                      (Attribute_Kind (Result_Profile) = A_Class_Attribute
                       or Attribute_Kind (Result_Profile) = A_Base_Attribute)

                  then
                     Attr_Flag := True;
                     Param := Unsubtype (Corresponding_Name_Declaration
                       (Normalize_Reference (Prefix (Result_Profile))));
                  else
                     Param := Unsubtype (Corresponding_Name_Declaration
                       (Normalize_Reference (Result_Profile)));

                  end if;

                  if Attr_Flag then
                     Exchange_Buff := new String'
                       (Params_Full_Image.all & ")"    &
                        Full_Name_Image (Param) &
                        "'Attr");
                  else
                     Exchange_Buff := new String'
                       (Params_Full_Image.all & ")" &
                        Full_Name_Image (Param));
                  end if;
                  Free (Params_Full_Image);
                  Params_Full_Image := new String'(Exchange_Buff.all);
                  Free (Exchange_Buff);

               when An_Access_Definition =>

                  case (Access_Definition_Kind (Param)) is

                     when An_Anonymous_Access_To_Function =>

                        Exchange_Buff := new String'
                          (Params_Full_Image.all
                           & ")"
                           & Handle_Parameters
                             (Access_To_Subprogram_Parameter_Profile (Param),
                              Access_To_Function_Result_Profile (Param))
                           & ";");
                        Free (Params_Full_Image);
                        Params_Full_Image := new String'(Exchange_Buff.all);
                        Free (Exchange_Buff);

                     when An_Anonymous_Access_To_Protected_Function =>

                        Exchange_Buff := new String'
                          (Params_Full_Image.all
                           & ")#"
                           & Handle_Parameters
                             (Access_To_Subprogram_Parameter_Profile (Param),
                              Access_To_Function_Result_Profile (Param))
                           & ";");
                        Free (Params_Full_Image);
                        Params_Full_Image := new String'(Exchange_Buff.all);
                        Free (Exchange_Buff);

                     when An_Anonymous_Access_To_Procedure =>

                        Exchange_Buff := new String'
                          (Params_Full_Image.all
                           & ")"
                           & Handle_Parameters
                             (Access_To_Subprogram_Parameter_Profile (Param),
                              Asis.Nil_Element)
                           & ";");
                        Free (Params_Full_Image);
                        Params_Full_Image := new String'(Exchange_Buff.all);
                        Free (Exchange_Buff);

                     when An_Anonymous_Access_To_Protected_Procedure =>

                        Exchange_Buff := new String'
                          (Params_Full_Image.all
                           & ")#"
                           & Handle_Parameters
                             (Access_To_Subprogram_Parameter_Profile (Param),
                              Asis.Nil_Element)
                           & ";");
                        Free (Params_Full_Image);
                        Params_Full_Image := new String'(Exchange_Buff.all);
                        Free (Exchange_Buff);

                     when others =>

                        if
                          Expression_Kind (Result_Profile) =
                          An_Attribute_Reference
                          and then
                            (Attribute_Kind (Result_Profile) =
                               A_Class_Attribute
                             or Attribute_Kind (Result_Profile) =
                               A_Base_Attribute)
                        then
                           Attr_Flag := True;
                           Param :=
                             Unsubtype
                               (Corresponding_Name_Declaration
                                    (Normalize_Reference
                                         (Prefix (Result_Profile))));
                        else
                           Param :=
                             Unsubtype
                               (Corresponding_Name_Declaration
                                    (Normalize_Reference (Result_Profile)));

                        end if;

                        if Attr_Flag then
                           Exchange_Buff := new String'
                             (Params_Full_Image.all & ")@" &
                                Full_Name_Image (Result_Profile) &  "'Attr");
                        else
                           Exchange_Buff := new String'
                             (Params_Full_Image.all & ")@" &
                                Full_Name_Image (Result_Profile));
                        end if;
                        Free (Params_Full_Image);
                        Params_Full_Image := new String'(Exchange_Buff.all);
                        Free (Exchange_Buff);
                  end case;

               when others =>
                  null;
            end case;

         else
            Exchange_Buff :=
              new String'(Params_Full_Image.all & ")");
            Free (Params_Full_Image);
            Params_Full_Image := new String'(Exchange_Buff.all);
            Free (Exchange_Buff);
         end if;

         return Params_Full_Image.all;

      end Handle_Parameters;

      ------------------------------
      -- Has_Controlling_Result_W --
      ------------------------------

      function Has_Controlling_Result_W (Param : Asis.Element) return Boolean
      is
      begin
         if Stub_Mode_ON then
            return False;
         else
            return Has_Controlling_Result (Param);
         end if;
      end Has_Controlling_Result_W;
      ------------------------------
      -- Is_Controlling_Parameter --
      ------------------------------

      function Is_Controlling_Parameter (Param : Asis.Element) return Boolean
      is
      begin
         if For_Stubs then
            --  For stub bodies and setters we do not need the root types.
            return False;
         end if;
         if N_Controlling then
            --  Hash version 2.1 treated all parameters as if they could be
            --  controlling.
            return True;
         end if;

         for J in Controlling_Params'Range loop
            if Is_Equal (Controlling_Params (J), Param) then
               return True;
            end if;
         end loop;
         return False;
      end Is_Controlling_Parameter;

      -----------------------
      --  Parameter_Image  --
      -----------------------

      function Parameter_Image (Param_Input : Asis.Element) return String is

         Name_List : constant Defining_Name_List := Names (Param_Input);

         Param_Full_Image : constant String_Access := new String'("");

         Param : Asis.Element;
      begin

         Param := Object_Declaration_View (Param_Input);

         case Definition_Kind (Param) is

            when Not_A_Definition =>

               if
                 Expression_Kind (Param) = An_Attribute_Reference
                 and then
                   (Attribute_Kind (Param) = A_Class_Attribute
                    or Attribute_Kind (Param) = A_Base_Attribute)
               then
                  Param := Unsubtype (Corresponding_Name_Declaration
                    (Normalize_Reference (Prefix (Param))));

                  return
                    Trim (Integer'Image (Name_List'Length), Both) &
                    Full_Name_Image (Param) &
                    "'Attr";
               else
                  return
                    Trim (Integer'Image (Name_List'Length), Both) &
                    Full_Name_Image (Unsubtype (Corresponding_Name_Declaration
                                     (Normalize_Reference (Param))));
               end if;

            when An_Access_Definition =>

               case (Access_Definition_Kind (Param)) is

                  when An_Anonymous_Access_To_Function =>

                     return
                       Trim (Integer'Image (Name_List'Length), Both) &
                       Handle_Parameters
                         (Access_To_Subprogram_Parameter_Profile (Param),
                          Access_To_Function_Result_Profile (Param));

                  when An_Anonymous_Access_To_Protected_Function =>

                     return
                       Trim (Integer'Image (Name_List'Length), Both)
                       & "#"
                       & Handle_Parameters
                         (Access_To_Subprogram_Parameter_Profile (Param),
                          Access_To_Function_Result_Profile (Param));

                  when An_Anonymous_Access_To_Procedure =>

                     return
                       Trim (Integer'Image (Name_List'Length), Both) &
                       Handle_Parameters
                         (Access_To_Subprogram_Parameter_Profile (Param),
                          Asis.Nil_Element);

                  when An_Anonymous_Access_To_Protected_Procedure =>

                     return
                       Trim (Integer'Image (Name_List'Length), Both)
                       & "#"
                       & Handle_Parameters
                         (Access_To_Subprogram_Parameter_Profile (Param),
                          Asis.Nil_Element);

                  when others =>
                     Param := Anonymous_Access_To_Object_Subtype_Mark (Param);

                     if
                       Expression_Kind (Param) = An_Attribute_Reference
                       and then
                         (Attribute_Kind (Param) = A_Class_Attribute
                          or Attribute_Kind (Param) = A_Base_Attribute)
                     then
                        Param := Unsubtype (Corresponding_Name_Declaration
                          (Normalize_Reference (Prefix (Param))));
                        return
                          Trim (Integer'Image (Name_List'Length), Both) & "@" &
                          Full_Name_Image (Param) & "'Attr";
                     else
                        Param := Unsubtype (Corresponding_Name_Declaration
                          (Normalize_Reference ((Param))));
                        return
                          Trim (Integer'Image (Name_List'Length), Both) & "@" &
                          Full_Name_Image (Param);
                     end if;
               end case;

            when others =>
               null;

         end case;

         return Param_Full_Image.all;

      end Parameter_Image;

      function Unsubtype (Arg : Asis.Declaration) return Asis.Declaration
      is
      begin
         if Declaration_Kind (Arg) = A_Subtype_Declaration then
            return Corresponding_First_Subtype (Arg);
         end if;
         return Arg;
      end Unsubtype;

   begin
      Trace
        (Me_Hash,
         "Mangle_Hash_Full for " & Subp_Name_Im
         & (if Is_Nil (L_Subp_Span) then ""
           else " at line" & L_Subp_Span.First_Line'Img));

      case Declaration_Kind (L_Subp) is
         when A_Function_Declaration             |
              A_Function_Renaming_Declaration    |
              An_Expression_Function_Declaration =>
            Sign_Image :=
              new String'("function" & Subp_Name_Im & "(");
         when A_Procedure_Declaration          |
              A_Procedure_Renaming_Declaration =>
            Sign_Image :=
              new String'("procedure" & Subp_Name_Im & "(");
         when others =>
            Trace (Me_Hash, "Unexpected element, returning empty hash");
            Trace (Me_Hash, To_String (Element_Image (L_Subp)));
            Trace (Me_Hash, To_String (Debug_Image (L_Subp)));
            return "";
      end case;

      Increase_Indent (Me_Hash);

      if
        Is_Dispatching_Operation (L_Subp)
        and then Definition_Kind (Primitive_Owner (L_Subp)) /=
          A_Private_Type_Definition
      then
         Tagged_Rec := Enclosing_Element (Primitive_Owner (L_Subp));
      end if;

      if Is_Nil (Tagged_Rec) then
         Root_Ignore := Asis.Nil_Element;
      else
         Root_Ignore := Root_Type_Declaration (Tagged_Rec);
      end if;

      for I in Params'Range loop

         Attr_Flag := False;

         Param := Params (I);

         Same_Type_Params := Names (Param)'Length;
         SW_Buff :=
           new String'(Sign_Image.all &
                       Trim (Integer'Image (Same_Type_Params), Both));
         Free (Sign_Image);
         Sign_Image := new String'(SW_Buff.all);
         Free (SW_Buff);

         Param := Object_Declaration_View (Param);

         case Definition_Kind (Param) is

            when Not_A_Definition =>

               if
                 Expression_Kind (Param) = An_Attribute_Reference
                 and then
                   (Attribute_Kind (Param) = A_Class_Attribute
                    or Attribute_Kind (Param) = A_Base_Attribute)
               then
                  Attr_Flag := True;
                  Param := Unsubtype (Corresponding_Name_Declaration
                    (Normalize_Reference (Prefix (Param))));
               else
                  Param := Unsubtype (Corresponding_Name_Declaration
                    (Normalize_Reference (Param)));

                  if not Is_Nil (Root_Ignore) then
                     if
                       Is_Controlling_Parameter (Params (I))
                       and then Is_Equal
                         (Root_Ignore,
                          Root_Type_Declaration (Param))
                     then
                        Param := Root_Ignore;
                     end if;
                  end if;
               end if;

               if Attr_Flag then
                  SW_Buff := new String'
                    (Sign_Image.all & Full_Name_Image (Param) & "'Attr;");
               else
                  SW_Buff := new String'
                    (Sign_Image.all & Full_Name_Image (Param) & ";");
               end if;
               Free (Sign_Image);
               Sign_Image := new String'(SW_Buff.all);
               Free (SW_Buff);

            when An_Access_Definition =>

               case (Access_Definition_Kind (Param)) is

                  when An_Anonymous_Access_To_Function =>

                     SW_Buff := new String'
                       (Sign_Image.all                                     &
                        Handle_Parameters
                          (Access_To_Subprogram_Parameter_Profile (Param),
                           Access_To_Function_Result_Profile (Param))      &
                        ";");
                     Free (Sign_Image);
                     Sign_Image := new String'(SW_Buff.all);
                     Free (SW_Buff);

                  when An_Anonymous_Access_To_Protected_Function =>

                     SW_Buff := new String'
                       (Sign_Image.all
                        & "#"
                        & Handle_Parameters
                          (Access_To_Subprogram_Parameter_Profile (Param),
                           Access_To_Function_Result_Profile (Param))
                        & ";");
                     Free (Sign_Image);
                     Sign_Image := new String'(SW_Buff.all);
                     Free (SW_Buff);

                  when An_Anonymous_Access_To_Procedure =>

                     SW_Buff := new String'
                       (Sign_Image.all                                     &
                        Handle_Parameters
                          (Access_To_Subprogram_Parameter_Profile (Param),
                           Asis.Nil_Element)                               &
                        ";");
                     Free (Sign_Image);
                     Sign_Image := new String'(SW_Buff.all);
                     Free (SW_Buff);

                  when An_Anonymous_Access_To_Protected_Procedure =>

                     SW_Buff := new String'
                       (Sign_Image.all
                        & "#"
                        & Handle_Parameters
                          (Access_To_Subprogram_Parameter_Profile (Param),
                           Asis.Nil_Element)
                        & ";");
                     Free (Sign_Image);
                     Sign_Image := new String'(SW_Buff.all);
                     Free (SW_Buff);

                  when others =>

                     Param := Anonymous_Access_To_Object_Subtype_Mark (Param);

                     if
                       Expression_Kind (Param) = An_Attribute_Reference
                       and then
                         (Attribute_Kind (Param) = A_Class_Attribute
                          or Attribute_Kind (Param) = A_Base_Attribute)
                     then
                        Attr_Flag := True;
                        Param := Unsubtype (Corresponding_Name_Declaration
                          (Normalize_Reference (Prefix (Param))));
                     else
                        Param := Unsubtype (Corresponding_Name_Declaration
                          (Normalize_Reference (Param)));

                        if not Is_Nil (Root_Ignore) then
                           if
                             Is_Controlling_Parameter (Params (I))
                             and then Is_Equal
                               (Root_Ignore,
                                Root_Type_Declaration (Param))
                           then
                              Param := Root_Ignore;
                           end if;
                        end if;
                     end if;

                     if Attr_Flag then
                        SW_Buff := new String'
                          (Sign_Image.all & "@" &
                           Full_Name_Image (Param) &  "'Attr;");
                     else
                        SW_Buff := new String'
                          (Sign_Image.all & "@" &
                           Full_Name_Image (Param) & ";");
                     end if;
                     Free (Sign_Image);
                     Sign_Image := new String'(SW_Buff.all);
                     Free (SW_Buff);

               end case;

            when others =>
               null;

         end case;

      end loop;

      if
        Declaration_Kind (L_Subp) = A_Function_Declaration or else
        Declaration_Kind (L_Subp) = A_Function_Renaming_Declaration or else
        Declaration_Kind (L_Subp) = An_Expression_Function_Declaration
      then

         Attr_Flag := False;

         Param := Result_Profile (L_Subp);

         case Definition_Kind (Param) is

            when Not_A_Definition =>

               if
                 Expression_Kind (Param) = An_Attribute_Reference
                 and then
                   (Attribute_Kind (Param) = A_Class_Attribute
                    or Attribute_Kind (Param) = A_Base_Attribute)
               then
                  Attr_Flag := True;
                  Param := Unsubtype (Corresponding_Name_Declaration
                    (Normalize_Reference (Prefix (Param))));
               else
                  Param := Unsubtype (Corresponding_Name_Declaration
                    (Normalize_Reference (Param)));

                  if not Is_Nil (Root_Ignore) then
                     if
                       (Has_Controlling_Result_W
                          (L_Subp) or else N_Controlling)
                       and then Is_Equal
                         (Root_Ignore,
                          Root_Type_Declaration (Param))
                     then
                        Param := Root_Ignore;
                     end if;
                  end if;
               end if;

               if Attr_Flag then
                  SW_Buff := new String'
                    (Sign_Image.all & ")"    &
                     Full_Name_Image (Param) &
                     "'Attr;");
               else
                  SW_Buff := new String'
                    (Sign_Image.all & ")" & Full_Name_Image (Param) & ";");
               end if;
               Free (Sign_Image);
               Sign_Image := new String'(SW_Buff.all);
               Free (SW_Buff);

            when An_Access_Definition =>

               case (Access_Definition_Kind (Param)) is

                  when An_Anonymous_Access_To_Function =>

                     SW_Buff := new String'
                       (Sign_Image.all
                        & ")"
                        & Handle_Parameters
                          (Access_To_Subprogram_Parameter_Profile (Param),
                           Access_To_Function_Result_Profile (Param))
                        & ";");
                     Free (Sign_Image);
                     Sign_Image := new String'(SW_Buff.all);
                     Free (SW_Buff);

                  when An_Anonymous_Access_To_Protected_Function =>

                     SW_Buff := new String'
                       (Sign_Image.all
                        & ")#"
                        & Handle_Parameters
                          (Access_To_Subprogram_Parameter_Profile (Param),
                           Access_To_Function_Result_Profile (Param))
                        & ";");
                     Free (Sign_Image);
                     Sign_Image := new String'(SW_Buff.all);
                     Free (SW_Buff);

                  when An_Anonymous_Access_To_Procedure =>

                     SW_Buff := new String'
                       (Sign_Image.all
                        & ")"
                        & Handle_Parameters
                          (Access_To_Subprogram_Parameter_Profile (Param),
                           Asis.Nil_Element)
                        & ";");
                     Free (Sign_Image);
                     Sign_Image := new String'(SW_Buff.all);
                     Free (SW_Buff);

                     when An_Anonymous_Access_To_Protected_Procedure =>

                     SW_Buff := new String'
                       (Sign_Image.all
                        & ")#"
                        & Handle_Parameters
                          (Access_To_Subprogram_Parameter_Profile (Param),
                           Asis.Nil_Element)
                        & ";");
                     Free (Sign_Image);
                     Sign_Image := new String'(SW_Buff.all);
                     Free (SW_Buff);

                  when others =>

                     Param := Anonymous_Access_To_Object_Subtype_Mark (Param);

                     if
                       Expression_Kind (Param) = An_Attribute_Reference
                       and then
                         (Attribute_Kind (Param) = A_Class_Attribute
                          or Attribute_Kind (Param) = A_Base_Attribute)
                     then
                        Attr_Flag := True;
                        Param := Unsubtype
                          (Corresponding_Name_Declaration
                             (Normalize_Reference (Prefix (Param))));
                     else
                        Param := Unsubtype (Corresponding_Name_Declaration
                                            (Normalize_Reference (Param)));

                        if not Is_Nil (Root_Ignore) then
                           if
                             Has_Controlling_Result_W (L_Subp)
                             and then Is_Equal
                               (Root_Ignore,
                                Root_Type_Declaration (Param))
                           then
                              Param := Root_Ignore;
                           end if;
                        end if;
                     end if;

                     if Attr_Flag then
                        SW_Buff := new String'
                          (Sign_Image.all & ")@" &
                             Full_Name_Image (Param) &  "'Attr;");
                     else
                        SW_Buff := new String'
                          (Sign_Image.all
                           & ")@" & Full_Name_Image (Param) & ";");
                     end if;
                     Free (Sign_Image);
                     Sign_Image := new String'(SW_Buff.all);
                     Free (SW_Buff);
               end case;

            when others =>
               null;
         end case;

      else
         SW_Buff := new String'(Sign_Image.all & ")");
         Free (Sign_Image);
         Sign_Image := new String'(SW_Buff.all);
         Free (SW_Buff);
      end if;

      if Case_Sensitive then
         SW_Buff := new String'(GNAT.SHA1.Digest (Sign_Image.all));
         Trace (Me_Hash, "Hash image: " & Sign_Image.all);
      else
         SW_Buff := new String'(GNAT.SHA1.Digest (To_Lower (Sign_Image.all)));
         Trace (Me_Hash, "Hash image: " & (To_Lower (Sign_Image.all)));
      end if;

      Decrease_Indent (Me_Hash);
      Trace
        (Me_Hash,
         "Hash : " & SW_Buff (SW_Buff'First .. SW_Buff'First + 15));

      return SW_Buff (SW_Buff'First .. SW_Buff'First + 15);

   end Mangle_Hash_Full;

   -------------------------
   -- First_Column_Number --
   -------------------------

   function First_Column_Number (Element : Asis.Element) return Line_Number is
      Sp : Asis.Text.Span;
   begin

      if Is_Text_Available (Element) then
         Sp := Element_Span (Element);

         return Sp.First_Column;

      else

         return 0;

      end if;

   end First_Column_Number;

   ----------------------
   --  Operator_Image  --
   ----------------------

   function Operator_Image (Op : Defining_Name) return String is
   begin
      case Operator_Kind (Op) is

         when An_And_Operator =>                   -- and
            return "And";
         when An_Or_Operator =>                    -- or
            return "Or";
         when An_Xor_Operator =>                   -- xor
            return "Xor";
         when An_Equal_Operator =>                 -- =
            return "Equal";
         when A_Not_Equal_Operator =>              -- /=
            return "Not_Equal";
         when A_Less_Than_Operator =>              -- <
            return "Less_Than";
         when A_Less_Than_Or_Equal_Operator =>     -- <=
            return "Less_Than_Or_Equal";
         when A_Greater_Than_Operator =>           -- >
            return "Greater_Than";
         when A_Greater_Than_Or_Equal_Operator =>  -- >=
            return "Greater_Than_Or_Equal";
         when A_Plus_Operator =>                   -- +
            return "Plus";
         when A_Minus_Operator =>                  -- -
            return "Minus";
         when A_Concatenate_Operator =>            -- &
            return "Concatenate";
         when A_Unary_Plus_Operator =>             -- +
            return "Unary_Plus";
         when A_Unary_Minus_Operator =>            -- -
            return "Unary_Minus";
         when A_Multiply_Operator =>               -- *
            return "Multiply";
         when A_Divide_Operator =>                 -- /
            return "Divide";
         when A_Mod_Operator =>                    -- mod
            return "Mod";
         when A_Rem_Operator =>                    -- rem
            return "Rem";
         when An_Exponentiate_Operator =>          -- **
            return "Exponentiate";
         when An_Abs_Operator =>                   -- abs
            return "Abs";
         when A_Not_Operator =>                    -- not
            return "Not";

         when others =>
            raise Fatal_Error;
      end case;

   end Operator_Image;

   -------------------------------
   --  Parent_Type_Declaration  --
   -------------------------------

   function Parent_Type_Declaration
     (Type_Dec : Asis.Element) return Asis.Element
   is
      Dec_Elem : Asis.Element := Type_Dec;
      Def_Elem : Asis.Element := Type_Declaration_View (Dec_Elem);
   begin

      if
        Declaration_Kind (Dec_Elem) = A_Tagged_Incomplete_Type_Declaration
      then
         Dec_Elem := Corresponding_Type_Completion (Dec_Elem);
         Def_Elem := Type_Declaration_View (Dec_Elem);
      end if;

      if Declaration_Kind (Dec_Elem) = A_Private_Type_Declaration then
         Dec_Elem := Corresponding_Type_Completion (Dec_Elem);
         Def_Elem := Type_Declaration_View (Dec_Elem);
      end if;

      if
        Definition_Kind (Def_Elem) = A_Private_Extension_Definition
      then

         Dec_Elem := Corresponding_Type_Declaration (Dec_Elem);
         Def_Elem := Type_Declaration_View (Dec_Elem);
      end if;

      Dec_Elem := Corresponding_Parent_Subtype (Def_Elem);

      if Declaration_Kind (Dec_Elem) = A_Subtype_Declaration then
         Dec_Elem := Corresponding_First_Subtype (Dec_Elem);
      end if;

      if
        Declaration_Kind (Dec_Elem) in
        A_Tagged_Incomplete_Type_Declaration | An_Incomplete_Type_Declaration
      then
         Dec_Elem := Corresponding_Type_Completion (Dec_Elem);
      end if;

      if Declaration_Kind (Dec_Elem) in
        A_Private_Type_Declaration | A_Private_Extension_Declaration
      then
         Dec_Elem := Corresponding_Type_Completion (Dec_Elem);
      end if;

      return Dec_Elem;

   exception

      when Asis.Exceptions.ASIS_Inappropriate_Element =>
         return Asis.Nil_Element;

   end Parent_Type_Declaration;

   ------------------------
   -- Put_Harness_Header --
   ------------------------

   procedure Put_Harness_Header is
   begin
      S_Put
        (0,
         "--  This package has been generated automatically by GNATtest.");
      Put_New_Line;
      S_Put
        (0,
         "--  Do not edit any part of it, "
         & "see GNATtest documentation for more details.");
      Put_New_Line;
      Put_New_Line;
   end Put_Harness_Header;

   ------------------
   -- Put_New_Line --
   ------------------

   procedure Put_New_Line is
   begin
      Char_Sequential_IO.Write (Output_File, ASCII.LF);
   end Put_New_Line;

   ------------------------
   -- Report_AUnit_Usage --
   ------------------------

   procedure Report_AUnit_Usage is
   begin
      Report_Err ("gnattest: trying to process aunit itself!");
      Report_Err ("gnattest: Fatal_Error raised, terminating process.");
   end Report_AUnit_Usage;

   ----------------
   -- Report_Err --
   ----------------

   procedure Report_Err (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
   end Report_Err;

   ----------------
   -- Report_Std --
   ----------------

   procedure Report_Std (Message : String; Offset : Integer := 0) is
   begin

      if GNATtest.Options.Quiet then
         return;
      end if;

      for I in 1 .. Offset loop
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Output, " ");
      end loop;

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output, Message);
   end Report_Std;

   -------------------------------------
   -- Report_Unhandled_ASIS_Exception --
   -------------------------------------

   procedure Report_Unhandled_ASIS_Exception (Ex : Exception_Occurrence) is
   begin
      Report_Err ("ASIS exception (" & Exception_Name (Ex) & ") is raised");
      Report_Err ("ASIS Error Status is " & Status'Img);
      Report_Err ("ASIS Diagnosis is " & To_String (Diagnosis));

      Set_Status;
   end Report_Unhandled_ASIS_Exception;

   --------------------------------
   -- Report_Unhandled_Exception --
   --------------------------------

   procedure Report_Unhandled_Exception (Ex : Exception_Occurrence) is
   begin
      Report_Err (Exception_Information (Ex));
   end Report_Unhandled_Exception;

   ---------------------------------
   -- Report_Exclusions_Not_Found --
   ---------------------------------

   procedure Report_Exclusions_Not_Found is
      Cur : String_Set.Cursor := Excluded_Files.First;
   begin
      while Cur /= String_Set.No_Element loop
         Report_Std
           ("warning: exemption: source " & String_Set.Element (Cur)
            & " not found");
         Next (Cur);
      end loop;
      Excluded_Files.Clear;
   end Report_Exclusions_Not_Found;

   -----------------------------
   --  Root_Type_Declaration  --
   -----------------------------

   function Root_Type_Declaration
     (Type_Dec : Asis.Element) return Asis.Element
   is
      Dec_Elem : Asis.Element := Type_Dec;
      Def_Elem : Asis.Element;
   begin

      loop

         if Declaration_Kind (Dec_Elem) = A_Subtype_Declaration then
            Dec_Elem := Corresponding_First_Subtype (Dec_Elem);
         end if;

         if
           Declaration_Kind (Dec_Elem) = A_Tagged_Incomplete_Type_Declaration
         then
            Dec_Elem := Corresponding_Type_Completion (Dec_Elem);
         end if;

         if Declaration_Kind (Dec_Elem) = A_Private_Type_Declaration then
            Dec_Elem := (Corresponding_Type_Declaration (Dec_Elem));
         end if;

         Def_Elem := Type_Declaration_View (Dec_Elem);

         if Definition_Kind (Def_Elem) = A_Private_Extension_Definition then
            Dec_Elem := Corresponding_Type_Declaration (Dec_Elem);
            Def_Elem := Type_Declaration_View (Dec_Elem);
         end if;

         if Type_Kind (Def_Elem) in
           A_Tagged_Record_Type_Definition | An_Interface_Type_Definition
         then
            return Dec_Elem;
         end if;

         if Definition_Kind (Def_Elem) = A_Tagged_Private_Type_Definition then
            Dec_Elem := Corresponding_Type_Declaration (Dec_Elem);
            return Dec_Elem;
         end if;

         Dec_Elem := Corresponding_Parent_Subtype (Def_Elem);

      end loop;

   exception
      when Asis.Exceptions.ASIS_Inappropriate_Element =>
         return Asis.Nil_Element;

   end Root_Type_Declaration;

   -----------
   -- S_Put --
   -----------

   procedure S_Put (Span : Natural; Text : String) is
   begin
      for J in 0 .. Span - 1 loop
         Char_Sequential_IO.Write (Output_File, ' ');
      end loop;
      for J in Text'Range loop
         Char_Sequential_IO.Write (Output_File, Text (J));
      end loop;
   end S_Put;

   ---------------------------------
   -- Store_Default_Excluded_Stub --
   ---------------------------------

   procedure Store_Default_Excluded_Stub (Excluded : String) is
   begin
      Trace (Me_Stub, "do not ever stub " & Excluded);
      Default_Stub_Exclusion_List.Include (Excluded);
   end Store_Default_Excluded_Stub;

   -------------------------
   -- Store_Excluded_Stub --
   -------------------------

   procedure Store_Excluded_Stub (Source : String; Excluded : String) is
      Local_Set : String_Set.Set := String_Set.Empty_Set;
   begin
      Trace (Me_Stub, "do not stub " & Excluded & " when testing " & Source);
      if Stub_Exclusion_Lists.Contains (Source) then
         Local_Set := Copy (Stub_Exclusion_Lists.Element (Source));
         Local_Set.Include (Excluded);
         Stub_Exclusion_Lists.Replace (Source, Local_Set);
      else
         Local_Set.Include (Excluded);
         Stub_Exclusion_Lists.Include (Source, Local_Set);
      end if;
   end Store_Excluded_Stub;

   --------------------------
   -- To_String_First_Name --
   --------------------------

   function To_String_First_Name (Elem : Asis.Element) return String
   is
   begin
      return To_String (Defining_Name_Image (First_Name (Elem)));
   end To_String_First_Name;

   -----------------------
   -- Unit_To_File_Name --
   -----------------------

   function Unit_To_File_Name (Old : String) return String is
      T : String_Access;
   begin
      T := new String'(Old);
      for J in T.all'First .. T.all'Last loop
         if T.all (J) = '.' then
            if J = T.all'First + 1 and then
              T.all (J - 1) in 'a' | 's' | 'i' | 'g' | 'A' | 'S' | 'I' | 'G'
            then
               T.all (J) := '~';
            else
               T.all (J) := '-';
            end if;
         end if;
      end loop;

      return To_Lower (T.all);
   end Unit_To_File_Name;

   --------------------
   -- Update_Closure --
   --------------------

   procedure Update_Closure is
      Closure   : constant List_Of_Strings.List   := Get_Closure_Diff;
      Cur       :          List_Of_Strings.Cursor := Closure.First;
      Success   :          Boolean;
      Recompute :          Boolean := False;
      VF        :          GNATCOLL.VFS.Virtual_File;

      use List_Of_Strings;

      procedure Create_Intermediate_ALI;
      procedure Create_Intermediate_ALI is
      begin
         Change_Dir
           (Temp_Dir.all & Directory_Separator & Closure_Subdir_Name);
         Create_ALI (VF.Display_Full_Name, Success);
         if not Success then
            Report_Std
              ("gnattest (warning): "
               & "cannot calculate closure for "
               & VF.Display_Base_Name
               & ", overall closure may be incomplete");
         else
            Recompute := True;
         end if;
         Change_Dir (Temp_Dir.all);
      end Create_Intermediate_ALI;
   begin
      Trace (Me_Closure, "Update_Closure");

      while Cur /= List_Of_Strings.No_Element loop
         VF := Create (+List_Of_Strings.Element (Cur));

         case Source_Project_Tree.Info (VF).Unit_Part is
            when Unit_Spec =>
               if Excluded_Files.Contains (VF.Display_Base_Name) then
                  --  This file should not be processed, but we need
                  --  to get its ALI in case it impacts the closure
                  Create_Intermediate_ALI;
                  Excluded_Files.Exclude (VF.Display_Base_Name);
               else
                  Skeleton.Source_Table.Add_Source_To_Process
                    (VF.Display_Full_Name);
               end if;

            when Unit_Body =>
               if not Skeleton.Source_Table.Source_Present
                 (Source_Project_Tree.Other_File
                    (VF).Display_Full_Name)
               then
                  --  There is no corresponding spec for this file, so we need
                  --  to get its ALI in case it impacts the closure
                  Create_Intermediate_ALI;
               end if;

            when others =>
               null;
         end case;

         Next (Cur);
      end loop;

      if Recompute then
         Update_Closure;
      end if;
   end Update_Closure;

end GNATtest.Common;
