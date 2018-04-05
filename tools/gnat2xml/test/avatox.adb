------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                    AVATOX (Ada, Via Asis, To Xml)                        --
--                                                                          --
--                                                                          --
--                Copyright (c) 2007, McKae Technologies.                   --
--                                                                          --
-- Avatox is free software; you can redistribute it and/or modify it        --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Avatox is distributed in the hope  that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- Avatox is based off the Display_Source software distributed as part of   --
-- the ASIS implementation for GNAT, and therefore inherits its GPL         --
-- licensing.  Ada Core Technologies maintains the Display_Source program   --
-- and its copyright is held by the Free Software Foundation.               --
--                                                                          --
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --
------------------------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Asis;
with Asis.Iterator;
with Asis.Elements;
with Asis.Exceptions;
with Asis.Compilation_Units.Relations;
with Asis.Ada_Environments;
with Asis.Implementation;
with Asis.Set_Get;

with Gnat.Regexp;

with Mckae.Environment.Command_Line_Processor;

--  functionality packages
with Vatox.Environment;
with Vatox.Traversal;
with Vatox.Xsl_Transformation;

procedure Avatox is

   use Ada;
   Temp_Filename_Prefix : constant String := ".avx";

   function "<" (L, R : Asis.Compilation_Unit) return Boolean is
   begin
      return Asis.Compilation_Units.Text_Name (L)
        < Asis.Compilation_Units.Text_Name (R);
   end "<";

   function "=" (L, R : Asis.Compilation_Unit) return Boolean is
   begin
      return Asis.Compilation_Units.Text_Name (L)
        = Asis.Compilation_Units.Text_Name (R);
   end "=";

   package Comp_Unit_Sets is new Ada.Containers.Ordered_Sets
     (Asis.Compilation_Unit);

   --  Instantiation of traverse_element
   procedure Traverse_Node is new Asis.Iterator.Traverse_Element
     (Vatox.Traversal.Info_Node,
      Vatox.Traversal.Pre_Procedure,
      Vatox.Traversal.Post_Procedure);

   -----------------------------------------------------------------------------

   function Is_Ads (File : String) return Boolean is
   begin
      return File (File'Last - 3 .. File'Last) = ".ads" or else
             File (File'Last - 3 .. File'Last) = ".ADS";
   end Is_Ads;

   -----------------------------------------------------------------------------

   function Is_Adb (File : String) return Boolean is
   begin
      return File (File'Last - 3 .. File'Last) = ".adb" or else
             File (File'Last - 3 .. File'Last) = ".ADB";
   end Is_Adb;

   -----------------------------------------------------------------------------

   function Main_Name (File : String) return Wide_String is  --  ???
   begin
      return Ada.Characters.Conversions.To_Wide_String
              (File (File'First .. File'Last - 4));
   end Main_Name;

   -----------------------------------------------------------------------------

   function Get_Compilation_Unit (Filename       : Unbounded_String;
                                  Avatox_Context : Asis.Context)
                                  return Asis.Compilation_Unit is
      Unite : String := To_String (Filename);
      The_Unit : Asis.Compilation_Unit := Asis.Nil_Compilation_Unit;

   begin
      --  Converting file name in Ada Unit Name
      --  first let's change the '-' in '.' in the filename
      for I in Unite'Range
      loop
         if Unite (I) = '-' then
            Unite (I) := '.';
         end if;
     end loop;

      if Is_Ads (Unite) then
         The_Unit :=  Asis.Compilation_Units.Library_Unit_Declaration
           (Main_Name (Unite), Avatox_Context);
      elsif Is_Adb (Unite) then
         The_Unit :=  Asis.Compilation_Units.Compilation_Unit_Body
           (Main_Name (Unite), Avatox_Context);
      end if;
      return The_Unit;
   end Get_Compilation_Unit;

   -----------------------------------------------------------------------------

   function Create_Unit_Filename (S             : String;
                                  Axf_Directory : String;
                                  Extension     : String;
                                  Is_Filename   : Boolean := True)
                                  return String is
      Unit_Name : String := S;
      Dir_Name  : String := Axf_Directory & ' ';
      Dir_Name_Length : Natural := Dir_Name'Length - 1;
   begin
      if Axf_Directory = "" then
         Dir_Name := ".";
         Dir_Name_Length := Dir_Name'Length;
      end if;
      if not Is_Filename then
         for I in Unit_Name'Range loop
            if Unit_Name (I) = '.' then
               Unit_Name (I) := '-';
            end if;
         end loop;
      end if;
      return Directories.Compose
        (Dir_Name (1 .. Dir_Name_Length), Unit_Name, Extension);
   end Create_Unit_Filename;

   -----------------------------------------------------------------------------

   function Filename_For (Unit          : Asis.Compilation_Unit;
                          Axf_Directory : String;
                          Extension     : String) return String is

      Unit_Text_Name : constant String
        := To_String (Asis.Compilation_Units.Text_Name (Unit));
      Unit_Comp_Name : constant String
        := To_String (Asis.Compilation_Units.Unit_Full_Name (Unit));
   begin
      if Unit_Text_Name = "" then
         pragma Assert (Unit_Comp_Name /= "");
         return Create_Unit_Filename (Unit_Comp_Name,
                                      Axf_Directory,
                                      Extension,
                                      Is_Filename => False);
      else
         return Create_Unit_Filename (Directories.Simple_Name (Unit_Text_Name),
                                      Axf_Directory,
                                      Extension);
      end if;
   end Filename_For;

   -----------------------------------------------------------------------------

   function Check_And_Validate_Axf_Directory (Axf_Directory : String;
                                              Verbose       : Boolean)
                                              return Boolean is
      F : File_Type;
   begin
      -- Does the target AXF directory exist?
      if not Directories.Exists (Axf_Directory) then
         -- Attempt to create it
         Directories.Create_Directory (Axf_Directory);
         if Verbose then
            Put_Line ("Created directory " & Axf_Directory);
         end if;
      end if;

      -- Is the directory writable?
      Create (F, Out_File, Directories.Compose (Axf_Directory, ".#avtxck#"));
      Delete (F);
      return True;
   exception
      when others =>
         Put_Line ("Cannot create/write to AXF directory " & Axf_Directory);
         return False;
   end Check_And_Validate_Axf_Directory;

   -----------------------------------------------------------------------------

   procedure Process (Element    : in     Asis.Element;
                      Control    : in out Asis.Traverse_Control;
                      State_Node : in out Vatox.Traversal.Info_Node
                   ) is
   begin
      Traverse_Node (Element, Control, State_Node);
   end Process;

   -----------------------------------------------------------------------------

   procedure Process_Unit
     (The_Unit             : in     Asis.Compilation_Unit;
      The_Node_Information : in out Vatox.Traversal.Info_Node) is

      The_Declaration : Asis.Declaration;
      The_Control : Asis.Traverse_Control := Asis.Continue;

   begin
      declare
         Clause_List : constant Asis.Context_Clause_List :=
           Asis.Elements.Context_Clause_Elements (The_Unit, True);
      begin
         for Each_Clause in Clause_List'Range loop
            Process (Clause_List (Each_Clause),
                     The_Control,
                     The_Node_Information
                    );
         end loop;
      end;

      --  and now the main unit declaration
      The_Declaration := Asis.Elements.Unit_Declaration (The_Unit);

      --  Initialization, depending on the application
      Vatox.Traversal.Initiate_Node (The_Unit, The_Control, The_Node_Information);

      --  Now we traverse the declaration ...
      Process (The_Declaration,
               The_Control,
               The_Node_Information);

      --  Termination, depending on the application
      Vatox.Traversal.Terminate_Node (The_Control, The_Node_Information);
   end Process_Unit;

   -----------------------------------------------------------------------------

   function Supporting_Application_Units
     (Context      : Asis.Context;
      Comp_Unit    : Asis.Compilation_Unit;
      Unit_Breadth : Vatox.Environment.Unit_Breadths;
      Filters      : Vatox.Environment.Filtering_Entries)
      return Comp_Unit_Sets.Set is

      use type Asis.Compilation_Unit;

      Comp_Unit_Set : Comp_Unit_Sets.Set := Comp_Unit_Sets.Empty_Set;
      Related_Units : Asis.Relation_Kinds;

      use Vatox;
      use type Vatox.Environment.Unit_Breadths;

   begin
      if not Asis.Compilation_Units.Is_Nil (Comp_Unit) then
         if Unit_Breadth = Environment.Single_Unit then
            Comp_Unit_Set := Comp_Unit_Sets.To_Set (Comp_Unit);
         else
            if Unit_Breadth = Environment.Full_Closure then
               Related_Units := Asis.Needed_Units;
            else
               Related_Units := Asis.Supporters;
            end if;

            declare
               use type Asis.Compilation_Unit_List;

               Closure : constant Asis.Compilation_Units.Relations.Relationship :=
                 Asis.Compilation_Units.Relations.Semantic_Dependence_Order
                   ((1 => Comp_Unit), Asis.Nil_Compilation_Unit_List,
                    Context, Related_Units);
               Units : constant Asis.Compilation_Unit_List := Comp_Unit
                 & Closure.Consistent;
               App_Units : Asis.Compilation_Unit_List (Units'Range);
               App_Unit_Count : Asis.Asis_Natural := 0;

               use Asis;
            begin
               for I in Units'Range loop
                  declare
                     Unit_Name : constant String
                       := Ada.Characters.Handling.To_Lower
                         (To_String (Asis.Compilation_Units.Unit_Full_Name (Units (I))));
                  begin
                     -- Take packages standard and system out of the list, since they're
                     -- usually "virtual"
                     if not (Asis.Set_Get.Is_Standard (Units (I))
                             or (Unit_Name = "system"))
                       and Environment.Passes_Filter (Unit_Name, Filters) then
                        App_Unit_Count := App_Unit_Count + 1;
                        App_Units (App_Unit_Count) := Units (I);
                     end if;
                  end;
               end loop;

               for I in 1 .. App_Unit_Count loop
                  Comp_Unit_Set.Include (App_Units (I));
               end loop;
            end;
         end if;
      end if;
      return Comp_Unit_Set;

   exception
      when others =>
         return Comp_Unit_Sets.Empty_Set;
   end Supporting_Application_Units;

   -----------------------------------------------------------------------------

   function Identify_Units
     (Primary_Files : Vatox.Environment.File_Name_Entries;
      Context       : Asis.Context;
      Unit_Breadth  : Vatox.Environment.Unit_Breadths;
      Filters       : Vatox.Environment.Filtering_Entries)
      return Comp_Unit_Sets.Set is

      Comp_Unit_File  : Unbounded_String;
      Comp_Unit       : Asis.Compilation_Unit;
      Remaining_Files : Vatox.Environment.File_Name_Entries;

      use Vatox.Environment.File_Name_Handling;
      use Comp_Unit_Sets;

   begin
      if Primary_Files = Vatox.Environment.File_Name_Handling.Empty_Set then
         return Comp_Unit_Sets.Empty_Set;
      else
         Comp_Unit_File := Primary_Files.First_Element;
         Comp_Unit := Get_Compilation_Unit (Comp_Unit_File, Context);

         -- Return the union of this set of supporting compilation units and
         -- that of the rest of the primary unit files
         return Supporting_Application_Units
           (Context, Comp_Unit, Unit_Breadth, Filters)
           or Identify_Units (Primary_Files - To_Set (Comp_Unit_File),
                            Context, Unit_Breadth, Filters);
      end if;
   end Identify_Units;

   -----------------------------------------------------------------------------

   procedure Process_Xsl_Transformation
     (XML_In_Filename  : String;
      Output_Filename  : String;
      Info_Node        : Vatox.Traversal.Info_Node) is

      use Vatox.Xsl_Transformation;

      Result         : Transformation_Results;
      Std_Out_Input  : Boolean := Index (Xml_In_Filename, Temp_Filename_Prefix) /= 0;
      Std_Out_Output : Boolean := Index (Output_Filename, Temp_Filename_Prefix) /= 0;

   begin
      Apply_Stylesheet
        (Xml_In_Filename,
         Output_Filename,
         Info_Node.Xsl_Info,
         Result);
      if Info_Node.Verbose then
         Put ("XSLT: ");
         if Std_Out_Input then
            Put ("Avatox stdout");
         else
            Put (Xml_In_Filename);
         end if;
         Put (" -> ");
         if Std_Out_Output then
            Put ("stdout");
         else
            Put (Output_Filename);
         end if;
         Put_Line (" ... " & XSL_Result_String (Result));
      elsif Result /= Success then
         Put_Line ("Failure transforming "
                   & Xml_In_Filename & " -> "
                   & Output_Filename);
      end if;
   end Process_Xsl_Transformation;

   -----------------------------------------------------------------------------

   function Set_To_Comp_Unit_List (Comp_Unit_Set : Comp_Unit_Sets.Set)
                                   return Asis.Compilation_Unit_List is
      Comp_Units       : Asis.Compilation_Unit_List
        (1 .. Natural (Comp_Unit_Set.Length));
      Comp_Unit_Cursor : Comp_Unit_Sets.Cursor := Comp_Unit_Set.First;
      use type Comp_Unit_Sets.Cursor;
   begin
      for I in Comp_Units'Range loop
         Comp_Units (I) := Comp_Unit_Sets.Element (Comp_Unit_Cursor);
         Comp_Unit_Sets.Next(Comp_Unit_Cursor);
      end loop;
      pragma Assert (Comp_Unit_Cursor = Comp_Unit_Sets.No_Element);
      return Comp_Units;
   end Set_To_Comp_Unit_List;

   -----------------------------------------------------------------------------

   procedure Process_Unit
     (Unit_Closure         : in     Vatox.Environment.Units_Needed;
      Unit_Breadth         : in     Vatox.Environment.Unit_Breadths;
      Comp_Units           : in     Asis.Compilation_Unit_List;
      Filters              : in     Vatox.Environment.Filtering_Entries;
      Axf_Directory        : in     String;
      Xml_File             : in out File_Access;
      The_Node_Information : in out Vatox.Traversal.Info_Node;
      Avatox_Context       : in out Asis.Context) is

      use type Asis.Compilation_Unit_List;

      Next_Unit            : Asis.Compilation_Unit;
      Multiple_Files       : constant Boolean := Xml_File = null;
      Xml_Start_Rep        : Boolean := True;
      Xml_End_Rep          : constant Boolean := Multiple_Files;
      XML_Out_File         : aliased File_Type;
      Filename             : Unbounded_String;

      use type Asis.Unit_Kinds;
      use type Asis.Unit_Origins;
      use type Vatox.Environment.Units_Needed;
      use Vatox;

   begin
      for I in Comp_Units'Range loop
         Next_Unit := Comp_Units (I);

         if Asis.Compilation_Units.Unit_Kind (Next_Unit)
           /= Asis.A_Configuration_Compilation then
            if (Unit_Closure = Environment.Predefined) or
              (Unit_Closure = Environment.Unit_Only) or
              ((Unit_Closure = Environment.App_Only) and
                 Asis.Compilation_Units.Unit_Origin (Next_Unit)
               = Asis.An_Application_Unit) then
               if Xml_Start_Rep then
                  if Multiple_Files then
                     Filename := To_Unbounded_String
                       (Filename_For (Next_Unit, Axf_Directory, "axf"));
                     Create (Xml_Out_File, Out_File, To_String (Filename));
                     if The_Node_Information.Verbose then
                        Put_Line ("Creating " & To_String (Filename));
                     end if;
                     Xml_File := Xml_Out_File'Unchecked_Access;
                  end if;

                  The_Node_Information.Xml_File := Xml_File;
                  Vatox.Traversal.Start_Representation (The_Node_Information);
                  Xml_Start_Rep := Multiple_Files;
               end if;

               Vatox.Traversal.Start_Unit (Next_Unit, The_Node_Information);

               -- Process
               Process_Unit (Next_Unit, The_Node_Information);

               Vatox.Traversal.End_Unit (Next_Unit, The_Node_Information);

               if Xml_End_Rep then
                  Vatox.Traversal.End_Representation (The_Node_Information);
                  Close (Xml_Out_File);
                  Xml_File := null;
                  if Vatox.Xsl_Transformation.XSL_Transformation_To_Be_Done
                    (The_Node_Information.Xsl_Info) then
                     Process_Xsl_Transformation
                       (To_String (Filename),
                        Filename_For (Next_Unit, Axf_Directory,
                          Vatox.Xsl_Transformation.
                            Get_Xsl_Extension (The_Node_Information.Xsl_Info)),
                        The_Node_Information);
                  end if;
               end if;
            end if;
         end if;
      end loop;

      if not Multiple_Files then
         Vatox.Traversal.End_Representation (The_Node_Information);
      end if;
   end Process_Unit;

   ----------------------------------------------------------------------------

   procedure Dump_Pseudo_Stdout
     (Filename    : in String;
      Delete : in Boolean := False) is

      Stdout_File   : File_Type;
      Final_Newline : Boolean := False;
      The_Line      : String (1 .. 50_000);
      The_Length    : Natural := 0;
    begin
      Open (Stdout_File, In_File, Filename);
      while not End_Of_File (Stdout_File) loop
         Final_Newline := False;
         Get_Line (Stdout_File, The_Line, The_Length);
         Put (The_Line (1 .. The_Length));
         if The_Length < The_Line'Length then
            New_Line;
            Final_Newline := True;
         end if;
      end loop;
      if Final_Newline then
         New_Line;
      end if;
      Close (Stdout_File);

      if Delete then
         Directories.Delete_File (Filename);
      end if;
   exception
      when others =>
         -- Ignore any errors involved with missing files
         null;
   end Dump_Pseudo_Stdout;

   ----------------------------------------------------------------------------

   The_Avatox_Context   : Asis.Context;
   The_Node_Information : Vatox.Traversal.Info_Node;
   Filename_Specs       : Vatox.Environment.File_Name_Entries;
   Primary_Files        : Vatox.Environment.File_Name_Entries;
   Unit_Cursor          : Vatox.Environment.File_Name_Handling.Cursor;
   Output_Filename      : Unbounded_String;
   Comp_Unit            : Asis.Compilation_Unit;
   Delete_Trees         : Boolean := True;
   Continue             : Boolean := True;
   XML_Out_File         : aliased File_Type;
   XML_File             : File_Access := Standard_Output;
   Asis_Context_Params  : constant String := "-CA -FM -I. ";
   Asis_Params          : Unbounded_String := To_Unbounded_String (Asis_Context_Params);
   Refed_Units          : Vatox.Environment.Units_Needed
     := Vatox.Environment.Unit_Only;
   Unit_Breadth         : Vatox.Environment.Unit_Breadths
     := Vatox.Environment.Single_Unit;
   Multiple_Files       : Boolean := False;
   Axf_Directory        : Unbounded_String;
   Filter_Entries       : Vatox.Environment.Filtering_Entries;
   Temp_Filename        : String := Temp_Filename_Prefix
     & Calendar.Formatting.Image (Calendar.Clock);
   Std_Out              : Boolean := False;


   use Vatox.Environment.File_Name_Handling;
   use type Ada.Containers.Count_Type;
   use type Vatox.Environment.Unit_Breadths;
   use type Vatox.Environment.Units_Needed;

begin
   Vatox.Environment.Process_Args_And_Options (The_Node_Information,
                                               Filename_Specs,
                                               Output_Filename,
                                               Delete_Trees,
                                               Asis_Params,
                                               Refed_Units,
                                               Unit_Breadth,
                                               Axf_Directory,
                                               Multiple_Files,
                                               Filter_Entries,
                                               Continue);

   if not Continue then
      return;
   end if;

   -- Adjust the temporary filename, which is based on the current time
   Overwrite(Temp_Filename, Index(Temp_Filename, " "), "-");

   -- Use a temporary filename when outputting to stdout
   Std_Out := (Output_Filename = Null_Unbounded_String) and not Multiple_Files;

   -- Given the filename specification(s), get the list of primary file names
   -- to process
   Vatox.Environment.Collect_Primary_Files (Filename_Specs, Primary_Files);

   -- Make sure there's some files to process
   if Primary_Files.Length = 0 then
      Put_Line ("No unit file(s) located");
      return;
   end if;

   --  Initialization of Asis environment.
   Asis.Implementation.Initialize("-asis05");
   Asis.Ada_Environments.Associate
     (The_Context => The_Avatox_Context,
      Name        => "The_Avatox_Context",
      Parameters  => To_Wide_String (To_String (Asis_Params)));

   -- The following code oddity: Open, ..., Close, then Open again, is to work
   -- around an ASIS bug in the GNAT GPL 2006 ASIS distribution. The "-FM"
   -- option in the association parameters directs that ASIS do on-the-fly
   -- compilations as needed to resolve ASIS queries, but if any tree (.adt)
   -- files are already present, use those instead. The ASIS-for-GNAT problem
   -- appears to be that when doing an OTF compilation something about the
   -- generated tree file is not correctly read in, and this shows up when
   -- making the Asis.Compilation_Units.Relations.Semantic_Dependence_Order
   -- query (and perhaps others). However, if the .adt tree file already exists
   -- when the context is opened, then the tree file is properly read in and the
   -- Semantic_Dependence_Order query works correctly. So what the following
   -- sequence does is initially open the ASIS context, make an ASIS query to
   -- force an OTF compilation (which generates and leaves the .adt file), close
   -- the context, and then reopens it--though now the adt file exists and is
   -- ready to load. And so off we go.

   Asis.Ada_Environments.Open (The_Avatox_Context);

   Unit_Cursor := Primary_Files.First;
   while Unit_Cursor /= No_Element loop
      Comp_Unit := Get_Compilation_Unit
        (Element(Unit_Cursor), The_Avatox_Context);
      Vatox.Environment.File_Name_Handling.Next(Unit_Cursor);
   end loop;

   Asis.Ada_Environments.Close (The_Avatox_Context);

   -- Open the ASIS context and start processing files
   Asis.Ada_Environments.Open (The_Avatox_Context);

   Continue := False;
   Unit_Cursor := Primary_Files.First;
   while Unit_Cursor /= No_Element loop
      Comp_Unit := Get_Compilation_Unit
        (Element (Unit_Cursor), The_Avatox_Context);

      if Asis.Compilation_Units.Is_Nil (Comp_Unit) then
         Put_Line (To_String (Element (Unit_Cursor))
                   & " not found or does not contain Ada source code");
      elsif (Unit_Breadth = Vatox.Environment.Full_Closure)
        and (not Asis.Compilation_Units.Can_Be_Main_Program (Comp_Unit)) then
         Put_Line ("Closure processing (-c) is only valid for subprograms");
         Put_Line ("that can serve as main program units.");
      else
         -- Indicate that there's at least one file that can be processed
         Continue := True;
      end if;
      Vatox.Environment.File_Name_Handling.Next (Unit_Cursor);
   end loop;

   if Continue then
      declare
         CUs_To_Process : constant Asis.Compilation_Unit_List
           := Set_To_Comp_Unit_List
             (Identify_Units
                  (Primary_Files, The_Avatox_Context,
                   Unit_Breadth, Filter_Entries));
      begin
         -- Generate the AXF document(s).
         if Std_Out then
            Create (Xml_Out_File, Out_File, Temp_Filename);
         elsif not Multiple_Files then
            Create (Xml_Out_File, Out_File, To_String (Output_Filename));
         end if;
         Xml_File := Xml_Out_File'Unchecked_Access;

         if Multiple_Files then
            Xml_File := null;

            if (Axf_Directory /= Null_Unbounded_String) then
               Continue := Check_And_Validate_Axf_Directory
                 (To_String (Axf_Directory),
                 Verbose => The_Node_Information.Verbose);
            end if;
         end if;

         if Continue then
            Process_Unit (Refed_Units, Unit_Breadth, CUs_To_Process,
                          Filter_Entries, To_String (Axf_Directory),
                          Xml_File, The_Node_Information, The_Avatox_Context);
         end if;

         if not Multiple_Files then
            Close (Xml_Out_File);
         end if;

         if Std_Out then
            -- If writing was to stdout, dump the contents of the temp file that
            -- was used in its stead.
            declare
               Xsl_Filename : Unbounded_String := To_Unbounded_String
                 (Vatox.Xsl_Transformation.Get_Output_Filename
                    (The_Node_Information.Xsl_Info));
               Xsl_Stdout   : Boolean := False;
            begin
               Dump_Pseudo_Stdout(Temp_Filename, Delete => False);
               if Vatox.Xsl_Transformation.XSL_Transformation_To_Be_Done
                 (The_Node_Information.Xsl_Info) then

                  -- If the XSL output is going to stdout, temporarily write it
                  -- to a file, dump that
                  if Length (Xsl_Filename) = 0 then
                     Xsl_Filename := To_Unbounded_String(Temp_Filename & ".axt");
                     Xsl_Stdout   := True;
                  end if;

                  Process_Xsl_Transformation
                    (Temp_Filename,
                     To_String(Xsl_Filename),
                     The_Node_Information);

                  if Xsl_Stdout then
                     Dump_Pseudo_Stdout (To_String (Xsl_Filename),
                                         Delete => True);
                  end if;
               end if;
               Open (Xml_Out_File, In_File, Temp_Filename);
               Delete (Xml_Out_File);
           end;
         elsif not Multiple_Files then
            if Vatox.Xsl_Transformation.XSL_Transformation_To_Be_Done
              (The_Node_Information.Xsl_Info) then
               Process_Xsl_Transformation
                 (To_String (Output_Filename),
                  Vatox.Xsl_Transformation.Get_Output_Filename
                    (The_Node_Information.Xsl_Info),
                  The_Node_Information);
             end if;
         end if;
      end;
   else
      Put_Line ("No files to process!");
   end if;

   ------------------------------
   --  Closing Asis ....
   Asis.Ada_Environments.Close (The_Avatox_Context);
   Asis.Ada_Environments.Dissociate (The_Avatox_Context);
   Asis.Implementation.Finalize ("");

   if Delete_Trees then
      Unit_Cursor := Primary_Files.First;
      while Unit_Cursor /= No_Element loop
         --  let's delete the *.at? and *.ali files
         declare
            To_Erase : String
              := Directories.Simple_Name (To_String (Element (Unit_Cursor)));
            File     : File_Type;
         begin
            if To_Erase (To_Erase'Last - 3 .. To_Erase'Last - 1) = ".ad" or else
              To_Erase (To_Erase'Last - 3 .. To_Erase'Last - 1) = ".AD"
            then
               --  tree file
               To_Erase (To_Erase'Last) := 't';
               Open (File, Out_File, To_Erase);
               Delete (File);

               --  ali file
               To_Erase (To_Erase'Last - 2 .. To_Erase'Last) := "ali";
               Open (File, Out_File, To_Erase);
               Delete (File);

            end if;
         exception
            when Name_Error =>
               -- An .adt file isn't necessarily generated for every compilation
               -- unit, such as when a child package and its parent packages are
               -- being processed.
               null;
         end;
         Vatox.Environment.File_Name_Handling.Next (Unit_Cursor);
      end loop;
   end if;

exception

   when Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit =>
      Put_Line ("The file " & To_String (Element(Unit_Cursor)) &
                 " does not contain any Ada Unit.");
      Vatox.Environment.Show_Usage;

   when Asis.Exceptions.ASIS_Failed |
        Asis.Exceptions.ASIS_Inappropriate_Element |
        Asis.Exceptions.ASIS_Inappropriate_Context =>
         Put_Line (Ada.Characters.Conversions.To_String
           (Asis.Implementation.Diagnosis));   --  ???

   when Status_Error | Mode_Error | Name_Error | Use_Error =>
      Put_Line ("Error opening/writing to : " &
                To_String (Output_Filename));

   when The_Error : others =>
      Put_Line ("The exception received : " &
                Ada.Exceptions.Exception_Name (The_Error));
      Put_Line (Ada.Exceptions.Exception_Information(The_Error));
      Put_Line (Ada.Characters.Conversions.To_String
         (Asis.Implementation.Diagnosis));

end Avatox;
