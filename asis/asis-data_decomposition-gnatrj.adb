------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--        A S I S . D A T A _ D E C O M P O S I T I O N . G N A T R         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 2019, Free Software Foundation, Inc.          --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License  distributed with ASIS-for-GNAT; see file     --
-- COPYING. If not, write to the Free Software Foundation,  59 Temple Place --
-- - Suite 330,  Boston, MA 02111-1307, USA.                                --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Directories;           use Ada.Directories;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Table;

with Asis.Compilation_Units;    use Asis.Compilation_Units;
with Asis.Elements;             use Asis.Elements;
with Asis.Extensions;           use Asis.Extensions;
with Asis.Declarations;         use Asis.Declarations;

with Repinfo.Input;             use Repinfo.Input;

with A4G.Contt;                 use A4G.Contt;

package body Asis.Data_Decomposition.gnatRj is

   Last_Read_Repinfo_File : String_Access;

   package JSON_Buffer_Table is new GNAT.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Text_Ptr,
     Table_Low_Bound      => 1,
     Table_Initial        => 2048,
     Table_Increment      => 100,
     Table_Name           => "JSON text buffer");

   JSON_Buffer : JSON_Buffer_Table.Table_Ptr renames JSON_Buffer_Table.Table;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Reset_Repinfo_File
      (New_Repinfo_Fname :     String;
       Success           : out Boolean);
   --  Checks if the last readed repinfo file is the same as New_Repinfo_Fname.
   --  If it is, sets Success ON and exits. Otherwise tries to read by calling
   --  Repinfo.Input.Read_JSON_Stream) representation information from
   --  New_Repinfo_Fname. (This procedure assumes that New_Repinfo_Fname is the
   --  name of some existing file). If the attempt is successful, sets Success
   --  ON and stores New_Repinfo_Fname as the name of the last file with
   --  representation info that has been read in. Otherwise sets Success OFF
   --  and sets the name of the last file with representation info that has
   --  been read in to om empty value.

   ---------------------
   -- Get_Entity_Name --
   ---------------------

   function Get_Entity_Name (E : Asis.Element) return String is
      Dcl : Asis.Element := E;
   begin

      while Element_Kind (Dcl) /= A_Declaration loop
         Dcl := Enclosing_Element (Dcl);
      end loop;

      if Is_Nil (Enclosing_Element (Dcl)) then
         --  Dcl is top-level declaration in a compilation unit
         return To_String (Defining_Name_Image (First_Name (Dcl)));
      else
         return Get_Entity_Name (Enclosing_Element (Dcl)) & "." &
            To_String (Defining_Name_Image (First_Name (Dcl)));
      end if;

   end Get_Entity_Name;

   ------------------------
   -- Reset_Repinfo_File --
   ------------------------

   procedure Reset_Repinfo_File
      (New_Repinfo_Fname :     String;
       Success           : out Boolean)
   is
      RF       : File_Type;

      Line_Buf : String (1 .. 2048);
      Line_Len : Natural := 0;
      --  Buffer to read next line from the file into

   begin
      Success := False;

      if Last_Read_Repinfo_File /= null
        and then
         Last_Read_Repinfo_File.all = New_Repinfo_Fname
      then
         Success := True;
         return;
      end if;

      Open (RF, In_File, New_Repinfo_Fname);

      JSON_Buffer_Table.Init;

      while not End_Of_File (RF) loop
         Get_Line (RF, Line_Buf, Line_Len);
         JSON_Buffer_Table.Append_All
           (JSON_Buffer_Table.Table_Type (Line_Buf (1 .. Line_Len)));
      end loop;

      Read_JSON_Stream
        (Text_Buffer (JSON_Buffer (1 .. JSON_Buffer_Table.Last)),
         New_Repinfo_Fname);

      Free (Last_Read_Repinfo_File);
      Last_Read_Repinfo_File := new String'(New_Repinfo_Fname);

      Success := True;

      Close (RF);

   exception
      when others =>
         Success := False;
         Free (Last_Read_Repinfo_File);
   end Reset_Repinfo_File;

   ----------------------
   -- Set_Repinfo_File --
   ----------------------

   procedure Set_Repinfo_File
     (E       :     Asis.Element;
      Success : out Boolean)
   is
      CU            : Asis.Compilation_Unit;
      CU_Name       : String_Access;
      Rep_File_Name : String_Access;

      JSON_Suffix : constant String := ".json";

      function Short_Source_Name
        (CU : Asis.Compilation_Unit)
         return  String
      is (Simple_Name (To_String (Text_Name (CU))));
   begin
      Success := False;

      if Is_Nil (E) then
         return;
      end if;

      CU      := Enclosing_Compilation_Unit (E);
      CU_Name := new String'(Short_Source_Name (CU));

      Rep_File_Name := Locate_In_Search_Path
                         (C         => Get_Current_Context,
                          File_Name => CU_Name.all & JSON_Suffix,
                          Dir_Kind  => Rep);

      if Rep_File_Name = null then
         Success := False;
         return;
      end if;

      Reset_Repinfo_File (Rep_File_Name.all, Success);

   end Set_Repinfo_File;

end Asis.Data_Decomposition.gnatRj;
