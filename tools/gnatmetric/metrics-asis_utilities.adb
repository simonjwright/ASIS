------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--               M E T R I C S . A S I S _ U T I L I T I E S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2002-2016, AdaCore                     --
--                                                                          --
-- GNAT Metrics Toolset  is free software;  you can  redistribute it and/or --
-- modify it under terms of the  GNU General Public License as published by --
-- the Free Software Foundation;  either version 2, or (at your option) any --
-- later version.  GNAT Metrics Toolset is  distributed in the hope that it --
-- will be useful, but  WITHOUT ANY WARRANTY; without even the implied war- --
-- ranty of  MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the --
-- GNU General Public License for more details.  You should have received a --
-- copy of the  GNU General Public License distributed with  GNAT; see file --
-- COPYING.  If not,  write to the  Free Software  Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- GNAT Metrics Toolset is maintained by AdaCore - (http://www.adacore.com) --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;    use Ada.Characters.Handling;

with Asis.Compilation_Units;     use Asis.Compilation_Units;
with Asis.Extensions;            use Asis.Extensions;

package body METRICS.ASIS_Utilities is

   -----------------------
   -- Local subprograms --
   -----------------------

   CU_Profile_Buffer : String (1 .. 1024);
   CU_Buf_Len        : Natural;
   --  Buffer to form the beginning of CU_Profille result

   procedure Add (S : String);
   --  Appends S to CU_Profile_Buffer, moving Cu_Buf_Len accordingly

   ---------
   -- Add --
   ---------

   procedure Add (S : String) is
   begin
      --  The buffer is never supposed to be filled completely

      CU_Profile_Buffer (CU_Buf_Len + 1 .. CU_Buf_Len + S'Length) := S;
      CU_Buf_Len := CU_Buf_Len + S'Length;
   end Add;

   ----------------
   -- CU_Profile --
   ----------------

   function CU_Profile (CU : Compilation_Unit) return String is
      Arg_Kind        : constant Unit_Kinds   := Unit_Kind (CU);
      Arg_Class       : constant Unit_Classes := Unit_Class (CU);
      Is_Generic_Body : Boolean               := False;
   begin

      CU_Buf_Len := 0;

      if Arg_Class in A_Private_Declaration .. A_Private_Body then
         Add ("private ");
      end if;

      if Arg_Kind in A_Library_Unit_Body then
         Is_Generic_Body :=
           Unit_Kind (Corresponding_Declaration (CU)) in
           A_Generic_Unit_Declaration;
      end if;

      case Arg_Kind is
         when A_Procedure =>
            Add ("procedure ");
         when A_Function =>
            Add ("function ");
         when A_Package =>
            Add ("package ");
         when A_Generic_Procedure =>
            Add ("generic procedure ");
         when A_Generic_Function =>
            Add ("generic function ");
         when A_Generic_Package =>
            Add ("generic package ");
         when A_Procedure_Instance =>
            Add ("procedure instance ");
         when A_Function_Instance =>
            Add ("function instance ");
         when A_Package_Instance =>
            Add ("package instance ");
         when A_Procedure_Renaming =>
            Add ("procedure renaming ");
         when A_Function_Renaming =>
            Add ("function renaming ");
         when A_Package_Renaming =>
            Add ("package renaming ");
         when A_Generic_Procedure_Renaming =>
            Add ("generic procedure renaming ");
         when A_Generic_Function_Renaming =>
            Add ("generic function renaming ");
         when A_Generic_Package_Renaming =>
            Add ("generic package renaming ");

         when A_Procedure_Body =>

            if Is_Generic_Body then
               Add ("generic ");
            end if;

            Add ("procedure body ");

         when A_Function_Body =>

            if Is_Generic_Body then
               Add ("generic ");
            end if;

            Add ("function body ");

         when A_Package_Body =>

            if Is_Generic_Body then
               Add ("generic ");
            end if;

            Add ("package body ");

         when A_Procedure_Body_Subunit =>
            Add ("procedure body subunit ");
         when A_Function_Body_Subunit =>
            Add ("function body subunit ");
         when A_Package_Body_Subunit =>
            Add ("package body subunit ");
         when A_Task_Body_Subunit =>
            Add ("task body subunit ");
         when A_Protected_Body_Subunit =>
            Add ("protected body subunit ");

         when others =>
            null;
      end case;

      return CU_Profile_Buffer (1 .. CU_Buf_Len) &
             To_String (Unit_Full_Name (CU));

   end CU_Profile;

   -----------------
   --  Is_Process --
   -----------------

   function Is_Process (El_Kind : Flat_Element_Kinds) return Boolean is
      Result : Boolean := False;
   begin

      case El_Kind is
         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Task_Body_Declaration      |
              An_Entry_Body_Declaration    =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Process;

   -------------------------------
   -- May_Contain_Program_Units --
   -------------------------------

   function May_Contain_Program_Units
     (El_Kind : Flat_Element_Kinds)
      return    Boolean
   is
   begin

      return False
        or else El_Kind = A_Task_Type_Declaration
        or else El_Kind = A_Protected_Type_Declaration
        or else El_Kind = A_Single_Task_Declaration
        or else El_Kind = A_Single_Protected_Declaration
        or else El_Kind = A_Package_Declaration
        or else El_Kind = A_Generic_Package_Declaration
        or else El_Kind = A_Package_Body_Declaration
        or else El_Kind = A_Procedure_Body_Declaration
        or else El_Kind = A_Function_Body_Declaration
        or else El_Kind = A_Task_Body_Declaration
        or else El_Kind = A_Protected_Body_Declaration
        or else El_Kind = An_Entry_Body_Declaration;

   end May_Contain_Program_Units;

end METRICS.ASIS_Utilities;
