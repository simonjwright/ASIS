------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--    G N A T P P . P R O C E S S I N G . P R E P A R E _ C O N T E X T     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2013, AdaCore                     --
--                                                                          --
-- GNATPP is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNATPP is  distributed in the  hope that it will  be  useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or  FITNESS  FOR A  PARTICULAR  PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write to the Free Software Foundation,  51 Franklin Street, Fifth Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com).                --
--                                                                          --
------------------------------------------------------------------------------

--  This version of Prepare_Context is supposed to be used for the non
--  GNSA-based gnatpp version

with Asis.Ada_Environments;    use Asis.Ada_Environments;
with Asis.Errors;
with Asis.Exceptions;          use Asis.Exceptions;

with ASIS_UL.Common;
with ASIS_UL.Compiler_Options; use ASIS_UL.Compiler_Options;

separate (GNATPP.Processing)
procedure Prepare_Context (SF : SF_Id; Success : out Boolean) is
   use type Asis.Errors.Error_Kinds;
begin

   Compile
     (new String'(Source_Name (SF)),
      Arg_List.all,
      Success,
      GCC => ASIS_UL.Common.Gcc_To_Call);

   if Success then

      Associate (The_Context => The_Context,
                 Name        => "",
                 Parameters  =>
                  "-C1 " & To_Wide_String (Suffixless_Name (SF) & ".adt"));

      Open (The_Context);
   end if;

exception
   when Version_Mismatch =>
      raise;

   when Ex : ASIS_Failed =>
      --  The only known situation when we can not open a C1 context for
      --  newly created tree is recompilation of System (see D617-017)

      if Asis.Implementation.Status = Asis.Errors.Use_Error
        and then
         Asis.Implementation.Diagnosis = "Internal implementation error:"
         & " Asis.Ada_Environments.Open - System is recompiled"
      then
         Put (Standard_Error, "gnatpp: can not process redefinition of " &
                 "System in "& Source_Name (SF));
         New_Line (Standard_Error);
         Success := False;
      else
         Put (Standard_Error, "gnatpp: unexpected bug when opening a context");
         Put ("(" & Source_Name (SF) & ")");
         New_Line (Standard_Error);
         GNATPP.Output.Report_Unhandled_ASIS_Exception (Ex);
         raise Fatal_Error;
      end if;

   when Ex : others =>
      Put (Standard_Error, "gnatpp: unexpected bug when opening a context");
      Put ("(" & Source_Name (SF) & ")");
      New_Line (Standard_Error);
      GNATPP.Output.Report_Unhandled_Exception (Ex);
      raise Fatal_Error;
end Prepare_Context;
