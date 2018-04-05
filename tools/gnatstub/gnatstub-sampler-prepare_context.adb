------------------------------------------------------------------------------
--                                                                          --
--                           GNATSTUB COMPONENTS                            --
--                                                                          --
--     G N A T S T U B . S A M P L E R . P R E P A R E _ C O N T E X T      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2003-2014, Free Software Foundation, Inc.          --
--                                                                          --
-- Gnatstub  is  free  software;  you can  redistribute it and/or modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 3 or (at your option) any later --
-- version. Gnatstub is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING3. If not, --
-- go to http://www.gnu.org/licenses for a complete copy of the license.    --
--                                                                          --
-- Gnatstub  is  distributed  as a part of the ASIS implementation for GNAT --
-- (ASIS-for-GNAT).                                                         --
--                                                                          --
-- Gnatstub  was  originally  developed  by  Alexei  Kuchumov  as a part of --
-- collaboration  between  Software  Engineering  Laboratory  of  the Swiss --
-- Federal  Institute  of  Technology  in  Lausanne,  Switzerland, and  the --
-- Scientific  Research  Computer  Center  of the  Moscow State University, --
-- Russia.  This  work  was  supported  by  a grant from the Swiss National --
-- Science Foundation,  no 7SUPJ048247,  funding a project  "Development of --
-- ASIS for GNAT with industry quality".                                    --
--                                                                          --
-- Gnatstub  is  now  maintained  by  AdaCore (http://www.adacore.com).     --
--                                                                          --
------------------------------------------------------------------------------

--  This version of the body is for non-GNSA version

with ASIS_UL.Common;
with ASIS_UL.Compiler_Options;

separate (Gnatstub.Sampler)
procedure Prepare_Context is
   Success : Boolean := False;
begin
   if Tree_Exists and then Reuse_Tree then
      return;
   end if;

   Compile
     (File_Name,
      Arg_List.all,
      Success,
      GCC          => ASIS_UL.Common.Gcc_To_Call,
      Display_Call => Debug_Flag_C);

   if not Success then
      Error ("cannot create the tree file for " & File_Name.all);
      raise Parameter_Error;
   else
      Tree_Exists := True;
   end if;

   Asis.Implementation.Initialize ("-ws -sv");

   Associate
     (My_Context,
     "My_Context",
     "-C1 " & To_Wide_String (Tree_Name.all));

   Open (My_Context);

   if Debug_Flag_T then
      Print_Tree_Sources;
   end if;
end Prepare_Context;
