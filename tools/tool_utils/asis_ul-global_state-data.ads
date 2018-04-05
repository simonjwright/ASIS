------------------------------------------------------------------------------
--                                                                          --
--                  COMMON ASIS TOOLS COMPONENTS LIBRARY                    --
--                                                                          --
--           A S I S _ U L . G L O B A L _ S T A T E . D A T A              --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2010-2016, AdaCore                     --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not,  write  to the  Free Software Foundation,  51 Franklin  --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  The package defines basic data structures for storing the global state
--  of the analyzed sources

pragma Ada_2012;

with Asis;

package ASIS_UL.Global_State.Data is

   function Is_Global_For
     (Scope : Scope_Id;
      Node  : GS_Node_Id)
      return  Boolean;
   --  Checks if Node is global for Scope (the actual for Scope is supposed
   --  to be a scope node). If Node corresponds to library-level data object,
   --  this function returns True.

   function Is_Global_For_Current_Scope
     (Def_Name : Asis.Element)
      return     Boolean;
   --  Checks if Def_Name (that is supposed to be of a Defining_Identifier
   --  kind) is global for the current scope.

   procedure Store_Reference
     (N              : GS_Node_Id;
      At_SLOC        : String_Loc;
      Reference_Kind : Reference_Kinds);
   --  Stores in the global structure the arc(s) representing that the Current
   --  Scope refers At_SLOC to the global data object N. This procedure assumes
   --  that Present (N), Note that arcs are stored for both scope and data
   --  nodes!

   procedure Check_If_Global_Reference
     (Element                       :     Asis.Element;
      Definition                    : out Asis.Element;
      Is_Global_Reference           : out Boolean;
      Can_Be_Accessed_By_Local_Task : out Boolean;
      Reference_Kind                : out Reference_Kinds;
      Compute_Reference_Kind        :     Boolean := False);
   --  Assuming that Element is an identifier element, checks if it is a
   --  reference to a variable that is a global variable for the current scope.
   --  If it is, Is_Global_Reference is set ON (Can_Be_Accessed_By_Local_Task
   --  is set OFF).
   --  In case if a given data object can be accessed by a task enclosed in
   --  the scope where the object is declared, we set Is_Global_Reference OFF
   --  (Is_Global_Referenceis set OFF),
   --  If either Is_Global_Reference or Can_Be_Accessed_By_Local_Task is ON,
   --  Definition is set to the defining identifier of this variable, and if
   --  Compute_Reference_Kind is ON, Reference_Kind represents if the reference
   --  is read, write or read-write.
   --  If both Is_Global_Reference and Can_Be_Accessed_By_Local_Task are OFF,
   --  Definition and Reference_Kind are indefinite.

   procedure Process_Global_Reference
     (Element                           : Asis.Element;
      Definition                        : Asis.Element;
      Reference_Kind                    : Reference_Kinds);
   --  Assuming that Element is a reference to a global variable for the
   --  current scope, and Definition is the corresponding defining name
   --  element, stores the information about the reference in the global
   --  structure according to Reference_Kind value. If the scope node is a task
   --  node or a foreign thread node, the information is stored for both data
   --  and scope nodes, othervise it is stored for the scope node only. The
   --  last paramenet tells if we have a reference to a local variable that can
   --  potentially be accessed (as non-local variable) by enclosed tasks, this
   --  information has to be stored for further call graph analysis.

end ASIS_UL.Global_State.Data;
