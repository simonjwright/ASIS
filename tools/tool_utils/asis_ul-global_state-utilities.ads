------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--       A S I S _ U L . G L O B A L _ S T A T E . U T I L I T I E S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

--  This package defines various utility programs needed for the implementation
--  of the data strucrure representing the progtam global state

package ASIS_UL.Global_State.Utilities is

   function Can_Create_Tasks (El : Asis.Element) return Boolean;
   --  Checks if the execution or elaboration of its argument can cause one or
   --  more tasks to be created. At the moment two cases are considered as
   --  (potential) task creations - object declarations and allocators (in both
   --  cases the type of object/value can be a task type or it can contain
   --  task components).

   function Corresponding_Element (El : Asis.Element) return Asis.Element;
   --  For the argument Element, computes the element that is used as the basis
   --  for representation of the given entity in the data structures
   --  representing the program global state. The rules for defining the
   --  corresponding element are:
   --
   --  * for a task       - the corresponding single task declaration or task
   --                       type declaration
   --
   --  * for a package    - the corresponding package declaration
   --
   --  * for a subprogram - if a separate declaration is given, the
   --                       corresponding element is the declaration, otherwise
   --                       the subprogram body, except it is a proper body for
   --                       a subunit, in this case the corresponding element
   --                       is either the corresponding declaration, if
   --                       present, or the body stub
   --
   --  * for a subprogram instantiation - the instantiation itself
   --
   --  * for an expanded subprogram spec or body corresponding to a subprogram
   --    instantiation - the corresponding instantiation
   --
   --  * for a subprogram renaming:
   --     - the Corresponding_Element of the renamed subprogram (or entry), if
   --       the renaming can be resolved statically to some subprogram;
   --
   --     - An_Attribute_Reference Element if the renaming renames an
   --       attribite subprogram
   --
   --     - An_Enumeration_Literal Element if the renaming renames an
   --       enumeration literal
   --
   --     - Nil_Element for all the other cases
   --
   --  * for entry body       - the corresponding entry declaration;
   --
   --  * for accept statement - the corresponding entry declaration;
   --
   --  * for ???  - to be extended...
   --
   --  If the argument Element does not correspond to any of the cases
   --  described above, it is returned as the result.

   function Enclosing_Scope (El : Asis.Element) return Asis.Element;
   --  Returns the closest Is_Scope Element that encloses the argument Element

   function Get_Renamed_Subprogram (El : Asis.Element) return Asis.Element;
   --  Assuming that El is a subprogram renaming element, tries to get the
   --  declaration of renamed callable entity by unwinding renamings. Returns
   --  Nil_Element if the renamed subprogram cannot be determined statically.

   function Get_Defining_Name (El : Asis.Element) return Asis.Element;
   --  Assuming that El is an Element wrom which a node in the global structure
   --  can be created, returns the defining name that is a part of this
   --  element.

   function Can_Create_Reference_To_Subprogram
     (El   : Asis.Element)
      return Boolean;
   --  Checks if El is a construct that can create a reference to subprogram
   --  then can be used for undirect subprogram calls. At the momnent this
   --  function return true if El is an attribute reference returning access or
   --  address value. This function does not check if the prefix of the
   --  attribute denotes a subprogram

   function Has_Discr_Init_Proc (El : Asis.Element) return Boolean;
   --  Checks if El is a type declaration that contains known discriminant
   --  part with default initializations.

   function Has_Type_Init_Proc (El : Asis.Element) return Boolean;
   --  Checks if El is a type declaration for that we may consider an
   --  initialization procedure that may be of interest for a call graph.
   --  At the moment we consider the following cases:
   --
   --  - a type declaration contains default initialization expression, and
   --    this expression contain a call to some function that is a componet of
   --    the call graph;
   --
   --  - a type contains a component that has a type for that the
   --    initialization procedure of interest exists;
   --
   --  In case of a private type or private extension, this function returns
   --  True for full declarations only.

   function Implemented_Operations
     (Op   : Asis.Element)
      return Asis.Element_List;
   --  Assumes that Op is a declaration of a dispatching operation. Returns the
   --  list of operations that are "directly "implemented" by the given
   --  operation (it can be a list in case of multiple inheritance)

   function Is_Call (El : Asis.Element) return Boolean;
   --  Checks if El is a call that is of interest for the call graph.

   function Is_Call_To_Default_Null_Procedure
     (El   : Asis.Element)
      return Boolean;
   --  Checks if El is a call to a default null procedure that corresponds
   --  to a formal procedure that has null default.

   function Is_Call_To_Predefined_Operation
     (Call : Asis.Element)
      return Boolean;
   --  Checks if the argument is a call to a predefined operation

   function Is_Renaming_Of_Null_Proc_Default
     (El   : Asis.Element)
      return Boolean;
   --  Checks if El is a renaming declaration that renames a default actual
   --  for null formal procedure. May be useful to detect situations when
   --  formal subprogram with null default is used to instantiate another
   --  generic inside the template code.

   function Is_Predefined_Operation_Renaming
     (Ren  : Asis.Element)
      return Boolean;
   --  Checks if the argument is a renaming of a predefined operation (returns
   --  true for undirect predefined operators renamings as well)

   function Is_Declaration_Of_Callable_Entity
     (El   : Asis.Element)
      return Boolean;
   --  Checks if El is a declaration of a callable entity that is of interest
   --  for call graph analysis. This function does not consider subprogram
   --  renamings as being declarations of callable entities - renamings are
   --  processed separately. Bodies (even if they act as specs) are also not
   --  considered as declarations of callable entities.
   --
   --  This function assumes, that it is called only in the source context that
   --  is of interest for our analyzis. That is, if it see a function
   --  declaration, it does not care if it is declared inside a generic - such
   --  possibilities are supposed to be filtered out by high-level traversal
   --  control.

   function Is_Non_Executable_Construct (El : Asis.Element) return Boolean;
   --  Checks if its argument is a non-executable construct. We are interested
   --  in constructs that can contain subprogram calls but that can never
   --  result in issuing real calls (generic declarations and bodies, type
   --  declarations, parameter profiles)

   function Is_Stream_Attribute_Redefinition
     (Element : Asis.Element)
      return    Boolean;
   --  Checks if Element represents an attribute redefinition clause that
   --  defines one of stream attributes (Write, Read, Output, Input)

   function Is_Scope (El : Asis.Element) return Boolean;
   --  Checks if El is a scope in the Call Graph sense - that is, an executable
   --  body of an entity that can be called by another entities and that itself
   --  can call other entities. We also consider as scopes library-level
   --  packages and library-level package instantiations.
   --  What about task entries and their accept statements?????????

   function Can_Be_Embedded_In_Equiality (El : Asis.Element) return Boolean;
   --  Checks if the argument is a user redefinition of a predefined "=" that
   --  can be embedded in the implementation of some predefined "=". See
   --  ARM12 3.4 (17/2) and 4.5.2 (14/3 .. 15/3)

end ASIS_UL.Global_State.Utilities;
