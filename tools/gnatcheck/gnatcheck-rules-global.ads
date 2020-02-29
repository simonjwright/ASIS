------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--               G N A T C H E C K . R U L E S . G L O B A L                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

--  This package defines gnatcheck global rules.
--
--  The rules in this packages are ordered alphabetically

pragma Ada_2012;

package Gnatcheck.Rules.Global is

   ----------------------------
   -- Deeply_Nested_Inlining --
   ----------------------------

   --  Flags a subprogram if a pragma Inline is applied to the corresponding
   --  subprogram (or generic subprogram in case if a flagged subprogram is a
   --  generic instantiation) and the subprogram body contains a call to
   --  another inlined subprogram that results in nested inlining with nesting
   --  depth more than N, where N is a rule parameter. This rule is similar to
   --  Deeply_Nested_Local_Inlining rule, but it assumes that calls to
   --  subprograms in with'ed units are inlined if at the place of the call the
   --  corresponding Inline pragma is "visible". This rule may be useful for
   --  the case when either '-gnatn' or '-gnatN' option is used when building
   --  the executable.
   --
   --  If a subprogram should be flagged according to this rule, and a separate
   --  spec is given for the subprogram, then only the spec is flagged, but the
   --  body is not flagged.
   --
   --  This rule requires the global analysis of all the set of compilation
   --  units that are gnatcheck arguments, that may affect performance
   --
   --  The rule has the following parameters:
   --
   --  * for +R option:
   --
   --      N - N is a positive integer specifying the maximal allowed depth of
   --          nested inlining

   type Deeply_Nested_Inlining_Rule_Type is new Global_Rule_Template
     with record
        Rule_Limit : Positive;
     end record;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Deeply_Nested_Inlining_Rule_Type);
   --  Activates the rule with the parameter equals to 3.

   overriding procedure Process_Rule_Parameter
    (Rule       : in out Deeply_Nested_Inlining_Rule_Type;
     Param      :        String;
     Enable     :        Boolean;
     Defined_At : String);
   --  Does exactly the same as Process_Rule_Parameter for
   --  One_Positive_Parameter_Rule_Template. Does any better solution then
   --  code duplication exist here???

   overriding procedure Init_Global_Structure
     (Rule : Deeply_Nested_Inlining_Rule_Type);
   --  Sets the condition on call graph creation: only inlined subprograms
   --  should be represented.

   overriding procedure Collect_Global_Info_Pre_Op
    (Rule    : in out Deeply_Nested_Inlining_Rule_Type;
     Element :        Asis.Element;
     Control : in out Traverse_Control;
     State   : in out Rule_Traversal_State);
   --  If Element is a subprogram, and it is inlined, marks the corresponding
   --  call graph node as being inlined.

   overriding procedure Check_Global_Structure_Node
    (Rule          :     Deeply_Nested_Inlining_Rule_Type;
     N             :     ASIS_UL.Global_State.GS_Node_Id;
     Detected      : out Boolean);
   --  Checks of the node has a chain of called inlined nodes longer then N.

   overriding procedure Init_Rule
     (Rule : in out Deeply_Nested_Inlining_Rule_Type);

   overriding procedure Print_Rule_To_File
     (Rule         : Deeply_Nested_Inlining_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : Deeply_Nested_Inlining_Rule_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Rule_Help
     (Rule  : Deeply_Nested_Inlining_Rule_Type;
      Level : Natural);

   overriding function Rule_Option
     (Rule          : Deeply_Nested_Inlining_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return          String;
   --  For Template_All_ON prints out rule option in the format
   --  "+R  rule_name : 1"

   overriding function More_Rule_Comment
     (Rule          : Deeply_Nested_Inlining_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return          String;
   --  Adds the note that the rule parameter may be junk so it needs
   --  revising.

   Deeply_Nested_Inlining_Rule :
     aliased Deeply_Nested_Inlining_Rule_Type;

   ---------------------------
   -- Recursive_Subprograms --
   ---------------------------

   --  Recursion (cycles in the call graph) is detected. Only declarations of
   --  recursive subprograms but not calls to recursive subprograms are
   --  flagged. In case of indirect recursion, the call chain resulted in
   --  recursion is not reported. Task entities are not considered by this
   --  rule.
   --
   --  The rule has the following optional parameter for +R option:
   --
   --    Skip_Dispatching_Calls - do not take into account dispatching calls
   --                             when creating and analyzing the call
   --                             graph

   type Recursive_Subprograms_Rule_Type is new Global_Rule_Template with
     record
         Skip_Dispatching_Calls : Boolean := False;
     end record;

   overriding procedure Process_Rule_Parameter
    (Rule       : in out Recursive_Subprograms_Rule_Type;
     Param      :        String;
     Enable     :        Boolean;
     Defined_At : String);

   overriding procedure Print_Rule_To_File
     (Rule         : Recursive_Subprograms_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : Recursive_Subprograms_Rule_Type;
      Indent_Level : Natural := 0);

   procedure Init_Global_Structure (Rule : Recursive_Subprograms_Rule_Type);
   --  Sets unconditional mode for call graph creation, sets
   --  Do_Transitive_Closure ON

   procedure Check_Global_Structure_Node
     (Rule          :     Recursive_Subprograms_Rule_Type;
      N             :     ASIS_UL.Global_State.GS_Node_Id;
      Detected      : out Boolean);
   --  Checks if the given call graph node calls itself.

   procedure Init_Rule (Rule : in out Recursive_Subprograms_Rule_Type);

   Recursive_Subprograms_Rule : aliased Recursive_Subprograms_Rule_Type;

   ---------------------------
   -- Side_Effect_Functions --  turned off!!! --
   ---------------------------

   --  Functions with side effect are detected.

   --  We define a side effect as changing any data object that is not local
   --  for the body of this function.

   --  At the moment, we do NOT consider a side effect any input-output
   --  operations (changing a state or a content of any file).

   --  We do not consider protected functions for this rule (???)

   --  There are the following sources of side effect:

   --  1. Explicit (or direct) side-effect:
   --
   --     - direct assignment to a non-local variable;
   --
   --     - direct call to an entity that is known to change some data object
   --       that is not local for the body of this function (Note, that if F1
   --       calls F2 and F2 does have a side effect, this does not
   --       automatically mean that F1 also have a side effect, because it may
   --       be the case that F2 is declared in F1's body and it changes some
   --       data object that is global for F2, but local for F1);

   --  2. Indirect side-effect:
   --
   --     - Subprogram calls implicitly issued by:
   --         - computing initialization expressions from type declarations as
   --           a part of object elaboration or allocator evaluation;
   --         - computing implicit parameters of subprogram or entry calls or
   --           generic instantiations;
   --
   --     - activation of a task that change some non-local data object
   --       (directly or indirectly);
   --
   --     - elaboration code of a package that is a result of a package
   --       instantiation;
   --
   --     - controlled objects;

   --  3. Situations when we can suspect a side-effect, but the full static
   --     check is either impossible or too hard:
   --
   --     - assignment to access variables or to the objects pointed by access
   --       variables;
   --
   --     - call to a subprogram pointed by access-to-subprogram value
   --
   --     - dispatching calls;

--   type Side_Effect_Functions_Rule_Type is new Global_Rule_Template
--     with null record;

--   procedure Collect_Global_Info_Pre_Op
--     (Rule    : in out Side_Effect_Functions_Rule_Type;
--      Element :        Asis.Element;
--      Control : in out Traverse_Control;
--      State   : in out Rule_Traversal_State);
   --  Checks that a given construct may cause a side effect for enclosing
   --  scope.

--   procedure Collect_Global_Info_Post_Op
--     (Rule    : in out Side_Effect_Functions_Rule_Type;
--      Element :        Asis.Element;
--      Control : in out Traverse_Control;
--      State   : in out Rule_Traversal_State);
   --  Checks if we can consider a side effect to be fully defined for a given
   --  scope. This is only a partial check, because we may not know the side
   --  effect status for callees. But in case if we can define that a side
   --  effect for a given scope is already known for sure, we do the
   --  corresponding settings in the global structure to speed up
   --  post-transitive-closure processing.

--   procedure Analyze_Global_Structure
--     (Rule : Side_Effect_Functions_Rule_Type);
   --  Traverses the call graph call chains (after making the transitive
   --  closure) and tries to resolve all the situations where we have a side
   --  effect not defined completely because a given entity calls some entity
   --  for that the side effect was not completely known at the moment of
   --  the traversing of the code of the first entity.
   --  ??? Recursive chains?

--   procedure Check_Global_Structure_Node
--     (Rule     :     Side_Effect_Functions_Rule_Type;
--      N        :     Gnatcheck.Global_State.GS_Node_Id;
--      Detected : out Boolean);
   --  Checks if for the given scope entity we have a side effect.

--   procedure Init_Rule (Rule : in out Side_Effect_Functions_Rule_Type);

--   Side_Effect_Functions_Rule : aliased Side_Effect_Functions_Rule_Type;

end Gnatcheck.Rules.Global;
