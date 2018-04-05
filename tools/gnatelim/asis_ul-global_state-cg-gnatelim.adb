------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS                            --
--                                                                          --
--     A S I S _ U L . G L O B A L _ S T A T E . C G . G N A T E L I M      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2016, AdaCore                     --
--                                                                          --
-- GNATELIM  is  free software;  you can  redistribute it and/or  modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 3 or (at your option) any later --
-- version. GNATELIM is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING3. If not, --
-- go to http://www.gnu.org/licenses for a complete copy of the license.    --
--                                                                          --
-- The original version  of  Gnatelim  was developed by  Alain  Le  Guennec --
-- It is now maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Gnatelim.Options;

package body ASIS_UL.Global_State.CG.Gnatelim is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Fix_Dispatching_Operation_Usage;
   --  The front-end implements the following policy for eliminating
   --  dispatching operations:
   --
   --    If an overriding dispatching primitive is eliminated then its parent
   --    must have been eliminated
   --
   --  gnatelim is more intelligent and it does not mark a dispatching
   --  primitive as used if there are only non-dispatching calls to its parent.
   --  This procedure implements front-end policy and marks each dispatching
   --  operation that has used parent as used.

   procedure Mark_As_Used (N : GS_Node_Id);
   --  Mark the node as used.

   procedure Mark_List_As_Used (L : Node_Lists.Set);
   --  Marks all the nodes in the argument set as used

   -------------------------------------
   -- Fix_Dispatching_Operation_Usage --
   -------------------------------------

   procedure Fix_Dispatching_Operation_Usage is
      First_Pass    : Boolean := True;
      More_Analysis : Boolean := False;

      Next_Impl_Subpr : Node_Lists.Cursor;
      Next_Impl_Node  : GS_Node_Id;
   begin

      if not Standard.Gnatelim.Options.Eliminate_Dispatching_Operations then
         --  Nothing to do, so
         return;
      end if;

      while More_Analysis or else First_Pass loop
         More_Analysis := False;

         for J in First_GS_Node .. Last_Node loop

            if Is_Dispatching_Operation_Node (J)
              and then
               Is_Used (J)
            then
               Next_Impl_Subpr := Node_Lists.First (Table (J).Node_List_3);

               while Node_Lists.Has_Element (Next_Impl_Subpr) loop
                  Next_Impl_Node := Node_Lists.Element (Next_Impl_Subpr);

                  if not Is_Used (Next_Impl_Node) then
                     More_Analysis := True;
                     Mark_As_Used (Next_Impl_Node);
                     Mark_List_As_Used (Table (Next_Impl_Node).Node_List_1);
                  end if;

                  Next_Impl_Subpr := Node_Lists.Next  (Next_Impl_Subpr);
               end loop;

            end if;

         end loop;

         First_Pass := False;
      end loop;

   end Fix_Dispatching_Operation_Usage;

   -------------
   -- Is_Used --
   -------------

   function Is_Used (N : GS_Node_Id) return Boolean renames
     ASIS_UL.Global_State.Get_Application_Flag_1;

   ------------------
   -- Mark_As_Used --
   ------------------

   procedure Mark_As_Used (N : GS_Node_Id) is
   begin
      pragma Assert (Present (N) and then GS_Node_Kind (N) in Callable_Nodes);
      Set_Application_Flag_1 (N, True);
   end Mark_As_Used;

   -----------------------
   -- Mark_List_As_Used --
   -----------------------

   procedure Mark_List_As_Used (L : Node_Lists.Set) is
      Next_Node : Node_Lists.Cursor;
   begin
      Next_Node := Node_Lists.First (L);

      while Node_Lists.Has_Element (Next_Node) loop
         Mark_As_Used (Node_Lists.Element (Next_Node));
         Next_Node := Node_Lists.Next (Next_Node);
      end loop;

   end Mark_List_As_Used;

   ---------------------------
   -- Mark_Used_Subprograms --
   ---------------------------

   procedure Mark_Used_Subprograms is
   begin
      --  Some subprograms can be marked as used because of different patches
      --  corresponding to frontend/gnatelim limitations (for example, see
      --  ASIS_UL.Global_State.CG.Patch_For_Default_Parameter_Initialization).
      --  But it has been done before call graph completion (and before call
      --  graph transitive closuer). If a subprogram called by such
      --  "already known as used" is called only by this node, and this node
      --  also is not called in a normal way by main subprogram, the called
      --  node will not be marked as used by traversing the list of nodes
      --  called by main subprogram (see J123-004). So we have to mark as
      --  used all the subprogrrams called by "already known as used":

      for J in First_GS_Node .. Last_Node loop
         if Is_Used (J) then
            Mark_List_As_Used (Table (J).Node_List_1);
         end if;
      end loop;

      Mark_List_As_Used (Table (Environment_Task_Node).Node_List_1);

      for J in First_GS_Node .. Last_Node loop

         if (not Standard.Gnatelim.Options.Eliminate_Dispatching_Operations
            and then
             Is_Dispatching_Operation_Node (J) and then not Is_Used (J))
          or else
            GS_Node_Kind (J) in
               A_Type_Discr_Init_Procedure | A_Type_Init_Procedure
          or else
            GS_Node_Kind (J) in A_Task | A_Task_Entry | A_Protected_Entry
         then
            Mark_As_Used (J);
            Mark_List_As_Used (Table (J).Node_List_1);
         end if;

      end loop;

      Fix_Dispatching_Operation_Usage;

   end Mark_Used_Subprograms;

end ASIS_UL.Global_State.CG.Gnatelim;
