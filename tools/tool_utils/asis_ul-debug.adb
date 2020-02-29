------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                        A S I S _ U L . D E B U G                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2009-2019, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

package body ASIS_UL.Debug is

   ---------------------------------
   -- Summary of Debug Flag Usage --
   ---------------------------------

   --  da  not used yet
   --  db  not used yet
   --  dc  output calls to other executables issued by a tool
   --  dd  progress indicator mode
   --  de  not used yet
   --  df  not used yet
   --  dg  print out debug image of global structure (all nodes)
   --  dh  not used yet
   --  di  not used yet
   --  dj  not used yet
   --  dk  not used yet
   --  dl  last chance debug info
   --  dm  not used yet
   --  dn  keep temporary files
   --  do  not used yet
   --  dp  not used yet
   --  dq  not used yet
   --  dr  not used yet
   --  ds  print out debug image of source table (all sources)
   --  dt  print out list of units used to create a tree
   --  du  not used yet
   --  dv  verbose
   --  dw  not used yet
   --  dx  not used yet
   --  dy  not used yet
   --  dz  not used yet

   --  dA   not used yet
   --  dB   not used yet
   --  dC   not used yet
   --  dD   not used yet
   --  dE   not used yet
   --  dF   not used yet
   --  dG   not used yet
   --  dH   not used yet
   --  dI   not used yet
   --  dJ   gnatcheck  : similar to gnat -gnatdJ option
   --  dK   not used yet
   --  dL   not used yet
   --  dM   not used yet
   --  dN   not used yet
   --  dO   not used yet
   --  dP   not used yet
   --  dQ   not used yet
   --  dR   gnatcheck  : indicate remediation level in brief rule help (-h)
   --  dS   not used yet
   --  dT   not used yet
   --  dU   not used yet
   --  dV   not used yet
   --  dW   not used yet
   --  dX   not used yet
   --  dY   not used yet
   --  dZ   not used yet

   -----------------------------------
   -- Documentation for Debug Flags --
   -----------------------------------

   ------------------------
   --  Low case letters: --
   ------------------------

   --  da   not used yet

   --  db   not used yet

   --  dc   print out the command used to generate the tree, and other calls
   --       to other executables issued by a tool,  with full switches and
   --       arguments

   --  dd   print out information for GPS progress indicator

   --  de   not used yet

   --  df   not used yet

   --  dg   print out debug image of global structure (all nodes)

   --  dh   not used yet

   --  di   not used yet

   --  dj   not used yet

   --  dk   not used yet

   --  dl   in the outermost exeption handlers, print out all the applicable
   --       debug information corresponding to other debug flags.

   --  dm   not used yet

   --  dn   do not delete the temporary working directory and all the generated
   --       temporary files (except tree and ALI files)

   --  do   not used yet

   --  dp   not used yet

   --  dq   not used yet

   --  dr   not used yet

   --  ds   print out debug image of source table (all sources - specifid as
   --       tool argument and added in the process of the tool run). Also print
   --       out all the file traces when filing in source table (with full
   --       absolute pathnames

   --  dt   print out the full list of Ada source files used to create the tree
   --       file for ASIS. Source file names contain full path information in
   --       absolure form.

   --  du   not used yet

   --  dv   Verbose. This differs from -v in that it avoids printing full path
   --       names, version numbers, and the like, so can be used in regression
   --       tests.

   --  dw   not used yet

   --  dx   not used yet

   --  dy   not used yet

   --  dz   not used yet

   --------------------------
   --  Upper case letters: --
   --------------------------

   --  dA   not used yet
   --  dB   not used yet
   --  dC   not used yet
   --  dD   not used yet
   --  dE   not used yet
   --  dF   not used yet
   --  dG   not used yet
   --  dH   not used yet
   --  dI   not used yet

   --  dJ   gnatcheck  : similar to gnat -gnatdJ option: adds scope name after
   --                    SLOC into the generated diagnoses
   --       other tools: not used yet

   --  dK   not used yet
   --  dL   not used yet
   --  dM   not used yet
   --  dN   not used yet
   --  dO   not used yet
   --  dP   not used yet
   --  dQ   not used yet
   --  dR   gnatcheck  :  indicate remediation level in the output generated
   --                      with '-h' gnatcheck option
   --       other tools: not used yet

   --  dS   not used yet
   --  dT   not used yet
   --  dU   not used yet
   --  dV   not used yet
   --  dW   not used yet
   --  dX   not used yet
   --  dY   not used yet
   --  dZ   not used yet

   --------------------
   -- Set_Debug_Flag --
   --------------------

   procedure Set_Debug_Flag (C : Character; Val : Boolean := True) is
      subtype Dig  is Character range '1' .. '9';
      subtype LLet is Character range 'a' .. 'z';
      subtype ULet is Character range 'A' .. 'Z';

   begin
      if C in Dig then
         case Dig'(C) is
            when '1' => Debug_Flag_1 := Val;
            when '2' => Debug_Flag_2 := Val;
            when '3' => Debug_Flag_3 := Val;
            when '4' => Debug_Flag_4 := Val;
            when '5' => Debug_Flag_5 := Val;
            when '6' => Debug_Flag_6 := Val;
            when '7' => Debug_Flag_7 := Val;
            when '8' => Debug_Flag_8 := Val;
            when '9' => Debug_Flag_9 := Val;
         end case;
      elsif C in LLet then
         case LLet'(C) is
            when 'a' => Debug_Flag_A := Val;
            when 'b' => Debug_Flag_B := Val;
            when 'c' => Debug_Flag_C := Val;
            when 'd' => Debug_Flag_D := Val;
            when 'e' => Debug_Flag_E := Val;
            when 'f' => Debug_Flag_F := Val;
            when 'g' => Debug_Flag_G := Val;
            when 'h' => Debug_Flag_H := Val;
            when 'i' => Debug_Flag_I := Val;
            when 'j' => Debug_Flag_J := Val;
            when 'k' => Debug_Flag_K := Val;
            when 'l' => Debug_Flag_L := Val;
            when 'm' => Debug_Flag_M := Val;
            when 'n' => Debug_Flag_N := Val;
            when 'o' => Debug_Flag_O := Val;
            when 'p' => Debug_Flag_P := Val;
            when 'q' => Debug_Flag_Q := Val;
            when 'r' => Debug_Flag_R := Val;
            when 's' => Debug_Flag_S := Val;
            when 't' => Debug_Flag_T := Val;
            when 'u' => Debug_Flag_U := Val;
            when 'v' => Debug_Flag_V := Val;
            when 'w' => Debug_Flag_W := Val;
            when 'x' => Debug_Flag_X := Val;
            when 'y' => Debug_Flag_Y := Val;
            when 'z' => Debug_Flag_Z := Val;
         end case;

      elsif C in ULet then
         case ULet (C) is
            when 'A' => Debug_Flag_AA := Val;
            when 'B' => Debug_Flag_BB := Val;
            when 'C' => Debug_Flag_CC := Val;
            when 'D' => Debug_Flag_DD := Val;
            when 'E' => Debug_Flag_EE := Val;
            when 'F' => Debug_Flag_FF := Val;
            when 'G' => Debug_Flag_GG := Val;
            when 'H' => Debug_Flag_HH := Val;
            when 'I' => Debug_Flag_II := Val;
            when 'J' => Debug_Flag_JJ := Val;
            when 'K' => Debug_Flag_KK := Val;
            when 'L' => Debug_Flag_LL := Val;
            when 'M' => Debug_Flag_MM := Val;
            when 'N' => Debug_Flag_NN := Val;
            when 'O' => Debug_Flag_OO := Val;
            when 'P' => Debug_Flag_PP := Val;
            when 'Q' => Debug_Flag_QQ := Val;
            when 'R' => Debug_Flag_RR := Val;
            when 'S' => Debug_Flag_SS := Val;
            when 'T' => Debug_Flag_TT := Val;
            when 'U' => Debug_Flag_UU := Val;
            when 'V' => Debug_Flag_VV := Val;
            when 'W' => Debug_Flag_WW := Val;
            when 'X' => Debug_Flag_XX := Val;
            when 'Y' => Debug_Flag_YY := Val;
            when 'Z' => Debug_Flag_ZZ := Val;
         end case;

      end if;
   end Set_Debug_Flag;

   -----------------------
   -- Set_Debug_Options --
   -----------------------

   procedure Set_Debug_Options (Options : String) is
      pragma Assert (Options /= "");
   begin
      for J in Options'Range loop
         Set_Debug_Flag (Options (J));
      end loop;
   end Set_Debug_Options;

end ASIS_UL.Debug;
