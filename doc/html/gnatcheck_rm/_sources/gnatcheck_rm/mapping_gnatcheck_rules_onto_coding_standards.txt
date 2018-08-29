.. _Mapping_gnatcheck_Rules_Onto_Coding_Standards:

***********************************************
Mapping *gnatcheck* Rules Onto Coding Standards
***********************************************

If a user would like use *gnatcheck* to check if some code
satisfies to a given coding standard, the following approach can be
used to simplify mapping of the coding standard requirements onto
*gnatcheck* rules:

*

   when specifying rule options, use synonyms for the rule names
   that are relevant to your coding standard:

   ::

     +R :My_Coding_Rule_1: Gnatcheck_Rule_1: param1
     ...
     +R :My_Coding_Rule_N: Gnatcheck_Rule_N

*

   call *gnatcheck* with `--show-rule` option that adds the rule names
   to the generated diagnoses. If a synonym is used in the rule option that
   enables the rule, then this synonym will be used to annotate the diagnosis
   instead of the rule name:

   ::

     foo.adb:2:28: something is wrong here [My_Coding_Rule_1]
     ...
     bar.ads:17:3: this is not good [My_Coding_Rule_N]

Currently this approach does not work for compiler-based checks integrated
in *gnatcheck* (implemented by `Restrictions`, `Style_Checks` and
`Warnings` rules.
