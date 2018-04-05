.. _Adding_the_Results_of_Compiler_Checks_to_gnatcheck_Output:

***********************************************************
Adding the Results of Compiler Checks to *gnatcheck* Output
***********************************************************

The *gnatcheck* tool can include in the generated diagnostic messages
and in
the report file the results of the checks performed by the compiler. Though
disabled by default, this effect may be obtained by using ``+R`` with
the following rule identifiers and parameters:



*Restrictions*
  To record restrictions violations (which are performed by the compiler if the
  pragma ``Restrictions`` or ``Restriction_Warnings`` are given),
  use the ``Restrictions`` rule
  with the same parameters as pragma
  ``Restrictions`` or ``Restriction_Warnings``.

  This rule allows parametric rule exemptions, the parameters
  that are allowed in the definition of exemption sections are
  the names of the restrictions except for the case when a restriction
  requires a non-numeric parameter, in this case the parameter should be
  the name of the restriction with the parameter, as it is given for the
  rule.


*Style_Checks*
  To record compiler style checks
  (see Style Checking section in
  GNAT User's Guide),
  use the
  ``Style_Checks`` rule.

  This rule takes a parameter in one of the following forms:

  *
    *All_Checks*,
        which enables the standard style checks corresponding to the ``-gnatyy``
        GNAT style check option, or

  *
    a string with the same
    structure and semantics as the ``string_LITERAL`` parameter of the
    GNAT pragma ``Style_Checks``
    (see "Pragma Style_Checks" in the GNAT Reference Manual).

  For example, the
  ``+RStyle_Checks:O`` rule option activates
  the compiler style check that corresponds to
  ``-gnatyO`` style check option.


*Warnings*
  To record compiler warnings
  (see Warning Message Control section in
  GNAT User's Guide),
  use the
  ``Warnings`` rule with a parameter that is a valid
  *static_string_expression* argument of the GNAT pragma ``Warnings``
  (see "Pragma Warnings" in the GNAT Reference Manual).
  Note that in case of gnatcheck
  's' parameter, that corresponds to the GNAT ``-gnatws`` option, disables
  all the specific warnings, but not suppresses the warning mode,
  and 'e' parameter, corresponding to ``-gnatwe`` that means
  "treat warnings as errors", does not have any effect.

  This rule allows parametric rule exemptions, the parameters
  that are allowed in the definition of exemption sections are the
  same as the parameters of the rule itself. Note that parametric
  exemption sections have their effect only if either ``.d`` parameter is
  specified for the ``Warnings`` rule or if the ``--show-rules`` option
  is set.


To disable a specific restriction check, use ``-RRestrictions`` gnatcheck
option with the corresponding restriction name as a parameter. ``-R`` is
not available for ``Style_Checks`` and ``Warnings`` options, to disable
warnings and style checks, use the corresponding warning and style options.
