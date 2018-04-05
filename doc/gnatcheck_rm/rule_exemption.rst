.. _Rule_exemption:

**************
Rule exemption
**************

.. index:: Rule exemption

One of the most useful applications of *gnatcheck* is to
automate the enforcement of project-specific coding standards,
for example in safety-critical systems where particular features
must be restricted in order to simplify the certification effort.
However, it may sometimes be appropriate to violate a coding standard rule,
and in such cases the rationale for the violation should be provided
in the source program itself so that the individuals
reviewing or maintaining the program can immediately understand the intent.

The *gnatcheck* tool supports this practice with the notion of
a 'rule exemption' covering a specific source code section. Normally
rule violation messages are issued both on :file:`stderr`
and in a report file. In contrast, exempted violations are not listed on
:file:`stderr`; thus users invoking *gnatcheck* interactively
(e.g. in its GPS interface) do not need to pay attention to known and
justified violations. However, exempted violations along with their
justification are documented in a special section of the report file that
*gnatcheck* generates.

.. _Using_pragma_``Annotate``_to_Control_Rule_Exemption:

Using pragma ``Annotate`` to Control Rule Exemption
===================================================

.. index:: Using pragma Annotate to control rule exemption

Rule exemption is controlled by pragma ``Annotate`` when its first
argument is 'gnatcheck'. The syntax of *gnatcheck*'s
exemption control annotations is as follows:


::

  pragma Annotate (gnatcheck, exemption_control, Rule_Name [, justification]);

  exemption_control ::= Exempt_On | Exempt_Off

  Rule_Name         ::= string_literal

  justification     ::= string_literal

When a *gnatcheck* annotation has more than four arguments,
*gnatcheck* issues a warning and ignores the additional arguments.
If the arguments do not follow the syntax above,
*gnatcheck* emits a warning and ignores the annotation.

The ``Rule_Name`` argument should be the name of some existing
*gnatcheck* rule.
Otherwise a warning message is generated and the pragma is
ignored. If ``Rule_Name`` denotes a rule that is not activated by the given
*gnatcheck* call, the pragma is ignored and no warning is issued. The
exception from this rule is that exemption sections for ``Warnings`` rule are
fully processed when ``Restrictions`` rule is activated.

A source code section where an exemption is active for a given rule is
delimited by an ``exempt_on`` and ``exempt_off`` annotation pair:


.. code-block:: ada

  pragma Annotate (gnatcheck, Exempt_On, "Rule_Name", "justification");
  -- source code section
  pragma Annotate (gnatcheck, Exempt_Off, "Rule_Name");


For some rules it is possible specify rule parameter(s) when defining
an exemption section for a rule. This means that only the checks
corresponding to the given rule parameter(s) are exempted in this section:


.. code-block:: ada

  pragma Annotate (gnatcheck, Exempt_On, "Rule_Name: Par1, Par2", "justification");
  -- source code section
  pragma Annotate (gnatcheck, Exempt_Off, "Rule_Name: Par1, Par2");


A parametric exemption section can be defined for a rule if a rule has
parameters and these parameters change the scope of the checks performed
by a rule. For example, if you define an exemption section for 'Restriction'
rule with the parameter 'No_Allocators', then in this section only the
checks for ``No_Allocators`` will be exempted, and the checks for all
the other restrictions from your coding standard will be performed as usial.

See the description of individual rules to check if parametric exemptions
are available for them and what is the format of the rule parameters to
be used in the corresponding parameters of the ``Annotate`` pragmas.

.. _*gnatcheck*_Annotations_Rules:

*gnatcheck* Annotations Rules
=============================

.. index:: gnatcheck annotations rules


*
  An 'Exempt_Off' annotation can only appear after a corresponding
  'Exempt_On' annotation.

*
  Exempted source code sections are only based on the source location of the
  annotations. Any source construct between the two
  annotations is part of the exempted source code section.

*
  Exempted source code sections for different rules are independent. They can
  be nested or intersect with one another without limitation.
  Creating nested or intersecting source code sections for the same rule is
  not allowed.

*
  A matching 'Exempt_Off' annotation pragma for an 'Exempt_On' pragma
  that defines a parametric exemption section is the pragma that contains
  exactly the same set of rule parameters for the same rule.

*
  Parametric exemption sections for the same rule with different parameters
  can intersect or overlap in case if the parameter sets for such sections
  have an empty intersection.

*
  Malformed exempted source code sections are reported by a warning, and
  the corresponding rule exemptions are ignored.

*
  When an exempted source code section does not contain at least one violation
  of the exempted rule, a warning is emitted on :file:`stderr`.

*
  If an 'Exempt_On' annotation pragma does not have a matching
  'Exempt_Off' annotation pragma in the same compilation unit, a warning is issued
  and the exemption section is considered to last until the
  end of the compilation unit source.
