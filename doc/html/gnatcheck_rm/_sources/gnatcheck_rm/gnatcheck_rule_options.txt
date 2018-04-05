.. _gnatcheck_Rule_Options:

************************
*gnatcheck* Rule Options
************************

The following options control the processing performed by
*gnatcheck*.


  .. index:: +R (gnatcheck)


``+Rrule_id[:param]``
  Turn on the check for a specified rule with the specified parameter, if any.
  `rule_id` must be the identifier of one of the currently implemented rules
  (use ``-h`` for the list of implemented rules). Rule identifiers
  are not case-sensitive. The `param` item must
  be a string representing a valid parameter(s) for the specified rule.
  If it contains any space characters then this string must be enclosed in
  quotation marks.

  .. index:: -R (gnatcheck)


``-Rrule_id[:param]``
  Turn off the check for a specified rule with the specified parameter, if any.

  .. index:: -from (gnatcheck)


``-from=rule_option_filename``
  Read the rule options from the text file `rule_option_filename`, referred
  to as a 'coding standard file' below.


The default behavior is that all the rule checks are disabled.

If more than one rule option
is specified for the same rule, these options are summed together. If a new option contradicts
the rule settings specified by previous options for this rule, the new option overrides
the previous settings.

A coding standard file is a text file that contains a set of rule options
described above.

.. index:: Coding standard file (for gnatcheck)

The file may contain empty lines and Ada-style comments (comment
lines and end-of-line comments). There can be several rule options on a
single line (separated by a space).

A coding standard file may reference other coding standard files by including
more ``-from=rule_option_filename``
options, each such option being replaced with the content of the
corresponding coding standard file during processing. In case a
cycle is detected (that is, :file:`rule_file_1` reads rule options
from :file:`rule_file_2`, and :file:`rule_file_2` reads
(directly or indirectly) rule options from :file:`rule_file_1`),
processing fails with an error message.

If the name of the coding standard file does not contain a path information in
absolute form, then it is treated as being relative to the current directory if
gnatcheck is called without a project file or as being relative to the project
file directory if gnatcheck is called with a project file as an argument.
