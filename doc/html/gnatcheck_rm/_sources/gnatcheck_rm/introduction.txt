.. _Introduction:

************
Introduction
************

.. index:: ASIS

The *gnatcheck* tool is an ASIS-based utility that checks properties
of Ada source files according to a given set of semantic rules.

In order to check compliance with a given rule, *gnatcheck* has to
semantically analyze the Ada sources.
Therefore, checks can only be performed on
legal Ada units. Moreover, when a unit depends semantically upon units located
outside the current directory, the source search path has to be provided when
calling *gnatcheck*, either through a specified project file or
through *gnatcheck* switches as described below.

If the set of sources to be processed by ``gnatcheck`` contains sources with
preprocessing directives
then the needed options should be provided to run preprocessor as a part of
the *gnatcheck* call, and detected rule violations
will correspond to preprocessed sources.

A number of rules are predefined in *gnatcheck* and are described
later in this chapter.
You can also add new rules, by modifying the *gnatcheck* code and
rebuilding the tool. In order to add a simple rule making some local checks,
a small amount of straightforward ASIS-based programming is usually needed.

Invoking *gnatcheck* on the command line has the form:


::

  $ gnatcheck [switches]  {filename}
        [-files={arg_list_filename}]
        [-cargs gcc_switches] -rules rule_options


where

*
  `switches` specify the general tool options

*
  Each `filename` is the name (including the extension) of a source
  file to process. 'Wildcards' are allowed, and
  the file name may contain path information.

*
  Each `arg_list_filename` is the name (including the extension) of a text
  file containing the names of the source files to process, separated by spaces
  or line breaks.

*
  `gcc_switches` is a list of switches for
  *gcc*. They will be passed on to all compiler invocations made by
  *gnatcheck* to generate the ASIS trees. Here you can provide
  ``-I`` switches to form the source search path,
  and use the ``-gnatec`` switch to set the configuration file,
  etc.

*
  `rule_options` is a list of options for controlling a set of
  rules to be checked by *gnatcheck* (:ref:`gnatcheck_Rule_Options`).

Either a :file:`filename` or an :file:`arg_list_filename` must be
supplied.
