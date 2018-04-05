.. _Introduction:

************
Introduction
************


What Is ASIS?
=============

The *Ada Semantic Interface Specification* (ASIS) is an open and
published callable interface that
allows a tool to access syntactic and semantic information about an
Ada program, independent of the compilation environment that compiled the
program.

.. index:: Asis package

.. index:: ASIS queries

Technically, ASIS comprises a hierarchy of Ada packages rooted
at the package ``Asis``.
These
packages define a set of Ada private types that model the components of an Ada program
(e.g., declarations, statements, expressions)
and their interrelationships. Operations for these types, called
*ASIS queries*, give you statically determinable information about
Ada compilation units in your environment.

You may use ASIS as a third-part Ada library to implement a number of useful
program analysis tools.

ASIS Scope --- Which Kinds of Tools Can Be Built with ASIS?
================================================================

The following ASIS properties define the ASIS scope:

*
  ASIS is a read-only interface.

*
  ASIS provides only statically-determinable information about Ada programs.

  .. index:: ASIS queries

*
  ASIS provides access to the syntactic and basic semantic properties of compiled
  Ada units. If some semantic property of a program cannot be directly
  queried by means of ASIS queries, an ASIS application can compute the needed
  piece of information itself from the information available through ASIS
  queries.

*
  ASIS provides
  information from/about Ada units in high-level terms that
  conform with the Ada Reference Manual and that are
  Ada/ASIS-implementation-independent in nature.

.. index:: Tools (that can use ASIS)

Examples of tools that benefit from the ASIS interface include, but are not
limited to: automated code monitors, browsers, call tree tools, code
reformators, coding standards compliance tools, correctness verifiers,
debuggers, dependency tree analysis tools, design tools, document generators,
metrics tools, quality assessment tools, reverse engineering tools,
re-engineering tools, style checkers, test tools, timing estimators, and
translators.
