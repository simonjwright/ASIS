.. _ASIS_Extensions:

***************
ASIS Extensions
***************

.. index:: ASIS Extensions

ASIS-for-GNAT provides some additional types and queries as ASIS extensions.
All these queries are defined and documented in the hierarchy headed by
package ``Asis.Extensions``. They are referred as 'ASIS extensions' or
'ASIS extension queries' below.

.. index:: Asis.Extensions package

All the ASIS extensions obey the general ASIS rules:

*
  When using ASIS
  extensions, you have to follow the required sequencing of calls

*
  Only ASIS-defined exceptions propagate outside ASIS extension
  queries

If the documentation of an ASIS extension query contains a list of
'appropriate' ``Element`` kinds, then the query can be applied only to
``Element``\ s from this list, and it raises
``ASIS_Inappropriate_Element``
with ``Value_Error``
status otherwise. If the documentation of an ASIS extension
query contains a list of 'expected' element kinds, then the query can be
applied to an ``Element`` having any kind, but it returns a meaningful
result only for ``Element``\ s from this list.

.. index:: ASIS_Inappropriate_Element exception
.. index:: Value_Error error status

The current set of ASIS extensions originated from the ASIS implementation
needs and from the development of some ASIS tools inside the ASIS-for-GNAT
team. The ``Asis.Extensions`` hierarchy is not necessarily
frozen: some further extension queries may be added,
and suggestions from ASIS application developers are welcome.

Note that some of the ASIS extensions are implemented as ASIS *secondary
queries* --- that is, the implementation of such a query is a sequence of
primary ASIS queries. Some other extensions are *pure extensions*;
that is, their implementation is based on direct access to GNAT's internal
data structures.

``Asis.Extensions``
===================

.. index:: Asis.Extensions package

This package, whose spec is located in the file
:file:`asis-extensions.ads`,
contains the declarations of various ASIS extensions, including
dynamic ``Element`` and ``Compilation_Unit`` list types, placeholder
actual parameters for ``Asis.Iterator.Traverse_Element``,
additional ``Element`` structural and
semantic queries, queries that return information about the status of the
source file for a ``Compilation_Unit``, queries returning the (images
of the) values of static expressions, etc.

``Asis.Extensions.Flat_Kinds``
==============================

.. index:: Asis.Extensions.Flat_Kinds package
.. index:: Flat_Element_Kinds type

The ASIS ``Element`` classification hierarchy
is based on a set of Ada enumeration types,
each corresponding to a 'level' in the hierarchy.
The package ``Asis.Extensions.Flat_Kinds``, whose spec is located in the
file :file:`asis-extensions-flat_kinds.ads`,
defines the enumeration type ``Flat_Element_Kinds``;
this type combines the values of all these types and thus provides
a 'flat' view onto the syntactic ``Element`` classification.

``Asis.Extensions.Iterator``
============================

.. index:: Asis.Extensions.Iterator package

This package, whose spec is located in the file
:file:`asis-extensions-iterator.ads`,
contains the declarations of ``Traverse_Unit`` generic procedure that
is a generalization of the standard ASIS ``Asis.Iterator.Traverse_Element``
iterator. ``Traverse_Unit`` provides the depth-first traversal of the
whole syntactical structure of the ASIS Compilation Unit.
